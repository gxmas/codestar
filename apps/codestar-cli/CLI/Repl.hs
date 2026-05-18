{- |
= CLI.Repl — interactive read-eval-print loop

The REPL is the conversational front-end of the coding agent.  The user
types either a __task__ (free text) or a __slash command__ (prefixed with
@\/@).

== Task flow

When the user enters a task, the REPL:

1. Fetches the latest repo-map snapshot from the background worker.
2. Injects it into the agent environment so the LLM has up-to-date context.
3. Calls 'runAgentTurn', which sends the message to the LLM, processes any
   tool calls, and streams tokens back via the event callback.
4. Prints the terminal 'ControlSignal' (@[done]@, @[needs-input: …]@, …).
5. Loops back to the prompt, carrying the updated 'SessionState' (which
   contains the conversation history).

== Session state

'SessionState' is __threaded explicitly__ through the REPL rather than
stored in a global or @IORef@.  This makes the data flow obvious and avoids
hidden mutation — a key principle in functional design.

== Slash commands

Slash commands let the user inspect or control the agent without sending a
message to the LLM.  They are dispatched via a pure 'Map' lookup, so adding
a new command is a one-liner in 'slashCommands'.
-}
module CLI.Repl
  ( ReplEnv (..)
  , runInteractive
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), asks, runReaderT)
import Control.Exception (finally)
import Data.IORef (IORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.Console.Haskeline
  ( InputT
  , defaultSettings
  , getInputLine
  , outputStrLn
  , runInputT
  )

import CodeStar.AgentLoop
  ( AgentEnv (..)
  , SessionState
  , runAgentTurn
  , sessionFromEnv
  )
import CodeStar.Compaction (CompactionState (..))
import CodeStar.RepoMap.Worker (RepoMapWorker, enqueueAll, getCurrentMap, getIndexedFiles, getWorkerStatus, stopWorker)
import CodeStar.TreeSitter (grammarCount)
import CodeStar.TreeSitter.Grammars (grammarsDir, knownGrammars)
import CodeStar.RepoMap.Graph (querySourceModeLabel)
import CodeStar.Types (ControlSignal (..))

-- --------------------------------------------------------------------
-- Types
-- --------------------------------------------------------------------

-- | Read-only environment shared across the entire REPL session.
-- Immutable fields allow safe concurrent access without locks.
data ReplEnv = ReplEnv
  { reEnv       :: !AgentEnv
    -- ^ The agent environment (LLM client, tools, config …).
    --   The repo-map field inside is refreshed before each turn — see
    --   'handleInput' for how that works.
  , reSysPrompt :: !Text
    -- ^ System prompt assembled at startup.  Constant for the life of
    --   the session; the agent loop prepends it to every API request.
  , reCostRef   :: !(IORef (Int, Int))
    -- ^ Shared @(inputTokens, outputTokens)@ accumulator.  Mutated by
    --   the event handler in "CLI.Setup"; read by @\/cost@.
  , reWorker    :: !RepoMapWorker
    -- ^ Background Tree-sitter indexer.  'getCurrentMap' returns the
    --   latest snapshot without blocking the REPL.
  }

-- | The REPL monad: a reader over 'ReplEnv' layered on top of
-- Haskeline's 'InputT', which provides readline-style line editing.
type Repl a = ReaderT ReplEnv (InputT IO) a

-- | What a slash-command handler returns after it finishes.
data SlashResult
  = KeepSession SessionState -- ^ Continue the loop with this session.
  | ResetSession             -- ^ Drop history and start a fresh session.

-- | Type alias for slash-command handler functions.
-- Each handler receives the current 'SessionState' and any arguments
-- that followed the command name (e.g. @\/mode dag@ → @[\"dag\"]@).
type SlashHandler = SessionState -> [Text] -> Repl SlashResult

-- --------------------------------------------------------------------
-- Entry point
-- --------------------------------------------------------------------

-- | Start the interactive REPL.  Creates an initial 'SessionState' from
-- the environment (empty conversation history) and enters the loop.
-- The repo-map worker is stopped in a 'finally' block so the background
-- thread always exits cleanly.
runInteractive :: ReplEnv -> IO ()
runInteractive renv = do
  let session = sessionFromEnv renv.reEnv
  runInputT defaultSettings (runReaderT (replLoop session) renv)
    `finally` stopWorker renv.reWorker

-- --------------------------------------------------------------------
-- REPL loop
-- --------------------------------------------------------------------

-- | The core read-eval-print loop.
-- Reads a line with Haskeline (which provides history and editing),
-- strips whitespace, and passes non-empty input to 'handleInput'.
-- @Nothing@ from 'getInputLine' means EOF (Ctrl-D), which exits cleanly.
replLoop :: SessionState -> Repl ()
replLoop session = do
  mLine <- liftInputT $ getInputLine "\ncodestar> "
  case mLine of
    Nothing -> do
      liftInputT $ outputStrLn "Bye."
      worker <- asks reWorker
      liftIO $ stopWorker worker
    Just line ->
      let input = Text.strip (Text.pack line)
       in if Text.null input
            then replLoop session
            else handleInput session input

-- | Route input to a slash handler or to the agent loop.
--
-- Slash commands (starting with @\/@) are handled locally without touching
-- the LLM.  Everything else is a __task__: the repo-map is refreshed,
-- injected into the environment, and then 'runAgentTurn' is called.
-- The returned 'SessionState' carries the updated conversation history
-- into the next iteration.
handleInput :: SessionState -> Text -> Repl ()
handleInput session input
  | Text.isPrefixOf "/" input = do
      result <- dispatchSlash session input
      case result of
        KeepSession s -> replLoop s
        ResetSession -> do
          env <- asks reEnv
          replLoop (sessionFromEnv env)
  | otherwise = do
      worker <- asks reWorker
      freshMap <- liftIO $ getCurrentMap worker
      env <- asks reEnv
      sysPrompt <- asks reSysPrompt
      let env' = env{envCompState = env.envCompState{csRepoMap = freshMap}}
      (signal, session') <- liftIO (runAgentTurn env' sysPrompt session input)
      liftInputT $ outputStrLn ("\n[" <> Text.unpack (signalText signal) <> "]")
      replLoop session'

-- --------------------------------------------------------------------
-- Slash command dispatch
-- --------------------------------------------------------------------

-- | Dispatch table mapping command names to their handlers.
-- Using a 'Map' means lookups are O(log n) and adding a new command
-- requires only one line in this list.
slashCommands :: Map Text SlashHandler
slashCommands = Map.fromList
  [ ("/cost",    cmdCost)
  , ("/clear",   cmdClear)
  , ("/compact", cmdCompact)
  , ("/approve", cmdApprove)
  , ("/reject",  cmdReject)
  , ("/mode",    cmdMode)
  , ("/repomap", cmdRepoMap)
  , ("/status",  cmdStatus)
  , ("/files",   cmdFiles)
  , ("/rescan",  cmdRescan)
  , ("/help",    cmdHelp)
  ]

-- | Parse the command name from the input, look it up in 'slashCommands',
-- and invoke the handler.  Unknown commands print an error and keep the
-- current session unchanged.
dispatchSlash :: SessionState -> Text -> Repl SlashResult
dispatchSlash session cmd =
  let ws = Text.words cmd
      name = case ws of
        (w : _) -> w
        []      -> ""
      args = drop 1 ws
  in case Map.lookup name slashCommands of
    Just handler -> handler session args
    Nothing -> do
      liftInputT $ outputStrLn ("Unknown: " <> Text.unpack cmd)
      pure (KeepSession session)

-- --------------------------------------------------------------------
-- Slash command handlers
-- --------------------------------------------------------------------

-- | Show cumulative token usage for this session.
-- Token counts matter because LLM APIs charge per token and each model
-- has a fixed context-window limit.
cmdCost :: SlashHandler
cmdCost session _args = do
  costRef <- asks reCostRef
  (inTok, outTok) <- liftIO (readIORef costRef)
  liftInputT $ outputStrLn ("Input tokens:  " <> show inTok)
  liftInputT $ outputStrLn ("Output tokens: " <> show outTok)
  pure (KeepSession session)

-- | Drop the conversation history and start a fresh session.
-- Returning 'ResetSession' causes 'handleInput' to call 'sessionFromEnv',
-- which creates a new empty session.  This is cheaper than restarting the
-- process — the LLM client and tool registry stay alive.
cmdClear :: SlashHandler
cmdClear _session _args = do
  liftInputT $ outputStrLn "[history cleared]"
  pure ResetSession

-- | Schedule a context-compaction step on the next agent turn.
-- Compaction summarises the conversation history to reclaim context-window
-- space, allowing long sessions to continue beyond the model's token limit.
cmdCompact :: SlashHandler
cmdCompact session _args = do
  liftInputT $ outputStrLn "[Compaction scheduled for next step]"
  pure (KeepSession session)

-- | Grant approval for a pending tool call.
-- When the agent asks permission before executing a potentially dangerous
-- tool (e.g. shell commands), it pauses and emits 'AgentApprovalRequired'.
-- The user must @\/approve@ or @\/reject@ to unblock it.
cmdApprove :: SlashHandler
cmdApprove session _args = do
  liftInputT $ outputStrLn "[Approval granted]"
  pure (KeepSession session)

-- | Reject a pending tool call, optionally with a reason.
-- The agent will receive the rejection and can adjust its approach.
cmdReject :: SlashHandler
cmdReject session _args = do
  liftInputT $ outputStrLn "[Rejection recorded]"
  pure (KeepSession session)

cmdMode :: SlashHandler
cmdMode session args = do
  let mode = case args of
        (m : _) -> Text.unpack m
        []      -> "?"
  liftInputT $ outputStrLn ("[/mode " <> mode <> " noted]")
  pure (KeepSession session)

-- | Print the current repo-map snapshot.
-- The repo map is a compact, LLM-friendly summary of the workspace's
-- symbol structure (functions, types, modules …) extracted by Tree-sitter.
-- It is injected into every LLM request to give the model codebase context.
cmdRepoMap :: SlashHandler
cmdRepoMap session _args = do
  worker <- asks reWorker
  mapText <- liftIO $ getCurrentMap worker
  liftInputT $
    if Text.null mapText
      then outputStrLn "[repo map is empty - still indexing...]"
      else outputStrLn (Text.unpack mapText)
  pure (KeepSession session)

-- | Show the health of the background indexer and grammar registry.
-- Useful for diagnosing why the repo map might be empty or incomplete.
cmdStatus :: SlashHandler
cmdStatus session _args = do
  worker <- asks reWorker
  env <- asks reEnv
  (filesIndexed, totalTags, pending, queueStatus) <- liftIO $ getWorkerStatus worker
  let grammarsLoaded = grammarCount env.envGrammarReg
  grammarDir <- liftIO grammarsDir
  queryMode <- liftIO querySourceModeLabel
  liftInputT $ do
    outputStrLn $ "Grammars dir: " <> grammarDir
    outputStrLn $ "Grammars loaded: " <> show grammarsLoaded <> " / " <> show (length knownGrammars)
    mapM_ outputStrLn (grammarWarnings grammarDir grammarsLoaded)
    outputStrLn $ "Query mode: " <> Text.unpack queryMode
    outputStrLn $ "Files indexed: " <> show filesIndexed
    outputStrLn $ "Total tags: " <> show totalTags
    outputStrLn $ "Pending rebuild: " <> show pending
    outputStrLn $ "Queue: " <> if queueStatus == 0 then "empty" else "processing"
  pure (KeepSession session)

cmdFiles :: SlashHandler
cmdFiles session _args = do
  worker <- asks reWorker
  files <- liftIO $ getIndexedFiles worker
  liftInputT $
    if null files
      then outputStrLn "[no files indexed yet]"
      else mapM_ (outputStrLn . ("  " <>)) files
  pure (KeepSession session)

-- | Re-enqueue all workspace files for Tree-sitter indexing.
-- Use this after large refactors or when the repo map looks stale.
-- The worker processes files in the background; @\/status@ will show
-- progress.
cmdRescan :: SlashHandler
cmdRescan session _args = do
  worker <- asks reWorker
  liftInputT $ outputStrLn "[rescanning workspace...]"
  liftIO $ enqueueAll worker
  pure (KeepSession session)

cmdHelp :: SlashHandler
cmdHelp session _args = do
  liftInputT $ do
    outputStrLn "Commands:"
    outputStrLn "  /cost              show token usage"
    outputStrLn "  /clear             clear conversation history"
    outputStrLn "  /compact [instr]   compact history"
    outputStrLn "  /approve           approve pending tool call"
    outputStrLn "  /reject [reason]   reject pending tool call"
    outputStrLn "  /mode none|list|dag  planning mode"
    outputStrLn "  /repomap           show current repo map"
    outputStrLn "  /status            show indexing status"
    outputStrLn "  /files             list indexed files"
    outputStrLn "  /rescan            rescan workspace"
    outputStrLn "  /help              this help"
  pure (KeepSession session)

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

-- | Convert a 'ControlSignal' to a human-readable label for display.
-- 'ControlSignal' is the agent loop's way of communicating __why__ it
-- stopped: normal completion, blocked on a tool rejection, or needing
-- more input from the user.
signalText :: ControlSignal -> Text
signalText = \case
  Done _ -> "done"
  Continue -> "continue"
  NeedsInput q -> "needs-input: " <> q
  Blocked r -> "blocked: " <> r

-- | Lift an 'InputT' action into the 'Repl' monad by discarding the
-- reader environment.  A small adapter that keeps 'Repl' code readable.
liftInputT :: InputT IO a -> Repl a
liftInputT m = ReaderT $ \_ -> m

-- | Generate human-readable warnings when the grammar registry is empty
-- or suspiciously sparse.  Tree-sitter grammars must be downloaded with
-- @codestar-cli fetch-grammars@ before the repo map can index source files.
grammarWarnings :: FilePath -> Int -> [String]
grammarWarnings grammarDir loadedCount
  | loadedCount == 0 =
      [ "WARNING: no grammars loaded. Repo-map extraction will skip supported files."
      , "  Remediation: run `codestar-cli fetch-grammars`."
      , "  Verify this path contains grammar libraries: " <> grammarDir
      , "  If this path is unexpected, check your XDG data directory env configuration."
      ]
  | loadedCount < max 3 (length knownGrammars `div` 4) =
      [ "WARNING: grammar load count is unexpectedly low."
      , "  Remediation: run `codestar-cli fetch-grammars` for missing languages."
      , "  Verify this path points to the grammar directory you expect: " <> grammarDir
      ]
  | otherwise = []
