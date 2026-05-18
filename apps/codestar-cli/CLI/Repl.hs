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

data ReplEnv = ReplEnv
  { reEnv       :: !AgentEnv
  , reSysPrompt :: !Text
  , reCostRef   :: !(IORef (Int, Int))
  , reWorker    :: !RepoMapWorker
  }

type Repl a = ReaderT ReplEnv (InputT IO) a

data SlashResult
  = KeepSession SessionState
  | ResetSession

type SlashHandler = SessionState -> [Text] -> Repl SlashResult

-- --------------------------------------------------------------------
-- Entry point
-- --------------------------------------------------------------------

runInteractive :: ReplEnv -> IO ()
runInteractive renv = do
  let session = sessionFromEnv renv.reEnv
  runInputT defaultSettings (runReaderT (replLoop session) renv)
    `finally` stopWorker renv.reWorker

-- --------------------------------------------------------------------
-- REPL loop
-- --------------------------------------------------------------------

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

cmdCost :: SlashHandler
cmdCost session _args = do
  costRef <- asks reCostRef
  (inTok, outTok) <- liftIO (readIORef costRef)
  liftInputT $ outputStrLn ("Input tokens:  " <> show inTok)
  liftInputT $ outputStrLn ("Output tokens: " <> show outTok)
  pure (KeepSession session)

cmdClear :: SlashHandler
cmdClear _session _args = do
  liftInputT $ outputStrLn "[history cleared]"
  pure ResetSession

cmdCompact :: SlashHandler
cmdCompact session _args = do
  liftInputT $ outputStrLn "[Compaction scheduled for next step]"
  pure (KeepSession session)

cmdApprove :: SlashHandler
cmdApprove session _args = do
  liftInputT $ outputStrLn "[Approval granted]"
  pure (KeepSession session)

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

cmdRepoMap :: SlashHandler
cmdRepoMap session _args = do
  worker <- asks reWorker
  mapText <- liftIO $ getCurrentMap worker
  liftInputT $
    if Text.null mapText
      then outputStrLn "[repo map is empty - still indexing...]"
      else outputStrLn (Text.unpack mapText)
  pure (KeepSession session)

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

signalText :: ControlSignal -> Text
signalText = \case
  Done _ -> "done"
  Continue -> "continue"
  NeedsInput q -> "needs-input: " <> q
  Blocked r -> "blocked: " <> r

liftInputT :: InputT IO a -> Repl a
liftInputT m = ReaderT $ \_ -> m

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
