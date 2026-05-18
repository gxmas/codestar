module CLI.Repl
  ( ReplEnv (..)
  , runInteractive
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), asks, runReaderT)
import Control.Exception (finally)
import Data.IORef (IORef, readIORef)
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
  | Text.isPrefixOf "/" input = handleSlash session input
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
-- Slash commands
-- --------------------------------------------------------------------

handleSlash :: SessionState -> Text -> Repl ()
handleSlash session cmd = case Text.words cmd of
  ["/cost"] -> do
    costRef <- asks reCostRef
    (inTok, outTok) <- liftIO (readIORef costRef)
    liftInputT $ outputStrLn ("Input tokens:  " <> show inTok)
    liftInputT $ outputStrLn ("Output tokens: " <> show outTok)
    replLoop session
  ["/clear"] -> do
    liftInputT $ outputStrLn "[history cleared]"
    env <- asks reEnv
    replLoop (sessionFromEnv env)
  ["/compact"] -> noopSlash "Compaction scheduled for next step" session
  ("/compact" : _) -> noopSlash "Compaction scheduled for next step" session
  ["/approve"] -> noopSlash "Approval granted" session
  ["/reject", _] -> noopSlash "Rejection recorded" session
  ["/mode", mode] -> noopSlash ("/mode " <> Text.unpack mode <> " noted") session
  ["/repomap"] -> do
    worker <- asks reWorker
    mapText <- liftIO $ getCurrentMap worker
    liftInputT $
      if Text.null mapText
        then outputStrLn "[repo map is empty - still indexing...]"
        else outputStrLn (Text.unpack mapText)
    replLoop session
  ["/status"] -> do
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
    replLoop session
  ["/files"] -> do
    worker <- asks reWorker
    files <- liftIO $ getIndexedFiles worker
    liftInputT $
      if null files
        then outputStrLn "[no files indexed yet]"
        else mapM_ (outputStrLn . ("  " <>)) files
    replLoop session
  ["/rescan"] -> do
    worker <- asks reWorker
    liftInputT $ outputStrLn "[rescanning workspace...]"
    liftIO $ enqueueAll worker
    replLoop session
  ["/help"] -> do
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
      outputStrLn "  /help              this help"
    replLoop session
  _ -> do
    liftInputT $ outputStrLn ("Unknown: " <> Text.unpack cmd)
    replLoop session

noopSlash :: String -> SessionState -> Repl ()
noopSlash msg session = do
  liftInputT $ outputStrLn ("[" <> msg <> "]")
  replLoop session

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
