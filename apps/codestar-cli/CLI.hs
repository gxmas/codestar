module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Exception (finally)
import Data.Aeson (encode)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Console.Haskeline
  ( InputT
  , defaultSettings
  , getInputLine
  , outputStrLn
  , runInputT
  )
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hFlush, hSetBuffering, stderr, stdout)

import CodeStar.AgentLoop
  ( AgentEnv (..)
  , AgentEvent (..)
  , SessionState
  , runAgent
  , runAgentTurn
  , sessionFromEnv
  )
import CodeStar.Compaction (CompactionState (..), emptyCompactionState)
import CodeStar.Compaction qualified as Compaction
import CodeStar.Config
  ( Config (..)
  , ApiKey (..)
  , BudgetSection (..)
  , CacheGcArgs (..)
  , TelemetrySection (..)
  , TelemetryMode (..)
  , ContextSection (..)
  , CompactionSection (..)
  , GuardrailsSection (..)
  , CliArgs (..)
  , CliCommand (..)
  , RunArgs (..)
  , loadConfig
  , parseCliArgs
  )
import CodeStar.Config.Migrate (migrateJsonToToml)
import CodeStar.Config.Paths qualified as Paths
import CodeStar.Context (ContextParts (..), assemble)
import CodeStar.Context qualified as CC
import CodeStar.Guardrails qualified as GR
import Control.Concurrent.STM (newTVarIO)
import CodeStar.Config.Types (ModelEntry (..))
import CodeStar.LLM.Anthropic (newAnthropicClient)
import CodeStar.LLM.Base (LlmClientDict, LlmError (..), ToolName (..), withDefaults, withRetry)
import CodeStar.LLM.OpenAI (newOpenAIClient)
import CodeStar.Memory (MemoryEntry (..), loadMemory, newMemoryStore)
import CodeStar.Permissions (newPermissionStore)
import CodeStar.Platform.CostTracker (newCostTracker)
import CodeStar.Platform.Sandbox (Sandbox, noSandbox)
import CodeStar.RepoMap.Cache (RepoMapCache (..), newRepoMapCache)
import CodeStar.RepoMap.CacheGc (CacheGcReport (..), StaleEntry (..), StaleReason (..), runCacheGc)
import CodeStar.RepoMap.Graph (querySourceModeLabel)
import CodeStar.RepoMap.Worker (RepoMapWorker, enqueueAll, enqueueFile, getCurrentMap, getIndexedFiles, getWorkerStatus, newRepoMapWorker, stopWorker)
import CodeStar.Storage (StorageBackend, newBackend)
import CodeStar.Telemetry
  ( OtelSettings (..)
  , TelemetryRecorder (..)
  , jsonRecorder
  , noOpRecorder
  , otlpRecorderWithHandle
  , shutdownTelemetry
  )
import CodeStar.Telemetry qualified as Tel
import CodeStar.Tools.Edit (editToolHandler)
import CodeStar.Tools.Glob (globToolHandler)
import CodeStar.Tools.Grep (grepToolHandler)
import CodeStar.Tools.MCP (connectMcpEndpoints)
import CodeStar.Tools.Read (ReadTracker, newReadTracker, readToolHandler)
import CodeStar.Tools.Registry
import CodeStar.Tools.Shell (shellToolHandler)
import CodeStar.Tools.TodoList (TodoStore, newTodoStore, todoListHandlers)
import CodeStar.TreeSitter (grammarCount, loadGrammarRegistry)
import CodeStar.TreeSitter.Grammars (GrammarSpec (..), fetchAllGrammars, fetchGrammar, grammarsDir, knownGrammars)
import CodeStar.Types (ControlSignal (..), SessionId (..), UserId (..))
import Resilience.Core (defaultRecoveryPolicy, newRecoveryEngine)

-- --------------------------------------------------------------------
-- Entry point
-- --------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- parseCliArgs
  case args.cliCommand of
    FetchGrammars mLang -> runFetchGrammars mLang
    MigrateConfig mWorkspace -> migrateJsonToToml mWorkspace
    CacheGc gcArgs -> runCacheGcCommand gcArgs
    RunAgent runArgs -> runAgentCli runArgs

-- --------------------------------------------------------------------
-- fetch-grammars command
-- --------------------------------------------------------------------

runFetchGrammars :: Maybe Text -> IO ()
runFetchGrammars mLang = do
  dir <- grammarsDir
  Text.IO.putStrLn ("Fetching grammars to " <> Text.pack dir <> " ...")
  results <- case mLang of
    Nothing -> fetchAllGrammars dir report
    Just l -> case filter (\g -> g.language == l) knownGrammars of
      [] -> Text.IO.hPutStr stderr ("Unknown: " <> l <> "\n") >> exitFailure
      (g : _) -> report l >> fmap (\r -> [(g, r)]) (fetchGrammar dir g)
  let failures = [(g, e) | (g, Left e) <- results]
      successes = length [() | (_, Right _) <- results]
  Text.IO.putStrLn ("\nDone. " <> Text.pack (show successes) <> " grammars installed.")
  if null failures
    then exitSuccess
    else do
      Text.IO.hPutStr stderr "Failed:\n"
      mapM_ (\(g, e) -> Text.IO.hPutStr stderr ("  " <> g.language <> ": " <> e <> "\n")) failures
      exitFailure

report :: Text -> IO ()
report lang = Text.IO.putStrLn ("  [→] " <> lang)

runCacheGcCommand :: CacheGcArgs -> IO ()
runCacheGcCommand args = do
  let workspace = maybe "." id args.cgWorkspace
      cacheRoot = Paths.projectDir workspace </> "cache"
  backend <- newBackend cacheRoot
  gcReport <- runCacheGc backend args.cgWorkspace args.cgDelete
  if args.cgJson
    then BL8.putStrLn (encode gcReport)
    else printCacheGcReport args.cgDelete gcReport

printCacheGcReport :: Bool -> CacheGcReport -> IO ()
printCacheGcReport deleted gcReport = do
  Text.IO.putStrLn ("Scanned entries: " <> tshow gcReport.scannedEntries)
  Text.IO.putStrLn ("Stale entries: " <> tshow gcReport.staleEntries)
  Text.IO.putStrLn ("Deleted entries: " <> tshow gcReport.deletedEntries)
  mapM_ printReason
    [StaleGlobal, StaleFile, StaleBoth]
  if null gcReport.entries
    then Text.IO.putStrLn "No stale entries found."
    else do
      Text.IO.putStrLn ""
      Text.IO.putStrLn
        (if deleted then "Processed stale entries:" else "Stale entries:")
      mapM_ printEntry gcReport.entries
 where
  printReason reason =
    Text.IO.putStrLn
      (reasonLabel reason <> ": " <> tshow (Map.findWithDefault 0 (reasonLabelInline reason) gcReport.staleByReason))

  reasonLabel StaleGlobal = "  stale-global"
  reasonLabel StaleFile = "  stale-file"
  reasonLabel StaleBoth = "  stale-both"

  printEntry entry =
    Text.IO.putStrLn
      ( "  - "
          <> maybe "<unknown-path>" Text.pack entry.stalePath
          <> " ["
          <> reasonLabelInline entry.staleReason
          <> "]"
      )

  reasonLabelInline StaleGlobal = "stale-global"
  reasonLabelInline StaleFile = "stale-file"
  reasonLabelInline StaleBoth = "stale-both"

tshow :: Show a => a -> Text
tshow = Text.pack . show

-- --------------------------------------------------------------------
-- run-agent command
-- --------------------------------------------------------------------

runAgentCli :: RunArgs -> IO ()
runAgentCli runArgs = do
  configResult <- loadConfig runArgs
  (config :: Config) <-
    either
      (\e -> Text.IO.hPutStr stderr (Text.pack (show e) <> "\n") >> exitFailure)
      pure
      configResult

  (recorder, shutdownRec) <- mkRecorder config.telemetry

  resEngine <- newRecoveryEngine defaultRecoveryPolicy
  let activeEntry = case filter (\m -> m.meName == config.activeModel) config.models of
        (e:_) -> e
        []    -> ModelEntry "default" "anthropic" "claude-sonnet-4-20250514"
                   config.apiKey Nothing Nothing (Just 8192)
  baseClient <- buildClientForEntryCli config activeEntry
  let onRetry err attempt = recorder.recordEvent Tel.EvLlmRetry
        { Tel.retryError       = llmErrorLabel err
        , Tel.retryAttempt     = attempt
        , Tel.retryAfterHintMs = case err of
            RateLimited secs -> round (secs * 1000)
            _                -> 0
        , Tel.lrSessionId      = ""
        }
      client = withRetry resEngine onRetry baseClient
  clientRef <- newIORef client
  pendingModelVar <- newTVarIO Nothing
  tracker <- newReadTracker
  todoStore <- newTodoStore
  globalCfgDir <- Paths.globalConfigDir
  let projDir = Paths.projectDir config.workspacePath
  perms <- newPermissionStore config.workspacePath globalCfgDir
  costRef <- newIORef (0 :: Int, 0 :: Int)
  costTracker <- newCostTracker config.budgets.sessionTokenMax config.budgets.dailyTokenMax
  memStore <- newMemoryStore (projDir </> "memory")
  memEntries <- loadMemory memStore
  let memPaths =
        [ projDir </> "memory" </> "entries" </> Text.unpack e.meId <> ".json"
        | e <- memEntries
        ]

  grammarReg <- loadGrammarRegistry
  grammarDir <- grammarsDir
  printGrammarDiagnostics grammarDir (grammarCount grammarReg)
  queryMode <- querySourceModeLabel
  Text.IO.putStrLn ("RepoMap query mode: " <> queryMode)
  cacheBackend <- newBackend (projDir </> "cache")
  printStaleFingerprintSafetyRail cacheBackend (Just config.workspacePath)
  let repoCache = newRepoMapCache cacheBackend

  -- Start background RepoMap worker - returns immediately with cached data
  repoWorker <- newRepoMapWorker grammarReg repoCache config.workspacePath
  repoMapText <- getCurrentMap repoWorker

  let sandbox = noSandbox config.workspacePath
  mcpHandlers <- connectMcpEndpoints recorder config.mcpEndpoints
  -- Wire edit callback to both invalidate cache AND enqueue for re-indexing
  let onEdit path = do
        repoCache.invalidate path
        enqueueFile repoWorker path
      registry = foldr register (buildRegistry tracker todoStore sandbox (Just onEdit)) mcpHandlers

  let ctxCfg = toContextConfig config.context
      compCfg = toCompactionConfig config.compaction
      grCfg = toGuardrailConfig config.guardrails

  (_ctxMsgs, ctxParts) <-
    assemble
      ctxCfg
      (buildSystemPrompt registry)
      repoMapText
      memPaths
      []
  let sysPrompt = ctxParts.systemPrompt

      env =
        AgentEnv
          { envLlm = clientRef
          , envTools = registry
          , envConfig = config
          , envTelemetry = recorder
          , envOnEvent = handleEvent costRef
          , envGuardrails = grCfg
          , envPermissions = Just perms
          , envCompaction = compCfg
          , envCompState = emptyCompactionState{csRepoMap = repoMapText}
          , envCostTracker = Just costTracker
          , envSessionId = SessionId "cli"
          , envUserId = UserId "local"
          , envGrammarReg = grammarReg
          , envMemoryStore = Just memStore
          , envWaitForInput = Nothing
          , envWaitForApproval = Nothing
          , envPendingModel = pendingModelVar
          }

  ( if runArgs.cliHeadless
      then runHeadless env sysPrompt runArgs
      else runInteractive env sysPrompt costRef repoWorker
    )
    `finally` shutdownRec

buildRegistry :: ReadTracker -> TodoStore -> Sandbox -> Maybe (FilePath -> IO ()) -> ToolRegistry
buildRegistry tracker todoStore sandbox mOnEdit =
  register (readToolHandler tracker) $
    register (editToolHandler tracker Nothing mOnEdit) $
      register globToolHandler $
        register grepToolHandler $
          register (shellToolHandler sandbox) $
            foldr register emptyRegistry (todoListHandlers todoStore)

buildSystemPrompt :: ToolRegistry -> Text
buildSystemPrompt registry =
  Text.unlines
    [ "You are CodeStar, an expert AI coding agent."
    , "Work methodically: read files before editing, validate changes,"
    , "and declare done only when you have evidence the task is complete."
    , ""
    , "## Available Tools"
    , ""
    , generateDocs registry
    ]

llmErrorLabel :: LlmError -> Text
llmErrorLabel (RateLimited _)         = "RateLimited"
llmErrorLabel (AuthenticationFailed _) = "AuthenticationFailed"
llmErrorLabel (ContextTooLong _ _)    = "ContextTooLong"
llmErrorLabel (ContentFiltered _)     = "ContentFiltered"
llmErrorLabel (InvalidRequest _)      = "InvalidRequest"
llmErrorLabel (ProviderError _)       = "ProviderError"
llmErrorLabel (NetworkError _)        = "NetworkError"

-- --------------------------------------------------------------------
-- Telemetry backend
-- --------------------------------------------------------------------

mkRecorder :: TelemetrySection -> IO (TelemetryRecorder, IO ())
mkRecorder tel = case tel.mode of
  TelemetryOff -> pure (noOpRecorder, pure ())
  TelemetryStderr -> pure (jsonRecorder, pure ())
  TelemetryOtlp ->
    mkOtelRecorder
      OtelSettings
        { serviceName = tel.serviceName
        , endpoint = tel.endpoint
        , logToStderr = tel.logToStderr
        , metricsEnabled = tel.metricsEnabled
        , metricsBindHost = tel.metricsBindHost
        , metricsPort = tel.metricsPort
        , sessionId = Nothing
        , userId = Nothing
        , tracesSampleRate = tel.sampleRate
        }

mkOtelRecorder :: OtelSettings -> IO (TelemetryRecorder, IO ())
mkOtelRecorder settings = do
  (recorder, handle) <- otlpRecorderWithHandle settings
  pure
    ( recorder
    , shutdownTelemetry handle
    )

-- --------------------------------------------------------------------
-- Headless mode
-- --------------------------------------------------------------------

runHeadless :: AgentEnv -> Text -> RunArgs -> IO ()
runHeadless env sysPrompt runArgs = do
  let task = case runArgs.cliTask of
        Just t -> t
        Nothing -> error "Headless mode requires --task"
  signal <- runAgent env sysPrompt task
  case signal of
    Done _ -> exitSuccess
    Blocked msg -> Text.IO.hPutStr stderr ("[blocked] " <> msg <> "\n") >> exitFailure
    _ -> exitFailure

-- --------------------------------------------------------------------
-- Interactive REPL
-- --------------------------------------------------------------------

runInteractive :: AgentEnv -> Text -> IORef (Int, Int) -> RepoMapWorker -> IO ()
runInteractive env sysPrompt costRef repoWorker = do
  runInputT defaultSettings (replLoop env sysPrompt costRef repoWorker (sessionFromEnv env))
    `finally` stopWorker repoWorker

replLoop :: AgentEnv -> Text -> IORef (Int, Int) -> RepoMapWorker -> SessionState -> InputT IO ()
replLoop env sysPrompt costRef repoWorker session = do
  mLine <- getInputLine "\ncodestar> "
  case mLine of
    Nothing -> do
      outputStrLn "Bye."
      liftIO $ stopWorker repoWorker
    Just line ->
      let input = Text.strip (Text.pack line)
       in if Text.null input
            then replLoop env sysPrompt costRef repoWorker session
            else handleInput env sysPrompt costRef repoWorker session input

handleInput :: AgentEnv -> Text -> IORef (Int, Int) -> RepoMapWorker -> SessionState -> Text -> InputT IO ()
handleInput env sysPrompt costRef repoWorker session input
  | Text.isPrefixOf "/" input = handleSlash env sysPrompt costRef repoWorker session input
  | otherwise = do
      -- Get fresh repo map before each turn (background worker keeps it updated)
      freshMap <- liftIO $ getCurrentMap repoWorker
      let env' = env{envCompState = env.envCompState{csRepoMap = freshMap}}
      (signal, session') <- liftIO (runAgentTurn env' sysPrompt session input)
      outputStrLn ("\n[" <> Text.unpack (signalText signal) <> "]")
      replLoop env sysPrompt costRef repoWorker session'

handleSlash :: AgentEnv -> Text -> IORef (Int, Int) -> RepoMapWorker -> SessionState -> Text -> InputT IO ()
handleSlash env sysPrompt costRef repoWorker session cmd = case Text.words cmd of
  ["/cost"] -> do
    (inTok, outTok) <- liftIO (readIORef costRef)
    outputStrLn ("Input tokens:  " <> show inTok)
    outputStrLn ("Output tokens: " <> show outTok)
    replLoop env sysPrompt costRef repoWorker session
  ["/clear"] -> do
    outputStrLn "[history cleared]"
    replLoop env sysPrompt costRef repoWorker (sessionFromEnv env)
  ["/compact"] -> noopSlash "Compaction scheduled for next step" env sysPrompt costRef repoWorker session
  ("/compact" : _) -> noopSlash "Compaction scheduled for next step" env sysPrompt costRef repoWorker session
  ["/approve"] -> noopSlash "Approval granted" env sysPrompt costRef repoWorker session
  ["/reject", _] -> noopSlash "Rejection recorded" env sysPrompt costRef repoWorker session
  ["/mode", mode] -> noopSlash ("/mode " <> Text.unpack mode <> " noted") env sysPrompt costRef repoWorker session
  ["/repomap"] -> do
    mapText <- liftIO $ getCurrentMap repoWorker
    if Text.null mapText
      then outputStrLn "[repo map is empty - still indexing...]"
      else outputStrLn (Text.unpack mapText)
    replLoop env sysPrompt costRef repoWorker session
  ["/status"] -> do
    (filesIndexed, totalTags, pending, queueStatus) <- liftIO $ getWorkerStatus repoWorker
    let grammarsLoaded = grammarCount env.envGrammarReg
    grammarDir <- liftIO grammarsDir
    queryMode <- liftIO querySourceModeLabel
    outputStrLn $ "Grammars dir: " <> grammarDir
    outputStrLn $ "Grammars loaded: " <> show grammarsLoaded <> " / " <> show (length knownGrammars)
    mapM_ outputStrLn (grammarWarnings grammarDir grammarsLoaded)
    outputStrLn $ "Query mode: " <> Text.unpack queryMode
    outputStrLn $ "Files indexed: " <> show filesIndexed
    outputStrLn $ "Total tags: " <> show totalTags
    outputStrLn $ "Pending rebuild: " <> show pending
    outputStrLn $ "Queue: " <> if queueStatus == 0 then "empty" else "processing"
    replLoop env sysPrompt costRef repoWorker session
  ["/files"] -> do
    files <- liftIO $ getIndexedFiles repoWorker
    if null files
      then outputStrLn "[no files indexed yet]"
      else mapM_ (outputStrLn . ("  " <>)) files
    replLoop env sysPrompt costRef repoWorker session
  ["/rescan"] -> do
    outputStrLn "[rescanning workspace...]"
    liftIO $ enqueueAll repoWorker
    replLoop env sysPrompt costRef repoWorker session
  ["/help"] -> do
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
    replLoop env sysPrompt costRef repoWorker session
  _ -> do
    outputStrLn ("Unknown: " <> Text.unpack cmd)
    replLoop env sysPrompt costRef repoWorker session

noopSlash :: String -> AgentEnv -> Text -> IORef (Int, Int) -> RepoMapWorker -> SessionState -> InputT IO ()
noopSlash msg env sysPrompt costRef repoWorker session =
  outputStrLn ("[" <> msg <> "]") >> replLoop env sysPrompt costRef repoWorker session

-- --------------------------------------------------------------------
-- Event handler
-- --------------------------------------------------------------------

handleEvent :: IORef (Int, Int) -> AgentEvent -> IO ()
handleEvent costRef = \case
  AgentToken t ->
    Text.IO.putStr t >> hFlush stdout
  AgentToolCall (ToolName name) args ->
    Text.IO.putStrLn ("\n  → " <> name <> " " <> Text.take 120 args)
  AgentToolResult (ToolName name) result ->
    Text.IO.putStrLn ("  ← " <> name <> ": " <> Text.take 200 result)
  AgentApprovalRequired (ToolName name) reason ->
    Text.IO.putStrLn ("\n[approval required] " <> name <> ": " <> reason)
  AgentCompacting ->
    Text.IO.putStrLn "\n[compacting…]"
  AgentProgress msg ->
    Text.IO.putStrLn ("[progress] " <> msg)
  AgentCostUpdate inTok outTok ->
    modifyIORef' costRef (\(i, o) -> (i + inTok, o + outTok))
  AgentDone signal ->
    Text.IO.putStrLn ("\n[done: " <> signalText signal <> "]")
  AgentError msg ->
    Text.IO.putStrLn ("[error] " <> msg)

signalText :: ControlSignal -> Text
signalText = \case
  Done _ -> "done"
  Continue -> "continue"
  NeedsInput q -> "needs-input: " <> q
  Blocked r -> "blocked: " <> r

-- --------------------------------------------------------------------
-- Config section conversions
-- --------------------------------------------------------------------

toContextConfig :: ContextSection -> CC.ContextConfig
toContextConfig c = CC.ContextConfig
  { CC.maxContextTokens = c.maxTokens
  , CC.repoMapReserve   = c.repoMapReserve
  , CC.memoryReserve    = c.memoryReserve
  , CC.compactionReserve = c.compactionReserve
  , CC.responseReserve  = c.responseReserve
  }

toCompactionConfig :: CompactionSection -> Compaction.CompactionConfig
toCompactionConfig c = Compaction.CompactionConfig
  { Compaction.triggerFraction  = c.triggerFraction
  , Compaction.maxContextTokens = c.maxContextTokens
  }

toGuardrailConfig :: GuardrailsSection -> GR.GuardrailConfig
toGuardrailConfig g = GR.GuardrailConfig
  { GR.denyList       = g.denyList
  , GR.allowList      = g.allowList
  , GR.allowedPaths   = Nothing
  , GR.secretPatterns = g.secretPatterns
  }

printGrammarDiagnostics :: FilePath -> Int -> IO ()
printGrammarDiagnostics grammarDir loadedCount = do
  Text.IO.putStrLn ("Grammars dir: " <> Text.pack grammarDir)
  Text.IO.putStrLn ("Grammars loaded: " <> tshow loadedCount <> " / " <> tshow (length knownGrammars))
  mapM_ (Text.IO.putStrLn . Text.pack) (grammarWarnings grammarDir loadedCount)

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

printStaleFingerprintSafetyRail :: StorageBackend -> Maybe FilePath -> IO ()
printStaleFingerprintSafetyRail cacheBackend mWorkspace = do
  gcReport <- runCacheGc cacheBackend mWorkspace False
  let staleGlobal = Map.findWithDefault 0 "stale-global" gcReport.staleByReason
      staleBoth = Map.findWithDefault 0 "stale-both" gcReport.staleByReason
      staleFingerprint = staleGlobal + staleBoth
  if staleFingerprint > 0
    then
      Text.IO.putStrLn
        ( "Note: detected "
            <> tshow staleFingerprint
            <> " stale repo-map cache entries from a previous extractor/query fingerprint; these entries are ignored. "
            <> "Run `codestar-cli cache-gc --delete` to clean them."
        )
    else pure ()

buildClientForEntryCli :: Config -> ModelEntry -> IO LlmClientDict
buildClientForEntryCli config entry =
  let ApiKey key = if unApiKey entry.meApiKey /= ""
                   then entry.meApiKey
                   else config.apiKey
  in case entry.meProvider of
    "anthropic" -> do
      c <- newAnthropicClient key entry.meModel
      pure (withDefaults entry.meTemperature entry.meTopP entry.meMaxTokens c)
    _ -> do
      c <- newOpenAIClient key entry.meModel
      pure (withDefaults entry.meTemperature entry.meTopP entry.meMaxTokens c)
