module CLI.Setup
  ( CliResources (..)
  , buildCliResources
  , loadConfigOrDie
  ) where

import Control.Concurrent.STM (newTVarIO)
import Data.IORef (IORef, modifyIORef', newIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stderr, stdout)

import CodeStar.AgentLoop
  ( AgentEnv (..)
  , AgentEvent (..)
  )
import CodeStar.AgentSetup (buildClientForEntry, buildRegistry, buildSystemPrompt, llmErrorLabel, mkRecorder)
import CodeStar.Compaction (CompactionState (..), emptyCompactionState)
import CLI.Diagnostics (printGrammarDiagnostics, printStaleFingerprintSafetyRail)
import CodeStar.Config
  ( Config (..)
  , BudgetSection (..)
  , RunArgs (..)
  , loadConfig
  )
import CodeStar.Config.Types (ModelEntry (..))
import CodeStar.Config.Convert (toContextConfig, toCompactionConfig, toGuardrailConfig)
import CodeStar.Config.Paths qualified as Paths
import CodeStar.Context (ContextParts (..), assemble)
import CodeStar.LLM.Base (LlmError (..), ToolName (..), withRetry)
import CodeStar.Memory (MemoryEntry (..), loadMemory, newMemoryStore)
import CodeStar.Permissions (newPermissionStore)
import CodeStar.Platform.CostTracker (newCostTracker)
import CodeStar.Platform.Sandbox (noSandbox)
import CodeStar.RepoMap.Cache (RepoMapCache (..), newRepoMapCache)
import CodeStar.RepoMap.Graph (querySourceModeLabel)
import CodeStar.RepoMap.Worker (RepoMapWorker, enqueueFile, getCurrentMap, newRepoMapWorker)
import CodeStar.Storage (newBackend)
import CodeStar.Telemetry (TelemetryRecorder (..))
import CodeStar.Telemetry qualified as Tel
import CodeStar.Tools.MCP (connectMcpEndpoints)
import CodeStar.Tools.Read (newReadTracker)
import CodeStar.Tools.Registry (register)
import CodeStar.Tools.TodoList (newTodoStore)
import CodeStar.TreeSitter (grammarCount, loadGrammarRegistry)
import CodeStar.TreeSitter.Grammars (grammarsDir)
import CodeStar.Types (ControlSignal (..), SessionId (..), UserId (..))
import Resilience.Core (defaultRecoveryPolicy, newRecoveryEngine)

-- --------------------------------------------------------------------
-- Public types
-- --------------------------------------------------------------------

data CliResources = CliResources
  { crEnv       :: !AgentEnv
  , crSysPrompt :: !Text
  , crCostRef   :: !(IORef (Int, Int))
  , crWorker    :: !RepoMapWorker
  , crShutdown  :: !(IO ())
  }

-- --------------------------------------------------------------------
-- Public API
-- --------------------------------------------------------------------

loadConfigOrDie :: RunArgs -> IO Config
loadConfigOrDie runArgs = do
  configResult <- loadConfig runArgs
  either
    (\e -> Text.IO.hPutStr stderr (Text.pack (show e) <> "\n") >> exitFailure)
    pure
    configResult

buildCliResources :: Config -> RunArgs -> IO CliResources
buildCliResources config _runArgs = do
  (recorder, shutdownRec) <- mkRecorder config.telemetry

  resEngine <- newRecoveryEngine defaultRecoveryPolicy
  let activeEntry = case filter (\m -> m.meName == config.activeModel) config.models of
        (e:_) -> e
        []    -> ModelEntry "default" "anthropic" "claude-sonnet-4-20250514"
                   config.apiKey Nothing Nothing (Just 8192)
  baseClient <- buildClientForEntry config activeEntry
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

  repoWorker <- newRepoMapWorker grammarReg repoCache config.workspacePath
  repoMapText <- getCurrentMap repoWorker

  let sandbox = noSandbox config.workspacePath
  mcpHandlers <- connectMcpEndpoints recorder config.mcpEndpoints
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

  pure CliResources
    { crEnv       = env
    , crSysPrompt = sysPrompt
    , crCostRef   = costRef
    , crWorker    = repoWorker
    , crShutdown  = shutdownRec
    }



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


