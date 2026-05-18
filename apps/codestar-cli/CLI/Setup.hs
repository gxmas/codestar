module CLI.Setup
  ( CliResources (..)
  , buildCliResources
  , loadConfigOrDie
  ) where

import Control.Concurrent.STM (newTVarIO)
import Data.IORef (IORef, modifyIORef', newIORef)
import Data.Map.Strict qualified as Map
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
import CodeStar.Compaction (CompactionState (..), emptyCompactionState)
import CodeStar.Compaction qualified as Compaction
import CLI.Telemetry (mkRecorder)
import CodeStar.Config
  ( Config (..)
  , ApiKey (..)
  , BudgetSection (..)
  , ContextSection (..)
  , CompactionSection (..)
  , GuardrailsSection (..)
  , RunArgs (..)
  , loadConfig
  )
import CodeStar.Config.Paths qualified as Paths
import CodeStar.Config.Types (ModelEntry (..))
import CodeStar.Context (ContextParts (..), assemble)
import CodeStar.Context qualified as CC
import CodeStar.Guardrails qualified as GR
import CodeStar.LLM.Anthropic (newAnthropicClient)
import CodeStar.LLM.Base (LlmClientDict, LlmError (..), ToolName (..), withDefaults, withRetry)
import CodeStar.LLM.OpenAI (newOpenAIClient)
import CodeStar.Memory (MemoryEntry (..), loadMemory, newMemoryStore)
import CodeStar.Permissions (newPermissionStore)
import CodeStar.Platform.CostTracker (newCostTracker)
import CodeStar.Platform.Sandbox (Sandbox, noSandbox)
import CodeStar.RepoMap.Cache (RepoMapCache (..), newRepoMapCache)
import CodeStar.RepoMap.CacheGc (CacheGcReport (..), runCacheGc)
import CodeStar.RepoMap.Graph (querySourceModeLabel)
import CodeStar.RepoMap.Worker (RepoMapWorker, enqueueFile, getCurrentMap, newRepoMapWorker)
import CodeStar.Storage (StorageBackend, newBackend)
import CodeStar.Telemetry (TelemetryRecorder (..))
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
import CodeStar.TreeSitter.Grammars (grammarsDir, knownGrammars)
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
-- LLM client construction
-- --------------------------------------------------------------------

buildClientForEntry :: Config -> ModelEntry -> IO LlmClientDict
buildClientForEntry config entry =
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

llmErrorLabel :: LlmError -> Text
llmErrorLabel (RateLimited _)         = "RateLimited"
llmErrorLabel (AuthenticationFailed _) = "AuthenticationFailed"
llmErrorLabel (ContextTooLong _ _)    = "ContextTooLong"
llmErrorLabel (ContentFiltered _)     = "ContentFiltered"
llmErrorLabel (InvalidRequest _)      = "InvalidRequest"
llmErrorLabel (ProviderError _)       = "ProviderError"
llmErrorLabel (NetworkError _)        = "NetworkError"

-- --------------------------------------------------------------------
-- Tool registry
-- --------------------------------------------------------------------

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

-- --------------------------------------------------------------------
-- Diagnostics
-- --------------------------------------------------------------------

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

tshow :: Show a => a -> Text
tshow = Text.pack . show
