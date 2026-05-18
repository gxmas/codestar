module Server.SessionSetup (spawnAgentSession) where

import Control.Concurrent.MVar (takeMVar)
import Control.Concurrent.STM (atomically, writeTVar)
import Data.IORef (newIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath ((</>))

import CodeStar.AgentLoop (AgentEnv (..), AgentEvent (..))
import CodeStar.AgentSetup (buildClientForEntry, buildRegistry, buildSystemPrompt, llmErrorLabel)
import Server.AgentRunner (runAgentWithTelemetry)
import CodeStar.Compaction (CompactionState (..), emptyCompactionState)
import CodeStar.Config (Config (..), AgentConfig, BudgetSection (..))
import CodeStar.Config.Convert (toContextConfig, toCompactionConfig, toGuardrailConfig)
import CodeStar.Config.Paths qualified as Paths
import CodeStar.Config.Types (ModelEntry (..))
import CodeStar.Context (ContextParts (..), assemble)
import CodeStar.LLM.Base (LlmError (..), withRetry)
import CodeStar.Memory (MemoryEntry (..), loadMemory, newMemoryStore)
import CodeStar.Permissions (newPermissionStore)
import CodeStar.Platform.Auth (Identity (..))
import CodeStar.Platform.CostTracker (newCostTracker)
import CodeStar.Platform.Sandbox (noSandbox)
import CodeStar.Platform.SessionManager (Session (..), SessionStatus (..))
import CodeStar.RepoMap.Cache (RepoMapCache (..), getOrComputeTags, newRepoMapCache)
import CodeStar.RepoMap.Graph (buildSymbolGraph, defaultWeights, extractTags, pageRank)
import CodeStar.RepoMap.Render (defaultRenderConfig, renderRepoMap)
import CodeStar.RepoMap.Render qualified as RepoMap
import CodeStar.Storage (newBackend)
import CodeStar.Telemetry (TelemetryRecorder (..))
import CodeStar.Telemetry qualified as Tel
import CodeStar.Tools.MCP (connectMcpEndpoints)
import CodeStar.Tools.Read (newReadTracker)
import CodeStar.Tools.Registry (register)
import CodeStar.Tools.TodoList (newTodoStore)
import CodeStar.Transport.Types (AgentEventEnvelope (..), CommandResult (..))
import CodeStar.TreeSitter (GrammarRegistry, loadGrammarRegistry)
import CodeStar.Types (SessionId (..))
import Resilience.Core (defaultRecoveryPolicy, newRecoveryEngine)

import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf)
import System.Directory (doesFileExist, listDirectory)
import System.Timeout (timeout)

spawnAgentSession ::
  AgentConfig ->
  TelemetryRecorder ->
  Session ->
  Identity ->
  (AgentEventEnvelope -> IO ()) ->
  Text ->
  IO CommandResult
spawnAgentSession config recorder session identity eventSinkFn task = do
  let activeEntry = case filter (\m -> m.meName == config.activeModel) config.models of
        (e:_) -> e
        []    -> ModelEntry "default" "anthropic" "claude-sonnet-4-20250514"
                   config.apiKey Nothing Nothing (Just 8192)
  baseClient <- buildClientForEntry config activeEntry
  resEngine <- newRecoveryEngine defaultRecoveryPolicy
  let SessionId sid0 = session.sessionId
      onRetry err attempt = recorder.recordEvent Tel.EvLlmRetry
        { Tel.retryError       = llmErrorLabel err
        , Tel.retryAttempt     = attempt
        , Tel.retryAfterHintMs = case err of
            RateLimited secs -> round (secs * 1000)
            _                -> 0
        , Tel.lrSessionId      = sid0
        }
      client = withRetry resEngine onRetry baseClient
  clientRef <- newIORef client
  tracker <- newReadTracker
  todoStore <- newTodoStore
  globalCfgDir <- Paths.globalConfigDir
  let projDir = Paths.projectDir config.workspacePath
  perms <- newPermissionStore config.workspacePath globalCfgDir
  costTracker <- newCostTracker config.budgets.sessionTokenMax config.budgets.dailyTokenMax
  memStore <- newMemoryStore (projDir </> "memory")
  memEntries <- loadMemory memStore
  let memPaths =
        [ projDir </> "memory" </> "entries" </> Text.unpack e.meId <> ".json"
        | e <- memEntries
        ]

  grammarReg <- loadGrammarRegistry
  cacheBackend <- newBackend (projDir </> "cache")
  let repoCache = newRepoMapCache cacheBackend
  repoMapText <- buildRepoMapSafe grammarReg repoCache config.workspacePath

  let sandbox = noSandbox config.workspacePath
  mcpHandlers <- connectMcpEndpoints recorder config.mcpEndpoints
  let onEdit = Just (repoCache.invalidate)
      registry = foldr register (buildRegistry tracker todoStore sandbox onEdit) mcpHandlers

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

  let waitInput query = do
        atomically $ writeTVar session.status SWaitingForInput
        session.eventSink $
          AgentEventEnvelope
            session.sessionId
            (AgentProgress ("Waiting for input: " <> query))
        takeMVar session.inputMVar

      waitApproval _reason = do
        atomically $ writeTVar session.status SWaitingForApproval
        takeMVar session.approvalMVar

  let env =
        AgentEnv
          { envLlm = clientRef
          , envTools = registry
          , envConfig = config
          , envTelemetry = recorder
          , envOnEvent = \ev -> eventSinkFn (AgentEventEnvelope session.sessionId ev)
          , envGuardrails = grCfg
          , envPermissions = Just perms
          , envCompaction = compCfg
          , envCompState = emptyCompactionState{csRepoMap = repoMapText}
          , envCostTracker = Just costTracker
          , envSessionId = session.sessionId
          , envUserId = identity.userId
          , envGrammarReg = grammarReg
          , envMemoryStore = Just memStore
          , envWaitForInput = Just waitInput
          , envWaitForApproval = Just waitApproval
          , envPendingModel = session.pendingModel
          }

  thread <- runAgentWithTelemetry recorder session identity.userId eventSinkFn env sysPrompt task
  atomically $ writeTVar session.workerThread (Just thread)

  pure CmdOk

-- --------------------------------------------------------------------
-- RepoMap utilities
-- --------------------------------------------------------------------

listWorkspaceFiles :: FilePath -> IO [FilePath]
listWorkspaceFiles root = do
  entries <- listDirectory root
  let visible = filter (not . ("." `isPrefixOf`)) entries
  fmap concat $ forM visible $ \name -> do
    let path = root </> name
    isFile <- doesFileExist path
    if isFile
      then pure [path]
      else listWorkspaceFiles path

buildRepoMapSafe :: GrammarRegistry -> RepoMapCache -> FilePath -> IO Text
buildRepoMapSafe grammarReg repoCache workDir = do
  wsFiles <- listWorkspaceFiles workDir
  result <- timeout 5000000 $ do
    allTags <- fmap concat $ forM wsFiles $ \f -> do
      src <- BS.readFile f
      getOrComputeTags repoCache f (extractTags grammarReg f src)
    let graph = buildSymbolGraph allTags
        scores = pageRank graph [] [] defaultWeights
    pure $!
      renderRepoMap
        allTags
        scores
        graph
        defaultRenderConfig{RepoMap.maxTokens = 4096}
  case result of
    Just repoMap -> pure repoMap
    Nothing -> pure Text.empty
