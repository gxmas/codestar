{- |
= Server.SessionSetup — per-session resource initialisation

Each @CmdStart@ command allocates a __fresh, isolated set of resources__
for the new agent session.  This module performs that allocation and wires
everything together before handing off to 'runAgentWithTelemetry'.

Compared to the CLI's "CLI.Setup", session setup has two additional concerns:

  * __Input\/approval blocking__: the agent loop can pause and wait for the
    client to send @CmdRespond@ or @CmdApprove@\/@CmdReject@.  This is
    implemented with @MVar@s stored in the 'Session' record; the
    @waitInput@ and @waitApproval@ callbacks block on them.

  * __Event forwarding__: agent events are wrapped in 'AgentEventEnvelope'
    (which tags them with the session ID) and sent back to the client as
    JSON-RPC notifications via @eventSinkFn@.

The heavy construction pattern here (many @let@ bindings, sequential IO
actions) is intentional: each step produces a value used by the next, so
there is no safe way to parallelise them.
-}
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
import CodeStar.RepoMap.Build (buildRepoMapSafe)
import CodeStar.RepoMap.Cache (RepoMapCache (..), newRepoMapCache)
import CodeStar.Storage (newBackend)
import CodeStar.Telemetry (TelemetryRecorder (..))
import CodeStar.Telemetry qualified as Tel
import CodeStar.Tools.MCP (connectMcpEndpoints)
import CodeStar.Tools.Read (newReadTracker)
import CodeStar.Tools.Registry (register)
import CodeStar.Tools.TodoList (newTodoStore)
import CodeStar.Transport.Types (AgentEventEnvelope (..), CommandResult (..))
import CodeStar.TreeSitter (loadGrammarRegistry)
import CodeStar.Types (SessionId (..))
import Resilience.Core (defaultRecoveryPolicy, newRecoveryEngine)


-- | Initialise all per-session resources and spawn the agent thread.
--
-- This is called once per @CmdStart@ command.  It:
--
-- 1. Selects the active model entry from config and builds an LLM client
--    wrapped with the retry/recovery engine.
-- 2. Allocates per-session state: read tracker, todo store, permission
--    store, cost tracker, memory store.
-- 3. Loads Tree-sitter grammars and builds the initial repo-map snapshot
--    synchronously (so the first agent turn has codebase context).
-- 4. Connects to MCP endpoints and registers all tools.
-- 5. Assembles the system prompt from template, repo map, and memory.
-- 6. Constructs the 'AgentEnv' with blocking callbacks for
--    @waitInput@ and @waitApproval@ — these are the hooks that allow the
--    server to pause the agent and wait for client responses.
-- 7. Calls 'runAgentWithTelemetry' to start the agent on a background
--    thread and stores the thread handle in the session for cancellation.
--
-- Returns 'CmdOk' immediately; the agent runs asynchronously and sends
-- progress back via @eventSinkFn@.
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

