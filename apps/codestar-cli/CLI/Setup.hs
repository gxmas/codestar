{- |
= CLI.Setup — resource initialisation

This module is the __dependency-injection root__ of the CLI.  Everything
the agent needs (LLM client, tool registry, repo-map, memory, permissions,
telemetry …) is wired together here and packaged into 'CliResources' so
the rest of the application stays decoupled from construction details.

Key design points worth studying:

  * __Retry wrapper__: the raw LLM client is wrapped with 'withRetry' so
    rate-limit and transient errors are handled transparently.
  * __Repo-map worker__: indexing runs in a background thread; the agent
    always gets the freshest available snapshot.
  * __Event callback__: 'handleEvent' is the bridge between the agent's
    internal event stream and the terminal (streaming tokens, tool traces …).
  * __Shutdown action__: 'crShutdown' is an @IO ()@ that the caller must
    invoke (typically with 'Control.Exception.finally') to flush telemetry
    and stop background threads.
-}
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

-- | All live resources needed to run the agent in CLI mode.
-- Fields are strict to avoid space leaks from unevaluated thunks.
data CliResources = CliResources
  { crEnv       :: !AgentEnv
    -- ^ The complete agent environment: LLM client, tool registry,
    --   compaction config, guardrails, etc.  Passed unchanged into
    --   'CodeStar.AgentLoop.runAgent' / 'CodeStar.AgentLoop.runAgentTurn'.
  , crSysPrompt :: !Text
    -- ^ The assembled system prompt.  Built once from the config and
    --   injected into every LLM request as the leading message.
  , crCostRef   :: !(IORef (Int, Int))
    -- ^ Mutable accumulator for @(inputTokens, outputTokens)@.  Updated
    --   by 'handleEvent' on every 'AgentCostUpdate' event; read by
    --   the @\/cost@ slash command.
  , crWorker    :: !RepoMapWorker
    -- ^ Background thread that keeps the repo-map index up to date.
    --   The REPL pulls the latest snapshot before each agent turn so
    --   newly edited files are reflected in the context window.
  , crShutdown  :: !(IO ())
    -- ^ Run this action when the process is about to exit.  Flushes
    --   the telemetry exporter and stops the repo-map worker thread.
  }

-- --------------------------------------------------------------------
-- Public API
-- --------------------------------------------------------------------

-- | Load and validate the TOML configuration, printing the error and
-- exiting with a non-zero code on failure.  The "OrDie" suffix is a
-- Haskell convention for functions that terminate the process on error
-- rather than returning an 'Either'.
loadConfigOrDie :: RunArgs -> IO Config
loadConfigOrDie runArgs = do
  configResult <- loadConfig runArgs
  either
    (\e -> Text.IO.hPutStr stderr (Text.pack (show e) <> "\n") >> exitFailure)
    pure
    configResult

-- | Construct every subsystem the agent needs and bundle them into
-- 'CliResources'.
--
-- Construction order matters — later steps depend on earlier ones:
--
-- 1. __Telemetry recorder__ — created first so every subsequent step
--    can emit spans/events.
-- 2. __LLM client__ — the raw HTTP client is wrapped with a retry layer
--    that handles rate limits and transient errors.
-- 3. __Tool registry__ — built from the configured MCP endpoints plus
--    the built-in tools (file read, shell, todo list …).
-- 4. __Repo-map worker__ — starts background Tree-sitter indexing of the
--    workspace; the initial snapshot is passed into 'AgentEnv'.
-- 5. __Context assembly__ — 'assemble' combines the system prompt
--    template, repo map, and memory entries into the final system prompt.
-- 6. __AgentEnv__ — the record that the agent loop reads on every turn.
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

-- | Translate agent-loop events into terminal output.
--
-- The agent loop is __event-driven__: rather than blocking on the LLM
-- and returning a final answer, it emits a stream of 'AgentEvent' values
-- while it works.  This callback is the CLI's subscriber:
--
--   * 'AgentToken' — a single streamed text token; printed immediately
--     so the user sees output as it arrives (no waiting for the full
--     response).
--   * 'AgentToolCall' / 'AgentToolResult' — tool invocations are shown
--     with @→@ / @←@ arrows so the user can follow the agent's reasoning.
--   * 'AgentCostUpdate' — token counts are accumulated; @\/cost@ reads them.
--   * 'AgentApprovalRequired' — the agent has paused and is waiting for
--     the user to @\/approve@ or @\/reject@ a tool call.
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


