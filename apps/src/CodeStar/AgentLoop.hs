{- |
Module      : CodeStar.AgentLoop
Description : Core agent control loop: LLM calls, tool dispatch, and turn management.

The agent loop is the heart of CodeStar. It implements the
/sense → reason → act/ cycle that defines an AI coding agent:

1. Call the LLM with the current message history ('callLlm')
2. If the response contains tool calls, execute each one ('executeToolCalls')
3. Append tool results to history and repeat
4. Stop when the LLM produces no tool calls, or when a terminal 'ControlSignal' is reached

= Architecture

The loop is driven by 'loop', which calls 'stepAgent' on each iteration.
A single call to 'stepAgent' represents one full /step/ of the agent:
one LLM call plus all the tool executions that result from it.

'runAgentTurn' drives a full user turn (from the user's task message to a
terminal signal). Multiple turns accumulate history in 'SessionState'.

= Execution modes

Three entry points offer different levels of task decomposition:

  * 'runAgent' — single-turn, single agent loop; for simple tasks
  * 'runAgentWithList' — multi-step sequential plan via "CodeStar.PlanExecution"
  * 'runAgentWithPlan' — multi-step DAG plan via "CodeStar.PlanExecution"

= Safety

Every tool call passes through 'executeTool', which evaluates guardrail
policy before dispatching. The caller provides 'AgentEnv.envWaitForApproval'
to handle 'RequireApproval' decisions interactively.

= Observability

Key telemetry is emitted throughout:

  * @agent.step@ span — one per 'stepAgent' invocation
  * @agent.compaction@ span — when history is compressed
  * @llm.call@ span — one per LLM request
  * @agent.verify@ span — when modified files are syntax-checked
  * 'Tel.EvLlmCall', 'Tel.EvToolStart', 'Tel.EvToolEnd', 'Tel.EvVerification' events
-}
module CodeStar.AgentLoop
  ( AgentEnv (..)
  , ApprovalDecision (..)
  , AgentEvent (..)
  , SessionState (..)
  , emptySession
  , sessionFromEnv
  , runAgent
  , runAgentTurn
  , runAgentWithList
  , runAgentWithPlan
  ) where

import Control.Exception (IOException, try)
import GHC.Clock (getMonotonicTimeNSec)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.JsonSchema.Serialization (encode)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO

import CodeStar.Compaction
  ( CompactionConfig (..)
  , CompactionState (..)
  , compact
  , emptyCompactionState
  , renderHistory
  , shouldCompact
  )
import CodeStar.RepoMap.Render (estimateTokens)
import CodeStar.Config (Config (..), BudgetSection (..))
import CodeStar.Guardrails (GuardrailConfig, evaluate)
import CodeStar.Guardrails qualified as GR
import CodeStar.History (defaultChain, processHistory)
import CodeStar.LLM.Base
import CodeStar.Memory (MemoryStore)
import CodeStar.Permissions (PermissionStore, check)
import CodeStar.PlanExecution
  ( PlanExecutionConfig (..)
  , defaultPlanExecutionConfig
  , runWithPlan
  , runWithPlanDag
  )
import CodeStar.Planning (PlanFingerprint, PlanStep (..))
import CodeStar.Platform.CostTracker (CostTracker, RecordResult (..), record, getCost)
import CodeStar.Signal
  ( assessControlSignal
  , assessWithHistory
  , classifyLlmError
  , classifyToolError
  )
import OTel.Attribute (AttributeValue (..))
import OTel.Attribute qualified as OTelAttr
import CodeStar.Telemetry (TelemetryRecorder (..))
import CodeStar.Telemetry qualified as Tel
import CodeStar.Tools.Registry
import CodeStar.TreeSitter (GrammarRegistry)
import CodeStar.Types
import CodeStar.Verification (VerificationResult (..), defaultVerificationConfig, verify)

-- | The result of a human approval prompt issued when a tool call triggers
-- a 'GR.RequireApproval' guardrail decision.
data ApprovalDecision
  = Approved
    -- ^ The human approved the tool call; execution proceeds normally.
  | Rejected Text
    -- ^ The human rejected the tool call. The @Text@ is the rejection reason,
    --   which is returned to the LLM as a tool error message.
  deriving stock (Eq, Show)

-- | Read-only configuration and services threaded through the entire agent
-- lifecycle. Constructed once per session in the server and passed
-- unchanged to every function in this module.
--
-- Fields are intentionally strict to ensure the environment is fully
-- evaluated before any agent work begins.
data AgentEnv = AgentEnv
  { envLlm :: ModelResolver
    -- ^ Resolves a 'ModelRole' to the concrete LLM client to use.
    --   Coder, Architect, and Summarizer roles may be backed by different
    --   models or providers.
  , envTools :: ToolRegistry
    -- ^ All tools available to the agent in this session, including both
    --   built-in tools and any tools discovered from MCP endpoints.
  , envConfig :: Config
    -- ^ Full configuration: budget limits, compaction settings, guardrail
    --   lists, context token budgets, and server parameters.
  , envTelemetry :: TelemetryRecorder
    -- ^ Telemetry sink. All spans, metrics, and structured log events
    --   produced during the session are emitted through this recorder.
  , envOnEvent :: AgentEvent -> IO ()
    -- ^ Callback for streaming agent progress events to the client
    --   (tokens, tool calls, tool results, completion signals). These are
    --   separate from telemetry — they drive the client UI.
  , envGuardrails :: GuardrailConfig
    -- ^ Policy controlling which tool calls are allowed, denied, or
    --   require explicit human approval before execution.
  , envPermissions :: Maybe PermissionStore
    -- ^ Optional persistent permission store. When present, previously
    --   approved tools bypass the approval prompt on subsequent calls.
  , envCompaction :: CompactionConfig
    -- ^ Controls when compaction triggers and the maximum context size
    --   the compactor targets. See "CodeStar.Compaction".
  , envCompState :: CompactionState
    -- ^ Mutable compaction bookkeeping carried across turns, including
    --   the task description (which must survive compaction) and the
    --   repo map used to contextualise summaries.
  , envCostTracker :: Maybe CostTracker
    -- ^ Optional budget enforcement. When present, token usage is
    --   accumulated after every LLM call and the session is blocked if
    --   the session or daily limit is exceeded.
  , envSessionId :: SessionId
    -- ^ Stable identifier for this session, used as a dimension on all
    --   telemetry events and spans.
  , envUserId :: UserId
    -- ^ Identifier of the user who initiated this session.
  , envGrammarReg :: GrammarRegistry
    -- ^ Tree-sitter grammar registry used by 'verifyStepResult' to
    --   parse and syntax-check files modified during a step.
  , envMemoryStore :: Maybe MemoryStore
    -- ^ Optional persistent memory store. When present, memory entries
    --   are loaded into the system prompt at session start.
  , envWaitForInput :: !(Maybe (Text -> IO Text))
    -- ^ When the LLM emits a 'NeedsInput' signal, this callback is
    --   invoked with the query text. The session blocks until the user
    --   responds. @Nothing@ causes 'NeedsInput' to be treated as a
    --   terminal signal rather than a pause.
  , envWaitForApproval :: !(Maybe (Text -> IO ApprovalDecision))
    -- ^ When a tool call triggers 'GR.RequireApproval', this callback
    --   is invoked with the reason text. @Nothing@ causes the call to
    --   be passed to the LLM as a deferred approval request rather than
    --   blocking.
  }

-- | Events streamed to the client via 'AgentEnv.envOnEvent' as the agent
-- runs. These drive the client UI and are distinct from the telemetry
-- events in "CodeStar.Telemetry".
data AgentEvent
  = AgentToken Text
    -- ^ A token of LLM output, streamed as it arrives.
  | AgentToolCall ToolName Text
    -- ^ The LLM has requested a tool call. The @Text@ is a JSON
    --   representation of the tool arguments, truncated for display.
  | AgentToolResult ToolName Text
    -- ^ A tool call has completed. The @Text@ is the tool's output or
    --   error message.
  | AgentApprovalRequired ToolName Text
    -- ^ A tool call triggered 'GR.RequireApproval'. The client should
    --   prompt the user and call back via the approval MVar.
  | AgentCompacting
    -- ^ Compaction is about to run. The client may display a progress
    --   indicator; the agent will be slower during this step.
  | AgentProgress Text
    -- ^ A free-form progress message, used for example when the agent
    --   is waiting for user input.
  | AgentCostUpdate Int Int
    -- ^ Updated cumulative token counts @(inputTokens, outputTokens)@
    --   after the most recent LLM call.
  | AgentDone ControlSignal
    -- ^ The agent has reached a terminal state. The 'ControlSignal'
    --   describes the outcome: 'Done', 'Blocked', or 'NeedsInput'.
  | AgentError Text
    -- ^ An unrecoverable error occurred. The session will be terminated.
  deriving stock (Show)

-- | All mutable state that persists across steps and turns within a single
-- agent session. 'SessionState' is threaded through the loop as a pure
-- value; each step returns an updated copy rather than mutating in place.
data SessionState = SessionState
  { ssHistory :: !(Seq Message)
    -- ^ The full conversation history between the user and the LLM,
    --   including all tool calls and tool results. This is the context
    --   window content passed to the LLM on each call. May be compacted
    --   by 'maybeCompact' when it grows too large.
  , ssCompState :: !CompactionState
    -- ^ Compaction bookkeeping: the task description, the repo map, and
    --   any state the compactor needs across turns. The task description
    --   in particular must survive compaction so the agent remembers what
    --   it is trying to accomplish.
  , ssTurnCount :: !Int
    -- ^ Number of user turns completed so far. Incremented by
    --   'runAgentTurn' at the start of each new user message.
  }

-- | An empty session with no history. Use 'sessionFromEnv' instead when
-- starting a session that should inherit compaction state from the environment.
emptySession :: SessionState
emptySession =
  SessionState
    { ssHistory = Seq.empty
    , ssCompState = emptyCompactionState
    , ssTurnCount = 0
    }

-- | Construct an initial 'SessionState' from the agent environment,
-- inheriting the compaction state (including the pre-built repo map)
-- from 'AgentEnv.envCompState'. Prefer this over 'emptySession' in
-- production sessions.
sessionFromEnv :: AgentEnv -> SessionState
sessionFromEnv env =
  SessionState
    { ssHistory = Seq.empty
    , ssCompState = env.envCompState
    , ssTurnCount = 0
    }

-- | Transient state that exists only for the duration of a single user turn.
-- Reset to 'TurnState { tsStep = 0, tsRecentOutcomes = [] }' at the start
-- of each call to 'runAgentTurn'.
data TurnState = TurnState
  { tsStep :: !Int
    -- ^ Zero-based index of the current step within the turn. Used as a
    --   dimension on @agent.step@ spans and 'Tel.EvLlmCall' events.
  , tsRecentOutcomes :: ![StepOutcome]
    -- ^ Outcomes from tool calls in previous steps of this turn. Passed
    --   to 'assessWithHistory' to detect repeated failures and upgrade
    --   the control signal accordingly (e.g., multiple consecutive tool
    --   failures escalate to 'Blocked').
  }

-- | The decision made by 'stepAgent' at the end of each step.
data StepAction
  = ContinueWith TurnState
    -- ^ The step completed normally. The updated 'TurnState' carries the
    --   incremented step counter and latest tool outcomes into the next step.
  | Stop ControlSignal
    -- ^ The agent has reached a terminal condition for this turn. The
    --   'ControlSignal' is propagated to the caller of 'loop'.

-- | Run a single user turn: append the user's task message to history,
-- execute the agent loop until a terminal signal is reached, and return
-- the signal together with the updated 'SessionState'.
--
-- The returned 'SessionState' can be passed back into a subsequent call
-- to 'runAgentTurn' to continue the conversation across multiple turns.
--
-- This is the primary entry point for multi-turn sessions. For single-turn
-- use, see 'runAgent'.
runAgentTurn :: AgentEnv -> Text -> SessionState -> Text -> IO (ControlSignal, SessionState)
runAgentTurn env sysPrompt session task =
  let compState' =
        if Text.null session.ssCompState.csTask
          then session.ssCompState{csTask = task}
          else session.ssCompState
      session' =
        session
          { ssHistory = session.ssHistory |> Message User [TextContent task]
          , ssCompState = compState'
          , ssTurnCount = session.ssTurnCount + 1
          }
      turn = TurnState{tsStep = 0, tsRecentOutcomes = []}
   in loop env sysPrompt session' turn

-- | Run the agent on a single task from a fresh session, discarding the
-- resulting 'SessionState'. Equivalent to:
--
-- @
-- fst \<$\> runAgentTurn env sysPrompt (sessionFromEnv env) task
-- @
--
-- Useful when the caller does not need to continue the conversation.
runAgent :: AgentEnv -> Text -> Text -> IO ControlSignal
runAgent env sysPrompt task =
  fst <$> runAgentTurn env sysPrompt (sessionFromEnv env) task

-- | Run the agent on a complex objective using a sequentially-executed
-- plan. The Architect and Planner LLM roles decompose the objective into
-- steps; the Coder role executes each step via 'runAgent'.
--
-- 'triedFps' is the set of plan fingerprints that have already been
-- attempted. The planner avoids regenerating identical plans when replanning.
--
-- See 'CodeStar.PlanExecution.runWithPlan' for the planning implementation.
runAgentWithList :: AgentEnv -> Text -> ObjectiveSpec -> Set PlanFingerprint -> IO ControlSignal
runAgentWithList env sysPrompt spec triedFps = do
  let arch = env.envLlm Architect
      planner = env.envLlm Architect
      cfg = defaultPlanExecutionConfig{repoContext = env.envCompState.csRepoMap}
      executeStep step = do
        prompt <- buildStepPrompt sysPrompt step
        runAgent env prompt step.description
  runWithPlan env.envTelemetry arch planner (env.envLlm Coder) cfg spec triedFps executeStep

-- | Run the agent on a complex objective using a DAG-scheduled plan.
-- Steps with no unresolved dependencies are eligible for execution;
-- the scheduler runs them in dependency order rather than strict sequence.
--
-- Prefer this over 'runAgentWithList' when the task decomposes into steps
-- that are genuinely independent (e.g., editing unrelated files).
--
-- See 'CodeStar.PlanExecution.runWithPlanDag' for the planning implementation.
runAgentWithPlan :: AgentEnv -> Text -> ObjectiveSpec -> Set PlanFingerprint -> IO ControlSignal
runAgentWithPlan env sysPrompt spec triedFps = do
  let arch = env.envLlm Architect
      planner = env.envLlm Architect
      cfg = defaultPlanExecutionConfig{repoContext = env.envCompState.csRepoMap}
      executeStep step = do
        prompt <- buildStepPrompt sysPrompt step
        runAgent env prompt step.description
  runWithPlanDag env.envTelemetry arch planner (env.envLlm Coder) cfg spec triedFps executeStep

-- | The inner step loop. Drives repeated calls to 'stepAgent' until
-- 'stepAgent' returns a 'Stop' action. Handles 'NeedsInput' by calling
-- 'AgentEnv.envWaitForInput' and appending the user's response to history
-- before continuing.
--
-- Terminates immediately (with 'Blocked') when the configured maximum step
-- count is reached, preventing infinite loops on tasks the agent cannot
-- complete.
loop :: AgentEnv -> Text -> SessionState -> TurnState -> IO (ControlSignal, SessionState)
loop env sysPrompt session turn
  | let b :: BudgetSection = env.envConfig.budgets, turn.tsStep >= b.maxSteps = do
      sig <- finish env (Blocked "Max steps exceeded")
      pure (sig, session)
  | otherwise = do
      (action, session') <- stepAgent env sysPrompt session turn
      let SessionId sid = env.envSessionId
          histTokens = estimateTokens (renderHistory session'.ssHistory)
      env.envTelemetry.recordEvent Tel.EvHistorySize
        { Tel.hsSessionId = sid
        , Tel.hsTokensEst = histTokens
        }
      case action of
        Stop (NeedsInput query) | Just waitFn <- env.envWaitForInput -> do
          response <- waitFn query
          let session'' = session'{ssHistory = session'.ssHistory |> Message User [TextContent response]}
          loop env sysPrompt session'' turn{tsStep = turn.tsStep + 1}
        Stop signal -> do
          sig <- finish env signal
          pure (sig, session')
        ContinueWith turn' -> loop env sysPrompt session' turn'

-- | Execute a single agent step: optionally compact history, call the LLM,
-- and process the response. Emits an @agent.step@ span covering the full
-- step and a 'Tel.EvHistorySize' event afterwards.
--
-- Returns 'ContinueWith' if the LLM made tool calls that were executed, or
-- 'Stop' if a terminal signal was reached (no tool calls, LLM error, or
-- budget exhaustion).
stepAgent :: AgentEnv -> Text -> SessionState -> TurnState -> IO (StepAction, SessionState)
stepAgent env sysPrompt session turn = do
  let SessionId sid = env.envSessionId
  stepSpan <- env.envTelemetry.startSpan "agent.step"
    [ ("session.id",  OTelAttr.StringValue sid)
    , ("step.number", OTelAttr.Int64Value (fromIntegral turn.tsStep))
    , ("turn.number", OTelAttr.Int64Value (fromIntegral session.ssTurnCount))
    ]
  result <- do
    (session', compacted) <- maybeCompact env session
    when compacted (env.envOnEvent AgentCompacting)
    let processedHistory = processHistory defaultChain session'.ssHistory
    llmResult <- callLlm env sysPrompt turn.tsStep session'.ssTurnCount session'.ssHistory processedHistory
    case llmResult of
      Left signal -> pure (Stop signal, session')
      Right (response, rawHistory') ->
        processResponse env session'{ssHistory = rawHistory'} turn response
  env.envTelemetry.endSpan stepSpan
  pure result

-- | Compact the conversation history if it has grown beyond the configured
-- threshold. Uses the Summarizer LLM role to produce a condensed history.
--
-- Returns the updated 'SessionState' and a flag indicating whether
-- compaction ran. Emits an @agent.compaction@ span and either a
-- 'Tel.EvCompaction' or 'Tel.EvCompactionFailed' event.
--
-- On compaction failure the original session is returned unchanged so the
-- agent can continue; the failure is recorded in telemetry but does not
-- abort the step.
maybeCompact :: AgentEnv -> SessionState -> IO (SessionState, Bool)
maybeCompact env session
  | shouldCompact env.envCompaction session.ssHistory = do
      let lenBefore = Seq.length session.ssHistory
          SessionId sid = env.envSessionId
      compSpan <- env.envTelemetry.startSpan "agent.compaction"
        [ ("session.id",       OTelAttr.StringValue sid)
        , ("history.len_before", OTelAttr.Int64Value (fromIntegral lenBefore))
        ]
      t0 <- getMonotonicTimeNSec
      result <- compact (env.envLlm Summarizer) session.ssCompState session.ssHistory Nothing
      t1 <- getMonotonicTimeNSec
      let durMs = fromIntegral ((t1 - t0) `div` 1_000_000) :: Double
      case result of
        Left err -> do
          env.envTelemetry.endSpan compSpan
          env.envTelemetry.recordEvent $ Tel.EvCompactionFailed
            { Tel.compactionError = Text.pack (show err)
            , Tel.historyLen = lenBefore
            , Tel.cfSessionId = sid
            }
          pure (session, False)
        Right hist -> do
          let lenAfter = Seq.length hist
          env.envTelemetry.setSpanAttrTyped compSpan "history.len_after" (OTelAttr.Int64Value (fromIntegral lenAfter))
          env.envTelemetry.endSpan compSpan
          env.envTelemetry.recordEvent $ Tel.EvCompaction
            { Tel.historyLenBefore = lenBefore
            , Tel.historyLenAfter  = lenAfter
            , Tel.compactionDurMs  = durMs
            , Tel.compSessionId    = sid
            }
          pure (session{ssHistory = hist}, True)
  | otherwise = pure (session, False)

-- | Call the Coder LLM with the processed history and return either a
-- terminal control signal (on error or budget exhaustion) or the
-- completion response together with the updated raw history.
--
-- Emits an @llm.call@ span and a 'Tel.EvLlmCall' event on success.
-- On LLM error, sets the span error status and classifies the error
-- into a 'ControlSignal' via 'assessControlSignal'.
--
-- 'rawHistory' and 'processedHistory' are kept separate because
-- 'processHistory' may transform messages for the LLM (e.g. applying
-- cache-control markers or truncating observations) without losing the
-- original data that must be stored.
callLlm :: AgentEnv -> Text -> Int -> Int -> Seq Message -> Seq Message -> IO (Either ControlSignal (CompletionResponse, Seq Message))
callLlm env sysPrompt stepNum turnNum rawHistory processedHistory = do
  let client = env.envLlm Coder
      SessionId sid = env.envSessionId
      UserId uid = env.envUserId
      req = CompletionRequest
        { messages = toList processedHistory
        , systemPrompt = Just sysPrompt
        , tools = toolSchemas env.envTools
        , maxTokens = 8192
        , temperature = Nothing
        , topP = Nothing
        }
  llmSpan <- env.envTelemetry.startSpan "llm.call"
    [ ("role",        StringValue "coder")
    , ("session.id",  StringValue sid)
    , ("user.id",     StringValue uid)
    , ("model.id",    StringValue client.clientInfo.modelId)
    , ("step.number", Int64Value (fromIntegral stepNum))
    , ("turn.number", Int64Value (fromIntegral turnNum))
    ]
  t0 <- getMonotonicTimeNSec
  result <- client.stream req (forwardTokens env)
  t1 <- getMonotonicTimeNSec
  case result of
    Left err -> do
      let errMsg = Text.pack (show err)
      env.envTelemetry.setSpanError llmSpan errMsg
      env.envTelemetry.setSpanAttr llmSpan "error.type" (llmErrorConstructor err)
      case err of
        RateLimited secs ->
          env.envTelemetry.setSpanAttr llmSpan "retry_after.seconds" (Text.pack (show secs))
        _ -> pure ()
      env.envTelemetry.endSpan llmSpan
      env.envOnEvent (AgentError ("LLM error: " <> errMsg))
      let cls = classifyLlmError err
      pure $ Left (assessControlSignal (StepFailure cls errMsg))
    Right response -> do
      env.envTelemetry.endSpan llmSpan
      env.envTelemetry.recordEvent $
        Tel.EvLlmCall
          { Tel.modelRole           = Coder
          , Tel.inputTokens         = fromIntegral response.usage.inputTokens
          , Tel.outputTokens        = fromIntegral response.usage.outputTokens
          , Tel.cacheCreationTokens = fromIntegral response.usage.cacheCreationTokens
          , Tel.cacheReadTokens     = fromIntegral response.usage.cacheReadTokens
          , Tel.durationMs          = fromIntegral ((t1 - t0) `div` 1_000_000)
          , Tel.modelId             = client.clientInfo.modelId
          , Tel.stepNumber          = stepNum
          , Tel.turnNumber          = turnNum
          }
      env.envOnEvent $
        AgentCostUpdate
          (fromIntegral response.usage.inputTokens)
          (fromIntegral response.usage.outputTokens)
      budgetResult <- checkBudget env response client.clientInfo.modelId
      case budgetResult of
        Just reason -> pure (Left (Blocked reason))
        Nothing -> do
          let assistantMsg = Message Assistant response.responseContent
          pure $ Right (response, rawHistory |> assistantMsg)

-- | Dispatch on the LLM response. If the response contains tool calls,
-- delegates to 'processToolCalls'. If there are no tool calls the LLM
-- has finished reasoning and the step terminates with the stop-reason
-- signal from 'completionSignal'.
processResponse :: AgentEnv -> SessionState -> TurnState -> CompletionResponse -> IO (StepAction, SessionState)
processResponse env session turn response =
  case extractToolCalls response.responseContent of
    [] -> pure (Stop (completionSignal response.stopReason), session)
    calls -> processToolCalls env session turn calls

-- | Execute a batch of tool calls, verify the results, and decide whether
-- to continue or stop.
--
-- Tool calls in a single LLM response are executed sequentially in the
-- order the LLM listed them. Results are appended to history as a single
-- 'User' message containing all tool results.
--
-- The worst-case 'ControlSignal' across all tool outcomes determines
-- whether the step continues or stops (e.g. a single 'Blocked' outcome
-- stops the turn even if other tools succeeded).
processToolCalls :: AgentEnv -> SessionState -> TurnState -> [ToolCall] -> IO (StepAction, SessionState)
processToolCalls env session turn calls = do
  stepResult <- executeToolCalls env turn.tsRecentOutcomes calls
  verifiedSignal <- verifyStepResult env calls stepResult.signal
  let session' = session{ssHistory = session.ssHistory |> toolResultMessage stepResult}
      turn' = turn
        { tsStep = turn.tsStep + 1
        , tsRecentOutcomes = turn.tsRecentOutcomes ++ [stepResult.outcome]
        }
  pure $ case verifiedSignal of
    Continue -> (ContinueWith turn', session')
    NeedsInput _ -> (ContinueWith turn', session')
    signal -> (Stop signal, session')

-- | The aggregated result of executing all tool calls in a single LLM response.
data StepResult = StepResult
  { signal :: ControlSignal
    -- ^ The worst-case signal derived from all tool outcomes, used to decide
    --   whether the turn continues or stops.
  , outcome :: StepOutcome
    -- ^ The outcome of the last tool call, carried into 'TurnState' for
    --   history-aware signal assessment in future steps.
  , toolResults :: [Content]
    -- ^ The raw tool result content blocks to be appended to the
    --   conversation history as a 'User' message.
  }

-- | Execute all tool calls in a batch, combining their outcomes into a
-- single 'StepResult'. The 'worstSignal' function aggregates individual
-- outcomes: a failure in any tool may escalate the overall signal.
executeToolCalls :: AgentEnv -> [StepOutcome] -> [ToolCall] -> IO StepResult
executeToolCalls env recentOutcomes calls = do
  results <- mapM (executeTool env recentOutcomes) calls
  let outcomes = map (\(o, _) -> o) results
      signals = map (\o -> assessWithHistory recentOutcomes o) outcomes
  pure StepResult
    { signal = worstSignal signals
    , outcome = last outcomes
    , toolResults = map snd results
    }

-- | Execute a single tool call, enforcing guardrail policy before dispatch.
--
-- Evaluation order:
--
-- 1. Check 'AgentEnv.envPermissions' — pre-approved tools bypass guardrails
-- 2. Evaluate 'AgentEnv.envGuardrails' — may produce 'GR.Allow', 'GR.Deny',
--    or 'GR.RequireApproval'
-- 3. On 'GR.RequireApproval', invoke 'AgentEnv.envWaitForApproval' if
--    available, otherwise return a 'StepNeedsReplan' outcome
-- 4. On 'GR.Allow', dispatch to 'runTool'
--
-- Emits 'Tel.EvToolStart', 'Tel.EvGuardrailDecision', and (via 'runTool')
-- 'Tel.EvToolEnd'.
executeTool :: AgentEnv -> [StepOutcome] -> ToolCall -> IO (StepOutcome, Content)
executeTool env _recentOutcomes tc = do
  env.envOnEvent (AgentToolCall tc.toolName (Text.pack (show tc.arguments)))
  env.envTelemetry.recordEvent $
    Tel.EvToolStart
      { Tel.toolName = unToolName tc.toolName
      , Tel.inputSummary = Text.take 200 (Text.pack (show tc.arguments))
      }
  permitted <- checkPermission env tc
  let SessionId sid = env.envSessionId
      UserId uid = env.envUserId
      decision = if permitted then GR.Allow else evaluate env.envGuardrails env.envTools tc
      (decisionLabel, reasonText, denied) = case decision of
        GR.Allow             -> ("allow", "", False)
        GR.RequireApproval r -> ("require_approval", r, False)
        GR.Deny r            -> ("deny", r, True)
  env.envTelemetry.recordEvent $ Tel.EvGuardrailDecision
    { Tel.toolName          = unToolName tc.toolName
    , Tel.guardrailDecision = decisionLabel
    , Tel.guardrailReason   = reasonText
    , Tel.isDenied          = denied
    , Tel.sessionId         = sid
    , Tel.userId            = uid
    }
  case decision of
    GR.Deny reason -> do
      let msg = "Denied: " <> reason
      env.envOnEvent (AgentToolResult tc.toolName msg)
      pure (StepFailure Policy reason, toToolResult tc.toolCallId msg True)
    GR.RequireApproval reason -> do
      env.envOnEvent (AgentApprovalRequired tc.toolName reason)
      case env.envWaitForApproval of
        Just waitFn -> do
          approvalResult <- waitFn reason
          case approvalResult of
            Approved -> runTool env tc
            Rejected msg -> pure (StepFailure Policy msg, toToolResult tc.toolCallId msg True)
        Nothing ->
          pure (StepNeedsReplan ("Approval required: " <> reason), toToolResult tc.toolCallId reason False)
    GR.Allow -> runTool env tc

-- | Dispatch a tool call to the registry and record the result. Measures
-- wall-clock duration and emits 'Tel.EvToolEnd' with success/failure status.
-- Does not enforce guardrail policy — callers must do that via 'executeTool'.
runTool :: AgentEnv -> ToolCall -> IO (StepOutcome, Content)
runTool env tc = do
  let input = ToolInput{arguments = extractArgs tc.arguments}
  t0 <- getMonotonicTimeNSec
  result <- dispatch env.envTools tc.toolName input
  t1 <- getMonotonicTimeNSec
  let durationMs = fromIntegral ((t1 - t0) `div` 1_000_000)
  case result of
    Left err -> do
      let msg = Text.pack (show err)
      env.envTelemetry.recordEvent $ Tel.EvToolEnd
        { Tel.toolName   = unToolName tc.toolName
        , Tel.success    = False
        , Tel.durationMs = durationMs
        , Tel.filePath   = toolFilePath tc
        , Tel.errorReason = Just (Text.take 200 msg)
        }
      env.envOnEvent (AgentToolResult tc.toolName ("ERROR: " <> msg))
      let cls = classifyToolError err
      pure (StepFailure cls msg, toToolResult tc.toolCallId msg True)
    Right output -> do
      env.envTelemetry.recordEvent $ Tel.EvToolEnd
        { Tel.toolName   = unToolName tc.toolName
        , Tel.success    = True
        , Tel.durationMs = durationMs
        , Tel.filePath   = toolFilePath tc
        , Tel.errorReason = Nothing
        }
      env.envOnEvent (AgentToolResult tc.toolName output.content)
      pure (StepSuccess output.content, toToolResult tc.toolCallId output.content False)

-- | Check whether the tool has been pre-approved via the permission store.
-- Returns @True@ if the permission store exists and has a stored approval
-- for this tool name, bypassing the guardrail evaluation in 'executeTool'.
checkPermission :: AgentEnv -> ToolCall -> IO Bool
checkPermission env tc =
  case env.envPermissions of
    Nothing -> pure False
    Just store -> check store (unToolName tc.toolName)

-- | Syntax-check any files modified by the tool calls in this step.
-- Uses tree-sitter grammars for each file's language.
--
-- Only runs when at least one @edit@ or @write@ tool call is present in
-- the batch. Files with no registered grammar are skipped.
--
-- Signal transformation:
--
--   * 'VerificationPassed' → 'Done' (replaces the incoming signal)
--   * 'VerificationFailed' → emits the error as 'AgentError', returns 'Continue'
--     so the agent can attempt a fix
--   * 'VerificationPartial' → passes the incoming signal through unchanged
--
-- Emits an @agent.verify@ span and a 'Tel.EvVerification' event.
verifyStepResult :: AgentEnv -> [ToolCall] -> ControlSignal -> IO ControlSignal
verifyStepResult env calls signal = do
  let modifiedFiles = extractModifiedPaths calls
  if null modifiedFiles
    then pure signal
    else do
      let SessionId sid = env.envSessionId
      verSpan <- env.envTelemetry.startSpan "agent.verify"
        [ ("session.id",   OTelAttr.StringValue sid)
        , ("files.count",  OTelAttr.Int64Value (fromIntegral (length modifiedFiles)))
        ]
      t0 <- getMonotonicTimeNSec
      result <- verify env.envGrammarReg env.envTools defaultVerificationConfig modifiedFiles
      t1 <- getMonotonicTimeNSec
      let durMs = fromIntegral ((t1 - t0) `div` 1_000_000) :: Double
          (outcome, reason, syntaxOk) = case result of
            VerificationFailed r  -> ("failed", Just r, False)
            VerificationPassed _  -> ("passed", Nothing, True)
            VerificationPartial _ -> ("partial", Nothing, True)
      env.envTelemetry.setSpanAttr verSpan "verify.outcome" outcome
      env.envTelemetry.endSpan verSpan
      env.envTelemetry.recordEvent $ Tel.EvVerification
        { Tel.verifiedFiles    = map Text.pack modifiedFiles
        , Tel.syntaxOk         = syntaxOk
        , Tel.verifyOutcome    = outcome
        , Tel.verifyReason     = reason
        , Tel.verifyDurationMs = durMs
        , Tel.sessionId        = sid
        }
      case result of
        VerificationFailed reason' -> do
          env.envOnEvent (AgentError ("Verification failed: " <> reason'))
          pure Continue
        VerificationPassed evidence -> pure (Done evidence)
        VerificationPartial _evidence -> pure signal

-- | Extract the @path@ argument from a tool call's JSON arguments, if
-- present and a string. Used to collect the set of files modified by
-- @edit@ and @write@ tool calls for 'verifyStepResult'.
toolFilePath :: ToolCall -> Maybe Text
toolFilePath tc = case Map.lookup "path" (extractArgs tc.arguments) of
  Just (String p) -> Just p
  _ -> Nothing

-- | Collect the file paths from all @edit@ and @write@ tool calls in a
-- batch. Only calls with a @path@ string argument contribute. Used to
-- determine which files to syntax-check in 'verifyStepResult'.
extractModifiedPaths :: [ToolCall] -> [FilePath]
extractModifiedPaths calls =
  [ Text.unpack path
  | tc <- calls
  , unToolName tc.toolName `elem` ["edit", "write"]
  , path <- case Map.lookup "path" (extractArgs tc.arguments) of
      Just (String p) -> [p]
      _ -> []
  ]

-- | Record the token usage from the most recent LLM call against the
-- session and daily budgets. Returns @Nothing@ if within budget, or
-- @Just reason@ if a limit has been exceeded, where @reason@ is a
-- human-readable message suitable for use as a 'Blocked' signal.
--
-- Also emits a 'Tel.EvCostUpdate' event with cumulative token counts and
-- a 'Tel.EvBudgetExhausted' event when a limit is first exceeded.
checkBudget :: AgentEnv -> CompletionResponse -> Text -> IO (Maybe Text)
checkBudget env response modelId = case env.envCostTracker of
  Nothing -> pure Nothing
  Just ct -> do
    result <- record ct env.envSessionId env.envUserId modelId
      (fromIntegral response.usage.inputTokens) (fromIntegral response.usage.outputTokens)
    (totalIn, totalOut, costUsd) <- getCost ct env.envSessionId
    let SessionId sid = env.envSessionId
        UserId uid    = env.envUserId
    env.envTelemetry.recordEvent $ Tel.EvCostUpdate
      { Tel.totalInputTokens  = fromIntegral totalIn
      , Tel.totalOutputTokens = fromIntegral totalOut
      , Tel.estimatedCostUsd  = costUsd
      , Tel.cuSessionId       = sid
      }
    case result of
      Recorded -> pure Nothing
      BudgetExhausted r -> do
        env.envTelemetry.recordEvent $ Tel.EvBudgetExhausted
          { Tel.beLimitType   = r
          , Tel.beSessionId   = sid
          , Tel.beUserId      = uid
          , Tel.beTotalTokens = fromIntegral totalIn + fromIntegral totalOut
          }
        pure (Just r)

-- | Emit the terminal telemetry event and the 'AgentDone' client event,
-- then return the signal unchanged. Called by 'loop' on every exit path.
finish :: AgentEnv -> ControlSignal -> IO ControlSignal
finish env signal = do
  env.envTelemetry.recordEvent (Tel.EvControlSignal signal)
  env.envOnEvent (AgentDone signal)
  pure signal

-- | Stream an LLM token event to the client via 'AgentEnv.envOnEvent'.
-- Non-token events (e.g. usage events) are silently ignored.
forwardTokens :: AgentEnv -> CompletionEvent -> IO ()
forwardTokens env (EventToken t) = env.envOnEvent (AgentToken t)
forwardTokens _ _ = pure ()

-- | Convert an LLM stop reason to a 'ControlSignal'. 'EndTurn' (the normal
-- case) is treated as 'Done' with unchecked evidence; all other stop reasons
-- ('MaxTokens', 'ToolUse', 'StopSequence') are treated as 'Continue',
-- prompting another step.
completionSignal :: StopReason -> ControlSignal
completionSignal EndTurn =
  Done Evidence{testsPass = NotChecked, buildSucceeds = NotChecked, filesVerified = [], regressions = []}
completionSignal _ = Continue

toolResultMessage :: StepResult -> Message
toolResultMessage sr = Message User sr.toolResults

toToolResult :: ToolCallId -> Text -> Bool -> Content
toToolResult callId body err =
  ToolResultContent ToolResult{toolResultId = callId, resultBody = body, isError = err}

extractToolCalls :: [Content] -> [ToolCall]
extractToolCalls = concatMap go
 where
  go (ToolUseContent tc) = [tc]
  go _ = []

extractArgs :: Value -> Map Text Value
extractArgs (Object o) = Map.mapKeys Key.toText (KM.toMap o)
extractArgs _ = Map.empty

toolSchemas :: ToolRegistry -> [Value]
toolSchemas reg = map toToolValue (listTools reg)
 where
  toToolValue def = object
    [ "name" .= unToolName def.name
    , "description" .= def.description
    , "input_schema" .= encode def.parameters
    ]

buildStepPrompt :: Text -> PlanStep -> IO Text
buildStepPrompt basePrompt step = do
  fileContents <- mapM loadContextFile step.contextFiles
  let filesSection =
        if null fileContents
          then Text.empty
          else "\n\n## Context Files\n\n"
            <> Text.intercalate "\n\n"
              [ "### " <> Text.pack p <> "\n\n```\n" <> c <> "\n```"
              | (p, c) <- fileContents
              ]
  pure $ basePrompt <> "\n\n## Current Step\n\n" <> step.description <> filesSection

loadContextFile :: FilePath -> IO (FilePath, Text)
loadContextFile path = do
  result <- try (Text.IO.readFile path) :: IO (Either IOException Text)
  case result of
    Left _ -> pure (path, "[file not found]")
    Right c -> pure (path, c)

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = pure ()

-- | Produce a low-cardinality string label for an 'LlmError' constructor,
-- suitable for use as a span attribute value. Used to annotate the
-- @llm.call@ span's @error.type@ attribute without embedding
-- variable-length error messages into the attribute.
llmErrorConstructor :: LlmError -> Text
llmErrorConstructor (RateLimited _)       = "RateLimited"
llmErrorConstructor (AuthenticationFailed _) = "AuthenticationFailed"
llmErrorConstructor (ContextTooLong _ _)  = "ContextTooLong"
llmErrorConstructor (ContentFiltered _)   = "ContentFiltered"
llmErrorConstructor (InvalidRequest _)    = "InvalidRequest"
llmErrorConstructor (ProviderError _)     = "ProviderError"
llmErrorConstructor (NetworkError _)      = "NetworkError"
