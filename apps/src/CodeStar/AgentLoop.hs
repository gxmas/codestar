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
  , shouldCompact
  )
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
import CodeStar.Platform.CostTracker (CostTracker, RecordResult (..), record)
import CodeStar.Signal
  ( assessControlSignal
  , assessWithHistory
  , classifyLlmError
  , classifyToolError
  )
import CodeStar.Telemetry (TelemetryRecorder (..))
import CodeStar.Telemetry qualified as Tel
import CodeStar.Tools.Registry
import CodeStar.TreeSitter (GrammarRegistry)
import CodeStar.Types
import CodeStar.Verification (VerificationResult (..), defaultVerificationConfig, verify)

data ApprovalDecision = Approved | Rejected Text
  deriving stock (Eq, Show)

data AgentEnv = AgentEnv
  { envLlm :: ModelResolver
  , envTools :: ToolRegistry
  , envConfig :: Config
  , envTelemetry :: TelemetryRecorder
  , envOnEvent :: AgentEvent -> IO ()
  , envGuardrails :: GuardrailConfig
  , envPermissions :: Maybe PermissionStore
  , envCompaction :: CompactionConfig
  , envCompState :: CompactionState
  , envCostTracker :: Maybe CostTracker
  , envSessionId :: SessionId
  , envUserId :: UserId
  , envGrammarReg :: GrammarRegistry
  , envMemoryStore :: Maybe MemoryStore
  , envWaitForInput :: !(Maybe (Text -> IO Text))
  , envWaitForApproval :: !(Maybe (Text -> IO ApprovalDecision))
  }

data AgentEvent
  = AgentToken Text
  | AgentToolCall ToolName Text
  | AgentToolResult ToolName Text
  | AgentApprovalRequired ToolName Text
  | AgentCompacting
  | AgentProgress Text
  | AgentCostUpdate Int Int
  | AgentDone ControlSignal
  | AgentError Text
  deriving stock (Show)

data SessionState = SessionState
  { ssHistory :: !(Seq Message)
  , ssCompState :: !CompactionState
  , ssTurnCount :: !Int
  }

emptySession :: SessionState
emptySession =
  SessionState
    { ssHistory = Seq.empty
    , ssCompState = emptyCompactionState
    , ssTurnCount = 0
    }

sessionFromEnv :: AgentEnv -> SessionState
sessionFromEnv env =
  SessionState
    { ssHistory = Seq.empty
    , ssCompState = env.envCompState
    , ssTurnCount = 0
    }

data TurnState = TurnState
  { tsStep :: !Int
  , tsRecentOutcomes :: ![StepOutcome]
  }

data StepAction
  = ContinueWith TurnState
  | Stop ControlSignal

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

runAgent :: AgentEnv -> Text -> Text -> IO ControlSignal
runAgent env sysPrompt task =
  fst <$> runAgentTurn env sysPrompt (sessionFromEnv env) task

runAgentWithList :: AgentEnv -> Text -> ObjectiveSpec -> Set PlanFingerprint -> IO ControlSignal
runAgentWithList env sysPrompt spec triedFps = do
  let arch = env.envLlm Architect
      planner = env.envLlm Architect
      cfg = defaultPlanExecutionConfig{repoContext = env.envCompState.csRepoMap}
      executeStep step = do
        prompt <- buildStepPrompt sysPrompt step
        runAgent env prompt step.description
  runWithPlan arch planner (env.envLlm Coder) cfg spec triedFps executeStep

runAgentWithPlan :: AgentEnv -> Text -> ObjectiveSpec -> Set PlanFingerprint -> IO ControlSignal
runAgentWithPlan env sysPrompt spec triedFps = do
  let arch = env.envLlm Architect
      planner = env.envLlm Architect
      cfg = defaultPlanExecutionConfig{repoContext = env.envCompState.csRepoMap}
      executeStep step = do
        prompt <- buildStepPrompt sysPrompt step
        runAgent env prompt step.description
  runWithPlanDag arch planner (env.envLlm Coder) cfg spec triedFps executeStep

loop :: AgentEnv -> Text -> SessionState -> TurnState -> IO (ControlSignal, SessionState)
loop env sysPrompt session turn
  | let b :: BudgetSection = env.envConfig.budgets, turn.tsStep >= b.maxSteps = do
      sig <- finish env (Blocked "Max steps exceeded")
      pure (sig, session)
  | otherwise = do
      (action, session') <- stepAgent env sysPrompt session turn
      case action of
        Stop (NeedsInput query) | Just waitFn <- env.envWaitForInput -> do
          response <- waitFn query
          let session'' = session'{ssHistory = session'.ssHistory |> Message User [TextContent response]}
          loop env sysPrompt session'' turn{tsStep = turn.tsStep + 1}
        Stop signal -> do
          sig <- finish env signal
          pure (sig, session')
        ContinueWith turn' -> loop env sysPrompt session' turn'

stepAgent :: AgentEnv -> Text -> SessionState -> TurnState -> IO (StepAction, SessionState)
stepAgent env sysPrompt session turn = do
  (session', compacted) <- maybeCompact env session
  when compacted (env.envOnEvent AgentCompacting)
  let processedHistory = processHistory defaultChain session'.ssHistory
  llmResult <- callLlm env sysPrompt session'.ssHistory processedHistory
  case llmResult of
    Left signal -> pure (Stop signal, session')
    Right (response, rawHistory') ->
      processResponse env session'{ssHistory = rawHistory'} turn response

maybeCompact :: AgentEnv -> SessionState -> IO (SessionState, Bool)
maybeCompact env session
  | shouldCompact env.envCompaction session.ssHistory = do
      result <- compact (env.envLlm Summarizer) session.ssCompState session.ssHistory Nothing
      case result of
        Left _ -> pure (session, False)
        Right hist -> pure (session{ssHistory = hist}, True)
  | otherwise = pure (session, False)

callLlm :: AgentEnv -> Text -> Seq Message -> Seq Message -> IO (Either ControlSignal (CompletionResponse, Seq Message))
callLlm env sysPrompt rawHistory processedHistory = do
  let client = env.envLlm Coder
      req = CompletionRequest
        { messages = toList processedHistory
        , systemPrompt = Just sysPrompt
        , tools = toolSchemas env.envTools
        , maxTokens = 8192
        , temperature = Nothing
        , topP = Nothing
        }
  llmSpan <- env.envTelemetry.startSpan "llm.call" [("role", "coder")]
  result <- client.stream req (forwardTokens env)
  env.envTelemetry.endSpan llmSpan
  case result of
    Left err -> do
      env.envOnEvent (AgentError ("LLM error: " <> Text.pack (show err)))
      let cls = classifyLlmError err
      pure $ Left (assessControlSignal (StepFailure cls (Text.pack (show err))))
    Right response -> do
      env.envTelemetry.recordEvent $
        Tel.EvLlmCall
          { Tel.modelRole = Coder
          , Tel.inputTokens = fromIntegral response.usage.inputTokens
          , Tel.outputTokens = fromIntegral response.usage.outputTokens
          , Tel.durationMs = 0
          }
      env.envOnEvent $
        AgentCostUpdate
          (fromIntegral response.usage.inputTokens)
          (fromIntegral response.usage.outputTokens)
      budgetResult <- checkBudget env response
      case budgetResult of
        Just reason -> pure (Left (Blocked reason))
        Nothing -> do
          let assistantMsg = Message Assistant response.responseContent
          pure $ Right (response, rawHistory |> assistantMsg)

processResponse :: AgentEnv -> SessionState -> TurnState -> CompletionResponse -> IO (StepAction, SessionState)
processResponse env session turn response =
  case extractToolCalls response.responseContent of
    [] -> pure (Stop (completionSignal response.stopReason), session)
    calls -> processToolCalls env session turn calls

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

data StepResult = StepResult
  { signal :: ControlSignal
  , outcome :: StepOutcome
  , toolResults :: [Content]
  }

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

executeTool :: AgentEnv -> [StepOutcome] -> ToolCall -> IO (StepOutcome, Content)
executeTool env _recentOutcomes tc = do
  env.envOnEvent (AgentToolCall tc.toolName (Text.pack (show tc.arguments)))
  env.envTelemetry.recordEvent $
    Tel.EvToolStart
      { Tel.toolName = unToolName tc.toolName
      , Tel.inputSummary = Text.take 200 (Text.pack (show tc.arguments))
      }
  permitted <- checkPermission env tc
  let decision = if permitted then GR.Allow else evaluate env.envGuardrails env.envTools tc
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

runTool :: AgentEnv -> ToolCall -> IO (StepOutcome, Content)
runTool env tc = do
  let input = ToolInput{arguments = extractArgs tc.arguments}
  result <- dispatch env.envTools tc.toolName input
  case result of
    Left err -> do
      let msg = Text.pack (show err)
      env.envTelemetry.recordEvent $
        Tel.EvToolEnd{Tel.toolName = unToolName tc.toolName, Tel.success = False, Tel.durationMs = 0}
      env.envOnEvent (AgentToolResult tc.toolName ("ERROR: " <> msg))
      let cls = classifyToolError err
      pure (StepFailure cls msg, toToolResult tc.toolCallId msg True)
    Right output -> do
      env.envTelemetry.recordEvent $
        Tel.EvToolEnd{Tel.toolName = unToolName tc.toolName, Tel.success = True, Tel.durationMs = 0}
      env.envOnEvent (AgentToolResult tc.toolName output.content)
      pure (StepSuccess output.content, toToolResult tc.toolCallId output.content False)

checkPermission :: AgentEnv -> ToolCall -> IO Bool
checkPermission env tc =
  case env.envPermissions of
    Nothing -> pure False
    Just store -> check store (unToolName tc.toolName)

verifyStepResult :: AgentEnv -> [ToolCall] -> ControlSignal -> IO ControlSignal
verifyStepResult env calls signal = do
  let modifiedFiles = extractModifiedPaths calls
  if null modifiedFiles
    then pure signal
    else do
      result <- verify env.envGrammarReg env.envTools defaultVerificationConfig modifiedFiles
      case result of
        VerificationFailed reason -> do
          env.envOnEvent (AgentError ("Verification failed: " <> reason))
          pure Continue
        VerificationPassed evidence -> pure (Done evidence)
        VerificationPartial _evidence -> pure signal

extractModifiedPaths :: [ToolCall] -> [FilePath]
extractModifiedPaths calls =
  [ Text.unpack path
  | tc <- calls
  , unToolName tc.toolName `elem` ["edit", "write"]
  , path <- case Map.lookup "path" (extractArgs tc.arguments) of
      Just (String p) -> [p]
      _ -> []
  ]

checkBudget :: AgentEnv -> CompletionResponse -> IO (Maybe Text)
checkBudget env response = case env.envCostTracker of
  Nothing -> pure Nothing
  Just ct -> do
    result <- record ct env.envSessionId env.envUserId "claude-sonnet"
      (fromIntegral response.usage.inputTokens) (fromIntegral response.usage.outputTokens)
    case result of
      Recorded -> pure Nothing
      BudgetExhausted r -> pure (Just r)

finish :: AgentEnv -> ControlSignal -> IO ControlSignal
finish env signal = do
  env.envTelemetry.recordEvent (Tel.EvControlSignal signal)
  env.envOnEvent (AgentDone signal)
  pure signal

forwardTokens :: AgentEnv -> CompletionEvent -> IO ()
forwardTokens env (EventToken t) = env.envOnEvent (AgentToken t)
forwardTokens _ _ = pure ()

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
