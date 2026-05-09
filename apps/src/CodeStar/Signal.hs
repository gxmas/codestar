module CodeStar.Signal
  ( -- * Primary assessment
    assessControlSignal
  , assessWithHistory

    -- * Error classification
  , classifyLlmError
  , classifyToolError

    -- * Re-export
  , worstSignal
  ) where

import Data.Text (Text)

import CodeStar.LLM.Base (LlmError (..))
import CodeStar.Tools.Registry (ToolError (..))
import CodeStar.Types

-- --------------------------------------------------------------------
-- FailureClass → ControlSignal mappings
-- --------------------------------------------------------------------
--
-- Transient    → Continue      (rate-limit, network blip; retry silently)
-- Validation   → NeedsInput    (bad arguments; agent must fix its inputs)
-- Precondition → NeedsInput    (required state missing; agent must establish it)
-- Execution    → NeedsInput    (tool failed; agent should interpret and replan)
-- Policy       → NeedsInput    (permission required; pause for user)
--
-- Loop detection overrides all of the above:
--   3+ consecutive failures of any class → Blocked

-- | Assess the signal for a single step outcome.
assessControlSignal :: StepOutcome -> ControlSignal
assessControlSignal = \case
  StepSuccess _ -> Continue
  StepFailure cls msg -> failureToSignal cls msg
  StepNeedsReplan reason -> NeedsInput ("Replan needed: " <> reason)
  StepTryAlternative _ -> Continue

failureToSignal :: FailureClass -> Text -> ControlSignal
failureToSignal cls msg = case cls of
  Transient -> Continue
  Validation -> NeedsInput ("Validation failure — fix inputs: " <> msg)
  Precondition -> NeedsInput ("Precondition not met — establish or replan: " <> msg)
  Execution -> NeedsInput ("Execution failure — interpret and replan: " <> msg)
  Policy -> NeedsInput ("Policy violation — awaiting approval: " <> msg)

-- --------------------------------------------------------------------
-- Loop detection
-- --------------------------------------------------------------------

{- | Assess a step outcome with awareness of the recent history.
If the last 3 (or more) consecutive outcomes were all failures,
upgrade the signal to Blocked regardless of the individual classification.
-}
assessWithHistory :: [StepOutcome] -> StepOutcome -> ControlSignal
assessWithHistory recentOutcomes current
  | countConsecutiveFailures recentOutcomes >= 3 =
      Blocked "Loop detected: 3 or more consecutive failures without progress"
  | otherwise =
      assessControlSignal current

-- | Count trailing consecutive failure outcomes.
countConsecutiveFailures :: [StepOutcome] -> Int
countConsecutiveFailures = length . takeWhile isFailure . reverse
 where
  isFailure (StepFailure _ _) = True
  isFailure (StepNeedsReplan _) = True
  isFailure _ = False

-- --------------------------------------------------------------------
-- Error classification
-- --------------------------------------------------------------------

classifyLlmError :: LlmError -> FailureClass
classifyLlmError = \case
  RateLimited _ -> Transient
  NetworkError _ -> Transient
  AuthenticationFailed _ -> Policy
  ContentFiltered _ -> Policy
  ContextTooLong _ _ -> Precondition
  InvalidRequest _ -> Validation
  ProviderError _ -> Execution

classifyToolError :: ToolError -> FailureClass
classifyToolError = \case
  ToolNotFound _ -> Validation
  InvalidInput _ -> Validation
  ExecutionFailed _ -> Execution
  Timeout -> Transient
  PolicyDenied _ -> Policy
