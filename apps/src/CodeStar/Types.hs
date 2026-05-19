{- |
= CodeStar.Types — shared domain types

This module is the __vocabulary__ of the agent system.  Every other module
imports from here; nothing here imports from the rest of codestar.  Keeping
all shared types in one place avoids import cycles and makes it easy to see
the complete data model at a glance.

== Key concepts for students

  * __ControlSignal__: the value the agent loop produces at the end of each
    turn.  'Continue' means keep going; 'NeedsInput' means pause and wait
    for the user; 'Blocked' means give up because a loop was detected; 'Done'
    means the task is finished with verifiable evidence.

  * __Evidence__: proof that the agent's work is correct — did the tests
    pass? did the build succeed? which files were verified?  The agent must
    produce evidence before emitting 'Done'.

  * __FailureClass__: a taxonomy that maps tool and LLM errors to
    actionable signals.  'Transient' errors are retried silently; 'Policy'
    errors pause for user approval; 'Validation' errors ask the agent to
    fix its own output.

  * __StepOutcome__: what happened on a single agent step (tool call +
    observation).  A sequence of 'StepOutcome' values is the raw material
    that 'Signal.assessWithHistory' converts to a 'ControlSignal'.

  * __PlanningMode__: whether the agent uses no plan, a flat list plan, or
    a DAG plan to structure its work.
-}
module CodeStar.Types
  ( -- * Domain Identifiers
    SessionId (..)
  , StepId (..)
  , UserId (..)
  , OrgId (..)

    -- * Task Classification
  , TaskType (..)

    -- * Failure Taxonomy
  , FailureClass (..)

    -- * Evidence
  , CheckResult (..)
  , Evidence (..)

    -- * Control Signals
  , ControlSignal (..)
  , controlSignalSeverity
  , worstSignal

    -- * Step Outcomes
  , StepOutcome (..)

    -- * Objective
  , ObjectiveSpec (..)


    -- * Planning
  , PlanningMode (..)

    -- * Cost
  , CostState (..)

    -- * Agent State Compartments
  , ObjectiveState (..)
  , StepState (..)
  , ObservationState (..)
  , RiskState (..)
  , PlanningState (..)
  , PermissionState (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Hashable (Hashable)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- --------------------------------------------------------------------
-- Domain Identifiers
-- --------------------------------------------------------------------

-- | Identifies a single agent session (one task from start to finish).
-- Newtypes for all domain IDs prevent accidental mix-ups between e.g.
-- a 'SessionId' and a 'UserId' at the type level.
newtype SessionId = SessionId Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, FromJSON, ToJSON)

newtype StepId = StepId Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, FromJSON, ToJSON)

newtype UserId = UserId Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, FromJSON, ToJSON)

newtype OrgId = OrgId Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, FromJSON, ToJSON)

-- --------------------------------------------------------------------
-- Task Classification
-- --------------------------------------------------------------------

data TaskType = Bug | Feature | Refactor | Unknown
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON TaskType where
  toJSON = \case
    Bug -> String "bug"
    Feature -> String "feature"
    Refactor -> String "refactor"
    Unknown -> String "unknown"

instance FromJSON TaskType where
  parseJSON = withText "TaskType" $ \case
    "bug" -> pure Bug
    "feature" -> pure Feature
    "refactor" -> pure Refactor
    "unknown" -> pure Unknown
    other -> fail $ "Unknown TaskType: " <> show other

-- --------------------------------------------------------------------
-- Failure Taxonomy
-- --------------------------------------------------------------------

-- | Why a tool call or LLM call failed.  'Signal.classifyLlmError' and
-- 'Signal.classifyToolError' map provider errors to these classes, which
-- in turn determine the resulting 'ControlSignal'.
data FailureClass
  = Transient    -- ^ Rate limit or network blip; retry silently.
  | Validation   -- ^ Bad inputs; the agent must fix its own output.
  | Precondition -- ^ Required state missing; the agent must establish it.
  | Execution    -- ^ The tool ran but failed; the agent must replan.
  | Policy       -- ^ Permission required; pause for user approval.
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON FailureClass where
  toJSON = \case
    Transient -> String "transient"
    Validation -> String "validation"
    Precondition -> String "precondition"
    Execution -> String "execution"
    Policy -> String "policy"

instance FromJSON FailureClass where
  parseJSON = withText "FailureClass" $ \case
    "transient" -> pure Transient
    "validation" -> pure Validation
    "precondition" -> pure Precondition
    "execution" -> pure Execution
    "policy" -> pure Policy
    other -> fail $ "Unknown FailureClass: " <> show other

-- --------------------------------------------------------------------
-- Evidence
-- --------------------------------------------------------------------

data CheckResult = Passed | Failed | NotChecked
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON CheckResult where
  toJSON = \case
    Passed -> String "passed"
    Failed -> String "failed"
    NotChecked -> String "notChecked"

instance FromJSON CheckResult where
  parseJSON = withText "CheckResult" $ \case
    "passed" -> pure Passed
    "failed" -> pure Failed
    "notChecked" -> pure NotChecked
    other -> fail $ "Unknown CheckResult: " <> show other

-- | Verifiable proof that the agent's work is correct.
-- The agent loop populates this before emitting 'Done'.
data Evidence = Evidence
  { testsPass      :: CheckResult  -- ^ Did the test suite pass?
  , buildSucceeds  :: CheckResult  -- ^ Did the build succeed?
  , filesVerified  :: [FilePath]   -- ^ Files whose syntax was verified.
  , regressions    :: [Text]       -- ^ Any regressions detected.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- --------------------------------------------------------------------
-- Control Signals
-- --------------------------------------------------------------------

-- | The outcome of one agent turn, deciding what happens next.
data ControlSignal
  = Continue             -- ^ Keep running; more tool calls may be needed.
  | NeedsInput Text      -- ^ Pause the loop and surface a question to the user.
  | Blocked Text         -- ^ Abort: the agent is stuck in a loop.
  | Done Evidence        -- ^ Task complete; evidence justifies the claim.
  deriving stock (Eq, Show, Generic)

instance ToJSON ControlSignal where
  toJSON = \case
    Continue -> object ["signal" .= ("continue" :: Text)]
    NeedsInput q -> object ["signal" .= ("needsInput" :: Text), "query" .= q]
    Blocked r -> object ["signal" .= ("blocked" :: Text), "reason" .= r]
    Done e -> object ["signal" .= ("done" :: Text), "evidence" .= e]

instance FromJSON ControlSignal where
  parseJSON = withObject "ControlSignal" $ \o -> do
    sig <- o .: "signal" :: Parser Text
    case sig of
      "continue" -> pure Continue
      "needsInput" -> NeedsInput <$> o .: "query"
      "blocked" -> Blocked <$> o .: "reason"
      "done" -> Done <$> o .: "evidence"
      other -> fail $ "Unknown ControlSignal: " <> show other

-- | Numeric severity for signal comparison: higher = more severe.
-- Used by 'worstSignal' to pick the most severe signal from a list.
controlSignalSeverity :: ControlSignal -> Int
controlSignalSeverity = \case
  Done{} -> 0
  Continue -> 1
  NeedsInput{} -> 2
  Blocked{} -> 3

worstSignal :: [ControlSignal] -> ControlSignal
worstSignal [] = Continue
worstSignal (x : xs) = foldl' pick x xs
 where
  pick a b
    | controlSignalSeverity b >= controlSignalSeverity a = b
    | otherwise = a

-- --------------------------------------------------------------------
-- Step Outcomes
-- --------------------------------------------------------------------

data StepOutcome
  = StepSuccess Text
  | StepFailure FailureClass Text
  | StepNeedsReplan Text
  | StepTryAlternative Int
  deriving stock (Eq, Show, Generic)

instance ToJSON StepOutcome where
  toJSON = \case
    StepSuccess msg ->
      object ["outcome" .= ("success" :: Text), "message" .= msg]
    StepFailure cls msg ->
      object ["outcome" .= ("failure" :: Text), "class" .= cls, "message" .= msg]
    StepNeedsReplan reason ->
      object ["outcome" .= ("needsReplan" :: Text), "reason" .= reason]
    StepTryAlternative n ->
      object ["outcome" .= ("tryAlternative" :: Text), "alternative" .= n]

instance FromJSON StepOutcome where
  parseJSON = withObject "StepOutcome" $ \o -> do
    outcome <- o .: "outcome" :: Parser Text
    case outcome of
      "success" -> StepSuccess <$> o .: "message"
      "failure" -> StepFailure <$> o .: "class" <*> o .: "message"
      "needsReplan" -> StepNeedsReplan <$> o .: "reason"
      "tryAlternative" -> StepTryAlternative <$> o .: "alternative"
      other -> fail $ "Unknown StepOutcome: " <> show other

-- --------------------------------------------------------------------
-- Objective
-- --------------------------------------------------------------------

data ObjectiveSpec = ObjectiveSpec
  { description :: Text
  , contextFiles :: [FilePath]
  , taskType :: TaskType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- --------------------------------------------------------------------
-- Model Roles
-- --------------------------------------------------------------------

-- --------------------------------------------------------------------
-- Planning
-- --------------------------------------------------------------------

data PlanningMode = NoPlan | ListPlan | DagPlan
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON PlanningMode where
  toJSON = \case
    NoPlan -> String "none"
    ListPlan -> String "list"
    DagPlan -> String "dag"

instance FromJSON PlanningMode where
  parseJSON = withText "PlanningMode" $ \case
    "none" -> pure NoPlan
    "list" -> pure ListPlan
    "dag" -> pure DagPlan
    other -> fail $ "Unknown PlanningMode: " <> show other

-- --------------------------------------------------------------------
-- Cost
-- --------------------------------------------------------------------

data CostState = CostState
  { estimatedCost :: Double
  , budget :: Maybe Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- --------------------------------------------------------------------
-- Agent State Compartments
-- --------------------------------------------------------------------

data ObjectiveState = ObjectiveState
  { spec :: ObjectiveSpec
  , startedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data StepState = StepState
  { currentStepId :: StepId
  , outcomes :: [StepOutcome]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ObservationState = ObservationState
  { observations :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RiskState = RiskState
  { consecutiveFailures :: Int
  , failureHistory :: [FailureClass]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PlanningState = PlanningState
  { replanCount :: Int
  , replanBudget :: Int
  , triedApproaches :: Set Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PermissionState = PermissionState
  { sessionPermissions :: Set Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
