{-# LANGUAGE OverloadedStrings #-}

-- | Property-based tests for plan execution telemetry.
--
-- These properties verify that 'runWithPlan' and 'runWithPlanDag' produce
-- the correct telemetry operations (spans, events, attributes) under all
-- scenarios: success, failure in each phase, and exceptions during execution.
--
-- We use a fake 'TelemetryRecorder' backed by an 'IORef' (same pattern as
-- 'TelemetrySpanSpec') and fake LLM clients that return scripted responses.
--
-- Key invariants tested:
--
--   1. Every startSpan is paired with an endSpan (span balance)
--   2. Span names are always from the expected set
--   3. plan.localize appears iff task type is Bug
--   4. plan.architect always precedes plan.planner
--   5. EvPlanGenerated fires iff phase 2 succeeds
--   6. plan.execute span ends even when executeStep throws
--   7. validation_retries attribute matches actual retry count
module CodeStar.PlanTelemetrySpec (spec) where

import Control.Exception (ErrorCall (..), SomeException, throwIO, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)

import CodeStar.LLM.Base
  ( ClientInfo (..)
  , CompletionResponse (..)
  , Content (..)
  , LlmClientDict (..)
  , LlmError (..)
  , StopReason (..)
  , TokenCount (..)
  )
import CodeStar.PlanExecution
  ( PlanExecutionConfig (..)
  , defaultPlanExecutionConfig
  , runWithPlan
  )
import OTel.Attribute (AttributeValue (..))
import CodeStar.Telemetry (AgentEvent (..), SpanHandle (..), TelemetryRecorder (..))
import CodeStar.Types (ControlSignal (..), ObjectiveSpec (..), TaskType (..))
import CodeStar.Types.Gen ()

-- ===================================================================
-- Fake recorder infrastructure
-- ===================================================================

-- | A recorded telemetry operation.
data TelOp
  = OpSpanStart Text [(Text, AttributeValue)]
  | OpSpanEnd
  | OpSetAttr Text Text
  | OpSetAttrTyped Text AttributeValue
  | OpSetError Text
  | OpEvent AgentEvent
  deriving stock (Show)

isSpanStart :: TelOp -> Bool
isSpanStart (OpSpanStart _ _) = True
isSpanStart _ = False

isSpanEnd :: TelOp -> Bool
isSpanEnd OpSpanEnd = True
isSpanEnd _ = False

spanStartName :: TelOp -> Maybe Text
spanStartName (OpSpanStart n _) = Just n
spanStartName _ = Nothing

isPlanGeneratedEvent :: TelOp -> Bool
isPlanGeneratedEvent (OpEvent EvPlanGenerated{}) = True
isPlanGeneratedEvent _ = False

data FakeRecorder = FakeRecorder
  { recorder :: TelemetryRecorder
  , getOps :: IO [TelOp]
  }

newFakeRecorder :: IO FakeRecorder
newFakeRecorder = do
  ref <- newIORef ([] :: [TelOp])
  let append op = atomicModifyIORef' ref (\ops -> (ops ++ [op], ()))
      dummyHandle = SpanHandle (error "FakeRecorder: no SomeSpan") (error "FakeRecorder: no Token")
      rec = TelemetryRecorder
        { recordEvent = \evt -> append (OpEvent evt)
        , startSpan = \name attrs -> do
            append (OpSpanStart name attrs)
            pure dummyHandle
        , endSpan = \_ -> append OpSpanEnd
        , setSpanAttr = \_ k v -> append (OpSetAttr k v)
        , setSpanError       = \_ msg -> append (OpSetError msg)
        , setSpanAttrTyped   = \_ k v -> append (OpSetAttrTyped k v)
        , adjustSessionCount = \_ -> pure ()
        }
  pure FakeRecorder{recorder = rec, getOps = readIORef ref}

-- ===================================================================
-- Fake LLM clients
-- ===================================================================

-- | A client that returns scripted text responses in order.
-- After exhausting the list, returns a fallback valid plan.
scriptedClient :: [Text] -> IO LlmClientDict
scriptedClient outputs = do
  ref <- newIORef outputs
  pure LlmClientDict
    { clientInfo = ClientInfo "fake" "fake-model"
    , complete = \_ -> Right . asResponse <$> popFrom ref
    , stream = \_ _ -> Right . asResponse <$> popFrom ref
    , countTokens = \_ -> pure (Right TokenCount{inputTokens = 0, outputTokens = 0, cacheCreationTokens = 0, cacheReadTokens = 0})
    }

-- | A client that always fails.
failingClient :: Text -> IO LlmClientDict
failingClient errMsg = do
  pure LlmClientDict
    { clientInfo = ClientInfo "fake" "failing-model"
    , complete = \_ -> pure (Left (err errMsg))
    , stream = \_ _ -> pure (Left (err errMsg))
    , countTokens = \_ -> pure (Right TokenCount{inputTokens = 0, outputTokens = 0, cacheCreationTokens = 0, cacheReadTokens = 0})
    }
 where
  err msg = toProviderError msg

toProviderError :: Text -> LlmError
toProviderError = ProviderError

popFrom :: IORef [Text] -> IO Text
popFrom ref = do
  xs <- readIORef ref
  case xs of
    (x : rest) -> writeIORef ref rest >> pure x
    [] -> pure fallbackPlanText

asResponse :: Text -> CompletionResponse
asResponse txt = CompletionResponse
  { responseContent = [TextContent txt]
  , stopReason = EndTurn
  , usage = TokenCount 1 1 0 0
  }

-- | A single-step valid plan.
fallbackPlanText :: Text
fallbackPlanText = Text.unlines
  [ "STEP: implement feature"
  , "ID: step-0"
  , "USES: src/A.hs"
  , "---"
  ]

-- | A plan with duplicate IDs (fails validation).
invalidPlanText :: Text
invalidPlanText = Text.unlines
  [ "STEP: first"
  , "ID: dup"
  , "USES: src/A.hs"
  , "---"
  , "STEP: second"
  , "ID: dup"
  , "USES: src/B.hs"
  , "---"
  ]

-- ===================================================================
-- Test scenarios
-- ===================================================================

-- | Scenario controlling which phases succeed/fail/throw.
data PlanScenario = PlanScenario
  { scenarioTaskType :: TaskType
  , scenarioArchOutcome :: PhaseOutcome
  , scenarioPlannerOutcome :: PlannerOutcome
  , scenarioExecuteOutcome :: ExecuteOutcome
  }
  deriving stock (Show)

data PhaseOutcome = PhaseSucceeds | PhaseFails
  deriving stock (Show, Eq, Enum, Bounded)

data PlannerOutcome = PlannerSucceeds Int | PlannerFails
  -- ^ Int = number of validation retries before success
  deriving stock (Show, Eq)

data ExecuteOutcome = ExecSucceeds | ExecThrows
  deriving stock (Show, Eq, Enum, Bounded)

instance Arbitrary PlanScenario where
  arbitrary = PlanScenario
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary PhaseOutcome where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PlannerOutcome where
  arbitrary = frequency
    [ (7, PlannerSucceeds <$> chooseInt (0, 2))
    , (3, pure PlannerFails)
    ]

instance Arbitrary ExecuteOutcome where
  arbitrary = arbitraryBoundedEnum

-- ===================================================================
-- Running a scenario
-- ===================================================================

-- | Run a plan scenario and return the recorded telemetry ops.
runScenario :: PlanScenario -> IO [TelOp]
runScenario scenario = do
  fr <- newFakeRecorder

  arch <- case scenario.scenarioArchOutcome of
    PhaseSucceeds -> scriptedClient ["src/A.hs, src/B.hs"]
    PhaseFails -> failingClient "architect failed"

  planner <- case scenario.scenarioPlannerOutcome of
    PlannerSucceeds retries ->
      let invalidAttempts = replicate retries invalidPlanText
       in scriptedClient (invalidAttempts ++ [fallbackPlanText])
    PlannerFails -> failingClient "planner failed"

  let cfg = defaultPlanExecutionConfig{maxReplans = 3}
      spec' = ObjectiveSpec
        { description = "Test task"
        , contextFiles = []
        , taskType = scenario.scenarioTaskType
        }
      executeStep _ = case scenario.scenarioExecuteOutcome of
        ExecSucceeds -> pure Continue
        ExecThrows -> throwIO (ErrorCall "executeStep blew up")

  _ <- try @SomeException $
    runWithPlan fr.recorder arch planner arch cfg spec' Set.empty executeStep
  fr.getOps

-- | Whether phase 2 succeeds in this scenario (arch must also succeed).
phase2Succeeds :: PlanScenario -> Bool
phase2Succeeds s =
  s.scenarioArchOutcome == PhaseSucceeds
    && case s.scenarioPlannerOutcome of
      PlannerSucceeds _ -> True
      PlannerFails -> False

-- | Whether execution is reached.
executionReached :: PlanScenario -> Bool
executionReached s = phase2Succeeds s

-- ===================================================================
-- Helpers
-- ===================================================================

countBy :: (a -> Bool) -> [a] -> Int
countBy p = length . filter p

-- | Find all span start names.
spanNames :: [TelOp] -> [Text]
spanNames = concatMap (\op -> case spanStartName op of Just n -> [n]; Nothing -> [])

-- | Index of first span start with given name.
indexOfSpanStart :: Text -> [TelOp] -> Maybe Int
indexOfSpanStart name ops =
  case filter (\(_, op) -> spanStartName op == Just name) (zip [0 ..] ops) of
    ((i, _) : _) -> Just i
    [] -> Nothing

-- ===================================================================
-- Properties
-- ===================================================================

spec :: Spec
spec = describe "CodeStar.PlanExecution telemetry" $ do

  -- ----------------------------------------------------------------
  -- Property 1: Every span has a matching endSpan
  --
  -- The fundamental resource-safety invariant. For any scenario
  -- (success, Left-failure, or exception), span starts must equal
  -- span ends. All phases use finally to guarantee span closure.
  -- ----------------------------------------------------------------
  describe "span balance" $ do
    prop "startSpan count equals endSpan count for all outcomes" $
      forAll arbitrary $ \scenario ->
        monadicIO $ do
          ops <- run (runScenario scenario)
          let starts = countBy isSpanStart ops
              ends = countBy isSpanEnd ops
          monitor (classify (scenario.scenarioArchOutcome == PhaseFails) "arch fails")
          monitor (classify (scenario.scenarioPlannerOutcome == PlannerFails) "planner fails")
          monitor (classify (executionReached scenario) "reaches execution")
          assert (starts == ends)

  -- ----------------------------------------------------------------
  -- Property 2: Span names are from the expected set
  --
  -- A naming invariant: we never emit spans with unexpected names.
  -- This catches typos and ensures dashboards can rely on the names.
  -- ----------------------------------------------------------------
  describe "span naming" $ do
    prop "all span names are from the expected set" $
      forAll arbitrary $ \scenario ->
        monadicIO $ do
          ops <- run (runScenario scenario)
          let names = spanNames ops
              expected = ["plan.localize", "plan.architect", "plan.planner", "plan.execute"]
          assert (all (`elem` expected) names)

  -- ----------------------------------------------------------------
  -- Property 3: plan.localize appears iff Bug task
  --
  -- A metamorphic relation: the presence of the localize span is
  -- determined solely by TaskType, independent of other scenario params.
  -- ----------------------------------------------------------------
  describe "localize span presence" $ do
    prop "plan.localize appears iff task type is Bug" $
      forAll arbitrary $ \scenario ->
        monadicIO $ do
          ops <- run (runScenario scenario)
          let names = spanNames ops
              hasLocalize = "plan.localize" `elem` names
          monitor (classify (scenario.scenarioTaskType == Bug) "Bug task")
          monitor (classify (scenario.scenarioTaskType /= Bug) "non-Bug task")
          assert (hasLocalize == (scenario.scenarioTaskType == Bug))

  -- ----------------------------------------------------------------
  -- Property 4: plan.architect always precedes plan.planner
  --
  -- A temporal ordering invariant. When both phases execute, the
  -- architect span must start before the planner span.
  -- ----------------------------------------------------------------
  describe "span ordering" $ do
    prop "plan.architect start precedes plan.planner start" $
      forAll (arbitrary `suchThat` (\s -> s.scenarioArchOutcome == PhaseSucceeds)) $ \scenario ->
        monadicIO $ do
          ops <- run (runScenario scenario)
          let archIdx = indexOfSpanStart "plan.architect" ops
              planIdx = indexOfSpanStart "plan.planner" ops
          case (archIdx, planIdx) of
            (Just a, Just p) -> assert (a < p)
            -- If planner isn't reached (e.g. fails), architect still starts
            (Just _, Nothing) -> assert True
            _ -> assert False -- architect must always start

  -- ----------------------------------------------------------------
  -- Property 5: EvPlanGenerated fires iff phase 2 succeeds
  --
  -- The event emission invariant: the plan-generated event appears
  -- exactly when the planner produces a valid plan (regardless of
  -- fingerprint match or execution outcome).
  -- ----------------------------------------------------------------
  describe "EvPlanGenerated event" $ do
    prop "EvPlanGenerated fires iff phase 2 succeeds" $
      forAll arbitrary $ \scenario ->
        monadicIO $ do
          ops <- run (runScenario scenario)
          let hasPlanEvent = any isPlanGeneratedEvent ops
          monitor (classify (phase2Succeeds scenario) "phase2 succeeds")
          monitor (classify (not (phase2Succeeds scenario)) "phase2 fails")
          assert (hasPlanEvent == phase2Succeeds scenario)

  -- ----------------------------------------------------------------
  -- Property 6: plan.execute span ends even when executeStep throws
  --
  -- All phases use finally to guarantee span closure. Even when
  -- executeStep throws an exception that is not caught by
  -- wrapExceptionsRaw (e.g. ErrorCall), the finally fires and the
  -- span is properly ended.
  -- ----------------------------------------------------------------
  describe "exception safety in execute phase" $ do
    prop "span balance holds even with throwing executeStep" $
      forAll (arbitrary `suchThat` (\s -> s.scenarioExecuteOutcome == ExecThrows && executionReached s)) $ \scenario ->
        monadicIO $ do
          ops <- run (runScenario scenario)
          let starts = countBy isSpanStart ops
              ends = countBy isSpanEnd ops
          monitor (counterexample $
            "starts=" <> show starts <> " ends=" <> show ends)
          assert (starts == ends)

  -- ----------------------------------------------------------------
  -- Property 7: validation_retries attribute matches actual retry count
  --
  -- A data-integrity property: the retry count reported in the span
  -- attribute must exactly match the number of validation attempts
  -- that preceded the successful plan generation.
  -- ----------------------------------------------------------------
  describe "validation_retries attribute" $ do
    prop "validation_retries matches the generated retry count" $
      forAll (chooseInt (0, 2)) $ \retries ->
        monadicIO $ do
          let scenario = PlanScenario
                { scenarioTaskType = Feature
                , scenarioArchOutcome = PhaseSucceeds
                , scenarioPlannerOutcome = PlannerSucceeds retries
                , scenarioExecuteOutcome = ExecSucceeds
                }
          ops <- run (runScenario scenario)
          let retriesAttr = findAttr "validation_retries" ops
          monitor (counterexample $ "ops: " <> show ops)
          monitor (classify (retries == 0) "zero retries")
          monitor (classify (retries > 0) "has retries")
          assert (retriesAttr == Just (Text.pack (show retries)))

-- | Find the value of a SetAttr (or SetAttrTyped) operation by key.
findAttr :: Text -> [TelOp] -> Maybe Text
findAttr key ops =
  case [v | OpSetAttr k v <- ops, k == key] of
    (v : _) -> Just v
    [] -> case [v | OpSetAttrTyped k v <- ops, k == key] of
      (Int64Value n : _) -> Just (Text.pack (show n))
      (StringValue s : _) -> Just s
      _ -> Nothing
