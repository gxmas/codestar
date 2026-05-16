{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

-- | Extended property-based tests for plan execution telemetry.
--
-- These properties cover the three telemetry fixes:
--
--   A. wrapExceptionsRaw catches IOException, re-throws everything else
--   B. All four spans closed even under async cancellation
--   C. setSpanError is called when a phase returns Left
--   D. No span error on success paths
--   E. task.type attribute present on execute span
--
-- Properties A-E complement the core invariants in PlanTelemetrySpec.
module CodeStar.PlanTelemetryExtraSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled (..), cancelWith, wait, withAsync)
import Control.Exception
  ( ArithException (DivideByZero)
  , ErrorCall (..)
  , IOException
  , SomeException
  , evaluate
  , throwIO
  , try
  )
import Control.Monad (void)
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
  , wrapExceptionsRaw
  )
import OTel.Attribute (AttributeValue (..))
import CodeStar.Telemetry (AgentEvent (..), SpanHandle (..), TelemetryRecorder (..))
import CodeStar.Types (ControlSignal (..), ObjectiveSpec (..), TaskType (..))
import CodeStar.Types.Gen ()

-- ===================================================================
-- Fake recorder (same pattern as PlanTelemetrySpec / TelemetrySpanSpec)
-- ===================================================================

data TelOp
  = OpSpanStart Text [(Text, AttributeValue)]
  | OpSpanEnd
  | OpSetAttr Text Text
  | OpSetError Text
  | OpEvent AgentEvent
  deriving stock (Show)

isSpanStart :: TelOp -> Bool
isSpanStart (OpSpanStart _ _) = True
isSpanStart _ = False

isSpanEnd :: TelOp -> Bool
isSpanEnd OpSpanEnd = True
isSpanEnd _ = False

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
        , setSpanAttrTyped   = \_ _ _ -> pure ()
        , adjustSessionCount = \_ -> pure ()
        }
  pure FakeRecorder{recorder = rec, getOps = readIORef ref}

-- ===================================================================
-- Fake LLM clients (reused from PlanTelemetrySpec pattern)
-- ===================================================================

scriptedClient :: [Text] -> IO LlmClientDict
scriptedClient outputs = do
  ref <- newIORef outputs
  pure LlmClientDict
    { clientInfo = ClientInfo "fake" "fake-model"
    , complete = \_ -> Right . asResponse <$> popFrom ref
    , stream = \_ _ -> Right . asResponse <$> popFrom ref
    , countTokens = \_ -> pure (Right TokenCount{inputTokens = 0, outputTokens = 0, cacheCreationTokens = 0, cacheReadTokens = 0})
    }

failingClient :: Text -> IO LlmClientDict
failingClient errMsg =
  pure LlmClientDict
    { clientInfo = ClientInfo "fake" "failing-model"
    , complete = \_ -> pure (Left (ProviderError errMsg))
    , stream = \_ _ -> pure (Left (ProviderError errMsg))
    , countTokens = \_ -> pure (Right TokenCount{inputTokens = 0, outputTokens = 0, cacheCreationTokens = 0, cacheReadTokens = 0})
    }

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

fallbackPlanText :: Text
fallbackPlanText = Text.unlines
  [ "STEP: implement feature"
  , "ID: step-0"
  , "USES: src/A.hs"
  , "---"
  ]

-- ===================================================================
-- Helpers
-- ===================================================================

countBy :: (a -> Bool) -> [a] -> Int
countBy p = length . filter p

-- | Find the attributes from a SpanStart with the given name.
attrsForSpan :: Text -> [TelOp] -> Maybe [(Text, AttributeValue)]
attrsForSpan name ops =
  case [attrs | OpSpanStart n attrs <- ops, n == name] of
    (a : _) -> Just a
    [] -> Nothing

-- | Collect all OpSetError values from the ops list.
allErrors :: [TelOp] -> [Text]
allErrors ops = [msg | OpSetError msg <- ops]

-- | Standard config and spec for running scenarios.
defaultCfg :: PlanExecutionConfig
defaultCfg = defaultPlanExecutionConfig{maxReplans = 3}

mkSpec :: TaskType -> ObjectiveSpec
mkSpec tt = ObjectiveSpec
  { description = "Test task"
  , contextFiles = []
  , taskType = tt
  }

-- ===================================================================
-- Property A: wrapExceptionsRaw catches IOException, re-throws rest
-- ===================================================================

-- | Generate an IOException with arbitrary message.
genIOException :: Gen IOException
genIOException = do
  msg <- listOf1 (elements ['a'..'z'])
  pure (userError msg)

-- ===================================================================
-- Properties
-- ===================================================================

spec :: Spec
spec = describe "CodeStar.PlanExecution telemetry (extended)" $ do

  -- ----------------------------------------------------------------
  -- Property A: wrapExceptionsRaw catches IOException
  --
  -- The algebraic law: wrapExceptionsRaw (throwIO ioe) == pure (Left (show ioe))
  -- for any IOException, and wrapExceptionsRaw (throwIO other) re-throws
  -- for non-IOExceptions. This is a round-trip-like property: the
  -- IOException is captured and its show representation preserved.
  -- ----------------------------------------------------------------
  describe "wrapExceptionsRaw" $ do
    prop "catches IOException and returns Left with show representation" $
      forAll genIOException $ \ioe ->
        monadicIO $ do
          result <- run (wrapExceptionsRaw @() (throwIO ioe))
          assert (result == Left (Text.pack (show ioe)))

    prop "re-throws ErrorCall (non-IOException)" $
      forAll (listOf1 (elements ['a'..'z'])) $ \msg ->
        monadicIO $ do
          result <- run (try @ErrorCall (wrapExceptionsRaw (throwIO (ErrorCall msg))))
          case result of
            Left (ErrorCall caught) -> assert (caught == msg)
            Right _ -> assert False

    prop "re-throws ArithException (non-IOException)" $
      monadicIO $ do
        result <- run (try @ArithException (wrapExceptionsRaw (throwIO DivideByZero)))
        case result of
          Left exc -> assert (exc == DivideByZero)
          Right _ -> assert False

    prop "returns Right on success" $
      forAll (arbitrary :: Gen Int) $ \x ->
        monadicIO $ do
          result <- run (wrapExceptionsRaw (evaluate x))
          assert (result == Right x)

  -- ----------------------------------------------------------------
  -- Property A (indirect): IOException in executeStep yields Blocked
  --
  -- When the execute step throws an IOException, wrapExceptionsRaw
  -- catches it and the pipeline returns Blocked. When it throws
  -- ErrorCall, the exception propagates out of runWithPlan.
  -- ----------------------------------------------------------------
  describe "wrapExceptionsRaw via execute phase" $ do
    prop "IOException in executeStep produces Blocked" $
      forAll genIOException $ \ioe ->
        monadicIO $ do
          result <- run $ do
            fr <- newFakeRecorder
            arch <- scriptedClient ["src/A.hs"]
            planner <- scriptedClient [fallbackPlanText]
            let executeStep _ = throwIO ioe
            runWithPlan fr.recorder arch planner arch defaultCfg
              (mkSpec Feature) Set.empty executeStep
          case result of
            Blocked _ -> assert True
            _ -> assert False

    prop "ErrorCall in executeStep propagates out" $
      forAll (listOf1 (elements ['a'..'z'])) $ \msg ->
        monadicIO $ do
          result <- run $ do
            fr <- newFakeRecorder
            arch <- scriptedClient ["src/A.hs"]
            planner <- scriptedClient [fallbackPlanText]
            let executeStep _ = throwIO (ErrorCall msg)
            try @ErrorCall $
              runWithPlan fr.recorder arch planner arch defaultCfg
                (mkSpec Feature) Set.empty executeStep
          case result of
            Left (ErrorCall caught) -> assert (caught == msg)
            Right _ -> assert False

  -- ----------------------------------------------------------------
  -- Property B: Span balance under async cancellation
  --
  -- The key safety invariant from Fix 2: finally guarantees endSpan
  -- runs even when the thread receives an asynchronous exception.
  -- We fork runWithPlan, cancel it after a variable delay, and
  -- assert that started spans == ended spans.
  --
  -- The delay generator targets the five interesting windows:
  -- before any work, during localize, architect, planner, execute.
  -- ----------------------------------------------------------------
  describe "async cancellation span safety" $ do
    prop "all started spans are ended even under async cancellation" $
      forAll ((,) <$> arbitrary <*> chooseInt (0, 5000)) $ \(tt, delay_us) ->
        monadicIO $ do
          (ops, _) <- run $ do
            fr <- newFakeRecorder
            -- Use clients that add realistic latency via scripted responses
            arch <- scriptedClient ["src/A.hs, src/B.hs"]
            planner <- scriptedClient [fallbackPlanText]
            let executeStep _ = threadDelay 1000 >> pure Continue
            withAsync
              ( runWithPlan fr.recorder arch planner arch defaultCfg
                  (mkSpec tt) Set.empty executeStep
              )
              $ \a -> do
                threadDelay delay_us
                cancelWith a AsyncCancelled
                void (try @SomeException (wait a))
            ops <- fr.getOps
            pure (ops, ())
          let starts = countBy isSpanStart ops
              ends = countBy isSpanEnd ops
          monitor (counterexample $
            "delay=" <> show delay_us <> "us starts=" <> show starts <> " ends=" <> show ends)
          monitor (classify (delay_us == 0) "immediate cancel")
          monitor (classify (delay_us > 0 && delay_us <= 1000) "early cancel")
          monitor (classify (delay_us > 1000) "late cancel")
          -- If no spans were started, that's fine (cancelled before first startSpan)
          -- But every started span must have a matching end
          assert (starts == ends)

  -- ----------------------------------------------------------------
  -- Property C: setSpanError called when phase returns Left
  --
  -- When architect fails, OpSetError should appear in the trace.
  -- When planner fails, same. This checks semantic correctness
  -- beyond mere span balance.
  -- ----------------------------------------------------------------
  describe "setSpanError on phase failure" $ do
    prop "architect failure sets span error" $
      forAll arbitrary $ \tt ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            arch <- failingClient "architect exploded"
            planner <- scriptedClient [fallbackPlanText]
            let executeStep _ = pure Continue
            _ <- try @SomeException $
              runWithPlan fr.recorder arch planner arch defaultCfg
                (mkSpec tt) Set.empty executeStep
            fr.getOps
          let errors = allErrors ops
          monitor (counterexample $ "errors: " <> show errors)
          -- At least one error must be set (on the architect span)
          assert (not (null errors))

    prop "planner failure sets span error" $
      forAll arbitrary $ \tt ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            arch <- scriptedClient ["src/A.hs"]
            planner <- failingClient "planner exploded"
            let executeStep _ = pure Continue
            _ <- try @SomeException $
              runWithPlan fr.recorder arch planner arch defaultCfg
                (mkSpec tt) Set.empty executeStep
            fr.getOps
          let errors = allErrors ops
          monitor (counterexample $ "errors: " <> show errors)
          assert (not (null errors))

    prop "IOException in execute sets span error" $
      forAll genIOException $ \ioe ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            arch <- scriptedClient ["src/A.hs"]
            planner <- scriptedClient [fallbackPlanText]
            let executeStep _ = throwIO ioe
            _ <- try @SomeException $
              runWithPlan fr.recorder arch planner arch defaultCfg
                (mkSpec Feature) Set.empty executeStep
            fr.getOps
          let errors = allErrors ops
          monitor (counterexample $ "errors: " <> show errors)
          -- The execute span should have an error set via wrapExceptionsRaw
          assert (not (null errors))

  -- ----------------------------------------------------------------
  -- Property D: No span error on success paths
  --
  -- The converse of Property C: when all phases succeed,
  -- setSpanError must never be called. This is important because
  -- a spurious error marker on a success span would corrupt
  -- error-rate dashboards.
  -- ----------------------------------------------------------------
  describe "no span error on success" $ do
    prop "no setSpanError when all phases succeed" $
      forAll arbitrary $ \tt ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            arch <- scriptedClient ["src/A.hs"]
            planner <- scriptedClient [fallbackPlanText]
            let executeStep _ = pure Continue
            _ <- runWithPlan fr.recorder arch planner arch defaultCfg
                   (mkSpec tt) Set.empty executeStep
            fr.getOps
          let errors = allErrors ops
          monitor (counterexample $ "unexpected errors: " <> show errors)
          monitor (classify (tt == Bug) "Bug (has localize)")
          assert (null errors)

  -- ----------------------------------------------------------------
  -- Property E: task.type attribute on execute span
  --
  -- The execute span must carry a "task.type" attribute matching
  -- the scenario's TaskType. This was a missing attribute that
  -- was added in the fix.
  -- ----------------------------------------------------------------
  describe "task.type on execute span" $ do
    prop "execute span carries task.type matching the ObjectiveSpec" $
      forAll arbitrary $ \tt ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            arch <- scriptedClient ["src/A.hs"]
            planner <- scriptedClient [fallbackPlanText]
            let executeStep _ = pure Continue
            _ <- runWithPlan fr.recorder arch planner arch defaultCfg
                   (mkSpec tt) Set.empty executeStep
            fr.getOps
          let execAttrs = attrsForSpan "plan.execute" ops
              expectedTT = Text.pack (show tt)
          monitor (counterexample $ "exec attrs: " <> show execAttrs)
          monitor (classify (tt == Bug) "Bug")
          monitor (classify (tt == Feature) "Feature")
          monitor (classify (tt == Refactor) "Refactor")
          case execAttrs of
            Just attrs -> assert (lookup "task.type" attrs == Just (StringValue expectedTT))
            Nothing -> assert False -- execute should always be reached

    prop "task.type on architect and planner spans matches too" $
      forAll arbitrary $ \tt ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            arch <- scriptedClient ["src/A.hs"]
            planner <- scriptedClient [fallbackPlanText]
            let executeStep _ = pure Continue
            _ <- runWithPlan fr.recorder arch planner arch defaultCfg
                   (mkSpec tt) Set.empty executeStep
            fr.getOps
          let expectedTT = Text.pack (show tt)
              archAttrs = attrsForSpan "plan.architect" ops
              planAttrs = attrsForSpan "plan.planner" ops
          case (archAttrs, planAttrs) of
            (Just aa, Just pa) -> do
              assert (lookup "task.type" aa == Just (StringValue expectedTT))
              assert (lookup "task.type" pa == Just (StringValue expectedTT))
            _ -> assert False
