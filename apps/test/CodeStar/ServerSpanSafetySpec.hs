{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Property-based tests for the async exception safety of the session
-- span lifecycle in Server.hs.
--
-- The server wraps each agent turn in a root span:
--
-- @
--   spanResult <- try (recorder.startSpan ...)
--   case spanResult of
--     Left ex  -> mark STerminated (no span to end)
--     Right sp -> finally (runAgent ... >> endSpan) (endSpan sp)
-- @
--
-- These properties verify:
--
--   5. endSpan is always called exactly once, even when runAgent receives
--      an async exception (throwTo) at an arbitrary point.
--   6. When startSpan itself throws, status becomes STerminated.
--   7. When startSpan throws, endSpan is never called (no phantom span end).
--
-- We use the same FakeRecorder infrastructure from TelemetrySpanSpec but
-- extend it to support a "throwing startSpan" mode and simulate async
-- exceptions via 'throwTo' with random delay.
--
-- Generator design:
--   - Exception timing is drawn from a geometric distribution (biased toward
--     early throws where the span is most vulnerable).
--   - We classify by whether the exception lands during runAgent vs. during
--     setSpanAttr/endSpan aftermath, to verify coverage of both windows.
module CodeStar.ServerSpanSafetySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, asyncThreadId)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception
  ( AsyncException (..)
  , ErrorCall (..)
  , SomeException
  , finally
  , mask
  , throwIO
  , throwTo
  , try
  )
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run, monitor)

import OTel.Attribute (AttributeValue (..))
import CodeStar.Telemetry (SpanHandle (..), TelemetryRecorder (..))
import CodeStar.Types (ControlSignal (..))
import CodeStar.Types.Gen () -- Arbitrary ControlSignal


-- ===================================================================
-- Fake recorder (reusable infrastructure)
-- ===================================================================

data SpanOp
  = SpanStart Text [(Text, AttributeValue)]
  | SpanEnd
  | SpanSetAttr Text Text
  | SpanSetError Text
  deriving stock (Show, Eq)

data FakeRecorder = FakeRecorder
  { recorder :: TelemetryRecorder
  , getOps   :: IO [SpanOp]
  }

-- | A recorder that records all operations. Optionally throws from startSpan.
newFakeRecorder :: Bool -> IO FakeRecorder
newFakeRecorder throwOnStart = do
  ref <- newIORef ([] :: [SpanOp])
  let append op = atomicModifyIORef' ref (\ops -> (ops ++ [op], ()))
      dummyHandle = SpanHandle
        (error "FakeRecorder: SomeSpan not used")
        (error "FakeRecorder: Token not used")
      rec = TelemetryRecorder
        { recordEvent      = \_ -> pure ()
        , startSpan        = \name attrs -> do
            if throwOnStart
              then throwIO (ErrorCall "startSpan failed: simulated")
              else do
                append (SpanStart name attrs)
                pure dummyHandle
        , endSpan          = \_ -> append SpanEnd
        , setSpanAttr      = \_ k v -> append (SpanSetAttr k v)
        , setSpanError       = \_ msg -> append (SpanSetError msg)
        , setSpanAttrTyped   = \_ _ _ -> pure ()
        , adjustSessionCount = \_ -> pure ()
        }
  pure FakeRecorder { recorder = rec, getOps = readIORef ref }


-- ===================================================================
-- Session status (mirrors SessionStatus from Platform.SessionManager)
-- ===================================================================

data TestSessionStatus
  = SRunning
  | SCompleted ControlSignal
  | STerminated
  deriving stock (Show, Eq)


-- ===================================================================
-- The span lifecycle under test
--
-- This is a faithful extraction of the Server.hs async block.
-- It takes a status TVar (simulating session.status) and an
-- agent action (simulating runAgent).
-- ===================================================================

-- | Reproduce the exact control flow from Server.hs lines 318-343.
runSessionSpanWithStatus
  :: TelemetryRecorder
  -> TVar TestSessionStatus
  -> Text    -- ^ session id
  -> Text    -- ^ user id
  -> Text    -- ^ task
  -> IO ControlSignal  -- ^ agent action (may throw or block)
  -> IO ()
runSessionSpanWithStatus rec statusVar sid uid task agentAction =
  mask $ \restore -> do
    spanResult <- restore $ try (rec.startSpan "agent.turn"
      [ ("session.id", StringValue sid)
      , ("user.id",    StringValue uid)
      , ("task",       StringValue (Text.take 200 task))
      ])
    case spanResult of
      Left (_spanEx :: SomeException) -> do
        atomically $ writeTVar statusVar STerminated
      Right rootSpan ->
        restore (do
          result <- try agentAction
          case result of
            Right signal -> do
              rec.setSpanAttr rootSpan "outcome" "done"
              atomically $ writeTVar statusVar (SCompleted signal)
            Left (ex :: SomeException) -> do
              let msg = Text.pack (show ex)
              rec.setSpanError rootSpan msg
              atomically $ writeTVar statusVar STerminated)
        `finally` rec.endSpan rootSpan


-- ===================================================================
-- Generators
-- ===================================================================

-- | Delay in microseconds before throwing an async exception.
-- Geometric distribution biased toward short delays (0-1000us)
-- to hit the vulnerable window between startSpan and the finally block.
genThrowDelay :: Gen Int
genThrowDelay = frequency
  [ (4, chooseInt (0, 100))       -- immediate / during startSpan
  , (3, chooseInt (100, 1000))    -- during agent startup
  , (2, chooseInt (1000, 10000))  -- during agent execution
  , (1, chooseInt (10000, 50000)) -- during cleanup / after completion
  ]

-- | How long the simulated agent "runs" in microseconds.
genAgentDuration :: Gen Int
genAgentDuration = frequency
  [ (2, pure 0)                   -- instant
  , (3, chooseInt (100, 2000))    -- short
  , (3, chooseInt (2000, 10000))  -- medium
  , (2, chooseInt (10000, 50000)) -- long
  ]


-- ===================================================================
-- Properties
-- ===================================================================

spec :: Spec
spec = describe "CodeStar.ServerSpanSafety" $ do

  -- ----------------------------------------------------------------
  -- Property 5: endSpan called exactly once even under async exception
  --
  -- We launch the session span lifecycle in an async thread, then
  -- throwTo it after a random delay. The endSpan must appear exactly
  -- once in the recorded ops (the 'finally' guarantee).
  --
  -- This is the core exception-safety invariant. Without 'finally',
  -- an async exception during runAgent would skip endSpan, leaking
  -- the span.
  -- ----------------------------------------------------------------
  describe "endSpan exactly once under async exception" $ do
    prop "endSpan appears exactly once when agent is killed" $
      forAll genThrowDelay $ \throwDelay ->
        forAll genAgentDuration $ \agentDur ->
          monadicIO $ do
            monitor (classify (throwDelay < agentDur) "exception during agent")
            monitor (classify (throwDelay >= agentDur) "exception after agent")
            monitor (cover 30 (throwDelay < agentDur) "hits during agent run")
            ops <- run $ do
              fr <- newFakeRecorder False
              statusVar <- newTVarIO SRunning
              let agentAction = do
                    threadDelay agentDur
                    pure Continue
              thread <- async $
                runSessionSpanWithStatus fr.recorder statusVar "s" "u" "task" agentAction
              -- Throw async exception after delay
              threadDelay throwDelay
              throwTo (asyncThreadId thread) ThreadKilled
              -- Wait for thread to finish (it should handle the exception)
              _ <- try @SomeException (wait thread)
              fr.getOps
            let ends = length (filter (== SpanEnd) ops)
            -- If startSpan succeeded (SpanStart present), endSpan must be called exactly once.
            -- If the exception arrived before startSpan completed, we might get 0 starts and 0 ends.
            let starts = length [() | SpanStart _ _ <- ops]
            if starts == 1
              then assert (ends == 1)
              else assert (ends == 0)  -- exception killed before startSpan

  -- ----------------------------------------------------------------
  -- Property 5b: endSpan exactly once on normal success/failure
  --
  -- Without async exceptions, the span lifecycle must still close
  -- the span exactly once for both success and exception paths.
  -- ----------------------------------------------------------------
  describe "endSpan exactly once (no async exception)" $ do
    prop "success path: endSpan called once" $
      forAll arbitrary $ \(signal :: ControlSignal) ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder False
            statusVar <- newTVarIO SRunning
            runSessionSpanWithStatus fr.recorder statusVar "s" "u" "task" (pure signal)
            fr.getOps
          assert (countEnds ops == 1)
          assert (countStarts ops == 1)

    prop "agent-throws path: endSpan called once" $
      forAll (Text.pack <$> listOf (elements ['a'..'z'])) $ \errMsg ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder False
            statusVar <- newTVarIO SRunning
            runSessionSpanWithStatus fr.recorder statusVar "s" "u" "task"
              (throwIO (ErrorCall (Text.unpack errMsg)))
            fr.getOps
          assert (countEnds ops == 1)
          assert (countStarts ops == 1)

  -- ----------------------------------------------------------------
  -- Property 6: startSpan failure -> STerminated
  --
  -- When the recorder's startSpan throws, the session lifecycle
  -- must set status to STerminated. This tests the 'try' wrapper
  -- around startSpan.
  -- ----------------------------------------------------------------
  describe "startSpan failure" $ do
    prop "startSpan failure sets STerminated" $
      monadicIO $ do
        status <- run $ do
          fr <- newFakeRecorder True  -- startSpan will throw
          statusVar <- newTVarIO SRunning
          runSessionSpanWithStatus fr.recorder statusVar "s" "u" "task" (pure Continue)
          readTVarIO statusVar
        assert (status == STerminated)

  -- ----------------------------------------------------------------
  -- Property 7: startSpan failure -> no SpanEnd emitted
  --
  -- If startSpan throws, there is no span handle, so endSpan must
  -- never be called. This ensures we don't call endSpan on a
  -- phantom/null handle.
  -- ----------------------------------------------------------------
  describe "no SpanEnd when startSpan fails" $ do
    prop "no SpanEnd in ops when startSpan throws" $
      monadicIO $ do
        ops <- run $ do
          fr <- newFakeRecorder True  -- startSpan will throw
          statusVar <- newTVarIO SRunning
          runSessionSpanWithStatus fr.recorder statusVar "s" "u" "task" (pure Continue)
          fr.getOps
        assert (countEnds ops == 0)
        assert (countStarts ops == 0)

    prop "no ops at all when startSpan throws (no attr/error either)" $
      monadicIO $ do
        ops <- run $ do
          fr <- newFakeRecorder True
          statusVar <- newTVarIO SRunning
          runSessionSpanWithStatus fr.recorder statusVar "s" "u" "task"
            (throwIO (ErrorCall "should never run"))
          fr.getOps
        assert (null ops)


-- ===================================================================
-- Helpers
-- ===================================================================

countStarts :: [SpanOp] -> Int
countStarts = length . filter isStart
 where isStart (SpanStart _ _) = True
       isStart _               = False

countEnds :: [SpanOp] -> Int
countEnds = length . filter (== SpanEnd)
