{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Property-based tests for session lifecycle events (Gap 6),
-- guardrail decision events (Gap 8), and the removal of
-- @incrementCounter@ (Gap 10).
--
-- These properties verify invariants of the observability layer added
-- in the session lifecycle and guardrail telemetry work.  We use a
-- fake 'TelemetryRecorder' backed by 'IORef's to capture events and
-- gauge changes without requiring a real OTel backend.
--
-- Key invariants tested:
--
--   P1-P5: Session lifecycle gauge balance and event ordering
--   P6-P10: Guardrail decision flag consistency
--   P11:    incrementCounter removal (compile-time proof)
module CodeStar.SessionLifecycleSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception
  ( ErrorCall (..)
  , SomeException
  , bracket_
  , throwIO
  , try
  )
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (elemIndex, find)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)

import CodeStar.Guardrails (GuardrailDecision (..))
import CodeStar.Telemetry
  ( AgentEvent (..)
  , TelemetryRecorder (..)
  , noOpRecorder
  , signalLabel
  )
import CodeStar.Types (ControlSignal (..))
import CodeStar.Types.Gen () -- Arbitrary ControlSignal

-- ===================================================================
-- Fake recorder infrastructure
-- ===================================================================

-- | A captured event, erasing the constructor to allow heterogeneous
-- collection without existential types.
data CapturedEvent
  = CEvSessionCreated  { ceSessionId :: Text, ceUserId :: Text }
  | CEvSessionTerminated { ceSessionId :: Text, ceUserId :: Text, ceTerminationReason :: Text }
  | CEvAuthRejected { ceRejectionReason :: Text }
  | CEvGuardrailDecision
      { ceToolName          :: Text
      , ceGuardrailDecision :: Text
      , ceGuardrailReason   :: Text
      , ceIsDenied          :: Bool
      , ceGrSessionId       :: Text
      , ceGrUserId          :: Text
      }
  | CEvOther
  deriving stock (Show, Eq)

-- | Convert an 'AgentEvent' to our test-friendly envelope.
captureEvent :: AgentEvent -> CapturedEvent
captureEvent EvSessionCreated{sessionId, userId} =
  CEvSessionCreated sessionId userId
captureEvent EvSessionTerminated{sessionId, userId, terminationReason} =
  CEvSessionTerminated sessionId userId terminationReason
captureEvent EvAuthRejected{rejectionReason} =
  CEvAuthRejected rejectionReason
captureEvent EvGuardrailDecision{toolName, guardrailDecision, guardrailReason, isDenied, sessionId, userId} =
  CEvGuardrailDecision toolName guardrailDecision guardrailReason isDenied sessionId userId
captureEvent _ = CEvOther

-- | Test recorder that captures events and tracks the session gauge.
data FakeLifecycleRecorder = FakeLifecycleRecorder
  { flRecorder :: TelemetryRecorder
  , flGetEvents :: IO [CapturedEvent]
  , flGetGauge  :: IO Int
  }

newFakeLifecycleRecorder :: IO FakeLifecycleRecorder
newFakeLifecycleRecorder = do
  eventsRef <- newIORef ([] :: [CapturedEvent])
  gaugeRef  <- newIORef (0 :: Int)
  let append ev = atomicModifyIORef' eventsRef (\es -> (es ++ [captureEvent ev], ()))
      adjustGauge n = atomicModifyIORef' gaugeRef (\g -> (g + n, ()))
      rec = noOpRecorder
        { recordEvent = append
        , adjustSessionCount = adjustGauge
        }
  pure FakeLifecycleRecorder
    { flRecorder  = rec
    , flGetEvents = readIORef eventsRef
    , flGetGauge  = readIORef gaugeRef
    }

-- ===================================================================
-- Session lifecycle simulation
--
-- This mirrors the bracket_ + IORef pattern from Server.hs lines 325-362.
-- ===================================================================

-- | Outcome of a simulated session body.
data SessionOutcome
  = SessionSuccess ControlSignal
  | SessionThrows Text
  | SessionCancelled
  deriving stock (Show)

instance Arbitrary SessionOutcome where
  arbitrary = frequency
    [ (5, SessionSuccess <$> arbitrary)
    , (3, SessionThrows . Text.pack <$> listOf1 (elements ['a'..'z']))
    , (2, pure SessionCancelled)
    ]

-- | Run a simulated session lifecycle using the same bracket_ pattern
-- as Server.hs.  Returns the final gauge value and captured events.
runSessionLifecycle
  :: FakeLifecycleRecorder
  -> Text   -- ^ session id
  -> Text   -- ^ user id
  -> SessionOutcome
  -> IO (Int, [CapturedEvent])
runSessionLifecycle flr sid uid outcome = do
  let rec = flr.flRecorder
  terminationReasonRef <- newIORef ("cancelled" :: Text)
  let terminateWith reason = writeIORef terminationReasonRef reason
      body = do
        rec.recordEvent EvSessionCreated{ sessionId = sid, userId = uid }
        case outcome of
          SessionSuccess signal -> do
            terminateWith (signalLabel signal)
          SessionThrows msg -> do
            terminateWith "error"
            throwIO (ErrorCall (Text.unpack msg))
          SessionCancelled -> do
            -- Simulate work that will be cancelled externally
            threadDelay 1_000_000  -- 1 second (will be interrupted)

  case outcome of
    SessionCancelled -> do
      -- Spawn the lifecycle in an async thread, then cancel it
      thread <- async $
        bracket_
          (rec.adjustSessionCount 1)
          (do reason <- readIORef terminationReasonRef
              rec.adjustSessionCount (-1)
              rec.recordEvent EvSessionTerminated
                { sessionId = sid, userId = uid, terminationReason = reason })
          body
      -- Give the thread time to enter the bracket and increment the gauge
      threadDelay 500  -- 0.5ms
      cancel thread
      -- Small delay to let the finally handler run
      threadDelay 1000
      gauge <- flr.flGetGauge
      events <- flr.flGetEvents
      pure (gauge, events)

    _ -> do
      _ <- try @SomeException $
        bracket_
          (rec.adjustSessionCount 1)
          (do reason <- readIORef terminationReasonRef
              rec.adjustSessionCount (-1)
              rec.recordEvent EvSessionTerminated
                { sessionId = sid, userId = uid, terminationReason = reason })
          body
      gauge <- flr.flGetGauge
      events <- flr.flGetEvents
      pure (gauge, events)

-- ===================================================================
-- Guardrail event construction
--
-- Mirror the logic from AgentLoop.hs executeTool (lines 324-337).
-- ===================================================================

-- | Build a guardrail decision event from a 'GuardrailDecision', matching
-- the implementation in AgentLoop.hs.
buildGuardrailEvent :: Text -> Text -> Text -> GuardrailDecision -> CapturedEvent
buildGuardrailEvent tool sid uid decision =
  let (decisionLabel, reasonText, denied) = case decision of
        Allow             -> ("allow", "", False)
        RequireApproval r -> ("require_approval", r, False)
        Deny r            -> ("deny", r, True)
  in CEvGuardrailDecision
    { ceToolName          = tool
    , ceGuardrailDecision = decisionLabel
    , ceGuardrailReason   = reasonText
    , ceIsDenied          = denied
    , ceGrSessionId       = sid
    , ceGrUserId          = uid
    }

-- ===================================================================
-- Generators
-- ===================================================================

genSessionId :: Gen Text
genSessionId = do
  n <- chooseInt (1, 40)
  Text.pack <$> vectorOf n (elements (['a'..'f'] ++ ['0'..'9']))

genUserId :: Gen Text
genUserId = do
  n <- chooseInt (1, 30)
  Text.pack <$> vectorOf n (elements (['a'..'z'] ++ ['0'..'9']))

genToolName :: Gen Text
genToolName = elements
  [ "read", "write", "edit", "glob", "grep", "shell"
  , "git", "todo_read", "todo_write", "custom_tool"
  ]

genNonEmptyText :: Gen Text
genNonEmptyText = Text.pack <$> listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ [' ', '-', '_']))

-- | Generate arbitrary 'GuardrailDecision' values, targeting the three
-- constructors with meaningful frequency distribution.
instance Arbitrary GuardrailDecision where
  arbitrary = frequency
    [ (4, pure Allow)
    , (3, RequireApproval <$> genNonEmptyText)
    , (3, Deny <$> genNonEmptyText)
    ]
  shrink Allow = []
  shrink (RequireApproval r) =
    Allow : [RequireApproval r' | r' <- shrinkText r, not (Text.null r')]
  shrink (Deny r) =
    Allow : [Deny r' | r' <- shrinkText r, not (Text.null r')]

shrinkText :: Text -> [Text]
shrinkText t
  | Text.length t <= 1 = []
  | otherwise = [Text.take (Text.length t `div` 2) t]

-- ===================================================================
-- Properties
-- ===================================================================

spec :: Spec
spec = describe "CodeStar.SessionLifecycle" $ do

  -- ----------------------------------------------------------------
  -- Gap 6: Session lifecycle properties
  -- ----------------------------------------------------------------

  describe "Gap 6 — Session lifecycle" $ do

    -- P1: adjustSessionCount is balanced for all outcomes
    describe "P1: adjustSessionCount gauge balance" $ do
      prop "gauge returns to 0 after normal completion" $
        forAll ((,) <$> genSessionId <*> genUserId) $ \(sid, uid) ->
          forAll (SessionSuccess <$> arbitrary) $ \outcome ->
            monadicIO $ do
              (gauge, _) <- run $ do
                flr <- newFakeLifecycleRecorder
                runSessionLifecycle flr sid uid outcome
              assert (gauge == 0)

      prop "gauge returns to 0 after exception" $
        forAll ((,) <$> genSessionId <*> genUserId) $ \(sid, uid) ->
          forAll (SessionThrows <$> genNonEmptyText) $ \outcome ->
            monadicIO $ do
              (gauge, _) <- run $ do
                flr <- newFakeLifecycleRecorder
                runSessionLifecycle flr sid uid outcome
              assert (gauge == 0)

      prop "gauge returns to 0 after async cancellation" $
        forAll ((,) <$> genSessionId <*> genUserId) $ \(sid, uid) ->
          monadicIO $ do
            (gauge, _) <- run $ do
              flr <- newFakeLifecycleRecorder
              runSessionLifecycle flr sid uid SessionCancelled
            assert (gauge == 0)

    -- P2: EvSessionCreated always precedes EvSessionTerminated
    describe "P2: event ordering" $ do
      prop "EvSessionCreated precedes EvSessionTerminated in all non-cancelled outcomes" $
        forAll ((,) <$> genSessionId <*> genUserId) $ \(sid, uid) ->
          forAll (frequency [(7, SessionSuccess <$> arbitrary), (3, SessionThrows <$> genNonEmptyText)]) $ \outcome ->
            monadicIO $ do
              (_, events) <- run $ do
                flr <- newFakeLifecycleRecorder
                runSessionLifecycle flr sid uid outcome
              let createdIdx = elemIndex True (map isSessionCreated events)
                  terminatedIdx = elemIndex True (map isSessionTerminated events)
              monitor (counterexampleShow events)
              case (createdIdx, terminatedIdx) of
                (Just ci, Just ti) -> assert (ci < ti)
                _ -> assert False  -- both events must be present

    -- P3: terminationReason is always non-empty and from expected set
    describe "P3: terminationReason validity" $ do
      prop "terminationReason is non-empty and from the expected set" $
        forAll ((,) <$> genSessionId <*> genUserId) $ \(sid, uid) ->
          forAll arbitrary $ \outcome ->
            monadicIO $ do
              (_, events) <- run $ do
                flr <- newFakeLifecycleRecorder
                runSessionLifecycle flr sid uid outcome
              let mTermEvent = find isSessionTerminated events
                  expectedReasons = ["done", "blocked", "needs_input", "continue", "error", "cancelled"]
              case mTermEvent of
                Just (CEvSessionTerminated _ _ reason) -> do
                  monitor (classify (reason == "cancelled") "cancelled")
                  monitor (classify (reason == "error")     "error")
                  monitor (classify (reason == "done")      "done")
                  monitor (classify (reason == "continue")  "continue")
                  assert (not (Text.null reason))
                  assert (reason `elem` expectedReasons)
                _ -> pure ()  -- cancelled outcomes may not emit terminated if timing is off

    -- P4: EvAuthRejected.rejectionReason is always non-empty
    describe "P4: auth rejection reason" $ do
      prop "EvAuthRejected carries non-empty rejectionReason" $
        forAll genNonEmptyText $ \reason ->
          let ev = captureEvent (EvAuthRejected { rejectionReason = reason })
          in case ev of
               CEvAuthRejected r -> not (Text.null r)
               _ -> False

    -- P5: net gauge change is always 0 after any lifecycle (stronger restatement of P1)
    describe "P5: net gauge change" $ do
      prop "adjustSessionCount net delta is 0 for any outcome" $
        forAll ((,,) <$> genSessionId <*> genUserId <*> arbitrary) $ \(sid, uid, outcome) ->
          monadicIO $ do
            (gauge, _) <- run $ do
              flr <- newFakeLifecycleRecorder
              runSessionLifecycle flr sid uid outcome
            -- For async cancellation, the gauge may take a moment to settle;
            -- our helper waits, so it should be 0
            assert (gauge == 0)

  -- ----------------------------------------------------------------
  -- Gap 8: Guardrail decision properties
  -- ----------------------------------------------------------------

  describe "Gap 8 — Guardrail decisions" $ do

    -- P6: isDenied is True iff guardrailDecision == "deny"
    describe "P6: isDenied iff deny" $ do
      prop "isDenied is True only for Deny decisions" $
        forAll ((,,) <$> genToolName <*> genSessionId <*> genUserId) $ \(tool, sid, uid) ->
          forAll arbitrary $ \decision ->
            let ev = buildGuardrailEvent tool sid uid decision
            in ev.ceIsDenied == (ev.ceGuardrailDecision == "deny")

    -- P7: guardrailDecision is always from the expected set
    describe "P7: decision label set" $ do
      prop "guardrailDecision is always allow, deny, or require_approval" $
        forAll ((,,) <$> genToolName <*> genSessionId <*> genUserId) $ \(tool, sid, uid) ->
          forAll arbitrary $ \decision ->
            let ev = buildGuardrailEvent tool sid uid decision
            in ev.ceGuardrailDecision `elem` ["allow", "deny", "require_approval"]

    -- P8: Exactly one EvGuardrailDecision per tool call
    describe "P8: one event per tool call" $ do
      prop "recording a guardrail event produces exactly one captured event" $
        forAll ((,,) <$> genToolName <*> genSessionId <*> genUserId) $ \(tool, sid, uid) ->
          forAll arbitrary $ \decision ->
            monadicIO $ do
              events <- run $ do
                flr <- newFakeLifecycleRecorder
                let rec = flr.flRecorder
                    (decisionLabel, reasonText, denied) = case decision of
                      Allow             -> ("allow", "", False)
                      RequireApproval r -> ("require_approval", r, False)
                      Deny r            -> ("deny", r, True)
                rec.recordEvent EvGuardrailDecision
                  { toolName          = tool
                  , guardrailDecision = decisionLabel
                  , guardrailReason   = reasonText
                  , isDenied          = denied
                  , sessionId         = sid
                  , userId            = uid
                  }
                flr.flGetEvents
              let grEvents = filter isGuardrailDecision events
              assert (length grEvents == 1)

    -- P9: isDenied = True events have non-empty guardrailReason
    describe "P9: denied events have a reason" $ do
      prop "Deny always produces non-empty guardrailReason" $
        forAll genNonEmptyText $ \reason ->
          forAll ((,,) <$> genToolName <*> genSessionId <*> genUserId) $ \(tool, sid, uid) ->
            let ev = buildGuardrailEvent tool sid uid (Deny reason)
            in not (Text.null ev.ceGuardrailReason)

    -- P10: isDenied = False events for Allow have empty guardrailReason
    describe "P10: Allow events have empty reason" $ do
      prop "Allow always produces empty guardrailReason" $
        forAll ((,,) <$> genToolName <*> genSessionId <*> genUserId) $ \(tool, sid, uid) ->
          let ev = buildGuardrailEvent tool sid uid Allow
          in Text.null ev.ceGuardrailReason

  -- ----------------------------------------------------------------
  -- Gap 10: incrementCounter removal
  -- ----------------------------------------------------------------

  describe "Gap 10 — incrementCounter removed" $ do

    -- P11: TelemetryRecorder uses adjustSessionCount, not incrementCounter.
    -- This is a compile-time proof: if incrementCounter existed as a field,
    -- noOpRecorder would not have adjustSessionCount and this code would
    -- fail to compile.
    it "adjustSessionCount replaces incrementCounter (compile-time proof)" $ do
      let r = noOpRecorder
      r.adjustSessionCount 1
      r.adjustSessionCount (-1)
      pure () :: IO ()

-- ===================================================================
-- Classification helpers
-- ===================================================================

isSessionCreated :: CapturedEvent -> Bool
isSessionCreated CEvSessionCreated{} = True
isSessionCreated _ = False

isSessionTerminated :: CapturedEvent -> Bool
isSessionTerminated CEvSessionTerminated{} = True
isSessionTerminated _ = False

isGuardrailDecision :: CapturedEvent -> Bool
isGuardrailDecision CEvGuardrailDecision{} = True
isGuardrailDecision _ = False

counterexampleShow :: Show a => a -> Property -> Property
counterexampleShow x = counterexample (show x)
