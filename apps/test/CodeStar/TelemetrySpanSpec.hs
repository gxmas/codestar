{-# LANGUAGE OverloadedStrings #-}

-- | Property-based tests for the telemetry span lifecycle.
--
-- These properties verify invariants of the root @"agent.turn"@ span that
-- wraps each agent session in Server.hs.  Rather than spinning up a real
-- OTel backend we use a fake 'TelemetryRecorder' backed by an 'IORef' that
-- records every span operation, then assert structural properties over the
-- resulting trace.
--
-- Key invariants tested:
--
--   1. endSpan is called exactly once per startSpan (span balance)
--   2. task attribute is truncated to <= 200 chars
--   3. outcome attribute is set before endSpan on success
--   4. setSpanError is called before endSpan on exception
--   5. session.id and user.id are always present on the root span
module CodeStar.TelemetrySpanSpec (spec) where

import Control.Exception (ErrorCall (..), SomeException, try, throwIO)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run, monitor)

import OTel.Attribute (AttributeValue (..))
import CodeStar.Telemetry
  ( SpanHandle (..)
  , TelemetryRecorder (..)
  , signalLabel
  )
import CodeStar.Types (ControlSignal (..))
import CodeStar.Types.Gen () -- Arbitrary ControlSignal

-- ===================================================================
-- Fake recorder infrastructure
-- ===================================================================

-- | A single recorded span operation.  We track just enough structure to
-- verify ordering and attribute invariants without re-implementing OTel.
data SpanOp
  = SpanStart Text [(Text, AttributeValue)]
  | SpanEnd
  | SpanSetAttr Text Text
  | SpanSetError Text
  deriving stock (Show, Eq)

-- | A test-only recorder that appends every operation to an 'IORef'.
-- The 'SpanHandle' it produces is a dummy value -- we only need one
-- span at a time in these tests (the root span).
data FakeRecorder = FakeRecorder
  { recorder :: TelemetryRecorder
  , getOps   :: IO [SpanOp]
  }

newFakeRecorder :: IO FakeRecorder
newFakeRecorder = do
  ref <- newIORef ([] :: [SpanOp])
  let append op = atomicModifyIORef' ref (\ops -> (ops ++ [op], ()))
      dummyHandle = SpanHandle (error "FakeRecorder: SomeSpan not used") (error "FakeRecorder: Token not used")
      rec = TelemetryRecorder
        { recordEvent      = \_ -> pure ()
        , startSpan        = \name attrs -> do
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
-- The span lifecycle under test
--
-- This mirrors the span logic in Server.hs (lines 310-326).
-- We extract it here so we can test the protocol in isolation
-- without starting a WebSocket server.
-- ===================================================================

-- | Simulate the root span lifecycle from Server.hs.
-- @agentAction@ stands in for @runAgent env sysPrompt task@.
runSessionSpan
  :: TelemetryRecorder
  -> Text          -- ^ session id
  -> Text          -- ^ user id
  -> Text          -- ^ task
  -> IO ControlSignal  -- ^ the agent action (may throw)
  -> IO ()
runSessionSpan rec sid uid task agentAction = do
  rootSpan <- rec.startSpan "agent.turn"
    [ ("session.id", StringValue sid)
    , ("user.id",    StringValue uid)
    , ("task",       StringValue (Text.take 200 task))
    ]
  result <- try agentAction
  case result of
    Right signal -> do
      rec.setSpanAttr rootSpan "outcome" (signalLabel signal)
      rec.endSpan rootSpan
    Left (ex :: SomeException) -> do
      let msg = Text.pack (show ex)
      rec.setSpanError rootSpan msg
      rec.endSpan rootSpan

-- ===================================================================
-- Generators
-- ===================================================================

-- | Arbitrary text that deliberately explores length boundaries relevant
-- to the 200-char truncation rule.
genTask :: Gen Text
genTask = frequency
  [ (1, pure Text.empty)                           -- empty
  , (2, Text.pack <$> vectorOf 1 arbitrary)        -- single char
  , (3, Text.pack <$> vectorOf 199 (elements ['a'..'z']))  -- just under
  , (3, Text.pack <$> vectorOf 200 (elements ['a'..'z']))  -- exact boundary
  , (3, Text.pack <$> vectorOf 201 (elements ['a'..'z']))  -- just over
  , (2, Text.pack <$> vectorOf 500 arbitrary)      -- well over
  , (1, Text.pack <$> vectorOf 10000 arbitrary)    -- extreme
  , (5, arbitraryText)                              -- random
  ]

-- | General-purpose short text for IDs and such.
arbitraryText :: Gen Text
arbitraryText = Text.pack <$> listOf (elements (['a'..'z'] ++ ['0'..'9'] ++ ['-', '_']))

genSessionId :: Gen Text
genSessionId = do
  n <- chooseInt (1, 40)
  Text.pack <$> vectorOf n (elements (['a'..'f'] ++ ['0'..'9']))

genUserId :: Gen Text
genUserId = do
  n <- chooseInt (1, 30)
  Text.pack <$> vectorOf n (elements (['a'..'z'] ++ ['0'..'9']))

-- | An action outcome: either a successful ControlSignal or an exception.
data AgentOutcome
  = AgentSuccess ControlSignal
  | AgentThrows Text
  deriving stock (Show)

instance Arbitrary AgentOutcome where
  arbitrary = frequency
    [ (7, AgentSuccess <$> arbitrary)
    , (3, AgentThrows <$> arbitraryText)
    ]

outcomeToAction :: AgentOutcome -> IO ControlSignal
outcomeToAction (AgentSuccess sig) = pure sig
outcomeToAction (AgentThrows msg)  = throwIO (ErrorCall (Text.unpack msg))

-- ===================================================================
-- Helpers for asserting over the ops trace
-- ===================================================================

countStarts :: [SpanOp] -> Int
countStarts = length . filter isStart
 where isStart (SpanStart _ _) = True
       isStart _               = False

countEnds :: [SpanOp] -> Int
countEnds = length . filter (== SpanEnd)

-- | True if @SpanSetAttr key _@ appears before the final @SpanEnd@.
attrBeforeEnd :: Text -> [SpanOp] -> Bool
attrBeforeEnd key ops =
  case break (== SpanEnd) (reverse ops) of
    (_, _:preceding) -> any matchesKey (reverse preceding)
    _                -> False
 where
  matchesKey (SpanSetAttr k _) = k == key
  matchesKey _                 = False

-- | True if @SpanSetError _@ appears before the final @SpanEnd@.
errorBeforeEnd :: [SpanOp] -> Bool
errorBeforeEnd ops =
  case break (== SpanEnd) (reverse ops) of
    (_, _:preceding) -> any matchesError (reverse preceding)
    _                -> False
 where
  matchesError (SpanSetError _) = True
  matchesError _                = False

-- | Extract the attributes from the first @SpanStart@.
firstStartAttrs :: [SpanOp] -> Maybe [(Text, AttributeValue)]
firstStartAttrs ops =
  case find isStart ops of
    Just (SpanStart _ attrs) -> Just attrs
    _                        -> Nothing
 where
  isStart (SpanStart _ _) = True
  isStart _               = False

-- ===================================================================
-- Properties
-- ===================================================================

spec :: Spec
spec = describe "CodeStar.TelemetrySpan" $ do

  -- ------------------------------------------------------------------
  -- Property 1: span balance
  --
  -- The fundamental resource-safety invariant: every startSpan must be
  -- paired with exactly one endSpan, regardless of whether the agent
  -- succeeds, returns any variant of ControlSignal, or throws.
  -- ------------------------------------------------------------------
  describe "span balance" $ do
    prop "endSpan is called exactly once per startSpan for any outcome" $
      forAll arbitrary $ \outcome ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            _ <- try @SomeException (runSessionSpan fr.recorder "s" "u" "t" (outcomeToAction outcome))
            fr.getOps
          let starts = countStarts ops
              ends   = countEnds ops
          monitor (classify (outcomeIsSuccess outcome) "success")
          monitor (classify (outcomeIsThrows outcome)  "exception")
          assert (starts == 1)
          assert (ends == 1)
          assert (starts == ends)

  -- ------------------------------------------------------------------
  -- Property 2: task truncation
  --
  -- The task text passed to startSpan must never exceed 200 characters.
  -- This is a metamorphic property: regardless of input length, the
  -- observed attribute is bounded.
  -- ------------------------------------------------------------------
  describe "task truncation" $ do
    prop "task attribute on root span is always <= 200 chars" $
      forAll genTask $ \task ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            _ <- try @SomeException (runSessionSpan fr.recorder "s" "u" task (pure Continue))
            fr.getOps
          let taskAttr = do
                attrs <- firstStartAttrs ops
                lookup "task" attrs
          monitor (classify (Text.length task <= 200) "within limit")
          monitor (classify (Text.length task > 200)  "over limit")
          monitor (classify (Text.null task)           "empty")
          case taskAttr of
            Nothing -> assert False  -- task attribute must be present
            Just (StringValue v) -> assert (Text.length v <= 200)
            Just _               -> assert False  -- task should be StringValue

  -- ------------------------------------------------------------------
  -- Property 3: outcome attribute ordering
  --
  -- On a successful agent run, the "outcome" attribute must be set
  -- *before* endSpan is called.  This ensures the span carries the
  -- outcome when it is exported.
  -- ------------------------------------------------------------------
  describe "outcome attribute on success" $ do
    prop "outcome attribute is set before endSpan for every ControlSignal" $
      forAll arbitrary $ \signal ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            runSessionSpan fr.recorder "s" "u" "task" (pure signal)
            fr.getOps
          monitor (classify (isDone signal)       "Done")
          monitor (classify (isContinue signal)   "Continue")
          monitor (classify (isNeedsInput signal) "NeedsInput")
          monitor (classify (isBlocked signal)    "Blocked")
          assert (attrBeforeEnd "outcome" ops)

    prop "outcome value matches signalLabel" $
      forAll arbitrary $ \signal ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            runSessionSpan fr.recorder "s" "u" "task" (pure signal)
            fr.getOps
          let outcomeVal = extractAttr "outcome" ops
          assert (outcomeVal == Just (signalLabel signal))

  -- ------------------------------------------------------------------
  -- Property 4: error before end on exception
  --
  -- When the agent throws, setSpanError must be called before endSpan.
  -- This is the dual of Property 3 for the failure path.
  -- ------------------------------------------------------------------
  describe "error handling on exception" $ do
    prop "setSpanError is called before endSpan when agent throws" $
      forAll arbitraryText $ \errMsg ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            _ <- try @SomeException
              (runSessionSpan fr.recorder "s" "u" "task"
                (throwIO (ErrorCall (Text.unpack errMsg))))
            fr.getOps
          assert (errorBeforeEnd ops)
          assert (countEnds ops == 1)

    prop "no outcome attribute is set on exception path" $
      forAll arbitraryText $ \errMsg ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            _ <- try @SomeException
              (runSessionSpan fr.recorder "s" "u" "task"
                (throwIO (ErrorCall (Text.unpack errMsg))))
            fr.getOps
          let outcomeVal = extractAttr "outcome" ops
          assert (outcomeVal == Nothing)

  -- ------------------------------------------------------------------
  -- Property 5: required attributes on root span
  --
  -- session.id and user.id must always appear in the root span's
  -- initial attributes, regardless of their content.
  -- ------------------------------------------------------------------
  describe "required root span attributes" $ do
    prop "session.id and user.id are present on every root span" $
      forAll ((,) <$> genSessionId <*> genUserId) $ \(sid, uid) ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            _ <- try @SomeException (runSessionSpan fr.recorder sid uid "task" (pure Continue))
            fr.getOps
          case firstStartAttrs ops of
            Nothing -> assert False
            Just attrs -> do
              assert (hasKey "session.id" attrs)
              assert (hasKey "user.id" attrs)

    prop "session.id and user.id values match the inputs" $
      forAll ((,) <$> genSessionId <*> genUserId) $ \(sid, uid) ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            runSessionSpan fr.recorder sid uid "task" (pure Continue)
            fr.getOps
          case firstStartAttrs ops of
            Nothing -> assert False
            Just attrs -> do
              assert (lookup "session.id" attrs == Just (StringValue sid))
              assert (lookup "user.id" attrs == Just (StringValue uid))

  -- ------------------------------------------------------------------
  -- Property 6: span name is always "agent.turn"
  --
  -- A simple but important invariant: the span name must be exactly
  -- "agent.turn" for every session, so dashboards and queries that
  -- filter on it remain correct.
  -- ------------------------------------------------------------------
  describe "span naming" $ do
    prop "root span is always named agent.turn" $
      forAll arbitrary $ \outcome ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            _ <- try @SomeException (runSessionSpan fr.recorder "s" "u" "t" (outcomeToAction outcome))
            fr.getOps
          case ops of
            (SpanStart name _ : _) -> assert (name == "agent.turn")
            _ -> assert False

  -- ------------------------------------------------------------------
  -- Property 7: operation ordering
  --
  -- The trace must always follow the pattern:
  --   SpanStart ... (SpanSetAttr|SpanSetError)* SpanEnd
  -- No operations after SpanEnd, no SpanEnd before SpanStart.
  -- ------------------------------------------------------------------
  describe "operation ordering" $ do
    prop "no operations appear after SpanEnd" $
      forAll arbitrary $ \outcome ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            _ <- try @SomeException (runSessionSpan fr.recorder "s" "u" "t" (outcomeToAction outcome))
            fr.getOps
          let afterEnd = drop 1 $ dropWhile (/= SpanEnd) ops
          assert (null afterEnd)

    prop "SpanStart is always the first operation" $
      forAll arbitrary $ \outcome ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            _ <- try @SomeException (runSessionSpan fr.recorder "s" "u" "t" (outcomeToAction outcome))
            fr.getOps
          case ops of
            (SpanStart _ _ : _) -> assert True
            _ -> assert False

-- ===================================================================
-- Classification helpers
-- ===================================================================

outcomeIsSuccess :: AgentOutcome -> Bool
outcomeIsSuccess (AgentSuccess _) = True
outcomeIsSuccess _ = False

outcomeIsThrows :: AgentOutcome -> Bool
outcomeIsThrows (AgentThrows _) = True
outcomeIsThrows _ = False

isDone :: ControlSignal -> Bool
isDone (Done _) = True
isDone _ = False

isContinue :: ControlSignal -> Bool
isContinue Continue = True
isContinue _ = False

isNeedsInput :: ControlSignal -> Bool
isNeedsInput (NeedsInput _) = True
isNeedsInput _ = False

isBlocked :: ControlSignal -> Bool
isBlocked (Blocked _) = True
isBlocked _ = False

hasKey :: Text -> [(Text, AttributeValue)] -> Bool
hasKey k = any ((== k) . fst)

extractAttr :: Text -> [SpanOp] -> Maybe Text
extractAttr key ops =
  case find isTarget ops of
    Just (SpanSetAttr _ v) -> Just v
    _ -> Nothing
 where
  isTarget (SpanSetAttr k _) = k == key
  isTarget _ = False
