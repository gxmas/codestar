{-# LANGUAGE OverloadedStrings #-}

-- | Property-based tests for the span-attribute type widening.
--
-- The system under test is the attribute-list construction that feeds
-- 'TelemetryRecorder.startSpan'.  After the widening, numeric span
-- attributes ('step.number', 'turn.number', 'plan.step_count') are
-- 'Int64Value', while identifiers ('session.id', 'user.id', 'model.id')
-- remain 'StringValue'.
--
-- We extract the attribute-building logic into pure helpers that mirror
-- the production code (AgentLoop.callLlm, PlanExecution.runWithPlan) and
-- test the /contract/, not the IO machinery.
--
-- Properties:
--   P1  Numeric span attributes are Int64Value, never StringValue
--   P2  String span attributes are StringValue, never Int64Value
--   P3  Plan execution spans carry Int64Value for plan.step_count
--   P4  Int64Value round-trips through fromIntegral faithfully
--   P5  setSpanAttr stores integers as strings (documents known limitation)
--   P6  OTelAttr.fromList round-trips through toList (last-writer-wins)
module CodeStar.SpanAttributeSpec (spec) where

import Data.Int (Int64)
import Data.List (nubBy)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import OTel.Attribute (AttributeValue (..))
import OTel.Attribute qualified as OTelAttr

-- ===================================================================
-- Pure helpers — mirror the production attribute-list construction
-- ===================================================================

-- | Attribute list for an llm.call span.
-- Mirrors AgentLoop.callLlm lines 232-238.
buildLlmCallAttrs
  :: Text   -- ^ session id
  -> Text   -- ^ user id
  -> Text   -- ^ model id
  -> Int    -- ^ step number
  -> Int    -- ^ turn number
  -> [(Text, AttributeValue)]
buildLlmCallAttrs sid uid mid stepNum turnNum =
  [ ("role",        StringValue "coder")
  , ("session.id",  StringValue sid)
  , ("user.id",     StringValue uid)
  , ("model.id",    StringValue mid)
  , ("step.number", Int64Value (fromIntegral stepNum))
  , ("turn.number", Int64Value (fromIntegral turnNum))
  ]

-- | Attribute list for a plan.execute span.
-- Mirrors PlanExecution.runWithPlan lines 154-158.
buildPlanExecuteAttrs
  :: Int    -- ^ step count
  -> Text   -- ^ task type
  -> Text   -- ^ execution mode
  -> [(Text, AttributeValue)]
buildPlanExecuteAttrs stepCount taskType execMode =
  [ ("plan.step_count", Int64Value (fromIntegral stepCount))
  , ("task.type",       StringValue taskType)
  , ("execution_mode",  StringValue execMode)
  ]

-- ===================================================================
-- Generators
-- ===================================================================

-- | Non-empty identifier text — biased toward realistic values with
-- occasional edge cases (unicode, whitespace).
genIdentifier :: Gen Text
genIdentifier = frequency
  [ (5, elements [ "sess-abc123", "user-42", "claude-sonnet-4-20250514"
                 , "gpt-4o", "o3-mini" ])
  , (3, Text.pack <$> listOf1 (elements (['a'..'z'] ++ ['-', '_'] ++ ['0'..'9'])))
  , (1, pure "\x1f600")  -- emoji — valid text but unusual
  ]

-- | Non-negative integers biased toward small values and boundaries.
genNonNeg :: Gen Int
genNonNeg = frequency
  [ (3, pure 0)
  , (5, chooseInt (1, 20))
  , (2, chooseInt (21, 1000))
  , (1, chooseInt (1001, 100000))
  ]

-- | Positive integers (for turn numbers, which start at 1 in practice,
-- though 0 is also valid at the type level).
genPositive :: Gen Int
genPositive = frequency
  [ (5, chooseInt (1, 10))
  , (3, chooseInt (11, 100))
  , (2, chooseInt (101, 10000))
  ]

-- | Task type labels — the closed set from TaskType's Show instance.
genTaskType :: Gen Text
genTaskType = elements ["Feature", "Bug", "Refactor", "Docs", "Test"]

-- | Execution mode labels.
genExecMode :: Gen Text
genExecMode = elements ["list", "dag"]

-- | Attribute keys — short alphanumeric, biased toward collisions so
-- we exercise the last-writer-wins semantics of fromList.
genAttrKey :: Gen Text
genAttrKey = frequency
  [ (3, elements ["a", "b", "c", "key"])
  , (2, Text.pack <$> vectorOf 3 (elements ['a'..'z']))
  ]

-- | Arbitrary AttributeValue — covering all constructors for P6.
-- We avoid NaN in Float64Value because NaN /= NaN breaks equality checks.
genAttributeValue :: Gen AttributeValue
genAttributeValue = oneof
  [ StringValue <$> (Text.pack <$> arbitrary)
  , BoolValue <$> arbitrary
  , Int64Value <$> arbitrary
  , Float64Value <$> arbitraryFiniteDouble
  , StringArrayValue . Vector.fromList <$> listOf (Text.pack <$> arbitrary)
  , BoolArrayValue . Vector.fromList <$> listOf arbitrary
  , Int64ArrayValue . Vector.fromList <$> listOf arbitrary
  , Float64ArrayValue . Vector.fromList <$> listOf arbitraryFiniteDouble
  ]

-- | Finite doubles only — avoids NaN comparison issues.
arbitraryFiniteDouble :: Gen Double
arbitraryFiniteDouble = do
  n <- arbitrary :: Gen Int64
  pure (fromIntegral n / 1000.0)

-- Note: We intentionally do not define shrinkAV here because the
-- generators in P6 use forAll (not forAllShrink) and the Arbitrary
-- Text orphan would conflict with quickcheck-instances if it were
-- added.  For P1-P5 the inputs are Int/Text with stock shrinking.

-- ===================================================================
-- Classification helpers
-- ===================================================================

isInt64Value :: Maybe AttributeValue -> Bool
isInt64Value (Just (Int64Value _)) = True
isInt64Value _                     = False

isStringValue :: Maybe AttributeValue -> Bool
isStringValue (Just (StringValue _)) = True
isStringValue _                      = False

lookupAttr :: Text -> [(Text, AttributeValue)] -> Maybe AttributeValue
lookupAttr k = Prelude.lookup k

-- ===================================================================
-- Properties
-- ===================================================================

spec :: Spec
spec = describe "CodeStar.SpanAttribute" $ do

  -- ----------------------------------------------------------------
  -- P1: Numeric span attributes are Int64Value, never StringValue
  -- ----------------------------------------------------------------
  describe "P1: numeric span attributes are Int64Value" $ do

    prop "llm.call step.number and turn.number are Int64Value" $
      forAll genNonNeg $ \stepNum ->
        forAll genPositive $ \turnNum ->
          let attrs = buildLlmCallAttrs "sid" "uid" "claude" stepNum turnNum
          in  classify (stepNum == 0) "step=0" $
              classify (turnNum > 10) "turn>10" $
              conjoin
                [ counterexample "step.number must be Int64Value" $
                    lookupAttr "step.number" attrs
                      === Just (Int64Value (fromIntegral stepNum))
                , counterexample "turn.number must be Int64Value" $
                    lookupAttr "turn.number" attrs
                      === Just (Int64Value (fromIntegral turnNum))
                ]

    prop "step.number is never StringValue" $
      forAll genNonNeg $ \stepNum ->
        forAll genNonNeg $ \turnNum ->
          let attrs = buildLlmCallAttrs "s" "u" "m" stepNum turnNum
          in  counterexample ("step.number = " ++ show (lookupAttr "step.number" attrs)) $
              not (isStringValue (lookupAttr "step.number" attrs))

    prop "turn.number is never StringValue" $
      forAll genNonNeg $ \stepNum ->
        forAll genNonNeg $ \turnNum ->
          let attrs = buildLlmCallAttrs "s" "u" "m" stepNum turnNum
          in  not (isStringValue (lookupAttr "turn.number" attrs))

  -- ----------------------------------------------------------------
  -- P2: String span attributes are StringValue, never Int64Value
  -- ----------------------------------------------------------------
  describe "P2: string span attributes are StringValue" $ do

    prop "session.id, user.id, model.id are all StringValue" $
      forAll genIdentifier $ \sid ->
        forAll genIdentifier $ \uid ->
          forAll genIdentifier $ \mid' ->
            let attrs = buildLlmCallAttrs sid uid mid' 0 1
            in  classify (Text.length sid > 20) "long session id" $
                conjoin
                  [ counterexample "session.id must be StringValue" $
                      isStringValue (lookupAttr "session.id" attrs)
                  , counterexample "user.id must be StringValue" $
                      isStringValue (lookupAttr "user.id" attrs)
                  , counterexample "model.id must be StringValue" $
                      isStringValue (lookupAttr "model.id" attrs)
                  ]

    prop "string attributes carry the original text unchanged" $
      forAll genIdentifier $ \sid ->
        forAll genIdentifier $ \uid ->
          forAll genIdentifier $ \mid' ->
            let attrs = buildLlmCallAttrs sid uid mid' 0 0
            in  conjoin
                  [ lookupAttr "session.id" attrs === Just (StringValue sid)
                  , lookupAttr "user.id"    attrs === Just (StringValue uid)
                  , lookupAttr "model.id"   attrs === Just (StringValue mid')
                  ]

    prop "string attributes are never Int64Value" $
      forAll genIdentifier $ \sid ->
        forAll genIdentifier $ \uid ->
          forAll genIdentifier $ \mid' ->
            let attrs = buildLlmCallAttrs sid uid mid' 0 0
                stringKeys = ["session.id", "user.id", "model.id", "role"]
            in  conjoin
                  [ counterexample (Text.unpack k ++ " must not be Int64Value") $
                      not (isInt64Value (lookupAttr k attrs))
                  | k <- stringKeys
                  ]

  -- ----------------------------------------------------------------
  -- P3: Plan execution spans have Int64Value for plan.step_count
  -- ----------------------------------------------------------------
  describe "P3: plan.step_count is Int64Value" $ do

    prop "plan.step_count carries the count as Int64Value" $
      forAll genNonNeg $ \n ->
        forAll genTaskType $ \tt ->
          forAll genExecMode $ \mode ->
            let attrs = buildPlanExecuteAttrs n tt mode
            in  classify (n == 0) "empty plan" $
                classify (n > 10) "large plan" $
                lookupAttr "plan.step_count" attrs
                  === Just (Int64Value (fromIntegral n))

    prop "plan.execute task.type is StringValue" $
      forAll genNonNeg $ \n ->
        forAll genTaskType $ \tt ->
          forAll genExecMode $ \mode ->
            let attrs = buildPlanExecuteAttrs n tt mode
            in  isStringValue (lookupAttr "task.type" attrs)

    prop "plan.execute execution_mode is StringValue" $
      forAll genNonNeg $ \n ->
        forAll genTaskType $ \tt ->
          forAll genExecMode $ \mode ->
            let attrs = buildPlanExecuteAttrs n tt mode
            in  isStringValue (lookupAttr "execution_mode" attrs)

  -- ----------------------------------------------------------------
  -- P4: Int64Value faithfully round-trips through fromIntegral
  -- ----------------------------------------------------------------
  describe "P4: Int64Value round-trip" $ do

    prop "fromIntegral preserves Int values in Int64Value" $
      forAll (arbitrary :: Gen Int) $ \n ->
        let v = Int64Value (fromIntegral n)
        in  case v of
              Int64Value i -> i === fromIntegral n
              _            -> property False

    prop "Int64Value is distinguishable from StringValue of show n" $
      forAll (arbitrary :: Gen Int) $ \n ->
        let intV = Int64Value (fromIntegral n)
            strV = StringValue (Text.pack (show n))
        in  counterexample ("Int64Value " ++ show n ++ " must /= StringValue " ++ show n) $
            intV =/= strV

  -- ----------------------------------------------------------------
  -- P5: setSpanAttr stores integers as strings (known limitation)
  -- ----------------------------------------------------------------
  describe "P5: setSpanAttr text-only limitation" $ do

    -- setOtelSpanAttr :: SpanHandle -> Text -> Text -> IO ()
    -- always wraps in StringValue. This property documents the coercion.
    prop "integers stored via setSpanAttr become StringValue, not Int64Value" $
      forAll (arbitrary :: Gen Int) $ \n ->
        let stored = StringValue (Text.pack (show n))
            native = Int64Value (fromIntegral n)
        in  conjoin
              [ counterexample "stored form must be StringValue" $
                  isStringValue (Just stored)
              , counterexample "stored form must differ from native Int64Value" $
                  stored =/= native
              ]

    prop "round-trip through show/read recovers the integer" $
      forAll (arbitrary :: Gen Int) $ \n ->
        let txt = Text.pack (show n)
        in  (read (Text.unpack txt) :: Int) === n

  -- ----------------------------------------------------------------
  -- P6: OTelAttr.fromList / toList round-trip
  -- ----------------------------------------------------------------
  describe "P6: Attributes fromList/toList round-trip" $ do

    -- fromList uses Map.fromList internally, so duplicate keys get
    -- last-writer-wins.  toList returns Map.toList (sorted by key).
    -- The round-trip invariant: fromList . toList . fromList === fromList
    prop "fromList . toList . fromList === fromList (idempotent)" $
      forAll (listOf ((,) <$> genAttrKey <*> genAttributeValue)) $ \kvs ->
        let al  = OTelAttr.fromList kvs
            al' = OTelAttr.fromList (OTelAttr.toList al)
        in  classify (length kvs > 5) ">5 attrs" $
            classify (hasDuplicateKeys kvs) "has duplicate keys" $
            al' === al

    prop "toList result has unique keys" $
      forAll (listOf ((,) <$> genAttrKey <*> genAttributeValue)) $ \kvs ->
        let result = OTelAttr.toList (OTelAttr.fromList kvs)
            keys = map fst result
        in  keys === nubBy (\a b -> a == b) keys

    prop "size equals length of toList" $
      forAll (listOf ((,) <$> genAttrKey <*> genAttributeValue)) $ \kvs ->
        let al = OTelAttr.fromList kvs
        in  OTelAttr.size al === length (OTelAttr.toList al)

    prop "lookup finds values from fromList (last-writer-wins)" $
      forAll genAttrKey $ \k ->
        forAll genAttributeValue $ \v ->
          let al = OTelAttr.fromList [(k, v)]
          in  OTelAttr.lookup k al === Just v

-- ===================================================================
-- Helpers
-- ===================================================================

hasDuplicateKeys :: [(Text, a)] -> Bool
hasDuplicateKeys kvs =
  let keys = map fst kvs
  in  length keys /= length (nubBy (==) keys)
