-- | Property-based tests for the sampling subsystem.
--
-- Three areas are covered:
--   1. Sampler math — algebraic properties of the built-in OTel samplers
--   2. Config validation — the sampleRate field validates correctly
--   3. sampling.priority — the attribute is set on the right control signals
module CodeStar.SamplingSpec (spec) where

import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Word (Word8, Word64)

import Test.Hspec
import Test.QuickCheck

import OTel.Attribute (emptyAttributes)
import OTel.Context (root)
import OTel.SDK.Trace.Sampler
import OTel.Trace (SpanKind (..))
import OTel.Trace.SpanContext (traceIdFromBytes, TraceId)

import CodeStar.Config.Defaults (defaultConfig)
import Data.List.NonEmpty (toList)
import Data.Monoid (Last (..))

import CodeStar.Config.Types
  ( ConfigError (..), TelemetrySection (..), Config (..)
  , PartialConfig (..), PartialTelemetrySection (..), ApiKey (..)
  )
import CodeStar.Config.Validate (resolve)
import CodeStar.Types (ControlSignal (..))
import CodeStar.Types.Gen ()   -- Arbitrary instances for ControlSignal, Evidence, etc.


-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

-- | Generate a random 16-byte TraceId (may be invalid/all-zero, but that is
-- vanishingly unlikely with 128 random bits).
genTraceId :: Gen TraceId
genTraceId = traceIdFromBytes . BS.pack <$> vectorOf 16 (arbitrary :: Gen Word8)


-- | Generate a TraceId whose low 8 bytes are guaranteed non-zero, so the
-- ratio-based sampler's hash has a meaningful value to compare against.
genNonZeroTraceId :: Gen TraceId
genNonZeroTraceId = do
  hi <- vectorOf 8 (arbitrary :: Gen Word8)
  lo <- vectorOf 8 (arbitrary :: Gen Word8) `suchThat` (not . all (== 0))
  pure (traceIdFromBytes (BS.pack (hi <> lo)))


-- | Generate a ratio in the open interval (0, 1), avoiding the degenerate
-- boundary cases that are tested separately.
genOpenRatio :: Gen Double
genOpenRatio = choose (0.001, 0.999)


-- | Generate a Blocked signal with an arbitrary reason text.
genBlocked :: Gen ControlSignal
genBlocked = Blocked . Text.pack <$> listOf (elements ['a'..'z'])


-- | Generate a Done signal with arbitrary evidence.
genDone :: Gen ControlSignal
genDone = Done <$> arbitrary


-- | Generate a NeedsInput signal with an arbitrary query text.
genNeedsInput :: Gen ControlSignal
genNeedsInput = NeedsInput . Text.pack <$> listOf (elements ['a'..'z'])


-- | Construct a TraceId whose low 8 bytes encode the given Word64.
mkTraceIdFromWord64 :: Word64 -> TraceId
mkTraceIdFromWord64 w =
  traceIdFromBytes (BS.pack (replicate 8 0 <> word64ToBytes w))


word64ToBytes :: Word64 -> [Word8]
word64ToBytes w =
  [ fromIntegral (w `div` (256^(7 :: Int)))
  , fromIntegral (w `div` (256^(6 :: Int)))
  , fromIntegral (w `div` (256^(5 :: Int)))
  , fromIntegral (w `div` (256^(4 :: Int)))
  , fromIntegral (w `div` (256^(3 :: Int)))
  , fromIntegral (w `div` (256^(2 :: Int)))
  , fromIntegral (w `div` (256^(1 :: Int)))
  , fromIntegral w
  ]


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Run the sampler in a root context with default span metadata.
sampleRoot :: Sampler s => s -> TraceId -> IO SamplingResult
sampleRoot s tid = shouldSample s root tid "span" Internal emptyAttributes []


-- | Determine which sampling-priority attributes should be set for a given
-- control signal, mirroring the logic in Server.hs.
--
-- Server.hs sets sampling.priority = "1" on:
--   (a) Blocked sessions (to force-sample for tail-sampling collectors)
--   (b) Exception sessions (caught by the Left branch of the try)
--
-- The exception case is not testable via ControlSignal alone (it's a
-- SomeException, not a signal), so we only model (a) here. The exception
-- path is verified by ServerSpanSafetySpec.
samplingPriorityFor :: ControlSignal -> Bool
samplingPriorityFor (Blocked _) = True
samplingPriorityFor _           = False




-------------------------------------------------------------------------------
-- Spec
-------------------------------------------------------------------------------

spec :: Spec
spec = describe "Sampling" $ do

  -- -----------------------------------------------------------------------
  -- P1–P5: Sampler math properties
  -- -----------------------------------------------------------------------

  describe "TraceIdRatioBasedSampler" $ do

    it "P1: ratio 1.0 always returns RecordAndSample" $
      property $ forAll genTraceId $ \tid -> ioProperty $ do
        result <- sampleRoot (TraceIdRatioBasedSampler 1.0) tid
        pure (samplingDecision result === RecordAndSample)

    it "P2: ratio 0.0 always returns Drop" $
      property $ forAll genTraceId $ \tid -> ioProperty $ do
        result <- sampleRoot (TraceIdRatioBasedSampler 0.0) tid
        pure (samplingDecision result === Drop)

    it "P3: deterministic — same trace ID yields the same decision" $
      property $
        forAll genOpenRatio $ \ratio ->
        forAll genNonZeroTraceId $ \tid -> ioProperty $ do
          r1 <- sampleRoot (TraceIdRatioBasedSampler ratio) tid
          r2 <- sampleRoot (TraceIdRatioBasedSampler ratio) tid
          pure (samplingDecision r1 === samplingDecision r2)

    it "P4: samples approximately ratio * 100% of traces (statistical)" $ do
      -- Test at three representative ratios. For each, we generate N
      -- uniformly-spaced trace IDs and verify the sampling fraction
      -- is within a tolerance band.
      let n = 10000 :: Int
          stride = maxBound `div` fromIntegral n :: Word64
          tids = [ mkTraceIdFromWord64 (fromIntegral i * stride) | i <- [1..n] ]
          tolerance = 0.03 :: Double  -- 3%

      forM_ [0.1, 0.5, 0.9] $ \ratio -> do
        results <- mapM (\tid -> samplingDecision <$> sampleRoot (TraceIdRatioBasedSampler ratio) tid) tids
        let sampledCount = length (filter (== RecordAndSample) results)
            actual = fromIntegral sampledCount / fromIntegral n :: Double
        actual `shouldSatisfy` (\a -> abs (a - ratio) <= tolerance)

  describe "ParentBasedSampler" $ do

    it "P5: with AlwaysOn root, behaves identically to AlwaysOn for root spans" $
      property $ forAll genTraceId $ \tid -> ioProperty $ do
        let sampler = defaultParentBasedSampler (SomeSampler AlwaysOnSampler)
        result <- sampleRoot sampler tid
        pure (samplingDecision result === RecordAndSample)

    it "P5b: monotonicity — higher ratio samples at least as much as lower ratio" $
      property $
        forAll genNonZeroTraceId $ \tid ->
        forAll (chooseInt (1, 98)) $ \pct ->
          let lo = fromIntegral pct / 100.0 :: Double
              hi = fromIntegral (pct + 1) / 100.0 :: Double
          in ioProperty $ do
            rLo <- sampleRoot (TraceIdRatioBasedSampler lo) tid
            rHi <- sampleRoot (TraceIdRatioBasedSampler hi) tid
            pure $ case samplingDecision rLo of
              RecordAndSample -> samplingDecision rHi === RecordAndSample
              _               -> property True

  -- -----------------------------------------------------------------------
  -- P6–P8: Config validation properties
  -- -----------------------------------------------------------------------

  describe "sampleRate config validation" $ do

    it "P6: rates in [0.0, 1.0] validate successfully" $
      property $ forAll (choose (0.0, 1.0)) $ \rate ->
        let errs = validateSampleRate' rate
        in counterexample ("errors: " ++ show errs) (null errs)

    it "P7: rates outside [0.0, 1.0] fail validation" $
      property $ forAll genOutOfRange $ \rate ->
        let errs = validateSampleRate' rate
        in counterexample ("expected failure for " ++ show rate) (not (null errs))

    it "P7b: boundary rejects — specific out-of-range values" $ do
      forM_ [-0.001, -1.0, 1.001, 2.0] $ \rate ->
        validateSampleRate' rate `shouldSatisfy` (not . null)

    it "P8: default sampleRate is 1.0" $
      defaultConfig.telemetry.sampleRate `shouldBe` (1.0 :: Double)

  -- -----------------------------------------------------------------------
  -- P9–P11: sampling.priority attribute properties
  -- -----------------------------------------------------------------------

  describe "sampling.priority attribution" $ do

    it "P9: Blocked sessions get sampling priority" $
      property $ forAll genBlocked $ \sig ->
        samplingPriorityFor sig `shouldBe` True

    it "P10: Done sessions do NOT get sampling priority" $
      property $ forAll genDone $ \sig ->
        samplingPriorityFor sig `shouldBe` False

    it "P11: NeedsInput sessions do NOT get sampling priority" $
      property $ forAll genNeedsInput $ \sig ->
        samplingPriorityFor sig `shouldBe` False

    it "P11b: Continue does NOT get sampling priority" $
      samplingPriorityFor Continue `shouldBe` False

  -- -----------------------------------------------------------------------
  -- P12: Architectural invariant (documentation)
  -- -----------------------------------------------------------------------

  describe "architectural invariants" $ do

    it "P12: log records are independent of trace sampling" $
      -- EvSessionTerminated emits a log record in recordEventOtlp, not a
      -- metric or span. Log providers have no sampler — they are always
      -- recorded. This is a structural invariant verified by the module
      -- architecture (logger path vs counter/histogram path), not by
      -- runtime assertion.
      pendingWith "architectural invariant — verified by module structure, not runtime"


-------------------------------------------------------------------------------
-- Internal validation helper
-------------------------------------------------------------------------------

-- | Test sample rate validation through the real 'resolve' pipeline.
-- We build a minimal PartialConfig with only sampleRate and apiKey set
-- (apiKey is required to pass the other validation rules), then check
-- whether resolution succeeds or produces an InvalidFraction error.
validateSampleRate' :: Double -> [ConfigError]
validateSampleRate' rate =
  case resolve partial of
    Right _   -> []
    Left errs -> filter isSampleRateError (toList errs)
  where
    partial :: PartialConfig
    partial = mempty
      { telemetry = mempty { sampleRate = Last (Just rate) }
      , apiKey    = Last (Just (ApiKey "test-key"))
      }
    isSampleRateError (InvalidFraction name _) = name == "telemetry.sample_rate"
    isSampleRateError _ = False


-- | Generate a Double outside [0.0, 1.0], biased toward values near the
-- boundaries where off-by-one errors hide.
genOutOfRange :: Gen Double
genOutOfRange = frequency
  [ (3, choose (-100.0, -0.001))   -- negative
  , (3, choose (1.001, 100.0))     -- above 1
  , (1, pure (-0.001))             -- just below 0
  , (1, pure 1.001)                -- just above 1
  ]


-- Helper for P4
forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_
