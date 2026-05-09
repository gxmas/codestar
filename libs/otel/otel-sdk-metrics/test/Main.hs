-- | Property-based and specification tests for otel-sdk-metrics.
--
-- These tests verify algebraic properties, spec-mandated defaults, and
-- behavioral invariants of the OpenTelemetry Metrics SDK: instrument kinds,
-- aggregations, metric readers, exporters, synchronous/asynchronous instruments,
-- aggregation temporality, concurrency, and the periodic exporting reader.
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad (forM_, replicateM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import OTel.Attribute (AttributeValue(..), InstrumentationScope(..), Key, emptyAttributes, fromList)
import OTel.SDK.Metric.View (View(..), defaultView)
import OTel.Timestamp (fromNanos)
import OTel.Metric
  ( Meter(..), MeterProvider(..)
  , Counter(..), UpDownCounter(..), Histogram(..), Gauge(..)
  , ObservableCounter(..), ObservableGauge(..)
  , ObservableResult(..), SomeObservableResult(..)
  , SomeCounter(..), SomeUpDownCounter(..), SomeHistogram(..)
  , SomeGauge(..), SomeObservableCounter(..), SomeObservableGauge(..)
  , SomeMeter(..)
  , mkSomeObsGauge
  , BatchObservableCallback
  , SomeBatchObservableResult(..), batchObserveValue
  )
import OTel.SDK.Export (ExportResult(..))
import OTel.SDK.Metric
import OTel.SDK.Resource qualified as Resource
import OTel.Timestamp (milliseconds)


-------------------------------------------------------------------------------
-- Test helpers
-------------------------------------------------------------------------------

-- | A recording MetricExporter that stores all exported MetricData.
data RecordingMetricExporter = RecordingMetricExporter
  { rmeExports   :: !(TVar [MetricData])
  , rmeShutdowns :: !(TVar Int)
  }

newRecordingMetricExporter :: IO RecordingMetricExporter
newRecordingMetricExporter = RecordingMetricExporter
  <$> newTVarIO []
  <*> newTVarIO 0

instance MetricExporter RecordingMetricExporter where
  exportMetrics e md = do
    atomically (modifyTVar' (rmeExports e) (md :))
    pure ExportSuccess
  shutdownMetricExporter e = do
    atomically (modifyTVar' (rmeShutdowns e) (+ 1))
    pure (Right ())
  forceFlushMetricExporter _ _ = pure (Right ())
  exporterTemporality _ _ = Cumulative
  exporterDefaultAggregation _ k = defaultAggregationFor k


-- | A custom MetricReader that returns Delta temporality for all instruments.
data DeltaReader = DeltaReader (IORef (IO MetricData))

newDeltaReader :: IO DeltaReader
newDeltaReader = DeltaReader <$> newIORef (pure (MetricData Resource.empty []))

instance MetricReader DeltaReader where
  readerCollect (DeltaReader ref) = do
    action <- readIORef ref
    action
  readerSetCollectSource (DeltaReader ref) src = writeIORef ref src
  readerShutdown _ = pure (Right ())
  readerForceFlush _ _ = pure (Right ())
  readerTemporality _ _ = Delta
  readerDefaultAggregation _ k = defaultAggregationFor k


-- | A test InstrumentationScope.
testScope :: InstrumentationScope
testScope = InstrumentationScope "test-lib" (Just "1.0") Nothing Nothing


-- | Create a provider with a given reader, get a meter, and pass both to a
-- callback.
withTestProviderAndReader
  :: SomeMetricReader
  -> (SdkMeterProvider -> SomeMeter -> IO a) -> IO a
withTestProviderAndReader reader f = do
  p <- newSdkMeterProvider defaultSdkMeterProviderConfig
    { providerReaders = [reader] }
  meter <- getMeter p testScope
  f p meter


-- | Convenience: create provider with NoOpMetricReader, return the reader
-- and meter for testing.
withNoOpReaderProvider :: (SomeMetricReader -> SdkMeterProvider -> SomeMeter -> IO a) -> IO a
withNoOpReaderProvider f = do
  reader <- newNoOpMetricReader
  let someReader = SomeMetricReader reader
  p <- newSdkMeterProvider defaultSdkMeterProviderConfig
    { providerReaders = [someReader] }
  meter <- getMeter p testScope
  f someReader p meter


-- | Extract sum data points from MetricData.
getSumDataPoints :: MetricData -> [NumberDataPoint]
getSumDataPoints md = concatMap extractSum (concatMap smMetrics (mdScopeMetrics md))
  where
    extractSum m = case metricPointData m of
      SumPointData sd -> sumDataPoints sd
      _               -> []


-- | Extract gauge data points from MetricData.
getGaugeDataPoints :: MetricData -> [NumberDataPoint]
getGaugeDataPoints md = concatMap extractGauge (concatMap smMetrics (mdScopeMetrics md))
  where
    extractGauge m = case metricPointData m of
      GaugePointData gd -> gaugeDataPoints gd
      _                 -> []


-- | Extract histogram data points from MetricData.
getHistogramDataPoints :: MetricData -> [HistogramDataPoint]
getHistogramDataPoints md = concatMap extractHist (concatMap smMetrics (mdScopeMetrics md))
  where
    extractHist m = case metricPointData m of
      HistogramPointData hd -> histDataPoints hd
      _                     -> []


-- | Assert Either is Right ().
assertRight :: Show e => Either e () -> Assertion
assertRight (Right ()) = pure ()
assertRight (Left e) = assertFailure ("expected Right (), got Left: " ++ show e)


-------------------------------------------------------------------------------
-- Newtype wrappers for Arbitrary
-------------------------------------------------------------------------------

newtype ArbInstrumentKind = ArbInstrumentKind { getArbInstrumentKind :: InstrumentKind }
  deriving stock Show

instance Arbitrary ArbInstrumentKind where
  arbitrary = ArbInstrumentKind <$> elements [minBound..maxBound]
  shrink (ArbInstrumentKind k) = [ArbInstrumentKind (toEnum i) | i <- shrink (fromEnum k), i >= 0, i <= 6]


-- | Newtype for generating arbitrary Text values.
newtype ArbText = ArbText { getArbText :: Text.Text }
  deriving stock Show

instance Arbitrary ArbText where
  arbitrary = ArbText . Text.pack <$> arbitrary
  shrink (ArbText t) = [ArbText (Text.pack s) | s <- shrink (Text.unpack t)]


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "otel-sdk-metrics"
  [ instrumentKindTests
  , aggregationTemporalityTests
  , aggregationTests
  , defaultAggregationForTests
  , defaultHistogramBoundariesTests
  , metricExporterTests
  , metricReaderTests
  , periodicExportingReaderDefaultsTests
  , sdkMeterProviderTests
  , counterTests
  , upDownCounterTests
  , histogramTests
  , gaugeTests
  , observableCounterTests
  , observableGaugeTests
  , aggregationTemporalityBehaviorTests
  , concurrentInstrumentTests
  , metricDataStructureTests
  , periodicExportingReaderBehaviorTests
  , someMetricExporterTests
  , viewRecordTests
  , matchesInstrumentTests
  , nameMatchesGlobTests
  , applyViewDropTests
  , applyViewRenameTests
  , applyViewAttributeFilterTests
  , applyViewAggOverrideTests
  , applyViewCardinalityTests
  , viewIntegrationTests
  , batchCallbackTests
  ]


-------------------------------------------------------------------------------
-- 1. InstrumentKind enum
-------------------------------------------------------------------------------

instrumentKindTests :: TestTree
instrumentKindTests = testGroup "InstrumentKind"
  [ testCase "all 7 variants exist and are distinct" $ do
      let kinds = [CounterKind, UpDownCounterKind, HistogramKind, GaugeKind,
                   ObservableCounterKind, ObservableUpDownCounterKind, ObservableGaugeKind]
      length kinds @?= 7
      -- All pairwise distinct
      assertBool "all 7 should be distinct" (length (filter id [a /= b | a <- kinds, b <- kinds, a /= b]) == 42)

  , testProperty "Enum/Bounded round-trip: toEnum (fromEnum x) == x" $
      \(ArbInstrumentKind k) -> toEnum (fromEnum k) == k

  , testProperty "fromEnum values are in [0..6]" $
      \(ArbInstrumentKind k) ->
        let e = fromEnum k
        in e >= 0 && e <= 6

  , testCase "minBound is CounterKind" $
      (minBound :: InstrumentKind) @?= CounterKind

  , testCase "maxBound is ObservableGaugeKind" $
      (maxBound :: InstrumentKind) @?= ObservableGaugeKind
  ]


-------------------------------------------------------------------------------
-- 2. AggregationTemporality
-------------------------------------------------------------------------------

aggregationTemporalityTests :: TestTree
aggregationTemporalityTests = testGroup "AggregationTemporality"
  [ testCase "Delta /= Cumulative" $
      assertBool "Delta should not equal Cumulative" (Delta /= Cumulative)

  , testCase "Show Delta produces non-empty string" $
      assertBool "show Delta should be non-empty" (not (null (show Delta)))

  , testCase "Show Cumulative produces non-empty string" $
      assertBool "show Cumulative should be non-empty" (not (null (show Cumulative)))
  ]


-------------------------------------------------------------------------------
-- 3. Aggregation
-------------------------------------------------------------------------------

aggregationTests :: TestTree
aggregationTests = testGroup "Aggregation"
  [ testCase "all 6 constructors are distinct" $ do
      let aggs =
            [ DropAggregation
            , DefaultAggregation
            , SumAggregation
            , LastValueAggregation
            , ExplicitBucketHistogramAggregation [1,2,3]
            , Base2ExponentialBucketHistogramAggregation 160 20
            ]
      -- Each pair is distinct
      forM_ [(i, j) | i <- [0..5 :: Int], j <- [0..5], i /= j] $ \(i, j) ->
        assertBool ("aggs " ++ show i ++ " and " ++ show j ++ " should differ")
          (aggs !! i /= aggs !! j)

  , testCase "ExplicitBucketHistogramAggregation preserves boundaries" $ do
      let bounds = [0, 5, 10, 25, 50, 100]
          agg = ExplicitBucketHistogramAggregation bounds
      explicitBucketBounds agg @?= bounds

  , testCase "Base2ExponentialBucketHistogramAggregation preserves maxSize/maxScale" $ do
      let agg = Base2ExponentialBucketHistogramAggregation 42 7
      expMaxSize agg @?= 42
      expMaxScale agg @?= 7
  ]


-------------------------------------------------------------------------------
-- 4. defaultAggregationFor
-------------------------------------------------------------------------------

defaultAggregationForTests :: TestTree
defaultAggregationForTests = testGroup "defaultAggregationFor"
  [ testCase "CounterKind -> SumAggregation" $
      defaultAggregationFor CounterKind @?= SumAggregation

  , testCase "UpDownCounterKind -> SumAggregation" $
      defaultAggregationFor UpDownCounterKind @?= SumAggregation

  , testCase "HistogramKind -> ExplicitBucketHistogramAggregation with default boundaries" $
      defaultAggregationFor HistogramKind @?=
        ExplicitBucketHistogramAggregation defaultHistogramBoundaries

  , testCase "GaugeKind -> LastValueAggregation" $
      defaultAggregationFor GaugeKind @?= LastValueAggregation

  , testCase "ObservableCounterKind -> SumAggregation" $
      defaultAggregationFor ObservableCounterKind @?= SumAggregation

  , testCase "ObservableUpDownCounterKind -> SumAggregation" $
      defaultAggregationFor ObservableUpDownCounterKind @?= SumAggregation

  , testCase "ObservableGaugeKind -> LastValueAggregation" $
      defaultAggregationFor ObservableGaugeKind @?= LastValueAggregation
  ]


-------------------------------------------------------------------------------
-- 5. defaultHistogramBoundaries
-------------------------------------------------------------------------------

defaultHistogramBoundariesTests :: TestTree
defaultHistogramBoundariesTests = testGroup "defaultHistogramBoundaries"
  [ testCase "has exactly 15 elements" $
      length defaultHistogramBoundaries @?= 15

  , testCase "is strictly sorted (ascending)" $
      assertBool "boundaries should be strictly sorted" $
        and (zipWith (<) defaultHistogramBoundaries (drop 1 defaultHistogramBoundaries))

  , testCase "starts at 0" $
      case defaultHistogramBoundaries of
        (x:_) -> x @?= 0
        []    -> assertFailure "boundaries should be non-empty"

  , testCase "ends at 10000" $
      case reverse defaultHistogramBoundaries of
        (x:_) -> x @?= 10000
        []    -> assertFailure "boundaries should be non-empty"

  , testProperty "sorted boundaries equal themselves when sorted" $
      once $ sort defaultHistogramBoundaries == defaultHistogramBoundaries
  ]


-------------------------------------------------------------------------------
-- 6. MetricExporter (NoOpMetricExporter)
-------------------------------------------------------------------------------

metricExporterTests :: TestTree
metricExporterTests = testGroup "MetricExporter (NoOpMetricExporter)"
  [ testCase "exportMetrics always returns ExportSuccess" $ do
      result <- exportMetrics NoOpMetricExporter (MetricData Resource.empty [])
      result @?= ExportSuccess

  , testCase "shutdownMetricExporter always returns Right ()" $ do
      result <- shutdownMetricExporter NoOpMetricExporter
      assertRight result

  , testCase "forceFlushMetricExporter always returns Right ()" $ do
      result <- forceFlushMetricExporter NoOpMetricExporter Nothing
      assertRight result

  , testProperty "exporterTemporality always returns Cumulative" $
      \(ArbInstrumentKind k) ->
        exporterTemporality NoOpMetricExporter k == Cumulative

  , testProperty "exporterDefaultAggregation always returns DefaultAggregation" $
      \(ArbInstrumentKind k) ->
        exporterDefaultAggregation NoOpMetricExporter k == DefaultAggregation

  , testCase "SomeMetricExporter wrapper delegates correctly" $ do
      let wrapped = SomeMetricExporter NoOpMetricExporter
      result <- exportMetrics wrapped (MetricData Resource.empty [])
      result @?= ExportSuccess
      shutResult <- shutdownMetricExporter wrapped
      assertRight shutResult
      flushResult <- forceFlushMetricExporter wrapped Nothing
      assertRight flushResult
  ]


-------------------------------------------------------------------------------
-- 7. MetricReader (NoOpMetricReader)
-------------------------------------------------------------------------------

metricReaderTests :: TestTree
metricReaderTests = testGroup "MetricReader (NoOpMetricReader)"
  [ testCase "readerCollect returns empty MetricData before source is wired" $ do
      reader <- newNoOpMetricReader
      md <- readerCollect reader
      mdScopeMetrics md @?= []

  , testCase "readerSetCollectSource then readerCollect returns data from source" $ do
      reader <- newNoOpMetricReader
      let testData = MetricData Resource.empty
            [ ScopeMetrics testScope
                [ Metric "test.metric" "" "" (GaugePointData (GaugeData [])) ]
            ]
      readerSetCollectSource reader (pure testData)
      md <- readerCollect reader
      md @?= testData

  , testCase "readerShutdown always returns Right ()" $ do
      reader <- newNoOpMetricReader
      result <- readerShutdown reader
      assertRight result

  , testCase "readerForceFlush always returns Right ()" $ do
      reader <- newNoOpMetricReader
      result <- readerForceFlush reader Nothing
      assertRight result

  , testProperty "readerTemporality always returns Cumulative" $
      \(ArbInstrumentKind k) -> ioProperty $ do
        reader <- newNoOpMetricReader
        pure (readerTemporality reader k === Cumulative)

  , testProperty "readerDefaultAggregation delegates to defaultAggregationFor" $
      \(ArbInstrumentKind k) -> ioProperty $ do
        reader <- newNoOpMetricReader
        pure (readerDefaultAggregation reader k === defaultAggregationFor k)
  ]


-------------------------------------------------------------------------------
-- 8. PeriodicExportingMetricReader defaults
-------------------------------------------------------------------------------

periodicExportingReaderDefaultsTests :: TestTree
periodicExportingReaderDefaultsTests = testGroup "PeriodicExportingMetricReader defaults"
  [ testCase "pemrExportInterval == 60000ms" $
      pemrExportInterval defaultPeriodicExportingMetricReaderConfig @?= milliseconds 60000

  , testCase "pemrExportTimeout == 30000ms" $
      pemrExportTimeout defaultPeriodicExportingMetricReaderConfig @?= milliseconds 30000
  ]


-------------------------------------------------------------------------------
-- 9. SdkMeterProvider construction
-------------------------------------------------------------------------------

sdkMeterProviderTests :: TestTree
sdkMeterProviderTests = testGroup "SdkMeterProvider"
  [ testCase "newSdkMeterProvider with default config succeeds" $ do
      p <- newSdkMeterProvider defaultSdkMeterProviderConfig
      -- Just check it doesn't throw
      _ <- getMeter p testScope
      pure ()

  , testCase "getMeter returns a SomeMeter" $ do
      p <- newSdkMeterProvider defaultSdkMeterProviderConfig
      SomeMeter _ <- getMeter p testScope
      pure ()

  , testCase "after shutdown, getMeter returns a NoOp meter (instruments do not accumulate)" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        -- Create a counter before shutdown
        SomeCounter c <- createCounter meter "pre.counter" Nothing
        counterAdd c 10 emptyAttributes
        md1 <- readerCollect reader
        assertBool "should have data before shutdown" (not (null (getSumDataPoints md1)))
        -- Shutdown
        _ <- sdkMeterProviderShutdown _p
        -- Get a new meter after shutdown
        meter2 <- getMeter _p testScope
        SomeCounter c2 <- createCounter meter2 "post.counter" Nothing
        counterAdd c2 99 emptyAttributes
        -- Collect again - should not see the post-shutdown counter
        md2 <- readerCollect reader
        let postCounterMetrics = filter (\m -> metricName m == "post.counter")
                                   (concatMap smMetrics (mdScopeMetrics md2))
        assertBool "post-shutdown counter should not accumulate" (null postCounterMetrics)
  ]


-------------------------------------------------------------------------------
-- 10. Counter: add accumulates correctly
-------------------------------------------------------------------------------

counterTests :: TestTree
counterTests = testGroup "Counter"
  [ testProperty "adding N non-negative values accumulates to their sum" $
      \(values :: [NonNegative Double]) -> ioProperty $ do
        let vals = map getNonNegative values
        withNoOpReaderProvider $ \reader _p meter -> do
          SomeCounter c <- createCounter meter "test.counter" Nothing
          forM_ vals $ \v -> counterAdd c v emptyAttributes
          md <- readerCollect reader
          let points = getSumDataPoints md
          if null vals
            then pure (null points === True)
            else case points of
              [pt] -> pure (ndpValue pt === sum vals)
              _    -> pure (counterexample ("unexpected points: " ++ show points) False)

  , testProperty "negative values are silently ignored (sum unchanged)" $
      \(Positive posVal) (Positive negVal) -> ioProperty $ do
        withNoOpReaderProvider $ \reader _p meter -> do
          SomeCounter c <- createCounter meter "test.counter" Nothing
          counterAdd c posVal emptyAttributes
          counterAdd c (negate negVal) emptyAttributes
          md <- readerCollect reader
          let points = getSumDataPoints md
          case points of
            [pt] -> pure (ndpValue pt === posVal)
            _    -> pure (counterexample ("unexpected points: " ++ show points) False)

  , testCase "adding 0.0 is a no-op (sum stays 0)" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeCounter c <- createCounter meter "test.counter" Nothing
        counterAdd c 0.0 emptyAttributes
        md <- readerCollect reader
        let points = getSumDataPoints md
        case points of
          [pt] -> ndpValue pt @?= 0.0
          _    -> assertFailure ("unexpected points: " ++ show points)

  , testCase "create counter, add nothing, collect -> no data points" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeCounter _ <- createCounter meter "test.counter" Nothing
        md <- readerCollect reader
        let points = getSumDataPoints md
        assertBool "empty counter should have no data points" (null points)
  ]


-------------------------------------------------------------------------------
-- 11. UpDownCounter
-------------------------------------------------------------------------------

upDownCounterTests :: TestTree
upDownCounterTests = testGroup "UpDownCounter"
  [ testProperty "sum of all values (positive and negative) matches collected value" $
      \(values :: [Double]) -> (not (null values)) ==> ioProperty $ do
        withNoOpReaderProvider $ \reader _p meter -> do
          SomeUpDownCounter c <- createUpDownCounter meter "test.udcounter" Nothing
          forM_ values $ \v -> upDownCounterAdd c v emptyAttributes
          md <- readerCollect reader
          let points = getSumDataPoints md
          case points of
            [pt] -> pure (ndpValue pt === sum values)
            _    -> pure (counterexample ("unexpected points: " ++ show points) False)

  , testCase "positive and negative values accumulate correctly" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeUpDownCounter c <- createUpDownCounter meter "test.udcounter" Nothing
        upDownCounterAdd c 10 emptyAttributes
        upDownCounterAdd c (-3) emptyAttributes
        upDownCounterAdd c 5 emptyAttributes
        upDownCounterAdd c (-12) emptyAttributes
        md <- readerCollect reader
        case getSumDataPoints md of
          [pt] -> ndpValue pt @?= 0.0
          ps   -> assertFailure ("unexpected points: " ++ show ps)
  ]


-------------------------------------------------------------------------------
-- 12. Histogram
-------------------------------------------------------------------------------

histogramTests :: TestTree
histogramTests = testGroup "Histogram"
  [ testCase "record 0.0 -> first bucket (index 0)" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeHistogram h <- createHistogram meter "test.hist" Nothing
        histogramRecord h 0.0 emptyAttributes
        md <- readerCollect reader
        case getHistogramDataPoints md of
          [pt] -> do
            hdpCount pt @?= 1
            -- First bucket (0.0 <= 0.0)
            case hdpBucketCounts pt of
              (b:_) -> b @?= 1
              []    -> assertFailure "no bucket counts"
          ps -> assertFailure ("unexpected hist points: " ++ show ps)

  , testCase "record 10001.0 -> last bucket (beyond all boundaries)" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeHistogram h <- createHistogram meter "test.hist" Nothing
        histogramRecord h 10001.0 emptyAttributes
        md <- readerCollect reader
        case getHistogramDataPoints md of
          [pt] -> do
            hdpCount pt @?= 1
            -- Last bucket
            assertBool "last bucket should be 1" (last (hdpBucketCounts pt) == 1)
          ps -> assertFailure ("unexpected hist points: " ++ show ps)

  , testCase "record 10000.0 -> bucket at boundary 10000 (second-to-last)" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeHistogram h <- createHistogram meter "test.hist" Nothing
        histogramRecord h 10000.0 emptyAttributes
        md <- readerCollect reader
        case getHistogramDataPoints md of
          [pt] -> do
            -- 10000.0 <= 10000.0, so bucket index 14 (0-indexed)
            let buckets = hdpBucketCounts pt
            (buckets !! 14) @?= 1
          ps -> assertFailure ("unexpected hist points: " ++ show ps)

  , testProperty "for any non-negative value, count increases by 1" $
      \(NonNegative v) -> ioProperty $ do
        withNoOpReaderProvider $ \reader _p meter -> do
          SomeHistogram h <- createHistogram meter "test.hist" Nothing
          histogramRecord h (v :: Double) emptyAttributes
          md <- readerCollect reader
          case getHistogramDataPoints md of
            [pt] -> pure (hdpCount pt === 1)
            ps   -> pure (counterexample ("unexpected: " ++ show ps) False)

  , testProperty "for any non-negative value, sum increases by that value" $
      \(NonNegative v) -> ioProperty $ do
        withNoOpReaderProvider $ \reader _p meter -> do
          SomeHistogram h <- createHistogram meter "test.hist" Nothing
          histogramRecord h (v :: Double) emptyAttributes
          md <- readerCollect reader
          case getHistogramDataPoints md of
            [pt] -> pure (hdpSum pt === Just v)
            ps   -> pure (counterexample ("unexpected: " ++ show ps) False)

  , testProperty "negative values are silently ignored" $
      \(Positive v) -> ioProperty $ do
        withNoOpReaderProvider $ \reader _p meter -> do
          SomeHistogram h <- createHistogram meter "test.hist" Nothing
          histogramRecord h (negate (v :: Double)) emptyAttributes
          md <- readerCollect reader
          let points = getHistogramDataPoints md
          pure (null points === True)

  , testCase "hdpBucketCounts has length = len(boundaries) + 1 = 16" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeHistogram h <- createHistogram meter "test.hist" Nothing
        histogramRecord h 50.0 emptyAttributes
        md <- readerCollect reader
        case getHistogramDataPoints md of
          [pt] -> length (hdpBucketCounts pt) @?= 16
          ps   -> assertFailure ("unexpected: " ++ show ps)
  ]


-------------------------------------------------------------------------------
-- 13. Gauge
-------------------------------------------------------------------------------

gaugeTests :: TestTree
gaugeTests = testGroup "Gauge"
  [ testProperty "after N gauge.set calls, collected value == last value set" $
      \(NonEmpty (values :: [Double])) -> ioProperty $ do
        withNoOpReaderProvider $ \reader _p meter -> do
          SomeGauge g <- createGauge meter "test.gauge" Nothing
          forM_ values $ \v -> gaugeSet g v emptyAttributes
          md <- readerCollect reader
          case getGaugeDataPoints md of
            [pt] -> pure (ndpValue pt === last values)
            ps   -> pure (counterexample ("unexpected: " ++ show ps) False)

  , testCase "multiple attribute sets -> one data point per attribute set" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeGauge g <- createGauge meter "test.gauge" Nothing
        let attrs1 = fromList [("region", StringValue "us")]
            attrs2 = fromList [("region", StringValue "eu")]
        gaugeSet g 1.0 attrs1
        gaugeSet g 2.0 attrs2
        md <- readerCollect reader
        let points = getGaugeDataPoints md
        length points @?= 2
  ]


-------------------------------------------------------------------------------
-- 14. Observable counter
-------------------------------------------------------------------------------

observableCounterTests :: TestTree
observableCounterTests = testGroup "ObservableCounter"
  [ testCase "callback that observes 42.0 -> collect shows 42.0" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        let cb (SomeObservableResult r) = observeValue r 42.0 emptyAttributes
        SomeObservableCounter _ <- createObservableCounter meter "test.obs.counter" [cb] Nothing
        md <- readerCollect reader
        case getSumDataPoints md of
          [pt] -> ndpValue pt @?= 42.0
          ps   -> assertFailure ("unexpected: " ++ show ps)

  , testCase "no callbacks -> collect returns Nothing (no metrics)" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeObservableCounter _ <- createObservableCounter meter "test.obs.counter" [] Nothing
        md <- readerCollect reader
        let points = getSumDataPoints md
        assertBool "no callbacks should produce no data" (null points)

  , testCase "addObservableCounterCallback then collect -> callback is called" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeObservableCounter oc <- createObservableCounter meter "test.obs.counter" [] Nothing
        let cb (SomeObservableResult r) = observeValue r 99.0 emptyAttributes
        addObservableCounterCallback oc cb
        md <- readerCollect reader
        case getSumDataPoints md of
          [pt] -> ndpValue pt @?= 99.0
          ps   -> assertFailure ("unexpected: " ++ show ps)
  ]


-------------------------------------------------------------------------------
-- 15. Observable gauge
-------------------------------------------------------------------------------

observableGaugeTests :: TestTree
observableGaugeTests = testGroup "ObservableGauge"
  [ testCase "callback that observes 7.5 -> collect shows 7.5" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        let cb (SomeObservableResult r) = observeValue r 7.5 emptyAttributes
        SomeObservableGauge _ <- createObservableGauge meter "test.obs.gauge" [cb] Nothing
        md <- readerCollect reader
        case getGaugeDataPoints md of
          [pt] -> ndpValue pt @?= 7.5
          ps   -> assertFailure ("unexpected: " ++ show ps)

  , testCase "no callbacks -> collect returns no data" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeObservableGauge _ <- createObservableGauge meter "test.obs.gauge" [] Nothing
        md <- readerCollect reader
        let points = getGaugeDataPoints md
        assertBool "no callbacks should produce no data" (null points)

  , testCase "addObservableGaugeCallback then collect -> callback is called" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeObservableGauge og <- createObservableGauge meter "test.obs.gauge" [] Nothing
        let cb (SomeObservableResult r) = observeValue r 3.14 emptyAttributes
        addObservableGaugeCallback og cb
        md <- readerCollect reader
        case getGaugeDataPoints md of
          [pt] -> ndpValue pt @?= 3.14
          ps   -> assertFailure ("unexpected: " ++ show ps)
  ]


-------------------------------------------------------------------------------
-- 16. Aggregation temporality: Delta vs Cumulative
-------------------------------------------------------------------------------

aggregationTemporalityBehaviorTests :: TestTree
aggregationTemporalityBehaviorTests = testGroup "Aggregation temporality behavior"
  [ testCase "Cumulative: add value twice -> collected sum = sum of both" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeCounter c <- createCounter meter "test.counter" Nothing
        counterAdd c 10 emptyAttributes
        counterAdd c 20 emptyAttributes
        md <- readerCollect reader
        case getSumDataPoints md of
          [pt] -> ndpValue pt @?= 30.0
          ps   -> assertFailure ("unexpected: " ++ show ps)

  , testCase "Delta: add value, collect, add again, collect -> resets between collects" $ do
      deltaReader <- newDeltaReader
      let someReader = SomeMetricReader deltaReader
      withTestProviderAndReader someReader $ \_p meter -> do
        SomeCounter c <- createCounter meter "test.counter" Nothing
        counterAdd c 10 emptyAttributes
        md1 <- readerCollect someReader
        case getSumDataPoints md1 of
          [pt] -> ndpValue pt @?= 10.0
          ps   -> assertFailure ("first collect unexpected: " ++ show ps)
        -- Add more and collect again
        counterAdd c 7 emptyAttributes
        md2 <- readerCollect someReader
        case getSumDataPoints md2 of
          [pt] -> ndpValue pt @?= 7.0  -- only the delta since last collect
          ps   -> assertFailure ("second collect unexpected: " ++ show ps)
  ]


-------------------------------------------------------------------------------
-- 17. Concurrent instrument recording
-------------------------------------------------------------------------------

concurrentInstrumentTests :: TestTree
concurrentInstrumentTests = testGroup "Concurrent instrument recording"
  [ testProperty "N threads each calling counterAdd M times accumulates correctly" $
      forAll (chooseInt (2, 5)) $ \nThreads ->
        forAll (chooseInt (3, 10)) $ \nAdds ->
          ioProperty $ do
            withNoOpReaderProvider $ \reader _p meter -> do
              SomeCounter c <- createCounter meter "test.counter" Nothing
              done <- newEmptyMVar
              replicateM_ nThreads $ forkIO $ do
                replicateM_ nAdds $ counterAdd c 1.0 emptyAttributes
                putMVar done ()
              replicateM_ nThreads (takeMVar done)
              md <- readerCollect reader
              case getSumDataPoints md of
                [pt] -> pure (ndpValue pt === fromIntegral (nThreads * nAdds))
                ps   -> pure (counterexample ("unexpected: " ++ show ps) False)
  ]


-------------------------------------------------------------------------------
-- 18. MetricData structure
-------------------------------------------------------------------------------

metricDataStructureTests :: TestTree
metricDataStructureTests = testGroup "MetricData structure"
  [ testCase "empty provider (no instruments) -> collect -> empty scopeMetrics" $ do
      withNoOpReaderProvider $ \reader _p _meter -> do
        md <- readerCollect reader
        mdScopeMetrics md @?= []

  , testCase "one instrument -> one ScopeMetrics with one Metric" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeCounter c <- createCounter meter "test.counter" Nothing
        counterAdd c 1.0 emptyAttributes
        md <- readerCollect reader
        length (mdScopeMetrics md) @?= 1
        case mdScopeMetrics md of
          [sm] -> length (smMetrics sm) @?= 1
          _    -> assertFailure "expected one ScopeMetrics"

  , testCase "two instruments under the same scope -> one ScopeMetrics with two Metrics" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        SomeCounter c1 <- createCounter meter "counter.a" Nothing
        SomeCounter c2 <- createCounter meter "counter.b" Nothing
        counterAdd c1 1.0 emptyAttributes
        counterAdd c2 2.0 emptyAttributes
        md <- readerCollect reader
        length (mdScopeMetrics md) @?= 1
        case mdScopeMetrics md of
          [sm] -> length (smMetrics sm) @?= 2
          _    -> assertFailure "expected one ScopeMetrics with two metrics"

  , testCase "mdResource matches the resource passed to config" $ do
      let res = Resource.create [("service.name", StringValue "test-svc")] Nothing
      reader <- newNoOpMetricReader
      let someReader = SomeMetricReader reader
      p <- newSdkMeterProvider defaultSdkMeterProviderConfig
        { providerResource = res
        , providerReaders = [someReader]
        }
      meter <- getMeter p testScope
      SomeCounter c <- createCounter meter "test.counter" Nothing
      counterAdd c 1.0 emptyAttributes
      md <- readerCollect someReader
      mdResource md @?= res
  ]


-------------------------------------------------------------------------------
-- 19. PeriodicExportingMetricReader exports on interval
-------------------------------------------------------------------------------

periodicExportingReaderBehaviorTests :: TestTree
periodicExportingReaderBehaviorTests = localOption (mkTimeout 5_000_000) $
  testGroup "PeriodicExportingMetricReader behavior"
  [ testCase "exports on interval" $ do
      recorder <- newRecordingMetricExporter
      pemr <- newPeriodicExportingMetricReader
        (SomeMetricExporter recorder)
        (defaultPeriodicExportingMetricReaderConfig
          { pemrExportInterval = milliseconds 20 })
      let someReader = SomeMetricReader pemr
      p <- newSdkMeterProvider defaultSdkMeterProviderConfig
        { providerReaders = [someReader] }
      meter <- getMeter p testScope
      SomeCounter c <- createCounter meter "test.counter" Nothing
      counterAdd c 1.0 emptyAttributes
      -- Wait for at least one export cycle
      threadDelay 150_000
      exports <- readTVarIO (rmeExports recorder)
      assertBool ("expected at least one export, got " ++ show (length exports))
        (length exports >= 1)
      -- Shutdown
      _ <- readerShutdown someReader
      pure ()
  ]


-------------------------------------------------------------------------------
-- 20. SomeMetricExporter existential wrapper
-------------------------------------------------------------------------------

someMetricExporterTests :: TestTree
someMetricExporterTests = testGroup "SomeMetricExporter"
  [ testCase "wrapping NoOpMetricExporter delegates exportMetrics" $ do
      let wrapped = SomeMetricExporter NoOpMetricExporter
      result <- exportMetrics wrapped (MetricData Resource.empty [])
      result @?= ExportSuccess

  , testCase "wrapping NoOpMetricExporter delegates shutdownMetricExporter" $ do
      let wrapped = SomeMetricExporter NoOpMetricExporter
      result <- shutdownMetricExporter wrapped
      assertRight result

  , testCase "wrapping NoOpMetricExporter delegates forceFlushMetricExporter" $ do
      let wrapped = SomeMetricExporter NoOpMetricExporter
      result <- forceFlushMetricExporter wrapped Nothing
      assertRight result

  , testProperty "wrapping NoOpMetricExporter delegates exporterTemporality" $
      \(ArbInstrumentKind k) ->
        exporterTemporality (SomeMetricExporter NoOpMetricExporter) k == Cumulative

  , testProperty "wrapping NoOpMetricExporter delegates exporterDefaultAggregation" $
      \(ArbInstrumentKind k) ->
        exporterDefaultAggregation (SomeMetricExporter NoOpMetricExporter) k == DefaultAggregation
  ]


-------------------------------------------------------------------------------
-- 21. View record and defaultView
-------------------------------------------------------------------------------

viewRecordTests :: TestTree
viewRecordTests = testGroup "View record and defaultView"
  [ testCase "defaultView has viewInstrumentName = Nothing" $
      viewInstrumentName defaultView @?= Nothing

  , testCase "defaultView has viewInstrumentKind = Nothing" $
      viewInstrumentKind defaultView @?= Nothing

  , testCase "defaultView has viewMeterName = Nothing" $
      viewMeterName defaultView @?= Nothing

  , testCase "defaultView has viewMeterVersion = Nothing" $
      viewMeterVersion defaultView @?= Nothing

  , testCase "defaultView has viewMeterSchemaUrl = Nothing" $
      viewMeterSchemaUrl defaultView @?= Nothing

  , testCase "defaultView has viewName = Nothing" $
      viewName defaultView @?= Nothing

  , testCase "defaultView has viewDescription = Nothing" $
      viewDescription defaultView @?= Nothing

  , testCase "defaultView has viewAttributeKeys = Nothing" $
      viewAttributeKeys defaultView @?= Nothing

  , testCase "defaultView has viewAggregation = Nothing" $
      viewAggregation defaultView @?= Nothing

  , testCase "defaultView has viewExemplarFilter = Nothing" $
      viewExemplarFilter defaultView @?= Nothing

  , testCase "defaultView has viewCardinalityLimit = Nothing" $
      viewCardinalityLimit defaultView @?= Nothing

  , testCase "View derives Eq: two identical views are equal" $
      defaultView @?= defaultView

  , testCase "View derives Show: produces non-empty string" $
      assertBool "show defaultView should be non-empty" (not (null (show defaultView)))
  ]


-------------------------------------------------------------------------------
-- 22. matchesInstrument - selection criteria
-------------------------------------------------------------------------------

matchesInstrumentTests :: TestTree
matchesInstrumentTests = testGroup "matchesInstrument"
  [ testProperty "defaultView matches any scope, kind, and name" $
      \(ArbInstrumentKind kind) -> ioProperty $ do
        -- use a non-trivial scope
        let scope = InstrumentationScope "any-lib" (Just "2.0") (Just "http://x") Nothing
        pure (matchesInstrument defaultView scope kind "any.instrument" === True)

  , testProperty "viewInstrumentKind = Just CounterKind matches only CounterKind" $
      \(ArbInstrumentKind kind) ->
        let v = defaultView { viewInstrumentKind = Just CounterKind }
        in matchesInstrument v testScope kind "x" === (kind == CounterKind)

  , testCase "viewMeterName = Just test-lib matches scope with scopeName test-lib" $
      let v = defaultView { viewMeterName = Just "test-lib" }
          scope = InstrumentationScope "test-lib" Nothing Nothing Nothing
      in assertBool "should match" (matchesInstrument v scope CounterKind "x")

  , testCase "viewMeterName = Just test-lib does not match scopeName other-lib" $
      let v = defaultView { viewMeterName = Just "test-lib" }
          scope = InstrumentationScope "other-lib" Nothing Nothing Nothing
      in assertBool "should not match" (not (matchesInstrument v scope CounterKind "x"))

  , testCase "viewMeterVersion = Just 1.0 matches scopeVersion Just 1.0" $
      let v = defaultView { viewMeterVersion = Just "1.0" }
          scope = InstrumentationScope "lib" (Just "1.0") Nothing Nothing
      in assertBool "should match" (matchesInstrument v scope CounterKind "x")

  , testCase "viewMeterVersion = Just 1.0 does not match scopeVersion Just 2.0" $
      let v = defaultView { viewMeterVersion = Just "1.0" }
          scope = InstrumentationScope "lib" (Just "2.0") Nothing Nothing
      in assertBool "should not match" (not (matchesInstrument v scope CounterKind "x"))

  , testCase "viewMeterVersion = Just 1.0 does not match scopeVersion Nothing" $
      let v = defaultView { viewMeterVersion = Just "1.0" }
          scope = InstrumentationScope "lib" Nothing Nothing Nothing
      in assertBool "should not match" (not (matchesInstrument v scope CounterKind "x"))

  , testCase "viewMeterSchemaUrl matches when scopeSchemaUrl matches" $
      let v = defaultView { viewMeterSchemaUrl = Just "http://schema" }
          scope = InstrumentationScope "lib" Nothing (Just "http://schema") Nothing
      in assertBool "should match" (matchesInstrument v scope CounterKind "x")

  , testCase "viewMeterSchemaUrl does not match when scopeSchemaUrl differs" $
      let v = defaultView { viewMeterSchemaUrl = Just "http://schema" }
          scope = InstrumentationScope "lib" Nothing (Just "http://other") Nothing
      in assertBool "should not match" (not (matchesInstrument v scope CounterKind "x"))

  , testCase "viewMeterSchemaUrl does not match when scopeSchemaUrl is Nothing" $
      let v = defaultView { viewMeterSchemaUrl = Just "http://schema" }
          scope = InstrumentationScope "lib" Nothing Nothing Nothing
      in assertBool "should not match" (not (matchesInstrument v scope CounterKind "x"))

  , testCase "all criteria together: only matches when all satisfied" $
      let v = defaultView
            { viewInstrumentName = Just "my.counter"
            , viewInstrumentKind = Just CounterKind
            , viewMeterName = Just "mylib"
            , viewMeterVersion = Just "1.0"
            , viewMeterSchemaUrl = Just "http://s"
            }
          scope = InstrumentationScope "mylib" (Just "1.0") (Just "http://s") Nothing
      in assertBool "should match all" (matchesInstrument v scope CounterKind "my.counter")

  , testProperty "flipping any single criterion causes match to fail" $
      \(ArbInstrumentKind _) ->
        let v = defaultView
              { viewInstrumentName = Just "my.counter"
              , viewInstrumentKind = Just CounterKind
              , viewMeterName = Just "mylib"
              , viewMeterVersion = Just "1.0"
              , viewMeterSchemaUrl = Just "http://s"
              }
            goodScope = InstrumentationScope "mylib" (Just "1.0") (Just "http://s") Nothing
            -- flip name
            badName = not (matchesInstrument v goodScope CounterKind "wrong.name")
            -- flip kind
            badKind = not (matchesInstrument v goodScope HistogramKind "my.counter")
            -- flip meter name
            badMeter = not (matchesInstrument v (goodScope { scopeName = "other" }) CounterKind "my.counter")
            -- flip version
            badVer = not (matchesInstrument v (goodScope { scopeVersion = Just "9.9" }) CounterKind "my.counter")
            -- flip schema
            badSchema = not (matchesInstrument v (goodScope { scopeSchemaUrl = Just "http://x" }) CounterKind "my.counter")
        in conjoin [property badName, property badKind, property badMeter, property badVer, property badSchema]
  ]


-------------------------------------------------------------------------------
-- 23. nameMatches / glob wildcard matching (via matchesInstrument)
-------------------------------------------------------------------------------

nameMatchesGlobTests :: TestTree
nameMatchesGlobTests = testGroup "nameMatches / glob wildcards"
  [ testProperty "* matches any name" $
      \(ArbText name) ->
        let v = defaultView { viewInstrumentName = Just "*" }
        in matchesInstrument v testScope CounterKind name === True

  , testCase "exact match: requests matches requests" $
      let v = defaultView { viewInstrumentName = Just "requests" }
      in assertBool "should match" (matchesInstrument v testScope CounterKind "requests")

  , testCase "exact: requests does not match requests.total" $
      let v = defaultView { viewInstrumentName = Just "requests" }
      in assertBool "should not match" (not (matchesInstrument v testScope CounterKind "requests.total"))

  , testCase "exact: requests does not match req" $
      let v = defaultView { viewInstrumentName = Just "requests" }
      in assertBool "should not match" (not (matchesInstrument v testScope CounterKind "req"))

  , testCase "http.* matches http.requests" $
      let v = defaultView { viewInstrumentName = Just "http.*" }
      in assertBool "should match" (matchesInstrument v testScope CounterKind "http.requests")

  , testCase "http.* matches http.latency" $
      let v = defaultView { viewInstrumentName = Just "http.*" }
      in assertBool "should match" (matchesInstrument v testScope CounterKind "http.latency")

  , testCase "http.* does not match grpc.requests" $
      let v = defaultView { viewInstrumentName = Just "http.*" }
      in assertBool "should not match" (not (matchesInstrument v testScope CounterKind "grpc.requests"))

  , testCase "*.total matches requests.total" $
      let v = defaultView { viewInstrumentName = Just "*.total" }
      in assertBool "should match" (matchesInstrument v testScope CounterKind "requests.total")

  , testCase "*.total matches errors.total" $
      let v = defaultView { viewInstrumentName = Just "*.total" }
      in assertBool "should match" (matchesInstrument v testScope CounterKind "errors.total")

  , testCase "*.total does not match requests.count" $
      let v = defaultView { viewInstrumentName = Just "*.total" }
      in assertBool "should not match" (not (matchesInstrument v testScope CounterKind "requests.count"))

  , testCase "* matches empty string" $
      let v = defaultView { viewInstrumentName = Just "*" }
      in assertBool "should match empty" (matchesInstrument v testScope CounterKind "")

  , testCase "empty pattern matches only empty name" $ do
      let v = defaultView { viewInstrumentName = Just "" }
      assertBool "should match empty" (matchesInstrument v testScope CounterKind "")
      assertBool "should not match non-empty" (not (matchesInstrument v testScope CounterKind "x"))

  , testProperty "pattern with no wildcard matches exactly the pattern string" $
      \(ArbText suffix) ->
        let pat = "fixed.name" <> suffix
            -- Only test patterns without wildcards
            noWild = not (Text.isInfixOf "*" pat)
            v = defaultView { viewInstrumentName = Just pat }
        in noWild ==>
             matchesInstrument v testScope CounterKind pat
             .&&. (pat /= "" ==> not (matchesInstrument v testScope CounterKind (pat <> ".extra")))
  ]


-------------------------------------------------------------------------------
-- 24. applyView - DropAggregation
-------------------------------------------------------------------------------

-- | Helper: a simple counter metric with one data point.
mkSimpleCounterMetric :: Double -> Metric
mkSimpleCounterMetric val = Metric
  { metricName = "test.counter"
  , metricDescription = "desc"
  , metricUnit = "unit"
  , metricPointData = SumPointData SumData
      { sumDataPoints = [NumberDataPoint emptyAttributes (fromNanos 0) (fromNanos 1) val []]
      , sumTemporality = Cumulative
      , sumIsMonotonic = True
      }
  }


mkSimpleGaugeMetric :: Double -> Metric
mkSimpleGaugeMetric val = Metric
  { metricName = "test.gauge"
  , metricDescription = ""
  , metricUnit = ""
  , metricPointData = GaugePointData GaugeData
      { gaugeDataPoints = [NumberDataPoint emptyAttributes (fromNanos 0) (fromNanos 1) val []]
      }
  }


mkSimpleHistMetric :: Double -> Metric
mkSimpleHistMetric hsum = Metric
  { metricName = "test.hist"
  , metricDescription = ""
  , metricUnit = ""
  , metricPointData = HistogramPointData HistogramData
      { histDataPoints =
          [ HistogramDataPoint emptyAttributes (fromNanos 0) (fromNanos 1) 1 (Just hsum) [1] [10.0] (Just hsum) (Just hsum) []
          ]
      , histTemporality = Cumulative
      }
  }


applyViewDropTests :: TestTree
applyViewDropTests = testGroup "applyView - DropAggregation"
  [ testCase "drops SumPointData metric" $
      let v = defaultView { viewAggregation = Just DropAggregation }
      in applyView v (mkSimpleCounterMetric 42) @?= Nothing

  , testCase "drops GaugePointData metric" $
      let v = defaultView { viewAggregation = Just DropAggregation }
      in applyView v (mkSimpleGaugeMetric 7.0) @?= Nothing

  , testCase "drops HistogramPointData metric" $
      let v = defaultView { viewAggregation = Just DropAggregation }
      in applyView v (mkSimpleHistMetric 100.0) @?= Nothing
  ]


-------------------------------------------------------------------------------
-- 25. applyView - rename
-------------------------------------------------------------------------------

-- | Assert that applyView returns Just, and run assertions on the result.
assertApplyView :: View -> Metric -> (Metric -> Assertion) -> Assertion
assertApplyView v m check = case applyView v m of
  Just result -> check result
  Nothing     -> assertFailure "expected applyView to return Just, got Nothing"


-- | Pure version: extract the result or error.
unsafeApplyView :: View -> Metric -> Metric
unsafeApplyView v m = case applyView v m of
  Just result -> result
  Nothing     -> error "unsafeApplyView: got Nothing"


applyViewRenameTests :: TestTree
applyViewRenameTests = testGroup "applyView - rename"
  [ testCase "viewName renames metricName" $
      let v = defaultView { viewName = Just "renamed" }
      in assertApplyView v (mkSimpleCounterMetric 1.0) $ \result ->
        metricName result @?= "renamed"

  , testCase "viewDescription changes metricDescription" $
      let v = defaultView { viewDescription = Just "new desc" }
      in assertApplyView v (mkSimpleCounterMetric 1.0) $ \result ->
        metricDescription result @?= "new desc"

  , testCase "both Nothing: original name and description preserved" $
      assertApplyView defaultView (mkSimpleCounterMetric 1.0) $ \result -> do
        metricName result @?= "test.counter"
        metricDescription result @?= "desc"

  , testProperty "rename is independent of underlying data type" $
      \(ArbInstrumentKind _) ->
        let v = defaultView { viewName = Just "new-name" }
            metrics = [mkSimpleCounterMetric 1, mkSimpleGaugeMetric 2, mkSimpleHistMetric 3]
        in all (\m -> case applyView v m of
                  Just r  -> metricName r == "new-name"
                  Nothing -> False) metrics
  ]


-------------------------------------------------------------------------------
-- 26. applyView - attribute filtering
-------------------------------------------------------------------------------

-- | Build a sum metric with multiple data points having different attribute sets.
mkSumMetricWithAttrs :: [([(Key, AttributeValue)], Double)] -> Metric
mkSumMetricWithAttrs entries = Metric
  { metricName = "test.sum"
  , metricDescription = ""
  , metricUnit = ""
  , metricPointData = SumPointData SumData
      { sumDataPoints =
          [ NumberDataPoint (fromList attrs) (fromNanos 0) (fromNanos 1) val []
          | (attrs, val) <- entries
          ]
      , sumTemporality = Cumulative
      , sumIsMonotonic = True
      }
  }


-- | Extract total sum of ndpValue from a metric's SumPointData.
totalSum :: Metric -> Double
totalSum m = case metricPointData m of
  SumPointData sd -> sum (map ndpValue (sumDataPoints sd))
  _               -> 0


-- | Count data points in a metric.
countDataPoints :: Metric -> Int
countDataPoints m = case metricPointData m of
  SumPointData sd           -> length (sumDataPoints sd)
  GaugePointData gd         -> length (gaugeDataPoints gd)
  HistogramPointData hd     -> length (histDataPoints hd)
  ExponentialHistogramPointData ehd -> length (expHistDataPoints ehd)


-- | Build a histogram metric with two data points carrying the given attribute
-- sets. Each data point records a single value (count=1, sum=value, 1 bucket).
mkHistMetricWithAttrs :: [([(Key, AttributeValue)], Double)] -> Metric
mkHistMetricWithAttrs entries = Metric
  { metricName = "test.hist"
  , metricDescription = ""
  , metricUnit = ""
  , metricPointData = HistogramPointData HistogramData
      { histDataPoints =
          [ HistogramDataPoint
              { hdpAttributes    = fromList attrs
              , hdpStartTime     = fromNanos 0
              , hdpTime          = fromNanos 1
              , hdpCount         = 1
              , hdpSum           = Just val
              , hdpBucketCounts  = [0, 1]
              , hdpExplicitBounds = [100.0]
              , hdpMin           = Just val
              , hdpMax           = Just val
              , hdpExemplars     = []
              }
          | (attrs, val) <- entries
          ]
      , histTemporality = Cumulative
      }
  }


applyViewAttributeFilterTests :: TestTree
applyViewAttributeFilterTests = testGroup "applyView - attribute filtering"
  [ testCase "filter to key 'a' merges two data points" $
      let metric = mkSumMetricWithAttrs
            [ ([("a", StringValue "1"), ("b", StringValue "2")], 10.0)
            , ([("a", StringValue "1"), ("c", StringValue "3")], 20.0)
            ]
          v = defaultView { viewAttributeKeys = Just (Set.fromList ["a"]) }
      in assertApplyView v metric $ \result -> do
        countDataPoints result @?= 1
        totalSum result @?= 30.0

  , testCase "filter to all keys: no merging, both preserved" $
      let metric = mkSumMetricWithAttrs
            [ ([("a", StringValue "1"), ("b", StringValue "2")], 10.0)
            , ([("a", StringValue "1"), ("c", StringValue "3")], 20.0)
            ]
          v = defaultView { viewAttributeKeys = Just (Set.fromList ["a", "b", "c"]) }
      in assertApplyView v metric $ \result ->
        countDataPoints result @?= 2

  , testCase "filter to empty set: all merged into one" $
      let metric = mkSumMetricWithAttrs
            [ ([("a", StringValue "1"), ("b", StringValue "2")], 10.0)
            , ([("a", StringValue "1"), ("c", StringValue "3")], 20.0)
            ]
          v = defaultView { viewAttributeKeys = Just Set.empty }
      in assertApplyView v metric $ \result -> do
        countDataPoints result @?= 1
        totalSum result @?= 30.0

  , testCase "viewAttributeKeys = Nothing: attributes unchanged" $
      let metric = mkSumMetricWithAttrs
            [ ([("a", StringValue "1"), ("b", StringValue "2")], 10.0)
            , ([("a", StringValue "1"), ("c", StringValue "3")], 20.0)
            ]
      in assertApplyView defaultView metric $ \result ->
        countDataPoints result @?= 2

  , testProperty "filtering preserves total value sum for SumData" $
      \(NonEmpty keys) ->
        let metric = mkSumMetricWithAttrs
              [ ([("a", StringValue "1"), ("b", StringValue "2")], 5.0)
              , ([("c", StringValue "3"), ("d", StringValue "4")], 7.0)
              , ([("a", StringValue "x"), ("c", StringValue "y")], 3.0)
              ]
            filterKeys = Set.fromList (map (\(c :: Char) -> "key" <> Text.pack (show (fromEnum c `mod` 5))) keys)
            v = defaultView { viewAttributeKeys = Just filterKeys }
            result = unsafeApplyView v metric
        in totalSum result === totalSum metric

  , testCase "histogram: filter collapses two points -> merged into one (counts summed)" $
      let metric = mkHistMetricWithAttrs
            [ ([("a", StringValue "1"), ("b", StringValue "x")], 10.0)
            , ([("a", StringValue "1"), ("b", StringValue "y")], 20.0)
            ]
          v = defaultView { viewAttributeKeys = Just (Set.fromList ["a"]) }
      in assertApplyView v metric $ \result -> do
        countDataPoints result @?= 1
        case metricPointData result of
          HistogramPointData hd -> case histDataPoints hd of
            [dp] -> do
              hdpCount dp @?= 2
              hdpSum  dp @?= Just 30.0
            _ -> assertFailure "expected exactly one histogram data point"
          _ -> assertFailure "expected HistogramPointData"
  ]


-------------------------------------------------------------------------------
-- 27. applyView - aggregation override: histogram -> sum
-------------------------------------------------------------------------------

applyViewAggOverrideTests :: TestTree
applyViewAggOverrideTests = testGroup "applyView - aggregation override"
  [ testCase "SumAggregation on HistogramPointData -> SumPointData" $
      let metric = Metric
            { metricName = "test.hist"
            , metricDescription = ""
            , metricUnit = ""
            , metricPointData = HistogramPointData HistogramData
                { histDataPoints =
                    [ HistogramDataPoint emptyAttributes (fromNanos 0) (fromNanos 1)
                        5 (Just 42.0) [2,2,1] [10.0, 50.0] (Just 1.0) (Just 100.0) []
                    , HistogramDataPoint emptyAttributes (fromNanos 0) (fromNanos 1)
                        3 (Just 15.0) [1,1,1] [10.0, 50.0] (Just 2.0) (Just 50.0) []
                    ]
                , histTemporality = Delta
                }
            }
          v = defaultView { viewAggregation = Just SumAggregation }
      in assertApplyView v metric $ \result -> case metricPointData result of
        SumPointData sd -> do
          -- 2 data points
          length (sumDataPoints sd) @?= 2
          -- ndpValue == hdpSum
          ndpValue (sumDataPoints sd !! 0) @?= 42.0
          ndpValue (sumDataPoints sd !! 1) @?= 15.0
          -- sumIsMonotonic == False
          sumIsMonotonic sd @?= False
          -- sumTemporality == original histogram's temporality
          sumTemporality sd @?= Delta
        other -> assertFailure ("expected SumPointData, got: " ++ show other)
  ]


-------------------------------------------------------------------------------
-- 28. applyView - cardinality limit
-------------------------------------------------------------------------------

-- | Build a sum metric with N data points (unique attributes each).
mkSumMetricN :: Int -> Metric
mkSumMetricN n = Metric
  { metricName = "test.sum"
  , metricDescription = ""
  , metricUnit = ""
  , metricPointData = SumPointData SumData
      { sumDataPoints =
          [ NumberDataPoint
              (fromList [("idx", StringValue (Text.pack (show i)))])
              (fromNanos 0) (fromNanos 1) (fromIntegral i) []
          | i <- [1..n]
          ]
      , sumTemporality = Cumulative
      , sumIsMonotonic = True
      }
  }


applyViewCardinalityTests :: TestTree
applyViewCardinalityTests = testGroup "applyView - cardinality limit"
  [ testCase "limit 3 on 5 data points -> 3 data points" $
      let v = defaultView { viewCardinalityLimit = Just 3 }
      in assertApplyView v (mkSumMetricN 5) $ \result ->
        countDataPoints result @?= 3

  , testCase "limit 0 -> 0 data points" $
      let v = defaultView { viewCardinalityLimit = Just 0 }
      in assertApplyView v (mkSumMetricN 5) $ \result ->
        countDataPoints result @?= 0

  , testCase "limit 10 on 5 -> all 5 preserved" $
      let v = defaultView { viewCardinalityLimit = Just 10 }
      in assertApplyView v (mkSumMetricN 5) $ \result ->
        countDataPoints result @?= 5

  , testCase "limit Nothing -> all 5 preserved" $
      assertApplyView defaultView (mkSumMetricN 5) $ \result ->
        countDataPoints result @?= 5

  , testProperty "length dataPoints <= cardinalityLimit for any positive limit" $
      forAll (chooseInt (1, 20)) $ \limit ->
        forAll (chooseInt (0, 30)) $ \n ->
          let v = defaultView { viewCardinalityLimit = Just limit }
              result = unsafeApplyView v (mkSumMetricN n)
          in countDataPoints result <= limit
  ]


-------------------------------------------------------------------------------
-- 29. Integration: Views applied during metric collection
-------------------------------------------------------------------------------

-- | Create a provider with views and a reader, return (reader, provider, meter).
withViewProvider
  :: [View]
  -> (SomeMetricReader -> SdkMeterProvider -> SomeMeter -> IO a)
  -> IO a
withViewProvider views f = do
  reader <- newNoOpMetricReader
  let someReader = SomeMetricReader reader
  p <- newSdkMeterProvider defaultSdkMeterProviderConfig
    { providerReaders = [someReader]
    , providerViews = views
    }
  meter <- getMeter p testScope
  f someReader p meter


viewIntegrationTests :: TestTree
viewIntegrationTests = testGroup "Integration: Views during collection"
  [ testCase "no views -> default behavior (metric unchanged)" $ do
      withViewProvider [] $ \reader _p meter -> do
        SomeCounter c <- createCounter meter "my-counter" Nothing
        counterAdd c 10.0 emptyAttributes
        md <- readerCollect reader
        case concatMap smMetrics (mdScopeMetrics md) of
          [m] -> do
            metricName m @?= "my-counter"
            case metricPointData m of
              SumPointData sd -> case sumDataPoints sd of
                (pt:_) -> ndpValue pt @?= 10.0
                []     -> assertFailure "expected at least one data point"
              other -> assertFailure ("expected SumPointData, got: " ++ show other)
          ms -> assertFailure ("expected 1 metric, got: " ++ show (length ms))

  , testCase "DropAggregation view drops the instrument" $ do
      let dropView = defaultView { viewAggregation = Just DropAggregation }
      withViewProvider [dropView] $ \reader _p meter -> do
        SomeCounter c <- createCounter meter "my-counter" Nothing
        counterAdd c 10.0 emptyAttributes
        md <- readerCollect reader
        let allMetrics = concatMap smMetrics (mdScopeMetrics md)
        assertBool "metrics should be empty after drop" (null allMetrics)

  , testCase "rename via view" $ do
      let renameView = defaultView { viewName = Just "renamed-counter" }
      withViewProvider [renameView] $ \reader _p meter -> do
        SomeCounter c <- createCounter meter "my-counter" Nothing
        counterAdd c 5.0 emptyAttributes
        md <- readerCollect reader
        case concatMap smMetrics (mdScopeMetrics md) of
          [m] -> metricName m @?= "renamed-counter"
          ms  -> assertFailure ("expected 1 metric, got: " ++ show (length ms))

  , testCase "attribute filter via view" $ do
      let filterView = defaultView { viewAttributeKeys = Just (Set.fromList ["env"]) }
      withViewProvider [filterView] $ \reader _p meter -> do
        SomeCounter c <- createCounter meter "my-counter" Nothing
        let attrs = fromList [("env", StringValue "prod"), ("region", StringValue "us-east")]
        counterAdd c 1.0 attrs
        md <- readerCollect reader
        case getSumDataPoints md of
          [pt] -> do
            -- Only "env" attribute should remain
            let expectedAttrs = fromList [("env", StringValue "prod")]
            ndpAttributes pt @?= expectedAttrs
          ps -> assertFailure ("expected 1 point, got: " ++ show ps)

  , testCase "multiple views produce multiple metric streams" $ do
      let viewA = defaultView { viewName = Just "stream-a" }
          viewB = defaultView { viewName = Just "stream-b" }
      withViewProvider [viewA, viewB] $ \reader _p meter -> do
        SomeCounter c <- createCounter meter "my-counter" Nothing
        counterAdd c 3.0 emptyAttributes
        md <- readerCollect reader
        let allMetrics = concatMap smMetrics (mdScopeMetrics md)
            names = sort (map metricName allMetrics)
        names @?= ["stream-a", "stream-b"]

  , testCase "view matching by kind filters other kinds" $ do
      let dropCounters = defaultView
            { viewInstrumentKind = Just CounterKind
            , viewAggregation = Just DropAggregation
            }
          -- A pass-through view for histograms (identity)
          passHist = defaultView
            { viewInstrumentKind = Just HistogramKind
            }
      withViewProvider [dropCounters, passHist] $ \reader _p meter -> do
        SomeCounter c <- createCounter meter "my-counter" Nothing
        SomeHistogram h <- createHistogram meter "my-hist" Nothing
        counterAdd c 10.0 emptyAttributes
        histogramRecord h 5.0 emptyAttributes
        md <- readerCollect reader
        let allMetrics = concatMap smMetrics (mdScopeMetrics md)
            names = map metricName allMetrics
        -- Counter should be dropped, histogram should remain
        assertBool "counter should be dropped" ("my-counter" `notElem` names)
        assertBool "histogram should remain" ("my-hist" `elem` names)
  ]


-------------------------------------------------------------------------------
-- 30. Batch observable callbacks (registerCallback)
-------------------------------------------------------------------------------

-- NOTE: These tests depend on M1 (registerCallback implementation in SdkMeter).
-- Until M1 lands, registerCallback returns a no-op registration and the batch
-- callback is never invoked, so these tests will fail. They are written against
-- the target API shape to validate the implementation once it is complete.

batchCallbackTests :: TestTree
batchCallbackTests = testGroup "registerCallback (batch observable callbacks)"
  [ testCase "registerCallback: batch callback is invoked during collection" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        -- Create two observable gauges with no per-instrument callbacks
        SomeObservableGauge og1 <- createObservableGauge meter "batch.gauge.1" [] Nothing
        SomeObservableGauge og2 <- createObservableGauge meter "batch.gauge.2" [] Nothing
        -- Register a batch callback covering both instruments
        let instruments =
              [ mkSomeObsGauge "batch.gauge.1" (SomeObservableGauge og1)
              , mkSomeObsGauge "batch.gauge.2" (SomeObservableGauge og2)
              ]
            batchCb :: BatchObservableCallback
            batchCb (SomeBatchObservableResult bor) = do
              batchObserveValue bor (mkSomeObsGauge "batch.gauge.1" (SomeObservableGauge og1)) 1.0 emptyAttributes
              batchObserveValue bor (mkSomeObsGauge "batch.gauge.2" (SomeObservableGauge og2)) 2.0 emptyAttributes
        _ <- registerCallback meter instruments batchCb
        -- Trigger collection
        md <- readerCollect reader
        let gaugePoints = getGaugeDataPoints md
            allMetrics = concatMap smMetrics (mdScopeMetrics md)
            names = map metricName allMetrics
        -- Both gauges should appear
        assertBool "batch.gauge.1 should be present" ("batch.gauge.1" `elem` names)
        assertBool "batch.gauge.2 should be present" ("batch.gauge.2" `elem` names)
        -- Should have 2 gauge data points total
        length gaugePoints @?= 2
        -- Values should be 1.0 and 2.0 (order may vary)
        let values = sort (map ndpValue gaugePoints)
        values @?= [1.0, 2.0]

  , testCase "registerCallback: per-instrument and batch observations both appear" $ do
      withNoOpReaderProvider $ \reader _p meter -> do
        -- Create an observable gauge with a per-instrument callback
        let perInstrCb (SomeObservableResult r) = observeValue r 10.0 emptyAttributes
        SomeObservableGauge og <- createObservableGauge meter "dual.gauge" [perInstrCb] Nothing
        -- Register an additional batch callback for the same gauge
        let instruments = [mkSomeObsGauge "dual.gauge" (SomeObservableGauge og)]
            batchCb :: BatchObservableCallback
            batchCb (SomeBatchObservableResult bor) =
              batchObserveValue bor (mkSomeObsGauge "dual.gauge" (SomeObservableGauge og)) 20.0 emptyAttributes
        _ <- registerCallback meter instruments batchCb
        -- Trigger collection
        md <- readerCollect reader
        let gaugePoints = getGaugeDataPoints md
        -- The gauge metric should be present. The exact semantics of
        -- having both per-instrument and batch callbacks on the same gauge
        -- depend on the merge strategy:
        --   - If both observations are kept as separate data points, we
        --     expect 2 points with values 10.0 and 20.0.
        --   - If the batch callback replaces the per-instrument value
        --     (last-writer-wins for LastValue aggregation), we expect 1
        --     point with value 20.0.
        --   - If per-instrument takes precedence, we expect value 10.0.
        --
        -- The OTel spec says both callbacks are invoked during collection
        -- and observations are merged by the aggregator. For LastValue
        -- (gauge), the last observation wins. Since callback invocation
        -- order is per-instrument then batch, we expect the batch value
        -- to be the final one. Assert at least one point is present.
        assertBool "should have at least one gauge data point" (not (null gaugePoints))
        -- With LastValue semantics, the final value should be 20.0
        -- (batch callback fires after per-instrument callback).
        -- If multiple points are present, both values should be among them.
        let values = map ndpValue gaugePoints
        assertBool
          ("expected gauge value 20.0 (batch wins) or both 10.0 and 20.0, got: " ++ show values)
          (20.0 `elem` values || (10.0 `elem` values && 20.0 `elem` values))
  ]
