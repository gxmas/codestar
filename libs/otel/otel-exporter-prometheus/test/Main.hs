module Main where

import Control.Exception (SomeException, finally, try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Word (Word64)
import Network.HTTP.Client

import OTel.Attribute (AttributeValue (..), Attributes, InstrumentationScope (..), emptyAttributes, fromList)
import OTel.Exporter.Prometheus
import OTel.SDK.Metric.Export
import OTel.SDK.Metric.Reader (MetricReader (..))
import OTel.SDK.Resource qualified as Resource
import OTel.Timestamp (fromNanos)
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "otel-exporter-prometheus"
  [ temporalityTests
  , defaultAggregationTests
  , textFormatTests
  , withoutUnitsTests
  , resourceToTelemetryTests
  , shutdownTest
  , notFoundTest
  ]


-------------------------------------------------------------------------------
-- Test helper
-------------------------------------------------------------------------------

-- | Spin up a Prometheus reader on a given port, wire up the collect source,
-- run the action with a fetch function, then shut down.
withPrometheusServer :: Int -> IO MetricData -> (IO ByteString -> IO a) -> IO a
withPrometheusServer port collect action = do
  let cfg = defaultPrometheusConfig { prometheusPort = port }
  reader <- newPrometheusMetricReader cfg
  readerSetCollectSource reader collect
  mgr <- newManager defaultManagerSettings
  let fetch = do
        req <- parseUrlThrow ("http://127.0.0.1:" <> show port <> "/metrics")
        resp <- httpLbs req mgr
        pure (LBS.toStrict (responseBody resp))
  action fetch `finally` readerShutdown reader


-- | Variant that allows custom config (for unit/resource flags).
withPrometheusServerCfg :: PrometheusConfig -> IO MetricData -> (IO ByteString -> IO a) -> IO a
withPrometheusServerCfg cfg collect action = do
  reader <- newPrometheusMetricReader cfg
  readerSetCollectSource reader collect
  mgr <- newManager defaultManagerSettings
  let fetch = do
        req <- parseUrlThrow ("http://127.0.0.1:" <> show (prometheusPort cfg) <> "/metrics")
        resp <- httpLbs req mgr
        pure (LBS.toStrict (responseBody resp))
  action fetch `finally` readerShutdown reader


-------------------------------------------------------------------------------
-- Test data builders
-------------------------------------------------------------------------------

mkNdp :: Double -> NumberDataPoint
mkNdp v = NumberDataPoint
  { ndpAttributes = emptyAttributes
  , ndpStartTime  = fromNanos 0
  , ndpTime       = fromNanos 1_000_000_000
  , ndpValue      = v
  , ndpExemplars  = []
  }


mkNdpWithAttrs :: Double -> Attributes -> NumberDataPoint
mkNdpWithAttrs v attrs = NumberDataPoint
  { ndpAttributes = attrs
  , ndpStartTime  = fromNanos 0
  , ndpTime       = fromNanos 1_000_000_000
  , ndpValue      = v
  , ndpExemplars  = []
  }


mkCounterMD :: Text -> Text -> Double -> MetricData
mkCounterMD name unit value = MetricData
  { mdResource     = Resource.empty
  , mdScopeMetrics =
      [ ScopeMetrics
          { smScope   = InstrumentationScope "test" Nothing Nothing Nothing
          , smMetrics =
              [ Metric
                  { metricName        = name
                  , metricDescription = ""
                  , metricUnit        = unit
                  , metricPointData   = SumPointData SumData
                      { sumDataPoints  = [mkNdp value]
                      , sumTemporality = Cumulative
                      , sumIsMonotonic = True
                      }
                  }
              ]
          }
      ]
  }


mkGaugeMD :: Text -> Double -> MetricData
mkGaugeMD name value = MetricData
  { mdResource     = Resource.empty
  , mdScopeMetrics =
      [ ScopeMetrics
          { smScope   = InstrumentationScope "test" Nothing Nothing Nothing
          , smMetrics =
              [ Metric
                  { metricName        = name
                  , metricDescription = ""
                  , metricUnit        = ""
                  , metricPointData   = GaugePointData GaugeData
                      { gaugeDataPoints = [mkNdp value]
                      }
                  }
              ]
          }
      ]
  }


mkHistogramMD :: Text -> [Double] -> [Word64] -> Word64 -> Maybe Double -> MetricData
mkHistogramMD name bounds buckets count mSum = MetricData
  { mdResource     = Resource.empty
  , mdScopeMetrics =
      [ ScopeMetrics
          { smScope   = InstrumentationScope "test" Nothing Nothing Nothing
          , smMetrics =
              [ Metric
                  { metricName        = name
                  , metricDescription = ""
                  , metricUnit        = ""
                  , metricPointData   = HistogramPointData HistogramData
                      { histDataPoints =
                          [ HistogramDataPoint
                              { hdpAttributes     = emptyAttributes
                              , hdpStartTime      = fromNanos 0
                              , hdpTime           = fromNanos 1_000_000_000
                              , hdpCount          = count
                              , hdpSum            = mSum
                              , hdpBucketCounts   = buckets
                              , hdpExplicitBounds = bounds
                              , hdpMin            = Nothing
                              , hdpMax            = Nothing
                              , hdpExemplars      = []
                              }
                          ]
                      , histTemporality = Cumulative
                      }
                  }
              ]
          }
      ]
  }


mkCounterMDWithAttrs :: Text -> Double -> Attributes -> MetricData
mkCounterMDWithAttrs name value attrs = MetricData
  { mdResource     = Resource.empty
  , mdScopeMetrics =
      [ ScopeMetrics
          { smScope   = InstrumentationScope "test" Nothing Nothing Nothing
          , smMetrics =
              [ Metric
                  { metricName        = name
                  , metricDescription = ""
                  , metricUnit        = ""
                  , metricPointData   = SumPointData SumData
                      { sumDataPoints  = [mkNdpWithAttrs value attrs]
                      , sumTemporality = Cumulative
                      , sumIsMonotonic = True
                      }
                  }
              ]
          }
      ]
  }


mkCounterMDWithResource :: Text -> Text -> Double -> Resource.Resource -> MetricData
mkCounterMDWithResource name unit value res = MetricData
  { mdResource     = res
  , mdScopeMetrics =
      [ ScopeMetrics
          { smScope   = InstrumentationScope "test" Nothing Nothing Nothing
          , smMetrics =
              [ Metric
                  { metricName        = name
                  , metricDescription = ""
                  , metricUnit        = unit
                  , metricPointData   = SumPointData SumData
                      { sumDataPoints  = [mkNdp value]
                      , sumTemporality = Cumulative
                      , sumIsMonotonic = True
                      }
                  }
              ]
          }
      ]
  }


-------------------------------------------------------------------------------
-- 1. Temporality tests (no network)
--
-- Prometheus is a pull-based system; it always expects cumulative temporality
-- regardless of instrument kind. This is a genuine invariant: every
-- InstrumentKind value must map to Cumulative.
-------------------------------------------------------------------------------

temporalityTests :: TestTree
temporalityTests = testGroup "temporality"
  [ testCase "CounterKind -> Cumulative" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerTemporality reader CounterKind @?= Cumulative
      _ <- readerShutdown reader
      pure ()
  , testCase "UpDownCounterKind -> Cumulative" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerTemporality reader UpDownCounterKind @?= Cumulative
      _ <- readerShutdown reader
      pure ()
  , testCase "HistogramKind -> Cumulative" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerTemporality reader HistogramKind @?= Cumulative
      _ <- readerShutdown reader
      pure ()
  , testCase "GaugeKind -> Cumulative" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerTemporality reader GaugeKind @?= Cumulative
      _ <- readerShutdown reader
      pure ()
  , testCase "ObservableCounterKind -> Cumulative" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerTemporality reader ObservableCounterKind @?= Cumulative
      _ <- readerShutdown reader
      pure ()
  , testCase "ObservableUpDownCounterKind -> Cumulative" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerTemporality reader ObservableUpDownCounterKind @?= Cumulative
      _ <- readerShutdown reader
      pure ()
  , testCase "ObservableGaugeKind -> Cumulative" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerTemporality reader ObservableGaugeKind @?= Cumulative
      _ <- readerShutdown reader
      pure ()
  ]


-------------------------------------------------------------------------------
-- 2. Default aggregation tests (no network)
--
-- The spec mandates specific default aggregations per instrument kind.
-- These are algebraic identities of the mapping function.
-------------------------------------------------------------------------------

defaultAggregationTests :: TestTree
defaultAggregationTests = testGroup "default aggregation"
  [ testCase "CounterKind -> SumAggregation" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerDefaultAggregation reader CounterKind @?= SumAggregation
      _ <- readerShutdown reader
      pure ()
  , testCase "UpDownCounterKind -> SumAggregation" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerDefaultAggregation reader UpDownCounterKind @?= SumAggregation
      _ <- readerShutdown reader
      pure ()
  , testCase "HistogramKind -> ExplicitBucketHistogramAggregation" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      case readerDefaultAggregation reader HistogramKind of
        ExplicitBucketHistogramAggregation bounds ->
          assertBool "bounds should be non-empty" (not (null bounds))
        other ->
          assertFailure ("Expected ExplicitBucketHistogramAggregation, got: " <> show other)
      _ <- readerShutdown reader
      pure ()
  , testCase "GaugeKind -> LastValueAggregation" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerDefaultAggregation reader GaugeKind @?= LastValueAggregation
      _ <- readerShutdown reader
      pure ()
  , testCase "ObservableCounterKind -> SumAggregation" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerDefaultAggregation reader ObservableCounterKind @?= SumAggregation
      _ <- readerShutdown reader
      pure ()
  , testCase "ObservableUpDownCounterKind -> SumAggregation" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerDefaultAggregation reader ObservableUpDownCounterKind @?= SumAggregation
      _ <- readerShutdown reader
      pure ()
  , testCase "ObservableGaugeKind -> LastValueAggregation" $ do
      reader <- newPrometheusMetricReader defaultPrometheusConfig { prometheusPort = 0 }
      readerDefaultAggregation reader ObservableGaugeKind @?= LastValueAggregation
      _ <- readerShutdown reader
      pure ()
  ]


-------------------------------------------------------------------------------
-- 3. Text format tests (HTTP)
--
-- These verify the Prometheus exposition format invariants:
-- - Counters must have a _total suffix (spec requirement)
-- - Gauges must NOT have a _total suffix
-- - Histograms must render _bucket, _count, _sum lines
-- - Attribute labels must appear in the label set
-------------------------------------------------------------------------------

textFormatTests :: TestTree
textFormatTests = testGroup "text format"
  [ testCase "counter gets _total suffix" $
      withPrometheusServer 19464 (pure (mkCounterMD "http_requests" "" 42.0)) $ \fetch -> do
        body <- fetch
        assertContains body "# TYPE http_requests_total counter"
        assertContains body "http_requests_total"
        assertContains body "42"

  , testCase "gauge has no _total suffix" $
      withPrometheusServer 19465 (pure (mkGaugeMD "active_connections" 7.0)) $ \fetch -> do
        body <- fetch
        assertContains body "# TYPE active_connections gauge"
        assertContains body "active_connections"
        assertNotContains body "active_connections_total"

  , testCase "histogram renders _bucket/_count/_sum" $
      withPrometheusServer 19466 (pure (mkHistogramMD "request_duration" [0.1, 0.5] [2, 3, 5] 10 (Just 1.5))) $ \fetch -> do
        body <- fetch
        assertContains body "_bucket{le=\"0.1\""
        assertContains body "_bucket{le=\"+Inf\""
        assertContains body "_count"
        assertContains body "_sum"
        assertContains body "# TYPE request_duration histogram"

  , testCase "attribute labels appear" $
      withPrometheusServer 19467 (pure (mkCounterMDWithAttrs "http_requests" 1.0 (fromList [("method", StringValue "GET")]))) $ \fetch -> do
        body <- fetch
        assertContains body "method=\"GET\""
  ]


-------------------------------------------------------------------------------
-- 4. Without-units tests (HTTP)
--
-- The Prometheus exporter can optionally strip unit suffixes from metric
-- names. This tests the prometheusWithoutUnits config flag.
-------------------------------------------------------------------------------

withoutUnitsTests :: TestTree
withoutUnitsTests = testGroup "without units"
  [ testCase "unit suffix included by default" $
      withPrometheusServer 19468 (pure (mkCounterMD "http_requests" "ms" 1.0)) $ \fetch -> do
        body <- fetch
        -- The metric name should incorporate the unit when prometheusWithoutUnits=False
        assertContains body "ms"

  , testCase "prometheusWithoutUnits=True omits unit from metric name" $ do
      let cfg = defaultPrometheusConfig
            { prometheusPort = 19469
            , prometheusWithoutUnits = True
            }
      withPrometheusServerCfg cfg (pure (mkCounterMD "http_requests" "ms" 1.0)) $ \fetch -> do
        body <- fetch
        -- Metric value lines (non-comment) should not contain the unit suffix
        let valueLines = filter (\l -> not (B8.isPrefixOf "#" l) && not (B8.null l)) (B8.lines body)
        assertBool "no metric value line should contain '_ms'" $
          not (any (B8.isInfixOf "_ms") valueLines)
  ]


-------------------------------------------------------------------------------
-- 5. Resource-to-telemetry tests (HTTP)
--
-- Controls whether OTel resource attributes are promoted to Prometheus labels.
-- Default is False (resource attrs stay out of labels).
-------------------------------------------------------------------------------

resourceToTelemetryTests :: TestTree
resourceToTelemetryTests = testGroup "resource to telemetry"
  [ testCase "resourceToTelemetry=False (default) omits resource attrs from labels" $ do
      let res = Resource.create [("service.name", StringValue "my-svc")] Nothing
      withPrometheusServer 19470 (pure (mkCounterMDWithResource "http_requests" "" 1.0 res)) $ \fetch -> do
        body <- fetch
        assertNotContains body "service_name"

  , testCase "resourceToTelemetry=True includes resource attrs in labels" $ do
      let cfg = defaultPrometheusConfig
            { prometheusPort = 19471
            , prometheusResourceToTelemetry = True
            }
          res = Resource.create [("service.name", StringValue "my-svc")] Nothing
          md = mkCounterMDWithResource "http_requests" "" 1.0 res
      withPrometheusServerCfg cfg (pure md) $ \fetch -> do
        body <- fetch
        assertContains body "service_name=\"my-svc\""
  ]


-------------------------------------------------------------------------------
-- 6. Shutdown test (HTTP)
--
-- After readerShutdown, the Warp server should stop. Any subsequent HTTP
-- request should fail with a connection error.
-------------------------------------------------------------------------------

shutdownTest :: TestTree
shutdownTest = testGroup "shutdown"
  [ testCase "connection refused after shutdown" $ do
      let cfg = defaultPrometheusConfig { prometheusPort = 19473 }
      reader <- newPrometheusMetricReader cfg
      readerSetCollectSource reader (pure (mkGaugeMD "x" 1.0))
      _ <- readerShutdown reader
      mgr <- newManager defaultManagerSettings
      result <- try @SomeException $ do
        req <- parseUrlThrow "http://127.0.0.1:19473/metrics"
        _ <- httpLbs req mgr
        pure ()
      case result of
        Left _  -> pure ()  -- Expected: connection refused
        Right _ -> assertFailure "Expected connection to be refused after shutdown"
  ]


-------------------------------------------------------------------------------
-- 7. Not-found test (HTTP)
--
-- Requests to paths other than the configured metrics path should not
-- return Prometheus exposition data.
-------------------------------------------------------------------------------

notFoundTest :: TestTree
notFoundTest = testGroup "not found"
  [ testCase "GET /wrong_path does not return metrics" $
      withPrometheusServer 19472 (pure (mkGaugeMD "x" 1.0)) $ \_ -> do
        mgr <- newManager defaultManagerSettings
        -- parseUrlThrow throws on non-2xx, so we use parseRequest instead
        req <- parseRequest "http://127.0.0.1:19472/wrong_path"
        resp <- httpLbs req mgr
        let body = LBS.toStrict (responseBody resp)
        -- Should NOT contain Prometheus metrics
        assertNotContains body "# TYPE"
  ]


-------------------------------------------------------------------------------
-- Assertion helpers
-------------------------------------------------------------------------------

assertContains :: ByteString -> ByteString -> Assertion
assertContains haystack needle =
  assertBool
    ("Expected response to contain " <> show needle <> "\n  but got:\n" <> B8.unpack haystack)
    (needle `B8.isInfixOf` haystack)


assertNotContains :: ByteString -> ByteString -> Assertion
assertNotContains haystack needle =
  assertBool
    ("Expected response NOT to contain " <> show needle <> "\n  but got:\n" <> B8.unpack haystack)
    (not (needle `B8.isInfixOf` haystack))
