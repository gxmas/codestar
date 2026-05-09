module Main where

import Control.Concurrent.Async (mapConcurrently_, replicateConcurrently_)

import Test.Tasty
import Test.Tasty.HUnit

import OTel.Attribute
  ( AttributeValue (..), InstrumentationScope (..), emptyAttributes
  , fromList, lookup
  )
import OTel.Context (root)
import OTel.Exporter.InMemory
import OTel.Log (LogBody (..), SeverityNumber (..))
import OTel.SDK.Export (ExportResult (..))
import OTel.SDK.Log.Export
  ( LogRecordExporter (..), ReadableLogRecord (..), SomeReadableLogRecord (..)
  )
import OTel.SDK.Metric.Export
  ( AggregationTemporality (..), InstrumentKind (..)
  , Metric (..), MetricData (..), MetricExporter (..)
  , MetricPointData (..), ScopeMetrics (..), SumData (..)
  )
import OTel.SDK.Resource qualified as Resource
import OTel.SDK.Trace
  ( SdkTracerProviderConfig (..), defaultSdkTracerProviderConfig
  , newSdkTracerProvider
  )
import OTel.SDK.Trace.Export
  ( ReadableSpan (..), SomeReadableSpan (..), SomeSpanExporter (..)
  , SpanExporter (..)
  )
import OTel.SDK.Trace.Processor
  ( SomeSpanProcessor (..), newSimpleSpanProcessor )
import OTel.Timestamp (fromNanos)
import OTel.Trace
  ( NoOpTracerProvider (..), Span (..)
  , SpanConfig (..), SpanKind (..), SpanStatus (..), StatusCode (..)
  , Tracer (..), TracerProvider (..), defaultSpanConfig, setSpanInContext
  )
import OTel.Trace.SpanContext
  ( SpanContext (..), TraceId, invalidSpanContext, isSampled, isValid )
import Prelude hiding (lookup)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "otel-exporter-inmemory"
  [ inMemoryExporterTests
  , integrationTests
  , inMemoryMetricExporterTests
  , inMemoryLogRecordExporterTests
  ]


-------------------------------------------------------------------------------
-- Minimal test span
-------------------------------------------------------------------------------

data TestSpan = TestSpan

instance ReadableSpan TestSpan where
  readSpanContext _ = invalidSpanContext
  readParentSpanContext _ = Nothing
  readName _ = "test-span"
  readKind _ = Internal
  readStartTimestamp _ = fromNanos 0
  readEndTimestamp _ = fromNanos 1000000000
  readAttributes _ = emptyAttributes
  readEvents _ = []
  readLinks _ = []
  readStatus _ = SpanStatus Unset Nothing
  readResource _ = Resource.empty
  readInstrumentationScope _ =
    InstrumentationScope "test" Nothing Nothing Nothing
  readDroppedAttributesCount _ = 0
  readDroppedEventsCount _ = 0
  readDroppedLinksCount _ = 0

testSpan :: SomeReadableSpan
testSpan = SomeReadableSpan TestSpan


-------------------------------------------------------------------------------
-- Metric test helpers
-------------------------------------------------------------------------------

emptyMetricData :: MetricData
emptyMetricData = MetricData Resource.empty []

oneMetricData :: MetricData
oneMetricData = MetricData Resource.empty
  [ ScopeMetrics (InstrumentationScope "test" Nothing Nothing Nothing)
      [ Metric "requests" "" "1" (SumPointData (SumData [] Cumulative True)) ]
  ]


-------------------------------------------------------------------------------
-- Log record test helper
-------------------------------------------------------------------------------

data TestLogRecord = TestLogRecord

instance ReadableLogRecord TestLogRecord where
  rlrTimestamp _ = Nothing
  rlrObservedTimestamp _ = fromNanos 0
  rlrSeverityNumber _ = Just SeverityInfo
  rlrSeverityText _ = Just "INFO"
  rlrBody _ = Just (LogBodyString "test message")
  rlrAttributes _ = emptyAttributes
  rlrDroppedAttributes _ = 0
  rlrSpanContext _ = Nothing
  rlrResource _ = Resource.empty
  rlrScope _ = InstrumentationScope "test" Nothing Nothing Nothing

testLogRecord :: SomeReadableLogRecord
testLogRecord = SomeReadableLogRecord TestLogRecord


-------------------------------------------------------------------------------
-- InMemorySpanExporter tests
-------------------------------------------------------------------------------

inMemoryExporterTests :: TestTree
inMemoryExporterTests = testGroup "InMemorySpanExporter"
  [ testCase "fresh exporter has no spans" $ do
      e <- newInMemorySpanExporter
      spans <- getFinishedSpans e
      length spans @?= 0

  , testCase "exportSpans returns ExportSuccess" $ do
      e <- newInMemorySpanExporter
      result <- exportSpans e [testSpan]
      result @?= ExportSuccess

  , testCase "after one export, getFinishedSpans returns 1 span" $ do
      e <- newInMemorySpanExporter
      _ <- exportSpans e [testSpan]
      spans <- getFinishedSpans e
      length spans @?= 1

  , testCase "two exports accumulate to 2 spans" $ do
      e <- newInMemorySpanExporter
      _ <- exportSpans e [testSpan]
      _ <- exportSpans e [testSpan]
      spans <- getFinishedSpans e
      length spans @?= 2

  , testCase "export of batch of 3 stores 3 spans" $ do
      e <- newInMemorySpanExporter
      _ <- exportSpans e [testSpan, testSpan, testSpan]
      spans <- getFinishedSpans e
      length spans @?= 3

  , testCase "reset clears all spans" $ do
      e <- newInMemorySpanExporter
      _ <- exportSpans e [testSpan, testSpan]
      reset e
      spans <- getFinishedSpans e
      length spans @?= 0

  , testCase "after reset, further exports work normally" $ do
      e <- newInMemorySpanExporter
      _ <- exportSpans e [testSpan]
      reset e
      _ <- exportSpans e [testSpan, testSpan]
      spans <- getFinishedSpans e
      length spans @?= 2

  , testCase "shutdownExporter returns Right ()" $ do
      e <- newInMemorySpanExporter
      result <- shutdownExporter e
      case result of
        Right () -> pure ()
        Left err -> assertFailure ("expected Right (), got Left: " <> show err)

  , testCase "forceFlushExporter returns Right ()" $ do
      e <- newInMemorySpanExporter
      result <- forceFlushExporter e Nothing
      case result of
        Right () -> pure ()
        Left err -> assertFailure ("expected Right (), got Left: " <> show err)

  , testCase "concurrent exports are thread-safe -- all 20 spans present" $ do
      e <- newInMemorySpanExporter
      mapConcurrently_ (\_ -> exportSpans e [testSpan] >>= \r -> r @?= ExportSuccess) ([1..20] :: [Int])
      spans <- getFinishedSpans e
      length spans @?= 20
  ]


-------------------------------------------------------------------------------
-- Integration tests: full pipeline API -> SDK -> Processor -> Exporter
-------------------------------------------------------------------------------

testScope :: InstrumentationScope
testScope = InstrumentationScope "integration-test" (Just "1.0.0") Nothing Nothing

integrationTests :: TestTree
integrationTests = testGroup "End-to-end pipeline"
  [ testCase "basic pipeline: span is captured by InMemorySpanExporter" $ do
      exporter <- newInMemorySpanExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter exporter)
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor proc] }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "test-operation" root
        defaultSpanConfig { spanKind = Server }
      setAttribute span_ "http.method" (StringValue "GET")
      addEvent span_ "processing" emptyAttributes Nothing
      setStatus span_ Ok Nothing
      end span_ Nothing
      spans <- getFinishedSpans exporter
      length spans @?= 1

  , testCase "exported span has correct name" $ do
      exporter <- newInMemorySpanExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter exporter)
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor proc] }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "my-operation" root defaultSpanConfig
      end span_ Nothing
      [SomeReadableSpan s] <- getFinishedSpans exporter
      readName s @?= "my-operation"

  , testCase "exported span has valid SpanContext with sampled flag set" $ do
      exporter <- newInMemorySpanExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter exporter)
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor proc] }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "sampled-span" root defaultSpanConfig
      end span_ Nothing
      [SomeReadableSpan s] <- getFinishedSpans exporter
      let sc = readSpanContext s
      assertBool "SpanContext should be valid" (isValid sc)
      assertBool "sampled flag should be set (AlwaysOnSampler)" (isSampled (traceFlags sc))

  , testCase "exported span has correct kind and status" $ do
      exporter <- newInMemorySpanExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter exporter)
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor proc] }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "srv-span" root
        defaultSpanConfig { spanKind = Server }
      setStatus span_ Ok Nothing
      end span_ Nothing
      [SomeReadableSpan s] <- getFinishedSpans exporter
      readKind s @?= Server
      readStatus s @?= SpanStatus Ok Nothing

  , testCase "exported span has set attributes" $ do
      exporter <- newInMemorySpanExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter exporter)
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor proc] }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "attr-span" root defaultSpanConfig
      setAttribute span_ "http.method" (StringValue "POST")
      setAttribute span_ "http.status_code" (Int64Value 201)
      end span_ Nothing
      [SomeReadableSpan s] <- getFinishedSpans exporter
      lookup "http.method" (readAttributes s) @?= Just (StringValue "POST")
      lookup "http.status_code" (readAttributes s) @?= Just (Int64Value 201)

  , testCase "exported span has recorded events" $ do
      exporter <- newInMemorySpanExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter exporter)
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor proc] }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "event-span" root defaultSpanConfig
      addEvent span_ "step-1" emptyAttributes Nothing
      addEvent span_ "step-2" (fromList [("key", StringValue "val")]) Nothing
      end span_ Nothing
      [SomeReadableSpan s] <- getFinishedSpans exporter
      length (readEvents s) @?= 2

  , testCase "resource is attached to exported span" $ do
      exporter <- newInMemorySpanExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter exporter)
      let res = Resource.create [("service.name", StringValue "my-service")] Nothing
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor proc]
        , providerResource = res
        }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "resource-span" root defaultSpanConfig
      end span_ Nothing
      [SomeReadableSpan s] <- getFinishedSpans exporter
      let resAttrs = Resource.getAttributes (readResource s)
      lookup "service.name" resAttrs @?= Just (StringValue "my-service")

  , testCase "parent-child: child inherits traceId from parent" $ do
      exporter <- newInMemorySpanExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter exporter)
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor proc] }
      tracer <- getTracer provider testScope
      parentSpan <- startSpan tracer "parent" root defaultSpanConfig
      parentSc <- getSpanContext parentSpan
      let childCtx = setSpanInContext parentSpan root
      childSpan <- startSpan tracer "child" childCtx defaultSpanConfig
      childSc <- getSpanContext childSpan
      end childSpan Nothing
      end parentSpan Nothing
      spans <- getFinishedSpans exporter
      length spans @?= 2
      (traceId childSc :: TraceId) @?= traceId parentSc

  , testCase "parent-child: child has parent SpanContext" $ do
      exporter <- newInMemorySpanExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter exporter)
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor proc] }
      tracer <- getTracer provider testScope
      parentSpan <- startSpan tracer "parent" root defaultSpanConfig
      parentSc <- getSpanContext parentSpan
      let childCtx = setSpanInContext parentSpan root
      childSpan <- startSpan tracer "child" childCtx defaultSpanConfig
      end childSpan Nothing
      end parentSpan Nothing
      -- child is exported first (SimpleSpanProcessor exports synchronously on end)
      (SomeReadableSpan child : _) <- getFinishedSpans exporter
      readParentSpanContext child @?= Just parentSc

  , testCase "no-op fallback: NoOpTracerProvider produces non-recording spans" $ do
      let noop = NoOpTracerProvider
      tracer <- getTracer noop testScope
      span_ <- startSpan tracer "noop-op" root defaultSpanConfig
      recording <- isRecording span_
      assertBool "no-op span should not be recording" (not recording)
      sc <- getSpanContext span_
      sc @?= invalidSpanContext
      -- All ops are silent -- no crash
      setAttribute span_ "key" (StringValue "val")
      addEvent span_ "evt" emptyAttributes Nothing
      setStatus span_ Error Nothing
      end span_ Nothing

  , testCase "double end is idempotent -- span exported only once" $ do
      exporter <- newInMemorySpanExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter exporter)
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor proc] }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "double-end" root defaultSpanConfig
      end span_ Nothing
      end span_ Nothing  -- second end is no-op
      spans <- getFinishedSpans exporter
      length spans @?= 1
  ]


-------------------------------------------------------------------------------
-- InMemoryMetricExporter tests
-------------------------------------------------------------------------------

inMemoryMetricExporterTests :: TestTree
inMemoryMetricExporterTests = testGroup "InMemoryMetricExporter"
  [ testCase "newInMemoryMetricExporter starts empty" $ do
      e <- newInMemoryMetricExporter
      ms <- getFinishedMetrics e
      ms @?= []

  , testCase "exportMetrics accumulates: export once -> 1 MetricData" $ do
      e <- newInMemoryMetricExporter
      _ <- exportMetrics e oneMetricData
      ms <- getFinishedMetrics e
      length ms @?= 1

  , testCase "exportMetrics accumulates: export twice -> 2 MetricData" $ do
      e <- newInMemoryMetricExporter
      _ <- exportMetrics e oneMetricData
      _ <- exportMetrics e emptyMetricData
      ms <- getFinishedMetrics e
      length ms @?= 2

  , testCase "resetMetrics clears all stored MetricData" $ do
      e <- newInMemoryMetricExporter
      _ <- exportMetrics e oneMetricData
      _ <- exportMetrics e oneMetricData
      resetMetrics e
      ms <- getFinishedMetrics e
      ms @?= []

  , testCase "exportMetrics always returns ExportSuccess" $ do
      e <- newInMemoryMetricExporter
      r1 <- exportMetrics e emptyMetricData
      r1 @?= ExportSuccess
      r2 <- exportMetrics e oneMetricData
      r2 @?= ExportSuccess

  , testCase "shutdownMetricExporter returns Right ()" $ do
      e <- newInMemoryMetricExporter
      result <- shutdownMetricExporter e
      case result of
        Right () -> pure ()
        Left err -> assertFailure ("expected Right (), got Left: " <> show err)

  , testCase "forceFlushMetricExporter returns Right ()" $ do
      e <- newInMemoryMetricExporter
      result <- forceFlushMetricExporter e Nothing
      case result of
        Right () -> pure ()
        Left err -> assertFailure ("expected Right (), got Left: " <> show err)

  , testCase "exporterTemporality returns Cumulative for all instrument kinds" $ do
      e <- newInMemoryMetricExporter
      let allKinds = [minBound .. maxBound] :: [InstrumentKind]
      mapM_ (\k -> exporterTemporality e k @?= Cumulative) allKinds

  , testCase "thread-safety: 10 concurrent exportMetrics -> 10 MetricData" $ do
      e <- newInMemoryMetricExporter
      replicateConcurrently_ 10 $ do
        r <- exportMetrics e oneMetricData
        r @?= ExportSuccess
      ms <- getFinishedMetrics e
      length ms @?= 10
  ]


-------------------------------------------------------------------------------
-- InMemoryLogRecordExporter tests
-------------------------------------------------------------------------------

inMemoryLogRecordExporterTests :: TestTree
inMemoryLogRecordExporterTests = testGroup "InMemoryLogRecordExporter"
  [ testCase "starts empty" $ do
      e <- newInMemoryLogRecordExporter
      rs <- getFinishedLogRecords e
      length rs @?= 0

  , testCase "exportLogRecords with 1 record -> list has 1 record" $ do
      e <- newInMemoryLogRecordExporter
      _ <- exportLogRecords e [testLogRecord]
      rs <- getFinishedLogRecords e
      length rs @?= 1

  , testCase "exportLogRecords with 2 records -> accumulated, not replaced" $ do
      e <- newInMemoryLogRecordExporter
      _ <- exportLogRecords e [testLogRecord]
      _ <- exportLogRecords e [testLogRecord, testLogRecord]
      rs <- getFinishedLogRecords e
      length rs @?= 3

  , testCase "resetLogRecords clears all stored records" $ do
      e <- newInMemoryLogRecordExporter
      _ <- exportLogRecords e [testLogRecord, testLogRecord]
      resetLogRecords e
      rs <- getFinishedLogRecords e
      length rs @?= 0

  , testCase "exportLogRecords returns ExportSuccess" $ do
      e <- newInMemoryLogRecordExporter
      result <- exportLogRecords e [testLogRecord]
      result @?= ExportSuccess

  , testCase "shutdownLogExporter returns Right ()" $ do
      e <- newInMemoryLogRecordExporter
      result <- shutdownLogExporter e
      case result of
        Right () -> pure ()
        Left err -> assertFailure ("expected Right (), got Left: " <> show err)

  , testCase "forceFlushLogExporter returns Right ()" $ do
      e <- newInMemoryLogRecordExporter
      result <- forceFlushLogExporter e Nothing
      case result of
        Right () -> pure ()
        Left err -> assertFailure ("expected Right (), got Left: " <> show err)

  , testCase "thread-safety: 5 threads each exporting 3 records -> 15 total" $ do
      e <- newInMemoryLogRecordExporter
      replicateConcurrently_ 5 $ do
        r <- exportLogRecords e [testLogRecord, testLogRecord, testLogRecord]
        r @?= ExportSuccess
      rs <- getFinishedLogRecords e
      length rs @?= 15
  ]
