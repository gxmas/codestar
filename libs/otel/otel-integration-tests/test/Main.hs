-- | Cross-signal integration tests for the OTel Haskell SDK.
--
-- These tests verify that traces, metrics, and logs work together through the
-- full pipeline: shared resources, trace-log correlation, concurrent operation,
-- and clean shutdown semantics.
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import OTel.Attribute
  ( AttributeValue (..), InstrumentationScope (..), emptyAttributes
  , lookup
  )
import OTel.Context (root)
import OTel.Log
  ( LogBody (..), LogRecord (..), Logger (..), LoggerProvider (..)
  , SomeLogger (..), defaultLogRecord, SeverityNumber (..)
  )
import OTel.Metric
  ( Counter (..), Meter (..), MeterProvider (..), SomeMeter (..)
  , SomeCounter (..)
  )
import OTel.SDK.Log
  ( SdkLoggerProviderConfig (..), defaultSdkLoggerProviderConfig
  , newSdkLoggerProvider, sdkLoggerProviderForceFlush
  , sdkLoggerProviderShutdown
  , SomeLogRecordProcessor (..), newSimpleLogRecordProcessor
  , ReadableLogRecord (..)
  )
import OTel.SDK.Log.Export (SomeLogRecordExporter (..))
import OTel.SDK.Metric
  ( SdkMeterProviderConfig (..), defaultSdkMeterProviderConfig
  , newSdkMeterProvider, sdkMeterProviderForceFlush
  , sdkMeterProviderShutdown
  , MetricData (..)
  , SomeMetricReader (..)
  )
import OTel.SDK.Metric.Reader
  ( newPeriodicExportingMetricReader
  , PeriodicExportingMetricReaderConfig (..)
  , defaultPeriodicExportingMetricReaderConfig
  )
import OTel.SDK.Metric.Export (SomeMetricExporter (..))
import OTel.SDK.Resource qualified as Resource
import OTel.SDK.Trace
  ( SdkTracerProviderConfig (..), defaultSdkTracerProviderConfig
  , newSdkTracerProvider, forceFlush, shutdown
  )
import OTel.SDK.Trace.Export (ReadableSpan (..))
import OTel.SDK.Trace.Processor (SomeSpanProcessor (..), newSimpleSpanProcessor)
import OTel.SDK.Trace.Export (SomeSpanExporter (..))
import OTel.Trace
  ( Span (..), Tracer (..), TracerProvider (..)
  , defaultSpanConfig, setSpanInContext
  )
import OTel.Trace.SpanContext qualified as SC
import OTel.Exporter.InMemory
  ( newInMemorySpanExporter, getFinishedSpans
  , newInMemoryMetricExporter, getFinishedMetrics
  , newInMemoryLogRecordExporter, getFinishedLogRecords
  )
import OTel.Timestamp (milliseconds)

import Data.Either (isRight)
import Prelude hiding (lookup)


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Cross-signal integration tests"
  [ allThreeSignalsTest
  , logTraceCorrelationTest
  , sharedResourceTest
  , shutdownSequenceTest
  ]


-------------------------------------------------------------------------------
-- Test 1: All three signals run simultaneously
-------------------------------------------------------------------------------

allThreeSignalsTest :: TestTree
allThreeSignalsTest = testCase
  "All three signals run simultaneously through the full pipeline" $ do

  -- Setup trace pipeline
  spanExporter <- newInMemorySpanExporter
  spanProc <- newSimpleSpanProcessor (SomeSpanExporter spanExporter)
  let resource = Resource.create [("service.name", StringValue "all-signals-svc")] Nothing
  tracerProvider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerResource   = resource
    , providerProcessors = [SomeSpanProcessor spanProc]
    }

  -- Setup metrics pipeline
  metricExporter <- newInMemoryMetricExporter
  metricReader <- newPeriodicExportingMetricReader
    (SomeMetricExporter metricExporter)
    defaultPeriodicExportingMetricReaderConfig
      { pemrExportInterval = milliseconds 60000 }
  meterProvider <- newSdkMeterProvider defaultSdkMeterProviderConfig
    { providerResource = resource
    , providerReaders  = [SomeMetricReader metricReader]
    }

  -- Setup logs pipeline
  logExporter <- newInMemoryLogRecordExporter
  logProc <- newSimpleLogRecordProcessor (SomeLogRecordExporter logExporter)
  logProvider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
    { llpResource   = resource
    , llpProcessors = [SomeLogRecordProcessor logProc]
    }

  -- Obtain instruments
  let scope = InstrumentationScope "test" (Just "1.0.0") Nothing Nothing
  tracer <- getTracer tracerProvider scope
  SomeMeter meter <- getMeter meterProvider scope
  SomeLogger logger <- getLogger logProvider scope

  -- Record one item per signal
  span1 <- startSpan tracer "integration-span" root defaultSpanConfig
  end span1 Nothing

  SomeCounter counter <- createCounter meter "test.counter" Nothing
  counterAdd counter 1.0 emptyAttributes

  emit logger defaultLogRecord
    { logBody = Just (LogBodyString "integration log")
    , logSeverityNumber = Just SeverityInfo
    }

  -- Force flush all signals
  _ <- forceFlush tracerProvider Nothing
  _ <- sdkMeterProviderForceFlush meterProvider Nothing
  _ <- sdkLoggerProviderForceFlush logProvider Nothing

  -- Assert each signal collected at least one item
  spans <- getFinishedSpans spanExporter
  assertBool "Expected at least one exported span" (not (null spans))

  metrics <- getFinishedMetrics metricExporter
  assertBool "Expected at least one exported metric batch" (not (null metrics))

  logRecords <- getFinishedLogRecords logExporter
  assertBool "Expected at least one exported log record" (not (null logRecords))

  -- Cleanup
  _ <- shutdown tracerProvider
  _ <- sdkMeterProviderShutdown meterProvider
  _ <- sdkLoggerProviderShutdown logProvider
  pure ()


-------------------------------------------------------------------------------
-- Test 2: Log record carries active span's traceId and spanId
-------------------------------------------------------------------------------

logTraceCorrelationTest :: TestTree
logTraceCorrelationTest = testCase
  "Log record carries active span's traceId and spanId" $ do

  -- Setup trace
  spanExporter <- newInMemorySpanExporter
  spanProc <- newSimpleSpanProcessor (SomeSpanExporter spanExporter)
  tracerProvider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerProcessors = [SomeSpanProcessor spanProc]
    }

  -- Setup logs
  logExporter <- newInMemoryLogRecordExporter
  logProc <- newSimpleLogRecordProcessor (SomeLogRecordExporter logExporter)
  logProvider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
    { llpProcessors = [SomeLogRecordProcessor logProc]
    }

  let scope = InstrumentationScope "correlation-test" Nothing Nothing Nothing
  tracer <- getTracer tracerProvider scope
  SomeLogger logger <- getLogger logProvider scope

  -- Start a span
  span1 <- startSpan tracer "correlated-span" root defaultSpanConfig
  activeSpanContext <- getSpanContext span1
  let spanCtx = setSpanInContext span1 root

  -- Emit a log record with the span's context
  emit logger defaultLogRecord
    { logBody    = Just (LogBodyString "correlated log")
    , logContext = Just spanCtx
    }

  -- End span and flush
  end span1 Nothing
  _ <- forceFlush tracerProvider Nothing
  _ <- sdkLoggerProviderForceFlush logProvider Nothing

  -- Assert correlation
  logRecords <- getFinishedLogRecords logExporter
  assertBool "Expected at least one log record" (not (null logRecords))

  case logRecords of
    [] -> assertFailure "No log records exported"
    (logRec : _) -> case rlrSpanContext logRec of
      Nothing -> assertFailure "Log record should have a SpanContext (got Nothing)"
      Just sc -> do
        assertEqual "traceId must match active span"
          (SC.traceId activeSpanContext)
          (SC.traceId sc)
        assertEqual "spanId must match active span"
          (SC.spanId activeSpanContext)
          (SC.spanId sc)

  -- Cleanup
  _ <- shutdown tracerProvider
  _ <- sdkLoggerProviderShutdown logProvider
  pure ()


-------------------------------------------------------------------------------
-- Test 3: All providers share the same Resource
-------------------------------------------------------------------------------

sharedResourceTest :: TestTree
sharedResourceTest = testCase
  "All providers share the same Resource; exported data reflects it" $ do

  let resource = Resource.create [("service.name", StringValue "shared-svc")] Nothing

  -- Trace
  spanExporter <- newInMemorySpanExporter
  spanProc <- newSimpleSpanProcessor (SomeSpanExporter spanExporter)
  tracerProvider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerResource   = resource
    , providerProcessors = [SomeSpanProcessor spanProc]
    }

  -- Metrics
  metricExporter <- newInMemoryMetricExporter
  metricReader <- newPeriodicExportingMetricReader
    (SomeMetricExporter metricExporter)
    defaultPeriodicExportingMetricReaderConfig
      { pemrExportInterval = milliseconds 60000 }
  meterProvider <- newSdkMeterProvider defaultSdkMeterProviderConfig
    { providerResource = resource
    , providerReaders  = [SomeMetricReader metricReader]
    }

  -- Logs
  logExporter <- newInMemoryLogRecordExporter
  logProc <- newSimpleLogRecordProcessor (SomeLogRecordExporter logExporter)
  logProvider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
    { llpResource   = resource
    , llpProcessors = [SomeLogRecordProcessor logProc]
    }

  let scope = InstrumentationScope "resource-test" Nothing Nothing Nothing

  -- Record one item per signal
  tracer <- getTracer tracerProvider scope
  span1 <- startSpan tracer "resource-span" root defaultSpanConfig
  end span1 Nothing

  SomeMeter meter <- getMeter meterProvider scope
  SomeCounter counter <- createCounter meter "res.counter" Nothing
  counterAdd counter 1.0 emptyAttributes

  SomeLogger logger <- getLogger logProvider scope
  emit logger defaultLogRecord
    { logBody = Just (LogBodyString "resource log") }

  -- Flush
  _ <- forceFlush tracerProvider Nothing
  _ <- sdkMeterProviderForceFlush meterProvider Nothing
  _ <- sdkLoggerProviderForceFlush logProvider Nothing

  -- Assert span resource
  spans <- getFinishedSpans spanExporter
  case spans of
    [] -> assertFailure "Expected at least one span"
    (spanItem : _) -> assertEqual "Span resource must have service.name"
      (Just (StringValue "shared-svc"))
      (lookup "service.name" (Resource.getAttributes (readResource spanItem)))

  -- Assert metric resource
  metrics <- getFinishedMetrics metricExporter
  case metrics of
    [] -> assertFailure "Expected at least one metric batch"
    (m : _) -> assertEqual "MetricData resource must have service.name"
      (Just (StringValue "shared-svc"))
      (lookup "service.name" (Resource.getAttributes (mdResource m)))

  -- Assert log resource
  logRecords <- getFinishedLogRecords logExporter
  case logRecords of
    [] -> assertFailure "Expected at least one log record"
    (logItem : _) -> assertEqual "Log record resource must have service.name"
      (Just (StringValue "shared-svc"))
      (lookup "service.name" (Resource.getAttributes (rlrResource logItem)))

  -- Cleanup
  _ <- shutdown tracerProvider
  _ <- sdkMeterProviderShutdown meterProvider
  _ <- sdkLoggerProviderShutdown logProvider
  pure ()


-------------------------------------------------------------------------------
-- Test 4: Providers shut down cleanly; shutdown is idempotent
-------------------------------------------------------------------------------

shutdownSequenceTest :: TestTree
shutdownSequenceTest = testCase
  "Providers shut down cleanly; data flushed; shutdown is idempotent" $ do

  -- Setup all three signals
  spanExporter <- newInMemorySpanExporter
  spanProc <- newSimpleSpanProcessor (SomeSpanExporter spanExporter)
  tracerProvider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerProcessors = [SomeSpanProcessor spanProc]
    }

  metricExporter <- newInMemoryMetricExporter
  metricReader <- newPeriodicExportingMetricReader
    (SomeMetricExporter metricExporter)
    defaultPeriodicExportingMetricReaderConfig
      { pemrExportInterval = milliseconds 60000 }
  meterProvider <- newSdkMeterProvider defaultSdkMeterProviderConfig
    { providerReaders = [SomeMetricReader metricReader]
    }

  logExporter <- newInMemoryLogRecordExporter
  logProc <- newSimpleLogRecordProcessor (SomeLogRecordExporter logExporter)
  logProvider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
    { llpProcessors = [SomeLogRecordProcessor logProc]
    }

  -- Record data
  let scope = InstrumentationScope "shutdown-test" Nothing Nothing Nothing
  tracer <- getTracer tracerProvider scope
  span1 <- startSpan tracer "shutdown-span" root defaultSpanConfig
  end span1 Nothing

  SomeLogger logger <- getLogger logProvider scope
  emit logger defaultLogRecord
    { logBody = Just (LogBodyString "shutdown log") }

  -- Force flush before shutdown to ensure data is captured
  _ <- forceFlush tracerProvider Nothing
  _ <- sdkLoggerProviderForceFlush logProvider Nothing

  -- Verify data was collected before shutdown
  spans <- getFinishedSpans spanExporter
  assertEqual "Expected exactly 1 span before shutdown" 1 (length spans)

  logRecords <- getFinishedLogRecords logExporter
  assertEqual "Expected exactly 1 log record before shutdown" 1 (length logRecords)

  -- First shutdown: all should succeed
  traceShutdown1 <- shutdown tracerProvider
  assertBool "First trace shutdown should succeed" (isRight traceShutdown1)

  metricShutdown1 <- sdkMeterProviderShutdown meterProvider
  assertBool "First metric shutdown should succeed" (isRight metricShutdown1)

  logShutdown1 <- sdkLoggerProviderShutdown logProvider
  assertBool "First log shutdown should succeed" (isRight logShutdown1)

  -- Second shutdown: idempotent (SDK returns Right () on repeated shutdown)
  traceShutdown2 <- shutdown tracerProvider
  assertBool "Second trace shutdown should be idempotent" (isRight traceShutdown2)

  metricShutdown2 <- sdkMeterProviderShutdown meterProvider
  assertBool "Second metric shutdown should be idempotent" (isRight metricShutdown2)

  logShutdown2 <- sdkLoggerProviderShutdown logProvider
  assertBool "Second log shutdown should be idempotent" (isRight logShutdown2)
