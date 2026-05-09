module Main (main) where

import Control.Monad (replicateM_)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nfIO)

import OTel.Attribute (InstrumentationScope (..), emptyAttributes)
import OTel.Context (attach, detach)
import OTel.Context qualified as Context
import OTel.Metric (Meter (..), MeterProvider (..))
import OTel.Metric.Instrument (Counter (..))
import OTel.SDK.Metric
  ( SdkMeterProviderConfig (..)
  , defaultSdkMeterProviderConfig
  , newSdkMeterProvider
  , SomeMetricReader (..)
  , SomeMetricExporter (..)
  , PeriodicExportingMetricReaderConfig (..)
  , defaultPeriodicExportingMetricReaderConfig
  , newPeriodicExportingMetricReader
  )
import OTel.SDK.Trace
  ( SdkTracerProviderConfig (..)
  , defaultSdkTracerProviderConfig
  , newSdkTracerProvider
  , forceFlush
  , shutdown
  )
import OTel.SDK.Trace.Export (SpanExporter (..), SomeSpanExporter (..))
import OTel.SDK.Trace.Processor
  ( SomeSpanProcessor (..)
  , newSimpleSpanProcessor
  , newBatchSpanProcessor
  , defaultBatchSpanProcessorConfig
  )
import OTel.Trace (Span (..), Tracer (..), TracerProvider (..), defaultSpanConfig)
import OTel.Context (root)
import OTel.SDK.Export (ExportResult (..))
import OTel.Timestamp (milliseconds)
import OTel.Exporter.InMemory (newInMemoryMetricExporter)


-------------------------------------------------------------------------------
-- No-op span exporter
-------------------------------------------------------------------------------

data NoOpSpanExporter = NoOpSpanExporter

instance SpanExporter NoOpSpanExporter where
  exportSpans _ _ = pure ExportSuccess
  shutdownExporter _ = pure (Right ())
  forceFlushExporter _ _ = pure (Right ())


-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

benchScope :: InstrumentationScope
benchScope = InstrumentationScope
  { scopeName = "otel-benchmarks"
  , scopeVersion = Nothing
  , scopeSchemaUrl = Nothing
  , scopeAttributes = Nothing
  }


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "span"
      [ bench "span creation: 1000 spans via SimpleSpanProcessor + no-op exporter" $
          nfIO benchSpanCreation
      , bench "batch processor: 1000 spans enqueued + forceFlush" $
          nfIO benchBatchProcessor
      ]
  , bgroup "metric"
      [ bench "metric recording: 1000 counter.add" $
          nfIO benchMetricRecording
      ]
  , bgroup "context"
      [ bench "context propagation: 1000 attach/detach cycles" $
          nfIO benchContextPropagation
      ]
  ]


-------------------------------------------------------------------------------
-- Benchmark 1: Span creation throughput
-------------------------------------------------------------------------------

benchSpanCreation :: IO ()
benchSpanCreation = do
  proc <- newSimpleSpanProcessor (SomeSpanExporter NoOpSpanExporter)
  provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerProcessors = [SomeSpanProcessor proc]
    }
  tracer <- getTracer provider benchScope
  replicateM_ 1000 $ do
    s <- startSpan tracer "bench-span" root defaultSpanConfig
    end s Nothing
  _ <- shutdown provider
  pure ()


-------------------------------------------------------------------------------
-- Benchmark 2: Metric recording throughput
-------------------------------------------------------------------------------

benchMetricRecording :: IO ()
benchMetricRecording = do
  exporter <- newInMemoryMetricExporter
  -- Use a very long interval so the periodic reader never fires during bench
  reader <- newPeriodicExportingMetricReader
    (SomeMetricExporter exporter)
    defaultPeriodicExportingMetricReaderConfig
      { pemrExportInterval = milliseconds 600000
      }
  provider <- newSdkMeterProvider defaultSdkMeterProviderConfig
    { providerReaders = [SomeMetricReader reader]
    }
  meter <- getMeter provider benchScope
  counter <- createCounter meter "bench.counter" Nothing
  replicateM_ 1000 $ counterAdd counter 1.0 emptyAttributes


-------------------------------------------------------------------------------
-- Benchmark 3: Context propagation overhead
-------------------------------------------------------------------------------

benchContextPropagation :: IO ()
benchContextPropagation = do
  let ctx = Context.root
  replicateM_ 1000 $ do
    token <- attach ctx
    detach token


-------------------------------------------------------------------------------
-- Benchmark 4: Batch processor throughput
-------------------------------------------------------------------------------

benchBatchProcessor :: IO ()
benchBatchProcessor = do
  proc <- newBatchSpanProcessor
    (SomeSpanExporter NoOpSpanExporter)
    defaultBatchSpanProcessorConfig
  provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerProcessors = [SomeSpanProcessor proc]
    }
  tracer <- getTracer provider benchScope
  replicateM_ 1000 $ do
    s <- startSpan tracer "batch-span" root defaultSpanConfig
    end s Nothing
  _ <- forceFlush provider Nothing
  _ <- shutdown provider
  pure ()
