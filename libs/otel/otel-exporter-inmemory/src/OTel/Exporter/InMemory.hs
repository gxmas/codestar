-- | In-memory exporters that store completed spans, metrics, and log records
-- in 'TVar's for test assertions. Thread-safe. Intended for use in test
-- suites only.
module OTel.Exporter.InMemory
  ( -- * Span exporter
    InMemorySpanExporter
  , newInMemorySpanExporter
  , getFinishedSpans
  , reset

    -- * Metric exporter
  , InMemoryMetricExporter
  , newInMemoryMetricExporter
  , getFinishedMetrics
  , resetMetrics

    -- * Log record exporter
  , InMemoryLogRecordExporter
  , newInMemoryLogRecordExporter
  , getFinishedLogRecords
  , resetLogRecords
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)

import OTel.SDK.Export (ExportResult (..))
import OTel.SDK.Log.Export (LogRecordExporter (..), SomeReadableLogRecord)
import OTel.SDK.Metric.Export
  ( AggregationTemporality (..)
  , MetricData
  , MetricExporter (..)
  )
import OTel.SDK.Metric.Reader (defaultAggregationFor)
import OTel.SDK.Trace.Export (SomeReadableSpan, SpanExporter (..))


-------------------------------------------------------------------------------
-- Span exporter
-------------------------------------------------------------------------------

-- | A span exporter that accumulates spans in memory. Retrieve them with
-- 'getFinishedSpans' and clear with 'reset'.
data InMemorySpanExporter = InMemorySpanExporter !(TVar [SomeReadableSpan])


-- | Create a new empty 'InMemorySpanExporter'.
newInMemorySpanExporter :: IO InMemorySpanExporter
newInMemorySpanExporter = InMemorySpanExporter <$> newTVarIO []


instance SpanExporter InMemorySpanExporter where
  exportSpans (InMemorySpanExporter ref) spans = do
    atomically $ modifyTVar' ref (<> spans)
    pure ExportSuccess
  shutdownExporter _ = pure (Right ())
  forceFlushExporter _ _ = pure (Right ())


-- | Retrieve all exported spans. Returns a snapshot; thread-safe.
getFinishedSpans :: InMemorySpanExporter -> IO [SomeReadableSpan]
getFinishedSpans (InMemorySpanExporter ref) = readTVarIO ref


-- | Clear all stored spans.
reset :: InMemorySpanExporter -> IO ()
reset (InMemorySpanExporter ref) = atomically $ writeTVar ref []


-------------------------------------------------------------------------------
-- Metric exporter
-------------------------------------------------------------------------------

-- | A metric exporter that accumulates 'MetricData' in memory. Retrieve
-- with 'getFinishedMetrics' and clear with 'resetMetrics'.
newtype InMemoryMetricExporter = InMemoryMetricExporter (TVar [MetricData])


-- | Create a new empty 'InMemoryMetricExporter'.
newInMemoryMetricExporter :: IO InMemoryMetricExporter
newInMemoryMetricExporter = InMemoryMetricExporter <$> newTVarIO []


instance MetricExporter InMemoryMetricExporter where
  exportMetrics (InMemoryMetricExporter ref) md = do
    atomically (modifyTVar' ref (md :))
    pure ExportSuccess
  shutdownMetricExporter _ = pure (Right ())
  forceFlushMetricExporter _ _ = pure (Right ())
  exporterTemporality _ _ = Cumulative
  exporterDefaultAggregation _ kind = defaultAggregationFor kind


-- | Retrieve all exported metric data. Returns a snapshot; thread-safe.
getFinishedMetrics :: InMemoryMetricExporter -> IO [MetricData]
getFinishedMetrics (InMemoryMetricExporter ref) = readTVarIO ref


-- | Clear all stored metric data.
resetMetrics :: InMemoryMetricExporter -> IO ()
resetMetrics (InMemoryMetricExporter ref) = atomically (writeTVar ref [])


-------------------------------------------------------------------------------
-- Log record exporter
-------------------------------------------------------------------------------

-- | A log record exporter that accumulates 'SomeReadableLogRecord' in memory.
-- Retrieve with 'getFinishedLogRecords' and clear with 'resetLogRecords'.
newtype InMemoryLogRecordExporter = InMemoryLogRecordExporter (TVar [SomeReadableLogRecord])


-- | Create a new empty 'InMemoryLogRecordExporter'.
newInMemoryLogRecordExporter :: IO InMemoryLogRecordExporter
newInMemoryLogRecordExporter = InMemoryLogRecordExporter <$> newTVarIO []


instance LogRecordExporter InMemoryLogRecordExporter where
  exportLogRecords (InMemoryLogRecordExporter ref) records = do
    atomically (modifyTVar' ref (<> records))
    pure ExportSuccess
  shutdownLogExporter _ = pure (Right ())
  forceFlushLogExporter _ _ = pure (Right ())


-- | Retrieve all exported log records. Returns a snapshot; thread-safe.
getFinishedLogRecords :: InMemoryLogRecordExporter -> IO [SomeReadableLogRecord]
getFinishedLogRecords (InMemoryLogRecordExporter ref) = readTVarIO ref


-- | Clear all stored log records.
resetLogRecords :: InMemoryLogRecordExporter -> IO ()
resetLogRecords (InMemoryLogRecordExporter ref) = atomically (writeTVar ref [])
