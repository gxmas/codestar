{-# LANGUAGE ExistentialQuantification #-}
-- | Log record export types: ReadableLogRecord, ReadWriteLogRecord,
-- LogRecordExporter, and existential wrappers.
module OTel.SDK.Log.Export
  ( -- * ReadableLogRecord
    ReadableLogRecord (..)
  , SomeReadableLogRecord (..)

    -- * ReadWriteLogRecord
  , ReadWriteLogRecord (..)
  , SomeReadWriteLogRecord (..)

    -- * LogRecordExporter
  , LogRecordExporter (..)
  , SomeLogRecordExporter (..)
  , NoOpLogRecordExporter (..)
  ) where

import Data.Text (Text)

import OTel.Attribute (AttributeValue, Attributes, InstrumentationScope, Key)
import OTel.Log (LogBody, SeverityNumber)
import OTel.SDK.Export (ExportResult (..), FlushError, ShutdownError)
import OTel.SDK.Resource (Resource)
import OTel.Timestamp (Duration, Timestamp)
import OTel.Trace.SpanContext (SpanContext)


-------------------------------------------------------------------------------
-- ReadableLogRecord
-------------------------------------------------------------------------------

-- | A read-only view of a log record. All methods are pure — they read from
-- an already-materialized snapshot. Exporters receive 'SomeReadableLogRecord'.
class ReadableLogRecord r where
  rlrTimestamp         :: r -> Maybe Timestamp
  rlrObservedTimestamp :: r -> Timestamp
  rlrSeverityNumber    :: r -> Maybe SeverityNumber
  rlrSeverityText      :: r -> Maybe Text
  rlrBody              :: r -> Maybe LogBody
  rlrAttributes        :: r -> Attributes
  rlrDroppedAttributes :: r -> Int
  rlrSpanContext       :: r -> Maybe SpanContext
  rlrResource          :: r -> Resource
  rlrScope             :: r -> InstrumentationScope


-- | Existential wrapper for any 'ReadableLogRecord' implementation.
data SomeReadableLogRecord = forall r. ReadableLogRecord r => SomeReadableLogRecord r

instance ReadableLogRecord SomeReadableLogRecord where
  rlrTimestamp         (SomeReadableLogRecord r) = rlrTimestamp r
  rlrObservedTimestamp (SomeReadableLogRecord r) = rlrObservedTimestamp r
  rlrSeverityNumber    (SomeReadableLogRecord r) = rlrSeverityNumber r
  rlrSeverityText      (SomeReadableLogRecord r) = rlrSeverityText r
  rlrBody              (SomeReadableLogRecord r) = rlrBody r
  rlrAttributes        (SomeReadableLogRecord r) = rlrAttributes r
  rlrDroppedAttributes (SomeReadableLogRecord r) = rlrDroppedAttributes r
  rlrSpanContext       (SomeReadableLogRecord r) = rlrSpanContext r
  rlrResource          (SomeReadableLogRecord r) = rlrResource r
  rlrScope             (SomeReadableLogRecord r) = rlrScope r


-------------------------------------------------------------------------------
-- ReadWriteLogRecord
-------------------------------------------------------------------------------

-- | A mutable log record passed to processors. Extends 'ReadableLogRecord'
-- with IO-based setters that processors can use to enrich or modify a log
-- record before it is exported.
class ReadableLogRecord r => ReadWriteLogRecord r where
  rwlrSetTimestamp      :: r -> Timestamp      -> IO ()
  rwlrSetObservedTime   :: r -> Timestamp      -> IO ()
  rwlrSetSeverityNumber :: r -> SeverityNumber  -> IO ()
  rwlrSetSeverityText   :: r -> Text           -> IO ()
  rwlrSetBody           :: r -> LogBody        -> IO ()
  rwlrSetAttribute      :: r -> Key -> AttributeValue -> IO ()


-- | Existential wrapper for any 'ReadWriteLogRecord' implementation.
data SomeReadWriteLogRecord = forall r. ReadWriteLogRecord r => SomeReadWriteLogRecord r

instance ReadableLogRecord SomeReadWriteLogRecord where
  rlrTimestamp         (SomeReadWriteLogRecord r) = rlrTimestamp r
  rlrObservedTimestamp (SomeReadWriteLogRecord r) = rlrObservedTimestamp r
  rlrSeverityNumber    (SomeReadWriteLogRecord r) = rlrSeverityNumber r
  rlrSeverityText      (SomeReadWriteLogRecord r) = rlrSeverityText r
  rlrBody              (SomeReadWriteLogRecord r) = rlrBody r
  rlrAttributes        (SomeReadWriteLogRecord r) = rlrAttributes r
  rlrDroppedAttributes (SomeReadWriteLogRecord r) = rlrDroppedAttributes r
  rlrSpanContext       (SomeReadWriteLogRecord r) = rlrSpanContext r
  rlrResource          (SomeReadWriteLogRecord r) = rlrResource r
  rlrScope             (SomeReadWriteLogRecord r) = rlrScope r

instance ReadWriteLogRecord SomeReadWriteLogRecord where
  rwlrSetTimestamp      (SomeReadWriteLogRecord r) = rwlrSetTimestamp r
  rwlrSetObservedTime   (SomeReadWriteLogRecord r) = rwlrSetObservedTime r
  rwlrSetSeverityNumber (SomeReadWriteLogRecord r) = rwlrSetSeverityNumber r
  rwlrSetSeverityText   (SomeReadWriteLogRecord r) = rwlrSetSeverityText r
  rwlrSetBody           (SomeReadWriteLogRecord r) = rwlrSetBody r
  rwlrSetAttribute      (SomeReadWriteLogRecord r) = rwlrSetAttribute r


-------------------------------------------------------------------------------
-- LogRecordExporter
-------------------------------------------------------------------------------

-- | The interface for exporting completed log records to a backend (e.g.,
-- OTLP collector, stdout, file).
class LogRecordExporter e where
  -- | Export a batch of log records. Returns 'ExportSuccess' if the batch was
  -- accepted, 'ExportFailure' otherwise.
  exportLogRecords :: e -> [SomeReadableLogRecord] -> IO ExportResult

  -- | Shut down the exporter, releasing any held resources.
  shutdownLogExporter :: e -> IO (Either ShutdownError ())

  -- | Force-flush any buffered log records. The optional 'Duration' is a timeout.
  forceFlushLogExporter :: e -> Maybe Duration -> IO (Either FlushError ())


-- | Existential wrapper for any 'LogRecordExporter' implementation.
data SomeLogRecordExporter = forall e. LogRecordExporter e => SomeLogRecordExporter e

instance LogRecordExporter SomeLogRecordExporter where
  exportLogRecords      (SomeLogRecordExporter e) = exportLogRecords e
  shutdownLogExporter   (SomeLogRecordExporter e) = shutdownLogExporter e
  forceFlushLogExporter (SomeLogRecordExporter e) = forceFlushLogExporter e


-- | A no-op exporter that discards all log records. Useful as a default
-- or for testing.
data NoOpLogRecordExporter = NoOpLogRecordExporter

instance LogRecordExporter NoOpLogRecordExporter where
  exportLogRecords      _ _ = pure ExportSuccess
  shutdownLogExporter   _   = pure (Right ())
  forceFlushLogExporter _ _ = pure (Right ())
