-- | Span export types: ReadableSpan, ReadWriteSpan, SpanExporter, and
-- supporting data types (SpanEvent, Link).
module OTel.SDK.Trace.Export
  ( -- * SpanExporter
    SpanExporter (..)
  , SomeSpanExporter (..)

    -- * ReadableSpan
  , ReadableSpan (..)
  , SomeReadableSpan (..)

    -- * ReadWriteSpan
  , ReadWriteSpan
  , SomeReadWriteSpan (..)

    -- * Span data types
  , SpanEvent (..)
  , Link (..)
  ) where

import Data.Text (Text)

import OTel.Attribute (Attributes, InstrumentationScope)
import OTel.SDK.Export (ExportResult, FlushError, ShutdownError)
import OTel.SDK.Resource (Resource)
import OTel.Timestamp (Duration, Timestamp)
import OTel.Trace (Span (..), SpanKind, SpanStatus)
import OTel.Trace.SpanContext (SpanContext)


-------------------------------------------------------------------------------
-- Span data types
-------------------------------------------------------------------------------

-- | A timestamped event recorded on a span, carrying a name and attributes.
data SpanEvent = SpanEvent
  { eventName :: !Text
  , eventTimestamp :: !Timestamp
  , eventAttributes :: !Attributes
  , eventDroppedAttributesCount :: !Int
  } deriving stock (Eq, Show)


-- | A link to another span context, carrying attributes.
data Link = Link
  { linkSpanContext :: !SpanContext
  , linkAttributes :: !Attributes
  , linkDroppedAttributesCount :: !Int
  } deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- ReadableSpan
-------------------------------------------------------------------------------

-- | A read-only view of a completed (or snapshotted) span. All methods are
-- pure — they read from an already-materialized snapshot.
class ReadableSpan s where
  readSpanContext :: s -> SpanContext
  readParentSpanContext :: s -> Maybe SpanContext
  readName :: s -> Text
  readKind :: s -> SpanKind
  readStartTimestamp :: s -> Timestamp
  readEndTimestamp :: s -> Timestamp
  readAttributes :: s -> Attributes
  readEvents :: s -> [SpanEvent]
  readLinks :: s -> [Link]
  readStatus :: s -> SpanStatus
  readResource :: s -> Resource
  readInstrumentationScope :: s -> InstrumentationScope
  readDroppedAttributesCount :: s -> Int
  readDroppedEventsCount :: s -> Int
  readDroppedLinksCount :: s -> Int


-- | Existential wrapper for any 'ReadableSpan' implementation.
data SomeReadableSpan = forall s. ReadableSpan s => SomeReadableSpan s

instance ReadableSpan SomeReadableSpan where
  readSpanContext (SomeReadableSpan s) = readSpanContext s
  readParentSpanContext (SomeReadableSpan s) = readParentSpanContext s
  readName (SomeReadableSpan s) = readName s
  readKind (SomeReadableSpan s) = readKind s
  readStartTimestamp (SomeReadableSpan s) = readStartTimestamp s
  readEndTimestamp (SomeReadableSpan s) = readEndTimestamp s
  readAttributes (SomeReadableSpan s) = readAttributes s
  readEvents (SomeReadableSpan s) = readEvents s
  readLinks (SomeReadableSpan s) = readLinks s
  readStatus (SomeReadableSpan s) = readStatus s
  readResource (SomeReadableSpan s) = readResource s
  readInstrumentationScope (SomeReadableSpan s) = readInstrumentationScope s
  readDroppedAttributesCount (SomeReadableSpan s) = readDroppedAttributesCount s
  readDroppedEventsCount (SomeReadableSpan s) = readDroppedEventsCount s
  readDroppedLinksCount (SomeReadableSpan s) = readDroppedLinksCount s


-------------------------------------------------------------------------------
-- ReadWriteSpan
-------------------------------------------------------------------------------

-- | A span that supports both mutable operations ('Span') and pure read
-- access ('ReadableSpan'). Available during 'SpanProcessor.onStart'.
class (Span s, ReadableSpan s) => ReadWriteSpan s


-- | Existential wrapper for any 'ReadWriteSpan' implementation.
data SomeReadWriteSpan = forall s. ReadWriteSpan s => SomeReadWriteSpan s

instance ReadableSpan SomeReadWriteSpan where
  readSpanContext (SomeReadWriteSpan s) = readSpanContext s
  readParentSpanContext (SomeReadWriteSpan s) = readParentSpanContext s
  readName (SomeReadWriteSpan s) = readName s
  readKind (SomeReadWriteSpan s) = readKind s
  readStartTimestamp (SomeReadWriteSpan s) = readStartTimestamp s
  readEndTimestamp (SomeReadWriteSpan s) = readEndTimestamp s
  readAttributes (SomeReadWriteSpan s) = readAttributes s
  readEvents (SomeReadWriteSpan s) = readEvents s
  readLinks (SomeReadWriteSpan s) = readLinks s
  readStatus (SomeReadWriteSpan s) = readStatus s
  readResource (SomeReadWriteSpan s) = readResource s
  readInstrumentationScope (SomeReadWriteSpan s) = readInstrumentationScope s
  readDroppedAttributesCount (SomeReadWriteSpan s) = readDroppedAttributesCount s
  readDroppedEventsCount (SomeReadWriteSpan s) = readDroppedEventsCount s
  readDroppedLinksCount (SomeReadWriteSpan s) = readDroppedLinksCount s

instance Span SomeReadWriteSpan where
  getSpanContext (SomeReadWriteSpan s) = getSpanContext s
  isRecording (SomeReadWriteSpan s) = isRecording s
  setAttribute (SomeReadWriteSpan s) = setAttribute s
  addEvent (SomeReadWriteSpan s) = addEvent s
  addLink (SomeReadWriteSpan s) = addLink s
  setStatus (SomeReadWriteSpan s) = setStatus s
  recordException (SomeReadWriteSpan s) = recordException s
  updateName (SomeReadWriteSpan s) = updateName s
  end (SomeReadWriteSpan s) = end s

instance ReadWriteSpan SomeReadWriteSpan


-------------------------------------------------------------------------------
-- SpanExporter
-------------------------------------------------------------------------------

-- | The interface for exporting completed spans to a backend (e.g., OTLP
-- collector, Jaeger, Zipkin, stdout).
class SpanExporter e where
  -- | Export a batch of completed spans. Returns 'ExportSuccess' if the
  -- batch was accepted, 'ExportFailure' otherwise.
  --
  -- Implementations MUST NOT block indefinitely. If retries are needed,
  -- they should be bounded internally.
  exportSpans :: e -> [SomeReadableSpan] -> IO ExportResult

  -- | Shut down the exporter, releasing any held resources.
  shutdownExporter :: e -> IO (Either ShutdownError ())

  -- | Force-flush any buffered spans. The optional 'Duration' is a timeout.
  forceFlushExporter :: e -> Maybe Duration -> IO (Either FlushError ())


-- | Existential wrapper for any 'SpanExporter' implementation.
data SomeSpanExporter = forall e. SpanExporter e => SomeSpanExporter e

instance SpanExporter SomeSpanExporter where
  exportSpans (SomeSpanExporter e) = exportSpans e
  shutdownExporter (SomeSpanExporter e) = shutdownExporter e
  forceFlushExporter (SomeSpanExporter e) = forceFlushExporter e
