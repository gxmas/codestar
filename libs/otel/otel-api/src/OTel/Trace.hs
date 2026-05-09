-- | Core tracing API: Span, Tracer, and TracerProvider type classes with
-- existential wrappers, no-op implementations, and global registration.
module OTel.Trace
  ( -- * Span kinds and status
    SpanKind (..)
  , StatusCode (..)
  , SpanStatus (..)

    -- * Span
  , Span (..)
  , SomeSpan (..)
  , NoOpSpan (..)
  , NonRecordingSpan (..)

    -- * Span configuration
  , SpanConfig (..)
  , defaultSpanConfig

    -- * Tracer
  , Tracer (..)
  , SomeTracer (..)
  , NoOpTracer (..)

    -- * TracerProvider
  , TracerProvider (..)
  , SomeTracerProvider (..)
  , NoOpTracerProvider (..)

    -- * Global registration
  , setGlobalTracerProvider
  , getGlobalTracerProvider

    -- * Context integration
  , getSpanFromContext
  , setSpanInContext
  , createNonRecordingSpan
  ) where

import Control.Exception (SomeException)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import OTel.Attribute (Attribute, AttributeValue, Attributes, InstrumentationScope, Key)
import OTel.Context (Context, getValue, setValue)
import OTel.Context.Key (ContextKey, newContextKey)
import OTel.Timestamp (Timestamp)
import OTel.Trace.SpanContext (SpanContext, invalidSpanContext)


-------------------------------------------------------------------------------
-- Enums
-------------------------------------------------------------------------------

-- | The type of span. Determines how the span relates to its parent and
-- children in a distributed trace.
data SpanKind = Internal | Server | Client | Producer | Consumer
  deriving stock (Eq, Show, Enum, Bounded)


-- | The status of a span. 'Unset' is the default and is appropriate for
-- successful operations where the caller has no need to explicitly assert
-- success. 'Error' indicates the operation failed. 'Ok' should only be set
-- when the caller wants to definitively mark a span as successful, because
-- 'Ok' is terminal: once set, it takes precedence over 'Error' or 'Unset'
-- and cannot be overridden.
data StatusCode = Unset | Ok | Error
  deriving stock (Eq, Show, Enum, Bounded)


-- | A status code paired with an optional human-readable description.
-- The description is only meaningful when 'statusCode' is 'Error'.
data SpanStatus = SpanStatus
  { statusCode :: !StatusCode
  , statusDescription :: !(Maybe Text)
  } deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- Span type class
-------------------------------------------------------------------------------

-- | The interface for a mutable span. All operations are in 'IO' because
-- SDK implementations mutate internal state.
class Span s where
  -- | Retrieve the immutable 'SpanContext' for this span.
  getSpanContext :: s -> IO SpanContext

  -- | Whether this span is recording events, attributes, and status.
  isRecording :: s -> IO Bool

  -- | Set a single attribute on this span.
  setAttribute :: s -> Key -> AttributeValue -> IO ()

  -- | Record a timestamped event with the given name and attributes.
  addEvent :: s -> Text -> Attributes -> Maybe Timestamp -> IO ()

  -- | Add a link to another span context with associated attributes.
  addLink :: s -> SpanContext -> Attributes -> IO ()

  -- | Set the span status. Per the spec, 'Ok' is terminal and cannot be overridden.
  setStatus :: s -> StatusCode -> Maybe Text -> IO ()

  -- | Record an exception as an event with structured attributes.
  recordException :: s -> SomeException -> Attributes -> IO ()

  -- | Update the span name. Use sparingly — prefer setting the name at creation.
  updateName :: s -> Text -> IO ()

  -- | Signal that the span's operation has ended. After this call, the span
  -- should not be modified.
  end :: s -> Maybe Timestamp -> IO ()


-------------------------------------------------------------------------------
-- Existential Span wrapper
-------------------------------------------------------------------------------

-- | An existential wrapper allowing heterogeneous collections of spans
-- and context storage without exposing the concrete span type.
data SomeSpan = forall s. Span s => SomeSpan s

instance Span SomeSpan where
  getSpanContext (SomeSpan s) = getSpanContext s
  isRecording (SomeSpan s) = isRecording s
  setAttribute (SomeSpan s) = setAttribute s
  addEvent (SomeSpan s) = addEvent s
  addLink (SomeSpan s) = addLink s
  setStatus (SomeSpan s) = setStatus s
  recordException (SomeSpan s) = recordException s
  updateName (SomeSpan s) = updateName s
  end (SomeSpan s) = end s


-------------------------------------------------------------------------------
-- NoOpSpan
-------------------------------------------------------------------------------

-- | A span that does nothing. Used when tracing is disabled or no SDK is
-- configured.
data NoOpSpan = NoOpSpan

instance Span NoOpSpan where
  getSpanContext _ = pure invalidSpanContext
  isRecording _ = pure False
  setAttribute _ _ _ = pure ()
  addEvent _ _ _ _ = pure ()
  addLink _ _ _ = pure ()
  setStatus _ _ _ = pure ()
  recordException _ _ _ = pure ()
  updateName _ _ = pure ()
  end _ _ = pure ()


-------------------------------------------------------------------------------
-- NonRecordingSpan
-------------------------------------------------------------------------------

-- | A span that carries a 'SpanContext' but does not record any data.
-- Useful for propagating context without collecting telemetry.
data NonRecordingSpan = NonRecordingSpan SpanContext

instance Span NonRecordingSpan where
  getSpanContext (NonRecordingSpan sc) = pure sc
  isRecording _ = pure False
  setAttribute _ _ _ = pure ()
  addEvent _ _ _ _ = pure ()
  addLink _ _ _ = pure ()
  setStatus _ _ _ = pure ()
  recordException _ _ _ = pure ()
  updateName _ _ = pure ()
  end _ _ = pure ()


-------------------------------------------------------------------------------
-- SpanConfig
-------------------------------------------------------------------------------

-- | Configuration for starting a new span. Use 'defaultSpanConfig' and
-- override fields with record update syntax.
data SpanConfig = SpanConfig
  { spanKind :: !SpanKind
  , spanAttributes :: ![Attribute]
  , spanLinks :: ![(SpanContext, Attributes)]
  , spanStartTimestamp :: !(Maybe Timestamp)
  , spanParent :: !(Maybe Context)
  -- ^ Explicit parent context. 'Nothing' means use current\/default.
  , spanNoParent :: !Bool
  -- ^ When 'True', create a root span with no parent.
  }


-- | Sensible defaults: 'Internal' kind, no attributes, no links, automatic
-- timestamp, inherit parent from caller context.
defaultSpanConfig :: SpanConfig
defaultSpanConfig = SpanConfig
  { spanKind = Internal
  , spanAttributes = []
  , spanLinks = []
  , spanStartTimestamp = Nothing
  , spanParent = Nothing
  , spanNoParent = False
  }


-------------------------------------------------------------------------------
-- Tracer type class
-------------------------------------------------------------------------------

-- | A named tracer that creates spans. Obtained from a 'TracerProvider'.
class Tracer t where
  -- | Start a new span with the given name, parent context, and configuration.
  startSpan :: t -> Text -> Context -> SpanConfig -> IO SomeSpan


-- | Existential wrapper for any 'Tracer' implementation.
data SomeTracer = forall t. Tracer t => SomeTracer t

instance Tracer SomeTracer where
  startSpan (SomeTracer t) = startSpan t


-- | A tracer that always produces 'NoOpSpan'.
data NoOpTracer = NoOpTracer

instance Tracer NoOpTracer where
  startSpan _ _ _ _ = pure (SomeSpan NoOpSpan)


-------------------------------------------------------------------------------
-- TracerProvider type class
-------------------------------------------------------------------------------

-- | A factory for 'Tracer' instances, scoped by instrumentation library.
class TracerProvider p where
  -- | Obtain a tracer for the given instrumentation scope.
  getTracer :: p -> InstrumentationScope -> IO SomeTracer


-- | Existential wrapper for any 'TracerProvider' implementation.
data SomeTracerProvider = forall p. TracerProvider p => SomeTracerProvider p

instance TracerProvider SomeTracerProvider where
  getTracer (SomeTracerProvider p) = getTracer p


-- | A provider that always returns 'NoOpTracer'.
data NoOpTracerProvider = NoOpTracerProvider

instance TracerProvider NoOpTracerProvider where
  getTracer _ _ = pure (SomeTracer NoOpTracer)


-------------------------------------------------------------------------------
-- Global registration
-------------------------------------------------------------------------------

globalTracerProviderRef :: IORef SomeTracerProvider
globalTracerProviderRef = unsafePerformIO (newIORef (SomeTracerProvider NoOpTracerProvider))
{-# NOINLINE globalTracerProviderRef #-}


-- | Set the global 'TracerProvider'. Typically called once at application
-- startup after configuring the SDK.
setGlobalTracerProvider :: SomeTracerProvider -> IO ()
setGlobalTracerProvider = writeIORef globalTracerProviderRef


-- | Retrieve the current global 'TracerProvider'. Returns a no-op provider
-- if none has been set.
getGlobalTracerProvider :: IO SomeTracerProvider
getGlobalTracerProvider = readIORef globalTracerProviderRef


-------------------------------------------------------------------------------
-- Context integration
-------------------------------------------------------------------------------

-- | The context key used to store the current span in a 'Context'.
spanContextKey :: ContextKey SomeSpan
spanContextKey = unsafePerformIO (newContextKey "otel-current-span")
{-# NOINLINE spanContextKey #-}


-- | Extract the current span from a 'Context', if any.
getSpanFromContext :: Context -> Maybe SomeSpan
getSpanFromContext ctx = getValue spanContextKey ctx


-- | Store a span in a 'Context', returning a new 'Context'.
setSpanInContext :: SomeSpan -> Context -> Context
setSpanInContext s ctx = setValue spanContextKey s ctx


-- | Create a non-recording span that carries the given 'SpanContext'.
-- Useful for wrapping remotely-propagated context without recording.
createNonRecordingSpan :: SpanContext -> SomeSpan
createNonRecordingSpan sc = SomeSpan (NonRecordingSpan sc)
