-- | Telemetry abstraction covering tracing, logging, and metrics.
--
-- All types are defined by this library — no @hs-opentelemetry@ types
-- leak through the public surface. Two backends are available:
--
-- * 'NoOpConfig' — all operations succeed silently (default before init)
-- * 'OpenTelemetryConfig' — routes to @hs-opentelemetry-api@
--
-- Backend selection happens once at application startup via 'initTelemetry'.
-- Libraries never select their own backend.
module Telemetry.Core
  ( -- * Initialization
    TelemetryConfig (..)
  , OtelSettings (..)
  , TelemetryHandle (..)
  , initTelemetry
  , shutdownTelemetry

    -- * Tracing — types
  , Span
  , SpanContext (..)
  , SpanName (..)
  , TraceId (..)
  , SpanId (..)
  , AttributeValue (..)

    -- * Tracing — operations
  , withSpan
  , startSpan
  , endSpan
  , addAttribute
  , recordException
  , getSpanContext
  , getCurrentSpanContext

    -- * Logging
  , Severity (..)
  , LogMessage (..)
  , log

    -- * Metrics
  , CounterName (..)
  , GaugeName (..)
  , HistogramName (..)
  , incrementCounter
  , addCounter
  , recordGauge
  , recordHistogram
  , exportMetrics
  ) where

import Prelude hiding (log)

import Control.Exception (SomeException)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

import Telemetry.Core.Types
import Telemetry.Core.Internal.Backend (Backend (..))
import Telemetry.Core.Internal.Global (getGlobalBackend, setGlobalBackend)
import Telemetry.Core.Internal.NoOp (noOpBackend)
import Telemetry.Core.Internal.OpenTelemetry (createOTelBackend)

-- ---------------------------------------------------------------------------
-- Initialization
-- ---------------------------------------------------------------------------

initTelemetry :: TelemetryConfig -> IO TelemetryHandle
initTelemetry NoOpConfig = do
  setGlobalBackend noOpBackend
  pure TelemetryHandle { shutdownAction = pure (), metricsPort = Nothing }
initTelemetry (OpenTelemetryConfig settings) = do
  (backend, boundPort, shutdown) <- createOTelBackend settings
  setGlobalBackend backend
  pure TelemetryHandle { shutdownAction = shutdown, metricsPort = boundPort }

shutdownTelemetry :: TelemetryHandle -> IO ()
shutdownTelemetry handle = do
  shutdownAction handle
  setGlobalBackend noOpBackend

-- ---------------------------------------------------------------------------
-- Tracing
-- ---------------------------------------------------------------------------

withSpan :: SpanName -> [(Text, AttributeValue)] -> IO a -> IO a
withSpan name attrs action = do
  backend <- getGlobalBackend
  bWithSpan backend name attrs action

startSpan :: SpanName -> [(Text, AttributeValue)] -> IO Span
startSpan name attrs = do
  backend <- getGlobalBackend
  bStartSpan backend name attrs

endSpan :: Span -> IO ()
endSpan span' = do
  backend <- getGlobalBackend
  bEndSpan backend span'

addAttribute :: Span -> Text -> AttributeValue -> IO ()
addAttribute span' key val = do
  backend <- getGlobalBackend
  bAddAttribute backend span' key val

recordException :: Span -> SomeException -> IO ()
recordException span' err = do
  backend <- getGlobalBackend
  bRecordException backend span' err

getSpanContext :: Span -> SpanContext
getSpanContext NoOpSpan = SpanContext (TraceId "") (SpanId "")
getSpanContext (RealSpan sd) = sdContext sd

getCurrentSpanContext :: IO (Maybe SpanContext)
getCurrentSpanContext = do
  backend <- getGlobalBackend
  bGetCurrentSpanCtx backend

-- ---------------------------------------------------------------------------
-- Logging
-- ---------------------------------------------------------------------------

log :: Severity -> LogMessage -> [(Text, AttributeValue)] -> IO ()
log severity msg attrs = do
  backend <- getGlobalBackend
  bLog backend severity msg attrs

-- ---------------------------------------------------------------------------
-- Metrics
-- ---------------------------------------------------------------------------

incrementCounter :: CounterName -> Int -> [(Text, AttributeValue)] -> IO ()
incrementCounter name val attrs = do
  backend <- getGlobalBackend
  bIncrementCounter backend name val attrs

addCounter :: CounterName -> Double -> [(Text, AttributeValue)] -> IO ()
addCounter name val attrs = do
  backend <- getGlobalBackend
  bAddCounter backend name val attrs

recordGauge :: GaugeName -> Double -> [(Text, AttributeValue)] -> IO ()
recordGauge name val attrs = do
  backend <- getGlobalBackend
  bRecordGauge backend name val attrs

recordHistogram :: HistogramName -> Double -> [(Text, AttributeValue)] -> IO ()
recordHistogram name val attrs = do
  backend <- getGlobalBackend
  bRecordHistogram backend name val attrs

-- | Export all registered metrics in Prometheus text exposition format.
-- Dispatches through the active backend — no direct dependency on
-- prometheus-client in call sites.
exportMetrics :: IO ByteString
exportMetrics = do
  backend <- getGlobalBackend
  bExportMetrics backend
