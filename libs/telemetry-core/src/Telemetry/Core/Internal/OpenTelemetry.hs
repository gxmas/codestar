module Telemetry.Core.Internal.OpenTelemetry
  ( createOTelBackend
  ) where

import Control.Concurrent.Async (cancel)
import Control.Exception (SomeException)
import Control.Monad (when)
import Data.Char (isAlpha, isAlphaNum)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', atomicModifyIORef')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Data.HashMap.Strict qualified as HashMap
import OpenTelemetry.Attributes (emptyAttributes)
import OpenTelemetry.Context (lookupSpan)
import OpenTelemetry.Context.ThreadLocal (getContext)
import OpenTelemetry.Exporter.OTLP.Span
  ( OTLPExporterConfig (..)
  , loadExporterEnvironmentVariables
  , otlpExporter
  )
import OpenTelemetry.Processor.Batch.Span (batchProcessor, batchTimeoutConfig)
import OpenTelemetry.Trace qualified as OTel
import OpenTelemetry.Trace.Core qualified as OTelCore
import OpenTelemetry.Trace.Id (Base (..), traceIdBaseEncodedText, spanIdBaseEncodedText)
import System.IO (hPutStrLn, stderr)

import Telemetry.Core.Internal.Backend (Backend (..))
import Telemetry.Core.Internal.MetricsServer (startMetricsServer)
import Telemetry.Core.Types

import Prometheus qualified as Prom

createOTelBackend :: OtelSettings -> IO (Backend, Maybe Int, IO ())
createOTelBackend settings = do
  tp <- case settings.endpoint of
    Nothing ->
      -- No endpoint in config; fall back to env-var-driven SDK detection
      -- (OTEL_TRACES_EXPORTER, OTEL_EXPORTER_OTLP_ENDPOINT, etc.).
      OTel.initializeGlobalTracerProvider
    Just url -> do
      -- Endpoint explicitly configured: create an OTLP HTTP exporter directly,
      -- merging any additional settings from standard env vars.
      baseConf  <- loadExporterEnvironmentVariables
      let conf  = baseConf { otlpTracesEndpoint = Just (T.unpack url) }
      exporter  <- otlpExporter conf
      processor <- batchProcessor batchTimeoutConfig exporter
      (_, opts) <- OTel.getTracerProviderInitializationOptions
      tp'       <- OTel.createTracerProvider [processor] opts
      OTel.setGlobalTracerProvider tp'
      pure tp'
  let tracer = OTel.makeTracer tp
        (OTelCore.InstrumentationLibrary
          { OTelCore.libraryName       = settings.serviceName
          , OTelCore.libraryVersion    = "0.1.0.0"
          , OTelCore.librarySchemaUrl  = ""
          , OTelCore.libraryAttributes = emptyAttributes
          })
        OTelCore.tracerOptions

  counterRegistry <- newIORef (Map.empty :: Map Text Prom.Counter)
  gaugeRegistry   <- newIORef (Map.empty :: Map Text Prom.Gauge)
  histoRegistry   <- newIORef (Map.empty :: Map Text Prom.Histogram)

  let backend = Backend
        { bWithSpan          = otelWithSpan tracer
        , bStartSpan         = otelStartSpan tracer
        , bEndSpan           = otelEndSpan
        , bAddAttribute      = otelAddAttribute
        , bRecordException   = otelRecordException
        , bGetCurrentSpanCtx = otelGetCurrentSpanCtx
        , bLog               = otelLog settings
        , bIncrementCounter  = otelIncrementCounter counterRegistry
        , bAddCounter        = \_ _ _ -> pure ()
        , bRecordGauge       = otelRecordGauge gaugeRegistry
        , bRecordHistogram   = otelRecordHistogram histoRegistry
        , bExportMetrics     = Prom.exportMetricsAsText
        , bShutdown          = OTel.shutdownTracerProvider tp
        }

  (boundPort, metricsShutdown) <- case settings.metricsPort of
    Just port -> do
      (actualPort, serverHandle) <- startMetricsServer settings.metricsBindHost port (bExportMetrics backend)
      hPutStrLn stderr $
        "Metrics server listening on "
          <> T.unpack settings.metricsBindHost
          <> ":"
          <> show actualPort
      pure (Just actualPort, cancel serverHandle)
    Nothing ->
      pure (Nothing, pure ())

  let shutdown = metricsShutdown >> OTel.shutdownTracerProvider tp

  pure (backend, boundPort, shutdown)

-- ---------------------------------------------------------------------------
-- Tracing
-- ---------------------------------------------------------------------------

otelWithSpan :: OTel.Tracer -> SpanName -> [(Text, AttributeValue)] -> IO a -> IO a
otelWithSpan tracer (SpanName name) attrs action =
  OTel.inSpan tracer name (toSpanArgs attrs) action

otelStartSpan :: OTel.Tracer -> SpanName -> [(Text, AttributeValue)] -> IO Span
otelStartSpan tracer (SpanName name) initialAttrs = do
  ctx <- getContext
  otelSpan <- OTelCore.createSpan tracer ctx name (toSpanArgs initialAttrs)
  now <- getCurrentTime
  attrsRef <- newIORef (Map.fromList initialAttrs)
  sc <- extractSpanContext otelSpan
  pure $ RealSpan SpanData
    { sdContext    = sc
    , sdStartTime  = now
    , sdAttributes = attrsRef
    , sdOtelSpan   = Just otelSpan
    }

otelEndSpan :: Span -> IO ()
otelEndSpan NoOpSpan = pure ()
otelEndSpan (RealSpan sd) = case sd.sdOtelSpan of
  Nothing -> pure ()
  Just s  -> OTelCore.endSpan s Nothing

otelAddAttribute :: Span -> Text -> AttributeValue -> IO ()
otelAddAttribute NoOpSpan _ _ = pure ()
otelAddAttribute (RealSpan sd) key val = do
  modifyIORef' sd.sdAttributes (Map.insert key val)
  case sd.sdOtelSpan of
    Nothing -> pure ()
    Just s  -> OTelCore.addAttribute s key (toOTelAttr val)

otelRecordException :: Span -> SomeException -> IO ()
otelRecordException NoOpSpan _ = pure ()
otelRecordException (RealSpan sd) err = do
  modifyIORef' sd.sdAttributes
    (Map.insert "exception.message" (TextValue (T.pack (show err))))
  case sd.sdOtelSpan of
    Nothing -> pure ()
    Just s  -> OTelCore.recordException s HashMap.empty Nothing err

otelGetCurrentSpanCtx :: IO (Maybe SpanContext)
otelGetCurrentSpanCtx = do
  ctx <- getContext
  case lookupSpan ctx of
    Nothing -> pure Nothing
    Just s  -> Just <$> extractSpanContext s

-- ---------------------------------------------------------------------------
-- Logging (structured JSON to stderr)
-- ---------------------------------------------------------------------------

otelLog :: OtelSettings -> Severity -> LogMessage -> [(Text, AttributeValue)] -> IO ()
otelLog settings severity (LogMessage msg) attrs = when settings.logToStderr $ do
  now <- getCurrentTime
  mCtx <- otelGetCurrentSpanCtx
  let entry = Aeson.object $
        [ AesonKey.fromText "timestamp" Aeson..= show now
        , AesonKey.fromText "severity"  Aeson..= show severity
        , AesonKey.fromText "message"   Aeson..= msg
        ]
        <> maybe []
            (\ctx ->
              [ AesonKey.fromText "trace_id" Aeson..= unTraceId ctx.traceId
              , AesonKey.fromText "span_id"  Aeson..= unSpanId ctx.spanId
              ])
            mCtx
        <> [ AesonKey.fromText k Aeson..= attrToJson v | (k, v) <- attrs ]
  hPutStrLn stderr (T.unpack (TE.decodeUtf8 (BL.toStrict (Aeson.encode entry))))

attrToJson :: AttributeValue -> Aeson.Value
attrToJson (TextValue t)   = Aeson.String t
attrToJson (IntValue i)    = Aeson.Number (fromIntegral i)
attrToJson (DoubleValue d) = Aeson.Number (realToFrac d)
attrToJson (BoolValue b)   = Aeson.Bool b

-- ---------------------------------------------------------------------------
-- Metrics (in-memory with prometheus-client)
-- ---------------------------------------------------------------------------

otelIncrementCounter :: IORef (Map Text Prom.Counter) -> CounterName -> Int -> [(Text, AttributeValue)] -> IO ()
otelIncrementCounter registry (CounterName name) amount _attrs = do
  counter <- getOrCreateCounter registry (normalizeMetricName name)
  _ <- Prom.addCounter counter (fromIntegral amount)
  pure ()

otelRecordGauge :: IORef (Map Text Prom.Gauge) -> GaugeName -> Double -> [(Text, AttributeValue)] -> IO ()
otelRecordGauge registry (GaugeName name) value _attrs = do
  gauge <- getOrCreateGauge registry (normalizeMetricName name)
  _ <- Prom.setGauge gauge value
  pure ()

otelRecordHistogram :: IORef (Map Text Prom.Histogram) -> HistogramName -> Double -> [(Text, AttributeValue)] -> IO ()
otelRecordHistogram registry (HistogramName name) value _attrs = do
  histogram <- getOrCreateHistogram registry (normalizeMetricName name)
  _ <- Prom.observe histogram value
  pure ()

getOrCreateCounter :: IORef (Map Text Prom.Counter) -> Text -> IO Prom.Counter
getOrCreateCounter registry name = do
  counters <- readIORef registry
  case Map.lookup name counters of
    Just c  -> pure c
    Nothing -> do
      c <- Prom.register $ Prom.counter (Prom.Info name "")
      atomicModifyIORef' registry (\m -> (Map.insert name c m, ()))
      pure c

getOrCreateGauge :: IORef (Map Text Prom.Gauge) -> Text -> IO Prom.Gauge
getOrCreateGauge registry name = do
  gauges <- readIORef registry
  case Map.lookup name gauges of
    Just g  -> pure g
    Nothing -> do
      g <- Prom.register $ Prom.gauge (Prom.Info name "")
      atomicModifyIORef' registry (\m -> (Map.insert name g m, ()))
      pure g

getOrCreateHistogram :: IORef (Map Text Prom.Histogram) -> Text -> IO Prom.Histogram
getOrCreateHistogram registry name = do
  histos <- readIORef registry
  case Map.lookup name histos of
    Just h  -> pure h
    Nothing -> do
      h <- Prom.register $ Prom.histogram (Prom.Info name "") Prom.defaultBuckets
      atomicModifyIORef' registry (\m -> (Map.insert name h m, ()))
      pure h

-- Prometheus metric names must match [a-zA-Z_:][a-zA-Z0-9_:]*.
normalizeMetricName :: Text -> Text
normalizeMetricName raw =
  let sanitized = T.map sanitizeChar raw
   in case T.uncons sanitized of
        Nothing -> "metric"
        Just (c, rest)
          | isValidFirst c -> T.cons c rest
          | otherwise -> T.cons '_' rest
 where
  sanitizeChar c
    | isAlphaNum c || c == '_' || c == ':' = c
    | otherwise = '_'

  isValidFirst c = isAlpha c || c == '_' || c == ':'

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

toSpanArgs :: [(Text, AttributeValue)] -> OTelCore.SpanArguments
toSpanArgs _attrs = OTelCore.defaultSpanArguments

toOTelAttr :: AttributeValue -> OTelCore.Attribute
toOTelAttr (TextValue t)   = OTelCore.toAttribute t
toOTelAttr (IntValue i)    = OTelCore.toAttribute i
toOTelAttr (DoubleValue d) = OTelCore.toAttribute d
toOTelAttr (BoolValue b)   = OTelCore.toAttribute b

extractSpanContext :: OTel.Span -> IO SpanContext
extractSpanContext s = do
  sc <- OTelCore.getSpanContext s
  pure SpanContext
    { traceId = TraceId (traceIdBaseEncodedText Base16 (OTelCore.traceId sc))
    , spanId  = SpanId (spanIdBaseEncodedText Base16 (OTelCore.spanId sc))
    }
