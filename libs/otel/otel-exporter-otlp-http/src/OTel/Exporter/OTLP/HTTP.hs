-- | OTLP/HTTP exporter for traces, metrics, and logs (protobuf and JSON).
module OTel.Exporter.OTLP.HTTP
  ( OtlpHttpConfig (..)
  , defaultOtlpHttpConfig
  , ContentType (..)
  , OtlpHttpSpanExporter
  , newOtlpHttpSpanExporter
  , newOtlpHttpSpanExporterFromEnv
  , OtlpHttpMetricExporter
  , newOtlpHttpMetricExporter
  , newOtlpHttpMetricExporterFromEnv
  , OtlpHttpLogRecordExporter
  , newOtlpHttpLogRecordExporter
  , newOtlpHttpLogRecordExporterFromEnv
  ) where

import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException, try)
import Data.Aeson ((.=), Value (..), encode, object)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.ProtoLens (encodeMessage)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word64)
import Codec.Compression.GZip qualified as GZip
import Network.HTTP.Client
  ( Manager
  , RequestBody (RequestBodyBS)
  , checkResponse
  , httpLbs
  , method
  , parseUrlThrow
  , requestBody
  , requestHeaders
  , responseBody
  , responseHeaders
  , responseStatus
  , responseTimeout
  , responseTimeoutMicro
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.Status qualified as Status
import Network.HTTP.Types (urlDecode)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Data.ByteString.Base64 qualified as B64
import Lens.Family2 ((^.))
import System.IO (hPutStrLn, stderr)

import OTel.Attribute qualified as Attr
import OTel.Exporter.OTLP.GRPC.Internal (Compression (..))
import OTel.Exporter.OTLP.GRPC.Internal.Proto
  ( spanListToExportRequest
  , metricDataToExportRequest
  , logListToExportRequest
  )
import OTel.Log (LogBody (..), SeverityNumber (..))

import Data.ProtoLens (decodeMessage)
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService ()
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as CTF
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService ()
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as CMF
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService ()
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields qualified as CLF
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as CollectorTrace
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as CollectorMetrics
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as CollectorLogs
import OTel.SDK.Export (ExportResult (..))
import OTel.SDK.Log.Export (LogRecordExporter (..), ReadableLogRecord (..), SomeReadableLogRecord (..))
import OTel.SDK.Metric.Export
  ( Aggregation (..)
  , AggregationTemporality (..)
  , ExponentialBuckets (..)
  , ExponentialHistogramData (..)
  , ExponentialHistogramDataPoint (..)
  , GaugeData (..)
  , HistogramData (..)
  , HistogramDataPoint (..)
  , MetricData (..)
  , MetricExporter (..)
  , MetricPointData (..)
  , NumberDataPoint (..)
  , ScopeMetrics (..)
  , SumData (..)
  )
import OTel.SDK.Metric.Export qualified as Metric
import OTel.SDK.Resource qualified as Resource
import OTel.SDK.Trace.Export
  ( Link (..)
  , ReadableSpan (..)
  , SomeReadableSpan (..)
  , SpanEvent (..)
  , SpanExporter (..)
  )
import OTel.Timestamp (Timestamp (..))
import OTel.Trace (SpanKind (..), SpanStatus (..), StatusCode (..))
import OTel.Trace.SpanContext qualified as SC
import OTel.Trace.TraceState qualified as TraceState


-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | Wire format for OTLP/HTTP payloads.
data ContentType = Protobuf | Json
  deriving stock (Show, Eq)


-- | Configuration for the OTLP/HTTP exporter.
data OtlpHttpConfig = OtlpHttpConfig
  { otlpHttpEndpoint    :: Text        -- ^ Base URL, e.g. "http://localhost:4318". Signal path (/v1/traces etc.) appended automatically.
  , otlpHttpHeaders     :: [(ByteString, ByteString)]
  , otlpHttpCompression :: Compression
  , otlpHttpTimeoutMs   :: Int
  , otlpHttpContentType :: ContentType
  } deriving stock (Show)


-- | Default OTLP/HTTP config: localhost:4318, protobuf, no compression.
defaultOtlpHttpConfig :: OtlpHttpConfig
defaultOtlpHttpConfig = OtlpHttpConfig
  { otlpHttpEndpoint    = "http://localhost:4318"
  , otlpHttpHeaders     = []
  , otlpHttpCompression = NoCompression
  , otlpHttpTimeoutMs   = 10_000
  , otlpHttpContentType = Protobuf
  }


-------------------------------------------------------------------------------
-- Span exporter
-------------------------------------------------------------------------------

-- | OTLP/HTTP span exporter.
data OtlpHttpSpanExporter = OtlpHttpSpanExporter
  { _httpSpanManager  :: Manager
  , _httpSpanConfig   :: OtlpHttpConfig
  , _httpSpanShutdown :: TVar Bool
  }


-- | Create an OTLP/HTTP span exporter from explicit config.
newOtlpHttpSpanExporter :: OtlpHttpConfig -> IO OtlpHttpSpanExporter
newOtlpHttpSpanExporter cfg = do
  mgr <- newTlsManager
  s <- newTVarIO False
  pure OtlpHttpSpanExporter
    { _httpSpanManager  = mgr
    , _httpSpanConfig   = cfg
    , _httpSpanShutdown = s
    }


-- | Create an OTLP/HTTP span exporter reading config from environment variables.
newOtlpHttpSpanExporterFromEnv :: IO OtlpHttpSpanExporter
newOtlpHttpSpanExporterFromEnv = do
  cfg <- readConfigFromEnv "TRACES" "/v1/traces"
  newOtlpHttpSpanExporter cfg


instance SpanExporter OtlpHttpSpanExporter where
  exportSpans e spans = do
    done <- readTVarIO e._httpSpanShutdown
    if done then pure ExportFailure
    else doHttpExport e._httpSpanManager e._httpSpanConfig
           (signalUrl e._httpSpanConfig.otlpHttpEndpoint "/v1/traces")
           (serializeSpans e._httpSpanConfig.otlpHttpContentType spans)
           (warnTracePartialSuccess e._httpSpanConfig.otlpHttpContentType)

  shutdownExporter e = do
    atomically (writeTVar e._httpSpanShutdown True)
    pure (Right ())

  forceFlushExporter _ _ = pure (Right ())


-------------------------------------------------------------------------------
-- Metric exporter
-------------------------------------------------------------------------------

-- | OTLP/HTTP metric exporter.
data OtlpHttpMetricExporter = OtlpHttpMetricExporter
  { _httpMetricManager  :: Manager
  , _httpMetricConfig   :: OtlpHttpConfig
  , _httpMetricShutdown :: TVar Bool
  }


-- | Create an OTLP/HTTP metric exporter from explicit config.
newOtlpHttpMetricExporter :: OtlpHttpConfig -> IO OtlpHttpMetricExporter
newOtlpHttpMetricExporter cfg = do
  mgr <- newTlsManager
  s <- newTVarIO False
  pure OtlpHttpMetricExporter
    { _httpMetricManager  = mgr
    , _httpMetricConfig   = cfg
    , _httpMetricShutdown = s
    }


-- | Create an OTLP/HTTP metric exporter reading config from environment variables.
newOtlpHttpMetricExporterFromEnv :: IO OtlpHttpMetricExporter
newOtlpHttpMetricExporterFromEnv = do
  cfg <- readConfigFromEnv "METRICS" "/v1/metrics"
  newOtlpHttpMetricExporter cfg


instance MetricExporter OtlpHttpMetricExporter where
  exportMetrics e md = do
    done <- readTVarIO e._httpMetricShutdown
    if done then pure ExportFailure
    else doHttpExport e._httpMetricManager e._httpMetricConfig
           (signalUrl e._httpMetricConfig.otlpHttpEndpoint "/v1/metrics")
           (serializeMetrics e._httpMetricConfig.otlpHttpContentType md)
           (warnMetricsPartialSuccess e._httpMetricConfig.otlpHttpContentType)

  shutdownMetricExporter e = do
    atomically (writeTVar e._httpMetricShutdown True)
    pure (Right ())

  forceFlushMetricExporter _ _ = pure (Right ())

  exporterTemporality _ _ = Cumulative

  exporterDefaultAggregation _ _ = DefaultAggregation


-------------------------------------------------------------------------------
-- Log record exporter
-------------------------------------------------------------------------------

-- | OTLP/HTTP log record exporter.
data OtlpHttpLogRecordExporter = OtlpHttpLogRecordExporter
  { _httpLogManager  :: Manager
  , _httpLogConfig   :: OtlpHttpConfig
  , _httpLogShutdown :: TVar Bool
  }


-- | Create an OTLP/HTTP log record exporter from explicit config.
newOtlpHttpLogRecordExporter :: OtlpHttpConfig -> IO OtlpHttpLogRecordExporter
newOtlpHttpLogRecordExporter cfg = do
  mgr <- newTlsManager
  s <- newTVarIO False
  pure OtlpHttpLogRecordExporter
    { _httpLogManager  = mgr
    , _httpLogConfig   = cfg
    , _httpLogShutdown = s
    }


-- | Create an OTLP/HTTP log exporter reading config from environment variables.
newOtlpHttpLogRecordExporterFromEnv :: IO OtlpHttpLogRecordExporter
newOtlpHttpLogRecordExporterFromEnv = do
  cfg <- readConfigFromEnv "LOGS" "/v1/logs"
  newOtlpHttpLogRecordExporter cfg


instance LogRecordExporter OtlpHttpLogRecordExporter where
  exportLogRecords e records = do
    done <- readTVarIO e._httpLogShutdown
    if done then pure ExportFailure
    else doHttpExport e._httpLogManager e._httpLogConfig
           (signalUrl e._httpLogConfig.otlpHttpEndpoint "/v1/logs")
           (serializeLogs e._httpLogConfig.otlpHttpContentType records)
           (warnLogsPartialSuccess e._httpLogConfig.otlpHttpContentType)

  shutdownLogExporter e = do
    atomically (writeTVar e._httpLogShutdown True)
    pure (Right ())

  forceFlushLogExporter _ _ = pure (Right ())


-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

serializeSpans :: ContentType -> [SomeReadableSpan] -> ByteString
serializeSpans Protobuf spans = encodeMessage (spanListToExportRequest spans)
serializeSpans Json spans     = LBS.toStrict (encode (spansToOtlpJson spans))


serializeMetrics :: ContentType -> MetricData -> ByteString
serializeMetrics Protobuf md = encodeMessage (metricDataToExportRequest md)
serializeMetrics Json md     = LBS.toStrict (encode (metricsToOtlpJson md))


serializeLogs :: ContentType -> [SomeReadableLogRecord] -> ByteString
serializeLogs Protobuf logs = encodeMessage (logListToExportRequest logs)
serializeLogs Json logs     = LBS.toStrict (encode (logsToOtlpJson logs))


-------------------------------------------------------------------------------
-- HTTP transport with retry
-------------------------------------------------------------------------------

-- | Build the final URL: if the base already ends with the signal path
-- (e.g., from a signal-specific env var), use it as-is; otherwise append.
signalUrl :: Text -> String -> String
signalUrl base path =
  let s = T.unpack base
  in if path `isSuffixOf` s then s else s <> path
  where isSuffixOf suf str = drop (length str - length suf) str == suf


doHttpExport :: Manager -> OtlpHttpConfig -> String -> ByteString -> (ByteString -> IO ()) -> IO ExportResult
doHttpExport mgr cfg url body checkPs = go (0 :: Int)
  where
    go attempt = do
      result <- try @SomeException $ do
        req0 <- parseUrlThrow url
        let (encodedBody, compHdr) = case cfg.otlpHttpCompression of
              GzipCompression ->
                ( LBS.toStrict (GZip.compress (LBS.fromStrict body))
                , [(CI.mk "Content-Encoding", "gzip")] )
              NoCompression -> (body, [])
            ctHdr = case cfg.otlpHttpContentType of
              Protobuf -> "application/x-protobuf"
              Json     -> "application/json"
            req = req0
              { method = "POST"
              , requestHeaders =
                  [(CI.mk "Content-Type", ctHdr)]
                  <> compHdr
                  <> map (\(k, v) -> (CI.mk k, v)) cfg.otlpHttpHeaders
              , requestBody = RequestBodyBS encodedBody
              , responseTimeout = responseTimeoutMicro (cfg.otlpHttpTimeoutMs * 1000)
              , checkResponse = \_ _ -> pure ()
              }
        httpLbs req mgr
      case result of
        Left _  -> pure ExportFailure
        Right resp ->
          let sc = Status.statusCode (responseStatus resp)
          in if sc >= 200 && sc < 300
             then do
               checkPs (LBS.toStrict (responseBody resp))
               pure ExportSuccess
             else if sc `elem` [408, 429, 502, 503, 504] && attempt < 4
               then do
                 let retryMs = parseRetryAfterMs (responseHeaders resp)
                     delayMs = fromMaybe (1000 * (2 ^ attempt)) retryMs
                 threadDelay (delayMs * 1000)
                 go (attempt + 1)
               else pure ExportFailure


parseRetryAfterMs :: [(HeaderName, ByteString)] -> Maybe Int
parseRetryAfterMs hdrs =
  case lookup (CI.mk "Retry-After") hdrs of
    Just val -> case readMaybe (B8.unpack val) of
      Just secs -> Just (secs * 1000)
      Nothing   -> Nothing
    Nothing -> Nothing


-------------------------------------------------------------------------------
-- Partial success warnings
-------------------------------------------------------------------------------

-- | Extract a rejected-count field from an OTLP JSON partial_success response.
-- Returns 0 if the field is absent or the body is not parseable.
jsonRejectedCount :: Aeson.Key -> ByteString -> Int
jsonRejectedCount field bs =
  case Aeson.eitherDecodeStrict bs of
    Left _               -> 0
    Right (Object o) ->
      case KM.lookup (AesonKey.fromString "partialSuccess") o of
        Just (Object ps) ->
          case KM.lookup field ps of
            Just (Number n) -> round n
            _               -> 0
        _ -> 0
    Right _ -> 0

warnTracePartialSuccess :: ContentType -> ByteString -> IO ()
warnTracePartialSuccess Json bs =
  let n = jsonRejectedCount (AesonKey.fromString "rejectedSpans") bs
  in when (n > 0) $ hPutStrLn stderr $
       "[OTel] OTLP/HTTP partial success: " <> show n <> " spans rejected"
warnTracePartialSuccess Protobuf bs = case decodeMessage bs of
  Right resp ->
    case (resp :: CollectorTrace.ExportTraceServiceResponse) ^. CTF.maybe'partialSuccess of
      Just ps | ps ^. CTF.rejectedSpans > 0 ->
        hPutStrLn stderr $ "[OTel] OTLP/HTTP partial success: " <>
          show (ps ^. CTF.rejectedSpans) <> " spans rejected"
      _ -> pure ()
  Left _ -> pure ()


warnMetricsPartialSuccess :: ContentType -> ByteString -> IO ()
warnMetricsPartialSuccess Json bs =
  let n = jsonRejectedCount (AesonKey.fromString "rejectedDataPoints") bs
  in when (n > 0) $ hPutStrLn stderr $
       "[OTel] OTLP/HTTP partial success: " <> show n <> " data points rejected"
warnMetricsPartialSuccess Protobuf bs = case decodeMessage bs of
  Right resp ->
    case (resp :: CollectorMetrics.ExportMetricsServiceResponse) ^. CMF.maybe'partialSuccess of
      Just ps | ps ^. CMF.rejectedDataPoints > 0 ->
        hPutStrLn stderr $ "[OTel] OTLP/HTTP partial success: " <>
          show (ps ^. CMF.rejectedDataPoints) <> " data points rejected"
      _ -> pure ()
  Left _ -> pure ()


warnLogsPartialSuccess :: ContentType -> ByteString -> IO ()
warnLogsPartialSuccess Json bs =
  let n = jsonRejectedCount (AesonKey.fromString "rejectedLogRecords") bs
  in when (n > 0) $ hPutStrLn stderr $
       "[OTel] OTLP/HTTP partial success: " <> show n <> " log records rejected"
warnLogsPartialSuccess Protobuf bs = case decodeMessage bs of
  Right resp ->
    case (resp :: CollectorLogs.ExportLogsServiceResponse) ^. CLF.maybe'partialSuccess of
      Just ps | ps ^. CLF.rejectedLogRecords > 0 ->
        hPutStrLn stderr $ "[OTel] OTLP/HTTP partial success: " <>
          show (ps ^. CLF.rejectedLogRecords) <> " log records rejected"
      _ -> pure ()
  Left _ -> pure ()


-------------------------------------------------------------------------------
-- JSON encoding — Traces
-------------------------------------------------------------------------------

spansToOtlpJson :: [SomeReadableSpan] -> Value
spansToOtlpJson spans =
  object ["resourceSpans" .= map resourceSpansJson (groupByResourceSpans spans)]


groupByResourceSpans :: [SomeReadableSpan] -> [(Resource.Resource, [(Attr.InstrumentationScope, [SomeReadableSpan])])]
groupByResourceSpans spans = accumToList $
  foldl (\m s@(SomeReadableSpan sp) -> accumInsertNested (readResource sp) (readInstrumentationScope sp) s m) [] spans


resourceSpansJson :: (Resource.Resource, [(Attr.InstrumentationScope, [SomeReadableSpan])]) -> Value
resourceSpansJson (res, scopeGroups) = object
  [ "resource" .= resourceJson res
  , "scopeSpans" .= map scopeSpansJson scopeGroups
  ]


scopeSpansJson :: (Attr.InstrumentationScope, [SomeReadableSpan]) -> Value
scopeSpansJson (is, ss) = object
  [ "scope" .= instrumentationScopeJson is
  , "spans" .= map (\(SomeReadableSpan s) -> spanJson s) ss
  ]


spanJson :: ReadableSpan s => s -> Value
spanJson s =
  let sc = readSpanContext s
  in object
    [ "traceId" .= SC.traceIdToHex sc.traceId
    , "spanId" .= SC.spanIdToHex sc.spanId
    , "traceState" .= traceStateJson sc.traceState
    , "parentSpanId" .= maybe ("" :: Text) (SC.spanIdToHex . (.spanId)) (readParentSpanContext s)
    , "name" .= readName s
    , "kind" .= spanKindStr (readKind s)
    , "startTimeUnixNano" .= T.pack (show (unTimestamp (readStartTimestamp s)))
    , "endTimeUnixNano" .= T.pack (show (unTimestamp (readEndTimestamp s)))
    , "attributes" .= attributesJson (readAttributes s)
    , "events" .= map spanEventJson (readEvents s)
    , "links" .= map linkJson (readLinks s)
    , "status" .= spanStatusJson (readStatus s)
    , "droppedAttributesCount" .= readDroppedAttributesCount s
    , "droppedEventsCount" .= readDroppedEventsCount s
    , "droppedLinksCount" .= readDroppedLinksCount s
    ]


spanKindStr :: SpanKind -> Text
spanKindStr Internal = "SPAN_KIND_INTERNAL"
spanKindStr Server   = "SPAN_KIND_SERVER"
spanKindStr Client   = "SPAN_KIND_CLIENT"
spanKindStr Producer = "SPAN_KIND_PRODUCER"
spanKindStr Consumer = "SPAN_KIND_CONSUMER"


statusCodeStr :: StatusCode -> Text
statusCodeStr Unset = "STATUS_CODE_UNSET"
statusCodeStr Ok    = "STATUS_CODE_OK"
statusCodeStr Error = "STATUS_CODE_ERROR"


spanStatusJson :: SpanStatus -> Value
spanStatusJson ss = object
  [ "code" .= statusCodeStr ss.statusCode
  , "message" .= fromMaybe ("" :: Text) ss.statusDescription
  ]


spanEventJson :: SpanEvent -> Value
spanEventJson ev = object
  [ "name" .= ev.eventName
  , "timeUnixNano" .= T.pack (show (unTimestamp ev.eventTimestamp))
  , "attributes" .= attributesJson ev.eventAttributes
  , "droppedAttributesCount" .= ev.eventDroppedAttributesCount
  ]


linkJson :: Link -> Value
linkJson lnk =
  let lsc = lnk.linkSpanContext
  in object
    [ "traceId" .= SC.traceIdToHex lsc.traceId
    , "spanId" .= SC.spanIdToHex lsc.spanId
    , "traceState" .= traceStateJson lsc.traceState
    , "attributes" .= attributesJson lnk.linkAttributes
    , "droppedAttributesCount" .= lnk.linkDroppedAttributesCount
    ]


traceStateJson :: TraceState.TraceState -> Text
traceStateJson ts =
  T.intercalate "," (map (\(k, v) -> k <> "=" <> v) (TraceState.toList ts))


-------------------------------------------------------------------------------
-- JSON encoding — Metrics
-------------------------------------------------------------------------------

metricsToOtlpJson :: MetricData -> Value
metricsToOtlpJson md = object
  [ "resourceMetrics" .=
      [ object
          [ "resource" .= resourceJson md.mdResource
          , "scopeMetrics" .= map scopeMetricsJson md.mdScopeMetrics
          ]
      ]
  ]


scopeMetricsJson :: ScopeMetrics -> Value
scopeMetricsJson sm = object
  [ "scope" .= instrumentationScopeJson sm.smScope
  , "metrics" .= map metricJson sm.smMetrics
  ]


metricJson :: Metric.Metric -> Value
metricJson met = object $
  [ "name" .= met.metricName
  , "description" .= met.metricDescription
  , "unit" .= met.metricUnit
  ] <> metricPointDataJson met.metricPointData


metricPointDataJson :: MetricPointData -> [Pair]
metricPointDataJson (SumPointData sd) =
  [ "sum" .= object
      [ "dataPoints" .= map numberDataPointJson sd.sumDataPoints
      , "isMonotonic" .= sd.sumIsMonotonic
      , "aggregationTemporality" .= temporalityStr sd.sumTemporality
      ]
  ]
metricPointDataJson (GaugePointData gd) =
  [ "gauge" .= object
      [ "dataPoints" .= map numberDataPointJson gd.gaugeDataPoints
      ]
  ]
metricPointDataJson (HistogramPointData hd) =
  [ "histogram" .= object
      [ "dataPoints" .= map histogramDataPointJson hd.histDataPoints
      , "aggregationTemporality" .= temporalityStr hd.histTemporality
      ]
  ]
metricPointDataJson (ExponentialHistogramPointData ehd) =
  [ "exponentialHistogram" .= object
      [ "dataPoints" .= map expHistogramDataPointJson ehd.expHistDataPoints
      , "aggregationTemporality" .= temporalityStr ehd.expHistTemporality
      ]
  ]


temporalityStr :: AggregationTemporality -> Text
temporalityStr Delta      = "AGGREGATION_TEMPORALITY_DELTA"
temporalityStr Cumulative = "AGGREGATION_TEMPORALITY_CUMULATIVE"


severityNumberStr :: SeverityNumber -> Text
severityNumberStr SeverityTrace  = "SEVERITY_NUMBER_TRACE"
severityNumberStr SeverityTrace2 = "SEVERITY_NUMBER_TRACE2"
severityNumberStr SeverityTrace3 = "SEVERITY_NUMBER_TRACE3"
severityNumberStr SeverityTrace4 = "SEVERITY_NUMBER_TRACE4"
severityNumberStr SeverityDebug  = "SEVERITY_NUMBER_DEBUG"
severityNumberStr SeverityDebug2 = "SEVERITY_NUMBER_DEBUG2"
severityNumberStr SeverityDebug3 = "SEVERITY_NUMBER_DEBUG3"
severityNumberStr SeverityDebug4 = "SEVERITY_NUMBER_DEBUG4"
severityNumberStr SeverityInfo   = "SEVERITY_NUMBER_INFO"
severityNumberStr SeverityInfo2  = "SEVERITY_NUMBER_INFO2"
severityNumberStr SeverityInfo3  = "SEVERITY_NUMBER_INFO3"
severityNumberStr SeverityInfo4  = "SEVERITY_NUMBER_INFO4"
severityNumberStr SeverityWarn   = "SEVERITY_NUMBER_WARN"
severityNumberStr SeverityWarn2  = "SEVERITY_NUMBER_WARN2"
severityNumberStr SeverityWarn3  = "SEVERITY_NUMBER_WARN3"
severityNumberStr SeverityWarn4  = "SEVERITY_NUMBER_WARN4"
severityNumberStr SeverityError  = "SEVERITY_NUMBER_ERROR"
severityNumberStr SeverityError2 = "SEVERITY_NUMBER_ERROR2"
severityNumberStr SeverityError3 = "SEVERITY_NUMBER_ERROR3"
severityNumberStr SeverityError4 = "SEVERITY_NUMBER_ERROR4"
severityNumberStr SeverityFatal  = "SEVERITY_NUMBER_FATAL"
severityNumberStr SeverityFatal2 = "SEVERITY_NUMBER_FATAL2"
severityNumberStr SeverityFatal3 = "SEVERITY_NUMBER_FATAL3"
severityNumberStr SeverityFatal4 = "SEVERITY_NUMBER_FATAL4"


numberDataPointJson :: NumberDataPoint -> Value
numberDataPointJson ndp = object
  [ "startTimeUnixNano" .= T.pack (show (unTimestamp ndp.ndpStartTime))
  , "timeUnixNano" .= T.pack (show (unTimestamp ndp.ndpTime))
  , "asDouble" .= ndp.ndpValue
  , "attributes" .= attributesJson ndp.ndpAttributes
  ]


histogramDataPointJson :: HistogramDataPoint -> Value
histogramDataPointJson hdp = object $
  [ "startTimeUnixNano" .= T.pack (show (unTimestamp hdp.hdpStartTime))
  , "timeUnixNano" .= T.pack (show (unTimestamp hdp.hdpTime))
  , "count" .= T.pack (show hdp.hdpCount)
  , "bucketCounts" .= map (T.pack . show) hdp.hdpBucketCounts
  , "explicitBounds" .= hdp.hdpExplicitBounds
  , "attributes" .= attributesJson hdp.hdpAttributes
  ] <> maybe [] (\s -> ["sum" .= s]) hdp.hdpSum
    <> maybe [] (\m -> ["min" .= m]) hdp.hdpMin
    <> maybe [] (\m -> ["max" .= m]) hdp.hdpMax


expHistogramDataPointJson :: ExponentialHistogramDataPoint -> Value
expHistogramDataPointJson ehdp = object $
  [ "startTimeUnixNano" .= T.pack (show (unTimestamp ehdp.ehdpStartTime))
  , "timeUnixNano" .= T.pack (show (unTimestamp ehdp.ehdpTime))
  , "count" .= T.pack (show ehdp.ehdpCount)
  , "scale" .= ehdp.ehdpScale
  , "zeroCount" .= T.pack (show ehdp.ehdpZeroCount)
  , "zeroThreshold" .= ehdp.ehdpZeroThreshold
  , "positive" .= expBucketsJson ehdp.ehdpPositive
  , "negative" .= expBucketsJson ehdp.ehdpNegative
  , "attributes" .= attributesJson ehdp.ehdpAttributes
  ] <> maybe [] (\s -> ["sum" .= s]) ehdp.ehdpSum
    <> maybe [] (\m -> ["min" .= m]) ehdp.ehdpMin
    <> maybe [] (\m -> ["max" .= m]) ehdp.ehdpMax


expBucketsJson :: ExponentialBuckets -> Value
expBucketsJson eb = object
  [ "offset" .= eb.ebOffset
  , "bucketCounts" .= map (T.pack . show) eb.ebBucketCounts
  ]


-------------------------------------------------------------------------------
-- JSON encoding — Logs
-------------------------------------------------------------------------------

logsToOtlpJson :: [SomeReadableLogRecord] -> Value
logsToOtlpJson records =
  object ["resourceLogs" .= map resourceLogsJson (groupByResourceLogs records)]


groupByResourceLogs :: [SomeReadableLogRecord] -> [(Resource.Resource, [(Attr.InstrumentationScope, [SomeReadableLogRecord])])]
groupByResourceLogs records = accumToList $
  foldl (\m r@(SomeReadableLogRecord lr) -> accumInsertNested (rlrResource lr) (rlrScope lr) r m) [] records


resourceLogsJson :: (Resource.Resource, [(Attr.InstrumentationScope, [SomeReadableLogRecord])]) -> Value
resourceLogsJson (res, scopeGroups) = object
  [ "resource" .= resourceJson res
  , "scopeLogs" .= map scopeLogsJson scopeGroups
  ]


scopeLogsJson :: (Attr.InstrumentationScope, [SomeReadableLogRecord]) -> Value
scopeLogsJson (is, recs) = object
  [ "scope" .= instrumentationScopeJson is
  , "logRecords" .= map (\(SomeReadableLogRecord r) -> logRecordJson r) recs
  ]


logRecordJson :: ReadableLogRecord r => r -> Value
logRecordJson r = object $
  [ "timeUnixNano" .= T.pack (show (maybe (0 :: Word64) unTimestamp (rlrTimestamp r)))
  , "observedTimeUnixNano" .= T.pack (show (unTimestamp (rlrObservedTimestamp r)))
  , "severityNumber" .= maybe ("SEVERITY_NUMBER_UNSPECIFIED" :: Text) severityNumberStr (rlrSeverityNumber r)
  , "severityText" .= fromMaybe ("" :: Text) (rlrSeverityText r)
  , "attributes" .= attributesJson (rlrAttributes r)
  , "droppedAttributesCount" .= rlrDroppedAttributes r
  , "traceId" .= maybe ("" :: Text) (SC.traceIdToHex . (.traceId)) (rlrSpanContext r)
  , "spanId" .= maybe ("" :: Text) (SC.spanIdToHex . (.spanId)) (rlrSpanContext r)
  ] <> maybe [] (\b -> ["body" .= logBodyJson b]) (rlrBody r)


logBodyJson :: LogBody -> Value
logBodyJson (LogBodyString t)  = object ["stringValue" .= t]
logBodyJson (LogBodyBool b)    = object ["boolValue" .= b]
logBodyJson (LogBodyInt64 i)   = object ["intValue" .= i]
logBodyJson (LogBodyFloat64 d) = object ["doubleValue" .= d]
logBodyJson (LogBodyBytes bs)  = object ["bytesValue" .= TE.decodeASCII (B64.encode bs)]
logBodyJson (LogBodyList xs)   = object ["arrayValue" .= object ["values" .= map logBodyJson xs]]
logBodyJson (LogBodyMap m)     = object ["kvlistValue" .= object ["values" .= map logKvJson (Map.toList m)]]


logKvJson :: (Text, LogBody) -> Value
logKvJson (k, v) = object ["key" .= k, "value" .= logBodyJson v]


-------------------------------------------------------------------------------
-- JSON encoding — Common
-------------------------------------------------------------------------------

resourceJson :: Resource.Resource -> Value
resourceJson res = object
  [ "attributes" .= attributesJson (Resource.getAttributes res)
  ]


instrumentationScopeJson :: Attr.InstrumentationScope -> Value
instrumentationScopeJson is = object
  [ "name" .= is.scopeName
  , "version" .= fromMaybe ("" :: Text) is.scopeVersion
  ]


attributesJson :: Attr.Attributes -> [Value]
attributesJson attrs = map attrKvJson (Attr.toList attrs)


attrKvJson :: (Text, Attr.AttributeValue) -> Value
attrKvJson (k, v) = object ["key" .= k, "value" .= attrValueJson v]


attrValueJson :: Attr.AttributeValue -> Value
attrValueJson (Attr.StringValue t)       = object ["stringValue" .= t]
attrValueJson (Attr.BoolValue b)         = object ["boolValue" .= b]
attrValueJson (Attr.Int64Value i)        = object ["intValue" .= i]
attrValueJson (Attr.Float64Value d)      = object ["doubleValue" .= d]
attrValueJson (Attr.StringArrayValue vs) = object ["arrayValue" .= object ["values" .= map (attrValueJson . Attr.StringValue) (V.toList vs)]]
attrValueJson (Attr.BoolArrayValue vs)   = object ["arrayValue" .= object ["values" .= map (attrValueJson . Attr.BoolValue) (V.toList vs)]]
attrValueJson (Attr.Int64ArrayValue vs)  = object ["arrayValue" .= object ["values" .= map (attrValueJson . Attr.Int64Value) (V.toList vs)]]
attrValueJson (Attr.Float64ArrayValue vs) = object ["arrayValue" .= object ["values" .= map (attrValueJson . Attr.Float64Value) (V.toList vs)]]


-------------------------------------------------------------------------------
-- Grouping utilities
-------------------------------------------------------------------------------

-- | Order-preserving two-level grouping accumulator.
type NestedAccum k1 k2 v = [(k1, [(k2, [v])])]


accumInsertNested :: (Eq k1, Eq k2) => k1 -> k2 -> v -> NestedAccum k1 k2 v -> NestedAccum k1 k2 v
accumInsertNested k1 k2 v [] = [(k1, [(k2, [v])])]
accumInsertNested k1 k2 v ((k1', inner) : rest)
  | k1 == k1' = (k1', insertInner k2 v inner) : rest
  | otherwise  = (k1', inner) : accumInsertNested k1 k2 v rest
  where
    insertInner :: Eq k => k -> a -> [(k, [a])] -> [(k, [a])]
    insertInner key val [] = [(key, [val])]
    insertInner key val ((key', vals) : xs)
      | key == key' = (key', vals ++ [val]) : xs
      | otherwise   = (key', vals) : insertInner key val xs


accumToList :: NestedAccum k1 k2 v -> [(k1, [(k2, [v])])]
accumToList = id


-------------------------------------------------------------------------------
-- Environment variable reading
-------------------------------------------------------------------------------

readConfigFromEnv :: String -> String -> IO OtlpHttpConfig
readConfigFromEnv signal signalPath = do
  specificEp <- lookupEnv ("OTEL_EXPORTER_OTLP_" <> signal <> "_ENDPOINT")
  genericEp  <- lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
  headersVal <- lookupEnvSignal signal "HEADERS"
  compressionVal <- lookupEnvSignal signal "COMPRESSION"
  timeoutVal <- lookupEnvSignal signal "TIMEOUT"

  let baseCfg = defaultOtlpHttpConfig

  -- Signal-specific endpoint is a FULL URL (use as-is).
  -- Generic endpoint is a BASE URL (append signal path).
  let cfgWithEndpoint = case specificEp of
        Just ep -> baseCfg { otlpHttpEndpoint = T.pack ep }
        Nothing -> case genericEp of
          Just ep -> baseCfg { otlpHttpEndpoint = T.pack ep <> T.pack signalPath }
          Nothing -> baseCfg { otlpHttpEndpoint = baseCfg.otlpHttpEndpoint <> T.pack signalPath }

  let cfgWithHeaders = case headersVal of
        Nothing -> cfgWithEndpoint
        Just hdrStr -> cfgWithEndpoint
          { otlpHttpHeaders = parseHeaders hdrStr }

  let cfgWithCompression = case compressionVal of
        Nothing -> cfgWithHeaders
        Just "gzip" -> cfgWithHeaders { otlpHttpCompression = GzipCompression }
        Just _ -> cfgWithHeaders { otlpHttpCompression = NoCompression }

  let cfgWithTimeout = case timeoutVal of
        Nothing -> cfgWithCompression
        Just tStr -> case readMaybe tStr of
          Just ms -> cfgWithCompression { otlpHttpTimeoutMs = ms }
          Nothing -> cfgWithCompression

  pure cfgWithTimeout


lookupEnvSignal :: String -> String -> IO (Maybe String)
lookupEnvSignal signal suffix = do
  specific <- lookupEnv ("OTEL_EXPORTER_OTLP_" <> signal <> "_" <> suffix)
  case specific of
    Just v  -> pure (Just v)
    Nothing -> lookupEnv ("OTEL_EXPORTER_OTLP_" <> suffix)


parseHeaders :: String -> [(ByteString, ByteString)]
parseHeaders str =
  let pairs = splitOn ',' str
  in concatMap parsePair pairs
  where
    parsePair :: String -> [(ByteString, ByteString)]
    parsePair s = case break (== '=') s of
      (k, '=':v) ->
        [(urlDecode False (B8.pack k), urlDecode False (B8.pack v))]
      _ -> []

    splitOn :: Char -> String -> [String]
    splitOn _ [] = []
    splitOn c xs =
      let (before, after) = break (== c) xs
      in before : case after of
        []     -> []
        _:rest -> splitOn c rest
