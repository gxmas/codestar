-- | Proto-lens conversion functions: SDK types to OTLP protobuf messages.
module OTel.Exporter.OTLP.GRPC.Internal.Proto
  ( -- * Common types
    toProtoAnyValue
  , toProtoAttributes
  , toProtoInstrumentationScope
  , toProtoResource

    -- * Trace
  , toProtoSpan

    -- * Export requests
  , spanListToExportRequest
  , metricDataToExportRequest
  , logListToExportRequest

    -- * Response parsing
  , parseTraceExportResult
  , parseMetricsExportResult
  , parseLogsExportResult
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.ProtoLens (decodeMessage, defMessage)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Data.Bits ((.|.))
import Data.Word (Word8, Word32)
import Lens.Family2 ((.~), (&), (^.))
import System.IO (hPutStrLn, stderr)

import OTel.Attribute qualified as Attr
import OTel.Log (LogBody (..), SeverityNumber, severityNumberValue)
import OTel.SDK.Export (ExportResult (..))
import OTel.SDK.Log.Export (ReadableLogRecord (..), SomeReadableLogRecord (..))
import OTel.SDK.Metric.Export
  ( AggregationTemporality (..)
  , ExponentialBuckets (..)
  , ExponentialHistogramData (..)
  , ExponentialHistogramDataPoint (..)
  , GaugeData (..)
  , HistogramData (..)
  , HistogramDataPoint (..)
  , MetricData (..)
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
  )
import OTel.Timestamp (Timestamp (..))
import OTel.Trace (SpanKind (..), SpanStatus (..), StatusCode (..))
import OTel.Trace.SpanContext qualified as SC
import OTel.Trace.TraceState qualified as TraceState

import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as CollectorLogs
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields qualified as CLF
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as CollectorMetrics
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as CMF
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as CollectorTrace
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as CTF
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as Common
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as CF
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as L
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as LF
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as M
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as MF
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as R
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields qualified as RF
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as T
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as TF


-------------------------------------------------------------------------------
-- Common types
-------------------------------------------------------------------------------

-- | Convert an SDK 'AttributeValue' to the OTLP AnyValue protobuf type.
toProtoAnyValue :: Attr.AttributeValue -> Common.AnyValue
toProtoAnyValue av = case av of
  Attr.StringValue t ->
    defMessage & CF.stringValue .~ t
  Attr.BoolValue b ->
    defMessage & CF.boolValue .~ b
  Attr.Int64Value i ->
    defMessage & CF.intValue .~ i
  Attr.Float64Value d ->
    defMessage & CF.doubleValue .~ d
  Attr.StringArrayValue vs ->
    defMessage & CF.arrayValue .~ toArrayValue (fmap (toProtoAnyValue . Attr.StringValue) vs)
  Attr.BoolArrayValue vs ->
    defMessage & CF.arrayValue .~ toArrayValue (fmap (toProtoAnyValue . Attr.BoolValue) vs)
  Attr.Int64ArrayValue vs ->
    defMessage & CF.arrayValue .~ toArrayValue (fmap (toProtoAnyValue . Attr.Int64Value) vs)
  Attr.Float64ArrayValue vs ->
    defMessage & CF.arrayValue .~ toArrayValue (fmap (toProtoAnyValue . Attr.Float64Value) vs)
  where
    toArrayValue :: V.Vector Common.AnyValue -> Common.ArrayValue
    toArrayValue vs = defMessage & CF.values .~ V.toList vs


-- | Convert SDK 'Attributes' to a list of OTLP KeyValue protobuf messages.
toProtoAttributes :: Attr.Attributes -> [Common.KeyValue]
toProtoAttributes attrs = map toKV (Attr.toList attrs)
  where
    toKV :: (Text, Attr.AttributeValue) -> Common.KeyValue
    toKV (k, v) = defMessage
      & CF.key .~ k
      & CF.value .~ toProtoAnyValue v


-- | Convert an SDK 'InstrumentationScope' to the OTLP protobuf type.
toProtoInstrumentationScope :: Attr.InstrumentationScope -> Common.InstrumentationScope
toProtoInstrumentationScope is = defMessage
  & CF.name .~ is.scopeName
  & CF.version .~ maybe "" id is.scopeVersion
  & CF.attributes .~ maybe [] toProtoAttributes is.scopeAttributes


-- | Convert an SDK 'Resource' to the OTLP protobuf Resource type.
toProtoResource :: Resource.Resource -> R.Resource
toProtoResource res = defMessage
  & RF.attributes .~ toProtoAttributes (Resource.getAttributes res)


-------------------------------------------------------------------------------
-- Trace
-------------------------------------------------------------------------------

toProtoSpanKind :: SpanKind -> T.Span'SpanKind
toProtoSpanKind sk = case sk of
  Internal -> T.Span'SPAN_KIND_INTERNAL
  Server   -> T.Span'SPAN_KIND_SERVER
  Client   -> T.Span'SPAN_KIND_CLIENT
  Producer -> T.Span'SPAN_KIND_PRODUCER
  Consumer -> T.Span'SPAN_KIND_CONSUMER


toProtoStatusCode :: StatusCode -> T.Status'StatusCode
toProtoStatusCode sc = case sc of
  Unset -> T.Status'STATUS_CODE_UNSET
  Ok    -> T.Status'STATUS_CODE_OK
  Error -> T.Status'STATUS_CODE_ERROR


toProtoStatus :: SpanStatus -> T.Status
toProtoStatus ss = defMessage
  & TF.code .~ toProtoStatusCode ss.statusCode
  & TF.message .~ maybe "" id ss.statusDescription


toProtoTraceState :: TraceState.TraceState -> Text
toProtoTraceState ts =
  Text.intercalate "," (map (\(k, v) -> k <> "=" <> v) (TraceState.toList ts))


toProtoSpanEvent :: SpanEvent -> T.Span'Event
toProtoSpanEvent ev = defMessage
  & TF.name .~ ev.eventName
  & TF.timeUnixNano .~ unTimestamp ev.eventTimestamp
  & TF.attributes .~ toProtoAttributes ev.eventAttributes
  & TF.droppedAttributesCount .~ fromIntegral @Int @Word32 ev.eventDroppedAttributesCount


toProtoSpanLink :: Link -> T.Span'Link
toProtoSpanLink lnk =
  let lsc = lnk.linkSpanContext
      baseFlags = fromIntegral @Word8 @Word32 (SC.traceFlagsToByte lsc.traceFlags)
      remoteFlags = if SC.isRemote lsc then 0x0300 else 0x0100
  in defMessage
    & TF.traceId .~ SC.traceIdToBytes lsc.traceId
    & TF.spanId .~ SC.spanIdToBytes lsc.spanId
    & TF.traceState .~ toProtoTraceState lsc.traceState
    & TF.attributes .~ toProtoAttributes lnk.linkAttributes
    & TF.droppedAttributesCount .~ fromIntegral @Int @Word32 lnk.linkDroppedAttributesCount
    & TF.flags .~ (baseFlags .|. remoteFlags)


-- | Convert a 'ReadableSpan' to the OTLP Span protobuf type.
toProtoSpan :: ReadableSpan s => s -> T.Span
toProtoSpan s =
  let sc = readSpanContext s
      baseFlags = fromIntegral @Word8 @Word32 (SC.traceFlagsToByte sc.traceFlags)
      remoteFlags = if SC.isRemote sc then 0x0300 else 0x0100
  in defMessage
    & TF.traceId .~ SC.traceIdToBytes sc.traceId
    & TF.spanId .~ SC.spanIdToBytes sc.spanId
    & TF.traceState .~ toProtoTraceState sc.traceState
    & TF.parentSpanId .~ maybe mempty (SC.spanIdToBytes . (.spanId)) (readParentSpanContext s)
    & TF.name .~ readName s
    & TF.kind .~ toProtoSpanKind (readKind s)
    & TF.startTimeUnixNano .~ unTimestamp (readStartTimestamp s)
    & TF.endTimeUnixNano .~ unTimestamp (readEndTimestamp s)
    & TF.attributes .~ toProtoAttributes (readAttributes s)
    & TF.events .~ map toProtoSpanEvent (readEvents s)
    & TF.links .~ map toProtoSpanLink (readLinks s)
    & TF.maybe'status .~ Just (toProtoStatus (readStatus s))
    & TF.droppedAttributesCount .~ fromIntegral @Int @Word32 (readDroppedAttributesCount s)
    & TF.droppedEventsCount .~ fromIntegral @Int @Word32 (readDroppedEventsCount s)
    & TF.droppedLinksCount .~ fromIntegral @Int @Word32 (readDroppedLinksCount s)
    & TF.flags .~ (baseFlags .|. remoteFlags)


-------------------------------------------------------------------------------
-- Span export request
-------------------------------------------------------------------------------

-- | Build an OTLP ExportTraceServiceRequest from a list of readable spans.
spanListToExportRequest :: [SomeReadableSpan] -> CollectorTrace.ExportTraceServiceRequest
spanListToExportRequest spans = defMessage
  & CTF.resourceSpans .~ map toResourceSpans (groupByResource spanAccessors spans)
  where
    toResourceSpans :: (Resource.Resource, [(Attr.InstrumentationScope, [SomeReadableSpan])]) -> T.ResourceSpans
    toResourceSpans (res, scopeGroups) = defMessage
      & TF.maybe'resource .~ Just (toProtoResource res)
      & TF.scopeSpans .~ map toScopeSpans scopeGroups
      & TF.schemaUrl .~ maybe "" id (Resource.getSchemaUrl res)

    toScopeSpans :: (Attr.InstrumentationScope, [SomeReadableSpan]) -> T.ScopeSpans
    toScopeSpans (is, ss) = defMessage
      & TF.maybe'scope .~ Just (toProtoInstrumentationScope is)
      & TF.spans .~ map (\(SomeReadableSpan s) -> toProtoSpan s) ss
      & TF.schemaUrl .~ maybe "" id is.scopeSchemaUrl


data Accessors a = Accessors
  { getRes :: a -> Resource.Resource
  , getScope :: a -> Attr.InstrumentationScope
  }


spanAccessors :: Accessors SomeReadableSpan
spanAccessors = Accessors
  { getRes = \(SomeReadableSpan s) -> readResource s
  , getScope = \(SomeReadableSpan s) -> readInstrumentationScope s
  }


logAccessors :: Accessors SomeReadableLogRecord
logAccessors = Accessors
  { getRes = \(SomeReadableLogRecord r) -> rlrResource r
  , getScope = \(SomeReadableLogRecord r) -> rlrScope r
  }


-- Group items by Resource, then by InstrumentationScope within each Resource.
-- Preserves insertion order within groups.
groupByResource
  :: Accessors a
  -> [a]
  -> [(Resource.Resource, [(Attr.InstrumentationScope, [a])])]
groupByResource acc items = map (\(r, scopeMap) -> (r, groupByScope scopeMap)) resourceGroups
  where
    resourceGroups = accumToList $
      foldl' (\m item -> accumInsert (acc.getRes item) item m) accumEmpty items

    groupByScope scopeItems = accumToList $
      foldl' (\m item -> accumInsert (acc.getScope item) item m) accumEmpty scopeItems


-- Order-preserving grouping accumulator. Uses a list of (key, [value]) pairs
-- to maintain insertion order of first occurrence.
type Accum k v = [(k, [v])]

accumEmpty :: Accum k v
accumEmpty = []

accumInsert :: Eq k => k -> v -> Accum k v -> Accum k v
accumInsert k v [] = [(k, [v])]
accumInsert k v ((k', vs) : rest)
  | k == k'   = (k', vs ++ [v]) : rest
  | otherwise  = (k', vs) : accumInsert k v rest

accumToList :: Accum k v -> [(k, [v])]
accumToList = id


-------------------------------------------------------------------------------
-- Metrics export request
-------------------------------------------------------------------------------

toProtoAggregationTemporality :: AggregationTemporality -> M.AggregationTemporality
toProtoAggregationTemporality at = case at of
  Delta      -> M.AGGREGATION_TEMPORALITY_DELTA
  Cumulative -> M.AGGREGATION_TEMPORALITY_CUMULATIVE


toProtoExemplar :: Metric.Exemplar -> M.Exemplar
toProtoExemplar ex = defMessage
  & MF.filteredAttributes .~ toProtoAttributes ex.exemplarAttributes
  & MF.timeUnixNano .~ unTimestamp ex.exemplarTime
  & MF.maybe'value .~ Just (M.Exemplar'AsDouble ex.exemplarValue)
  & MF.spanId .~ maybe mempty SC.spanIdToBytes ex.exemplarSpanId
  & MF.traceId .~ maybe mempty SC.traceIdToBytes ex.exemplarTraceId


toProtoNumberDataPoint :: NumberDataPoint -> M.NumberDataPoint
toProtoNumberDataPoint ndp = defMessage
  & MF.attributes .~ toProtoAttributes ndp.ndpAttributes
  & MF.startTimeUnixNano .~ unTimestamp ndp.ndpStartTime
  & MF.timeUnixNano .~ unTimestamp ndp.ndpTime
  & MF.maybe'value .~ Just (M.NumberDataPoint'AsDouble ndp.ndpValue)
  & MF.exemplars .~ map toProtoExemplar ndp.ndpExemplars


toProtoHistogramDataPoint :: HistogramDataPoint -> M.HistogramDataPoint
toProtoHistogramDataPoint hdp = defMessage
  & MF.attributes .~ toProtoAttributes hdp.hdpAttributes
  & MF.startTimeUnixNano .~ unTimestamp hdp.hdpStartTime
  & MF.timeUnixNano .~ unTimestamp hdp.hdpTime
  & MF.count .~ hdp.hdpCount
  & MF.maybe'sum .~ hdp.hdpSum
  & MF.bucketCounts .~ hdp.hdpBucketCounts
  & MF.explicitBounds .~ hdp.hdpExplicitBounds
  & MF.maybe'min .~ hdp.hdpMin
  & MF.maybe'max .~ hdp.hdpMax
  & MF.exemplars .~ map toProtoExemplar hdp.hdpExemplars


toProtoExpBuckets :: ExponentialBuckets -> M.ExponentialHistogramDataPoint'Buckets
toProtoExpBuckets eb = defMessage
  & MF.offset .~ fromIntegral @Int @Int32 eb.ebOffset
  & MF.bucketCounts .~ eb.ebBucketCounts


toProtoExpHistogramDataPoint :: ExponentialHistogramDataPoint -> M.ExponentialHistogramDataPoint
toProtoExpHistogramDataPoint ehdp = defMessage
  & MF.attributes .~ toProtoAttributes ehdp.ehdpAttributes
  & MF.startTimeUnixNano .~ unTimestamp ehdp.ehdpStartTime
  & MF.timeUnixNano .~ unTimestamp ehdp.ehdpTime
  & MF.count .~ ehdp.ehdpCount
  & MF.maybe'sum .~ ehdp.ehdpSum
  & MF.scale .~ fromIntegral @Int @Int32 ehdp.ehdpScale
  & MF.zeroCount .~ ehdp.ehdpZeroCount
  & MF.zeroThreshold .~ ehdp.ehdpZeroThreshold
  & MF.maybe'positive .~ Just (toProtoExpBuckets ehdp.ehdpPositive)
  & MF.maybe'negative .~ Just (toProtoExpBuckets ehdp.ehdpNegative)
  & MF.maybe'min .~ ehdp.ehdpMin
  & MF.maybe'max .~ ehdp.ehdpMax
  & MF.exemplars .~ map toProtoExemplar ehdp.ehdpExemplars


toProtoMetricData :: MetricPointData -> M.Metric -> M.Metric
toProtoMetricData mpd m = case mpd of
  SumPointData sd -> m
    & MF.maybe'sum .~ Just (defMessage
      & MF.dataPoints .~ map toProtoNumberDataPoint sd.sumDataPoints
      & MF.aggregationTemporality .~ toProtoAggregationTemporality sd.sumTemporality
      & MF.isMonotonic .~ sd.sumIsMonotonic)
  GaugePointData gd -> m
    & MF.maybe'gauge .~ Just (defMessage
      & MF.dataPoints .~ map toProtoNumberDataPoint gd.gaugeDataPoints)
  HistogramPointData hd -> m
    & MF.maybe'histogram .~ Just (defMessage
      & MF.dataPoints .~ map toProtoHistogramDataPoint hd.histDataPoints
      & MF.aggregationTemporality .~ toProtoAggregationTemporality hd.histTemporality)
  ExponentialHistogramPointData ehd -> m
    & MF.maybe'exponentialHistogram .~ Just (defMessage
      & MF.dataPoints .~ map toProtoExpHistogramDataPoint ehd.expHistDataPoints
      & MF.aggregationTemporality .~ toProtoAggregationTemporality ehd.expHistTemporality)


toProtoMetric :: Metric.Metric -> M.Metric
toProtoMetric met =
  toProtoMetricData met.metricPointData $
    defMessage
      & MF.name .~ met.metricName
      & MF.description .~ met.metricDescription
      & MF.unit .~ met.metricUnit


-- | Build an OTLP ExportMetricsServiceRequest from SDK metric data.
metricDataToExportRequest :: MetricData -> CollectorMetrics.ExportMetricsServiceRequest
metricDataToExportRequest md = defMessage
  & CMF.resourceMetrics .~ [rm]
  where
    rm :: M.ResourceMetrics
    rm = defMessage
      & MF.maybe'resource .~ Just (toProtoResource md.mdResource)
      & MF.scopeMetrics .~ map toScopeMetrics md.mdScopeMetrics
      & MF.schemaUrl .~ maybe "" id (Resource.getSchemaUrl md.mdResource)

    toScopeMetrics :: ScopeMetrics -> M.ScopeMetrics
    toScopeMetrics sm = defMessage
      & MF.maybe'scope .~ Just (toProtoInstrumentationScope sm.smScope)
      & MF.metrics .~ map toProtoMetric sm.smMetrics
      & MF.schemaUrl .~ maybe "" id sm.smScope.scopeSchemaUrl


-------------------------------------------------------------------------------
-- Logs export request
-------------------------------------------------------------------------------

toProtoLogBody :: LogBody -> Common.AnyValue
toProtoLogBody lb = case lb of
  LogBodyString t  -> defMessage & CF.stringValue .~ t
  LogBodyBool b    -> defMessage & CF.boolValue .~ b
  LogBodyInt64 i   -> defMessage & CF.intValue .~ i
  LogBodyFloat64 d -> defMessage & CF.doubleValue .~ d
  LogBodyBytes bs  -> defMessage & CF.bytesValue .~ bs
  LogBodyList xs   -> defMessage & CF.arrayValue .~ (defMessage & CF.values .~ map toProtoLogBody xs)
  LogBodyMap m     -> defMessage & CF.kvlistValue .~ (defMessage & CF.values .~ map toLogKV (Map.toList m))
  where
    toLogKV :: (Text, LogBody) -> Common.KeyValue
    toLogKV (k, v) = defMessage
      & CF.key .~ k
      & CF.value .~ toProtoLogBody v


toProtoSeverityNumber :: SeverityNumber -> L.SeverityNumber
toProtoSeverityNumber sn = toEnum (severityNumberValue sn)


toProtoLogRecord :: ReadableLogRecord r => r -> L.LogRecord
toProtoLogRecord r = defMessage
  & LF.timeUnixNano .~ maybe 0 unTimestamp (rlrTimestamp r)
  & LF.observedTimeUnixNano .~ unTimestamp (rlrObservedTimestamp r)
  & LF.severityNumber .~ maybe L.SEVERITY_NUMBER_UNSPECIFIED toProtoSeverityNumber (rlrSeverityNumber r)
  & LF.severityText .~ maybe "" id (rlrSeverityText r)
  & LF.maybe'body .~ fmap toProtoLogBody (rlrBody r)
  & LF.attributes .~ toProtoAttributes (rlrAttributes r)
  & LF.droppedAttributesCount .~ fromIntegral @Int @Word32 (rlrDroppedAttributes r)
  & LF.traceId .~ maybe mempty (SC.traceIdToBytes . (.traceId)) (rlrSpanContext r)
  & LF.spanId .~ maybe mempty (SC.spanIdToBytes . (.spanId)) (rlrSpanContext r)
  & LF.flags .~ maybe 0 (\lsc ->
      let baseFlags = fromIntegral @Word8 @Word32 (SC.traceFlagsToByte lsc.traceFlags)
          remoteFlags = if SC.isRemote lsc then 0x0300 else 0x0100
      in baseFlags .|. remoteFlags) (rlrSpanContext r)


-- | Build an OTLP ExportLogsServiceRequest from a list of readable log records.
logListToExportRequest :: [SomeReadableLogRecord] -> CollectorLogs.ExportLogsServiceRequest
logListToExportRequest records = defMessage
  & CLF.resourceLogs .~ map toResourceLogs (groupByResource logAccessors records)
  where
    toResourceLogs :: (Resource.Resource, [(Attr.InstrumentationScope, [SomeReadableLogRecord])]) -> L.ResourceLogs
    toResourceLogs (res, scopeGroups) = defMessage
      & LF.maybe'resource .~ Just (toProtoResource res)
      & LF.scopeLogs .~ map toScopeLogs scopeGroups
      & LF.schemaUrl .~ maybe "" id (Resource.getSchemaUrl res)

    toScopeLogs :: (Attr.InstrumentationScope, [SomeReadableLogRecord]) -> L.ScopeLogs
    toScopeLogs (is, recs) = defMessage
      & LF.maybe'scope .~ Just (toProtoInstrumentationScope is)
      & LF.logRecords .~ map (\(SomeReadableLogRecord r) -> toProtoLogRecord r) recs
      & LF.schemaUrl .~ maybe "" id is.scopeSchemaUrl


-------------------------------------------------------------------------------
-- Response parsing
-------------------------------------------------------------------------------

-- | Decode the OTLP trace export response and return the export result.
parseTraceExportResult :: ByteString -> IO ExportResult
parseTraceExportResult bs = case decodeMessage bs of
  Left _     -> pure ExportFailure
  Right (resp :: CollectorTrace.ExportTraceServiceResponse) -> do
    case resp ^. CTF.maybe'partialSuccess of
      Just ps | ps ^. CTF.rejectedSpans > 0 ->
        hPutStrLn stderr $ "[OTel] OTLP partial success: " <>
          show (ps ^. CTF.rejectedSpans) <> " spans rejected by server"
      _ -> pure ()
    pure ExportSuccess


-- | Decode the OTLP metrics export response and return the export result.
parseMetricsExportResult :: ByteString -> IO ExportResult
parseMetricsExportResult bs = case decodeMessage bs of
  Left _     -> pure ExportFailure
  Right (resp :: CollectorMetrics.ExportMetricsServiceResponse) -> do
    case resp ^. CMF.maybe'partialSuccess of
      Just ps | ps ^. CMF.rejectedDataPoints > 0 ->
        hPutStrLn stderr $ "[OTel] OTLP partial success: " <>
          show (ps ^. CMF.rejectedDataPoints) <> " data points rejected by server"
      _ -> pure ()
    pure ExportSuccess


-- | Decode the OTLP logs export response and return the export result.
parseLogsExportResult :: ByteString -> IO ExportResult
parseLogsExportResult bs = case decodeMessage bs of
  Left _     -> pure ExportFailure
  Right (resp :: CollectorLogs.ExportLogsServiceResponse) -> do
    case resp ^. CLF.maybe'partialSuccess of
      Just ps | ps ^. CLF.rejectedLogRecords > 0 ->
        hPutStrLn stderr $ "[OTel] OTLP partial success: " <>
          show (ps ^. CLF.rejectedLogRecords) <> " log records rejected by server"
      _ -> pure ()
    pure ExportSuccess
