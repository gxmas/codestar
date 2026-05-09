-- | Console exporters that print human-readable summaries of spans, metrics,
-- and log records to an output handle (stdout by default). Intended for
-- development and debugging.
module OTel.Exporter.Console
  ( -- * Span exporter
    ConsoleSpanExporter
  , newConsoleSpanExporter
  , newConsoleSpanExporterWith

    -- * Metric exporter
  , ConsoleMetricExporter
  , newConsoleMetricExporter
  , newConsoleMetricExporterWith

    -- * Log record exporter
  , ConsoleLogRecordExporter
  , newConsoleLogRecordExporter
  , newConsoleLogRecordExporterWith
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import System.IO (Handle, hPutStrLn, stdout)

import OTel.Attribute (AttributeValue (..), InstrumentationScope (..), size, toList)
import OTel.Log (LogBody (..))
import OTel.SDK.Export (ExportResult (..))
import OTel.SDK.Metric.Export
  ( AggregationTemporality (..)
  , ExponentialHistogramData (..)
  , GaugeData (..)
  , HistogramData (..)
  , Metric (..)
  , MetricData (..)
  , MetricExporter (..)
  , MetricPointData (..)
  , ScopeMetrics (..)
  , SumData (..)
  )
import OTel.SDK.Metric.Reader (defaultAggregationFor)
import OTel.SDK.Log.Export
  ( LogRecordExporter (..)
  , ReadableLogRecord (..)
  , SomeReadableLogRecord (..)
  )
import OTel.SDK.Resource (getAttributes)
import OTel.SDK.Trace.Export (ReadableSpan (..), SomeReadableSpan (..), SpanExporter (..))
import OTel.Timestamp (Timestamp (..))
import OTel.Trace (SpanKind (..), SpanStatus (..), StatusCode (..))
import OTel.Trace.SpanContext (SpanContext (..), spanIdToHex, traceIdToHex)


-------------------------------------------------------------------------------
-- Span exporter
-------------------------------------------------------------------------------

-- | A span exporter that writes a human-readable text representation of each
-- span to the configured 'Handle'.
data ConsoleSpanExporter = ConsoleSpanExporter !Handle


-- | Create a 'ConsoleSpanExporter' that writes to stdout.
newConsoleSpanExporter :: IO ConsoleSpanExporter
newConsoleSpanExporter = pure (ConsoleSpanExporter stdout)


-- | Create a 'ConsoleSpanExporter' that writes to the given 'Handle'.
newConsoleSpanExporterWith :: Handle -> IO ConsoleSpanExporter
newConsoleSpanExporterWith h = pure (ConsoleSpanExporter h)


instance SpanExporter ConsoleSpanExporter where
  exportSpans (ConsoleSpanExporter h) spans = do
    mapM_ (printSpan h) spans
    pure ExportSuccess
  shutdownExporter _ = pure (Right ())
  forceFlushExporter _ _ = pure (Right ())


printSpan :: Handle -> SomeReadableSpan -> IO ()
printSpan h (SomeReadableSpan s) = do
  let ctx = readSpanContext s
      name = readName s
      kind = readKind s
      status = readStatus s
      startTs = readStartTimestamp s
      endTs = readEndTimestamp s
      attrs = toList (readAttributes s)
      events = readEvents s
      links = readLinks s

  hPutStrLn h $ "Span: \"" <> Text.unpack name <> "\""
  hPutStrLn h $ "  TraceId: " <> Text.unpack (traceIdToHex ctx.traceId)
  hPutStrLn h $ "  SpanId:  " <> Text.unpack (spanIdToHex ctx.spanId)
  hPutStrLn h $ "  Kind:    " <> showKind kind
  hPutStrLn h $ "  Status:  " <> showStatus status
  hPutStrLn h $ "  Start:   " <> show (unTimestamp startTs)
  hPutStrLn h $ "  End:     " <> show (unTimestamp endTs)
  case attrs of
    [] -> pure ()
    _ -> do
      hPutStrLn h "  Attributes:"
      mapM_ (printAttribute h) attrs
  hPutStrLn h $ "  Events: " <> show (length events)
  hPutStrLn h $ "  Links: " <> show (length links)


printAttribute :: Handle -> (Text, AttributeValue) -> IO ()
printAttribute h (k, v) =
  hPutStrLn h $ "    " <> Text.unpack k <> " = " <> showAttrValue v


showKind :: SpanKind -> String
showKind Internal = "Internal"
showKind Server = "Server"
showKind Client = "Client"
showKind Producer = "Producer"
showKind Consumer = "Consumer"


showStatus :: SpanStatus -> String
showStatus (SpanStatus Unset _) = "Unset"
showStatus (SpanStatus Ok _) = "Ok"
showStatus (SpanStatus Error Nothing) = "Error"
showStatus (SpanStatus Error (Just desc)) = "Error: " <> Text.unpack desc


showAttrValue :: AttributeValue -> String
showAttrValue (StringValue t) = "\"" <> Text.unpack t <> "\""
showAttrValue (BoolValue b) = show b
showAttrValue (Int64Value n) = show n
showAttrValue (Float64Value d) = show d
showAttrValue (StringArrayValue vs) =
  "[" <> commaSep (fmap (\t -> "\"" <> Text.unpack t <> "\"") (Vector.toList vs)) <> "]"
showAttrValue (BoolArrayValue vs) =
  "[" <> commaSep (fmap show (Vector.toList vs)) <> "]"
showAttrValue (Int64ArrayValue vs) =
  "[" <> commaSep (fmap show (Vector.toList vs)) <> "]"
showAttrValue (Float64ArrayValue vs) =
  "[" <> commaSep (fmap show (Vector.toList vs)) <> "]"


commaSep :: [String] -> String
commaSep [] = ""
commaSep [x] = x
commaSep (x : xs) = x <> ", " <> commaSep xs


-------------------------------------------------------------------------------
-- Metric exporter
-------------------------------------------------------------------------------

-- | A metric exporter that writes a human-readable text summary of each
-- MetricData batch to the configured 'Handle'.
data ConsoleMetricExporter = ConsoleMetricExporter !Handle


-- | Create a 'ConsoleMetricExporter' that writes to stdout.
newConsoleMetricExporter :: IO ConsoleMetricExporter
newConsoleMetricExporter = pure (ConsoleMetricExporter stdout)


-- | Create a 'ConsoleMetricExporter' that writes to the given 'Handle'.
newConsoleMetricExporterWith :: Handle -> IO ConsoleMetricExporter
newConsoleMetricExporterWith h = pure (ConsoleMetricExporter h)


instance MetricExporter ConsoleMetricExporter where
  exportMetrics (ConsoleMetricExporter h) md = do
    printMetricData h md
    pure ExportSuccess
  shutdownMetricExporter _ = pure (Right ())
  forceFlushMetricExporter _ _ = pure (Right ())
  exporterTemporality _ _ = Cumulative
  exporterDefaultAggregation _ kind = defaultAggregationFor kind


printMetricData :: Handle -> MetricData -> IO ()
printMetricData h md = do
  hPutStrLn h "Metrics:"
  let resAttrs = getAttributes (mdResource md)
  hPutStrLn h $ "  Resource: " <> show (size resAttrs) <> " attributes"
  mapM_ (printScopeMetrics h) (mdScopeMetrics md)


printScopeMetrics :: Handle -> ScopeMetrics -> IO ()
printScopeMetrics h sm = do
  let scope = smScope sm
      ver = maybe "" (" " <>) (scopeVersion scope)
  hPutStrLn h $ "  Scope: " <> Text.unpack (scopeName scope) <> Text.unpack ver
  mapM_ (printMetric h) (smMetrics sm)


printMetric :: Handle -> Metric -> IO ()
printMetric h m = do
  let unitStr = if Text.null (metricUnit m) then "" else " [" <> Text.unpack (metricUnit m) <> "]"
  hPutStrLn h $ "    Metric: \"" <> Text.unpack (metricName m) <> "\"" <> unitStr
  hPutStrLn h $ "      " <> showPointData (metricPointData m)


showPointData :: MetricPointData -> String
showPointData (SumPointData sd) =
  "Sum (monotonic=" <> show (sumIsMonotonic sd) <> ", " <> showTemporality (sumTemporality sd)
    <> "): " <> show (length (sumDataPoints sd)) <> " data points"
showPointData (GaugePointData gd) =
  "Gauge: " <> show (length (gaugeDataPoints gd)) <> " data points"
showPointData (HistogramPointData hd) =
  "Histogram (" <> showTemporality (histTemporality hd)
    <> "): " <> show (length (histDataPoints hd)) <> " data points"
showPointData (ExponentialHistogramPointData ehd) =
  "ExponentialHistogram (" <> showTemporality (expHistTemporality ehd)
    <> "): " <> show (length (expHistDataPoints ehd)) <> " data points"


showTemporality :: AggregationTemporality -> String
showTemporality Delta = "Delta"
showTemporality Cumulative = "Cumulative"


-------------------------------------------------------------------------------
-- Log record exporter
-------------------------------------------------------------------------------

-- | A log record exporter that writes a human-readable text representation
-- of each log record to the configured 'Handle'.
data ConsoleLogRecordExporter = ConsoleLogRecordExporter !Handle


-- | Create a 'ConsoleLogRecordExporter' that writes to stdout.
newConsoleLogRecordExporter :: IO ConsoleLogRecordExporter
newConsoleLogRecordExporter = pure (ConsoleLogRecordExporter stdout)


-- | Create a 'ConsoleLogRecordExporter' that writes to the given 'Handle'.
newConsoleLogRecordExporterWith :: Handle -> IO ConsoleLogRecordExporter
newConsoleLogRecordExporterWith h = pure (ConsoleLogRecordExporter h)


instance LogRecordExporter ConsoleLogRecordExporter where
  exportLogRecords (ConsoleLogRecordExporter h) records = do
    mapM_ (printLogRecord h) records
    pure ExportSuccess
  shutdownLogExporter _ = pure (Right ())
  forceFlushLogExporter _ _ = pure (Right ())


printLogRecord :: Handle -> SomeReadableLogRecord -> IO ()
printLogRecord h (SomeReadableLogRecord r) = do
  hPutStrLn h "LogRecord:"
  case rlrSeverityNumber r of
    Nothing -> hPutStrLn h "  Severity: <none>"
    Just sev -> case rlrSeverityText r of
      Nothing  -> hPutStrLn h $ "  Severity: " <> show sev
      Just txt -> hPutStrLn h $ "  Severity: " <> show sev <> " (" <> Text.unpack txt <> ")"
  case rlrBody r of
    Nothing   -> hPutStrLn h "  Body: <none>"
    Just body -> hPutStrLn h $ "  Body: " <> showBody body
  case rlrSpanContext r of
    Nothing  -> pure ()
    Just ctx -> do
      hPutStrLn h $ "  TraceId: " <> Text.unpack (traceIdToHex ctx.traceId)
      hPutStrLn h $ "  SpanId:  " <> Text.unpack (spanIdToHex ctx.spanId)
  hPutStrLn h $ "  Attributes: " <> show (size (rlrAttributes r))


showBody :: LogBody -> String
showBody (LogBodyString t) = Text.unpack t
showBody (LogBodyBool b) = show b
showBody (LogBodyInt64 n) = show n
showBody (LogBodyFloat64 d) = show d
showBody (LogBodyBytes _bs) = "<bytes>"
showBody (LogBodyList xs) = "[" <> commaSep (fmap showBody xs) <> "]"
showBody (LogBodyMap m) = "{" <> commaSep (fmap showEntry (Map.toList m)) <> "}"
  where
    showEntry (k, v) = Text.unpack k <> ": " <> showBody v
