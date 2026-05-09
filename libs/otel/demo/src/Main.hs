-- | Walking skeleton demo: full trace pipeline end-to-end.
--
-- Run with: cabal run demo
--
-- Simulates realistic scenarios demonstrating the OTel trace pipeline:
--   1. Successful HTTP request with database child span
--   2. Failed request — error status and recorded exception
--   3. Four-level call chain (HTTP → service → cache → DB)
--   4. Background job — root span with no parent
--   5. SpanLimits — attribute and event overflow
--   6. Span links — producer/consumer messaging (two independent traces linked)
--   7. Concurrent requests — thread safety of SdkTracerProvider
--   8. Dynamic span naming — updateName after route resolution
--   9. Provider shutdown — spans become no-ops after shutdown
--  10. Rich attribute types — all eight AttributeValue variants
--  11. Multiple exporters fan-out — same span to three destinations
--  12. Status override rules — the Ok-is-terminal state machine
--  13. Explicit timestamps — back-dating a span
--  14. Custom sampler — 1-in-N sampling with decision visible in flags
--  15. recordException details — inspecting the generated span event
--  16. Cross-signal correlation — traces + metrics + logs for one request
--  17. W3C Trace Context propagation — simulated service boundary
--  18. W3C Baggage — passing tenant context across boundaries
--  19. Metrics — request counter and latency histogram
--  20. Observable gauge — queue depth reported at collection time
--  21. Metric Views — attribute filtering and aggregation override
--  22. Resource Detectors — auto-detected environment attributes
--  23. Sampling strategies — TraceIdRatio and ParentBased
--  24. Semantic Conventions — standard attribute key strings
--  25. Log–trace correlation — structured log carries span context
--  26. Full-stack from YAML — all providers from config
module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, toException)
import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import OTel.SDK.Trace.Sampler
  ( Sampler (..), SamplingDecision (..), SamplingResult (..), SomeSampler (..)
  , TraceIdRatioBasedSampler (..), defaultParentBasedSampler, AlwaysOnSampler (..)
  )
import OTel.Timestamp (Timestamp (..), fromNanos)
import OTel.Trace.SpanContext (isSampled)

import OTel.Attribute
  ( Attributes, AttributeValue (..), InstrumentationScope (..), Key
  , emptyAttributes, fromList, toList
  )
import OTel.Baggage
  ( getBaggage, setBaggage, setValue, emptyBaggage, baggageToList
  , BaggageEntry (..)
  )
import OTel.Context (root)
import OTel.Exporter.Console (newConsoleSpanExporter)
import OTel.Exporter.InMemory
  ( InMemorySpanExporter, getFinishedSpans, newInMemorySpanExporter, reset
  , newInMemoryLogRecordExporter, getFinishedLogRecords
  )
import OTel.Log
  ( LogRecord (..), LogBody (..), SeverityNumber (..), Logger (..)
  , LoggerProvider (..), defaultLogRecord, severityNumberValue
  )
import OTel.Metric
  ( MeterProvider (..), Meter (..), Counter (..), Histogram (..)
  , ObservableResult (..)
  , SomeCounter (..), SomeHistogram (..), SomeObservableGauge (..)
  , SomeObservableResult (..)
  )
import OTel.Propagation (TextMapPropagator (..))
import OTel.Propagation.W3C (W3CTraceContextPropagator (..), W3CBaggagePropagator (..))
import OTel.SDK.Config
  ( parseYaml, createTracerProvider, createLoggerProvider, createPropagator )
import OTel.SDK.Log
  ( SdkLoggerProviderConfig (..), defaultSdkLoggerProviderConfig, newSdkLoggerProvider
  , SomeLogRecordProcessor (..), SomeLogRecordExporter (..)
  , ReadableLogRecord (..), SomeReadableLogRecord (..)
  )
import OTel.SDK.Log.Processor (newSimpleLogRecordProcessor)
import OTel.SDK.Metric
  ( SdkMeterProviderConfig (..), defaultSdkMeterProviderConfig, newSdkMeterProvider
  , SomeMetricReader (..), MetricData (..), ScopeMetrics (..), Metric (..)
  , MetricPointData (..), SumData (..), HistogramData (..), GaugeData (..)
  , NumberDataPoint (..), HistogramDataPoint (..)
  )
import OTel.SDK.Metric.Reader (newNoOpMetricReader, MetricReader (..))
import OTel.SDK.Metric.View (View (..), defaultView)
import OTel.SDK.Resource qualified as Resource
import OTel.SDK.Resource.Detectors
  ( defaultResource, ProcessResourceDetector (..)
  , HostResourceDetector (..), OsResourceDetector (..)
  )
import OTel.SDK.Resource (SomeResourceDetector (..), getAttributes)
import OTel.SDK.Trace
  ( SdkTracerProviderConfig (..), SpanLimits (..), defaultSdkTracerProviderConfig
  , defaultSpanLimits, newSdkTracerProvider, shutdown
  )
import OTel.SDK.Trace.Export
  ( Link (..), ReadableSpan (..), SomeReadableSpan (..), SomeSpanExporter (..)
  , SpanEvent (..)
  )
import OTel.SDK.Trace.Processor
  ( SomeSpanProcessor (..), SpanProcessor (..), newSimpleSpanProcessor )
import OTel.SemConv.HTTP qualified as SemHTTP
import OTel.SemConv.DB qualified as SemDB
import OTel.Trace
  ( Span (..), SpanConfig (..), SpanKind (..)
  , SpanStatus (..), StatusCode (..)
  , Tracer (..), TracerProvider (..), defaultSpanConfig, setSpanInContext
  , getSpanFromContext
  )
import OTel.Trace.SpanContext
  ( SpanContext (..), isValid, spanIdToHex, traceIdToHex )
import OTel.Trace.TraceState qualified
import Data.Text (Text, unpack)
import Data.Text qualified as Text

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Prelude hiding (lookup)


main :: IO ()
main = do
  putStrLn "╔══════════════════════════════════════════════════╗"
  putStrLn "║     OTel Haskell — Walking Skeleton Demo         ║"
  putStrLn "╚══════════════════════════════════════════════════╝"
  putStrLn ""

  let resource = Resource.create
        [ ("service.name",    StringValue "order-service")
        , ("service.version", StringValue "2.1.0")
        , ("deployment.environment.name", StringValue "demo")
        ] Nothing

  consoleExporter <- newConsoleSpanExporter
  memoryExporter  <- newInMemorySpanExporter
  consoleProc     <- newSimpleSpanProcessor (SomeSpanExporter consoleExporter)
  memoryProc      <- newSimpleSpanProcessor (SomeSpanExporter memoryExporter)

  provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerResource   = resource
    , providerProcessors = [SomeSpanProcessor consoleProc, SomeSpanProcessor memoryProc]
    }

  let scope = InstrumentationScope "order-service" (Just "2.1.0") Nothing Nothing
  tracer <- getTracer provider scope

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 1: Successful HTTP request → DB query
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 1: Successful HTTP GET with database child span"

  httpSpan <- startSpan tracer "GET /api/orders" root
    defaultSpanConfig { spanKind = Server }
  setAttribute httpSpan "http.request.method" (StringValue "GET")
  setAttribute httpSpan "http.route"          (StringValue "/api/orders")
  setAttribute httpSpan "user.id"             (StringValue "usr-42")
  addEvent httpSpan "request.received" emptyAttributes Nothing

  let dbCtx = setSpanInContext httpSpan root
  dbSpan <- startSpan tracer "SELECT orders" dbCtx
    defaultSpanConfig { spanKind = Client }
  setAttribute dbSpan "db.system"        (StringValue "postgresql")
  setAttribute dbSpan "db.query.text"    (StringValue "SELECT * FROM orders WHERE user_id = $1")
  setAttribute dbSpan "db.response.rows" (Int64Value 5)
  setStatus dbSpan Ok Nothing
  end dbSpan Nothing

  addEvent httpSpan "response.sent"
    (fromList [("http.response.status_code", Int64Value 200)]) Nothing
  setAttribute httpSpan "http.response.status_code" (Int64Value 200)
  setStatus httpSpan Ok Nothing
  end httpSpan Nothing

  verify memoryExporter 2 "Scenario 1"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 2: Failed request — error status + recorded exception
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 2: Failed POST — error with recorded exception"
  reset memoryExporter

  errSpan <- startSpan tracer "POST /api/orders" root
    defaultSpanConfig { spanKind = Server }
  setAttribute errSpan "http.request.method" (StringValue "POST")
  setAttribute errSpan "http.route"          (StringValue "/api/orders")
  addEvent errSpan "request.received" emptyAttributes Nothing

  let upCtx = setSpanInContext errSpan root
  upSpan <- startSpan tracer "POST inventory-service/reserve" upCtx
    defaultSpanConfig { spanKind = Client }
  setAttribute upSpan "server.address" (StringValue "inventory-service")
  setAttribute upSpan "server.port"    (Int64Value 8080)

  let exc = toException (userError "inventory service unavailable") :: SomeException
  recordException upSpan exc emptyAttributes
  setStatus upSpan Error (Just "upstream inventory service failed")
  end upSpan Nothing

  setAttribute errSpan "http.response.status_code" (Int64Value 503)
  setStatus errSpan Error (Just "inventory service unavailable")
  end errSpan Nothing

  verify memoryExporter 2 "Scenario 2"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 3: Four-level call chain
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 3: Four-level call chain (HTTP → service → cache → DB)"
  reset memoryExporter

  l1 <- startSpan tracer "GET /api/products/123" root
    defaultSpanConfig { spanKind = Server }
  setAttribute l1 "http.request.method" (StringValue "GET")
  setAttribute l1 "http.route"          (StringValue "/api/products/{id}")

  let l2ctx = setSpanInContext l1 root
  l2 <- startSpan tracer "ProductService.getById" l2ctx
    defaultSpanConfig { spanKind = Internal }
  setAttribute l2 "product.id" (StringValue "prod-123")

  let l3ctx = setSpanInContext l2 l2ctx
  l3 <- startSpan tracer "cache.get" l3ctx
    defaultSpanConfig { spanKind = Client }
  setAttribute l3 "cache.system" (StringValue "redis")
  setAttribute l3 "cache.key"    (StringValue "product:prod-123")
  addEvent l3 "cache.miss" emptyAttributes Nothing
  setStatus l3 Ok Nothing
  end l3 Nothing

  l4 <- startSpan tracer "SELECT products" l3ctx
    defaultSpanConfig { spanKind = Client }
  setAttribute l4 "db.system"     (StringValue "postgresql")
  setAttribute l4 "db.query.text" (StringValue "SELECT * FROM products WHERE id = $1")
  setStatus l4 Ok Nothing
  end l4 Nothing

  setStatus l2 Ok Nothing
  end l2 Nothing
  setAttribute l1 "http.response.status_code" (Int64Value 200)
  setStatus l1 Ok Nothing
  end l1 Nothing

  spans3 <- getFinishedSpans memoryExporter
  let traceIds3 = map (\(SomeReadableSpan s) -> traceIdToHex (traceId (readSpanContext s))) spans3
      allSame   = case traceIds3 of { [] -> True; (t:ts) -> all (== t) ts }
  putStrLn $ "  All 4 spans on same trace: " <> show allSame
  verify memoryExporter 4 "Scenario 3"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 4: Background job — root span, no parent
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 4: Background job — root span with no parent"
  reset memoryExporter

  jobSpan <- startSpan tracer "order.cleanup.job" root
    defaultSpanConfig { spanKind = Internal, spanNoParent = True }
  setAttribute jobSpan "job.name"       (StringValue "cleanup-expired-orders")
  setAttribute jobSpan "job.batch_size" (Int64Value 100)
  addEvent jobSpan "job.started"   emptyAttributes Nothing
  addEvent jobSpan "job.completed" (fromList [("orders.deleted", Int64Value 7)]) Nothing
  setStatus jobSpan Ok Nothing
  end jobSpan Nothing

  [SomeReadableSpan js] <- getFinishedSpans memoryExporter
  putStrLn $ "  Parent span context: " <>
    maybe "(none — root span)" (const "(has parent)") (readParentSpanContext js)
  verify memoryExporter 1 "Scenario 4"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 5: SpanLimits — attribute and event overflow
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 5: SpanLimits — overflow drops and counts"

  -- Use a dedicated exporter so shutdown doesn't affect the shared memoryExporter.
  limitsMemory <- newInMemorySpanExporter
  limitsProc   <- newSimpleSpanProcessor (SomeSpanExporter limitsMemory)
  tightProvider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerResource   = resource
    , providerProcessors = [SomeSpanProcessor limitsProc]
    , providerSpanLimits = defaultSpanLimits { maxAttributes = 3, maxEvents = 2 }
    }
  tightTracer <- getTracer tightProvider
    (InstrumentationScope "limits-demo" (Just "1.0") Nothing Nothing)

  limSpan <- startSpan tightTracer "overloaded-span" root defaultSpanConfig
  setAttribute limSpan "attr.1" (StringValue "a")
  setAttribute limSpan "attr.2" (StringValue "b")
  setAttribute limSpan "attr.3" (StringValue "c")
  setAttribute limSpan "attr.4" (StringValue "d")   -- dropped
  setAttribute limSpan "attr.5" (StringValue "e")   -- dropped
  setAttribute limSpan "attr.6" (StringValue "f")   -- dropped
  addEvent limSpan "evt.1" emptyAttributes Nothing
  addEvent limSpan "evt.2" emptyAttributes Nothing
  addEvent limSpan "evt.3" emptyAttributes Nothing   -- dropped
  addEvent limSpan "evt.4" emptyAttributes Nothing   -- dropped
  setStatus limSpan Ok Nothing
  end limSpan Nothing

  _ <- shutdown tightProvider
  limSpans <- getFinishedSpans limitsMemory
  case limSpans of
    [SomeReadableSpan ls] -> do
      putStrLn $ "  Attributes stored:  3 (limit)"
      putStrLn $ "  Attributes dropped: " <> show (readDroppedAttributesCount ls)
      putStrLn $ "  Events stored:      " <> show (length (readEvents ls))
      putStrLn $ "  Events dropped:     " <> show (readDroppedEventsCount ls)
    _ -> putStrLn "  (unexpected span count)"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 6: Span links — producer/consumer (two separate traces)
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 6: Span links — producer/consumer across two traces"
  reset memoryExporter

  -- Producer: publishes a message (its own trace)
  producerSpan <- startSpan tracer "kafka.publish" root
    defaultSpanConfig { spanKind = Producer }
  setAttribute producerSpan "messaging.system"      (StringValue "kafka")
  setAttribute producerSpan "messaging.destination" (StringValue "orders.created")
  setAttribute producerSpan "messaging.message_id"  (StringValue "msg-abc-123")
  setStatus producerSpan Ok Nothing
  producerSc <- getSpanContext producerSpan
  end producerSpan Nothing

  -- Consumer: processes the message in a completely separate trace,
  -- but links back to the producer span.
  consumerSpan <- startSpan tracer "kafka.consume" root
    defaultSpanConfig
      { spanKind  = Consumer
      , spanLinks = [(producerSc, fromList [("messaging.link.reason", StringValue "follows-from")])]
      }
  setAttribute consumerSpan "messaging.system"      (StringValue "kafka")
  setAttribute consumerSpan "messaging.destination" (StringValue "orders.created")
  setAttribute consumerSpan "messaging.message_id"  (StringValue "msg-abc-123")
  addEvent consumerSpan "message.processed" emptyAttributes Nothing
  setStatus consumerSpan Ok Nothing
  _consumerSc <- getSpanContext consumerSpan
  end consumerSpan Nothing

  spans6 <- getFinishedSpans memoryExporter
  case spans6 of
    [SomeReadableSpan prod, SomeReadableSpan cons] -> do
      let producerTid = traceIdToHex (traceId (readSpanContext prod))
          consumerTid = traceIdToHex (traceId (readSpanContext cons))
          links       = readLinks cons
      putStrLn $ "  Producer traceId: " <> unpack (Text.take 16 producerTid) <> "…"
      putStrLn $ "  Consumer traceId: " <> unpack (Text.take 16 consumerTid) <> "…"
      putStrLn $ "  Different traces: " <> show (producerTid /= consumerTid)
      putStrLn $ "  Consumer links to producer: " <> case links of
        [Link sc _ _] -> show (traceIdToHex (traceId sc) == producerTid)
        _             -> "false"
    _ -> putStrLn "  Unexpected number of spans"
  verify memoryExporter 2 "Scenario 6"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 7: Concurrent requests — thread safety
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 7: Concurrent requests — SdkTracerProvider is thread-safe"
  reset memoryExporter  -- start fresh

  -- Spawn 10 concurrent request handlers, each creating a parent + child span.
  results <- mapConcurrently (\i -> do
      parent <- startSpan tracer (Text.pack ("GET /api/item/" <> show (i :: Int))) root
        defaultSpanConfig { spanKind = Server }
      setAttribute parent "http.request.method" (StringValue "GET")
      let childCtx2 = setSpanInContext parent root
      child <- startSpan tracer "db.lookup" childCtx2
        defaultSpanConfig { spanKind = Client }
      setAttribute child "item.id" (Int64Value (fromIntegral i))
      setStatus child Ok Nothing
      end child Nothing
      setStatus parent Ok Nothing
      end parent Nothing
      parentSc2 <- getSpanContext parent
      childSc2  <- getSpanContext child
      pure (parentSc2, childSc2)
    ) [1..10]

  spans7 <- getFinishedSpans memoryExporter
  let uniqueTraces = nub (map (\(SomeReadableSpan s) -> traceIdToHex (traceId (readSpanContext s))) spans7 :: [Text])
      parentChildSame = all (\(psc, csc) -> traceId psc == traceId csc) results
  putStrLn $ "  Concurrent handlers: 10"
  putStrLn $ "  Total spans: " <> show (length spans7) <> " (10 parent + 10 child)"
  putStrLn $ "  Unique traces: " <> show (length uniqueTraces) <> " (one per handler)"
  putStrLn $ "  All parent-child pairs share traceId: " <> show parentChildSame
  verify memoryExporter 20 "Scenario 7"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 8: Dynamic span naming — updateName after route resolution
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 8: Dynamic span naming — updateName after route resolution"
  reset memoryExporter  -- start fresh

  -- The raw URL is used as the span name initially.
  dynSpan <- startSpan tracer "GET /api/orders/ord-9f3a1b" root
    defaultSpanConfig { spanKind = Server }
  setAttribute dynSpan "http.request.method" (StringValue "GET")
  setAttribute dynSpan "http.url"            (StringValue "/api/orders/ord-9f3a1b")
  addEvent dynSpan "route.resolved"
    (fromList [("route", StringValue "/api/orders/{id}")]) Nothing

  -- Once the framework resolves the route template, rename the span.
  updateName dynSpan "GET /api/orders/{id}"
  setAttribute dynSpan "http.route"                  (StringValue "/api/orders/{id}")
  setAttribute dynSpan "order.id"                    (StringValue "ord-9f3a1b")
  setAttribute dynSpan "http.response.status_code"   (Int64Value 200)
  setStatus dynSpan Ok Nothing
  end dynSpan Nothing

  dynSpans <- getFinishedSpans memoryExporter
  case dynSpans of
    [SomeReadableSpan ds] -> do
      putStrLn $ "  Final span name: " <> show (readName ds)
      putStrLn $ "  (was \"GET /api/orders/ord-9f3a1b\", updated to route template)"
    _ -> putStrLn "  (unexpected span count)"
  verify memoryExporter 1 "Scenario 8"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 9: Provider shutdown — spans become no-ops
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 9: Provider shutdown — startSpan returns no-op after shutdown"

  -- Use a dedicated exporter so shutdown doesn't affect the shared memoryExporter.
  shutdownMemory <- newInMemorySpanExporter
  shutdownProc   <- newSimpleSpanProcessor (SomeSpanExporter shutdownMemory)
  shutdownProvider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerResource   = resource
    , providerProcessors = [SomeSpanProcessor shutdownProc]
    }
  shutdownTracer <- getTracer shutdownProvider
    (InstrumentationScope "shutdown-demo" (Just "1.0") Nothing Nothing)

  -- Create a span before shutdown — it should be recorded.
  beforeSpan <- startSpan shutdownTracer "before-shutdown" root defaultSpanConfig
  setStatus beforeSpan Ok Nothing
  end beforeSpan Nothing

  -- Shut the provider down.
  _ <- shutdown shutdownProvider
  putStrLn "  Provider shut down."

  -- Create a span after shutdown — it should be a no-op.
  afterSpan <- startSpan shutdownTracer "after-shutdown" root defaultSpanConfig
  recording <- isRecording afterSpan
  sc9 <- getSpanContext afterSpan
  putStrLn $ "  Span after shutdown — isRecording: " <> show recording
  putStrLn $ "  Span after shutdown — SpanContext valid: " <> show (isValid sc9)

  -- Only the before-shutdown span should have been exported.
  spans9 <- getFinishedSpans shutdownMemory
  putStrLn $ "  Spans exported: " <> show (length spans9) <> " (only before-shutdown)"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 10: Rich attribute types — all eight variants
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 10: Rich attribute types — all eight AttributeValue variants"
  reset memoryExporter  -- start fresh

  richSpan <- startSpan tracer "rich-attributes" root defaultSpanConfig
  setAttribute richSpan "attr.string"       (StringValue "hello world")
  setAttribute richSpan "attr.bool"         (BoolValue True)
  setAttribute richSpan "attr.int64"        (Int64Value (-9223372036854775808))
  setAttribute richSpan "attr.float64"      (Float64Value 3.14159265358979)
  setAttribute richSpan "attr.string_array" (StringArrayValue (Vector.fromList ["a", "b", "c"]))
  setAttribute richSpan "attr.bool_array"   (BoolArrayValue  (Vector.fromList [True, False, True]))
  setAttribute richSpan "attr.int_array"    (Int64ArrayValue (Vector.fromList [1, 2, 3, 4, 5]))
  setAttribute richSpan "attr.float_array"  (Float64ArrayValue (Vector.fromList [1.1, 2.2, 3.3]))
  setStatus richSpan Ok Nothing
  end richSpan Nothing

  putStrLn "  (See console output above for all 8 attribute variants)"
  verify memoryExporter 1 "Scenario 10"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 11: Multiple exporters fan-out — same span to three destinations
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 11: Multiple exporters fan-out — three independent exporters"
  reset memoryExporter

  mem1 <- newInMemorySpanExporter
  mem2 <- newInMemorySpanExporter
  mem3 <- newInMemorySpanExporter

  -- A counting processor that just tallies spans without storing them.
  counter <- newTVarIO (0 :: Int)
  let countingProc = CountingProcessor counter

  proc1 <- newSimpleSpanProcessor (SomeSpanExporter mem1)
  proc2 <- newSimpleSpanProcessor (SomeSpanExporter mem2)
  proc3 <- newSimpleSpanProcessor (SomeSpanExporter mem3)

  fanoutProvider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerResource   = resource
    , providerProcessors =
        [ SomeSpanProcessor proc1
        , SomeSpanProcessor proc2
        , SomeSpanProcessor proc3
        , SomeSpanProcessor countingProc
        ]
    }
  fanoutTracer <- getTracer fanoutProvider
    (InstrumentationScope "fanout-demo" (Just "1.0") Nothing Nothing)

  fanSpan <- startSpan fanoutTracer "fanout-span" root defaultSpanConfig
  setAttribute fanSpan "demo" (StringValue "fan-out")
  setStatus fanSpan Ok Nothing
  end fanSpan Nothing

  s1 <- getFinishedSpans mem1
  s2 <- getFinishedSpans mem2
  s3 <- getFinishedSpans mem3
  count <- readTVarIO counter

  putStrLn $ "  Exporter 1 received: " <> show (length s1) <> " span(s)"
  putStrLn $ "  Exporter 2 received: " <> show (length s2) <> " span(s)"
  putStrLn $ "  Exporter 3 received: " <> show (length s3) <> " span(s)"
  putStrLn $ "  Counting processor saw: " <> show count <> " span(s)"

  _ <- shutdown fanoutProvider

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 12: Status override rules — Ok-is-terminal state machine
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 12: Status override rules — Ok is terminal"
  reset memoryExporter

  -- Case A: Unset → Error (works)
  spanA <- startSpan tracer "status-unset-to-error" root defaultSpanConfig
  setStatus spanA Error (Just "something went wrong")
  end spanA Nothing

  -- Case B: Unset → Ok (works)
  spanB <- startSpan tracer "status-unset-to-ok" root defaultSpanConfig
  setStatus spanB Ok Nothing
  end spanB Nothing

  -- Case C: Error → Ok (Ok wins — Ok can always be set)
  spanC <- startSpan tracer "status-error-then-ok" root defaultSpanConfig
  setStatus spanC Error (Just "transient error")
  setStatus spanC Ok Nothing  -- overrides Error
  end spanC Nothing

  -- Case D: Ok → Error (ignored — Ok is terminal)
  spanD <- startSpan tracer "status-ok-then-error" root defaultSpanConfig
  setStatus spanD Ok Nothing
  setStatus spanD Error (Just "this should be ignored")  -- silently ignored
  end spanD Nothing

  spans12 <- getFinishedSpans memoryExporter
  putStrLn "  Results after all setStatus calls:"
  let showStatus (SomeReadableSpan s) =
        show (readName s) <> " → " <>
          case statusCode (readStatus s) of
            Unset -> "UNSET"
            Ok    -> "OK"
            Error -> "ERROR: " <> maybe "" show (statusDescription (readStatus s))
  mapM_ (putStrLn . ("    " <>) . showStatus) spans12
  verify memoryExporter 4 "Scenario 12"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 13: Explicit timestamps — back-dating a span
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 13: Explicit timestamps — recording a past event"
  reset memoryExporter

  -- Simulate: we received a message that was sent 500ms ago.
  -- We record the span with an explicit start time in the past.
  let msgSentAt   = fromNanos 1_700_000_000_000_000_000  -- fixed past time
      msgRecvedAt = fromNanos 1_700_000_000_500_000_000  -- 500ms later

  msgSpan <- startSpan tracer "process-delayed-message" root
    defaultSpanConfig
      { spanKind           = Consumer
      , spanStartTimestamp = Just msgSentAt  -- back-date to when message was sent
      }
  setAttribute msgSpan "messaging.system"        (StringValue "kafka")
  setAttribute msgSpan "messaging.message_id"    (StringValue "msg-xyz-789")
  setAttribute msgSpan "messaging.delay_ms"      (Int64Value 500)
  addEvent msgSpan "message.received"
    (fromList [("delay_ms", Int64Value 500)]) (Just msgRecvedAt)
  setStatus msgSpan Ok Nothing
  end msgSpan (Just (fromNanos 1_700_000_000_600_000_000))  -- explicit end time

  msgSpans <- getFinishedSpans memoryExporter
  case msgSpans of
    [SomeReadableSpan ms] -> do
      let Timestamp start = readStartTimestamp ms
          Timestamp end_  = readEndTimestamp ms
          durationMs      = (end_ - start) `div` 1_000_000
      putStrLn $ "  Start (ns): " <> show start <> " (explicit — message sent time)"
      putStrLn $ "  End   (ns): " <> show end_  <> " (explicit — processing done)"
      putStrLn $ "  Duration:   " <> show durationMs <> " ms (600ms total including 500ms delay)"
    _ -> putStrLn "  (unexpected span count)"
  verify memoryExporter 1 "Scenario 13"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 14: Custom sampler — 1-in-3 sampling
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 14: Custom sampler — 1-in-3 keeps every third span"
  reset memoryExporter

  -- Build a provider with our custom 1-in-3 sampler.
  samplerMemory <- newInMemorySpanExporter
  samplerProc   <- newSimpleSpanProcessor (SomeSpanExporter samplerMemory)
  samplerProvider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerResource   = resource
    , providerProcessors = [SomeSpanProcessor samplerProc]
    , providerSampler    = SomeSampler OneInThreeSampler
    }
  samplerTracer <- getTracer samplerProvider
    (InstrumentationScope "sampler-demo" (Just "1.0") Nothing Nothing)

  -- Create 9 spans — expect ~3 to be sampled (every 3rd one).
  samplerSpans <- mapM (\i -> do
      s <- startSpan samplerTracer (Text.pack ("request-" <> show (i :: Int))) root
        defaultSpanConfig { spanKind = Server }
      setStatus s Ok Nothing
      sc14 <- getSpanContext s
      end s Nothing
      pure (isSampled (traceFlags sc14))
    ) [1..9]

  let sampledCount = length (filter id samplerSpans)
      droppedCount = length (filter not samplerSpans)
  _ <- shutdown samplerProvider
  finalSampled <- getFinishedSpans samplerMemory
  putStrLn $ "  Spans created:  9"
  putStrLn $ "  Sampled (flag): " <> show sampledCount <>
    " — recorded and exported"
  putStrLn $ "  Dropped:        " <> show droppedCount <>
    " — neither recorded nor exported"
  putStrLn $ "  Spans in exporter: " <> show (length finalSampled)

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 15: recordException details — inspect the span event
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 15: recordException details — inspecting the generated event"
  reset memoryExporter

  excSpan <- startSpan tracer "payment-processing" root
    defaultSpanConfig { spanKind = Internal }
  setAttribute excSpan "payment.id"     (StringValue "pay-001")
  setAttribute excSpan "payment.amount" (Float64Value 99.95)

  -- Record an exception with additional context attributes.
  let payExc = toException (userError "insufficient funds") :: SomeException
  recordException excSpan payExc
    (fromList [("payment.error.code", StringValue "INSUFFICIENT_FUNDS")])

  -- Per spec: recordException does NOT auto-set status.
  -- We must call setStatus separately.
  setStatus excSpan Error (Just "payment declined")
  end excSpan Nothing

  excSpans <- getFinishedSpans memoryExporter
  case excSpans of
    [SomeReadableSpan es] -> do
      let events = readEvents es
      putStrLn $ "  Span events: " <> show (length events)
      mapM_ (\evt -> do
          putStrLn $ "  Event: \"" <> unpack (eventName evt) <> "\""
          let attrs = eventAttributes evt
          putStrLn   "    Attributes on the event:"
          mapM_ (\(k, v) -> putStrLn $ "      " <> unpack k <> " = " <> showAttrVal v)
            (toAttrList attrs)
        ) events
      putStrLn $ "  Span status: " <>
        case statusCode (readStatus es) of
          Error -> "ERROR (set explicitly — recordException does NOT auto-set status)"
          _     -> "other"
    _ -> putStrLn "  (unexpected span count)"
  verify memoryExporter 1 "Scenario 15"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 16: Cross-signal correlation — traces + metrics + logs
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 16: Cross-signal correlation — traces + metrics + logs"
  reset memoryExporter

  -- Set up metrics
  mReader16 <- newNoOpMetricReader
  mProvider16 <- newSdkMeterProvider defaultSdkMeterProviderConfig
    { providerReaders = [SomeMetricReader mReader16] }
  meter16 <- getMeter mProvider16 scope

  -- Set up logs
  logMemory16 <- newInMemoryLogRecordExporter
  logProc16 <- newSimpleLogRecordProcessor (SomeLogRecordExporter logMemory16)
  logProvider16 <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
    { llpProcessors = [SomeLogRecordProcessor logProc16] }
  logger16 <- getLogger logProvider16 scope

  -- Create counter + histogram
  SomeCounter reqCounter16 <- createCounter meter16 "requests_total" Nothing
  SomeHistogram latHist16 <- createHistogram meter16 "http_latency_ms" Nothing

  -- Create a request span
  reqSpan16 <- startSpan tracer "GET /api/orders" root
    defaultSpanConfig { spanKind = Server }
  reqSc16 <- getSpanContext reqSpan16
  let reqCtx16 = setSpanInContext reqSpan16 root

  -- Record metrics while span is active
  counterAdd reqCounter16 1.0 emptyAttributes
  histogramRecord latHist16 45.0 emptyAttributes

  -- Emit a log with the active context
  emit logger16 defaultLogRecord
    { logBody           = Just (LogBodyString "order request received")
    , logSeverityNumber = Just SeverityInfo
    , logContext        = Just reqCtx16
    }

  setStatus reqSpan16 Ok Nothing
  end reqSpan16 Nothing

  -- Collect metrics
  md16 <- readerCollect mReader16
  let metrics16 = concatMap smMetrics (mdScopeMetrics md16)
  putStrLn $ "  Request span: traceId=" <> unpack (Text.take 12 (traceIdToHex (traceId reqSc16))) <> "..."
  putStrLn $ "    spanId=" <> unpack (Text.take 8 (spanIdToHex (spanId reqSc16))) <> "..."
  forM_ metrics16 $ \m16 ->
    case metricPointData m16 of
      SumPointData sd -> do
        let total = sum [ndpValue p | p <- sumDataPoints sd]
        putStrLn $ "  Counter: " <> unpack (metricName m16) <> " = " <> show total
      HistogramPointData hd -> do
        let cnt = sum [hdpCount p | p <- histDataPoints hd]
            s = sum [maybe 0 id (hdpSum p) | p <- histDataPoints hd]
        putStrLn $ "  Histogram: " <> unpack (metricName m16) <> ": count=" <> show cnt <> ", sum=" <> show s
      _ -> pure ()

  -- Check log record span context
  logRecords16 <- getFinishedLogRecords logMemory16
  case logRecords16 of
    (SomeReadableLogRecord lr : _) -> do
      case rlrSpanContext lr of
        Just logSc ->
          putStrLn $ "  Log record span context: traceId=" <>
            unpack (Text.take 12 (traceIdToHex (traceId logSc))) <> "... " <>
            if traceId logSc == traceId reqSc16 then "(matches span)" else "(MISMATCH)"
        Nothing -> putStrLn "  Log record: no span context attached"
    _ -> putStrLn "  (no log records captured)"

  verify memoryExporter 1 "Scenario 16"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 17: W3C Trace Context propagation
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 17: W3C Trace Context propagation — simulated service boundary"
  reset memoryExporter

  -- Service A: create root span, inject into headers
  serviceASpan <- startSpan tracer "GET /api/orders" root
    defaultSpanConfig { spanKind = Server }
  serviceASc <- getSpanContext serviceASpan
  let serviceACtx = setSpanInContext serviceASpan root
  putStrLn $ "  [Service A] Starting root span: GET /api/orders"

  let carrier0 = Map.empty :: Map.Map Text Text
  carrier1 <- inject W3CTraceContextPropagator serviceACtx carrier0
  putStrLn $ "  [Service A] Injected headers:"
  mapM_ (\(k, v) -> putStrLn $ "    " <> unpack k <> ": " <> unpack v) (Map.toList carrier1)

  setStatus serviceASpan Ok Nothing
  end serviceASpan Nothing

  -- Service B: extract context, create child span
  serviceBCtx <- extract W3CTraceContextPropagator root carrier1
  serviceBSpan <- startSpan tracer "process-order" serviceBCtx
    defaultSpanConfig { spanKind = Server }
  serviceBSc <- getSpanContext serviceBSpan

  let traceMatches17 = traceId serviceBSc == traceId serviceASc
  putStrLn $ "  [Service B] Extracted context -> child span"
  putStrLn $ "  [Service B] Child traceId matches Service A: " <> show traceMatches17

  -- Check parent: the extracted span context's spanId should be the parent
  case getSpanFromContext serviceBCtx of
    Just someExtracted -> do
      extractedSc <- getSpanContext someExtracted
      putStrLn $ "  [Service B] Child has parent spanId from Service A: " <>
        show (spanId extractedSc == spanId serviceASc)
    Nothing -> putStrLn "  [Service B] No parent extracted"

  setStatus serviceBSpan Ok Nothing
  end serviceBSpan Nothing

  verify memoryExporter 2 "Scenario 17"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 18: W3C Baggage — passing application context
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 18: W3C Baggage — passing tenant context across boundaries"
  reset memoryExporter

  -- Service A: set baggage
  let baggage18 = setValue "tenant-id" "acme" Nothing
                $ setValue "user-tier" "premium" Nothing
                $ emptyBaggage
      ctxWithBaggage = setBaggage baggage18 root
  putStrLn "  [Service A] Setting baggage: tenant-id=acme, user-tier=premium"

  baggageCarrier0 <- inject W3CBaggagePropagator ctxWithBaggage (Map.empty :: Map.Map Text Text)
  putStrLn $ "  [Service A] Injected baggage header: " <>
    maybe "(none)" unpack (Map.lookup "baggage" baggageCarrier0)

  -- Service B: extract baggage
  ctxB18 <- extract W3CBaggagePropagator root baggageCarrier0
  let extractedBaggage = getBaggage ctxB18
  putStrLn "  [Service B] Extracted baggage:"
  forM_ (baggageToList extractedBaggage) $ \(k, entry) ->
    putStrLn $ "    " <> unpack k <> " = " <> unpack (entryValue entry)

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 19: Metrics — request counter + latency histogram
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 19: Metrics — request counter and latency histogram"

  mReader19 <- newNoOpMetricReader
  mProvider19 <- newSdkMeterProvider defaultSdkMeterProviderConfig
    { providerReaders = [SomeMetricReader mReader19] }
  meter19 <- getMeter mProvider19 scope

  SomeCounter httpCounter19 <- createCounter meter19 "http_requests_total" Nothing
  SomeHistogram httpHist19 <- createHistogram meter19 "http_latency_ms" Nothing

  let latencies19 = [12, 45, 8, 67, 23] :: [Double]
  putStrLn "  Simulating 5 HTTP requests..."
  forM_ latencies19 $ \lat -> do
    counterAdd httpCounter19 1.0 emptyAttributes
    histogramRecord httpHist19 lat emptyAttributes

  md19 <- readerCollect mReader19
  let metrics19 = concatMap smMetrics (mdScopeMetrics md19)
  putStrLn "  Collected metrics:"
  forM_ metrics19 $ \m19 ->
    case metricPointData m19 of
      SumPointData sd -> do
        let total = sum [ndpValue p | p <- sumDataPoints sd]
        putStrLn $ "    " <> unpack (metricName m19) <> " (Sum, monotonic=" <>
          show (sumIsMonotonic sd) <> "): " <> show (length (sumDataPoints sd)) <> " data point(s)"
        putStrLn $ "      Total requests: " <> show total
      HistogramPointData hd -> do
        let cnt = sum [hdpCount p | p <- histDataPoints hd]
        putStrLn $ "    " <> unpack (metricName m19) <> " (Histogram): " <>
          show (length (histDataPoints hd)) <> " data point(s)"
        putStrLn $ "      Total count: " <> show cnt
        putStrLn $ "      Latencies: " <> show latencies19
      _ -> pure ()

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 20: Observable gauge — lazy evaluation at collection time
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 20: Observable gauge — queue depth reported at collection time"

  queueDepthRef <- newIORef (10 :: Int)
  mReader20 <- newNoOpMetricReader
  mProvider20 <- newSdkMeterProvider defaultSdkMeterProviderConfig
    { providerReaders = [SomeMetricReader mReader20] }
  meter20 <- getMeter mProvider20 scope

  let gaugeCallback :: SomeObservableResult -> IO ()
      gaugeCallback obsResult = do
        depth <- readIORef queueDepthRef
        observeValue obsResult (fromIntegral depth) emptyAttributes
  SomeObservableGauge _obsGauge20 <- createObservableGauge meter20 "queue_depth"
    [gaugeCallback] Nothing

  -- Collection 1
  md20a <- readerCollect mReader20
  let gaugeVal20 md = case concatMap smMetrics (mdScopeMetrics md) of
        (m20:_) -> case metricPointData m20 of
          GaugePointData gd -> case gaugeDataPoints gd of
            (p:_) -> show (ndpValue p)
            _     -> "?"
          _ -> "?"
        _ -> "?"
  putStrLn $ "  Collection 1: queue_depth = " <> gaugeVal20 md20a <> " (callback invoked lazily)"

  writeIORef queueDepthRef 8
  md20b <- readerCollect mReader20
  putStrLn $ "  Collection 2: queue_depth = " <> gaugeVal20 md20b <> " (callback invoked lazily again)"

  writeIORef queueDepthRef 6
  md20c <- readerCollect mReader20
  putStrLn $ "  Collection 3: queue_depth = " <> gaugeVal20 md20c
  putStrLn "  Callbacks run AT collect time, not at instrument creation time"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 21: Metric Views — attribute filtering
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 21: Metric Views — attribute filtering collapses data points"

  mReader21 <- newNoOpMetricReader
  let regionView = defaultView
        { viewInstrumentName = Just "region_counter"
        , viewAttributeKeys  = Just (Set.fromList ["region"])
        }
  mProvider21 <- newSdkMeterProvider defaultSdkMeterProviderConfig
    { providerReaders = [SomeMetricReader mReader21]
    , providerViews   = [regionView]
    }
  meter21 <- getMeter mProvider21 scope

  SomeCounter regionCounter21 <- createCounter meter21 "region_counter" Nothing

  -- Record with 3 attribute sets
  counterAdd regionCounter21 1.0 (fromList [("region", StringValue "us"), ("env", StringValue "prod"), ("tier", StringValue "web")])
  counterAdd regionCounter21 1.0 (fromList [("region", StringValue "us"), ("env", StringValue "prod"), ("tier", StringValue "api")])
  counterAdd regionCounter21 1.0 (fromList [("region", StringValue "eu"), ("env", StringValue "prod"), ("tier", StringValue "web")])

  putStrLn "  Counter recorded with 3 attribute sets"
  putStrLn "  View: keep only {region}"

  md21 <- readerCollect mReader21
  let metrics21 = concatMap smMetrics (mdScopeMetrics md21)
  putStrLn "  After filtering:"
  forM_ metrics21 $ \m21 ->
    case metricPointData m21 of
      SumPointData sd ->
        forM_ (sumDataPoints sd) $ \dp ->
          putStrLn $ "    " <> show (toList (ndpAttributes dp)) <> ": value=" <> show (ndpValue dp)
      _ -> pure ()

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 22: Resource Detectors — auto-detected attributes
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 22: Resource Detectors — auto-detected environment attributes"

  detectedRes22 <- defaultResource
    [ SomeResourceDetector ProcessResourceDetector
    , SomeResourceDetector HostResourceDetector
    , SomeResourceDetector OsResourceDetector
    ]
  let resAttrs22 = toList (getAttributes detectedRes22)
  putStrLn "  Detected resource attributes:"
  forM_ resAttrs22 $ \(k, v) ->
    putStrLn $ "    " <> unpack k <> " = " <> showAttrVal v
  let hasSdkName = any (\(k, _) -> k == "telemetry.sdk.name") resAttrs22
  putStrLn $ "  SdkResourceDetector always included: " <> show hasSdkName

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 23: Sampling strategies — TraceIdRatio + ParentBased
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 23: Sampling strategies — TraceIdRatio and ParentBased"

  -- TraceIdRatioBasedSampler at 50%
  ratioMemory23 <- newInMemorySpanExporter
  ratioProc23 <- newSimpleSpanProcessor (SomeSpanExporter ratioMemory23)
  ratioProvider23 <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerResource   = resource
    , providerProcessors = [SomeSpanProcessor ratioProc23]
    , providerSampler    = SomeSampler (TraceIdRatioBasedSampler 0.5)
    }
  ratioTracer23 <- getTracer ratioProvider23 scope

  ratioResults23 <- mapM (\i -> do
      s <- startSpan ratioTracer23 (Text.pack ("ratio-span-" <> show (i :: Int))) root
        defaultSpanConfig { spanKind = Server }
      sc23 <- getSpanContext s
      end s Nothing
      pure (isSampled (traceFlags sc23))
    ) [1..20]

  let sampledRatio = length (filter id ratioResults23)
      droppedRatio = length (filter not ratioResults23)
  _ <- shutdown ratioProvider23
  putStrLn $ "  TraceIdRatioBasedSampler(0.5): created 20 root spans"
  putStrLn $ "    Sampled: " <> show sampledRatio <> ", Dropped: " <> show droppedRatio

  -- ParentBasedSampler
  parentMemory23 <- newInMemorySpanExporter
  parentProc23 <- newSimpleSpanProcessor (SomeSpanExporter parentMemory23)
  parentProvider23 <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerResource   = resource
    , providerProcessors = [SomeSpanProcessor parentProc23]
    , providerSampler    = SomeSampler (defaultParentBasedSampler (SomeSampler AlwaysOnSampler))
    }
  parentTracer23 <- getTracer parentProvider23 scope

  -- Sampled parent -> child should be sampled
  sampledParent23 <- startSpan parentTracer23 "sampled-parent" root defaultSpanConfig
  sampledParentSc23 <- getSpanContext sampledParent23
  let sampledCtx23 = setSpanInContext sampledParent23 root
  sampledChild23 <- startSpan parentTracer23 "sampled-child" sampledCtx23 defaultSpanConfig
  sampledChildSc23 <- getSpanContext sampledChild23
  end sampledChild23 Nothing
  end sampledParent23 Nothing

  putStrLn "  ParentBasedSampler: sampled parent always produces sampled child"
  putStrLn $ "    Sampled parent -> child sampled: " <>
    show (isSampled (traceFlags sampledParentSc23) && isSampled (traceFlags sampledChildSc23))

  _ <- shutdown parentProvider23

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 24: Semantic Conventions — HTTP + DB attribute keys
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 24: Semantic Conventions — standard attribute key strings"
  reset memoryExporter

  putStrLn "  Using OTel.SemConv constants (semconv v1.27.0):"
  putStrLn $ "  HTTP: httpRequestMethod = " <> show SemHTTP.httpRequestMethod
  putStrLn $ "        httpResponseStatusCode = " <> show SemHTTP.httpResponseStatusCode
  putStrLn $ "        urlFull = " <> show SemHTTP.urlFull
  putStrLn $ "        serverAddress = " <> show SemHTTP.serverAddress
  putStrLn $ "  DB:   dbSystem = " <> show SemDB.dbSystem
  putStrLn $ "        dbQueryText = " <> show SemDB.dbQueryText
  putStrLn $ "        dbOperationName = " <> show SemDB.dbOperationName

  -- Create a span with semconv attributes
  scSpan24 <- startSpan tracer "GET /api/users" root
    defaultSpanConfig { spanKind = Server }
  setAttribute scSpan24 SemHTTP.httpRequestMethod (StringValue "GET")
  setAttribute scSpan24 SemHTTP.httpResponseStatusCode (Int64Value 200)
  setAttribute scSpan24 SemDB.dbSystem (StringValue "postgresql")
  setStatus scSpan24 Ok Nothing
  end scSpan24 Nothing

  spans24 <- getFinishedSpans memoryExporter
  case spans24 of
    [SomeReadableSpan s24] -> do
      putStrLn "  Span with semconv attributes:"
      forM_ (toList (readAttributes s24)) $ \(k, v) ->
        putStrLn $ "    " <> unpack k <> " = " <> showAttrVal v
    _ -> putStrLn "  (unexpected span count)"
  verify memoryExporter 1 "Scenario 24"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 25: Log-trace correlation — structured log with span context
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 25: Log-trace correlation — structured log carries span context"
  reset memoryExporter

  logMemory25 <- newInMemoryLogRecordExporter
  logProc25 <- newSimpleLogRecordProcessor (SomeLogRecordExporter logMemory25)
  logProvider25 <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
    { llpProcessors = [SomeLogRecordProcessor logProc25] }
  logger25 <- getLogger logProvider25 scope

  -- Create a span, emit log within its context
  activeSpan25 <- startSpan tracer "order-processing" root
    defaultSpanConfig { spanKind = Internal }
  activeSc25 <- getSpanContext activeSpan25
  let activeCtx25 = setSpanInContext activeSpan25 root

  putStrLn $ "  Active span: traceId=" <> unpack (Text.take 12 (traceIdToHex (traceId activeSc25))) <>
    "... spanId=" <> unpack (Text.take 8 (spanIdToHex (spanId activeSc25))) <> "..."

  -- Log WITH context
  emit logger25 defaultLogRecord
    { logBody           = Just (LogBodyString "order created")
    , logSeverityNumber = Just SeverityInfo
    , logContext        = Just activeCtx25
    }

  -- Log WITHOUT context
  emit logger25 defaultLogRecord
    { logBody           = Just (LogBodyString "general startup message")
    , logSeverityNumber = Just SeverityInfo
    , logContext        = Nothing
    }

  end activeSpan25 Nothing

  logRecords25 <- getFinishedLogRecords logMemory25
  case logRecords25 of
    (SomeReadableLogRecord lr1 : SomeReadableLogRecord lr2 : _) -> do
      putStrLn $ "  Log emitted with: severity=INFO, body=\"order created\", context=activeCtx"
      case rlrSpanContext lr1 of
        Just sc25 -> do
          putStrLn "  Log record received:"
          putStrLn $ "    body: " <> maybe "?" showLogBody (rlrBody lr1)
          putStrLn $ "    severity: SeverityInfo (" <> show (maybe 0 severityNumberValue (rlrSeverityNumber lr1)) <> ")"
          putStrLn $ "    traceId: " <> unpack (Text.take 12 (traceIdToHex (traceId sc25))) <>
            "... " <> if traceId sc25 == traceId activeSc25 then "(matches active span)" else "(MISMATCH)"
          putStrLn $ "    spanId:  " <> unpack (Text.take 8 (spanIdToHex (spanId sc25))) <>
            "... " <> if spanId sc25 == spanId activeSc25 then "(matches active span)" else "(MISMATCH)"
        Nothing -> putStrLn "  Log record: no span context (unexpected)"
      putStrLn "  Log record WITHOUT context:"
      putStrLn $ "    traceId: " <> show (fmap (traceIdToHex . traceId) (rlrSpanContext lr2)) <>
        " (no context attached)"
    _ -> putStrLn $ "  (unexpected log record count: " <> show (length logRecords25) <> ")"

  verify memoryExporter 1 "Scenario 25"

  -- ─────────────────────────────────────────────────────────────
  -- Scenario 26: Full-stack from YAML config
  -- ─────────────────────────────────────────────────────────────
  section "Scenario 26: Full-stack from YAML — all providers from config"
  reset memoryExporter

  let configYaml = Text.unlines
        [ "sdk:"
        , "  resource:"
        , "    service_name: demo-service"
        , "  tracer_provider:"
        , "    sampler: always_on"
        , "    processors:"
        , "      - type: simple"
        , "        exporter: console"
        , "  logger_provider:"
        , "    processors:"
        , "      - type: simple"
        , "        exporter: console"
        , "propagators:"
        , "  - tracecontext"
        , "  - baggage"
        ]

  putStrLn "  Parsing config YAML..."
  parseResult26 <- parseYaml configYaml
  case parseResult26 of
    Left err -> putStrLn $ "  Parse error: " <> show err
    Right config26 -> do
      putStrLn "  Building providers from config..."

      tpResult26 <- createTracerProvider config26
      case tpResult26 of
        Right _tp26 -> putStrLn "    TracerProvider: built"
        Left err    -> putStrLn $ "    TracerProvider: FAILED " <> show err

      lpResult26 <- createLoggerProvider config26
      case lpResult26 of
        Right _lp26 -> putStrLn "    LoggerProvider: built"
        Left err    -> putStrLn $ "    LoggerProvider: FAILED " <> show err

      propResult26 <- createPropagator config26
      case propResult26 of
        Right _prop26 -> putStrLn "    Propagator: W3C TraceContext + Baggage built"
        Left err      -> putStrLn $ "    Propagator: FAILED " <> show err

      -- Verify we can use the config-built tracer provider
      case tpResult26 of
        Right tp26 -> do
          cfgTracer26 <- getTracer tp26 scope
          cfgSpan26 <- startSpan cfgTracer26 "config-test-span" root defaultSpanConfig
          setStatus cfgSpan26 Ok Nothing
          end cfgSpan26 Nothing
          putStrLn "  Emitting telemetry through config-built providers:"
          putStrLn "    Span: emitted (1 span via config tracer)"
          _ <- shutdown tp26
          pure ()
        Left _ -> putStrLn "  (skipped telemetry — tracer provider failed)"

  -- ─────────────────────────────────────────────────────────────
  -- Final shutdown
  -- ─────────────────────────────────────────────────────────────
  putStrLn ""
  putStrLn "╔══════════════════════════════════════════════════╗"
  putStrLn "║  All scenarios complete. Shutting down.          ║"
  putStrLn "╚══════════════════════════════════════════════════╝"
  result <- shutdown provider
  case result of
    Right () -> putStrLn "Shutdown complete."
    Left err -> putStrLn $ "Shutdown error: " <> show err


-------------------------------------------------------------------------------
-- Counting processor — counts spans without storing them
-------------------------------------------------------------------------------

newtype CountingProcessor = CountingProcessor (TVar Int)

instance SpanProcessor CountingProcessor where
  onStart _ _ _   = pure ()
  onEnd (CountingProcessor ref) _ = atomically (modifyTVar' ref (+1))
  shutdownProcessor _   = pure (Right ())
  forceFlushProcessor _ _ = pure (Right ())


-------------------------------------------------------------------------------
-- OneInThreeSampler — samples every 3rd span based on SpanId low byte
-------------------------------------------------------------------------------

data OneInThreeSampler = OneInThreeSampler

instance Sampler OneInThreeSampler where
  shouldSample _ _ traceId_ _ _ _ _ = do
    -- Sample every 3rd span using the sum of chars of the traceId hex as a
    -- simple, deterministic hash. In production use TraceIdRatioBased instead.
    let hexStr = show traceId_
        h = sum (map fromEnum hexStr) `mod` 3
    pure SamplingResult
      { samplingDecision   = if h == 0 then RecordAndSample else Drop
      , samplingAttributes = emptyAttributes
      , samplingTraceState = OTel.Trace.TraceState.empty
      }
  samplerDescription _ = "OneInThreeSampler"


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

section :: String -> IO ()
section title = do
  putStrLn ""
  putStrLn $ "┌─ " <> title
  putStrLn ""

verify :: InMemorySpanExporter -> Int -> String -> IO ()
verify exporter expected label = do
  spans <- getFinishedSpans exporter
  let got = length spans
  putStrLn ""
  putStrLn $ "  [" <> label <> "] Spans captured: " <> show got <>
    if got == expected then " ✓" else " ✗ (expected " <> show expected <> ")"
  mapM_ printSpanSummary spans

showLogBody :: LogBody -> String
showLogBody (LogBodyString t) = unpack t
showLogBody (LogBodyBool b)   = show b
showLogBody (LogBodyInt64 n)  = show n
showLogBody (LogBodyFloat64 d) = show d
showLogBody (LogBodyBytes _)  = "<bytes>"
showLogBody (LogBodyList _)   = "<list>"
showLogBody (LogBodyMap _)    = "<map>"

showAttrVal :: AttributeValue -> String
showAttrVal (StringValue t)      = "\"" <> unpack t <> "\""
showAttrVal (BoolValue b)        = show b
showAttrVal (Int64Value n)       = show n
showAttrVal (Float64Value d)     = show d
showAttrVal (StringArrayValue _) = "[...]"
showAttrVal (BoolArrayValue _)   = "[...]"
showAttrVal (Int64ArrayValue _)  = "[...]"
showAttrVal (Float64ArrayValue _)= "[...]"

toAttrList :: Attributes -> [(Key, AttributeValue)]
toAttrList = toList

printSpanSummary :: SomeReadableSpan -> IO ()
printSpanSummary (SomeReadableSpan s) = do
  let sc     = readSpanContext s
      tid    = unpack (Text.take 16 (traceIdToHex (traceId sc)))
      sid    = unpack (Text.take 8  (spanIdToHex  (spanId  sc)))
      status = case statusCode (readStatus s) of
                 Ok    -> "OK"
                 Error -> "ERROR"
                 Unset -> "UNSET"
  putStrLn $ "  ├ " <> show (readName s)
    <> "  trace=" <> tid <> "…"
    <> "  span=" <> sid <> "…"
    <> "  kind=" <> show (readKind s)
    <> "  status=" <> status
    <> "  events=" <> show (length (readEvents s))
