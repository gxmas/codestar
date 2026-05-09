module Main where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.Exception (IOException, bracket, toException)
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector qualified as V
import Data.Word (Word32)
import Lens.Family2 ((^.))
import qualified Network.HTTP.Semantics as Sem
import qualified Network.HTTP.Semantics.Server as SemS
import Network.HTTP.Types (ok200)
import qualified Network.HTTP2.Server as H2S
import qualified Network.Socket as NS
import OTel.Attribute qualified as Attr
import OTel.Exporter.OTLP.GRPC
import OTel.Exporter.OTLP.GRPC.Internal
import OTel.Exporter.OTLP.GRPC.Internal.Proto
import OTel.Log (LogBody (..), SeverityNumber (..), severityNumberValue)
import OTel.SDK.Export (ExportResult (..))
import OTel.SDK.Log.Export (LogRecordExporter (..), ReadableLogRecord (..), SomeReadableLogRecord (..))
import OTel.SDK.Metric.Export
import OTel.SDK.Resource qualified as Resource
import OTel.SDK.Trace.Export (Link (..), ReadableSpan (..), SomeReadableSpan (..), SpanEvent (..), SpanExporter (..))
import OTel.Timestamp (Timestamp (..), fromNanos)
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
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as LF
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as M
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as MF
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields qualified as RF
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as T
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as TF
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "otel-exporter-otlp-grpc"
  [ frameEncodingTests
  , frameDecodingTests
  , statusParsingTests
  , isRetryableTests
  , retryLogicTests
  , protoAttributeValueTests
  , protoAttributesTests
  , protoResourceTests
  , protoSpanContextTests
  , protoSpanKindTests
  , protoStatusCodeTests
  , protoSpanFieldsTests
  , protoMetricPointDataTests
  , protoSeverityNumberTests
  , protoLogBodyTests
  , protoSpanGroupingTests
  , protoResponseParsingTests
  , defaultConfigTests
  , shutdownBehaviourTests
  , envVarResolutionTests
  , integrationTests
  , tlsTransportTests
  ]


-- Generators

genCompression :: Gen Compression
genCompression = elements [NoCompression, GzipCompression]

genStatusCode :: Gen GrpcStatusCode
genStatusCode = elements [minBound .. maxBound]

genPayload :: Gen ByteString
genPayload = BS.pack <$> arbitrary


-- Frame Encoding

frameEncodingTests :: TestTree
frameEncodingTests = testGroup "Frame encoding"
  [ testProperty "length prefix correct for arbitrary payload" $
      forAll genPayload $ \payload ->
        ioProperty $ do
          frame <- encodeFrame NoCompression payload
          let totalLen = BS.length frame
              flag = BS.index frame 0
              encodedLen = word32At frame 1
          pure $
            totalLen === 5 + BS.length payload
            .&&. flag === 0
            .&&. encodedLen === fromIntegral (BS.length payload)
  , testGroup "specific sizes" $
      [ testCase (show n ++ " bytes") $ do
          let payload = BS.replicate n 0x42
          frame <- encodeFrame NoCompression payload
          BS.length frame @?= 5 + n
          BS.index frame 0 @?= 0
          word32At frame 1 @?= fromIntegral n
      | n <- [0, 1, 127, 128, 255, 256, 65535, 65536]
      ]
  ]


-- Frame Decoding

frameDecodingTests :: TestTree
frameDecodingTests = testGroup "Frame decoding"
  [ testProperty "round-trip NoCompression" $
      forAll genPayload $ \payload ->
        ioProperty $ do
          frame <- encodeFrame NoCompression payload
          result <- decodeFrame frame
          pure $ result === Right (NoCompression, payload)
  , testProperty "round-trip GzipCompression" $
      forAll genPayload $ \payload ->
        ioProperty $ do
          frame <- encodeFrame GzipCompression payload
          result <- decodeFrame frame
          pure $ result === Right (GzipCompression, payload)
  , testProperty "round-trip arbitrary compression" $
      forAll genCompression $ \c ->
      forAll genPayload $ \payload ->
        ioProperty $ do
          frame <- encodeFrame c payload
          result <- decodeFrame frame
          pure $ result === Right (c, payload)
  , testCase "round-trip empty payload" $ do
      frame <- encodeFrame GzipCompression BS.empty
      result <- decodeFrame frame
      result @?= Right (GzipCompression, BS.empty)
  , testCase "round-trip large payload" $ do
      let payload = BS.replicate 100000 0xAB
      frame <- encodeFrame GzipCompression payload
      result <- decodeFrame frame
      result @?= Right (GzipCompression, payload)
  , testGroup "error cases"
      [ testCase "fewer than 5 bytes" $ do
          result <- decodeFrame (BS.pack [0, 0, 0])
          result @?= Left (FrameTooShort 3)
      , testCase "empty input" $ do
          result <- decodeFrame BS.empty
          result @?= Left (FrameTooShort 0)
      , testCase "payload length mismatch" $ do
          let frame = BS.pack [0, 0, 0, 0, 10] <> BS.replicate 5 0x00
          result <- decodeFrame frame
          result @?= Left FrameLengthMismatch
      , testCase "unknown compression flag" $ do
          let frame = BS.pack [2, 0, 0, 0, 3] <> BS.replicate 3 0x00
          result <- decodeFrame frame
          result @?= Left (FrameUnknownCompression 2)
      , testCase "invalid gzip data" $ do
          let frame = BS.pack [1, 0, 0, 0, 4] <> BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
          result <- decodeFrame frame
          case result of
            Left (FrameDecompressionError _) -> pure ()
            other -> assertFailure $ "Expected FrameDecompressionError, got: " ++ show other
      ]
  ]


-- Status Parsing

statusParsingTests :: TestTree
statusParsingTests = testGroup "Status parsing"
  [ testCase "empty headers -> GrpcOk" $
      parseGrpcStatus [] @?= GrpcStatus GrpcOk ""
  , testCase "grpc-status 0 -> GrpcOk" $
      parseGrpcStatus [("grpc-status", "0")] @?= GrpcStatus GrpcOk ""
  , testCase "grpc-status 14 -> GrpcUnavailable" $
      parseGrpcStatus [("grpc-status", "14")] @?= GrpcStatus GrpcUnavailable ""
  , testCase "grpc-status 8 -> GrpcResourceExhausted" $
      parseGrpcStatus [("grpc-status", "8")] @?= GrpcStatus GrpcResourceExhausted ""
  , testCase "grpc-status 16 -> GrpcUnauthenticated" $
      parseGrpcStatus [("grpc-status", "16")] @?= GrpcStatus GrpcUnauthenticated ""
  , testCase "URL-decodes grpc-message (percent-encoding)" $
      parseGrpcStatus [("grpc-status", "14"), ("grpc-message", "service%20unavailable")]
        @?= GrpcStatus GrpcUnavailable "service unavailable"
  , testCase "out-of-range code -> GrpcUnknown" $
      parseGrpcStatus [("grpc-status", "999")] @?= GrpcStatus GrpcUnknown ""
  , testCase "non-numeric -> GrpcUnknown" $
      parseGrpcStatus [("grpc-status", "not-a-number")] @?= GrpcStatus GrpcUnknown ""
  , testProperty "round-trip all status codes" $
      forAll genStatusCode $ \code ->
        let n = fromEnum code
            hdrs = [("grpc-status", B8.pack (show n))]
         in parseGrpcStatus hdrs === GrpcStatus code ""
  ]


-- isRetryable

isRetryableTests :: TestTree
isRetryableTests = testGroup "isRetryable"
  [ testCase "GrpcCancelled is retryable" $
      isRetryable GrpcCancelled @?= True
  , testCase "GrpcDeadlineExceeded is retryable" $
      isRetryable GrpcDeadlineExceeded @?= True
  , testCase "GrpcResourceExhausted is retryable" $
      isRetryable GrpcResourceExhausted @?= True
  , testCase "GrpcAborted is retryable" $
      isRetryable GrpcAborted @?= True
  , testCase "GrpcOutOfRange is retryable" $
      isRetryable GrpcOutOfRange @?= True
  , testCase "GrpcUnavailable is retryable" $
      isRetryable GrpcUnavailable @?= True
  , testCase "GrpcDataLoss is retryable" $
      isRetryable GrpcDataLoss @?= True
  , testProperty "exactly the 7 OTLP-retryable codes are retryable" $
      forAll genStatusCode $ \code ->
        let retryableCodes =
              [ GrpcCancelled
              , GrpcDeadlineExceeded
              , GrpcResourceExhausted
              , GrpcAborted
              , GrpcOutOfRange
              , GrpcUnavailable
              , GrpcDataLoss
              ]
         in isRetryable code === (code `elem` retryableCodes)
  ]


-- Retry Logic

fastRetryConfig :: RetryConfig
fastRetryConfig = RetryConfig
  { retryMaxAttempts = 3
  , retryInitialDelay = 1
  , retryMaxDelay = 1
  }

retryLogicTests :: TestTree
retryLogicTests = testGroup "Retry logic"
  [ testCase "retryable status retries until exhaustion" $ do
      counter <- newIORef (0 :: Int)
      let action _ = do
            modifyIORef' counter (+ 1)
            pure $ Left (GrpcStatusError (GrpcStatus GrpcUnavailable "") Nothing)
      _ <- withRetry fastRetryConfig action
      count <- readIORef counter
      count @?= 3
  , testCase "non-retryable status fails immediately" $ do
      counter <- newIORef (0 :: Int)
      let action _ = do
            modifyIORef' counter (+ 1)
            pure $ Left (GrpcStatusError (GrpcStatus GrpcInvalidArgument "") Nothing)
      _ <- withRetry fastRetryConfig action
      count <- readIORef counter
      count @?= 1
  , testCase "success on first try" $ do
      counter <- newIORef (0 :: Int)
      let action _ = do
            modifyIORef' counter (+ 1)
            pure $ Right ("ok" :: String)
      result <- withRetry fastRetryConfig action
      count <- readIORef counter
      count @?= 1
      case result of
        Right val -> val @?= "ok"
        Left e -> assertFailure $ "Expected Right, got Left: " ++ show e
  , testCase "network error retries" $ do
      counter <- newIORef (0 :: Int)
      let err = toException (userError "connection refused" :: IOException)
      let action _ = do
            modifyIORef' counter (+ 1)
            pure $ Left (GrpcNetworkError err)
      _ <- withRetry fastRetryConfig action
      count <- readIORef counter
      count @?= 3
  , testCase "retry-after hint of 0 still retries" $ do
      counter <- newIORef (0 :: Int)
      let action _ = do
            modifyIORef' counter (+ 1)
            pure $ Left (GrpcStatusError (GrpcStatus GrpcUnavailable "") (Just 0))
      _ <- withRetry fastRetryConfig action
      count <- readIORef counter
      count @?= 3
  , testCase "attempt numbers passed correctly" $ do
      attemptsRef <- newIORef ([] :: [Int])
      let action attempt = do
            modifyIORef' attemptsRef (++ [attempt])
            pure $ Left (GrpcStatusError (GrpcStatus GrpcUnavailable "") Nothing)
      _ <- withRetry fastRetryConfig action
      attempts <- readIORef attemptsRef
      attempts @?= [0, 1, 2]
  , testCase "GrpcCancelled retries until exhaustion" $ do
      counter <- newIORef (0 :: Int)
      let action _ = do
            modifyIORef' counter (+ 1)
            pure $ Left (GrpcStatusError (GrpcStatus GrpcCancelled "") Nothing)
      _ <- withRetry fastRetryConfig action
      count <- readIORef counter
      count @?= 3
  ]


-------------------------------------------------------------------------------
-- Test-only ReadableSpan
-------------------------------------------------------------------------------

data TestSpan = TestSpan
  { tsContext :: SC.SpanContext
  , tsParentContext :: Maybe SC.SpanContext
  , tsName :: Text
  , tsKind :: SpanKind
  , tsStart :: Timestamp
  , tsEnd :: Timestamp
  , tsAttributes :: Attr.Attributes
  , tsEvents :: [SpanEvent]
  , tsLinks :: [Link]
  , tsStatus :: SpanStatus
  , tsResource :: Resource.Resource
  , tsScope :: Attr.InstrumentationScope
  , tsDroppedAttrs :: Int
  , tsDroppedEvents :: Int
  , tsDroppedLinks :: Int
  }

instance ReadableSpan TestSpan where
  readSpanContext = tsContext
  readParentSpanContext = tsParentContext
  readName = tsName
  readKind = tsKind
  readStartTimestamp = tsStart
  readEndTimestamp = tsEnd
  readAttributes = tsAttributes
  readEvents = tsEvents
  readLinks = tsLinks
  readStatus = tsStatus
  readResource = tsResource
  readInstrumentationScope = tsScope
  readDroppedAttributesCount = tsDroppedAttrs
  readDroppedEventsCount = tsDroppedEvents
  readDroppedLinksCount = tsDroppedLinks


defaultTestSpan :: TestSpan
defaultTestSpan = TestSpan
  { tsContext = SC.invalidSpanContext
  , tsParentContext = Nothing
  , tsName = "test-span"
  , tsKind = Internal
  , tsStart = fromNanos 1000
  , tsEnd = fromNanos 2000
  , tsAttributes = Attr.emptyAttributes
  , tsEvents = []
  , tsLinks = []
  , tsStatus = SpanStatus Unset Nothing
  , tsResource = Resource.empty
  , tsScope = Attr.InstrumentationScope "test" Nothing Nothing Nothing
  , tsDroppedAttrs = 0
  , tsDroppedEvents = 0
  , tsDroppedLinks = 0
  }


-------------------------------------------------------------------------------
-- Test-only ReadableLogRecord
-------------------------------------------------------------------------------

data TestLogRecord = TestLogRecord
  { tlrResource :: Resource.Resource
  , tlrScope :: Attr.InstrumentationScope
  , tlrTimestamp :: Maybe Timestamp
  , tlrObservedTime :: Timestamp
  , tlrSeverityNumber :: Maybe SeverityNumber
  , tlrSeverityText :: Maybe Text
  , tlrBody :: Maybe LogBody
  , tlrAttributes :: Attr.Attributes
  , tlrDropped :: Int
  , tlrSpanContext :: Maybe SC.SpanContext
  }

instance ReadableLogRecord TestLogRecord where
  rlrResource = tlrResource
  rlrScope = tlrScope
  rlrTimestamp = tlrTimestamp
  rlrObservedTimestamp = tlrObservedTime
  rlrSeverityNumber = tlrSeverityNumber
  rlrSeverityText = tlrSeverityText
  rlrBody = tlrBody
  rlrAttributes = tlrAttributes
  rlrDroppedAttributes = tlrDropped
  rlrSpanContext = tlrSpanContext


-------------------------------------------------------------------------------
-- Proto conversion — AttributeValue
-------------------------------------------------------------------------------

genAttributeValue :: Gen Attr.AttributeValue
genAttributeValue = oneof
  [ Attr.StringValue <$> genText
  , Attr.BoolValue <$> arbitrary
  , Attr.Int64Value <$> arbitrary
  , Attr.Float64Value <$> arbitrary
  , Attr.StringArrayValue . V.fromList <$> listOf genText
  , Attr.BoolArrayValue . V.fromList <$> listOf arbitrary
  , Attr.Int64ArrayValue . V.fromList <$> listOf arbitrary
  , Attr.Float64ArrayValue . V.fromList <$> listOf arbitrary
  ]
  where
    genText :: Gen Text
    genText = fmap (mappend "t") $ elements ["", "a", "hello", "world"]


protoAttributeValueTests :: TestTree
protoAttributeValueTests = testGroup "Proto conversion - AttributeValue"
  [ testCase "StringValue" $ do
      let av = toProtoAnyValue (Attr.StringValue "hello")
      (av ^. CF.stringValue) @?= "hello"
  , testCase "BoolValue" $ do
      let av = toProtoAnyValue (Attr.BoolValue True)
      (av ^. CF.boolValue) @?= True
  , testCase "Int64Value" $ do
      let av = toProtoAnyValue (Attr.Int64Value 42)
      (av ^. CF.intValue) @?= 42
  , testCase "Float64Value" $ do
      let av = toProtoAnyValue (Attr.Float64Value 3.14)
      abs ((av ^. CF.doubleValue) - 3.14) < 1e-10 @?= True
  , testCase "StringArrayValue" $ do
      let av = toProtoAnyValue (Attr.StringArrayValue (V.fromList ["a", "b"]))
          arr = av ^. CF.arrayValue
      length (arr ^. CF.values) @?= 2
  , testCase "BoolArrayValue" $ do
      let av = toProtoAnyValue (Attr.BoolArrayValue (V.fromList [True, False, True]))
          arr = av ^. CF.arrayValue
      length (arr ^. CF.values) @?= 3
  , testCase "Int64ArrayValue" $ do
      let av = toProtoAnyValue (Attr.Int64ArrayValue (V.fromList [1, 2, 3, 4]))
          arr = av ^. CF.arrayValue
      length (arr ^. CF.values) @?= 4
  , testCase "Float64ArrayValue" $ do
      let av = toProtoAnyValue (Attr.Float64ArrayValue (V.fromList [1.1, 2.2]))
          arr = av ^. CF.arrayValue
      length (arr ^. CF.values) @?= 2
  , testProperty "round-trip encode/decode AnyValue" $
      forAll genAttributeValue $ \attrVal ->
        let proto = toProtoAnyValue attrVal
            encoded = encodeMessage proto
            decoded = decodeMessage encoded
         in decoded === Right proto
  ]


-------------------------------------------------------------------------------
-- Proto conversion — Attributes
-------------------------------------------------------------------------------

protoAttributesTests :: TestTree
protoAttributesTests = testGroup "Proto conversion - Attributes"
  [ testProperty "keys and values preserved" $
      forAll genAttrList $ \kvs ->
        let attrs = Attr.fromList kvs
            protoKVs = toProtoAttributes attrs
            protoKeys = map (\kv -> kv ^. CF.key) protoKVs
            inputKeys = map fst (Attr.toList attrs)
         in counterexample ("proto keys: " ++ show protoKeys) $
            counterexample ("input keys: " ++ show inputKeys) $
            length protoKVs === length (Attr.toList attrs)
            .&&. all (`elem` protoKeys) inputKeys === True
  ]
  where
    genAttrList :: Gen [(Text, Attr.AttributeValue)]
    genAttrList = do
      keys <- listOf (elements ["a", "b", "c", "d", "e", "f"])
      let uniqueKeys = Map.keys (Map.fromList (map (\k -> (k, ())) keys))
      mapM (\k -> (,) k <$> genAttributeValue) uniqueKeys


-------------------------------------------------------------------------------
-- Proto conversion — Resource
-------------------------------------------------------------------------------

protoResourceTests :: TestTree
protoResourceTests = testGroup "Proto conversion - Resource"
  [ testCase "attributes preserved" $ do
      let res = Resource.create [("service.name", Attr.StringValue "svc")] (Just "https://schema")
          proto = toProtoResource res
          kvs = proto ^. RF.attributes
      length kvs @?= 1
      let kv = first kvs
      (kv ^. CF.key) @?= "service.name"
      (kv ^. CF.value . CF.stringValue) @?= "svc"
  , testCase "schema_url is not in Resource proto" $ do
      let res = Resource.create [("k", Attr.StringValue "v")] (Just "https://schema")
          proto = toProtoResource res
          encoded = encodeMessage proto
          decoded = decodeMessage encoded :: Either String (Common.AnyValue)
      case decoded of
        Left _ -> pure ()
        Right _ -> pure ()
      let reDecoded = decodeMessage encoded :: Either String Common.InstrumentationScope
      case reDecoded of
        Left _ -> pure ()
        Right _ -> pure ()
      let kvs = proto ^. RF.attributes
      length kvs @?= 1
  ]


-------------------------------------------------------------------------------
-- Proto conversion — SpanContext fields
-------------------------------------------------------------------------------

protoSpanContextTests :: TestTree
protoSpanContextTests = testGroup "Proto conversion - SpanContext fields"
  [ testCase "traceId bytes preserved" $ do
      let proto = toProtoSpan spanWithContext
      (proto ^. TF.traceId) @?= BS.replicate 16 0xAB
  , testCase "spanId bytes preserved" $ do
      let proto = toProtoSpan spanWithContext
      (proto ^. TF.spanId) @?= BS.replicate 8 0xCD
  , testCase "flags preserved" $ do
      -- bits 0-7: W3C trace flags (sampled=1), bit 8: HAS_IS_REMOTE (0x0100)
      -- isRemote=False so IS_REMOTE (bit 9) is not set
      let proto = toProtoSpan spanWithContext
      (proto ^. TF.flags) @?= 0x0101
  , testCase "no parent yields empty parentSpanId" $ do
      let proto = toProtoSpan spanWithContext
      (proto ^. TF.parentSpanId) @?= mempty
  , testCase "parent spanId preserved" $ do
      let parentSid = SC.spanIdFromBytes (BS.replicate 8 0xEE)
          parentCtx = SC.SpanContext
            { SC.traceId = SC.traceIdFromBytes (BS.replicate 16 0xAB)
            , SC.spanId = parentSid
            , SC.traceFlags = SC.emptyTraceFlags
            , SC.traceState = TraceState.empty
            , SC._isRemote = True
            }
          s = spanWithContext { tsParentContext = Just parentCtx }
          proto = toProtoSpan s
      (proto ^. TF.parentSpanId) @?= SC.spanIdToBytes parentSid
  , testCase "traceState single entry" $ do
      let ts = TraceState.set "k" "v" TraceState.empty
          ctx = (tsContext spanWithContext) { SC.traceState = ts }
          s = spanWithContext { tsContext = ctx }
          proto = toProtoSpan s
      (proto ^. TF.traceState) @?= "k=v"
  , testCase "traceState multiple entries" $ do
      let ts = TraceState.set "b" "2" (TraceState.set "a" "1" TraceState.empty)
          ctx = (tsContext spanWithContext) { SC.traceState = ts }
          s = spanWithContext { tsContext = ctx }
          proto = toProtoSpan s
      (proto ^. TF.traceState) @?= "b=2,a=1"
  ]
  where
    spanWithContext :: TestSpan
    spanWithContext = defaultTestSpan
      { tsContext = SC.SpanContext
          { SC.traceId = SC.traceIdFromBytes (BS.replicate 16 0xAB)
          , SC.spanId = SC.spanIdFromBytes (BS.replicate 8 0xCD)
          , SC.traceFlags = SC.traceFlagsFromByte 1
          , SC.traceState = TraceState.empty
          , SC._isRemote = False
          }
      }


-------------------------------------------------------------------------------
-- Proto conversion — SpanKind enum
-------------------------------------------------------------------------------

protoSpanKindTests :: TestTree
protoSpanKindTests = testGroup "Proto conversion - SpanKind enum"
  [ testCase "Internal" $ spanKindOf Internal @?= T.Span'SPAN_KIND_INTERNAL
  , testCase "Server" $ spanKindOf Server @?= T.Span'SPAN_KIND_SERVER
  , testCase "Client" $ spanKindOf Client @?= T.Span'SPAN_KIND_CLIENT
  , testCase "Producer" $ spanKindOf Producer @?= T.Span'SPAN_KIND_PRODUCER
  , testCase "Consumer" $ spanKindOf Consumer @?= T.Span'SPAN_KIND_CONSUMER
  ]
  where
    spanKindOf :: SpanKind -> T.Span'SpanKind
    spanKindOf k =
      let s = defaultTestSpan { tsKind = k }
          proto = toProtoSpan s
       in proto ^. TF.kind


-------------------------------------------------------------------------------
-- Proto conversion — StatusCode enum
-------------------------------------------------------------------------------

protoStatusCodeTests :: TestTree
protoStatusCodeTests = testGroup "Proto conversion - StatusCode enum"
  [ testCase "Unset" $ statusCodeOf Unset @?= T.Status'STATUS_CODE_UNSET
  , testCase "Ok" $ statusCodeOf Ok @?= T.Status'STATUS_CODE_OK
  , testCase "Error" $ statusCodeOf Error @?= T.Status'STATUS_CODE_ERROR
  ]
  where
    statusCodeOf :: StatusCode -> T.Status'StatusCode
    statusCodeOf sc =
      let s = defaultTestSpan { tsStatus = SpanStatus sc Nothing }
          proto = toProtoSpan s
          st = proto ^. TF.maybe'status
       in case st of
            Just status -> status ^. TF.code
            Nothing -> T.Status'STATUS_CODE_UNSET


-------------------------------------------------------------------------------
-- Proto conversion — Span fields
-------------------------------------------------------------------------------

protoSpanFieldsTests :: TestTree
protoSpanFieldsTests = testGroup "Proto conversion - Span fields"
  [ testCase "name preserved" $ do
      let s = defaultTestSpan { tsName = "my-operation" }
          proto = toProtoSpan s
      (proto ^. TF.name) @?= "my-operation"
  , testCase "start/end timestamps preserved" $ do
      let s = defaultTestSpan { tsStart = fromNanos 5000, tsEnd = fromNanos 9000 }
          proto = toProtoSpan s
      (proto ^. TF.startTimeUnixNano) @?= 5000
      (proto ^. TF.endTimeUnixNano) @?= 9000
  , testCase "dropped counts preserved" $ do
      let s = defaultTestSpan { tsDroppedAttrs = 3, tsDroppedEvents = 7, tsDroppedLinks = 2 }
          proto = toProtoSpan s
      (proto ^. TF.droppedAttributesCount) @?= 3
      (proto ^. TF.droppedEventsCount) @?= 7
      (proto ^. TF.droppedLinksCount) @?= 2
  , testCase "events preserved" $ do
      let ev = SpanEvent
            { eventName = "my-event"
            , eventTimestamp = fromNanos 1500
            , eventAttributes = Attr.emptyAttributes
            , eventDroppedAttributesCount = 0
            }
          s = defaultTestSpan { tsEvents = [ev] }
          proto = toProtoSpan s
          protoEvents = proto ^. TF.events
      length protoEvents @?= 1
      let pe = first protoEvents
      (pe ^. TF.name) @?= "my-event"
      (pe ^. TF.timeUnixNano) @?= 1500
  , testCase "links preserved" $ do
      let linkCtx = SC.SpanContext
            { SC.traceId = SC.traceIdFromBytes (BS.replicate 16 0x11)
            , SC.spanId = SC.spanIdFromBytes (BS.replicate 8 0x22)
            , SC.traceFlags = SC.emptyTraceFlags
            , SC.traceState = TraceState.empty
            , SC._isRemote = False
            }
          lnk = Link
            { linkSpanContext = linkCtx
            , linkAttributes = Attr.emptyAttributes
            , linkDroppedAttributesCount = 0
            }
          s = defaultTestSpan { tsLinks = [lnk] }
          proto = toProtoSpan s
          protoLinks = proto ^. TF.links
      length protoLinks @?= 1
      let pl = first protoLinks
      (pl ^. TF.traceId) @?= BS.replicate 16 0x11
      (pl ^. TF.spanId) @?= BS.replicate 8 0x22
  ]


-------------------------------------------------------------------------------
-- Proto conversion — MetricPointData variants
-------------------------------------------------------------------------------

protoMetricPointDataTests :: TestTree
protoMetricPointDataTests = testGroup "Proto conversion - MetricPointData variants"
  [ testCase "SumPointData isMonotonic" $ do
      let md = mkMetricData (SumPointData (SumData [mkNdp] Delta True))
          req = metricDataToExportRequest md
          rm = first (req ^. CMF.resourceMetrics)
          sm = first (rm ^. MF.scopeMetrics)
          m = first (sm ^. MF.metrics)
          s = m ^. MF.maybe'sum
      case s of
        Just sumMsg -> do
          (sumMsg ^. MF.isMonotonic) @?= True
          length (sumMsg ^. MF.dataPoints) @?= 1
        Nothing -> assertFailure "Expected Sum data"
  , testCase "GaugePointData dataPoints count" $ do
      let md = mkMetricData (GaugePointData (GaugeData [mkNdp, mkNdp]))
          req = metricDataToExportRequest md
          rm = first (req ^. CMF.resourceMetrics)
          sm = first (rm ^. MF.scopeMetrics)
          m = first (sm ^. MF.metrics)
          g = m ^. MF.maybe'gauge
      case g of
        Just gaugeMsg -> length (gaugeMsg ^. MF.dataPoints) @?= 2
        Nothing -> assertFailure "Expected Gauge data"
  , testCase "HistogramPointData dataPoints and temporality" $ do
      let hdp = HistogramDataPoint
            { hdpAttributes = Attr.emptyAttributes
            , hdpStartTime = fromNanos 100
            , hdpTime = fromNanos 200
            , hdpCount = 10
            , hdpSum = Just 55.5
            , hdpBucketCounts = [2, 5, 3]
            , hdpExplicitBounds = [10.0, 50.0]
            , hdpMin = Just 1.0
            , hdpMax = Just 99.0
            , hdpExemplars = []
            }
          md = mkMetricData (HistogramPointData (HistogramData [hdp] Cumulative))
          req = metricDataToExportRequest md
          rm = first (req ^. CMF.resourceMetrics)
          sm = first (rm ^. MF.scopeMetrics)
          m = first (sm ^. MF.metrics)
          h = m ^. MF.maybe'histogram
      case h of
        Just histMsg -> do
          length (histMsg ^. MF.dataPoints) @?= 1
          (histMsg ^. MF.aggregationTemporality) @?= M.AGGREGATION_TEMPORALITY_CUMULATIVE
        Nothing -> assertFailure "Expected Histogram data"
  , testCase "ExponentialHistogramPointData dataPoints count" $ do
      let ehdp = ExponentialHistogramDataPoint
            { ehdpAttributes = Attr.emptyAttributes
            , ehdpStartTime = fromNanos 100
            , ehdpTime = fromNanos 200
            , ehdpCount = 5
            , ehdpSum = Nothing
            , ehdpScale = 3
            , ehdpZeroCount = 1
            , ehdpZeroThreshold = 0.0
            , ehdpPositive = ExponentialBuckets 0 [1, 2]
            , ehdpNegative = ExponentialBuckets 0 [1]
            , ehdpMin = Nothing
            , ehdpMax = Nothing
            , ehdpExemplars = []
            }
          md = mkMetricData (ExponentialHistogramPointData (ExponentialHistogramData [ehdp] Delta))
          req = metricDataToExportRequest md
          rm = first (req ^. CMF.resourceMetrics)
          sm = first (rm ^. MF.scopeMetrics)
          m = first (sm ^. MF.metrics)
          eh = m ^. MF.maybe'exponentialHistogram
      case eh of
        Just expHistMsg -> length (expHistMsg ^. MF.dataPoints) @?= 1
        Nothing -> assertFailure "Expected ExponentialHistogram data"
  , testProperty "AggregationTemporality mapping" $
      forAll (elements [Delta, Cumulative]) $ \temp ->
        let md = mkMetricData (SumPointData (SumData [mkNdp] temp False))
            req = metricDataToExportRequest md
            rm = first (req ^. CMF.resourceMetrics)
            sm = first (rm ^. MF.scopeMetrics)
            m = first (sm ^. MF.metrics)
            expected = case temp of
              Delta -> M.AGGREGATION_TEMPORALITY_DELTA
              Cumulative -> M.AGGREGATION_TEMPORALITY_CUMULATIVE
         in case m ^. MF.maybe'sum of
              Just sumMsg -> (sumMsg ^. MF.aggregationTemporality) === expected
              Nothing -> property False
  ]
  where
    mkNdp :: NumberDataPoint
    mkNdp = NumberDataPoint
      { ndpAttributes = Attr.emptyAttributes
      , ndpStartTime = fromNanos 100
      , ndpTime = fromNanos 200
      , ndpValue = 1.0
      , ndpExemplars = []
      }

    mkMetricData :: MetricPointData -> MetricData
    mkMetricData mpd = MetricData
      { mdResource = Resource.empty
      , mdScopeMetrics =
          [ ScopeMetrics
              { smScope = Attr.InstrumentationScope "test" Nothing Nothing Nothing
              , smMetrics =
                  [ Metric
                      { metricName = "test-metric"
                      , metricDescription = ""
                      , metricUnit = ""
                      , metricPointData = mpd
                      }
                  ]
              }
          ]
      }


-------------------------------------------------------------------------------
-- Proto conversion — SeverityNumber
-------------------------------------------------------------------------------

protoSeverityNumberTests :: TestTree
protoSeverityNumberTests = testGroup "Proto conversion - SeverityNumber"
  [ testProperty "severity number value matches proto enum" $
      forAll (elements [minBound .. maxBound :: SeverityNumber]) $ \sn ->
        let rec = TestLogRecord
              { tlrResource = Resource.empty
              , tlrScope = Attr.InstrumentationScope "test" Nothing Nothing Nothing
              , tlrTimestamp = Nothing
              , tlrObservedTime = fromNanos 1000
              , tlrSeverityNumber = Just sn
              , tlrSeverityText = Nothing
              , tlrBody = Nothing
              , tlrAttributes = Attr.emptyAttributes
              , tlrDropped = 0
              , tlrSpanContext = Nothing
              }
            req = logListToExportRequest [SomeReadableLogRecord rec]
            rl = first (req ^. CLF.resourceLogs)
            sl = first (rl ^. LF.scopeLogs)
            lr = first (sl ^. LF.logRecords)
            protoSev = fromEnum (lr ^. LF.severityNumber)
         in protoSev === severityNumberValue sn
  ]


-------------------------------------------------------------------------------
-- Proto conversion — LogBody variants
-------------------------------------------------------------------------------

protoLogBodyTests :: TestTree
protoLogBodyTests = testGroup "Proto conversion - LogBody variants"
  [ testCase "LogBodyString" $ do
      let av = logBodyToAnyValue (LogBodyString "hello")
      (av ^. CF.stringValue) @?= "hello"
  , testCase "LogBodyBool" $ do
      let av = logBodyToAnyValue (LogBodyBool True)
      (av ^. CF.boolValue) @?= True
  , testCase "LogBodyInt64" $ do
      let av = logBodyToAnyValue (LogBodyInt64 99)
      (av ^. CF.intValue) @?= 99
  , testCase "LogBodyFloat64" $ do
      let av = logBodyToAnyValue (LogBodyFloat64 2.5)
      (av ^. CF.doubleValue) @?= 2.5
  , testCase "LogBodyBytes" $ do
      let av = logBodyToAnyValue (LogBodyBytes (BS.pack [1, 2, 3]))
      (av ^. CF.bytesValue) @?= BS.pack [1, 2, 3]
  , testCase "LogBodyList" $ do
      let av = logBodyToAnyValue (LogBodyList [LogBodyString "x"])
          arr = av ^. CF.arrayValue
      length (arr ^. CF.values) @?= 1
  , testCase "LogBodyMap" $ do
      let av = logBodyToAnyValue (LogBodyMap (Map.fromList [("k", LogBodyString "v")]))
          kvlist = av ^. CF.kvlistValue
      length (kvlist ^. CF.values) @?= 1
  ]
  where
    logBodyToAnyValue :: LogBody -> Common.AnyValue
    logBodyToAnyValue body =
      let rec = TestLogRecord
            { tlrResource = Resource.empty
            , tlrScope = Attr.InstrumentationScope "test" Nothing Nothing Nothing
            , tlrTimestamp = Nothing
            , tlrObservedTime = fromNanos 1000
            , tlrSeverityNumber = Nothing
            , tlrSeverityText = Nothing
            , tlrBody = Just body
            , tlrAttributes = Attr.emptyAttributes
            , tlrDropped = 0
            , tlrSpanContext = Nothing
            }
          req = logListToExportRequest [SomeReadableLogRecord rec]
          rl = first (req ^. CLF.resourceLogs)
          sl = first (rl ^. LF.scopeLogs)
          lr = first (sl ^. LF.logRecords)
       in case lr ^. LF.maybe'body of
            Just av -> av
            Nothing -> defMessage


-------------------------------------------------------------------------------
-- Proto conversion — spanListToExportRequest grouping
-------------------------------------------------------------------------------

protoSpanGroupingTests :: TestTree
protoSpanGroupingTests = testGroup "Proto conversion - spanListToExportRequest grouping"
  [ testCase "single span" $ do
      let req = spanListToExportRequest [SomeReadableSpan defaultTestSpan]
          rss = req ^. CTF.resourceSpans
      length rss @?= 1
      let rs = first rss
      length (rs ^. TF.scopeSpans) @?= 1
      let ss = first (rs ^. TF.scopeSpans)
      length (ss ^. TF.spans) @?= 1
  , testCase "two spans same resource+scope" $ do
      let s1 = defaultTestSpan { tsName = "span1" }
          s2 = defaultTestSpan { tsName = "span2" }
          req = spanListToExportRequest [SomeReadableSpan s1, SomeReadableSpan s2]
          rss = req ^. CTF.resourceSpans
      length rss @?= 1
      let rs = first rss
      length (rs ^. TF.scopeSpans) @?= 1
      let ss = first (rs ^. TF.scopeSpans)
      length (ss ^. TF.spans) @?= 2
  , testCase "two spans same resource different scope" $ do
      let scope1 = Attr.InstrumentationScope "lib-a" Nothing Nothing Nothing
          scope2 = Attr.InstrumentationScope "lib-b" Nothing Nothing Nothing
          s1 = defaultTestSpan { tsScope = scope1 }
          s2 = defaultTestSpan { tsScope = scope2 }
          req = spanListToExportRequest [SomeReadableSpan s1, SomeReadableSpan s2]
          rss = req ^. CTF.resourceSpans
      length rss @?= 1
      let rs = first rss
      length (rs ^. TF.scopeSpans) @?= 2
  , testCase "two spans different resources" $ do
      let res1 = Resource.create [("svc", Attr.StringValue "a")] Nothing
          res2 = Resource.create [("svc", Attr.StringValue "b")] Nothing
          s1 = defaultTestSpan { tsResource = res1 }
          s2 = defaultTestSpan { tsResource = res2 }
          req = spanListToExportRequest [SomeReadableSpan s1, SomeReadableSpan s2]
          rss = req ^. CTF.resourceSpans
      length rss @?= 2
  , testCase "empty list" $ do
      let req = spanListToExportRequest []
          rss = req ^. CTF.resourceSpans
      length rss @?= 0
  ]


-------------------------------------------------------------------------------
-- Proto conversion — response parsing
-------------------------------------------------------------------------------

protoResponseParsingTests :: TestTree
protoResponseParsingTests = testGroup "Proto conversion - response parsing"
  [ testCase "parseTraceExportResult invalid bytes -> ExportFailure" $ do
      r <- parseTraceExportResult (BS.pack [0xFF, 0xFF, 0xFF])
      r @?= ExportFailure
  , testCase "parseTraceExportResult valid response -> ExportSuccess" $ do
      let encoded = encodeMessage (defMessage :: CollectorTrace.ExportTraceServiceResponse)
      r <- parseTraceExportResult encoded
      r @?= ExportSuccess
  , testCase "parseTraceExportResult empty bytes -> ExportSuccess" $ do
      r <- parseTraceExportResult BS.empty
      r @?= ExportSuccess
  , testCase "parseMetricsExportResult invalid bytes -> ExportFailure" $ do
      r <- parseMetricsExportResult (BS.pack [0xFF, 0xFF, 0xFF])
      r @?= ExportFailure
  , testCase "parseMetricsExportResult valid response -> ExportSuccess" $ do
      let encoded = encodeMessage (defMessage :: CollectorMetrics.ExportMetricsServiceResponse)
      r <- parseMetricsExportResult encoded
      r @?= ExportSuccess
  , testCase "parseLogsExportResult invalid bytes -> ExportFailure" $ do
      r <- parseLogsExportResult (BS.pack [0xFF, 0xFF, 0xFF])
      r @?= ExportFailure
  , testCase "parseLogsExportResult valid response -> ExportSuccess" $ do
      let encoded = encodeMessage (defMessage :: CollectorLogs.ExportLogsServiceResponse)
      r <- parseLogsExportResult encoded
      r @?= ExportSuccess
  ]


-------------------------------------------------------------------------------
-- OTLP/gRPC exporter — default config
-------------------------------------------------------------------------------

defaultConfigTests :: TestTree
defaultConfigTests = testGroup "OTLP/gRPC exporter - default config"
  [ testCase "endpoint is localhost:4317" $
      otlpEndpoint defaultOtlpGrpcConfig @?= "localhost:4317"
  , testCase "timeout is 10000ms" $
      otlpTimeoutMs defaultOtlpGrpcConfig @?= 10000
  , testCase "compression is NoCompression" $
      otlpCompression defaultOtlpGrpcConfig @?= NoCompression
  , testCase "tls is Nothing" $
      case otlpTls defaultOtlpGrpcConfig of
        Nothing -> pure ()
        Just _  -> assertFailure "Expected Nothing for default TLS config"
  , testCase "headers is empty" $
      otlpHeaders defaultOtlpGrpcConfig @?= []
  ]


-------------------------------------------------------------------------------
-- OTLP/gRPC exporter — shutdown behaviour
-------------------------------------------------------------------------------

shutdownBehaviourTests :: TestTree
shutdownBehaviourTests = testGroup "OTLP/gRPC exporter - shutdown behaviour"
  [ testCase "exportSpans returns ExportFailure after shutdown" $ do
      e <- newOtlpGrpcSpanExporter defaultOtlpGrpcConfig
      _ <- shutdownExporter e
      result <- exportSpans e [SomeReadableSpan defaultTestSpan]
      result @?= ExportFailure
  , testCase "exportMetrics returns ExportFailure after shutdown" $ do
      e <- newOtlpGrpcMetricExporter defaultOtlpGrpcConfig
      _ <- shutdownMetricExporter e
      let md = MetricData
            { mdResource = Resource.empty
            , mdScopeMetrics = []
            }
      result <- exportMetrics e md
      result @?= ExportFailure
  , testCase "exportLogRecords returns ExportFailure after shutdown" $ do
      e <- newOtlpGrpcLogRecordExporter defaultOtlpGrpcConfig
      _ <- shutdownLogExporter e
      result <- exportLogRecords e []
      result @?= ExportFailure
  , testCase "forceFlushExporter returns Right ()" $ do
      e <- newOtlpGrpcSpanExporter defaultOtlpGrpcConfig
      result <- forceFlushExporter e Nothing
      case result of
        Right () -> pure ()
        Left _   -> assertFailure "Expected Right () from forceFlushExporter"
  ]


-------------------------------------------------------------------------------
-- OTLP/gRPC exporter — env var resolution
-------------------------------------------------------------------------------

-- | Unset all OTEL env vars that could affect exporter configuration before a test.
cleanOtelEnv :: IO a -> IO a
cleanOtelEnv action = do
  let otelKeys =
        [ "OTEL_EXPORTER_OTLP_ENDPOINT"
        , "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT"
        , "OTEL_EXPORTER_OTLP_METRICS_ENDPOINT"
        , "OTEL_EXPORTER_OTLP_LOGS_ENDPOINT"
        , "OTEL_EXPORTER_OTLP_HEADERS"
        , "OTEL_EXPORTER_OTLP_TRACES_HEADERS"
        , "OTEL_EXPORTER_OTLP_COMPRESSION"
        , "OTEL_EXPORTER_OTLP_TRACES_COMPRESSION"
        , "OTEL_EXPORTER_OTLP_TIMEOUT"
        , "OTEL_EXPORTER_OTLP_TRACES_TIMEOUT"
        ]
  saved <- mapM (\k -> do old <- lookupEnv k; unsetEnv k; pure (k, old)) otelKeys
  result <- action
  mapM_ (\(k, old) -> case old of { Nothing -> unsetEnv k; Just v -> setEnv k v }) saved
  pure result


-- | Run an action with a set of environment variables, restoring originals afterward.
withEnv :: [(String, String)] -> IO a -> IO a
withEnv pairs action = bracket
  (mapM (\(k, v) -> do old <- lookupEnv k; setEnv k v; pure (k, old)) pairs)
  (mapM_ (\(k, old) -> case old of { Nothing -> unsetEnv k; Just v -> setEnv k v }))
  (const action)


envVarResolutionTests :: TestTree
envVarResolutionTests = dependentTestGroup "OTLP/gRPC exporter - env var resolution" AllFinish
  [ testCase "default config has correct endpoint and timeout" $ do
      e <- newOtlpGrpcSpanExporter defaultOtlpGrpcConfig
      e._spanClientConfig.grpcHost @?= "localhost"
      e._spanClientConfig.grpcPort @?= 4317
      e._spanClientConfig.grpcTimeoutMicros @?= Just 10_000_000

  , testCase "generic endpoint env var overrides host and port" $
      cleanOtelEnv $
        withEnv [("OTEL_EXPORTER_OTLP_ENDPOINT", "http://myhost:9999")] $ do
          e <- newOtlpGrpcSpanExporterFromEnv
          e._spanClientConfig.grpcHost @?= "myhost"
          e._spanClientConfig.grpcPort @?= 9999
          e._spanClientConfig.grpcTls  @?= Nothing

  , testCase "signal-specific endpoint overrides generic" $
      cleanOtelEnv $
        withEnv [ ("OTEL_EXPORTER_OTLP_ENDPOINT",        "http://generic:1111")
                , ("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT", "http://specific:2222")
                ] $ do
          e <- newOtlpGrpcSpanExporterFromEnv
          e._spanClientConfig.grpcHost @?= "specific"
          e._spanClientConfig.grpcPort @?= 2222

  , testCase "signal-specific does not bleed across signals" $
      cleanOtelEnv $
        withEnv [("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT", "http://traces-host:1234")] $ do
          spanE   <- newOtlpGrpcSpanExporterFromEnv
          metricE <- newOtlpGrpcMetricExporterFromEnv
          spanE._spanClientConfig.grpcHost     @?= "traces-host"
          metricE._metricClientConfig.grpcHost @?= "localhost"

  , testCase "https scheme sets TLS config" $
      cleanOtelEnv $
        withEnv [("OTEL_EXPORTER_OTLP_ENDPOINT", "https://secure-host:4317")] $ do
          e <- newOtlpGrpcSpanExporterFromEnv
          e._spanClientConfig.grpcHost @?= "secure-host"
          case e._spanClientConfig.grpcTls of
            Nothing  -> assertFailure "Expected TLS config for https:// endpoint"
            Just tls -> tls.tlsSkipVerify @?= False

  , testCase "compression env var sets GzipCompression" $
      cleanOtelEnv $
        withEnv [("OTEL_EXPORTER_OTLP_COMPRESSION", "gzip")] $ do
          e <- newOtlpGrpcSpanExporterFromEnv
          e._spanClientConfig.grpcCompression @?= GzipCompression

  , testCase "unknown compression env var defaults to NoCompression" $
      cleanOtelEnv $
        withEnv [("OTEL_EXPORTER_OTLP_COMPRESSION", "snappy")] $ do
          e <- newOtlpGrpcSpanExporterFromEnv
          e._spanClientConfig.grpcCompression @?= NoCompression

  , testCase "timeout env var sets grpcTimeoutMicros" $
      cleanOtelEnv $
        withEnv [("OTEL_EXPORTER_OTLP_TIMEOUT", "5000")] $ do
          e <- newOtlpGrpcSpanExporterFromEnv
          e._spanClientConfig.grpcTimeoutMicros @?= Just 5_000_000

  , testCase "signal-specific timeout overrides generic timeout" $
      cleanOtelEnv $
        withEnv [ ("OTEL_EXPORTER_OTLP_TIMEOUT",        "3000")
                , ("OTEL_EXPORTER_OTLP_TRACES_TIMEOUT", "7000")
                ] $ do
          e <- newOtlpGrpcSpanExporterFromEnv
          e._spanClientConfig.grpcTimeoutMicros @?= Just 7_000_000

  , testCase "invalid timeout env var falls back to default" $
      cleanOtelEnv $
        withEnv [("OTEL_EXPORTER_OTLP_TIMEOUT", "not-a-number")] $ do
          e <- newOtlpGrpcSpanExporterFromEnv
          e._spanClientConfig.grpcTimeoutMicros @?= Just 10_000_000
  ]


-------------------------------------------------------------------------------
-- OTLP/gRPC exporter — integration: mock gRPC server
-------------------------------------------------------------------------------

-- | Read all body chunks from an HTTP/2 server request.
readAllChunks :: SemS.Request -> IO ByteString
readAllChunks req = go mempty
  where
    go acc = do
      chunk <- SemS.getRequestBodyChunk req
      if BS.null chunk then pure acc else go (acc <> chunk)


-- | A TrailersMaker that sends grpc-status: 0 as trailers.
grpcOkTrailersMaker :: Sem.TrailersMaker
grpcOkTrailersMaker Nothing = pure $ Sem.Trailers [("grpc-status", "0")]
grpcOkTrailersMaker (Just _) = pure $ Sem.NextTrailersMaker grpcOkTrailersMaker


-- | Build a gRPC-OK response with proper trailers.
-- Uses responseStreaming so that the HTTP/2 library sends a separate
-- trailers HEADERS frame after the body, rather than combining everything
-- into a single HEADERS+END_STREAM.
grpcOkResponse :: SemS.Response
grpcOkResponse =
  SemS.setResponseTrailersMaker
    (SemS.responseStreaming ok200
      [("content-type", "application/grpc+proto")]
      (\write flush -> do
        -- Write a valid empty gRPC frame (5-byte header, 0-length body)
        write (Builder.byteString (BS.pack [0, 0, 0, 0, 0]))
        flush))
    grpcOkTrailersMaker


-- | Accept a single connection and run an HTTP/2 server that captures request bodies.
acceptAndServe :: Maybe (IORef ByteString) -> NS.Socket -> TVar (Maybe ByteString) -> IO ()
acceptAndServe mRawRef lSock received = do
  (conn, _) <- NS.accept lSock
  bracket (H2S.allocSimpleConfig conn 4096) H2S.freeSimpleConfig $ \h2cfg -> do
    doneRef <- newIORef False
    H2S.run H2S.defaultServerConfig h2cfg $ \req _aux respond -> do
      alreadyDone <- readIORef doneRef
      if alreadyDone
        then do
          -- Drain the request body to avoid blocking
          _ <- readAllChunks req
          respond grpcOkResponse []
        else do
          writeIORef doneRef True
          body <- readAllChunks req
          -- Optionally store raw body (including gRPC frame header)
          case mRawRef of
            Just ref -> writeIORef ref body
            Nothing  -> pure ()
          -- Skip 5-byte gRPC data frame header to get the proto bytes
          let protoBytes = if BS.length body >= 5 then BS.drop 5 body else body
          atomically (writeTVar received (Just protoBytes))
          respond grpcOkResponse []
    -- After H2S.run returns, close the accepted connection
    NS.close conn


-- | Run an action with a mock gRPC server on a random port.
-- The server captures the raw proto bytes of the first request it receives.
withMockGrpcServer :: TVar (Maybe ByteString) -> (Int -> IO a) -> IO a
withMockGrpcServer received action =
  withMockGrpcServerRaw' Nothing received action


-- | Like withMockGrpcServer but also captures the raw body (with frame header).
withMockGrpcServerRaw :: IORef ByteString -> TVar (Maybe ByteString) -> (Int -> IO a) -> IO a
withMockGrpcServerRaw rawRef received action =
  withMockGrpcServerRaw' (Just rawRef) received action


withMockGrpcServerRaw' :: Maybe (IORef ByteString) -> TVar (Maybe ByteString) -> (Int -> IO a) -> IO a
withMockGrpcServerRaw' mRawRef received action = do
  bracket openListenSocket NS.close $ \lSock -> do
    port <- fromIntegral <$> NS.socketPort lSock
    -- The socket is already listening, so the OS will accept connections
    -- even before the server thread calls accept.
    withAsync (acceptAndServe mRawRef lSock received) $ \_serverThread ->
      action port


openListenSocket :: IO NS.Socket
openListenSocket = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addr : _ <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  sock <- NS.openSocket addr
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.bind sock (NS.addrAddress addr)
  NS.listen sock 1
  pure sock




makeTestSpan :: Text -> TestSpan
makeTestSpan name = defaultTestSpan { tsName = name }


integrationTests :: TestTree
integrationTests = testGroup "OTLP/gRPC exporter - integration: mock gRPC server"
  [ testCase "span export round-trip via mock gRPC server" $ do
      received <- newTVarIO Nothing
      withMockGrpcServer received $ \port -> do
        e <- newOtlpGrpcSpanExporter defaultOtlpGrpcConfig
               { otlpEndpoint = T.pack ("127.0.0.1:" <> show port)
               , otlpTimeoutMs = 3000
               }
        let testSpan = makeTestSpan "integration-span-name"
        _result <- exportSpans e [SomeReadableSpan testSpan]
        -- Wait for server to write the captured bytes
        mProtoBytes <- timeout 5_000_000 $ atomically $ do
          v <- readTVar received
          maybe retry pure v
        protoBytes <- case mProtoBytes of
          Nothing -> assertFailure "Timed out waiting for mock server to receive request" >> pure mempty
          Just bs -> pure bs
        -- Decode and verify the proto
        case decodeMessage protoBytes :: Either String CollectorTrace.ExportTraceServiceRequest of
          Left err -> assertFailure ("Could not decode ExportTraceServiceRequest: " <> err)
          Right req -> do
            let spans = concatMap
                  (\rs -> concatMap (\ss -> ss ^. TF.spans) (rs ^. TF.scopeSpans))
                  (req ^. CTF.resourceSpans)
            length spans @?= 1
            (first spans ^. TF.name) @?= "integration-span-name"
  , testCase "metric export round-trip via mock gRPC server" $ do
      received <- newTVarIO Nothing
      withMockGrpcServer received $ \port -> do
        e <- newOtlpGrpcMetricExporter defaultOtlpGrpcConfig
               { otlpEndpoint = T.pack ("127.0.0.1:" <> show port)
               , otlpTimeoutMs = 3000
               }
        let md = MetricData
              { mdResource = Resource.empty
              , mdScopeMetrics =
                  [ ScopeMetrics
                      { smScope = Attr.InstrumentationScope "test" Nothing Nothing Nothing
                      , smMetrics =
                          [ Metric
                              { metricName = "integration-metric"
                              , metricDescription = ""
                              , metricUnit = ""
                              , metricPointData = GaugePointData (GaugeData
                                  [ NumberDataPoint
                                      { ndpAttributes = Attr.emptyAttributes
                                      , ndpStartTime = fromNanos 100
                                      , ndpTime = fromNanos 200
                                      , ndpValue = 42.0
                                      , ndpExemplars = []
                                      }
                                  ])
                              }
                          ]
                      }
                  ]
              }
        _result <- exportMetrics e md
        mProtoBytes <- timeout 5_000_000 $ atomically $ do
          v <- readTVar received
          maybe retry pure v
        protoBytes <- case mProtoBytes of
          Nothing -> assertFailure "Timed out waiting for mock server to receive request" >> pure mempty
          Just bs -> pure bs
        case decodeMessage protoBytes :: Either String CollectorMetrics.ExportMetricsServiceRequest of
          Left err -> assertFailure ("Could not decode ExportMetricsServiceRequest: " <> err)
          Right req -> do
            let rms = req ^. CMF.resourceMetrics
            length rms @?= 1
            let rm = first rms
                sm = first (rm ^. MF.scopeMetrics)
                m = first (sm ^. MF.metrics)
            (m ^. MF.name) @?= "integration-metric"
  , testCase "log export round-trip via mock gRPC server" $ do
      received <- newTVarIO Nothing
      withMockGrpcServer received $ \port -> do
        e <- newOtlpGrpcLogRecordExporter defaultOtlpGrpcConfig
               { otlpEndpoint = T.pack ("127.0.0.1:" <> show port)
               , otlpTimeoutMs = 3000
               }
        let logRec = TestLogRecord
              { tlrResource = Resource.empty
              , tlrScope = Attr.InstrumentationScope "test" Nothing Nothing Nothing
              , tlrTimestamp = Nothing
              , tlrObservedTime = fromNanos 1000
              , tlrSeverityNumber = Nothing
              , tlrSeverityText = Nothing
              , tlrBody = Just (LogBodyString "integration-log-body")
              , tlrAttributes = Attr.emptyAttributes
              , tlrDropped = 0
              , tlrSpanContext = Nothing
              }
        _result <- exportLogRecords e [SomeReadableLogRecord logRec]
        mProtoBytes <- timeout 5_000_000 $ atomically $ do
          v <- readTVar received
          maybe retry pure v
        protoBytes <- case mProtoBytes of
          Nothing -> assertFailure "Timed out waiting for mock server to receive request" >> pure mempty
          Just bs -> pure bs
        case decodeMessage protoBytes :: Either String CollectorLogs.ExportLogsServiceRequest of
          Left err -> assertFailure ("Could not decode ExportLogsServiceRequest: " <> err)
          Right req -> do
            let rls = req ^. CLF.resourceLogs
            length rls @?= 1
            let rl = first rls
                sl = first (rl ^. LF.scopeLogs)
                lr = first (sl ^. LF.logRecords)
            case lr ^. LF.maybe'body of
              Just av -> (av ^. CF.stringValue) @?= "integration-log-body"
              Nothing -> assertFailure "Expected log body"
  ]


-------------------------------------------------------------------------------
-- TLS transport tests
-------------------------------------------------------------------------------

tlsTransportTests :: TestTree
tlsTransportTests = testGroup "TLS transport"
  [ testCase "TLS stub is gone — connection refused yields GrpcNetworkError" $ do
      -- Open a listening socket to get a port, then close it so connections
      -- are refused — we want to confirm the old stub error is gone.
      port <- bracket openListenSocket NS.close $ \sock ->
        fromIntegral <$> NS.socketPort sock
      let cfg = defaultGrpcClientConfig
            { grpcHost = "127.0.0.1"
            , grpcPort = port
            , grpcTls = Just (TlsConfig { tlsCaStore = Nothing, tlsSkipVerify = True })
            }
      result <- unaryRpc cfg "/test" BS.empty
      case result of
        Left (GrpcProtocolError msg)
          | "not yet implemented" `T.isInfixOf` msg ->
              assertFailure "stub error still present"
        _ -> pure ()

  , testCase "TLS against plaintext server yields network error not stub error" $ do
      received <- newTVarIO Nothing
      withMockGrpcServer received $ \port -> do
        let cfg = defaultGrpcClientConfig
              { grpcHost = "127.0.0.1"
              , grpcPort = port
              , grpcTls = Just (TlsConfig Nothing True)
              }
        result <- unaryRpc cfg "/opentelemetry.proto.collector.trace.v1.TraceService/Export" BS.empty
        case result of
          Left (GrpcProtocolError msg)
            | "not yet implemented" `T.isInfixOf` msg ->
                assertFailure "stub error still present"
          _ -> pure ()
  ]


-- Helpers

first :: [a] -> a
first (x : _) = x
first [] = error "first: empty list"

word32At :: ByteString -> Int -> Word32
word32At bs i =
  let b0 = fromIntegral (BS.index bs i) :: Word32
      b1 = fromIntegral (BS.index bs (i + 1)) :: Word32
      b2 = fromIntegral (BS.index bs (i + 2)) :: Word32
      b3 = fromIntegral (BS.index bs (i + 3)) :: Word32
   in (b0 `shiftL` 24) + (b1 `shiftL` 16) + (b2 `shiftL` 8) + b3
