module Main where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Exception (bracket)
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.ProtoLens (decodeMessage)
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Family2 ((^.))
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB
import OTel.Attribute qualified as Attr
import OTel.Exporter.OTLP.GRPC.Internal (Compression (..))
import OTel.Exporter.OTLP.HTTP
import OTel.Log (LogBody (..), SeverityNumber)
import OTel.SDK.Export (ExportResult (..))
import OTel.SDK.Log.Export (LogRecordExporter (..), ReadableLogRecord (..), SomeReadableLogRecord (..))
import OTel.SDK.Metric.Export
import OTel.SDK.Resource qualified as Resource
import OTel.SDK.Trace.Export (Link (..), ReadableSpan (..), SomeReadableSpan (..), SpanEvent (..), SpanExporter (..))
import OTel.Timestamp (Timestamp (..), fromNanos)
import OTel.Trace (SpanKind (..), SpanStatus (..), StatusCode (..))
import OTel.Trace.SpanContext qualified as SC
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as CollectorTrace
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as CTF
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as TF
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "otel-exporter-otlp-http"
  [ defaultConfigTests
  , shutdownTests
  , protoBodyTests
  , jsonBodyTests
  , retryTests
  , integrationTests
  , partialSuccessTests
  ]


-------------------------------------------------------------------------------
-- 1. Default config tests (pure, no network)
-------------------------------------------------------------------------------

defaultConfigTests :: TestTree
defaultConfigTests = testGroup "default config"
  [ testCase "endpoint is http://localhost:4318" $
      otlpHttpEndpoint defaultOtlpHttpConfig @?= "http://localhost:4318"
  , testCase "timeout is 10000ms" $
      otlpHttpTimeoutMs defaultOtlpHttpConfig @?= 10000
  , testCase "content type is Protobuf" $
      otlpHttpContentType defaultOtlpHttpConfig @?= Protobuf
  , testCase "compression is NoCompression" $
      otlpHttpCompression defaultOtlpHttpConfig @?= NoCompression
  ]


-------------------------------------------------------------------------------
-- 2. Shutdown tests (create exporter, no real network needed)
-------------------------------------------------------------------------------

-- | Config pointing at a port that refuses connections instantly.
refusedConfig :: OtlpHttpConfig
refusedConfig = defaultOtlpHttpConfig { otlpHttpEndpoint = "http://127.0.0.1:1" }


shutdownTests :: TestTree
shutdownTests = testGroup "shutdown behaviour"
  [ testCase "exportSpans returns ExportFailure after shutdown" $ do
      e <- newOtlpHttpSpanExporter refusedConfig
      _ <- shutdownExporter e
      result <- exportSpans e [SomeReadableSpan defaultTestSpan]
      result @?= ExportFailure
  , testCase "exportMetrics returns ExportFailure after shutdown" $ do
      e <- newOtlpHttpMetricExporter refusedConfig
      _ <- shutdownMetricExporter e
      let md = MetricData
            { mdResource = Resource.empty
            , mdScopeMetrics = []
            }
      result <- exportMetrics e md
      result @?= ExportFailure
  , testCase "exportLogRecords returns ExportFailure after shutdown" $ do
      e <- newOtlpHttpLogRecordExporter refusedConfig
      _ <- shutdownLogExporter e
      result <- exportLogRecords e [SomeReadableLogRecord defaultTestLogRecord]
      result @?= ExportFailure
  , testCase "forceFlushExporter returns Right ()" $ do
      e <- newOtlpHttpSpanExporter refusedConfig
      result <- forceFlushExporter e Nothing
      case result of
        Right () -> pure ()
        Left _   -> assertFailure "Expected Right () from forceFlushExporter"
  ]


-------------------------------------------------------------------------------
-- 3. Proto body tests (mock HTTP server)
-------------------------------------------------------------------------------

protoBodyTests :: TestTree
protoBodyTests = testGroup "protobuf content type"
  [ testCase "request has Content-Type application/x-protobuf" $ do
      captured <- newTVarIO Nothing
      withMockHttpServer captured $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpContentType = Protobuf
              , otlpHttpTimeoutMs = 5000
              }
        e <- newOtlpHttpSpanExporter cfg
        _result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        mCap <- timeout 5_000_000 $ atomically $ do
          v <- readTVar captured
          maybe retry pure v
        case mCap of
          Nothing -> assertFailure "Timed out waiting for mock server"
          Just (hdrs, _body) -> do
            let ct = findHeader "content-type" hdrs
            ct @?= Just "application/x-protobuf"
  , testCase "body decodes as ExportTraceServiceRequest with 1 span" $ do
      captured <- newTVarIO Nothing
      withMockHttpServer captured $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpContentType = Protobuf
              , otlpHttpTimeoutMs = 5000
              }
        e <- newOtlpHttpSpanExporter cfg
        _result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        mCap <- timeout 5_000_000 $ atomically $ do
          v <- readTVar captured
          maybe retry pure v
        case mCap of
          Nothing -> assertFailure "Timed out waiting for mock server"
          Just (_hdrs, body) ->
            case decodeMessage body :: Either String CollectorTrace.ExportTraceServiceRequest of
              Left err -> assertFailure ("Could not decode proto: " <> err)
              Right req -> do
                let spans = concatMap
                      (\rs -> concatMap (\ss -> ss ^. TF.spans) (rs ^. TF.scopeSpans))
                      (req ^. CTF.resourceSpans)
                length spans @?= 1
  ]


-------------------------------------------------------------------------------
-- 4. JSON body tests (mock HTTP server)
-------------------------------------------------------------------------------

jsonBodyTests :: TestTree
jsonBodyTests = testGroup "JSON content type"
  [ testCase "request has Content-Type application/json" $ do
      captured <- newTVarIO Nothing
      withMockHttpServer captured $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpContentType = Json
              , otlpHttpTimeoutMs = 5000
              }
        e <- newOtlpHttpSpanExporter cfg
        _result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        mCap <- timeout 5_000_000 $ atomically $ do
          v <- readTVar captured
          maybe retry pure v
        case mCap of
          Nothing -> assertFailure "Timed out waiting for mock server"
          Just (hdrs, _body) -> do
            let ct = findHeader "content-type" hdrs
            ct @?= Just "application/json"
  , testCase "body is valid JSON with resourceSpans key" $ do
      captured <- newTVarIO Nothing
      withMockHttpServer captured $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpContentType = Json
              , otlpHttpTimeoutMs = 5000
              }
        e <- newOtlpHttpSpanExporter cfg
        _result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        mCap <- timeout 5_000_000 $ atomically $ do
          v <- readTVar captured
          maybe retry pure v
        case mCap of
          Nothing -> assertFailure "Timed out waiting for mock server"
          Just (_hdrs, body) ->
            case A.eitherDecode (BL.fromStrict body) :: Either String A.Value of
              Left err -> assertFailure ("Body is not valid JSON: " <> err)
              Right val ->
                case val of
                  A.Object obj ->
                    assertBool "Missing resourceSpans key" $
                      AKM.member (AK.fromString "resourceSpans") obj
                  _ -> assertFailure "Expected JSON object at top level"
  ]


-------------------------------------------------------------------------------
-- 5. Retry tests (mock HTTP server with controlled responses)
-------------------------------------------------------------------------------

retryTests :: TestTree
retryTests = testGroup "retry behaviour"
  [ testCase "429 triggers retry, eventually succeeds" $ do
      counter <- newTVarIO (0 :: Int)
      -- First request gets 429, second gets 200
      withMockHttpServerResponding [429, 200] counter $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpTimeoutMs = 10000
              }
        e <- newOtlpHttpSpanExporter cfg
        result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        count <- readTVarIO counter
        assertBool "Expected at least 2 requests for 429 retry" (count >= 2)
        result @?= ExportSuccess
  , testCase "400 does NOT trigger retry (fails after 1 attempt)" $ do
      counter <- newTVarIO (0 :: Int)
      withMockHttpServerResponding [400] counter $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpTimeoutMs = 5000
              }
        e <- newOtlpHttpSpanExporter cfg
        result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        count <- readTVarIO counter
        count @?= 1
        result @?= ExportFailure
  , testCase "503 triggers retry" $ do
      counter <- newTVarIO (0 :: Int)
      withMockHttpServerResponding [503, 200] counter $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpTimeoutMs = 10000
              }
        e <- newOtlpHttpSpanExporter cfg
        result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        count <- readTVarIO counter
        assertBool "Expected at least 2 requests for 503 retry" (count >= 2)
        result @?= ExportSuccess
  ]


-------------------------------------------------------------------------------
-- 6. Integration tests (full round-trip)
-------------------------------------------------------------------------------

integrationTests :: TestTree
integrationTests = testGroup "integration: mock HTTP server round-trip"
  [ testCase "span export round-trip: decode proto body, verify span name" $ do
      captured <- newTVarIO Nothing
      withMockHttpServer captured $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpContentType = Protobuf
              , otlpHttpTimeoutMs = 5000
              }
        e <- newOtlpHttpSpanExporter cfg
        let testSpan = makeTestSpan "http-integration-span"
        _result <- exportSpans e [SomeReadableSpan testSpan]
        mCap <- timeout 5_000_000 $ atomically $ do
          v <- readTVar captured
          maybe retry pure v
        case mCap of
          Nothing -> assertFailure "Timed out waiting for mock server"
          Just (_hdrs, body) ->
            case decodeMessage body :: Either String CollectorTrace.ExportTraceServiceRequest of
              Left err -> assertFailure ("Could not decode proto: " <> err)
              Right req -> do
                let spans = concatMap
                      (\rs -> concatMap (\ss -> ss ^. TF.spans) (rs ^. TF.scopeSpans))
                      (req ^. CTF.resourceSpans)
                length spans @?= 1
                (first spans ^. TF.name) @?= "http-integration-span"
  ]


-------------------------------------------------------------------------------
-- Mock HTTP/1.1 server
-------------------------------------------------------------------------------

-- | Captured data: (raw headers as ByteString, body bytes)
type Captured = (ByteString, ByteString)


-- | Open a listening TCP socket on a random port.
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


-- | Run an action with a mock HTTP server that captures the first request
-- and responds with 200.
withMockHttpServer :: TVar (Maybe Captured) -> (Int -> IO a) -> IO a
withMockHttpServer captured action =
  bracket openListenSocket NS.close $ \lSock -> do
    port <- fromIntegral <$> NS.socketPort lSock
    withAsync (serveOne lSock captured 200) $ \_ -> action port


-- | Accept one connection, read HTTP/1.1 request, respond with given status.
serveOne :: NS.Socket -> TVar (Maybe Captured) -> Int -> IO ()
serveOne lSock captured status = do
  (conn, _) <- NS.accept lSock
  bracket (pure conn) NS.close $ \c -> do
    (rawHdrs, bodyPrefix) <- readUntilBlankLine c
    let bodyLen = parseContentLength rawHdrs
        remaining = bodyLen - BS.length bodyPrefix
    bodyRest <- recvN c remaining
    let body = bodyPrefix <> bodyRest
    atomically (writeTVar captured (Just (rawHdrs, body)))
    let resp = "HTTP/1.1 " <> B8.pack (show status) <> " OK\r\nContent-Length: 0\r\n\r\n"
    NSB.sendAll c resp


-- | Serve N requests with different status codes, counting requests.
withMockHttpServerResponding :: [Int] -> TVar Int -> (Int -> IO a) -> IO a
withMockHttpServerResponding statuses counter action =
  bracket openListenSocket NS.close $ \lSock -> do
    port <- fromIntegral <$> NS.socketPort lSock
    withAsync (serveMany lSock statuses counter) $ \_ -> action port


-- | Serve multiple requests on a single keep-alive connection with given
-- status codes. http-client reuses connections by default, so retries land
-- on the same connection.
serveMany :: NS.Socket -> [Int] -> TVar Int -> IO ()
serveMany lSock statuses counter = do
  (conn, _) <- NS.accept lSock
  bracket (pure conn) NS.close $ \c -> serveRequests c statuses
  where
    serveRequests _ [] = pure ()
    serveRequests c (status : rest) = do
      (rawHdrs, bodyPrefix) <- readUntilBlankLine c
      if BS.null rawHdrs
        then pure ()
        else do
          let bodyLen = parseContentLength rawHdrs
              bodyRemainder = bodyLen - BS.length bodyPrefix
          _body <- recvN c bodyRemainder
          atomically (modifyTVar' counter (+ 1))
          let retryAfter = if status `elem` [429, 502, 503, 504]
                           then "Retry-After: 0\r\n"
                           else ""
              resp = "HTTP/1.1 " <> B8.pack (show status) <> " OK\r\n"
                  <> retryAfter
                  <> "Content-Length: 0\r\n\r\n"
          NSB.sendAll c resp
          serveRequests c rest


-- | Read from socket until we see the blank line "\r\n\r\n" that separates
-- HTTP headers from the body. Returns (headers, already-read body prefix).
readUntilBlankLine :: NS.Socket -> IO (ByteString, ByteString)
readUntilBlankLine sock = go mempty
  where
    go acc = do
      chunk <- NSB.recv sock 4096
      if BS.null chunk
        then pure (acc, mempty)
        else do
          let combined = acc <> chunk
          case BS.breakSubstring "\r\n\r\n" combined of
            (hdrs, rest) | not (BS.null rest) ->
              pure (hdrs, BS.drop 4 rest)
            _ -> go combined


-- | Parse Content-Length value from raw HTTP headers.
parseContentLength :: ByteString -> Int
parseContentLength hdrs =
  case filter (BS.isPrefixOf "content-length:") (map (B8.map toLowerAscii) (B8.lines hdrs)) of
    (line : _) ->
      let val = B8.dropWhile (== ' ') (BS.drop 15 line) -- "content-length:" is 15 chars
       in case B8.readInt (B8.takeWhile (\c -> c >= '0' && c <= '9') val) of
            Just (n, _) -> n
            Nothing     -> 0
    [] -> 0
  where
    toLowerAscii :: Char -> Char
    toLowerAscii c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c


-- | Receive exactly n bytes from a socket.
recvN :: NS.Socket -> Int -> IO ByteString
recvN _sock 0 = pure mempty
recvN sock n = go mempty n
  where
    go acc 0 = pure acc
    go acc remaining = do
      chunk <- NSB.recv sock (min 4096 remaining)
      if BS.null chunk
        then pure acc
        else go (acc <> chunk) (remaining - BS.length chunk)


-- | Find a header value (case-insensitive key match) from raw headers.
findHeader :: ByteString -> ByteString -> Maybe ByteString
findHeader key hdrs =
  let keyLower = B8.map toLowerAscii key
      lines_ = B8.lines hdrs
      matching = filter (\l -> keyLower `BS.isPrefixOf` B8.map toLowerAscii l) lines_
   in case matching of
        (line : _) ->
          let afterColon = BS.drop 1 (B8.dropWhile (/= ':') line)
              trimmed = B8.dropWhile (== ' ') afterColon
              -- Remove trailing \r if present
              cleaned = if not (BS.null trimmed) && B8.last trimmed == '\r'
                        then BS.init trimmed
                        else trimmed
           in Just cleaned
        [] -> Nothing
  where
    toLowerAscii :: Char -> Char
    toLowerAscii c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c


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


makeTestSpan :: Text -> TestSpan
makeTestSpan name = defaultTestSpan { tsName = name }


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


defaultTestLogRecord :: TestLogRecord
defaultTestLogRecord = TestLogRecord
  { tlrResource = Resource.empty
  , tlrScope = Attr.InstrumentationScope "test" Nothing Nothing Nothing
  , tlrTimestamp = Nothing
  , tlrObservedTime = fromNanos 1000
  , tlrSeverityNumber = Nothing
  , tlrSeverityText = Nothing
  , tlrBody = Just (LogBodyString "test-log")
  , tlrAttributes = Attr.emptyAttributes
  , tlrDropped = 0
  , tlrSpanContext = Nothing
  }


-------------------------------------------------------------------------------
-- 7. Partial-success warning tests (JSON mode)
-------------------------------------------------------------------------------

partialSuccessTests :: TestTree
partialSuccessTests = testGroup "partial-success warning (JSON mode)"
  [ testCase "JSON response with rejectedSpans > 0 does not throw; export succeeds" $ do
      -- The server returns 200 with a JSON body containing partialSuccess.
      -- warnTracePartialSuccess should parse this (after the fix) and emit a
      -- warning to stderr without throwing. We verify ExportSuccess here;
      -- full stderr-capture testing requires a dedicated test harness.
      let jsonBody = "{\"partialSuccess\":{\"rejectedSpans\":3}}"
      withMockHttpServerJsonBody jsonBody $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpContentType = Json
              , otlpHttpTimeoutMs = 5000
              }
        e <- newOtlpHttpSpanExporter cfg
        result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        result @?= ExportSuccess

  , testCase "JSON response with rejectedSpans = 0 does not throw; export succeeds" $ do
      let jsonBody = "{\"partialSuccess\":{\"rejectedSpans\":0}}"
      withMockHttpServerJsonBody jsonBody $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpContentType = Json
              , otlpHttpTimeoutMs = 5000
              }
        e <- newOtlpHttpSpanExporter cfg
        result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        result @?= ExportSuccess

  , testCase "JSON response with no partialSuccess key does not throw; export succeeds" $ do
      let jsonBody = "{}"
      withMockHttpServerJsonBody jsonBody $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpContentType = Json
              , otlpHttpTimeoutMs = 5000
              }
        e <- newOtlpHttpSpanExporter cfg
        result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        result @?= ExportSuccess

  , testCase "invalid JSON response body does not throw; export succeeds" $ do
      let jsonBody = "not json at all"
      withMockHttpServerJsonBody jsonBody $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpContentType = Json
              , otlpHttpTimeoutMs = 5000
              }
        e <- newOtlpHttpSpanExporter cfg
        result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        result @?= ExportSuccess

  , testCase "empty response body does not throw; export succeeds" $ do
      withMockHttpServerJsonBody "" $ \port -> do
        let cfg = defaultOtlpHttpConfig
              { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
              , otlpHttpContentType = Json
              , otlpHttpTimeoutMs = 5000
              }
        e <- newOtlpHttpSpanExporter cfg
        result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        result @?= ExportSuccess
  ]


-- | Run an action with a mock HTTP server that responds 200 with a given body.
withMockHttpServerJsonBody :: ByteString -> (Int -> IO a) -> IO a
withMockHttpServerJsonBody respBody action =
  bracket openListenSocket NS.close $ \lSock -> do
    port <- fromIntegral <$> NS.socketPort lSock
    withAsync (serveOneWithBody lSock respBody) $ \_ -> action port


-- | Accept one connection, consume the request, respond 200 with the given body.
serveOneWithBody :: NS.Socket -> ByteString -> IO ()
serveOneWithBody lSock body = do
  (conn, _) <- NS.accept lSock
  bracket (pure conn) NS.close $ \c -> do
    (rawHdrs, bodyPrefix) <- readUntilBlankLine c
    let bodyLen = parseContentLength rawHdrs
        remaining = bodyLen - BS.length bodyPrefix
    _bodyRest <- recvN c remaining
    let resp = "HTTP/1.1 200 OK\r\n"
            <> "Content-Type: application/json\r\n"
            <> "Content-Length: " <> B8.pack (show (BS.length body)) <> "\r\n"
            <> "\r\n"
            <> body
    NSB.sendAll c resp


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

first :: [a] -> a
first (x : _) = x
first [] = error "first: empty list"
