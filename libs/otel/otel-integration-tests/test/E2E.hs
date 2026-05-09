module Main where

import Codec.Compression.GZip qualified as GZip
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.Exception (bracket)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Family2 ((.~), (&), (^.))
import Network.HTTP.Semantics qualified as Sem
import Network.HTTP.Semantics.Server qualified as SemS
import Network.HTTP.Types (ok200)
import Network.HTTP2.Server qualified as H2S
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB
import OTel.Attribute qualified as Attr
import OTel.Exporter.OTLP.GRPC
import OTel.Exporter.OTLP.HTTP
import OTel.SDK.Export (ExportResult (..))
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
tests = testGroup "e2e: full export pipeline"
  [ grpcExportTest
  , httpProtobufExportTest
  , httpJsonExportTest
  , gzipCompressionTest
  , grpcPartialSuccessTest
  , httpPartialSuccessTest
  ]


-------------------------------------------------------------------------------
-- Test 1: gRPC export end-to-end
-------------------------------------------------------------------------------

grpcExportTest :: TestTree
grpcExportTest = testCase "gRPC export end-to-end: span round-trips through mock server" $ do
  received <- newTVarIO Nothing
  withMockGrpcServer received $ \port -> do
    e <- newOtlpGrpcSpanExporter defaultOtlpGrpcConfig
           { otlpEndpoint = T.pack ("127.0.0.1:" <> show port)
           , otlpTimeoutMs = 5000
           }
    let testSpan = makeTestSpan "e2e-grpc-span"
    result <- exportSpans e [SomeReadableSpan testSpan]
    result @?= ExportSuccess
    -- Wait for mock server to capture the request
    mProtoBytes <- timeout 5_000_000 $ atomically $ do
      v <- readTVar received
      maybe retry pure v
    protoBytes <- case mProtoBytes of
      Nothing -> assertFailure "Timed out waiting for mock gRPC server" >> pure mempty
      Just bs -> pure bs
    case decodeMessage protoBytes :: Either String CollectorTrace.ExportTraceServiceRequest of
      Left err -> assertFailure ("Could not decode ExportTraceServiceRequest: " <> err)
      Right req -> do
        let spans = concatMap
              (\rs -> concatMap (\ss -> ss ^. TF.spans) (rs ^. TF.scopeSpans))
              (req ^. CTF.resourceSpans)
        length spans @?= 1
        (first spans ^. TF.name) @?= "e2e-grpc-span"


-------------------------------------------------------------------------------
-- Test 2: HTTP protobuf export end-to-end
-------------------------------------------------------------------------------

httpProtobufExportTest :: TestTree
httpProtobufExportTest = testCase "HTTP protobuf export end-to-end: correct Content-Type and body" $ do
  captured <- newTVarIO Nothing
  withMockHttpServer captured 200 BS.empty $ \port -> do
    let cfg = defaultOtlpHttpConfig
          { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
          , otlpHttpContentType = Protobuf
          , otlpHttpTimeoutMs = 5000
          }
    e <- newOtlpHttpSpanExporter cfg
    let testSpan = makeTestSpan "e2e-http-proto-span"
    result <- exportSpans e [SomeReadableSpan testSpan]
    result @?= ExportSuccess
    mCap <- timeout 5_000_000 $ atomically $ do
      v <- readTVar captured
      maybe retry pure v
    case mCap of
      Nothing -> assertFailure "Timed out waiting for mock HTTP server"
      Just (hdrs, body) -> do
        -- Verify Content-Type
        let ct = findHeader "content-type" hdrs
        ct @?= Just "application/x-protobuf"
        -- Verify body decodes as ExportTraceServiceRequest with 1 span
        case decodeMessage body :: Either String CollectorTrace.ExportTraceServiceRequest of
          Left err -> assertFailure ("Could not decode proto body: " <> err)
          Right req -> do
            let spans = concatMap
                  (\rs -> concatMap (\ss -> ss ^. TF.spans) (rs ^. TF.scopeSpans))
                  (req ^. CTF.resourceSpans)
            length spans @?= 1
            (first spans ^. TF.name) @?= "e2e-http-proto-span"


-------------------------------------------------------------------------------
-- Test 3: HTTP JSON export end-to-end
-------------------------------------------------------------------------------

httpJsonExportTest :: TestTree
httpJsonExportTest = testCase "HTTP JSON export end-to-end: valid JSON with resourceSpans, string enums" $ do
  captured <- newTVarIO Nothing
  withMockHttpServer captured 200 BS.empty $ \port -> do
    let cfg = defaultOtlpHttpConfig
          { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
          , otlpHttpContentType = Json
          , otlpHttpTimeoutMs = 5000
          }
    e <- newOtlpHttpSpanExporter cfg
    let testSpan = makeTestSpan "e2e-http-json-span"
    result <- exportSpans e [SomeReadableSpan testSpan]
    result @?= ExportSuccess
    mCap <- timeout 5_000_000 $ atomically $ do
      v <- readTVar captured
      maybe retry pure v
    case mCap of
      Nothing -> assertFailure "Timed out waiting for mock HTTP server"
      Just (hdrs, body) -> do
        -- Verify Content-Type
        let ct = findHeader "content-type" hdrs
        ct @?= Just "application/json"
        -- Verify body is valid JSON
        case A.eitherDecode (LBS.fromStrict body) :: Either String A.Value of
          Left err -> assertFailure ("Body is not valid JSON: " <> err)
          Right val -> do
            -- Verify top-level resourceSpans key
            case val of
              A.Object obj ->
                assertBool "Missing 'resourceSpans' key" $
                  AKM.member (AK.fromString "resourceSpans") obj
              _ -> assertFailure "Expected JSON object at top level"
            -- Verify the JSON uses string enum names per proto3 JSON spec
            -- (D-HTTP-JSON fix): kind must be "SPAN_KIND_INTERNAL" not 1
            let bodyStr = B8.unpack body
            assertBool "Expected string enum 'SPAN_KIND_INTERNAL' in JSON body" $
              "SPAN_KIND_INTERNAL" `isSubstringOf` bodyStr


-------------------------------------------------------------------------------
-- Test 4: gzip compression end-to-end
-------------------------------------------------------------------------------

gzipCompressionTest :: TestTree
gzipCompressionTest = testCase "HTTP gzip compression: Content-Encoding header and valid compressed body" $ do
  captured <- newTVarIO Nothing
  withMockHttpServer captured 200 BS.empty $ \port -> do
    let cfg = defaultOtlpHttpConfig
          { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
          , otlpHttpContentType = Protobuf
          , otlpHttpCompression = GzipCompression
          , otlpHttpTimeoutMs = 5000
          }
    e <- newOtlpHttpSpanExporter cfg
    let testSpan = makeTestSpan "e2e-gzip-span"
    result <- exportSpans e [SomeReadableSpan testSpan]
    result @?= ExportSuccess
    mCap <- timeout 5_000_000 $ atomically $ do
      v <- readTVar captured
      maybe retry pure v
    case mCap of
      Nothing -> assertFailure "Timed out waiting for mock HTTP server"
      Just (hdrs, body) -> do
        -- Verify Content-Encoding: gzip
        let ce = findHeader "content-encoding" hdrs
        ce @?= Just "gzip"
        -- Decompress and verify the body decodes as ExportTraceServiceRequest
        let decompressed = LBS.toStrict (GZip.decompress (LBS.fromStrict body))
        case decodeMessage decompressed :: Either String CollectorTrace.ExportTraceServiceRequest of
          Left err -> assertFailure ("Could not decode decompressed proto: " <> err)
          Right req -> do
            let spans = concatMap
                  (\rs -> concatMap (\ss -> ss ^. TF.spans) (rs ^. TF.scopeSpans))
                  (req ^. CTF.resourceSpans)
            length spans @?= 1
            (first spans ^. TF.name) @?= "e2e-gzip-span"


-------------------------------------------------------------------------------
-- Test 5: D9 partial_success warning via gRPC
-------------------------------------------------------------------------------

grpcPartialSuccessTest :: TestTree
grpcPartialSuccessTest = testCase "gRPC partial_success: ExportSuccess returned despite rejected spans" $ do
  received <- newTVarIO Nothing
  let partialResp = defMessage
        & CTF.partialSuccess .~ (defMessage & CTF.rejectedSpans .~ 3)
        :: CollectorTrace.ExportTraceServiceResponse
  withMockGrpcServerWithResponse (encodeMessage partialResp) received $ \port -> do
    e <- newOtlpGrpcSpanExporter defaultOtlpGrpcConfig
           { otlpEndpoint = T.pack ("127.0.0.1:" <> show port)
           , otlpTimeoutMs = 5000
           }
    let testSpan = makeTestSpan "e2e-partial-grpc"
    -- partial_success with grpc-status 0 should still yield ExportSuccess
    result <- exportSpans e [SomeReadableSpan testSpan]
    result @?= ExportSuccess


-------------------------------------------------------------------------------
-- Test 6: D9 partial_success warning via HTTP
-------------------------------------------------------------------------------

httpPartialSuccessTest :: TestTree
httpPartialSuccessTest = testCase "HTTP partial_success: ExportSuccess returned despite rejected spans" $ do
  captured <- newTVarIO Nothing
  -- Build a protobuf response with rejectedSpans = 5
  let partialResp = defMessage
        & CTF.partialSuccess .~ (defMessage & CTF.rejectedSpans .~ 5)
        :: CollectorTrace.ExportTraceServiceResponse
      respBody = encodeMessage partialResp
  withMockHttpServer captured 200 respBody $ \port -> do
    let cfg = defaultOtlpHttpConfig
          { otlpHttpEndpoint = "http://127.0.0.1:" <> T.pack (show port)
          , otlpHttpContentType = Protobuf
          , otlpHttpTimeoutMs = 5000
          }
    e <- newOtlpHttpSpanExporter cfg
    let testSpan = makeTestSpan "e2e-partial-http"
    -- 200 with partial_success body should still yield ExportSuccess
    result <- exportSpans e [SomeReadableSpan testSpan]
    result @?= ExportSuccess


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
-- and responds with the given status and body.
withMockHttpServer :: TVar (Maybe Captured) -> Int -> ByteString -> (Int -> IO a) -> IO a
withMockHttpServer captured status respBody action =
  bracket openListenSocket NS.close $ \lSock -> do
    port <- fromIntegral <$> NS.socketPort lSock
    withAsync (serveOne lSock captured status respBody) $ \_ -> action port


-- | Accept one connection, read HTTP/1.1 request, respond with given status and body.
serveOne :: NS.Socket -> TVar (Maybe Captured) -> Int -> ByteString -> IO ()
serveOne lSock captured status respBody = do
  (conn, _) <- NS.accept lSock
  bracket (pure conn) NS.close $ \c -> do
    (rawHdrs, bodyPrefix) <- readUntilBlankLine c
    let bodyLen = parseContentLength rawHdrs
        remaining = bodyLen - BS.length bodyPrefix
    bodyRest <- recvN c remaining
    let body = bodyPrefix <> bodyRest
    atomically (writeTVar captured (Just (rawHdrs, body)))
    let contentLenStr = B8.pack (show (BS.length respBody))
        resp = "HTTP/1.1 " <> B8.pack (show status) <> " OK\r\n"
            <> "Content-Length: " <> contentLenStr <> "\r\n\r\n"
            <> respBody
    NSB.sendAll c resp


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
-- Mock gRPC server (HTTP/2)
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


-- | Build a gRPC-OK response with an empty frame and proper trailers.
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


-- | Build a gRPC-OK response with the given protobuf body.
grpcOkResponseWithBody :: ByteString -> SemS.Response
grpcOkResponseWithBody bodyProto =
  let frameLen = BS.length bodyProto
      lenBytes = BS.pack
        [ fromIntegral ((frameLen `div` (256*256*256)) `mod` 256)
        , fromIntegral ((frameLen `div` (256*256)) `mod` 256)
        , fromIntegral ((frameLen `div` 256) `mod` 256)
        , fromIntegral (frameLen `mod` 256)
        ]
      frame = BS.cons 0 lenBytes <> bodyProto
  in SemS.setResponseTrailersMaker
       (SemS.responseStreaming ok200
         [("content-type", "application/grpc+proto")]
         (\write flush -> do
           write (Builder.byteString frame)
           flush))
       grpcOkTrailersMaker


-- | Accept a single connection and run an HTTP/2 server that captures request bodies.
acceptAndServe :: SemS.Response -> NS.Socket -> TVar (Maybe ByteString) -> IO ()
acceptAndServe resp lSock received = do
  (conn, _) <- NS.accept lSock
  bracket (H2S.allocSimpleConfig conn 4096) H2S.freeSimpleConfig $ \h2cfg -> do
    doneRef <- newIORef False
    H2S.run H2S.defaultServerConfig h2cfg $ \req _aux respond -> do
      alreadyDone <- readIORef doneRef
      if alreadyDone
        then do
          _ <- readAllChunks req
          respond resp []
        else do
          writeIORef doneRef True
          body <- readAllChunks req
          -- Skip 5-byte gRPC data frame header to get the proto bytes
          let protoBytes = if BS.length body >= 5 then BS.drop 5 body else body
          atomically (writeTVar received (Just protoBytes))
          respond resp []
    NS.close conn


-- | Run an action with a mock gRPC server on a random port.
-- The server captures the raw proto bytes of the first request and responds with OK + empty body.
withMockGrpcServer :: TVar (Maybe ByteString) -> (Int -> IO a) -> IO a
withMockGrpcServer received action =
  bracket openListenSocket NS.close $ \lSock -> do
    port <- fromIntegral <$> NS.socketPort lSock
    withAsync (acceptAndServe grpcOkResponse lSock received) $ \_serverThread ->
      action port


-- | Like withMockGrpcServer but responds with a custom protobuf body.
withMockGrpcServerWithResponse :: ByteString -> TVar (Maybe ByteString) -> (Int -> IO a) -> IO a
withMockGrpcServerWithResponse respProto received action =
  bracket openListenSocket NS.close $ \lSock -> do
    port <- fromIntegral <$> NS.socketPort lSock
    withAsync (acceptAndServe (grpcOkResponseWithBody respProto) lSock received) $ \_serverThread ->
      action port


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
-- Helpers
-------------------------------------------------------------------------------

first :: [a] -> a
first (x : _) = x
first [] = error "first: empty list"


isSubstringOf :: String -> String -> Bool
isSubstringOf needle haystack = go haystack
  where
    n = length needle
    go [] = null needle
    go s@(_ : rest)
      | take n s == needle = True
      | otherwise = go rest
