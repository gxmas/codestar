module OTel.Exporter.OTLP.GRPC.Internal.Client
  ( TlsConfig (..)
  , GrpcClientConfig (..)
  , defaultGrpcClientConfig
  , unaryRpc
  ) where

import Control.Exception (SomeException, bracket, catch, finally, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as Builder
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Network.HTTP2.Client as H2
import qualified Network.HTTP.Semantics as Sem
import qualified Network.HTTP.Semantics.Client as SemC
import Network.HTTP.Types.Header (RequestHeaders)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.TLS as TLS
import Data.X509.CertificateStore (readCertificateStore)
import System.X509 (getSystemCertificateStore)
import Network.TLS.Extra.Cipher (ciphersuite_default)

import OTel.Exporter.OTLP.GRPC.Internal.Frame
import OTel.Exporter.OTLP.GRPC.Internal.Retry (GrpcError (..))
import OTel.Exporter.OTLP.GRPC.Internal.Status


-- | TLS configuration for gRPC connections.
data TlsConfig = TlsConfig
  { tlsCaStore :: Maybe FilePath
  , tlsSkipVerify :: Bool
  } deriving stock (Show, Eq)


-- | Low-level gRPC client configuration.
data GrpcClientConfig = GrpcClientConfig
  { grpcHost :: ByteString
  , grpcPort :: Int
  , grpcTls :: Maybe TlsConfig
  , grpcCompression :: Compression
  , grpcHeaders :: [(ByteString, ByteString)]
  , grpcTimeoutMicros :: Maybe Int
  } deriving stock (Show)


-- | Default gRPC client config.
defaultGrpcClientConfig :: GrpcClientConfig
defaultGrpcClientConfig = GrpcClientConfig
  { grpcHost = "localhost"
  , grpcPort = 4317
  , grpcTls = Nothing
  , grpcCompression = NoCompression
  , grpcHeaders = []
  , grpcTimeoutMicros = Nothing
  }


-- | Perform a unary gRPC RPC call over HTTP/2.
unaryRpc
  :: GrpcClientConfig
  -> ByteString
  -> ByteString
  -> IO (Either GrpcError ByteString)
unaryRpc cfg path requestBody =
  (doUnaryRpc cfg path requestBody)
    `catch` \(e :: SomeException) ->
      pure (Left (GrpcNetworkError e))


doUnaryRpc
  :: GrpcClientConfig
  -> ByteString
  -> ByteString
  -> IO (Either GrpcError ByteString)
doUnaryRpc cfg path requestBody = do
  framedBody <- encodeFrame cfg.grpcCompression requestBody
  let host = B8.unpack cfg.grpcHost
      port = show cfg.grpcPort
  addr <- resolveAddr host port
  bracket (openConnection addr) closeConnection $ \sock -> do
    case cfg.grpcTls of
      Nothing -> runPlaintext cfg sock path framedBody
      Just tlsCfg -> runWithTls cfg tlsCfg sock path framedBody


resolveAddr :: String -> String -> IO NS.AddrInfo
resolveAddr host port = do
  let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    [] -> throwIO (userError ("Cannot resolve " <> host <> ":" <> port))
    (a:_) -> pure a


openConnection :: NS.AddrInfo -> IO NS.Socket
openConnection addr = do
  sock <- NS.openSocket addr
  NS.connect sock (NS.addrAddress addr)
  pure sock


closeConnection :: NS.Socket -> IO ()
closeConnection = NS.close


runPlaintext
  :: GrpcClientConfig
  -> NS.Socket
  -> ByteString
  -> ByteString
  -> IO (Either GrpcError ByteString)
runPlaintext cfg sock path framedBody =
  bracket (H2.allocSimpleConfig sock 4096) H2.freeSimpleConfig $ \h2Cfg -> do
    let clientCfg = H2.defaultClientConfig
          { H2.scheme = "http"
          , H2.authority = B8.unpack cfg.grpcHost <> ":" <> show cfg.grpcPort
          }
    H2.run clientCfg h2Cfg $ \sendReq _aux ->
      executeRequest cfg sendReq path framedBody


runWithTls
  :: GrpcClientConfig
  -> TlsConfig
  -> NS.Socket
  -> ByteString
  -> ByteString
  -> IO (Either GrpcError ByteString)
runWithTls cfg tlsCfg sock path framedBody = do
  params <- buildTlsParams (B8.unpack cfg.grpcHost) tlsCfg
  ctx <- TLS.contextNew (tlsSocketBackend sock) params
  TLS.handshake ctx
  bracket (H2.allocSimpleConfig sock 4096) H2.freeSimpleConfig $ \h2Cfg ->
    let h2TlsCfg = h2Cfg
          { H2.confSendAll = \bs -> TLS.sendData ctx (LBS.fromStrict bs)
          , H2.confReadN   = tlsRecvExact ctx
          }
        clientCfg = H2.defaultClientConfig
          { H2.scheme    = "https"
          , H2.authority = B8.unpack cfg.grpcHost <> ":" <> show cfg.grpcPort
          }
    in H2.run clientCfg h2TlsCfg (\sendReq _aux ->
         executeRequest cfg sendReq path framedBody)
       `finally` (TLS.bye ctx `catch` \(_ :: SomeException) -> pure ())


tlsSocketBackend :: NS.Socket -> TLS.Backend
tlsSocketBackend sock = TLS.Backend
  { TLS.backendFlush = pure ()
  , TLS.backendClose = NS.close sock
  , TLS.backendSend  = NSB.sendAll sock
  , TLS.backendRecv  = NSB.recv sock
  }


tlsRecvExact :: TLS.Context -> Int -> IO ByteString
tlsRecvExact ctx = go mempty
  where
    go acc n
      | n <= 0    = pure acc
      | otherwise = do
          chunk <- TLS.recvData ctx
          let len = BS.length chunk
          if len >= n
            then pure (acc <> BS.take n chunk)
            else go (acc <> chunk) (n - len)


buildTlsParams :: String -> TlsConfig -> IO TLS.ClientParams
buildTlsParams host tlsCfg = do
  caStore <- case tlsCfg.tlsCaStore of
    Nothing   -> getSystemCertificateStore
    Just path -> do
      ms <- readCertificateStore path
      maybe getSystemCertificateStore pure ms
  let params = TLS.defaultParamsClient host BS.empty
      hooks  = (TLS.clientHooks params)
        { TLS.onSuggestALPN = pure (Just ["h2"])
        , TLS.onServerCertificate =
            if tlsCfg.tlsSkipVerify
              then \_ _ _ _ -> pure []
              else TLS.onServerCertificate (TLS.clientHooks params)
        }
      shared = (TLS.clientShared params) { TLS.sharedCAStore = caStore }
  pure params
    { TLS.clientShared    = shared
    , TLS.clientHooks     = hooks
    , TLS.clientSupported = (TLS.clientSupported params)
        { TLS.supportedVersions = [TLS.TLS13, TLS.TLS12]
        , TLS.supportedCiphers  = ciphersuite_default
        }
    }


executeRequest
  :: GrpcClientConfig
  -> SemC.SendRequest
  -> ByteString
  -> ByteString
  -> IO (Either GrpcError ByteString)
executeRequest cfg sendReq path framedBody = do
  let headers = buildRequestHeaders cfg path
      req = SemC.requestBuilder "POST" path headers (Builder.byteString framedBody)
  sendReq req $ \resp -> do
    body <- readResponseBody resp
    trailerTable <- SemC.getResponseTrailers resp
    let trailers = tokenHeaderTableToList trailerTable
        status = parseGrpcStatus trailers
        retryAfterMs = parseRetryAfterMs trailers
    case status.statusCode of
      GrpcOk
        | BS.null body -> pure (Right BS.empty)
        | otherwise -> do
            decoded <- decodeFrame body
            case decoded of
              Left err -> pure (Left (GrpcProtocolError (T.pack (show err))))
              Right (_compression, payload) -> pure (Right payload)
      _ -> pure (Left (GrpcStatusError status retryAfterMs))


buildRequestHeaders :: GrpcClientConfig -> ByteString -> RequestHeaders
buildRequestHeaders cfg _path =
  [ (CI.mk "content-type", "application/grpc+proto")
  , (CI.mk "te", "trailers")
  , (CI.mk "grpc-accept-encoding", "identity,gzip")
  ]
  <> encodingHeader
  <> timeoutHeader
  <> map (\(k, v) -> (CI.mk k, v)) cfg.grpcHeaders
  where
    encodingHeader = case cfg.grpcCompression of
      GzipCompression -> [(CI.mk "grpc-encoding", "gzip")]
      NoCompression -> []
    timeoutHeader = case cfg.grpcTimeoutMicros of
      Nothing -> []
      Just micros -> [(CI.mk "grpc-timeout", B8.pack (show micros) <> "u")]


readResponseBody :: SemC.Response -> IO ByteString
readResponseBody resp = go mempty
  where
    go acc = do
      chunk <- SemC.getResponseBodyChunk resp
      if BS.null chunk
        then pure acc
        else go (acc <> chunk)


tokenHeaderTableToList :: Maybe Sem.TokenHeaderTable -> [(ByteString, ByteString)]
tokenHeaderTableToList Nothing = []
tokenHeaderTableToList (Just (tokenList, _)) =
  map (\(tok, val) -> (CI.original (Sem.tokenKey tok), val)) tokenList


parseRetryAfterMs :: [(ByteString, ByteString)] -> Maybe Int
parseRetryAfterMs hdrs =
  case lookup "grpc-retry-after-ms" hdrs of
    Nothing -> Nothing
    Just bs -> case B8.readInt bs of
      Just (n, rest) | B8.null rest -> Just n
      _ -> Nothing
