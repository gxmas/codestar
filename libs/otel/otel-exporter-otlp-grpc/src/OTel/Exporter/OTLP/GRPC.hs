-- | OTLP/gRPC exporter for traces, metrics, and logs.
module OTel.Exporter.OTLP.GRPC
  ( -- * Configuration
    OtlpGrpcConfig (..)
  , defaultOtlpGrpcConfig

    -- * Span exporter
  , OtlpGrpcSpanExporter (..)
  , newOtlpGrpcSpanExporter
  , newOtlpGrpcSpanExporterFromEnv

    -- * Metric exporter
  , OtlpGrpcMetricExporter (..)
  , newOtlpGrpcMetricExporter
  , newOtlpGrpcMetricExporterFromEnv

    -- * Log record exporter
  , OtlpGrpcLogRecordExporter (..)
  , newOtlpGrpcLogRecordExporter
  , newOtlpGrpcLogRecordExporterFromEnv

    -- * Re-exports from Internal
  , Compression (..)
  , TlsConfig (..)
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.ProtoLens (encodeMessage)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (urlDecode)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import OTel.Exporter.OTLP.GRPC.Internal
import OTel.Exporter.OTLP.GRPC.Internal.Proto
import OTel.SDK.Export (ExportResult (..))
import OTel.SDK.Log.Export (LogRecordExporter (..))
import OTel.SDK.Metric.Export
  ( Aggregation (..)
  , AggregationTemporality (..)
  , MetricExporter (..)
  )
import OTel.SDK.Trace.Export (SpanExporter (..))


-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | Configuration for the OTLP/gRPC exporter.
data OtlpGrpcConfig = OtlpGrpcConfig
  { otlpEndpoint    :: Text
  , otlpTls         :: Maybe TlsConfig
  , otlpHeaders     :: [(ByteString, ByteString)]
  , otlpCompression :: Compression
  , otlpTimeoutMs   :: Int
  } deriving stock (Show)


-- | Default OTLP/gRPC config: localhost:4317, no TLS, no compression.
defaultOtlpGrpcConfig :: OtlpGrpcConfig
defaultOtlpGrpcConfig = OtlpGrpcConfig
  { otlpEndpoint    = "localhost:4317"
  , otlpTls         = Nothing
  , otlpHeaders     = []
  , otlpCompression = NoCompression
  , otlpTimeoutMs   = 10_000
  }


-------------------------------------------------------------------------------
-- Span exporter
-------------------------------------------------------------------------------

-- | OTLP/gRPC span exporter.
data OtlpGrpcSpanExporter = OtlpGrpcSpanExporter
  { _spanClientConfig :: GrpcClientConfig
  , _spanRetryConfig  :: RetryConfig
  , _spanShutdown     :: TVar Bool
  }


-- | Create an OTLP/gRPC span exporter from explicit config.
newOtlpGrpcSpanExporter :: OtlpGrpcConfig -> IO OtlpGrpcSpanExporter
newOtlpGrpcSpanExporter cfg = do
  s <- newTVarIO False
  pure OtlpGrpcSpanExporter
    { _spanClientConfig = configToGrpcClientConfig cfg
    , _spanRetryConfig  = defaultRetryConfig
    , _spanShutdown     = s
    }


-- | Create an OTLP/gRPC span exporter reading config from environment variables.
newOtlpGrpcSpanExporterFromEnv :: IO OtlpGrpcSpanExporter
newOtlpGrpcSpanExporterFromEnv = do
  cfg <- readConfigFromEnv "TRACES"
  newOtlpGrpcSpanExporter cfg


instance SpanExporter OtlpGrpcSpanExporter where
  exportSpans e spans = do
    done <- readTVarIO e._spanShutdown
    if done
      then pure ExportFailure
      else do
        let body = encodeMessage (spanListToExportRequest spans)
        result <- withRetry e._spanRetryConfig $ \_ ->
          unaryRpc e._spanClientConfig tracePath body
        case result of
          Right respBytes -> parseTraceExportResult respBytes
          Left _          -> pure ExportFailure
    where
      tracePath = "/opentelemetry.proto.collector.trace.v1.TraceService/Export"

  shutdownExporter e = do
    atomically (writeTVar e._spanShutdown True)
    pure (Right ())

  forceFlushExporter _ _ = pure (Right ())


-------------------------------------------------------------------------------
-- Metric exporter
-------------------------------------------------------------------------------

-- | OTLP/gRPC metric exporter.
data OtlpGrpcMetricExporter = OtlpGrpcMetricExporter
  { _metricClientConfig :: GrpcClientConfig
  , _metricRetryConfig  :: RetryConfig
  , _metricShutdown     :: TVar Bool
  }


-- | Create an OTLP/gRPC metric exporter from explicit config.
newOtlpGrpcMetricExporter :: OtlpGrpcConfig -> IO OtlpGrpcMetricExporter
newOtlpGrpcMetricExporter cfg = do
  s <- newTVarIO False
  pure OtlpGrpcMetricExporter
    { _metricClientConfig = configToGrpcClientConfig cfg
    , _metricRetryConfig  = defaultRetryConfig
    , _metricShutdown     = s
    }


-- | Create an OTLP/gRPC metric exporter reading config from environment variables.
newOtlpGrpcMetricExporterFromEnv :: IO OtlpGrpcMetricExporter
newOtlpGrpcMetricExporterFromEnv = do
  cfg <- readConfigFromEnv "METRICS"
  newOtlpGrpcMetricExporter cfg


instance MetricExporter OtlpGrpcMetricExporter where
  exportMetrics e md = do
    done <- readTVarIO e._metricShutdown
    if done
      then pure ExportFailure
      else do
        let body = encodeMessage (metricDataToExportRequest md)
        result <- withRetry e._metricRetryConfig $ \_ ->
          unaryRpc e._metricClientConfig metricsPath body
        case result of
          Right respBytes -> parseMetricsExportResult respBytes
          Left _          -> pure ExportFailure
    where
      metricsPath = "/opentelemetry.proto.collector.metrics.v1.MetricsService/Export"

  shutdownMetricExporter e = do
    atomically (writeTVar e._metricShutdown True)
    pure (Right ())

  forceFlushMetricExporter _ _ = pure (Right ())

  exporterTemporality _ _ = Cumulative

  exporterDefaultAggregation _ _ = DefaultAggregation


-------------------------------------------------------------------------------
-- Log record exporter
-------------------------------------------------------------------------------

-- | OTLP/gRPC log record exporter.
data OtlpGrpcLogRecordExporter = OtlpGrpcLogRecordExporter
  { _logClientConfig :: GrpcClientConfig
  , _logRetryConfig  :: RetryConfig
  , _logShutdown     :: TVar Bool
  }


-- | Create an OTLP/gRPC log record exporter from explicit config.
newOtlpGrpcLogRecordExporter :: OtlpGrpcConfig -> IO OtlpGrpcLogRecordExporter
newOtlpGrpcLogRecordExporter cfg = do
  s <- newTVarIO False
  pure OtlpGrpcLogRecordExporter
    { _logClientConfig = configToGrpcClientConfig cfg
    , _logRetryConfig  = defaultRetryConfig
    , _logShutdown     = s
    }


-- | Create an OTLP/gRPC log exporter reading config from environment variables.
newOtlpGrpcLogRecordExporterFromEnv :: IO OtlpGrpcLogRecordExporter
newOtlpGrpcLogRecordExporterFromEnv = do
  cfg <- readConfigFromEnv "LOGS"
  newOtlpGrpcLogRecordExporter cfg


instance LogRecordExporter OtlpGrpcLogRecordExporter where
  exportLogRecords e records = do
    done <- readTVarIO e._logShutdown
    if done
      then pure ExportFailure
      else do
        let body = encodeMessage (logListToExportRequest records)
        result <- withRetry e._logRetryConfig $ \_ ->
          unaryRpc e._logClientConfig logsPath body
        case result of
          Right respBytes -> parseLogsExportResult respBytes
          Left _          -> pure ExportFailure
    where
      logsPath = "/opentelemetry.proto.collector.logs.v1.LogsService/Export"

  shutdownLogExporter e = do
    atomically (writeTVar e._logShutdown True)
    pure (Right ())

  forceFlushLogExporter _ _ = pure (Right ())


-------------------------------------------------------------------------------
-- Config to GrpcClientConfig
-------------------------------------------------------------------------------

configToGrpcClientConfig :: OtlpGrpcConfig -> GrpcClientConfig
configToGrpcClientConfig cfg =
  let (host, port, tls) = parseEndpoint cfg.otlpEndpoint
  in defaultGrpcClientConfig
    { grpcHost          = host
    , grpcPort          = port
    , grpcTls           = case tls of
                            Just _ -> tls
                            Nothing -> cfg.otlpTls
    , grpcCompression   = cfg.otlpCompression
    , grpcHeaders       = cfg.otlpHeaders
    , grpcTimeoutMicros = Just (cfg.otlpTimeoutMs * 1000)
    }


parseEndpoint :: Text -> (ByteString, Int, Maybe TlsConfig)
parseEndpoint endpoint
  | Just rest <- T.stripPrefix "https://" endpoint =
      let (host, port) = splitHostPort rest
      in (TE.encodeUtf8 host, port, Just defaultTlsConfig)
  | Just rest <- T.stripPrefix "http://" endpoint =
      let (host, port) = splitHostPort rest
      in (TE.encodeUtf8 host, port, Nothing)
  | otherwise =
      let (host, port) = splitHostPort endpoint
      in (TE.encodeUtf8 host, port, Nothing)


defaultTlsConfig :: TlsConfig
defaultTlsConfig = TlsConfig
  { tlsCaStore = Nothing
  , tlsSkipVerify = False
  }


splitHostPort :: Text -> (Text, Int)
splitHostPort t =
  case T.splitOn ":" t of
    [h, p] -> case readMaybe (T.unpack p) of
      Just port -> (h, port)
      Nothing   -> (h, 4317)
    [h] -> (h, 4317)
    _   -> (t, 4317)


-------------------------------------------------------------------------------
-- Environment variable reading
-------------------------------------------------------------------------------

readConfigFromEnv :: String -> IO OtlpGrpcConfig
readConfigFromEnv signal = do
  endpointVal <- lookupEnvSignal signal "ENDPOINT"
  headersVal <- lookupEnvSignal signal "HEADERS"
  compressionVal <- lookupEnvSignal signal "COMPRESSION"
  timeoutVal <- lookupEnvSignal signal "TIMEOUT"

  let baseCfg = defaultOtlpGrpcConfig

  let cfgWithEndpoint = case endpointVal of
        Nothing -> baseCfg
        Just ep ->
          let epText = T.pack ep
              (_, _, tls) = parseEndpoint epText
          in baseCfg
            { otlpEndpoint = epText
            , otlpTls = tls
            }

  let cfgWithHeaders = case headersVal of
        Nothing -> cfgWithEndpoint
        Just hdrStr -> cfgWithEndpoint
          { otlpHeaders = parseHeaders hdrStr }

  let cfgWithCompression = case compressionVal of
        Nothing -> cfgWithHeaders
        Just "gzip" -> cfgWithHeaders { otlpCompression = GzipCompression }
        Just _ -> cfgWithHeaders { otlpCompression = NoCompression }

  let cfgWithTimeout = case timeoutVal of
        Nothing -> cfgWithCompression
        Just tStr -> case readMaybe tStr of
          Just ms -> cfgWithCompression { otlpTimeoutMs = ms }
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
        []    -> []
        _:rest -> splitOn c rest
