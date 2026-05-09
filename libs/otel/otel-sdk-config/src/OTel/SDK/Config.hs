-- | File-based SDK configuration: parse YAML config files, apply environment
-- variable overrides, and construct SDK providers.
module OTel.SDK.Config
  ( -- * Error types
    ConfigParseError (..)
  , ConfigBuildError (..)

    -- * Configuration
  , Configuration

    -- * Parsing
  , parseYaml
  , parse

    -- * Provider construction
  , createTracerProvider
  , createMeterProvider
  , createLoggerProvider
  , createPropagator
  ) where

import Data.Aeson (Value (..), (.:?), withObject)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEnc
import Data.Yaml (ParseException, prettyPrintParseException)
import Data.Yaml qualified as Yaml
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import OTel.Attribute (AttributeValue (..))
import OTel.Exporter.Console
  ( newConsoleLogRecordExporter
  , newConsoleMetricExporter
  , newConsoleSpanExporter
  )
import OTel.Propagation
  ( CompositePropagator (..)
  , SomeTextMapPropagator (..)
  )
import OTel.Propagation.W3C
  ( W3CBaggagePropagator (..)
  , W3CTraceContextPropagator (..)
  )
import OTel.SDK.Log
  ( SdkLoggerProvider
  , SdkLoggerProviderConfig (..)
  , defaultSdkLoggerProviderConfig
  , newSdkLoggerProvider
  , SomeLogRecordExporter (..)
  , SomeLogRecordProcessor (..)
  )
import OTel.SDK.Log.Processor
  ( BatchLogRecordProcessorConfig (..)
  , defaultBatchLogRecordProcessorConfig
  , newBatchLogRecordProcessor
  , newSimpleLogRecordProcessor
  )
import OTel.SDK.Metric
  ( SdkMeterProvider
  , SdkMeterProviderConfig (..)
  , defaultSdkMeterProviderConfig
  , newSdkMeterProvider
  , SomeMetricExporter (..)
  , SomeMetricReader (..)
  )
import OTel.SDK.Metric.Reader
  ( PeriodicExportingMetricReaderConfig (..)
  , defaultPeriodicExportingMetricReaderConfig
  , newPeriodicExportingMetricReader
  )
import OTel.SDK.Resource qualified as Resource
import OTel.SDK.Trace
  ( SdkTracerProvider
  , SdkTracerProviderConfig (..)
  , defaultSdkTracerProviderConfig
  , newSdkTracerProvider
  )
import OTel.SDK.Trace.Export (SomeSpanExporter (..))
import OTel.SDK.Trace.Processor
  ( BatchSpanProcessorConfig (..)
  , SomeSpanProcessor (..)
  , defaultBatchSpanProcessorConfig
  , newBatchSpanProcessor
  , newSimpleSpanProcessor
  )
import OTel.SDK.Trace.Sampler
  ( AlwaysOffSampler (..)
  , AlwaysOnSampler (..)
  , SomeSampler (..)
  , TraceIdRatioBasedSampler (..)
  , defaultParentBasedSampler
  )
import OTel.Timestamp (milliseconds)


-------------------------------------------------------------------------------
-- Error types
-------------------------------------------------------------------------------

-- | Error from parsing the YAML configuration file.
data ConfigParseError = ConfigParseError
  { cpeSource   :: !Text
  , cpePosition :: !(Maybe (Int, Int))
  , cpeMessage  :: !Text
  } deriving stock (Eq, Show)


-- | Error from building SDK components from the parsed configuration.
data ConfigBuildError = ConfigBuildError
  { cbeSection :: !Text
  , cbeMessage :: !Text
  } deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- Configuration types (opaque)
-------------------------------------------------------------------------------

-- | Parsed SDK configuration from a YAML file.
data Configuration = Configuration
  { _cfgResource       :: !ResourceConfig
  , _cfgTracerProvider :: !(Maybe TracerProviderConfig)
  , _cfgMeterProvider  :: !(Maybe MeterProviderConfig)
  , _cfgLoggerProvider :: !(Maybe LoggerProviderConfig)
  , _cfgPropagators    :: ![Text]
  }


data ResourceConfig = ResourceConfig
  { _rcServiceName :: !(Maybe Text)
  , _rcAttributes  :: ![(Text, Text)]
  }


data SamplerConfig
  = SamplerAlwaysOn
  | SamplerAlwaysOff
  | SamplerTraceIdRatio !Double
  | SamplerParentBasedAlwaysOn
  | SamplerParentBasedAlwaysOff
  | SamplerParentBasedTraceIdRatio !Double
  deriving stock (Eq, Show)


data ExporterRef = ExporterConsole
  deriving stock (Eq, Show)


data SpanProcessorConfig
  = SpanProcessorBatch
      { _spcExporter      :: !ExporterRef
      , _spcScheduleDelay :: !Int
      , _spcExportTimeout :: !Int
      , _spcMaxQueueSize  :: !Int
      , _spcMaxBatchSize  :: !Int
      }
  | SpanProcessorSimple !ExporterRef
  deriving stock (Eq, Show)


data TracerProviderConfig = TracerProviderConfig
  { _tpcSampler    :: !SamplerConfig
  , _tpcProcessors :: ![SpanProcessorConfig]
  } deriving stock (Eq, Show)


data MetricReaderConfig
  = MetricReaderPeriodic
      { _mrcExporter :: !ExporterRef
      , _mrcInterval :: !Int
      , _mrcTimeout  :: !Int
      }
  deriving stock (Eq, Show)


data MeterProviderConfig = MeterProviderConfig
  { _mpcReaders :: ![MetricReaderConfig]
  } deriving stock (Eq, Show)


data LogProcessorConfig
  = LogProcessorBatch
      { _lpcExporter      :: !ExporterRef
      , _lpcScheduleDelay :: !Int
      , _lpcMaxQueueSize  :: !Int
      , _lpcMaxBatchSize  :: !Int
      }
  | LogProcessorSimple !ExporterRef
  deriving stock (Eq, Show)


data LoggerProviderConfig = LoggerProviderConfig
  { _llpcProcessors :: ![LogProcessorConfig]
  } deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- | Parse a YAML string into a 'Configuration'.
parseYaml :: Text -> IO (Either ConfigParseError Configuration)
parseYaml txt =
  let bs = TextEnc.encodeUtf8 txt
  in case Yaml.decodeEither' bs of
       Left exc -> pure (Left (parseExceptionToError "<string>" exc))
       Right val -> pure (valueToConfig "<string>" val)

-- | Parse a YAML file into a 'Configuration'.
parse :: FilePath -> IO (Either ConfigParseError Configuration)
parse fp = do
  bs <- BS.readFile fp
  let src = Text.pack fp
  case Yaml.decodeEither' bs of
    Left exc -> pure (Left (parseExceptionToError src exc))
    Right val -> pure (valueToConfig src val)


parseExceptionToError :: Text -> ParseException -> ConfigParseError
parseExceptionToError src exc = ConfigParseError
  { cpeSource   = src
  , cpePosition = Nothing
  , cpeMessage  = Text.pack (prettyPrintParseException exc)
  }


valueToConfig :: Text -> Value -> Either ConfigParseError Configuration
valueToConfig src val =
  case parseEither parseConfigValue val of
    Left msg -> Left ConfigParseError
      { cpeSource   = src
      , cpePosition = Nothing
      , cpeMessage  = Text.pack msg
      }
    Right cfg -> Right cfg


parseConfigValue :: Value -> Parser Configuration
parseConfigValue = withObject "Configuration" $ \o -> do
  mSdk <- o .:? Key.fromText "sdk"
  mPropagators <- o .:? Key.fromText "propagators"
  let propagators = fromMaybe [] mPropagators
  case mSdk of
    Nothing -> pure Configuration
      { _cfgResource       = emptyResourceConfig
      , _cfgTracerProvider = Nothing
      , _cfgMeterProvider  = Nothing
      , _cfgLoggerProvider = Nothing
      , _cfgPropagators    = propagators
      }
    Just sdkVal -> withObject "sdk" (parseSdk propagators) sdkVal

  where
    parseSdk :: [Text] -> KM.KeyMap Value -> Parser Configuration
    parseSdk propagators sdk = do
      mRes <- sdk .:? Key.fromText "resource"
      mTp  <- sdk .:? Key.fromText "tracer_provider"
      mMp  <- sdk .:? Key.fromText "meter_provider"
      mLp  <- sdk .:? Key.fromText "logger_provider"
      resource <- case mRes of
        Nothing -> pure emptyResourceConfig
        Just rv -> parseResourceConfig rv
      tp <- case mTp of
        Nothing  -> pure Nothing
        Just tpv -> Just <$> parseTracerProviderConfig tpv
      mp <- case mMp of
        Nothing  -> pure Nothing
        Just mpv -> Just <$> parseMeterProviderConfig mpv
      lp <- case mLp of
        Nothing  -> pure Nothing
        Just lpv -> Just <$> parseLoggerProviderConfig lpv
      pure Configuration
        { _cfgResource       = resource
        , _cfgTracerProvider = tp
        , _cfgMeterProvider  = mp
        , _cfgLoggerProvider = lp
        , _cfgPropagators    = propagators
        }


emptyResourceConfig :: ResourceConfig
emptyResourceConfig = ResourceConfig Nothing []


defaultTracerProviderConfig :: TracerProviderConfig
defaultTracerProviderConfig = TracerProviderConfig
  { _tpcSampler    = SamplerParentBasedAlwaysOn  -- spec default per OTEL_TRACES_SAMPLER
  , _tpcProcessors = []
  }


parseResourceConfig :: Value -> Parser ResourceConfig
parseResourceConfig = withObject "resource" $ \o -> do
  mSvcName <- o .:? Key.fromText "service_name"
  mAttrsObj <- o .:? Key.fromText "attributes"
  let attrs = case mAttrsObj of
        Nothing          -> []
        Just (Object km) -> [ (Key.toText k, textFromValue v)
                            | (k, v) <- KM.toList km
                            ]
        Just _           -> []
  pure ResourceConfig
    { _rcServiceName = mSvcName
    , _rcAttributes  = attrs
    }


textFromValue :: Value -> Text
textFromValue (String t) = t
textFromValue (Number n) = Text.pack (show n)
textFromValue (Bool b)   = if b then "true" else "false"
textFromValue Null       = ""
textFromValue _          = ""


parseTracerProviderConfig :: Value -> Parser TracerProviderConfig
parseTracerProviderConfig = withObject "tracer_provider" $ \o -> do
  mSampler    <- o .:? Key.fromText "sampler"
  mSamplerArg <- o .:? Key.fromText "sampler_arg"
  mProcs      <- o .:? Key.fromText "processors"
  let samplerName = fromMaybe "always_on" mSampler
  sampler <- case lookupSamplerConfig samplerName mSamplerArg of
    Nothing -> fail ("Unknown sampler: " <> Text.unpack samplerName)
    Just s  -> pure s
  let procs = fromMaybe [] mProcs
  parsedProcs <- mapM parseSpanProcessorConfig procs
  pure TracerProviderConfig
    { _tpcSampler    = sampler
    , _tpcProcessors = parsedProcs
    }


lookupSamplerConfig :: Text -> Maybe Double -> Maybe SamplerConfig
lookupSamplerConfig "always_on"              _    = Just SamplerAlwaysOn
lookupSamplerConfig "always_off"             _    = Just SamplerAlwaysOff
lookupSamplerConfig "traceidratio"           mArg = Just (SamplerTraceIdRatio (fromMaybe 1.0 mArg))
lookupSamplerConfig "parentbased_always_on"  _    = Just SamplerParentBasedAlwaysOn
lookupSamplerConfig "parentbased_always_off" _    = Just SamplerParentBasedAlwaysOff
lookupSamplerConfig "parentbased_traceidratio" mArg =
  Just (SamplerParentBasedTraceIdRatio (fromMaybe 1.0 mArg))
lookupSamplerConfig _                        _    = Nothing


parseSpanProcessorConfig :: Value -> Parser SpanProcessorConfig
parseSpanProcessorConfig = withObject "span_processor" $ \o -> do
  ty <- fromMaybe ("batch" :: Text) <$> o .:? Key.fromText "type"
  exporterName <- fromMaybe ("console" :: Text) <$> o .:? Key.fromText "exporter"
  exporter <- parseExporterRef exporterName
  case ty of
    "simple" -> pure (SpanProcessorSimple exporter)
    _ -> do
      schedDelay <- fromMaybe 5000  <$> o .:? Key.fromText "schedule_delay"
      expTimeout <- fromMaybe 30000 <$> o .:? Key.fromText "export_timeout"
      maxQueue   <- fromMaybe 2048  <$> o .:? Key.fromText "max_queue_size"
      maxBatch   <- fromMaybe 512   <$> o .:? Key.fromText "max_export_batch_size"
      pure SpanProcessorBatch
        { _spcExporter      = exporter
        , _spcScheduleDelay = schedDelay
        , _spcExportTimeout = expTimeout
        , _spcMaxQueueSize  = maxQueue
        , _spcMaxBatchSize  = maxBatch
        }


parseExporterRef :: Text -> Parser ExporterRef
parseExporterRef "console" = pure ExporterConsole
parseExporterRef name      = fail ("Unknown exporter: " <> Text.unpack name)


parseMeterProviderConfig :: Value -> Parser MeterProviderConfig
parseMeterProviderConfig = withObject "meter_provider" $ \o -> do
  mReaders <- o .:? Key.fromText "readers"
  let readers = fromMaybe [] mReaders
  parsedReaders <- mapM parseMetricReaderConfig readers
  pure MeterProviderConfig { _mpcReaders = parsedReaders }


parseMetricReaderConfig :: Value -> Parser MetricReaderConfig
parseMetricReaderConfig = withObject "metric_reader" $ \o -> do
  exporterName <- fromMaybe ("console" :: Text) <$> o .:? Key.fromText "exporter"
  exporter <- parseExporterRef exporterName
  interval <- fromMaybe 60000 <$> o .:? Key.fromText "interval"
  tout     <- fromMaybe 30000 <$> o .:? Key.fromText "timeout"
  pure MetricReaderPeriodic
    { _mrcExporter = exporter
    , _mrcInterval = interval
    , _mrcTimeout  = tout
    }


parseLoggerProviderConfig :: Value -> Parser LoggerProviderConfig
parseLoggerProviderConfig = withObject "logger_provider" $ \o -> do
  mProcs <- o .:? Key.fromText "processors"
  let procs = fromMaybe [] mProcs
  parsedProcs <- mapM parseLogProcessorConfig procs
  pure LoggerProviderConfig { _llpcProcessors = parsedProcs }


parseLogProcessorConfig :: Value -> Parser LogProcessorConfig
parseLogProcessorConfig = withObject "log_processor" $ \o -> do
  ty <- fromMaybe ("batch" :: Text) <$> o .:? Key.fromText "type"
  exporterName <- fromMaybe ("console" :: Text) <$> o .:? Key.fromText "exporter"
  exporter <- parseExporterRef exporterName
  case ty of
    "simple" -> pure (LogProcessorSimple exporter)
    _ -> do
      schedDelay <- fromMaybe 1000 <$> o .:? Key.fromText "schedule_delay"
      maxQueue   <- fromMaybe 2048 <$> o .:? Key.fromText "max_queue_size"
      maxBatch   <- fromMaybe 512  <$> o .:? Key.fromText "max_export_batch_size"
      pure LogProcessorBatch
        { _lpcExporter      = exporter
        , _lpcScheduleDelay = schedDelay
        , _lpcMaxQueueSize  = maxQueue
        , _lpcMaxBatchSize  = maxBatch
        }


-------------------------------------------------------------------------------
-- Environment variable overrides
-------------------------------------------------------------------------------

applyEnvOverrides :: Configuration -> IO (Either ConfigBuildError Configuration)
applyEnvOverrides cfg = do
  mSvcName     <- lookupEnv "OTEL_SERVICE_NAME"
  mResAttrs    <- lookupEnv "OTEL_RESOURCE_ATTRIBUTES"
  mSampler     <- lookupEnv "OTEL_TRACES_SAMPLER"
  mSamplerArg  <- lookupEnv "OTEL_TRACES_SAMPLER_ARG"
  mPropagators <- lookupEnv "OTEL_PROPAGATORS"
  let samplerArgDouble = mSamplerArg >>= readMaybe
  case validateSamplerEnv mSampler samplerArgDouble of
    Left err -> pure (Left err)
    Right () -> pure (Right cfg
      { _cfgResource       = applyResourceEnv mSvcName mResAttrs (_cfgResource cfg)
      , _cfgTracerProvider =
          let base = fromMaybe defaultTracerProviderConfig (_cfgTracerProvider cfg)
          in case mSampler of
               Nothing -> _cfgTracerProvider cfg
               Just _  -> Just (applyTracerEnv mSampler samplerArgDouble base)
      , _cfgPropagators    = maybe (_cfgPropagators cfg) parsePropagators mPropagators
      })


validateSamplerEnv :: Maybe String -> Maybe Double -> Either ConfigBuildError ()
validateSamplerEnv Nothing     _    = Right ()
validateSamplerEnv (Just name) mArg =
  case lookupSamplerConfig (Text.pack name) mArg of
    Nothing -> Left ConfigBuildError
      { cbeSection = "tracer_provider"
      , cbeMessage = "Unknown OTEL_TRACES_SAMPLER value: " <> Text.pack name
      }
    Just _  -> Right ()


applyResourceEnv :: Maybe String -> Maybe String -> ResourceConfig -> ResourceConfig
applyResourceEnv mSvcName mResAttrs rc = rc
  { _rcServiceName = maybe (_rcServiceName rc) (Just . Text.pack) mSvcName
  -- config first, env last: Map.fromList last-wins makes env vars take precedence per spec
  , _rcAttributes  = _rcAttributes rc <> maybe [] parseResAttrs mResAttrs
  }


parseResAttrs :: String -> [(Text, Text)]
parseResAttrs s =
  [ (Text.strip k, Text.strip v)
  | pair <- Text.splitOn "," (Text.pack s)
  , let (k, rest) = Text.breakOn "=" pair
  , v <- maybeToList (Text.stripPrefix "=" rest)
  ]


applyTracerEnv :: Maybe String -> Maybe Double -> TracerProviderConfig -> TracerProviderConfig
applyTracerEnv Nothing     _    tpc = tpc
applyTracerEnv (Just name) mArg tpc = tpc
  { _tpcSampler = fromMaybe (_tpcSampler tpc) (lookupSamplerConfig (Text.pack name) mArg)
  }


parsePropagators :: String -> [Text]
parsePropagators s =
  [ Text.strip p | p <- Text.splitOn "," (Text.pack s), not (Text.null (Text.strip p)) ]


-------------------------------------------------------------------------------
-- Provider construction
-------------------------------------------------------------------------------

-- | Build an 'SdkTracerProvider' from a 'Configuration', applying env var overrides.
createTracerProvider :: Configuration -> IO (Either ConfigBuildError SdkTracerProvider)
createTracerProvider cfg = do
  eCfg <- applyEnvOverrides cfg
  case eCfg of
    Left err   -> pure (Left err)
    Right cfg' -> do
      resource <- buildResource (_cfgResource cfg')
      let tpc = _cfgTracerProvider cfg'
      sampler <- buildSampler (maybe SamplerParentBasedAlwaysOn _tpcSampler tpc)
      processors <- mapM buildSpanProcessor (maybe [] _tpcProcessors tpc)
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerResource   = resource
        , providerSampler    = sampler
        , providerProcessors = processors
        }
      pure (Right provider)


-- | Build an 'SdkMeterProvider' from a 'Configuration', applying env var overrides.
createMeterProvider :: Configuration -> IO (Either ConfigBuildError SdkMeterProvider)
createMeterProvider cfg = do
  eCfg <- applyEnvOverrides cfg
  case eCfg of
    Left err   -> pure (Left err)
    Right cfg' -> do
      resource <- buildResource (_cfgResource cfg')
      let mpc = _cfgMeterProvider cfg'
      readers <- mapM buildMetricReader (maybe [] _mpcReaders mpc)
      provider <- newSdkMeterProvider defaultSdkMeterProviderConfig
        { providerResource = resource
        , providerReaders  = readers
        }
      pure (Right provider)


-- | Build an 'SdkLoggerProvider' from a 'Configuration', applying env var overrides.
createLoggerProvider :: Configuration -> IO (Either ConfigBuildError SdkLoggerProvider)
createLoggerProvider cfg = do
  eCfg <- applyEnvOverrides cfg
  case eCfg of
    Left err   -> pure (Left err)
    Right cfg' -> do
      resource <- buildResource (_cfgResource cfg')
      let lpc = _cfgLoggerProvider cfg'
      processors <- mapM buildLogProcessor (maybe [] _llpcProcessors lpc)
      provider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
        { llpResource   = resource
        , llpProcessors = processors
        }
      pure (Right provider)


-- | Build a composite 'TextMapPropagator' from a 'Configuration'.
createPropagator :: Configuration -> IO (Either ConfigBuildError SomeTextMapPropagator)
createPropagator cfg = do
  eCfg <- applyEnvOverrides cfg
  case eCfg of
    Left err   -> pure (Left err)
    Right cfg' -> do
      let names = if null (_cfgPropagators cfg')
                  then ["tracecontext", "baggage"]
                  else _cfgPropagators cfg'
      case traverse buildOnePropagator names of
        Left err    -> pure (Left err)
        Right props -> pure (Right (SomeTextMapPropagator (CompositePropagator props)))


-------------------------------------------------------------------------------
-- Internal builders
-------------------------------------------------------------------------------

buildResource :: ResourceConfig -> IO Resource.Resource
buildResource rc = do
  let svcAttrs = case _rcServiceName rc of
        Nothing   -> []
        Just name -> [("service.name", StringValue name)]
  let cfgAttrs = [ (k, StringValue v) | (k, v) <- _rcAttributes rc ]
  pure (Resource.create (svcAttrs <> cfgAttrs) Nothing)


buildSampler :: SamplerConfig -> IO SomeSampler
buildSampler SamplerAlwaysOn           = pure (SomeSampler AlwaysOnSampler)
buildSampler SamplerAlwaysOff          = pure (SomeSampler AlwaysOffSampler)
buildSampler (SamplerTraceIdRatio r)   = pure (SomeSampler (TraceIdRatioBasedSampler r))
buildSampler SamplerParentBasedAlwaysOn =
  pure (SomeSampler (defaultParentBasedSampler (SomeSampler AlwaysOnSampler)))
buildSampler SamplerParentBasedAlwaysOff =
  pure (SomeSampler (defaultParentBasedSampler (SomeSampler AlwaysOffSampler)))
buildSampler (SamplerParentBasedTraceIdRatio r) =
  pure (SomeSampler (defaultParentBasedSampler (SomeSampler (TraceIdRatioBasedSampler r))))


buildSpanProcessor :: SpanProcessorConfig -> IO SomeSpanProcessor
buildSpanProcessor (SpanProcessorSimple ref) = do
  exporter <- buildSpanExporter ref
  proc <- newSimpleSpanProcessor exporter
  pure (SomeSpanProcessor proc)
buildSpanProcessor spc = do
  exporter <- buildSpanExporter (_spcExporter spc)
  let bspCfg = defaultBatchSpanProcessorConfig
        { bspScheduledDelay     = milliseconds (fromIntegral (_spcScheduleDelay spc))
        , bspExportTimeout      = milliseconds (fromIntegral (_spcExportTimeout spc))
        , bspMaxQueueSize       = _spcMaxQueueSize spc
        , bspMaxExportBatchSize = _spcMaxBatchSize spc
        }
  proc <- newBatchSpanProcessor exporter bspCfg
  pure (SomeSpanProcessor proc)


buildSpanExporter :: ExporterRef -> IO SomeSpanExporter
buildSpanExporter ExporterConsole = SomeSpanExporter <$> newConsoleSpanExporter


buildMetricReader :: MetricReaderConfig -> IO SomeMetricReader
buildMetricReader mrc = do
  exporter <- buildMetricExporter (_mrcExporter mrc)
  let readerCfg = defaultPeriodicExportingMetricReaderConfig
        { pemrExportInterval = milliseconds (fromIntegral (_mrcInterval mrc))
        , pemrExportTimeout  = milliseconds (fromIntegral (_mrcTimeout mrc))
        }
  reader <- newPeriodicExportingMetricReader exporter readerCfg
  pure (SomeMetricReader reader)


buildMetricExporter :: ExporterRef -> IO SomeMetricExporter
buildMetricExporter ExporterConsole = SomeMetricExporter <$> newConsoleMetricExporter


buildLogProcessor :: LogProcessorConfig -> IO SomeLogRecordProcessor
buildLogProcessor (LogProcessorSimple ref) = do
  exporter <- buildLogExporter ref
  proc <- newSimpleLogRecordProcessor exporter
  pure (SomeLogRecordProcessor proc)
buildLogProcessor lpc = do
  exporter <- buildLogExporter (_lpcExporter lpc)
  let blrpCfg = defaultBatchLogRecordProcessorConfig
        { blrpScheduledDelay     = milliseconds (fromIntegral (_lpcScheduleDelay lpc))
        , blrpMaxQueueSize       = _lpcMaxQueueSize lpc
        , blrpMaxExportBatchSize = _lpcMaxBatchSize lpc
        }
  proc <- newBatchLogRecordProcessor exporter blrpCfg
  pure (SomeLogRecordProcessor proc)


buildLogExporter :: ExporterRef -> IO SomeLogRecordExporter
buildLogExporter ExporterConsole = SomeLogRecordExporter <$> newConsoleLogRecordExporter


buildOnePropagator :: Text -> Either ConfigBuildError SomeTextMapPropagator
buildOnePropagator "tracecontext" = Right (SomeTextMapPropagator W3CTraceContextPropagator)
buildOnePropagator "baggage"      = Right (SomeTextMapPropagator W3CBaggagePropagator)
buildOnePropagator name           = Left ConfigBuildError
  { cbeSection = "propagators"
  , cbeMessage = "Unknown propagator: " <> name
  }
