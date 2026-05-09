-- | Prometheus metric exporter for OpenTelemetry.
--
-- Implements a 'MetricReader' that starts an HTTP server exposing metrics
-- in Prometheus text format (version 0.0.4). The server is started when
-- the SDK wires up the collect source via 'readerSetCollectSource'.
module OTel.Exporter.Prometheus
  ( PrometheusConfig (..)
  , defaultPrometheusConfig
  , PrometheusMetricReader
  , newPrometheusMetricReader
  ) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (join)
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum, isDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (scanl')
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp
  ( defaultSettings, runSettings, setBeforeMainLoop
  , setGracefulShutdownTimeout, setHost, setLogger, setPort
  )
import Numeric (showFFloat)

import OTel.Attribute (Attributes, AttributeValue (..), InstrumentationScope (..), toList)
import OTel.SDK.Metric.Export
import OTel.SDK.Export (FlushError (..))
import OTel.SDK.Metric.Reader (MetricReader (..), defaultHistogramBoundaries)
import OTel.SDK.Resource qualified as Resource
import OTel.Timestamp (Timestamp (..))


-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | Configuration for the Prometheus exporter HTTP server.
data PrometheusConfig = PrometheusConfig
  { prometheusHost                :: String
  , prometheusPort                :: Int
  , prometheusPath                :: String
  , prometheusResourceToTelemetry :: Bool
  , prometheusWithoutUnits        :: Bool
  , prometheusWithoutTypeSuffix   :: Bool
  , prometheusWithoutScopeInfo    :: Bool
  } deriving stock (Show)


-- | Default configuration: listen on 127.0.0.1:9464, serve at @/metrics@.
defaultPrometheusConfig :: PrometheusConfig
defaultPrometheusConfig = PrometheusConfig
  { prometheusHost                = "127.0.0.1"
  , prometheusPort                = 9464
  , prometheusPath                = "/metrics"
  , prometheusResourceToTelemetry = False
  , prometheusWithoutUnits        = False
  , prometheusWithoutTypeSuffix   = False
  , prometheusWithoutScopeInfo    = False
  }


-------------------------------------------------------------------------------
-- PrometheusMetricReader
-------------------------------------------------------------------------------

-- | A metric reader that serves metrics via an embedded Prometheus HTTP server.
data PrometheusMetricReader = PrometheusMetricReader
  { _promConfig     :: PrometheusConfig
  , _promCollectRef :: IORef (IO MetricData)
  , _promShutdown   :: TVar Bool
  , _promServer     :: TVar (Maybe (Async ()))
  }


-- | Create a new Prometheus metric reader. The HTTP server starts when the collect source is wired.
newPrometheusMetricReader :: PrometheusConfig -> IO PrometheusMetricReader
newPrometheusMetricReader cfg = do
  collectRef <- newIORef (pure emptyMetricData)
  shutdownVar <- newTVarIO False
  serverVar <- newTVarIO Nothing
  pure PrometheusMetricReader
    { _promConfig     = cfg
    , _promCollectRef = collectRef
    , _promShutdown   = shutdownVar
    , _promServer     = serverVar
    }


emptyMetricData :: MetricData
emptyMetricData = MetricData Resource.empty []


-------------------------------------------------------------------------------
-- MetricReader instance
-------------------------------------------------------------------------------

instance MetricReader PrometheusMetricReader where
  readerCollect r = do
    isShutdown <- readTVarIO r._promShutdown
    if isShutdown then pure emptyMetricData
    else join (readIORef r._promCollectRef)

  readerSetCollectSource r collect = do
    writeIORef r._promCollectRef collect
    a <- startWarpServer r._promConfig collect
    atomically (writeTVar r._promServer (Just a))

  readerShutdown r = do
    atomically (writeTVar r._promShutdown True)
    mA <- readTVarIO r._promServer
    mapM_ cancel mA
    pure (Right ())

  readerForceFlush r _ = do
    isShutdown <- readTVarIO r._promShutdown
    if isShutdown
      then pure (Left FlushError { flushComponent = "PrometheusMetricReader", flushTimedOut = False, flushCause = Nothing })
      else pure (Right ())

  readerTemporality _ _ = Cumulative

  readerDefaultAggregation _ kind = case kind of
    CounterKind                 -> SumAggregation
    UpDownCounterKind           -> SumAggregation
    HistogramKind               -> ExplicitBucketHistogramAggregation defaultHistogramBoundaries
    GaugeKind                   -> LastValueAggregation
    ObservableCounterKind       -> SumAggregation
    ObservableUpDownCounterKind -> SumAggregation
    ObservableGaugeKind         -> LastValueAggregation


-------------------------------------------------------------------------------
-- Warp server
-------------------------------------------------------------------------------

startWarpServer :: PrometheusConfig -> IO MetricData -> IO (Async ())
startWarpServer cfg collect = do
  readyVar <- newEmptyMVar
  a <- async $ do
    let settings
          = setHost (fromString cfg.prometheusHost)
          $ setPort cfg.prometheusPort
          $ setBeforeMainLoop (putMVar readyVar ())
          $ setGracefulShutdownTimeout (Just 1)
          $ setLogger (\_ _ _ -> pure ())
          $ defaultSettings
    runSettings settings (prometheusApp cfg collect)
  takeMVar readyVar
  pure a


prometheusApp :: PrometheusConfig -> IO MetricData -> Application
prometheusApp cfg collect req respond =
  if rawPathInfo req == B8.pack cfg.prometheusPath
    then do
      md <- collect
      let body = renderMetricData cfg md
      respond $ responseLBS status200
        [("Content-Type", "text/plain; version=0.0.4; charset=utf-8")]
        (LBS.fromStrict body)
    else respond $ responseLBS status404 [] "Not Found"


-------------------------------------------------------------------------------
-- Prometheus text format rendering
-------------------------------------------------------------------------------

renderMetricData :: PrometheusConfig -> MetricData -> ByteString
renderMetricData cfg md =
  LBS.toStrict $ Builder.toLazyByteString $ mconcat builders
  where
    resource = md.mdResource
    resourceLabels
      | cfg.prometheusResourceToTelemetry =
          map (attrToLabel . fmap attrValueToText) (toList (Resource.getAttributes resource))
      | otherwise = []

    builders = concatMap (renderScopeMetrics cfg resourceLabels) md.mdScopeMetrics


renderScopeMetrics
  :: PrometheusConfig -> [(ByteString, ByteString)] -> ScopeMetrics -> [Builder.Builder]
renderScopeMetrics cfg resourceLabels sm =
  concatMap (renderMetric cfg baseLabels) sm.smMetrics
  where
    scopeLabels
      | cfg.prometheusWithoutScopeInfo = []
      | otherwise =
          let nameLabel = ("otel_scope_name", escLabelValue (TE.encodeUtf8 sm.smScope.scopeName))
              versionLabel = case sm.smScope.scopeVersion of
                Just v  -> [("otel_scope_version", escLabelValue (TE.encodeUtf8 v))]
                Nothing -> []
          in nameLabel : versionLabel
    baseLabels = scopeLabels ++ resourceLabels


renderMetric
  :: PrometheusConfig -> [(ByteString, ByteString)] -> Metric -> [Builder.Builder]
renderMetric cfg baseLabels metric = case metric.metricPointData of
  SumPointData sumD ->
    let isMonotonic = sumD.sumIsMonotonic
        promType = if isMonotonic then "counter" else "gauge"
        fullName
          | isMonotonic && not cfg.prometheusWithoutTypeSuffix = baseName <> "_total"
          | otherwise = baseName
    in  helpLine fullName
     <> typeLine fullName promType
     <> concatMap (renderNumberDataPoint cfg fullName baseLabels) sumD.sumDataPoints

  GaugePointData gaugeD ->
       helpLine baseName
    <> typeLine baseName "gauge"
    <> concatMap (renderNumberDataPoint cfg baseName baseLabels) gaugeD.gaugeDataPoints

  HistogramPointData histD ->
       helpLine baseName
    <> typeLine baseName "histogram"
    <> concatMap (renderHistogramDataPoint cfg baseName baseLabels) histD.histDataPoints

  ExponentialHistogramPointData expD ->
       helpLine baseName
    <> typeLine baseName "gauge"
    <> concatMap (renderExpHistDataPoint cfg baseName baseLabels) expD.expHistDataPoints

  where
    baseName = sanitizeMetricName cfg metric
    desc = TE.encodeUtf8 metric.metricDescription

    helpLine name =
      [ Builder.byteString "# HELP "
        <> Builder.byteString name
        <> Builder.char8 ' '
        <> Builder.byteString (escHelpText desc)
        <> Builder.char8 '\n'
      ]

    typeLine name ty =
      [ Builder.byteString "# TYPE "
        <> Builder.byteString name
        <> Builder.char8 ' '
        <> Builder.byteString ty
        <> Builder.char8 '\n'
      ]


renderNumberDataPoint
  :: PrometheusConfig -> ByteString -> [(ByteString, ByteString)]
  -> NumberDataPoint -> [Builder.Builder]
renderNumberDataPoint _cfg name baseLabels ndp =
  [ Builder.byteString name
    <> labelsBuilder allLabels
    <> Builder.char8 ' '
    <> doubleBuilder ndp.ndpValue
    <> Builder.char8 ' '
    <> timestampBuilder ndp.ndpTime
    <> Builder.char8 '\n'
  ]
  where
    dpLabels = attrsToLabels ndp.ndpAttributes
    allLabels = dpLabels ++ baseLabels


renderHistogramDataPoint
  :: PrometheusConfig -> ByteString -> [(ByteString, ByteString)]
  -> HistogramDataPoint -> [Builder.Builder]
renderHistogramDataPoint cfg name baseLabels hdp =
  bucketLines ++ countLine ++ sumLine
  where
    dpLabels = attrsToLabels hdp.hdpAttributes
    allLabels = dpLabels ++ baseLabels
    ts = hdp.hdpTime

    -- Compute cumulative counts
    rawCounts = hdp.hdpBucketCounts
    cumCounts = scanl' (+) 0 rawCounts
    -- cumCounts has length = len(rawCounts) + 1, drop the leading 0
    cumulativeCounts = drop 1 cumCounts

    bounds = hdp.hdpExplicitBounds
    totalCount = hdp.hdpCount

    bucketSuffix = if cfg.prometheusWithoutTypeSuffix then "" else "_bucket"
    countSuffix  = if cfg.prometheusWithoutTypeSuffix then "" else "_count"
    sumSuffix    = if cfg.prometheusWithoutTypeSuffix then "" else "_sum"

    bucketName = name <> bucketSuffix
    countName  = name <> countSuffix
    sumName    = name <> sumSuffix

    -- Finite bound bucket lines
    finiteBuckets = zipWith mkBucketLine bounds cumulativeCounts
    -- +Inf bucket
    infBucket = mkBucketLineInf totalCount

    bucketLines = finiteBuckets ++ [infBucket]

    mkBucketLine bound count =
      Builder.byteString bucketName
      <> labelsBuilder (("le", B8.pack (formatDouble bound)) : allLabels)
      <> Builder.char8 ' '
      <> Builder.word64Dec count
      <> Builder.char8 ' '
      <> timestampBuilder ts
      <> Builder.char8 '\n'

    mkBucketLineInf count =
      Builder.byteString bucketName
      <> labelsBuilder (("le", "+Inf") : allLabels)
      <> Builder.char8 ' '
      <> Builder.word64Dec count
      <> Builder.char8 ' '
      <> timestampBuilder ts
      <> Builder.char8 '\n'

    countLine =
      [ Builder.byteString countName
        <> labelsBuilder allLabels
        <> Builder.char8 ' '
        <> Builder.word64Dec totalCount
        <> Builder.char8 ' '
        <> timestampBuilder ts
        <> Builder.char8 '\n'
      ]

    sumLine = case hdp.hdpSum of
      Just s ->
        [ Builder.byteString sumName
          <> labelsBuilder allLabels
          <> Builder.char8 ' '
          <> doubleBuilder s
          <> Builder.char8 ' '
          <> timestampBuilder ts
          <> Builder.char8 '\n'
        ]
      Nothing -> []


renderExpHistDataPoint
  :: PrometheusConfig -> ByteString -> [(ByteString, ByteString)]
  -> ExponentialHistogramDataPoint -> [Builder.Builder]
renderExpHistDataPoint _cfg name baseLabels ehdp =
  [ Builder.byteString name
    <> labelsBuilder allLabels
    <> Builder.char8 ' '
    <> doubleBuilder (fromIntegral @Word64 @Double ehdp.ehdpCount)
    <> Builder.char8 ' '
    <> timestampBuilder ehdp.ehdpTime
    <> Builder.char8 '\n'
  ]
  where
    dpLabels = attrsToLabels ehdp.ehdpAttributes
    allLabels = dpLabels ++ baseLabels


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

sanitizeMetricName :: PrometheusConfig -> Metric -> ByteString
sanitizeMetricName cfg metric =
  let rawName = sanitizeName (TE.encodeUtf8 metric.metricName)
      unitSuffix
        | cfg.prometheusWithoutUnits = ""
        | T.null metric.metricUnit = ""
        | T.isPrefixOf "{" metric.metricUnit = ""
        | otherwise = "_" <> sanitizeName (TE.encodeUtf8 metric.metricUnit)
  in rawName <> unitSuffix


sanitizeName :: ByteString -> ByteString
sanitizeName bs = B8.pack $ case B8.unpack bs of
  [] -> ""
  (c:cs)
    | isValidStart c -> c : map sanitizeChar cs
    | otherwise      -> '_' : map sanitizeChar cs
  where
    isValidStart c = c == '_' || c == ':' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    sanitizeChar c
      | isAlphaNum c || c == '_' || c == ':' = c
      | otherwise = '_'


sanitizeLabelKey :: ByteString -> ByteString
sanitizeLabelKey bs = B8.pack $ case B8.unpack bs of
  [] -> ""
  (c:cs)
    | isDigit c -> '_' : sanitizeLabelChar c : map sanitizeLabelChar cs
    | isValidLabelStart c -> c : map sanitizeLabelChar cs
    | otherwise -> '_' : map sanitizeLabelChar cs
  where
    isValidLabelStart c = c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    sanitizeLabelChar c
      | isAlphaNum c || c == '_' = c
      | otherwise = '_'


attrsToLabels :: Attributes -> [(ByteString, ByteString)]
attrsToLabels attrs =
  map (\(k, v) -> (sanitizeLabelKey (TE.encodeUtf8 k), escLabelValue (TE.encodeUtf8 (attrValueToText v))))
      (toList attrs)


attrToLabel :: (Text, Text) -> (ByteString, ByteString)
attrToLabel (k, v) = (sanitizeLabelKey (TE.encodeUtf8 k), escLabelValue (TE.encodeUtf8 v))


labelsBuilder :: [(ByteString, ByteString)] -> Builder.Builder
labelsBuilder [] = mempty
labelsBuilder ls = Builder.char8 '{' <> go ls <> Builder.char8 '}'
  where
    go [] = mempty
    go [x] = labelPair x
    go (x:xs) = labelPair x <> Builder.char8 ',' <> go xs
    labelPair (k, v) =
      Builder.byteString k <> Builder.byteString "=\"" <> Builder.byteString v <> Builder.char8 '"'


escLabelValue :: ByteString -> ByteString
escLabelValue = B8.concatMap $ \c -> case c of
  '\\' -> "\\\\"
  '"'  -> "\\\""
  '\n' -> "\\n"
  _    -> B8.singleton c


escHelpText :: ByteString -> ByteString
escHelpText = B8.concatMap $ \c -> case c of
  '\\' -> "\\\\"
  '\n' -> "\\n"
  _    -> B8.singleton c


doubleBuilder :: Double -> Builder.Builder
doubleBuilder v
  | isNaN v = Builder.byteString "NaN"
  | isInfinite v && v > 0 = Builder.byteString "+Inf"
  | isInfinite v = Builder.byteString "-Inf"
  | otherwise = Builder.string8 (showFFloat Nothing v "")


timestampBuilder :: Timestamp -> Builder.Builder
timestampBuilder (Timestamp nanos) =
  Builder.word64Dec (nanos `div` 1_000_000)


attrValueToText :: AttributeValue -> Text
attrValueToText (StringValue t) = t
attrValueToText (BoolValue True) = "true"
attrValueToText (BoolValue False) = "false"
attrValueToText (Int64Value i) = T.pack (show i)
attrValueToText (Float64Value d) = T.pack (showFFloat Nothing d "")
attrValueToText (StringArrayValue vs) = T.pack (show vs)
attrValueToText (BoolArrayValue vs) = T.pack (show vs)
attrValueToText (Int64ArrayValue vs) = T.pack (show vs)
attrValueToText (Float64ArrayValue vs) = T.pack (show vs)


formatDouble :: Double -> String
formatDouble v
  | isNaN v = "NaN"
  | isInfinite v && v > 0 = "+Inf"
  | isInfinite v = "-Inf"
  | otherwise = showFFloat Nothing v ""
