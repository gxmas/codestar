-- | MetricReader type class and PeriodicExportingMetricReader.
--
-- A MetricReader is the bridge between the SDK's internal instrument registry
-- and a MetricExporter. The provider wires up a collect source (an @IO MetricData@
-- action) at registration time; the reader invokes it to gather data and then
-- exports.
module OTel.SDK.Metric.Reader
  ( -- * MetricReader type class
    MetricReader (..)
  , SomeMetricReader (..)
  , NoOpMetricReader
  , newNoOpMetricReader
    -- * PeriodicExportingMetricReader
  , PeriodicExportingMetricReaderConfig (..)
  , defaultPeriodicExportingMetricReaderConfig
  , PeriodicExportingMetricReader
  , newPeriodicExportingMetricReader
    -- * Helpers
  , defaultAggregationFor
  , defaultHistogramBoundaries
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word64)

import OTel.SDK.Export (ExportResult (..), FlushError (..), ShutdownError)
import OTel.SDK.Metric.Export
import OTel.SDK.Resource qualified as Resource
import OTel.Timestamp (Duration (..), milliseconds)


-------------------------------------------------------------------------------
-- MetricReader type class
-------------------------------------------------------------------------------

-- | Interface for collecting and exporting metrics on a schedule or on demand.
class MetricReader r where
  -- | Collect metrics from the registered source. Returns empty MetricData
  -- if no source has been registered.
  readerCollect :: r -> IO MetricData

  -- | Wire up the collect source. Called by SdkMeterProvider at registration.
  readerSetCollectSource :: r -> IO MetricData -> IO ()

  -- | Shut down the reader and any background workers.
  readerShutdown :: r -> IO (Either ShutdownError ())

  -- | Force flush any pending exports.
  readerForceFlush :: r -> Maybe Duration -> IO (Either FlushError ())

  -- | Preferred aggregation temporality for a given instrument kind.
  readerTemporality :: r -> InstrumentKind -> AggregationTemporality

  -- | Default aggregation for a given instrument kind.
  readerDefaultAggregation :: r -> InstrumentKind -> Aggregation


-- | Existential wrapper for any 'MetricReader' implementation.
data SomeMetricReader = forall r. MetricReader r => SomeMetricReader r

instance Show SomeMetricReader where
  show _ = "SomeMetricReader"

instance MetricReader SomeMetricReader where
  readerCollect (SomeMetricReader r) = readerCollect r
  readerSetCollectSource (SomeMetricReader r) = readerSetCollectSource r
  readerShutdown (SomeMetricReader r) = readerShutdown r
  readerForceFlush (SomeMetricReader r) = readerForceFlush r
  readerTemporality (SomeMetricReader r) = readerTemporality r
  readerDefaultAggregation (SomeMetricReader r) = readerDefaultAggregation r


-------------------------------------------------------------------------------
-- NoOpMetricReader
-------------------------------------------------------------------------------

-- | A metric reader that discards all collected data.
newtype NoOpMetricReader = NoOpMetricReader (IORef (IO MetricData))

-- | Create a new no-op metric reader.
newNoOpMetricReader :: IO NoOpMetricReader
newNoOpMetricReader = NoOpMetricReader <$> newIORef (pure emptyMetricData)

emptyMetricData :: MetricData
emptyMetricData = MetricData Resource.empty []

instance MetricReader NoOpMetricReader where
  readerCollect (NoOpMetricReader ref) = do
    action <- readIORef ref
    action
  readerSetCollectSource (NoOpMetricReader ref) src = writeIORef ref src
  readerShutdown _ = pure (Right ())
  readerForceFlush _ _ = pure (Right ())
  readerTemporality _ _ = Cumulative
  readerDefaultAggregation _ kind = defaultAggregationFor kind


-------------------------------------------------------------------------------
-- PeriodicExportingMetricReader
-------------------------------------------------------------------------------

-- | Configuration for the periodic exporting metric reader.
data PeriodicExportingMetricReaderConfig = PeriodicExportingMetricReaderConfig
  { pemrExportInterval :: !Duration  -- ^ default 60000ms per spec
  , pemrExportTimeout  :: !Duration  -- ^ default 30000ms per spec
  } deriving stock (Eq, Show)


-- | Default config: 60s export interval, 30s timeout.
defaultPeriodicExportingMetricReaderConfig :: PeriodicExportingMetricReaderConfig
defaultPeriodicExportingMetricReaderConfig = PeriodicExportingMetricReaderConfig
  { pemrExportInterval = milliseconds 60000
  , pemrExportTimeout  = milliseconds 30000
  }


-- | A metric reader that periodically exports via a background worker.
data PeriodicExportingMetricReader = PeriodicExportingMetricReader
  { pemrExporter      :: !SomeMetricExporter
  , pemrConfig        :: !PeriodicExportingMetricReaderConfig
  , pemrCollectSource :: !(IORef (IO MetricData))
  , pemrShutdownFlag  :: !(TVar Bool)
  , pemrWorker        :: !(Async ())
  }


-- | Create a new periodic exporting metric reader. Starts a background
-- worker that collects and exports at the configured interval.
newPeriodicExportingMetricReader
  :: SomeMetricExporter
  -> PeriodicExportingMetricReaderConfig
  -> IO PeriodicExportingMetricReader
newPeriodicExportingMetricReader exporter config = do
  collectRef <- newIORef (pure emptyMetricData)
  shutdownFlag <- newTVarIO False
  worker <- async (workerLoop collectRef shutdownFlag config exporter)
  pure PeriodicExportingMetricReader
    { pemrExporter      = exporter
    , pemrConfig        = config
    , pemrCollectSource = collectRef
    , pemrShutdownFlag  = shutdownFlag
    , pemrWorker        = worker
    }


workerLoop
  :: IORef (IO MetricData)
  -> TVar Bool
  -> PeriodicExportingMetricReaderConfig
  -> SomeMetricExporter
  -> IO ()
workerLoop collectRef shutdownFlag config exporter = go
  where
    intervalMicros :: Int
    intervalMicros = fromIntegral @Word64 @Int (unDuration (pemrExportInterval config) `div` 1000)

    go :: IO ()
    go = do
      threadDelay intervalMicros
      isShutdown <- readTVarIO shutdownFlag
      if isShutdown
        then pure ()
        else do
          collectAction <- readIORef collectRef
          md <- collectAction
          _ <- exportMetrics exporter md
          go


instance MetricReader PeriodicExportingMetricReader where
  readerCollect r = do
    action <- readIORef (pemrCollectSource r)
    action

  readerSetCollectSource r src = writeIORef (pemrCollectSource r) src

  readerTemporality r kind = exporterTemporality (pemrExporter r) kind
  readerDefaultAggregation r kind = exporterDefaultAggregation (pemrExporter r) kind

  readerShutdown r = do
    atomically $ writeTVar (pemrShutdownFlag r) True
    cancel (pemrWorker r)
    -- Final flush: collect and export remaining data
    collectAction <- readIORef (pemrCollectSource r)
    md <- collectAction
    _ <- exportMetrics (pemrExporter r) md
    _ <- shutdownMetricExporter (pemrExporter r)
    pure (Right ())

  readerForceFlush r _timeout = do
    collectAction <- readIORef (pemrCollectSource r)
    md <- collectAction
    result <- exportMetrics (pemrExporter r) md
    case result of
      ExportSuccess -> pure (Right ())
      ExportFailure -> pure (Left (FlushError "PeriodicExportingMetricReader" False Nothing))


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Default histogram boundaries per the OTel specification.
defaultHistogramBoundaries :: [Double]
defaultHistogramBoundaries =
  [0, 5, 10, 25, 50, 75, 100, 250, 500, 750, 1000, 2500, 5000, 7500, 10000]


-- | The spec-mandated default aggregation for each instrument kind.
defaultAggregationFor :: InstrumentKind -> Aggregation
defaultAggregationFor CounterKind                 = SumAggregation
defaultAggregationFor UpDownCounterKind           = SumAggregation
defaultAggregationFor HistogramKind               = ExplicitBucketHistogramAggregation defaultHistogramBoundaries
defaultAggregationFor GaugeKind                   = LastValueAggregation
defaultAggregationFor ObservableCounterKind       = SumAggregation
defaultAggregationFor ObservableUpDownCounterKind = SumAggregation
defaultAggregationFor ObservableGaugeKind         = LastValueAggregation
