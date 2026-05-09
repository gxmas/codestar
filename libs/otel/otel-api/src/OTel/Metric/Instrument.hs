-- | Metric instrument types: synchronous and asynchronous instrument type
-- classes, no-op implementations, existential wrappers, and supporting types.
module OTel.Metric.Instrument
  ( -- * Instrument configuration
    AdvisoryParameters (..)
  , InstrumentOptions (..)
  , defaultInstrumentOptions

    -- * Observable result
  , ObservableResult (..)
  , NoOpObservableResult (..)
  , SomeObservableResult (..)

    -- * Batch observable result
  , BatchObservableResult (..)
  , NoOpBatchObservableResult (..)
  , SomeBatchObservableResult (..)

    -- * Callback registration
  , CallbackRegistration (..)
  , NoOpCallbackRegistration (..)
  , SomeCallbackRegistration (..)

    -- * Callback type aliases
  , ObservableCallback
  , BatchObservableCallback

    -- * Observable instrument sum
  , SomeObservableInstrument (..)
  , SomeObservableInstrKind (..)
  , mkSomeObsCounter
  , mkSomeObsUpDownCounter
  , mkSomeObsGauge

    -- * Synchronous instruments
  , Counter (..)
  , NoOpCounter (..)
  , SomeCounter (..)
  , UpDownCounter (..)
  , NoOpUpDownCounter (..)
  , SomeUpDownCounter (..)
  , Histogram (..)
  , NoOpHistogram (..)
  , SomeHistogram (..)
  , Gauge (..)
  , NoOpGauge (..)
  , SomeGauge (..)

    -- * Asynchronous (observable) instruments
  , ObservableCounter (..)
  , NoOpObservableCounter (..)
  , SomeObservableCounter (..)
  , ObservableUpDownCounter (..)
  , NoOpObservableUpDownCounter (..)
  , SomeObservableUpDownCounter (..)
  , ObservableGauge (..)
  , NoOpObservableGauge (..)
  , SomeObservableGauge (..)
  ) where

import Data.Text (Text)
import OTel.Attribute (Attributes)


-------------------------------------------------------------------------------
-- Instrument configuration
-------------------------------------------------------------------------------

-- | Advisory parameters that SDKs may use to configure aggregation.
data AdvisoryParameters = AdvisoryParameters
  { explicitBucketBoundaries :: ![Double]
  , advisoryAttributes       :: ![Text]
  } deriving stock (Eq, Show)


-- | Options for creating any instrument. Use 'defaultInstrumentOptions' and
-- override fields with record update syntax.
data InstrumentOptions = InstrumentOptions
  { instrumentDescription :: !(Maybe Text)
  , instrumentUnit        :: !(Maybe Text)
  , instrumentAdvisory    :: !(Maybe AdvisoryParameters)
  } deriving stock (Eq, Show)


-- | Sensible defaults: no description, no unit, no advisory parameters.
defaultInstrumentOptions :: InstrumentOptions
defaultInstrumentOptions = InstrumentOptions Nothing Nothing Nothing


-------------------------------------------------------------------------------
-- ObservableResult
-------------------------------------------------------------------------------

-- | A handle passed to observable callbacks so they can report observed values.
class ObservableResult r where
  observeValue :: r -> Double -> Attributes -> IO ()


-- | No-op implementation of 'ObservableResult' that discards all observations.
data NoOpObservableResult = NoOpObservableResult

instance ObservableResult NoOpObservableResult where
  observeValue _ _ _ = pure ()


-- | Existential wrapper for any 'ObservableResult'.
data SomeObservableResult = forall r. ObservableResult r => SomeObservableResult r

instance ObservableResult SomeObservableResult where
  observeValue (SomeObservableResult r) = observeValue r


-------------------------------------------------------------------------------
-- BatchObservableResult
-------------------------------------------------------------------------------

-- | A handle passed to batch observable callbacks. Allows reporting values
-- for multiple observable instruments in a single callback invocation.
class BatchObservableResult r where
  batchObserveValue :: r -> SomeObservableInstrument -> Double -> Attributes -> IO ()


-- | No-op batch observable result that discards all observations.
data NoOpBatchObservableResult = NoOpBatchObservableResult

instance BatchObservableResult NoOpBatchObservableResult where
  batchObserveValue _ _ _ _ = pure ()


-- | Existential wrapper for any 'BatchObservableResult'.
data SomeBatchObservableResult = forall r. BatchObservableResult r => SomeBatchObservableResult r

instance BatchObservableResult SomeBatchObservableResult where
  batchObserveValue (SomeBatchObservableResult r) = batchObserveValue r


-------------------------------------------------------------------------------
-- CallbackRegistration
-------------------------------------------------------------------------------

-- | A handle returned from 'registerCallback' that can be used to unregister
-- the callback.
class CallbackRegistration r where
  unregister :: r -> IO ()


-- | No-op callback registration.
data NoOpCallbackRegistration = NoOpCallbackRegistration

instance CallbackRegistration NoOpCallbackRegistration where
  unregister _ = pure ()


-- | Existential wrapper for any 'CallbackRegistration'.
data SomeCallbackRegistration = forall r. CallbackRegistration r => SomeCallbackRegistration r

instance CallbackRegistration SomeCallbackRegistration where
  unregister (SomeCallbackRegistration r) = unregister r


-------------------------------------------------------------------------------
-- Callback type aliases
-------------------------------------------------------------------------------

-- | Callback invoked by the SDK to collect observations from a single
-- observable instrument.
type ObservableCallback = SomeObservableResult -> IO ()

-- | Callback invoked by the SDK to collect observations from multiple
-- observable instruments in a single call.
type BatchObservableCallback = SomeBatchObservableResult -> IO ()


-------------------------------------------------------------------------------
-- SomeObservableInstrument
-------------------------------------------------------------------------------

-- | A tagged sum of all observable instrument types. Used by
-- 'registerCallback' to associate a batch callback with specific instruments.
data SomeObservableInstrument = SomeObservableInstrument
  { soiName :: !Text
  , soiKind :: SomeObservableInstrKind
  }

data SomeObservableInstrKind
  = ObsCounter       SomeObservableCounter
  | ObsUpDownCounter SomeObservableUpDownCounter
  | ObsGauge         SomeObservableGauge

mkSomeObsCounter :: Text -> SomeObservableCounter -> SomeObservableInstrument
mkSomeObsCounter n c = SomeObservableInstrument n (ObsCounter c)

mkSomeObsUpDownCounter :: Text -> SomeObservableUpDownCounter -> SomeObservableInstrument
mkSomeObsUpDownCounter n c = SomeObservableInstrument n (ObsUpDownCounter c)

mkSomeObsGauge :: Text -> SomeObservableGauge -> SomeObservableInstrument
mkSomeObsGauge n c = SomeObservableInstrument n (ObsGauge c)


-------------------------------------------------------------------------------
-- Synchronous instruments
-------------------------------------------------------------------------------

-- | A monotonically increasing counter. Values must be non-negative.
class Counter c where
  counterAdd :: c -> Double -> Attributes -> IO ()

-- | No-op counter that discards all recordings.
data NoOpCounter = NoOpCounter
instance Counter NoOpCounter where
  counterAdd _ _ _ = pure ()

-- | Existential wrapper for any 'Counter'.
data SomeCounter = forall c. Counter c => SomeCounter c
instance Counter SomeCounter where
  counterAdd (SomeCounter c) = counterAdd c


-- | A counter that can go up or down. Useful for tracking values that
-- increase and decrease, such as queue depth.
class UpDownCounter u where
  upDownCounterAdd :: u -> Double -> Attributes -> IO ()

-- | No-op up-down counter that discards all recordings.
data NoOpUpDownCounter = NoOpUpDownCounter
instance UpDownCounter NoOpUpDownCounter where
  upDownCounterAdd _ _ _ = pure ()

-- | Existential wrapper for any 'UpDownCounter'.
data SomeUpDownCounter = forall u. UpDownCounter u => SomeUpDownCounter u
instance UpDownCounter SomeUpDownCounter where
  upDownCounterAdd (SomeUpDownCounter u) = upDownCounterAdd u


-- | Records a distribution of values, typically used for measuring
-- request durations or response sizes.
class Histogram h where
  histogramRecord :: h -> Double -> Attributes -> IO ()

-- | No-op histogram that discards all recordings.
data NoOpHistogram = NoOpHistogram
instance Histogram NoOpHistogram where
  histogramRecord _ _ _ = pure ()

-- | Existential wrapper for any 'Histogram'.
data SomeHistogram = forall h. Histogram h => SomeHistogram h
instance Histogram SomeHistogram where
  histogramRecord (SomeHistogram h) = histogramRecord h


-- | A synchronous gauge that reports instantaneous values.
class Gauge g where
  gaugeSet :: g -> Double -> Attributes -> IO ()

-- | No-op gauge that discards all recordings.
data NoOpGauge = NoOpGauge
instance Gauge NoOpGauge where
  gaugeSet _ _ _ = pure ()

-- | Existential wrapper for any 'Gauge'.
data SomeGauge = forall g. Gauge g => SomeGauge g
instance Gauge SomeGauge where
  gaugeSet (SomeGauge g) = gaugeSet g


-------------------------------------------------------------------------------
-- Asynchronous (observable) instruments
-------------------------------------------------------------------------------

-- | An observable counter that reports monotonically increasing values
-- via callbacks invoked by the SDK during collection.
class ObservableCounter c where
  addObservableCounterCallback :: c -> ObservableCallback -> IO ()

-- | No-op observable counter.
data NoOpObservableCounter = NoOpObservableCounter
instance ObservableCounter NoOpObservableCounter where
  addObservableCounterCallback _ _ = pure ()

-- | Existential wrapper for any 'ObservableCounter'.
data SomeObservableCounter = forall c. ObservableCounter c => SomeObservableCounter c
instance ObservableCounter SomeObservableCounter where
  addObservableCounterCallback (SomeObservableCounter c) = addObservableCounterCallback c


-- | An observable counter that can go up or down.
class ObservableUpDownCounter u where
  addObservableUpDownCounterCallback :: u -> ObservableCallback -> IO ()

-- | No-op observable up-down counter.
data NoOpObservableUpDownCounter = NoOpObservableUpDownCounter
instance ObservableUpDownCounter NoOpObservableUpDownCounter where
  addObservableUpDownCounterCallback _ _ = pure ()

-- | Existential wrapper for any 'ObservableUpDownCounter'.
data SomeObservableUpDownCounter = forall u. ObservableUpDownCounter u => SomeObservableUpDownCounter u
instance ObservableUpDownCounter SomeObservableUpDownCounter where
  addObservableUpDownCounterCallback (SomeObservableUpDownCounter u) = addObservableUpDownCounterCallback u


-- | An observable gauge that reports instantaneous values via callbacks.
class ObservableGauge g where
  addObservableGaugeCallback :: g -> ObservableCallback -> IO ()

-- | No-op observable gauge.
data NoOpObservableGauge = NoOpObservableGauge
instance ObservableGauge NoOpObservableGauge where
  addObservableGaugeCallback _ _ = pure ()

-- | Existential wrapper for any 'ObservableGauge'.
data SomeObservableGauge = forall g. ObservableGauge g => SomeObservableGauge g
instance ObservableGauge SomeObservableGauge where
  addObservableGaugeCallback (SomeObservableGauge g) = addObservableGaugeCallback g
