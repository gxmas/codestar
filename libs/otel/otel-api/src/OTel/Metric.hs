-- | Core metrics API: MeterProvider, Meter type classes with existential
-- wrappers, no-op implementations, global registration, and re-exports
-- of all instrument types.
module OTel.Metric
  ( -- * MeterProvider
    MeterProvider (..)
  , SomeMeterProvider (..)
  , NoOpMeterProvider (..)

    -- * Meter
  , Meter (..)
  , SomeMeter (..)
  , NoOpMeter (..)

    -- * Global registration
  , setGlobalMeterProvider
  , getGlobalMeterProvider

    -- * Re-exports
  , module OTel.Metric.Instrument
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import OTel.Attribute (InstrumentationScope)
import OTel.Metric.Instrument


-------------------------------------------------------------------------------
-- MeterProvider type class
-------------------------------------------------------------------------------

-- | A factory for 'Meter' instances, scoped by instrumentation library.
class MeterProvider p where
  -- | Obtain a meter for the given instrumentation scope.
  getMeter :: p -> InstrumentationScope -> IO SomeMeter


-- | Existential wrapper for any 'MeterProvider' implementation.
data SomeMeterProvider = forall p. MeterProvider p => SomeMeterProvider p

instance MeterProvider SomeMeterProvider where
  getMeter (SomeMeterProvider p) = getMeter p


-- | A provider that always returns 'NoOpMeter'.
data NoOpMeterProvider = NoOpMeterProvider

instance MeterProvider NoOpMeterProvider where
  getMeter _ _ = pure (SomeMeter NoOpMeter)


-------------------------------------------------------------------------------
-- Meter type class
-------------------------------------------------------------------------------

-- | A named meter that creates instruments. Obtained from a 'MeterProvider'.
class Meter m where
  -- | Create a synchronous counter.
  createCounter :: m -> Text -> Maybe InstrumentOptions -> IO SomeCounter

  -- | Create a synchronous up-down counter.
  createUpDownCounter :: m -> Text -> Maybe InstrumentOptions -> IO SomeUpDownCounter

  -- | Create a synchronous histogram.
  createHistogram :: m -> Text -> Maybe InstrumentOptions -> IO SomeHistogram

  -- | Create a synchronous gauge.
  createGauge :: m -> Text -> Maybe InstrumentOptions -> IO SomeGauge

  -- | Create an observable counter with initial callbacks.
  createObservableCounter :: m -> Text -> [ObservableCallback] -> Maybe InstrumentOptions -> IO SomeObservableCounter

  -- | Create an observable up-down counter with initial callbacks.
  createObservableUpDownCounter :: m -> Text -> [ObservableCallback] -> Maybe InstrumentOptions -> IO SomeObservableUpDownCounter

  -- | Create an observable gauge with initial callbacks.
  createObservableGauge :: m -> Text -> [ObservableCallback] -> Maybe InstrumentOptions -> IO SomeObservableGauge

  -- | Register a batch callback for multiple observable instruments.
  registerCallback :: m -> [SomeObservableInstrument] -> BatchObservableCallback -> IO SomeCallbackRegistration


-- | Existential wrapper for any 'Meter' implementation.
data SomeMeter = forall m. Meter m => SomeMeter m

instance Meter SomeMeter where
  createCounter                 (SomeMeter m) = createCounter m
  createUpDownCounter           (SomeMeter m) = createUpDownCounter m
  createHistogram               (SomeMeter m) = createHistogram m
  createGauge                   (SomeMeter m) = createGauge m
  createObservableCounter       (SomeMeter m) = createObservableCounter m
  createObservableUpDownCounter (SomeMeter m) = createObservableUpDownCounter m
  createObservableGauge         (SomeMeter m) = createObservableGauge m
  registerCallback              (SomeMeter m) = registerCallback m


-- | A meter that always produces no-op instruments.
data NoOpMeter = NoOpMeter

instance Meter NoOpMeter where
  createCounter                 _ _ _ = pure (SomeCounter NoOpCounter)
  createUpDownCounter           _ _ _ = pure (SomeUpDownCounter NoOpUpDownCounter)
  createHistogram               _ _ _ = pure (SomeHistogram NoOpHistogram)
  createGauge                   _ _ _ = pure (SomeGauge NoOpGauge)
  createObservableCounter       _ _ _ _ = pure (SomeObservableCounter NoOpObservableCounter)
  createObservableUpDownCounter _ _ _ _ = pure (SomeObservableUpDownCounter NoOpObservableUpDownCounter)
  createObservableGauge         _ _ _ _ = pure (SomeObservableGauge NoOpObservableGauge)
  registerCallback              _ _ _ = pure (SomeCallbackRegistration NoOpCallbackRegistration)


-------------------------------------------------------------------------------
-- Global registration
-------------------------------------------------------------------------------

globalMeterProviderRef :: IORef SomeMeterProvider
globalMeterProviderRef = unsafePerformIO (newIORef (SomeMeterProvider NoOpMeterProvider))
{-# NOINLINE globalMeterProviderRef #-}


-- | Set the global 'MeterProvider'. Typically called once at application
-- startup after configuring the SDK.
setGlobalMeterProvider :: SomeMeterProvider -> IO ()
setGlobalMeterProvider = writeIORef globalMeterProviderRef


-- | Retrieve the current global 'MeterProvider'. Returns a no-op provider
-- if none has been set.
getGlobalMeterProvider :: IO SomeMeterProvider
getGlobalMeterProvider = readIORef globalMeterProviderRef
