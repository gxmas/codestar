-- | (Experimental) OpenTelemetry Profiles SDK.
--
-- The Profiles signal is under active development. This SDK implementation
-- follows the minimal experimental spec: a provider with lifecycle management
-- and an exporter interface, but no stable profile data model.
module OTel.SDK.Profile
  ( -- * ProfileExporter
    ProfileExporter (..)
  , SomeProfileExporter (..)
  , NoOpProfileExporter (..)

    -- * SdkProfilerProvider
  , SdkProfilerProvider
  , SdkProfilerProviderConfig (..)
  , defaultSdkProfilerProviderConfig
  , newSdkProfilerProvider
  , sdkProfilerProviderShutdown
  , sdkProfilerProviderForceFlush
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, writeTVar)
import OTel.Profile (ProfilerProvider)
import OTel.SDK.Export (ExportResult (..), FlushError, ShutdownError)
import OTel.Timestamp (Duration)


-- | (Experimental) Interface for exporting profile data.
-- The profile data type is () because no stable profile data model exists yet.
class ProfileExporter e where
  exportProfiles :: e -> IO ExportResult
  shutdownProfileExporter :: e -> IO (Either ShutdownError ())
  forceFlushProfileExporter :: e -> Maybe Duration -> IO (Either FlushError ())


-- | Existential wrapper for any 'ProfileExporter'.
data SomeProfileExporter = forall e. ProfileExporter e => SomeProfileExporter e

instance Show SomeProfileExporter where
  show _ = "SomeProfileExporter"

instance ProfileExporter SomeProfileExporter where
  exportProfiles (SomeProfileExporter e) = exportProfiles e
  shutdownProfileExporter (SomeProfileExporter e) = shutdownProfileExporter e
  forceFlushProfileExporter (SomeProfileExporter e) = forceFlushProfileExporter e


-- | No-op profile exporter that always succeeds.
data NoOpProfileExporter = NoOpProfileExporter
  deriving stock (Show)

instance ProfileExporter NoOpProfileExporter where
  exportProfiles _ = pure ExportSuccess
  shutdownProfileExporter _ = pure (Right ())
  forceFlushProfileExporter _ _ = pure (Right ())


-- | Configuration for 'SdkProfilerProvider'.
data SdkProfilerProviderConfig = SdkProfilerProviderConfig
  { sppExporters :: ![SomeProfileExporter]
  }
  deriving stock (Show)

-- | Default profiler provider config with no exporters.
defaultSdkProfilerProviderConfig :: SdkProfilerProviderConfig
defaultSdkProfilerProviderConfig = SdkProfilerProviderConfig
  { sppExporters = []
  }


-- | SDK implementation of 'ProfilerProvider' (experimental).
data SdkProfilerProvider = SdkProfilerProvider
  { _sppExporters :: ![SomeProfileExporter]
  , _sppShutdown :: !(TVar Bool)
  }

instance ProfilerProvider SdkProfilerProvider

-- | Create a new SDK profiler provider.
newSdkProfilerProvider :: SdkProfilerProviderConfig -> IO SdkProfilerProvider
newSdkProfilerProvider cfg = do
  flag <- newTVarIO False
  pure SdkProfilerProvider
    { _sppExporters = sppExporters cfg
    , _sppShutdown = flag
    }

-- | Shut down the profiler provider and all registered exporters.
sdkProfilerProviderShutdown :: SdkProfilerProvider -> IO (Either ShutdownError ())
sdkProfilerProviderShutdown p = do
  atomically $ writeTVar (_sppShutdown p) True
  results <- mapM shutdownProfileExporter (_sppExporters p)
  pure $ case [e | Left e <- results] of
    [] -> Right ()
    (e : _) -> Left e

-- | Force flush all profiler exporters.
sdkProfilerProviderForceFlush :: SdkProfilerProvider -> Maybe Duration -> IO (Either FlushError ())
sdkProfilerProviderForceFlush p timeout_ = do
  results <- mapM (\e -> forceFlushProfileExporter e timeout_) (_sppExporters p)
  pure $ case [e | Left e <- results] of
    [] -> Right ()
    (e : _) -> Left e
