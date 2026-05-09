-- | (Experimental) Profiles API: global ProfilerProvider registration.
--
-- The Profiles signal is under active development and subject to breaking
-- changes. Depend on this module only if you are experimenting with the
-- OTel Profiles specification.
module OTel.Profile
  ( -- * ProfilerProvider
    ProfilerProvider
  , NoOpProfilerProvider (..)
  , SomeProfilerProvider (..)
    -- * Global registration
  , setGlobalProfilerProvider
  , getGlobalProfilerProvider
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)


-- | (Experimental) A provider for profiling interfaces.
-- No stable methods are defined at this time.
class ProfilerProvider p


-- | No-op profiler provider that produces no profiling data.
data NoOpProfilerProvider = NoOpProfilerProvider

instance ProfilerProvider NoOpProfilerProvider


-- | Existential wrapper for any 'ProfilerProvider'.
data SomeProfilerProvider = forall p. ProfilerProvider p => SomeProfilerProvider p

instance ProfilerProvider SomeProfilerProvider


globalProfilerProviderRef :: IORef SomeProfilerProvider
globalProfilerProviderRef =
  unsafePerformIO (newIORef (SomeProfilerProvider NoOpProfilerProvider))
{-# NOINLINE globalProfilerProviderRef #-}


-- | Set the global profiler provider.
setGlobalProfilerProvider :: SomeProfilerProvider -> IO ()
setGlobalProfilerProvider = writeIORef globalProfilerProviderRef


-- | Get the global profiler provider.
getGlobalProfilerProvider :: IO SomeProfilerProvider
getGlobalProfilerProvider = readIORef globalProfilerProviderRef
