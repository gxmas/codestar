module Telemetry.Core.Internal.Global
  ( globalBackend
  , setGlobalBackend
  , getGlobalBackend
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import Telemetry.Core.Internal.Backend (Backend)
import Telemetry.Core.Internal.NoOp (noOpBackend)

{-# NOINLINE globalBackend #-}
globalBackend :: IORef Backend
globalBackend = unsafePerformIO (newIORef noOpBackend)

setGlobalBackend :: Backend -> IO ()
setGlobalBackend = writeIORef globalBackend

getGlobalBackend :: IO Backend
getGlobalBackend = readIORef globalBackend
