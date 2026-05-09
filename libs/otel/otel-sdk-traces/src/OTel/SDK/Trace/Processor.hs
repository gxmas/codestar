-- | SpanProcessor type class, existential wrapper, and built-in processors.
module OTel.SDK.Trace.Processor
  ( -- * SpanProcessor
    SpanProcessor (..)
  , SomeSpanProcessor (..)

    -- * Simple span processor
  , SimpleSpanProcessor
  , newSimpleSpanProcessor

    -- * Batch span processor
  , BatchSpanProcessorConfig (..)
  , defaultBatchSpanProcessorConfig
  , BatchSpanProcessor
  , newBatchSpanProcessor
  ) where

import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
  ( TBQueue, TMVar, TVar, STM
  , atomically, isFullTBQueue, newEmptyTMVarIO, newTBQueueIO, newTVarIO
  , orElse, putTMVar, readTVar, readTVarIO, registerDelay, retry, swapTVar
  , takeTMVar, tryPutTMVar, tryReadTBQueue, writeTBQueue
  )
import Control.Monad (unless, void)
import Data.Word (Word64)
import OTel.Context (Context)
import OTel.SDK.Export (FlushError(..), ShutdownError)
import System.Timeout (timeout)
import OTel.SDK.Trace.Export (SomeReadWriteSpan, SomeReadableSpan, SomeSpanExporter(..), SpanExporter(..))
import OTel.Timestamp (Duration(..), milliseconds)


-------------------------------------------------------------------------------
-- SpanProcessor
-------------------------------------------------------------------------------

-- | A processor receives span lifecycle events. Typical implementations
-- include the simple processor (exports on end) and the batching processor
-- (buffers spans and exports in bulk).
class SpanProcessor p where
  -- | Called when a span is started. Receives a mutable read-write span
  -- and the parent context.
  onStart :: p -> SomeReadWriteSpan -> Context -> IO ()

  -- | Called when a span is ended. Receives a read-only snapshot.
  onEnd :: p -> SomeReadableSpan -> IO ()

  -- | Shut down the processor, releasing any held resources.
  shutdownProcessor :: p -> IO (Either ShutdownError ())

  -- | Force-flush any buffered spans. The optional 'Duration' is a timeout.
  forceFlushProcessor :: p -> Maybe Duration -> IO (Either FlushError ())


-- | Existential wrapper for any 'SpanProcessor' implementation.
data SomeSpanProcessor = forall p. SpanProcessor p => SomeSpanProcessor p

instance SpanProcessor SomeSpanProcessor where
  onStart (SomeSpanProcessor p) = onStart p
  onEnd (SomeSpanProcessor p) = onEnd p
  shutdownProcessor (SomeSpanProcessor p) = shutdownProcessor p
  forceFlushProcessor (SomeSpanProcessor p) = forceFlushProcessor p


-------------------------------------------------------------------------------
-- SimpleSpanProcessor
-------------------------------------------------------------------------------

-- | A processor that exports each span synchronously when it ends.
-- Blocks the calling thread during export. Intended for debugging and
-- testing only; use a batching processor for production workloads.
data SimpleSpanProcessor = SimpleSpanProcessor
  { simpleExporter :: !SomeSpanExporter
  , simpleShutdown :: !(TVar Bool)
  }


-- | Create a 'SimpleSpanProcessor' that delegates to the given exporter.
newSimpleSpanProcessor :: SomeSpanExporter -> IO SimpleSpanProcessor
newSimpleSpanProcessor exporter = do
  shutdown <- newTVarIO False
  pure (SimpleSpanProcessor exporter shutdown)


instance SpanProcessor SimpleSpanProcessor where
  onStart _ _ _ = pure ()

  onEnd proc span_ = do
    isShutdown <- readTVarIO proc.simpleShutdown
    unless isShutdown $ do
      _ <- exportSpans proc.simpleExporter [span_]
      pure ()

  shutdownProcessor proc = do
    alreadyShutdown <- atomically $ swapTVar proc.simpleShutdown True
    if alreadyShutdown
      then pure (Right ())
      else shutdownExporter proc.simpleExporter

  forceFlushProcessor proc mtimeout = do
    isShutdown <- readTVarIO proc.simpleShutdown
    if isShutdown
      then pure (Right ())
      else forceFlushExporter proc.simpleExporter mtimeout


-------------------------------------------------------------------------------
-- BatchSpanProcessor
-------------------------------------------------------------------------------

-- | Configuration for 'BatchSpanProcessor'. Use 'defaultBatchSpanProcessorConfig'
-- and override fields with record update syntax.
data BatchSpanProcessorConfig = BatchSpanProcessorConfig
  { bspScheduledDelay     :: !Duration  -- ^ How often to export. Default: 5000ms.
  , bspExportTimeout      :: !Duration  -- ^ Per-export timeout. Default: 30000ms.
  , bspMaxQueueSize       :: !Int       -- ^ Max buffered spans. Default: 2048.
  , bspMaxExportBatchSize :: !Int       -- ^ Max spans per export call. Default: 512.
  } deriving stock (Eq, Show)


-- | Spec-mandated defaults.
defaultBatchSpanProcessorConfig :: BatchSpanProcessorConfig
defaultBatchSpanProcessorConfig = BatchSpanProcessorConfig
  { bspScheduledDelay     = milliseconds 5000
  , bspExportTimeout      = milliseconds 30000
  , bspMaxQueueSize       = 2048
  , bspMaxExportBatchSize = 512
  }


-- | A processor that buffers spans in a bounded queue and exports them in
-- batches on a background thread. Queue-full spans are dropped without
-- blocking the caller. Use 'newBatchSpanProcessor' to construct.
data BatchSpanProcessor = BatchSpanProcessor
  { bspQueue        :: !(TBQueue SomeReadableSpan)
  , bspShutdownVar  :: !(TVar Bool)
  , bspFlushRequest :: !(TMVar (MVar (Either FlushError ())))
  , bspWorkerAsync  :: !(Async ())
  , bspExporter_    :: !SomeSpanExporter
  , bspConfig_      :: !BatchSpanProcessorConfig
  }


-- | Create a 'BatchSpanProcessor' with the given exporter and configuration.
-- Starts the background export worker immediately.
newBatchSpanProcessor
  :: SomeSpanExporter
  -> BatchSpanProcessorConfig
  -> IO BatchSpanProcessor
newBatchSpanProcessor exporter config = do
  queue       <- newTBQueueIO (fromIntegral (bspMaxQueueSize config))
  shutdownVar <- newTVarIO False
  flushReq    <- newEmptyTMVarIO
  worker      <- async (batchWorker exporter config queue shutdownVar flushReq)
  pure BatchSpanProcessor
    { bspQueue        = queue
    , bspShutdownVar  = shutdownVar
    , bspFlushRequest = flushReq
    , bspWorkerAsync  = worker
    , bspExporter_    = exporter
    , bspConfig_      = config
    }


instance SpanProcessor BatchSpanProcessor where
  onStart _ _ _ = pure ()

  onEnd proc span_ = do
    isDown <- readTVarIO proc.bspShutdownVar
    unless isDown $
      atomically $ do
        full <- isFullTBQueue proc.bspQueue
        unless full $ writeTBQueue proc.bspQueue span_

  shutdownProcessor proc = do
    alreadyDown <- atomically $ swapTVar proc.bspShutdownVar True
    if alreadyDown
      then pure (Right ())
      else do
        -- Wake the worker if it is blocking on the delay timer
        wakeVar <- newEmptyMVar
        atomically $ void $ tryPutTMVar proc.bspFlushRequest wakeVar
        -- Wait for the worker to finish draining and exit
        wait proc.bspWorkerAsync
        pure (Right ())

  forceFlushProcessor proc mtimeout = do
    isDown <- readTVarIO proc.bspShutdownVar
    if isDown
      then pure (Right ())
      else do
        resultVar <- newEmptyMVar
        atomically $ putTMVar proc.bspFlushRequest resultVar
        case mtimeout of
          Nothing -> takeMVar resultVar
          Just d  -> do
            let micros = fromIntegral (durationToMicros d)
            mResult <- timeout micros (takeMVar resultVar)
            case mResult of
              Just r  -> pure r
              Nothing -> pure (Left FlushError
                { flushComponent = "BatchSpanProcessor"
                , flushTimedOut  = True
                , flushCause     = Nothing
                })


-------------------------------------------------------------------------------
-- Batch worker internals
-------------------------------------------------------------------------------

-- | What caused the worker to wake up.
data BatchTrigger
  = ScheduledExport
  | FlushRequested (MVar (Either FlushError ()))


-- | The background export loop. Exits when 'bspShutdownVar' is 'True' and the
-- queue is drained.
batchWorker
  :: SomeSpanExporter
  -> BatchSpanProcessorConfig
  -> TBQueue SomeReadableSpan
  -> TVar Bool
  -> TMVar (MVar (Either FlushError ()))
  -> IO ()
batchWorker exporter config queue shutdownVar flushReq = loop
  where
    delayMicros :: Int
    delayMicros = fromIntegral (durationToMicros (bspScheduledDelay config))

    loop :: IO ()
    loop = do
      isDown <- readTVarIO shutdownVar
      if isDown
        then drainAndExit
        else do
          delayDone <- registerDelay delayMicros
          trigger <- atomically $
            (FlushRequested <$> takeTMVar flushReq)
            `orElse`
            (do done <- readTVar delayDone
                if done then pure ScheduledExport else retry)
          exportBatch
          case trigger of
            ScheduledExport -> loop
            FlushRequested resultVar -> do
              putMVar resultVar (Right ())
              isDown2 <- readTVarIO shutdownVar
              if isDown2 then drainAndExit else loop

    exportBatch :: IO ()
    exportBatch = do
      batch <- atomically $ drainN (bspMaxExportBatchSize config) queue
      unless (null batch) $ do
        let micros = fromIntegral (durationToMicros (bspExportTimeout config))
        void $ timeout micros (exportSpans exporter batch)

    drainAndExit :: IO ()
    drainAndExit = do
      remaining <- atomically $ drainAll queue
      unless (null remaining) $ do
        let micros = fromIntegral (durationToMicros (bspExportTimeout config))
        void $ timeout micros (exportSpans exporter remaining)


-- | Convert a 'Duration' (nanoseconds) to microseconds for 'registerDelay'.
durationToMicros :: Duration -> Word64
durationToMicros (Duration nanos) = nanos `div` 1000


-- | Drain up to @n@ items from a 'TBQueue' atomically.
drainN :: Int -> TBQueue a -> STM [a]
drainN 0 _ = pure []
drainN n queue = do
  mx <- tryReadTBQueue queue
  case mx of
    Nothing -> pure []
    Just x  -> (x :) <$> drainN (n - 1) queue


-- | Drain all remaining items from a 'TBQueue' atomically.
drainAll :: TBQueue a -> STM [a]
drainAll queue = do
  mx <- tryReadTBQueue queue
  case mx of
    Nothing -> pure []
    Just x  -> (x :) <$> drainAll queue
