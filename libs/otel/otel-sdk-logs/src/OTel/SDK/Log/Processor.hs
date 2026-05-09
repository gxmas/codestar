{-# LANGUAGE ExistentialQuantification #-}
-- | LogRecordProcessor type class, existential wrapper, and built-in
-- processors (simple and batching).
module OTel.SDK.Log.Processor
  ( -- * LogRecordProcessor type class
    LogRecordProcessor (..)
  , SomeLogRecordProcessor (..)

    -- * SimpleLogRecordProcessor
  , SimpleLogRecordProcessor
  , newSimpleLogRecordProcessor

    -- * BatchLogRecordProcessor
  , BatchLogRecordProcessorConfig (..)
  , defaultBatchLogRecordProcessorConfig
  , BatchLogRecordProcessor
  , newBatchLogRecordProcessor
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
import System.Timeout (timeout)

import OTel.Context (Context)
import OTel.SDK.Export (FlushError (..), ShutdownError)
import OTel.SDK.Log.Export
  ( LogRecordExporter (..)
  , SomeLogRecordExporter (..)
  , SomeReadWriteLogRecord (..)
  , SomeReadableLogRecord (..)
  )
import OTel.Timestamp (Duration (..), milliseconds)


-------------------------------------------------------------------------------
-- LogRecordProcessor
-------------------------------------------------------------------------------

-- | A processor receives log record lifecycle events. Typical implementations
-- include the simple processor (exports immediately) and the batching
-- processor (buffers log records and exports in bulk).
class LogRecordProcessor p where
  -- | Called when a log record is emitted. Receives a mutable read-write
  -- log record and the associated context.
  onEmit :: p -> SomeReadWriteLogRecord -> Context -> IO ()

  -- | Shut down the processor, releasing any held resources.
  shutdownProcessor :: p -> IO (Either ShutdownError ())

  -- | Force-flush any buffered log records. The optional 'Duration' is a timeout.
  forceFlushProcessor :: p -> Maybe Duration -> IO (Either FlushError ())


-- | Existential wrapper for any 'LogRecordProcessor' implementation.
data SomeLogRecordProcessor = forall p. LogRecordProcessor p => SomeLogRecordProcessor p

instance LogRecordProcessor SomeLogRecordProcessor where
  onEmit (SomeLogRecordProcessor p) = onEmit p
  shutdownProcessor (SomeLogRecordProcessor p) = shutdownProcessor p
  forceFlushProcessor (SomeLogRecordProcessor p) = forceFlushProcessor p


-------------------------------------------------------------------------------
-- SimpleLogRecordProcessor
-------------------------------------------------------------------------------

-- | A processor that exports each log record synchronously when it is emitted.
-- Blocks the calling thread during export. Intended for debugging and testing
-- only; use a batching processor for production workloads.
data SimpleLogRecordProcessor = SimpleLogRecordProcessor
  { slrpExporter :: !SomeLogRecordExporter
  , slrpShutdown :: !(TVar Bool)
  }


-- | Create a 'SimpleLogRecordProcessor' that delegates to the given exporter.
newSimpleLogRecordProcessor :: SomeLogRecordExporter -> IO SimpleLogRecordProcessor
newSimpleLogRecordProcessor exporter = do
  shutdownVar <- newTVarIO False
  pure (SimpleLogRecordProcessor exporter shutdownVar)


instance LogRecordProcessor SimpleLogRecordProcessor where
  onEmit proc rwRecord _ctx = do
    isShutdown <- readTVarIO proc.slrpShutdown
    unless isShutdown $ do
      -- SomeReadWriteLogRecord has a ReadableLogRecord instance, so we can
      -- wrap it in SomeReadableLogRecord for the exporter.
      void $ exportLogRecords proc.slrpExporter [SomeReadableLogRecord rwRecord]

  shutdownProcessor proc = do
    alreadyShutdown <- atomically $ swapTVar proc.slrpShutdown True
    if alreadyShutdown
      then pure (Right ())
      else shutdownLogExporter proc.slrpExporter

  forceFlushProcessor proc mtimeout = do
    isShutdown <- readTVarIO proc.slrpShutdown
    if isShutdown
      then pure (Right ())
      else forceFlushLogExporter proc.slrpExporter mtimeout


-------------------------------------------------------------------------------
-- BatchLogRecordProcessor
-------------------------------------------------------------------------------

-- | Configuration for 'BatchLogRecordProcessor'. Use
-- 'defaultBatchLogRecordProcessorConfig' and override fields with record
-- update syntax.
data BatchLogRecordProcessorConfig = BatchLogRecordProcessorConfig
  { blrpMaxQueueSize       :: !Int       -- ^ Max buffered log records. Default: 2048.
  , blrpScheduledDelay     :: !Duration  -- ^ How often to export. Default: 1000ms.
  , blrpExportTimeout      :: !Duration  -- ^ Per-export timeout. Default: 30000ms.
  , blrpMaxExportBatchSize :: !Int       -- ^ Max log records per export call. Default: 512.
  } deriving stock (Eq, Show)


-- | Spec-mandated defaults for the log batch processor.
-- Note: scheduled delay is 1000ms (not 5000ms as in the trace batch processor).
defaultBatchLogRecordProcessorConfig :: BatchLogRecordProcessorConfig
defaultBatchLogRecordProcessorConfig = BatchLogRecordProcessorConfig
  { blrpMaxQueueSize       = 2048
  , blrpScheduledDelay     = milliseconds 1000
  , blrpExportTimeout      = milliseconds 30000
  , blrpMaxExportBatchSize = 512
  }


-- | A processor that buffers log records in a bounded queue and exports them
-- in batches on a background thread. Queue-full log records are dropped
-- without blocking the caller. Use 'newBatchLogRecordProcessor' to construct.
data BatchLogRecordProcessor = BatchLogRecordProcessor
  { blrpQueue        :: !(TBQueue SomeReadableLogRecord)
  , blrpShutdownVar  :: !(TVar Bool)
  , blrpFlushRequest :: !(TMVar (MVar (Either FlushError ())))
  , blrpWorkerAsync  :: !(Async ())
  , blrpExporter_    :: !SomeLogRecordExporter
  , blrpConfig_      :: !BatchLogRecordProcessorConfig
  }


-- | Create a 'BatchLogRecordProcessor' with the given exporter and
-- configuration. Starts the background export worker immediately.
newBatchLogRecordProcessor
  :: SomeLogRecordExporter
  -> BatchLogRecordProcessorConfig
  -> IO BatchLogRecordProcessor
newBatchLogRecordProcessor exporter config = do
  queue       <- newTBQueueIO (fromIntegral (blrpMaxQueueSize config))
  shutdownVar <- newTVarIO False
  flushReq    <- newEmptyTMVarIO
  worker      <- async (batchWorker exporter config queue shutdownVar flushReq)
  pure BatchLogRecordProcessor
    { blrpQueue        = queue
    , blrpShutdownVar  = shutdownVar
    , blrpFlushRequest = flushReq
    , blrpWorkerAsync  = worker
    , blrpExporter_    = exporter
    , blrpConfig_      = config
    }


instance LogRecordProcessor BatchLogRecordProcessor where
  onEmit proc rwRecord _ctx = do
    isDown <- readTVarIO proc.blrpShutdownVar
    unless isDown $
      atomically $ do
        full <- isFullTBQueue proc.blrpQueue
        unless full $ writeTBQueue proc.blrpQueue (SomeReadableLogRecord rwRecord)

  shutdownProcessor proc = do
    alreadyDown <- atomically $ swapTVar proc.blrpShutdownVar True
    if alreadyDown
      then pure (Right ())
      else do
        -- Wake the worker if it is blocking on the delay timer
        wakeVar <- newEmptyMVar
        atomically $ void $ tryPutTMVar proc.blrpFlushRequest wakeVar
        -- Wait for the worker to finish draining and exit
        wait proc.blrpWorkerAsync
        pure (Right ())

  forceFlushProcessor proc mtimeout = do
    isDown <- readTVarIO proc.blrpShutdownVar
    if isDown
      then pure (Right ())
      else do
        resultVar <- newEmptyMVar
        atomically $ putTMVar proc.blrpFlushRequest resultVar
        case mtimeout of
          Nothing -> takeMVar resultVar
          Just d  -> do
            let micros = fromIntegral (durationToMicros d)
            mResult <- timeout micros (takeMVar resultVar)
            case mResult of
              Just r  -> pure r
              Nothing -> pure (Left FlushError
                { flushComponent = "BatchLogRecordProcessor"
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


-- | The background export loop. Exits when 'blrpShutdownVar' is 'True' and
-- the queue is drained.
batchWorker
  :: SomeLogRecordExporter
  -> BatchLogRecordProcessorConfig
  -> TBQueue SomeReadableLogRecord
  -> TVar Bool
  -> TMVar (MVar (Either FlushError ()))
  -> IO ()
batchWorker exporter config queue shutdownVar flushReq = loop
  where
    delayMicros :: Int
    delayMicros = fromIntegral (durationToMicros (blrpScheduledDelay config))

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
      batch <- atomically $ drainN (blrpMaxExportBatchSize config) queue
      unless (null batch) $ do
        let micros = fromIntegral (durationToMicros (blrpExportTimeout config))
        void $ timeout micros (exportLogRecords exporter batch)

    drainAndExit :: IO ()
    drainAndExit = do
      remaining <- atomically $ drainAll queue
      unless (null remaining) $ do
        let micros = fromIntegral (durationToMicros (blrpExportTimeout config))
        void $ timeout micros (exportLogRecords exporter remaining)


-- | Convert a 'Duration' (nanoseconds) to microseconds for 'registerDelay'.
durationToMicros :: Duration -> Word64
durationToMicros (Duration nanos) = nanos `div` 1000


-- | Drain up to @n@ items from a 'TBQueue' atomically.
drainN :: Int -> TBQueue a -> STM [a]
drainN 0 _ = pure []
drainN n q = do
  mx <- tryReadTBQueue q
  case mx of
    Nothing -> pure []
    Just x  -> (x :) <$> drainN (n - 1) q


-- | Drain all remaining items from a 'TBQueue' atomically.
drainAll :: TBQueue a -> STM [a]
drainAll q = do
  mx <- tryReadTBQueue q
  case mx of
    Nothing -> pure []
    Just x  -> (x :) <$> drainAll q
