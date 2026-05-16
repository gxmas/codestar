{-# LANGUAGE ScopedTypeVariables #-}

-- | Stream processing with backpressure and progress tracking.
module Streaming.Core
  ( -- * Stream types
    StreamID
  , Stream
  , StreamState (..)
  , StreamChunk (..)
  , StreamConfig (..)
  , defaultStreamConfig

    -- * Write/read results
  , WriteResult (..)
  , ReadResult (..)

    -- * Progress
  , ProgressUpdate (..)

    -- * Lifecycle
  , createStream
  , closeStream
  , pauseStream
  , resumeStream
  , getStreamState
  , getStreamId

    -- * I/O
  , writeChunk
  , readChunk

    -- * Combinators
  , mergeStreams
  , splitStream

    -- * Progress reporting
  , reportProgress
  ) where

import Prelude hiding (log)

import Control.Concurrent.Async (async, waitAny)
import Control.Concurrent.STM
  ( TBQueue, TVar, STM
  , atomically, newTBQueueIO, newTVarIO
  , readTVar, writeTVar, readTBQueue, writeTBQueue
  , isFullTBQueue, tryReadTBQueue
  )
import Data.Text (Text)
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)

import OTel.Log
  ( getGlobalLoggerProvider
  , getLogger
  , defaultLogRecord
  , LogBody (..)
  , SeverityNumber (..)
  , LogRecord (..)
  , emit
  )
import OTel.Attribute (AttributeValue (..), Attribute, InstrumentationScope (..))
import OTel.Attribute qualified as OTelAttr
import OTel.Context (getCurrent)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

newtype StreamID = StreamID UUID
  deriving stock (Eq, Ord, Show, Generic)

data StreamState
  = StreamActive
  | StreamPaused
  | StreamCompleted
  | StreamFailed
  deriving stock (Eq, Show, Bounded, Enum, Generic)

data StreamChunk a = StreamChunk
  { chunkData     :: !a
  , chunkSequence :: !Int
  , chunkFinal    :: !Bool
  }
  deriving stock (Eq, Show, Generic)

data StreamConfig = StreamConfig
  { bufferSize          :: !Int
  , backpressureEnabled :: !Bool
  }
  deriving stock (Eq, Show, Generic)

defaultStreamConfig :: StreamConfig
defaultStreamConfig = StreamConfig
  { bufferSize          = 64
  , backpressureEnabled = True
  }

data WriteResult
  = WriteOk
  | WriteBackpressure
  | WriteClosed
  deriving stock (Eq, Show)

data ReadResult a
  = ReadOk (StreamChunk a)
  | ReadEndOfStream
  | ReadPaused
  deriving stock (Eq, Show)

data ProgressUpdate = ProgressUpdate
  { progressStream    :: !StreamID
  , progressCompleted :: !Int
  , progressTotal     :: !(Maybe Int)
  , progressMessage   :: !Text
  }
  deriving stock (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Stream (opaque, mutable)
-- ---------------------------------------------------------------------------

data Stream a = Stream
  { streamId       :: !StreamID
  , streamQueue    :: !(TBQueue (StreamChunk a))
  , streamState    :: !(TVar StreamState)
  , streamProgress :: !(TVar (Maybe ProgressUpdate))
  , streamConfig   :: !StreamConfig
  }

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

createStream :: StreamConfig -> IO (Stream a)
createStream cfg = do
  sid <- StreamID <$> nextRandom
  q <- newTBQueueIO (fromIntegral (bufferSize cfg))
  st <- newTVarIO StreamActive
  prog <- newTVarIO Nothing
  pure Stream
    { streamId       = sid
    , streamQueue    = q
    , streamState    = st
    , streamProgress = prog
    , streamConfig   = cfg
    }

closeStream :: Stream a -> IO ()
closeStream s = atomically (writeTVar (streamState s) StreamCompleted)

pauseStream :: Stream a -> IO ()
pauseStream s = atomically $ do
  st <- readTVar (streamState s)
  case st of
    StreamActive -> writeTVar (streamState s) StreamPaused
    _            -> pure ()

resumeStream :: Stream a -> IO ()
resumeStream s = atomically $ do
  st <- readTVar (streamState s)
  case st of
    StreamPaused -> writeTVar (streamState s) StreamActive
    _            -> pure ()

getStreamState :: Stream a -> IO StreamState
getStreamState s = atomically (readTVar (streamState s))

getStreamId :: Stream a -> StreamID
getStreamId = streamId

-- ---------------------------------------------------------------------------
-- I/O
-- ---------------------------------------------------------------------------

writeChunk :: Stream a -> StreamChunk a -> IO WriteResult
writeChunk s chunk = do
  result <- atomically $ do
    st <- readTVar (streamState s)
    case st of
      StreamActive -> do
        full <- isFullTBQueue (streamQueue s)
        if full && backpressureEnabled (streamConfig s)
          then pure WriteBackpressure
          else do
            writeTBQueue (streamQueue s) chunk
            pure WriteOk
      StreamPaused -> pure WriteBackpressure
      _            -> pure WriteClosed
  case result of
    WriteBackpressure ->
      logMsg SeverityWarn "Backpressure triggered"
        [("stream.id", StringValue (streamIdText s))]
    _ -> pure ()
  pure result

readChunk :: Stream a -> IO (ReadResult a)
readChunk s = atomically $ readChunkSTM s

readChunkSTM :: Stream a -> STM (ReadResult a)
readChunkSTM s = do
  mChunk <- tryReadTBQueue (streamQueue s)
  case mChunk of
    Just chunk -> pure (ReadOk chunk)
    Nothing -> do
      st <- readTVar (streamState s)
      case st of
        StreamCompleted -> pure ReadEndOfStream
        StreamFailed    -> pure ReadEndOfStream
        StreamPaused    -> pure ReadPaused
        StreamActive    -> do
          chunk <- readTBQueue (streamQueue s)
          pure (ReadOk chunk)

-- ---------------------------------------------------------------------------
-- Combinators
-- ---------------------------------------------------------------------------

mergeStreams :: [Stream a] -> IO (Stream a)
mergeStreams inputs = do
  out <- createStream defaultStreamConfig
  readers <- mapM (\inp -> async (drainInto inp out)) inputs
  _ <- async $ do
    waitAll readers
    closeStream out
  pure out
  where
    drainInto inp out = go
      where
        go = do
          result <- readChunk inp
          case result of
            ReadOk chunk -> do
              _ <- writeChunk out chunk
              go
            ReadEndOfStream -> pure ()
            ReadPaused -> go

    waitAll [] = pure ()
    waitAll (r:rs) = do
      _ <- waitAny [r]
      waitAll rs

splitStream :: Stream a -> (a -> Bool) -> IO (Stream a, Stream a)
splitStream inp predicate = do
  trueStream <- createStream (streamConfig inp)
  falseStream <- createStream (streamConfig inp)
  _ <- async $ do
    let go = do
          result <- readChunk inp
          case result of
            ReadOk chunk ->
              if predicate (chunkData chunk)
                then writeChunk trueStream chunk >> go
                else writeChunk falseStream chunk >> go
            ReadEndOfStream -> do
              closeStream trueStream
              closeStream falseStream
            ReadPaused -> go
    go
  pure (trueStream, falseStream)

-- ---------------------------------------------------------------------------
-- Progress
-- ---------------------------------------------------------------------------

reportProgress :: Stream a -> ProgressUpdate -> IO ()
reportProgress s update =
  atomically (writeTVar (streamProgress s) (Just update))


-- ---------------------------------------------------------------------------
-- Logging helper
-- ---------------------------------------------------------------------------

logMsg :: SeverityNumber -> Text -> [Attribute] -> IO ()
logMsg sev body attrs = do
  provider <- getGlobalLoggerProvider
  logger <- getLogger provider InstrumentationScope
    { scopeName = "streaming-core"
    , scopeVersion = Nothing
    , scopeSchemaUrl = Nothing
    , scopeAttributes = Nothing
    }
  ctx <- getCurrent
  emit logger defaultLogRecord
    { logSeverityNumber = Just sev
    , logBody = Just (LogBodyString body)
    , logAttributes = OTelAttr.fromList attrs
    , logContext = Just ctx
    }

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

streamIdText :: Stream a -> Text
streamIdText s = let StreamID uuid = streamId s in toText uuid
