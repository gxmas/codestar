-- | Batch processing lifecycle state machine.
--
-- Enforces the strict state machine: @in_progress -> canceling -> ended@.
-- No backward transitions.
module Anthropic.Server.Batch
  ( -- * Batch Lifecycle
    BatchLifecycle
  , newBatchLifecycle

    -- * Operations
  , createBatch
  , pollBatch
  , cancelBatch
  , deleteBatch
  , checkExpiry
  , transitionBatch

    -- * Errors
  , BatchLifecycleError (..)
  ) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Anthropic.Protocol.Batch
  ( BatchItem (..)
  , BatchResponse (..)
  , BatchStatus (..)
  , BatchCounts (..)
  , DeletedBatch (..)
  )

-- | Error from batch lifecycle operations.
data BatchLifecycleError
  = InvalidTransition !BatchStatus !BatchStatus
    -- ^ Attempted transition from first status to second status.
  | BatchNotFound !Text
  | BatchNotEnded !Text
    -- ^ Attempted to delete a batch that has not ended.
  deriving stock (Eq, Show)

-- | Internal batch state.
data BatchState = BatchState
  { stateResponse :: !BatchResponse
  , stateCreatedAt :: !UTCTime
  }

-- | Opaque batch lifecycle manager.
data BatchLifecycle = BatchLifecycle
  { lifecycleBatches :: !(TVar (Map Text BatchState))
  , lifecycleCounter :: !(TVar Int)
  }

-- | Create a new batch lifecycle manager.
newBatchLifecycle :: IO BatchLifecycle
newBatchLifecycle = do
  batches <- newTVarIO Map.empty
  counter <- newTVarIO 0
  pure BatchLifecycle
    { lifecycleBatches = batches
    , lifecycleCounter = counter
    }

-- | Create a new batch.
createBatch :: BatchLifecycle -> [BatchItem] -> IO (Either BatchLifecycleError BatchResponse)
createBatch lifecycle items = do
  now <- getCurrentTime
  batchId <- atomically $ do
    count <- readTVar lifecycle.lifecycleCounter
    writeTVar lifecycle.lifecycleCounter (count + 1)
    pure $ "batch_" <> T.pack (show count)

  let expiresAt = addUTCTime (24 * 3600) now  -- 24 hours
      response = BatchResponse
        { batchId = batchId
        , processingStatus = BatchInProgress
        , requestCounts = BatchCounts
            { processing = length items
            , succeeded = 0
            , errored = 0
            , canceled = 0
            , expired = 0
            }
        , createdAt = T.pack $ iso8601Show now
        , endedAt = Nothing
        , expiresAt = T.pack $ iso8601Show expiresAt
        , archivedAt = Nothing
        , cancelInitiatedAt = Nothing
        , resultsUrl = Nothing
        }
      state = BatchState
        { stateResponse = response
        , stateCreatedAt = now
        }

  atomically $ do
    batches <- readTVar lifecycle.lifecycleBatches
    writeTVar lifecycle.lifecycleBatches (Map.insert batchId state batches)

  pure $ Right response

-- | Poll a batch for status.
pollBatch :: BatchLifecycle -> Text -> IO (Either BatchLifecycleError BatchResponse)
pollBatch lifecycle batchId = do
  mState <- atomically $ do
    batches <- readTVar lifecycle.lifecycleBatches
    pure $ Map.lookup batchId batches

  case mState of
    Nothing -> pure $ Left $ BatchNotFound batchId
    Just state -> pure $ Right state.stateResponse

-- | Cancel an in-progress batch.
cancelBatch :: BatchLifecycle -> Text -> IO (Either BatchLifecycleError BatchResponse)
cancelBatch lifecycle batchId = do
  now <- getCurrentTime
  result <- atomically $ do
    batches <- readTVar lifecycle.lifecycleBatches
    case Map.lookup batchId batches of
      Nothing -> pure $ Left $ BatchNotFound batchId
      Just state -> case state.stateResponse.processingStatus of
        BatchInProgress -> do
          let response = state.stateResponse
                { processingStatus = BatchCanceling
                , cancelInitiatedAt = Just $ T.pack $ iso8601Show now
                }
              newState = state { stateResponse = response }
          writeTVar lifecycle.lifecycleBatches (Map.insert batchId newState batches)
          pure $ Right response
        BatchCanceling -> pure $ Right state.stateResponse  -- Idempotent
        BatchEnded -> pure $ Left $ InvalidTransition BatchEnded BatchCanceling

  pure result

-- | Delete an ended batch.
deleteBatch :: BatchLifecycle -> Text -> IO (Either BatchLifecycleError DeletedBatch)
deleteBatch lifecycle batchId = do
  result <- atomically $ do
    batches <- readTVar lifecycle.lifecycleBatches
    case Map.lookup batchId batches of
      Nothing -> pure $ Left $ BatchNotFound batchId
      Just state -> case state.stateResponse.processingStatus of
        BatchEnded -> do
          writeTVar lifecycle.lifecycleBatches (Map.delete batchId batches)
          pure $ Right $ DeletedBatch { batchId = batchId }
        _ -> pure $ Left $ BatchNotEnded batchId

  pure result

-- | Check whether a batch has expired (24-hour window).
checkExpiry :: BatchLifecycle -> Text -> IO (Either BatchLifecycleError Bool)
checkExpiry lifecycle batchId = do
  now <- getCurrentTime
  mState <- atomically $ do
    batches <- readTVar lifecycle.lifecycleBatches
    pure $ Map.lookup batchId batches

  case mState of
    Nothing -> pure $ Left $ BatchNotFound batchId
    Just state -> do
      let elapsed = diffUTCTime now state.stateCreatedAt
          expired = elapsed > (24 * 3600)  -- 24 hours
      pure $ Right expired

-- | Transition a batch to a new status.
-- Enforces state machine: in_progress -> canceling -> ended (no backward transitions).
transitionBatch :: BatchLifecycle -> Text -> BatchStatus -> IO (Either BatchLifecycleError BatchResponse)
transitionBatch lifecycle batchId newStatus = do
  now <- getCurrentTime
  result <- atomically $ do
    batches <- readTVar lifecycle.lifecycleBatches
    case Map.lookup batchId batches of
      Nothing -> pure $ Left $ BatchNotFound batchId
      Just state -> do
        let currentStatus = state.stateResponse.processingStatus
        if isValidTransition currentStatus newStatus
          then do
            let response = state.stateResponse
                  { processingStatus = newStatus
                  , endedAt = if newStatus == BatchEnded
                              then Just $ T.pack $ iso8601Show now
                              else state.stateResponse.endedAt
                  }
                newState = state { stateResponse = response }
            writeTVar lifecycle.lifecycleBatches (Map.insert batchId newState batches)
            pure $ Right response
          else
            pure $ Left $ InvalidTransition currentStatus newStatus

  pure result

-- | Check if a state transition is valid.
isValidTransition :: BatchStatus -> BatchStatus -> Bool
isValidTransition from to = case (from, to) of
  -- Valid forward transitions
  (BatchInProgress, BatchCanceling) -> True
  (BatchInProgress, BatchEnded)     -> True
  (BatchCanceling, BatchEnded)      -> True
  -- Idempotent (same state)
  (s1, s2) | s1 == s2                -> True
  -- All other transitions are invalid (no backward transitions)
  _                                  -> False
