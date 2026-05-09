{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Error recovery with retry, backoff, and circuit breaker.
module Resilience.Core
  ( -- * Backoff strategies
    BackoffStrategy (..)
  , FallbackStrategy (..)

    -- * Recovery policy
  , RecoveryPolicy (..)
  , defaultRecoveryPolicy

    -- * Circuit breaker
  , CircuitBreakerConfig (..)
  , CircuitState (..)

    -- * Results
  , RecoveryResult (..)
  , RetryContext (..)

    -- * Engine
  , RecoveryEngine
  , newRecoveryEngine

    -- * Execution
  , withRecovery
  , withRecoveryAsync
  , withRecoveryEither

    -- * Circuit breaker management
  , getCircuitState
  , resetCircuit

    -- * Policy management
  , setDefaultPolicy
  , setOperationPolicy
  , getPolicy

    -- * Testing support
  , computeDelay
  ) where

import Prelude hiding (log)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, writeTVar)
import Control.Exception (SomeException, try)
import Data.Aeson (ToJSON, FromJSON, Value)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)
import System.Random (randomRIO)

import qualified Data.Map.Strict as Map

import qualified Telemetry.Core
import Telemetry.Core (withSpan, AttributeValue (..))

-- ---------------------------------------------------------------------------
-- Backoff strategies
-- ---------------------------------------------------------------------------

data BackoffStrategy
  = Constant !NominalDiffTime
  | Linear !NominalDiffTime
  | Exponential !NominalDiffTime !Double
  | Jittered !BackoffStrategy
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Fallback strategies
-- ---------------------------------------------------------------------------

data FallbackStrategy
  = Alternative !Text
  | Cached
  | DefaultValue !Value
  | Fail
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Circuit breaker
-- ---------------------------------------------------------------------------

data CircuitBreakerConfig = CircuitBreakerConfig
  { cbFailureThreshold :: !Int
  , cbSuccessThreshold :: !Int
  , cbTimeout          :: !NominalDiffTime
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data CircuitState
  = CircuitClosed
  | CircuitOpen !UTCTime
  | CircuitHalfOpen
  deriving stock (Eq, Show)

data CircuitInfo = CircuitInfo
  { ciState    :: !(TVar CircuitState)
  , ciConfig   :: !CircuitBreakerConfig
  , ciFailures :: !(IORef Int)
  , ciSuccesses :: !(IORef Int)
  }

-- ---------------------------------------------------------------------------
-- Recovery policy
-- ---------------------------------------------------------------------------

data RecoveryPolicy = RecoveryPolicy
  { rpMaxRetries     :: !Int
  , rpBackoff        :: !BackoffStrategy
  , rpTimeout        :: !NominalDiffTime
  , rpFallback       :: !(Maybe FallbackStrategy)
  , rpCircuitBreaker :: !(Maybe CircuitBreakerConfig)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

defaultRecoveryPolicy :: RecoveryPolicy
defaultRecoveryPolicy = RecoveryPolicy
  { rpMaxRetries     = 3
  , rpBackoff        = Exponential 0.1 2.0
  , rpTimeout        = 30
  , rpFallback       = Just Fail
  , rpCircuitBreaker = Nothing
  }

-- ---------------------------------------------------------------------------
-- Results
-- ---------------------------------------------------------------------------

data RecoveryResult a
  = RecoverySuccess !a
  | FailedAfterRetries !SomeException !Int
  | FallbackUsed !a !Text
  | RecoveryCircuitOpen
  deriving stock (Show)

data RetryContext = RetryContext
  { rcOperation :: !Text
  , rcAttempt   :: !Int
  , rcLastError :: !SomeException
  , rcStarted   :: !UTCTime
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Engine
-- ---------------------------------------------------------------------------

data RecoveryEngine = RecoveryEngine
  { reDefaultPolicy     :: !(IORef RecoveryPolicy)
  , reOperationPolicies :: !(IORef (Map Text RecoveryPolicy))
  , reCircuits          :: !(IORef (Map Text CircuitInfo))
  }

newRecoveryEngine :: RecoveryPolicy -> IO RecoveryEngine
newRecoveryEngine policy = RecoveryEngine
  <$> newIORef policy
  <*> newIORef Map.empty
  <*> newIORef Map.empty

-- ---------------------------------------------------------------------------
-- Execution
-- ---------------------------------------------------------------------------

withRecovery :: RecoveryEngine -> Text -> IO a -> IO (RecoveryResult a)
withRecovery engine opName action =
  withSpan "resilience.recover"
    [("resilience.operation", TextValue opName)] $ do
  policy <- getPolicy engine opName
  case rpCircuitBreaker policy of
    Nothing -> retryLoop policy 0
    Just cbConfig -> do
      ci <- getOrCreateCircuit engine opName cbConfig
      state <- readTVarIO (ciState ci)
      now <- getCurrentTime
      case state of
        CircuitOpen openedAt
          | diffUTCTime now openedAt >= cbTimeout cbConfig -> do
              atomically $ writeTVar (ciState ci) CircuitHalfOpen
              writeIORef (ciSuccesses ci) 0
              probeCircuit ci policy
          | otherwise -> pure RecoveryCircuitOpen
        CircuitClosed -> retryLoopWithCircuit ci policy 0
        CircuitHalfOpen -> probeCircuit ci policy
  where
    retryLoop policy attempt = do
      result <- try action
      case result of
        Right val -> pure (RecoverySuccess val)
        Left (err :: SomeException)
          | attempt >= rpMaxRetries policy ->
              pure (FailedAfterRetries err (attempt + 1))
          | otherwise -> do
              delay <- computeDelay (rpBackoff policy) (attempt + 1)
              span' <- Telemetry.Core.startSpan "resilience.retry"
                [ ("retry.attempt", IntValue (attempt + 1))
                , ("retry.delay_ms", IntValue (round (delay * 1000)))
                , ("retry.error", TextValue (T.pack (show err)))
                ]
              threadDelay (nominalToMicros delay)
              Telemetry.Core.endSpan span'
              retryLoop policy (attempt + 1)

    retryLoopWithCircuit ci policy attempt = do
      result <- try action
      case result of
        Right val -> do
          writeIORef (ciFailures ci) 0
          pure (RecoverySuccess val)
        Left (err :: SomeException) -> do
          modifyIORef' (ciFailures ci) (+ 1)
          failures <- readIORef (ciFailures ci)
          whenIO (failures >= cbFailureThreshold (ciConfig ci)) $ do
            now <- getCurrentTime
            atomically $ writeTVar (ciState ci) (CircuitOpen now)
          if attempt >= rpMaxRetries policy
            then pure (FailedAfterRetries err (attempt + 1))
            else do
              delay <- computeDelay (rpBackoff policy) (attempt + 1)
              threadDelay (nominalToMicros delay)
              retryLoopWithCircuit ci policy (attempt + 1)

    probeCircuit ci _policy = do
      result <- try action
      case result of
        Right val -> do
          modifyIORef' (ciSuccesses ci) (+ 1)
          successes <- readIORef (ciSuccesses ci)
          whenIO (successes >= cbSuccessThreshold (ciConfig ci)) $
            atomically $ writeTVar (ciState ci) CircuitClosed
          pure (RecoverySuccess val)
        Left (err :: SomeException) -> do
          now <- getCurrentTime
          atomically $ writeTVar (ciState ci) (CircuitOpen now)
          pure (FailedAfterRetries err 1)

    whenIO True  m = m
    whenIO False _ = pure ()

withRecoveryAsync :: RecoveryEngine -> Text -> IO a -> IO (Async (RecoveryResult a))
withRecoveryAsync engine opName action =
  async (withRecovery engine opName action)

-- ---------------------------------------------------------------------------
-- Circuit breaker management
-- ---------------------------------------------------------------------------

getCircuitState :: RecoveryEngine -> Text -> IO (Maybe CircuitState)
getCircuitState engine opName = do
  circuits <- readIORef (reCircuits engine)
  case Map.lookup opName circuits of
    Nothing -> pure Nothing
    Just ci -> Just <$> readTVarIO (ciState ci)

resetCircuit :: RecoveryEngine -> Text -> IO ()
resetCircuit engine opName = do
  circuits <- readIORef (reCircuits engine)
  case Map.lookup opName circuits of
    Nothing -> pure ()
    Just ci -> do
      atomically $ writeTVar (ciState ci) CircuitClosed
      writeIORef (ciFailures ci) 0
      writeIORef (ciSuccesses ci) 0

-- ---------------------------------------------------------------------------
-- Policy management
-- ---------------------------------------------------------------------------

setDefaultPolicy :: RecoveryEngine -> RecoveryPolicy -> IO ()
setDefaultPolicy engine policy = writeIORef (reDefaultPolicy engine) policy

setOperationPolicy :: RecoveryEngine -> Text -> RecoveryPolicy -> IO ()
setOperationPolicy engine opName policy =
  modifyIORef' (reOperationPolicies engine) (Map.insert opName policy)

getPolicy :: RecoveryEngine -> Text -> IO RecoveryPolicy
getPolicy engine opName = do
  perOp <- readIORef (reOperationPolicies engine)
  case Map.lookup opName perOp of
    Just p  -> pure p
    Nothing -> readIORef (reDefaultPolicy engine)

-- ---------------------------------------------------------------------------
-- Delay computation
-- ---------------------------------------------------------------------------

computeDelay :: BackoffStrategy -> Int -> IO NominalDiffTime
computeDelay (Constant d) _ = pure d
computeDelay (Linear d) attempt = pure $ d * fromIntegral attempt
computeDelay (Exponential base mult) attempt =
  pure $ base * realToFrac (mult ** fromIntegral (attempt - 1))
computeDelay (Jittered strat) attempt = do
  baseDelay <- computeDelay strat attempt
  jitter <- randomRIO (0.5 :: Double, 1.5)
  pure $ baseDelay * realToFrac jitter

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

getOrCreateCircuit :: RecoveryEngine -> Text -> CircuitBreakerConfig -> IO CircuitInfo
getOrCreateCircuit engine opName cbConfig = do
  circuits <- readIORef (reCircuits engine)
  case Map.lookup opName circuits of
    Just ci -> pure ci
    Nothing -> do
      ci <- CircuitInfo
        <$> newTVarIO CircuitClosed
        <*> pure cbConfig
        <*> newIORef 0
        <*> newIORef 0
      modifyIORef' (reCircuits engine) (Map.insert opName ci)
      pure ci

nominalToMicros :: NominalDiffTime -> Int
nominalToMicros dt = round (realToFrac dt * (1_000_000 :: Double))

-- ---------------------------------------------------------------------------
-- Either-returning variant
-- ---------------------------------------------------------------------------

-- | Like 'withRecovery' but for actions returning 'Either' rather than
-- throwing exceptions. The predicate decides which errors are retryable;
-- the delay override optionally replaces the policy backoff (e.g. to
-- honour a server-supplied Retry-After value).
withRecoveryEither
  :: RecoveryEngine
  -> Text
  -> (e -> Bool)                  -- ^ is this error retryable?
  -> (e -> Maybe NominalDiffTime) -- ^ optional per-error delay override
  -> IO (Either e a)
  -> IO (Either e a)
withRecoveryEither engine opName isRetryable delayOverride action =
  withSpan "resilience.recover"
    [("resilience.operation", TextValue opName)] $ do
  policy <- getPolicy engine opName
  go policy 0
  where
    go policy attempt = do
      result <- action
      case result of
        Right a -> pure (Right a)
        Left err
          | not (isRetryable err)        -> pure (Left err)
          | attempt >= rpMaxRetries policy -> pure (Left err)
          | otherwise -> do
              delay <- case delayOverride err of
                Just d  -> pure d
                Nothing -> computeDelay (rpBackoff policy) (attempt + 1)
              span' <- Telemetry.Core.startSpan "resilience.retry"
                [ ("retry.attempt",   IntValue (attempt + 1))
                , ("retry.delay_ms",  IntValue (round (delay * 1000)))
                ]
              threadDelay (nominalToMicros delay)
              Telemetry.Core.endSpan span'
              go policy (attempt + 1)
