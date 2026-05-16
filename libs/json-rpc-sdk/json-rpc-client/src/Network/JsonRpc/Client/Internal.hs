{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.JsonRpc.Client.Internal
Stability   : unstable
Portability : GHC2021

__This is an internal module.__ The API may change between minor
versions without notice. Import "Network.JsonRpc.Client" for the
stable public interface.

Exports the internal representation of the client, pending request
tracking, and id generation.
-}
module Network.JsonRpc.Client.Internal (
    -- * Client
    Client (..),
    newClient,
    ClientConfig (..),
    defaultClientConfig,

    -- * Id generation
    IdGenerator (..),
    sequentialIds,
    setIdGenerator,

    -- * Building requests
    request,
    notify,
    PendingRequest (..),

    -- * Batch building
    BatchIntent (..),
    PendingBatch (..),
    batch,

    -- * Response handling
    MatchResult (..),
    handleResponse,
    handleBatchResponse,

    -- * Timeout
    ExpiredRequest (..),
    expire,

    -- * Inspection
    pending,
    PendingEntry (..),
) where

import Prelude hiding (log)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Network.JsonRpc.Types.Internal
import OTel.Trace (withSpan, SomeTracer, getGlobalTracerProvider, getTracer)
import OTel.Attribute (AttributeValue(..), InstrumentationScope(..))

-- ====================================================================
-- Id generation
-- ====================================================================

{- | An id generator produces fresh 'Id' values for outgoing requests.
The generator is mutable (it runs in 'IO') to support sequential
counters and similar stateful strategies.
-}
newtype IdGenerator = IdGenerator
    { nextId :: IO Id
    -- ^ Generate the next request id.
    }

{- | Sequential numeric ids starting from 1. This is the default
generator. Thread-safe via 'atomicModifyIORef''.
-}
sequentialIds :: IO IdGenerator
sequentialIds = do
    ref <- newIORef (0 :: Int)
    pure $
        IdGenerator $ do
            n <- atomicModifyIORef' ref (\i -> let i' = i + 1 in (i', i'))
            pure (IdNum (fromFloatDigits (fromIntegral n :: Double)))

-- ====================================================================
-- Pending entry
-- ====================================================================

{- | An entry in the client's pending request map. Contains the
original request and the time it was created, for timeout
management.
-}
data PendingEntry = PendingEntry
    { entryId :: !Id
    -- ^ The request id.
    , entryRequest :: !Request
    -- ^ The original request.
    , entryTimestamp :: !UTCTime
    -- ^ The time the request was created.
    }
    deriving (Show)

-- ====================================================================
-- Client
-- ====================================================================

-- | Client configuration.
newtype ClientConfig = ClientConfig
    { onTimeout :: ExpiredRequest -> IO ()
    -- ^ Called for each request removed by 'expire'.
    }

-- | Default client configuration with no-op callbacks.
defaultClientConfig :: ClientConfig
defaultClientConfig =
    ClientConfig
        { onTimeout = \_ -> pure ()
        }

{- | An opaque JSON-RPC 2.0 client that tracks pending requests and
generates unique ids. Create with 'newClient'.
-}
data Client = Client
    { clientPending :: !(IORef (HashMap Id PendingEntry))
    , clientIdGen :: !(IORef IdGenerator)
    , clientConfig :: !ClientConfig
    , clientTracer :: !SomeTracer
    }

{- | Create a new client with the given configuration. Uses
'sequentialIds' as the default id generator.
-}
newClient :: ClientConfig -> IO Client
newClient config = do
    pendingRef <- newIORef HM.empty
    gen <- sequentialIds
    genRef <- newIORef gen
    provider <- getGlobalTracerProvider
    tracer <- getTracer provider InstrumentationScope
        { scopeName       = "json-rpc-client"
        , scopeVersion    = Nothing
        , scopeSchemaUrl  = Nothing
        , scopeAttributes = Nothing
        }
    pure (Client pendingRef genRef config tracer)

{- | Replace the client's id generator. Useful for custom id schemes
(e.g., UUIDs).
-}
setIdGenerator :: Client -> IdGenerator -> IO ()
setIdGenerator c = writeIORef (clientIdGen c)

-- ====================================================================
-- Building requests
-- ====================================================================

{- | A pending request: the generated id and the full 'Request' value.
The caller serializes the request (via "Network.JsonRpc.Codec") and
sends it over the transport.
-}
data PendingRequest = PendingRequest
    { pendingRequestId :: !Id
    -- ^ The generated request id.
    , pendingRequestMessage :: !Request
    -- ^ The full request, ready for serialization.
    }
    deriving (Eq, Show)

{- | Build a request, generate an id, and track it as pending. The
returned 'PendingRequest' contains the 'Request' to serialize
and send.
-}
request :: Client -> Text -> Params -> IO PendingRequest
request c methodName params =
    withSpan (clientTracer c) "rpc.client.request" [("rpc.system", StringValue "jsonrpc"), ("rpc.method", StringValue methodName)] $ do
        gen <- readIORef (clientIdGen c)
        reqId <- nextId gen
        now <- getCurrentTime
        let req = Request methodName params (Just reqId)
            entry = PendingEntry reqId req now
        atomicModifyIORef' (clientPending c) $ \hm ->
            (HM.insert reqId entry hm, ())
        pure (PendingRequest reqId req)

{- | Build a notification. Notifications are not tracked (the server
will not reply).
-}
notify :: Client -> Text -> Params -> IO Notification
notify c methodName params =
    withSpan (clientTracer c) "rpc.client.notify" [("rpc.system", StringValue "jsonrpc"), ("rpc.method", StringValue methodName)] $
        pure (Notification methodName params)

-- ====================================================================
-- Batch building
-- ====================================================================

{- | An intent for a batch operation: either a request (which will
get an id and be tracked) or a notification.
-}
data BatchIntent
    = -- | A request intent: method name and parameters.
      IntentRequest !Text !Params
    | -- | A notification intent: method name and parameters.
      IntentNotify !Text !Params
    deriving (Eq, Show)

{- | The result of building a batch: the list of messages to serialize
as a JSON array, plus the ids of the request elements (for later
correlation).
-}
data PendingBatch = PendingBatch
    { pendingBatchMessages :: ![Message]
    -- ^ The full list of messages to serialize and send as a batch.
    , pendingBatchIds :: ![Id]
    -- ^ The ids of the request elements (not notifications).
    }
    deriving (Eq, Show)

{- | Build a batch of requests and notifications. Requests get
generated ids and are tracked as pending. Returns the batch
messages and the request ids.
-}
batch :: Client -> [BatchIntent] -> IO PendingBatch
batch c intents = do
    gen <- readIORef (clientIdGen c)
    now <- getCurrentTime
    (msgs, ids) <- go gen now [] [] intents
    pure (PendingBatch (reverse msgs) (reverse ids))
  where
    go :: IdGenerator -> UTCTime -> [Message] -> [Id] -> [BatchIntent] -> IO ([Message], [Id])
    go _ _ msgs ids [] = pure (msgs, ids)
    go gen now msgs ids (IntentRequest m p : rest) = do
        reqId <- nextId gen
        let req = Request m p (Just reqId)
            entry = PendingEntry reqId req now
        atomicModifyIORef' (clientPending c) $ \hm ->
            (HM.insert reqId entry hm, ())
        go gen now (MsgRequest req : msgs) (reqId : ids) rest
    go gen now msgs ids (IntentNotify m p : rest) = do
        let notif = Notification m p
        go gen now (MsgNotification notif : msgs) ids rest

-- ====================================================================
-- Response handling
-- ====================================================================

-- | The result of correlating a response with a pending request.
data MatchResult
    = {- | The response matched a pending request. The request has been
      removed from the pending map.
      -}
      Matched !Id !Response
    | {- | The response did not match any pending request (unknown id,
      or a response to a notification).
      -}
      Unmatched !Response
    deriving (Eq, Show)

{- | Correlate a single response with the client's pending requests.
If the response's id matches a pending request, the request is
removed from the pending map and 'Matched' is returned.
-}
handleResponse :: Client -> Response -> IO MatchResult
handleResponse c resp = do
    let mid = responseId resp
    case mid of
        Nothing -> pure (Unmatched resp)
        Just reqId -> do
            removed <- atomicModifyIORef' (clientPending c) $ \hm ->
                case HM.lookup reqId hm of
                    Nothing -> (hm, False)
                    Just _ -> (HM.delete reqId hm, True)
            if removed
                then pure (Matched reqId resp)
                else pure (Unmatched resp)

{- | Correlate a list of responses with the client's pending requests
(batch response handling).
-}
handleBatchResponse :: Client -> [Response] -> IO [MatchResult]
handleBatchResponse c = mapM (handleResponse c)

-- | Extract the id from a response.
responseId :: Response -> Maybe Id
responseId (ResponseSuccess s) = successId s
responseId (ResponseFailure f) = failureId f

-- ====================================================================
-- Timeout
-- ====================================================================

{- | A request that was removed from the pending map because it
exceeded the timeout.
-}
data ExpiredRequest = ExpiredRequest
    { expiredId :: !Id
    -- ^ The expired request's id.
    , expiredRequest :: !Request
    -- ^ The original request.
    , expiredAge :: !NominalDiffTime
    -- ^ How long the request was pending.
    }
    deriving (Show)

{- | Remove all pending requests older than the given timeout.
Returns the expired requests and calls the 'onTimeout' callback
for each one.
-}
expire :: Client -> NominalDiffTime -> IO [ExpiredRequest]
expire c timeout = do
    now <- getCurrentTime
    let isExpired entry = diffUTCTime now (entryTimestamp entry) > timeout
    expired <- atomicModifyIORef' (clientPending c) $ \hm ->
        let keep = HM.filter (not . isExpired) hm
            old = HM.filter isExpired hm
            expiredList =
                [ ExpiredRequest
                    (entryId e)
                    (entryRequest e)
                    (diffUTCTime now (entryTimestamp e))
                | e <- HM.elems old
                ]
         in (keep, expiredList)
    mapM_ (onTimeout (clientConfig c)) expired
    pure expired

-- ====================================================================
-- Inspection
-- ====================================================================

-- | Return all currently pending requests for inspection.
pending :: Client -> IO [PendingEntry]
pending c = HM.elems <$> readIORef (clientPending c)
