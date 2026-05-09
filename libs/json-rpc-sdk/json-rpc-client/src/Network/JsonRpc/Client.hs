-- |
-- Module      : Network.JsonRpc.Client
-- Stability   : stable
-- Portability : GHC2021
--
-- A JSON-RPC 2.0 client that generates request ids, tracks pending
-- requests, correlates responses, and supports batch operations.
--
-- The client is transport-agnostic. It builds 'Request' and
-- 'Notification' values that you serialize (via "Network.JsonRpc.Codec")
-- and send over your chosen transport. When responses arrive, feed
-- them to 'handleResponse' to correlate with pending requests.
--
-- @
-- import Network.JsonRpc.Types
-- import Network.JsonRpc.Codec
-- import Network.JsonRpc.Client
--
-- example :: IO ()
-- example = do
--   c <- newClient defaultClientConfig
--   pr <- request c \"add\" (ParamsByName params)
--   let bytes = serialize (MsgRequest (pendingRequestMessage pr))
--   -- send bytes over transport, receive response bytes
--   case parse responseBytes of
--     SingleMessage (MsgResponse resp) -> do
--       result <- handleResponse c resp
--       print result
--     _ -> pure ()
-- @
module Network.JsonRpc.Client
  ( -- * Client
    Client
  , newClient
  , ClientConfig (..)
  , defaultClientConfig

    -- * Id generation
  , IdGenerator (..)
  , sequentialIds
  , setIdGenerator

    -- * Building requests
  , request
  , notify
  , PendingRequest (..)

    -- * Batch building
  , BatchIntent (..)
  , PendingBatch (..)
  , batch

    -- * Response handling
  , MatchResult (..)
  , handleResponse
  , handleBatchResponse

    -- * Timeout
  , ExpiredRequest (..)
  , expire

    -- * Inspection
  , pending
  , PendingEntry (..)
  ) where

import Network.JsonRpc.Client.Internal
