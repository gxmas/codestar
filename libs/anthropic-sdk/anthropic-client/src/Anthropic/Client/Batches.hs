{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Batch API operations.
module Anthropic.Client.Batches
  ( -- * Batch Operations
    createBatch
  , getBatch
  , listBatches
  , cancelBatch
  , deleteBatch
  , getBatchResults
  ) where

import Prelude hiding (log)

import Control.Exception (catch)
import qualified Data.Aeson as Aeson
import Data.Aeson (eitherDecodeStrict', (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( HttpException(..)
  , withResponse, responseBody
  , responseHeaders
  )
import Streaming (Stream, Of)
import qualified Streaming as S
import qualified Streaming.Prelude as SP

import Anthropic.Types (Page, BatchId(..), RequestId(..), ResponseMeta(..))
import Anthropic.Client.Config
import Anthropic.Client.Internal.Execute (executeJsonPost, executeJsonGet, executeDelete)
import Anthropic.Client.Internal.Http (buildGetRequest, baseUrl)
import Anthropic.Client.Internal.RateLimit (parseRateLimitHeaders, updateRateLimits)
import Anthropic.Protocol.Batch (BatchItem, BatchResponse, BatchResultItem, DeletedBatch)
import Telemetry.Core (withSpan, AttributeValue(..))

-- | Create a new message batch.
createBatch
  :: AnthropicClient
  -> [BatchItem]
  -> IO (Either ClientError BatchResponse)
createBatch client items =
  withSpan "gen_ai.batch" [("gen_ai.system", TextValue "anthropic")] $
    executeJsonPost client "/v1/messages/batches" (Aeson.object ["requests" .= items])

-- | Retrieve a batch by ID.
getBatch
  :: AnthropicClient
  -> BatchId
  -> IO (Either ClientError BatchResponse)
getBatch client (BatchId bid) =
  executeJsonGet client ("/v1/messages/batches/" <> TE.encodeUtf8 bid)

-- | List batches with pagination.
listBatches
  :: AnthropicClient
  -> Maybe Text     -- ^ @after_id@ cursor
  -> Maybe Text     -- ^ @before_id@ cursor
  -> Maybe Int      -- ^ @limit@ (1-1000, default 20)
  -> IO (Either ClientError (Page BatchResponse))
listBatches client afterId beforeId limit =
  executeJsonGet client (buildPath "/v1/messages/batches" params)
  where
    params = concat
      [ maybe [] (\x -> [("after_id",  TE.encodeUtf8 x)])  afterId
      , maybe [] (\x -> [("before_id", TE.encodeUtf8 x)])  beforeId
      , maybe [] (\x -> [("limit",     BS8.pack (show x))]) limit
      ]

-- | Cancel an in-progress batch.
cancelBatch
  :: AnthropicClient
  -> BatchId
  -> IO (Either ClientError BatchResponse)
cancelBatch client (BatchId bid) =
  executeJsonPost client
    ("/v1/messages/batches/" <> TE.encodeUtf8 bid <> "/cancel")
    (Aeson.object [])

-- | Delete an ended batch.
deleteBatch
  :: AnthropicClient
  -> BatchId
  -> IO (Either ClientError DeletedBatch)
deleteBatch client (BatchId bid) =
  executeDelete client ("/v1/messages/batches/" <> TE.encodeUtf8 bid)

-- | Stream batch results (JSONL, continuation-passing).
--
-- Results are not guaranteed to be in request order.
-- Use @customId@ to match results to requests.
getBatchResults
  :: AnthropicClient
  -> BatchId
  -> (Stream (Of BatchResultItem) IO () -> IO a)
  -> IO (Either ClientError a)
getBatchResults client (BatchId bid) k = do
  let cfg = client.clientConfig
      base = fromMaybe baseUrl (cfg.baseUrl)
      path = "/v1/messages/batches/" <> TE.encodeUtf8 bid <> "/results"
      req  = buildGetRequest
               (cfg.apiKey) base path
               (cfg.defaultHeaders) (cfg.betaFeatures) (cfg.timeout)

  (do
    result <- withResponse req (client.clientManager) $ \resp -> do
      -- Extract rate limit headers
      let hdrs = responseHeaders resp
          mRateLimits = parseRateLimitHeaders hdrs
          mRequestId  = fmap (RequestId . TE.decodeUtf8) (lookup (CI.mk "request-id") hdrs)

      case (mRateLimits, mRequestId) of
        (Just rl, Just rid) -> do
          updateRateLimits (client.clientRateLimits) rl
          (client.clientConfig).onResponseMeta ResponseMeta
            { rateLimits = rl
            , requestId  = rid
            }
        (Just rl, Nothing) ->
          updateRateLimits (client.clientRateLimits) rl
        _ -> pure ()

      -- Parse JSONL: each line is a BatchResultItem
      let bodyReader = responseBody resp
          lineStream = readJsonlLines bodyReader
      k lineStream

    pure (Right result)
    ) `catch` \(e :: HttpException) ->
        pure (Left (NetworkError e))

-- | Read JSONL lines from a BodyReader, yielding parsed BatchResultItems.
readJsonlLines :: IO BS.ByteString -> Stream (Of BatchResultItem) IO ()
readJsonlLines readChunk = go mempty
  where
    go buffer = do
      chunk <- S.lift readChunk
      if BS.null chunk
        then
          -- Flush any remaining data
          case splitLine buffer of
            Just (line, _) -> yieldLine line
            Nothing        -> pure ()
        else
          processBuffer (buffer <> chunk)

    processBuffer buffer =
      case splitLine buffer of
        Just (line, rest) -> do
          yieldLine line
          processBuffer rest
        Nothing ->
          go buffer

    splitLine bs =
      case BS8.elemIndex '\n' bs of
        Just idx -> Just (BS.take idx bs, BS.drop (idx + 1) bs)
        Nothing  -> Nothing

    yieldLine line
      | BS.null line = pure ()
      | otherwise =
          case eitherDecodeStrict' line of
            Right item -> SP.yield item
            Left _     -> pure ()  -- Skip malformed lines

-- | Build a path with query parameters.
buildPath :: BS8.ByteString -> [(BS8.ByteString, BS8.ByteString)] -> BS8.ByteString
buildPath path [] = path
buildPath path ps = path <> "?" <> BS8.intercalate "&" (map (\(k, v) -> k <> "=" <> v) ps)
