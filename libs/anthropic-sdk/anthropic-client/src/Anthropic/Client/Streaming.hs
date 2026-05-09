{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Streaming message operations.
module Anthropic.Client.Streaming
  ( -- * Event Handler (push-based)
    EventHandler (..)
  , defaultEventHandler

    -- * Streaming Operations
  , streamMessages
  , streamMessagesWith
  ) where

import Prelude hiding (log)

import Control.Exception (catch)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as TE
import Streaming (Stream, Of)
import qualified Streaming.Prelude as SP
import Telemetry.Core (withSpan, AttributeValue(..))

import Anthropic.Types
import Anthropic.Client.Config
import Anthropic.Client.Internal.Execute (ErrorEnvelope (..))
import Anthropic.Client.Internal.Http
import Anthropic.Client.Internal.RateLimit
import Anthropic.Client.Internal.SSE (consumeSseResponse)
import Anthropic.Protocol.Message (MessageRequest(..), MessageResponse(..))
import Anthropic.Protocol.Stream
  ( StreamEvent(..), Delta(..)
  , StreamState, StreamPhase(..), streamPhase, accumulatedBlocks
  )
import Anthropic.Protocol.Stream.Accumulator (accumulate, initialStreamState)

import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
  ( HttpException(..)
  , withResponse, responseHeaders, responseBody, responseStatus
  )
import Network.HTTP.Types (statusIsSuccessful, statusCode)

-- | Record of callbacks for push-based streaming.
--
-- Use 'defaultEventHandler' and override the callbacks you care about
-- with record update syntax:
--
-- @
-- result <- streamMessagesWith client req defaultEventHandler
--   { onContentBlockDelta = \\_ delta -> case delta of
--       TextDelta t -> T.putStr t
--       _           -> pure ()
--   }
-- @
data EventHandler = EventHandler
  { onMessageStart      :: MessageResponse -> IO ()
    -- ^ Called when the stream starts with the initial message.
  , onContentBlockStart :: ContentBlockIndex -> ContentBlock -> IO ()
    -- ^ Called when a new content block begins.
  , onContentBlockDelta :: ContentBlockIndex -> Delta -> IO ()
    -- ^ Called for each incremental update to a content block.
  , onContentBlockStop  :: ContentBlockIndex -> ContentBlock -> IO ()
    -- ^ Called when a content block is finalized.
  , onMessageComplete   :: MessageResponse -> IO ()
    -- ^ Called when the complete message is assembled.
  , onError             :: ApiError -> IO ()
    -- ^ Called when a stream error event is received.
  }

-- | Default event handler with no-op callbacks.
--
-- Override the callbacks you need:
--
-- @
-- defaultEventHandler { onContentBlockDelta = \\_ d -> handleDelta d }
-- @
defaultEventHandler :: EventHandler
defaultEventHandler = EventHandler
  { onMessageStart      = \_ -> pure ()
  , onContentBlockStart = \_ _ -> pure ()
  , onContentBlockDelta = \_ _ -> pure ()
  , onContentBlockStop  = \_ _ -> pure ()
  , onMessageComplete   = \_ -> pure ()
  , onError             = \_ -> pure ()
  }

-- | Stream a message response (pull-based, continuation-passing).
--
-- The stream is scoped to the continuation to prevent the consumer
-- from holding a reference after the HTTP connection is closed.
-- See ADR-002 and ADR-007.
--
-- @
-- result <- streamMessages client req $ \\stream -> do
--   finalMsg <- S.mapM_ handleEvent stream
--   pure finalMsg
-- @
streamMessages
  :: AnthropicClient
  -> MessageRequest
  -> (Stream (Of StreamEvent) IO MessageResponse -> IO a)
  -> IO (Either ClientError a)
streamMessages client msgReq k =
  withSpan "gen_ai.chat" [("gen_ai.system", TextValue "anthropic")] $ do
  let cfg = client.clientConfig
      base = fromMaybe baseUrl (cfg.baseUrl)
      -- Force stream: true in the serialized body
      body = case Aeson.toJSON msgReq of
        Aeson.Object o ->
          Aeson.Object (KM.insert "stream" (Aeson.Bool True) o)
        v -> v
      httpReq = buildRequest
                  (cfg.apiKey) base "/v1/messages"
                  (cfg.defaultHeaders) (cfg.betaFeatures) (cfg.timeout)
                  body

  (do
    result <- withResponse httpReq (client.clientManager) $ \resp -> do
      -- Extract rate limit headers from the initial response
      let hdrs = responseHeaders resp
          status = responseStatus resp
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

      if statusIsSuccessful status
        then do
          -- Bridge the response body to a stream of SSE events
          let eventStream = consumeSseResponse resp
          Right <$> k eventStream
        else do
          -- Read the error body and return a ClientError
          chunks <- readAllChunks (responseBody resp)
          let rawBody = LBS.fromChunks chunks
          case Aeson.eitherDecode rawBody of
            Right (ErrorEnvelope apiErr) ->
              pure (Left (ApiErrorResponse apiErr mRateLimits))
            Left _ ->
              let msg = "HTTP " <> T.pack (show (statusCode status))
              in pure (Left (DeserializationError msg (LBS.toStrict rawBody)))

    case result of
      Right val -> pure (Right val)
      Left err  -> pure (Left err)
    ) `catch` \(e :: HttpException) ->
        pure (Left (NetworkError e))

-- | Stream a message response (push-based via 'EventHandler').
--
-- This is the primary ergonomic streaming API. The library drives
-- the stream and dispatches events to the handler callbacks.
--
-- @
-- result <- streamMessagesWith client req defaultEventHandler
--   { onContentBlockDelta = \\_ delta -> case delta of
--       TextDelta t -> T.putStr t
--       _           -> pure ()
--   }
-- @
streamMessagesWith
  :: AnthropicClient
  -> MessageRequest
  -> EventHandler
  -> IO (Either ClientError MessageResponse)
streamMessagesWith client msgReq handler =
  withSpan "gen_ai.chat" [("gen_ai.system", TextValue "anthropic")] $
  streamMessages client msgReq $ \stream -> do
    -- Fold over the stream, dispatching events to the handler
    -- and accumulating stream state
    (finalMsg, finalState) <- SP.foldM_
      (\(startMsg, st) evt -> do
        dispatchEvent handler st evt
        let st' = accumulate st evt
        let msg' = case evt of
              EventMessageStart m -> Just m
              _                   -> startMsg
        pure (msg', st')
      )
      (pure (Nothing, initialStreamState))
      pure
      stream

    -- Build the final MessageResponse from accumulated state
    let assembled = case finalMsg of
          Just msg -> msg
            { content    = accumulatedBlocks finalState
            , stopReason = case finalState of
                _ | streamPhase finalState == StreamDone ->
                    finalMsg >>= (.stopReason)
                _ -> Nothing
            }
          Nothing -> error "streamMessagesWith: stream ended without message_start"

    handler.onMessageComplete assembled
    pure assembled

-- | Dispatch a single stream event to the appropriate EventHandler callback.
dispatchEvent :: EventHandler -> StreamState -> StreamEvent -> IO ()
dispatchEvent handler st = \case
  EventMessageStart msg ->
    handler.onMessageStart msg

  EventContentBlockStart idx block ->
    handler.onContentBlockStart idx block

  EventContentBlockDelta idx delta ->
    handler.onContentBlockDelta idx delta

  EventContentBlockStop idx ->
    -- The accumulated state has the finalized block
    let blocks = accumulatedBlocks (accumulate st (EventContentBlockStop idx))
        block  = if null blocks
                 then TextContent (TextBlock "" Nothing Nothing)
                 else last blocks
    in handler.onContentBlockStop idx block

  EventMessageDelta _mStop _mStopSeq _usage ->
    pure ()

  EventMessageStop ->
    pure ()

  EventPing ->
    pure ()

  EventError apiErr ->
    handler.onError apiErr


-- | Read all chunks from a BodyReader until empty.
readAllChunks :: IO BS.ByteString -> IO [BS.ByteString]
readAllChunks reader = go []
  where
    go acc = do
      chunk <- reader
      if BS.null chunk
        then pure (reverse acc)
        else go (chunk : acc)
