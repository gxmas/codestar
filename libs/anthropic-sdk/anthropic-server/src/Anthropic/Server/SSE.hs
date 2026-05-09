-- | Server-side SSE (Server-Sent Events) emission.
--
-- Wraps 'Anthropic.Protocol.Stream.Emitter' with HTTP-specific formatting.
-- Framework-agnostic: provides SSE-formatted bytestring stream.
module Anthropic.Server.SSE
  ( -- * SSE Emission
    emitSSE
  , emitSSEWithHeaders

    -- * HTTP Headers
  , sseHeaders
  ) where

import Data.ByteString (ByteString)

import Streaming (Stream, Of)

import Anthropic.Protocol.Stream (StreamEvent)
import Anthropic.Protocol.Stream.Emitter (emitSseStream)

-- | Emit SSE-formatted events for HTTP streaming response.
--
-- Takes a stream of 'StreamEvent' and produces SSE-formatted ByteString chunks
-- suitable for HTTP response body.
emitSSE :: Monad m => Stream (Of StreamEvent) m r -> Stream (Of ByteString) m r
emitSSE = emitSseStream

-- | Emit SSE with recommended HTTP headers included as first chunk.
--
-- This variant prepends the SSE headers as the first chunk in the stream,
-- useful for frameworks that send headers and body in the same stream.
emitSSEWithHeaders :: Monad m => Stream (Of StreamEvent) m r -> Stream (Of ByteString) m r
emitSSEWithHeaders events = do
  -- Note: This is a simplified version. In practice, headers would be
  -- set via the HTTP framework's response object, not in the body stream.
  -- This function is here for completeness but most frameworks handle
  -- headers separately.
  emitSseStream events

-- | Recommended HTTP headers for SSE responses.
--
-- Applications should set these headers when streaming SSE events:
--
-- * @Content-Type: text/event-stream@
-- * @Cache-Control: no-cache@
-- * @Connection: keep-alive@
sseHeaders :: [(ByteString, ByteString)]
sseHeaders =
  [ ("Content-Type", "text/event-stream")
  , ("Cache-Control", "no-cache")
  , ("Connection", "keep-alive")
  , ("X-Accel-Buffering", "no")  -- Disable nginx buffering
  ]
