-- | Internal: SSE response body consumption.
--
-- __This module is not part of the public API.__
module Anthropic.Client.Internal.SSE
  ( -- * SSE Consumption
    consumeSseResponse
  ) where

import Network.HTTP.Client (Response, BodyReader, responseBody)
import Streaming (Stream, Of)

import Anthropic.Protocol.Stream (StreamEvent)
import Anthropic.Protocol.Stream.Parser (parseSseStream)
import Anthropic.Protocol.Message (MessageResponse)

-- | Consume an HTTP response body as a stream of SSE events.
--
-- Bridges @http-client@'s 'BodyReader' to the streaming-based
-- event stream via 'parseSseStream'. The 'BodyReader' returns
-- empty 'ByteString' at end-of-stream, which is exactly what
-- 'parseSseStream' expects.
consumeSseResponse :: Response BodyReader -> Stream (Of StreamEvent) IO MessageResponse
consumeSseResponse resp = parseSseStream (responseBody resp)
