-- | SSE event emission (server-side).
module Anthropic.Protocol.Stream.Emitter
  ( -- * Emission
    emitSseStream
  ) where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S

import Anthropic.Protocol.Stream (StreamEvent(..))

-- | Convert a stream of 'StreamEvent' values into SSE-formatted bytes.
--
-- Each event is formatted as:
--
-- @
-- event: \<event_name\>
-- data: \<json_payload\>
--
-- @
--
-- The trailing blank line is required by the SSE specification to
-- delimit events.
emitSseStream :: (Monad m) => Stream (Of StreamEvent) m r -> Stream (Of ByteString) m r
emitSseStream = S.map formatEvent
  where
    formatEvent :: StreamEvent -> ByteString
    formatEvent evt =
      let name = sseEventName evt
          payload = LBS.toStrict (Aeson.encode evt)
      in "event: " <> name <> "\ndata: " <> payload <> "\n\n"

-- | SSE event name for each stream event type.
sseEventName :: StreamEvent -> ByteString
sseEventName (EventMessageStart _)        = "message_start"
sseEventName (EventContentBlockStart _ _) = "content_block_start"
sseEventName (EventContentBlockDelta _ _) = "content_block_delta"
sseEventName (EventContentBlockStop _)    = "content_block_stop"
sseEventName (EventMessageDelta _ _ _)    = "message_delta"
sseEventName EventMessageStop             = "message_stop"
sseEventName EventPing                    = "ping"
sseEventName (EventError _)               = "error"
