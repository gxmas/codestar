{-# LANGUAGE ScopedTypeVariables #-}

-- | SSE line parsing: bytes to stream events.
module Anthropic.Protocol.Stream.Parser
  ( -- * Parsing
    parseSseStream
  ) where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Streaming (Stream, Of)
import qualified Streaming as S
import qualified Streaming.Prelude as SP

import Anthropic.Protocol.Stream (StreamEvent(..))
import Anthropic.Protocol.Message (MessageResponse)

-- | Parse a stream of raw bytes (from an HTTP response body) into
-- a stream of 'StreamEvent' values, terminating with a 'MessageResponse'.
--
-- The input action produces successive chunks of the SSE response body.
-- The output stream yields individual events as they are parsed from
-- @event:@/@data:@ lines separated by blank lines.
--
-- SSE format:
--
-- @
-- event: message_start
-- data: {"type":"message_start","message":{...}}
--
-- event: content_block_start
-- data: {"type":"content_block_start",...}
--
-- @
--
-- Each event block is delimited by a blank line. The @event:@ line names
-- the event type (used for logging/diagnostics but not for parsing — the
-- JSON payload's @type@ field is authoritative). The @data:@ line(s)
-- contain the JSON payload.
parseSseStream :: forall m. (Monad m) => m ByteString -> Stream (Of StreamEvent) m MessageResponse
parseSseStream readChunk = go mempty Nothing
  where
    go :: ByteString -> Maybe MessageResponse -> Stream (Of StreamEvent) m MessageResponse
    go buffer startMsg = do
      chunk <- S.lift readChunk
      if BS.null chunk
        then
          case startMsg of
            Just msg -> pure msg
            Nothing  -> error "parseSseStream: stream ended without message_start"
        else
          processBuffer (buffer <> chunk) startMsg

    processBuffer :: ByteString -> Maybe MessageResponse -> Stream (Of StreamEvent) m MessageResponse
    processBuffer buffer startMsg =
      case splitOnDoubleNewline buffer of
        Nothing -> go buffer startMsg
        Just (block, rest) ->
          let dataPayload = extractData block
          in case parseEventData dataPayload of
            Nothing -> processBuffer rest startMsg
            Just evt -> do
              SP.yield evt
              let startMsg' = case evt of
                    EventMessageStart msg -> Just msg
                    _                     -> startMsg
              case evt of
                EventMessageStop ->
                  case startMsg' of
                    Just msg -> pure msg
                    Nothing  -> error "parseSseStream: message_stop before message_start"
                _ -> processBuffer rest startMsg'

    -- Split buffer on "\n\n"
    splitOnDoubleNewline :: ByteString -> Maybe (ByteString, ByteString)
    splitOnDoubleNewline bs =
      case BS8.breakSubstring "\n\n" bs of
        (_, rest) | BS.null rest -> Nothing
        (before, rest)           -> Just (before, BS.drop 2 rest)

    -- Extract data payload from an SSE block
    extractData :: ByteString -> ByteString
    extractData block =
      let ls = BS8.lines block
          dataLines = filter (BS8.isPrefixOf "data: ") ls
      in BS.intercalate "\n" (map (BS.drop 6) dataLines)

    -- Parse JSON data payload into a StreamEvent
    parseEventData :: ByteString -> Maybe StreamEvent
    parseEventData bs
      | BS.null bs = Nothing
      | otherwise  = Aeson.decodeStrict' bs
