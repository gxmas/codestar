{-# LANGUAGE RecordWildCards #-}

-- | Stream event accumulation and ordering validation.
module Anthropic.Protocol.Stream.Accumulator
  ( -- * Accumulation
    accumulate
  , initialStreamState

    -- * Validation
  , OrderingViolation (..)
  , validateOrdering
  ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Anthropic.Types
import Anthropic.Protocol.Stream

-- | Description of an SSE event ordering violation.
data OrderingViolation = OrderingViolation
  { expectedPhase :: !Text
  , receivedEvent :: !Text
  , description   :: !Text
  }
  deriving stock (Eq, Show)

-- | Initial stream state before any events have been processed.
initialStreamState :: StreamState
initialStreamState = StreamState AwaitingStart Nothing [] Nothing

-- | Accumulate a stream event into the stream state.
--
-- Returns the updated state with content blocks tracked,
-- JSON fragments accumulated for tool input, and phase transitions applied.
accumulate :: StreamState -> StreamEvent -> StreamState
accumulate (StreamState phase idx blocks jsonAcc) event = case event of
  EventMessageStart _ ->
    StreamState InMessage Nothing [] Nothing

  EventContentBlockStart blockIdx block ->
    StreamState InBlock (Just blockIdx) (blocks ++ [block]) Nothing

  EventContentBlockDelta _ delta ->
    case delta of
      TextDelta t ->
        StreamState InBlock idx (updateLastBlock (appendText t) blocks) jsonAcc
      InputJsonDelta t ->
        StreamState InBlock idx blocks (Just (maybe t (<> t) jsonAcc))
      ThinkingDelta t ->
        StreamState InBlock idx (updateLastBlock (appendThinking t) blocks) jsonAcc
      SignatureDelta _ ->
        -- Signature deltas are for redacted thinking; state unchanged
        StreamState InBlock idx blocks jsonAcc
      CitationsDelta _ ->
        -- Citations are appended to the current text block
        StreamState InBlock idx blocks jsonAcc

  EventContentBlockStop _ ->
    -- If we accumulated JSON fragments, finalize the tool use block
    let blocks' = case jsonAcc of
          Just json -> updateLastBlock (finalizeToolInput json) blocks
          Nothing   -> blocks
    in StreamState BetweenBlocks Nothing blocks' Nothing

  EventMessageDelta _ _ _ ->
    StreamState BetweenBlocks Nothing blocks Nothing

  EventMessageStop ->
    StreamState StreamDone Nothing blocks Nothing

  EventPing ->
    StreamState phase idx blocks jsonAcc

  EventError _ ->
    StreamState phase idx blocks jsonAcc

-- | Validate that a stream event is valid for the current stream state.
--
-- Returns 'Nothing' if the event is valid, or 'Just' with a description
-- of the ordering violation.
validateOrdering :: StreamState -> StreamEvent -> Maybe OrderingViolation
validateOrdering (StreamState phase _ _ _) event = case (phase, event) of
  -- AwaitingStart: only message_start or ping allowed
  (AwaitingStart, EventMessageStart _) -> Nothing
  (AwaitingStart, EventPing)           -> Nothing
  (AwaitingStart, EventError _)        -> Nothing
  (AwaitingStart, _)                   -> Just $ OrderingViolation
    "AwaitingStart" (eventName event)
    "Expected message_start, ping, or error"

  -- InMessage: content_block_start, message_delta, or ping
  (InMessage, EventContentBlockStart _ _) -> Nothing
  (InMessage, EventMessageDelta _ _ _)    -> Nothing
  (InMessage, EventPing)                  -> Nothing
  (InMessage, EventError _)               -> Nothing
  (InMessage, _)                          -> Just $ OrderingViolation
    "InMessage" (eventName event)
    "Expected content_block_start, message_delta, ping, or error"

  -- InBlock: content_block_delta, content_block_stop, or ping
  (InBlock, EventContentBlockDelta _ _) -> Nothing
  (InBlock, EventContentBlockStop _)    -> Nothing
  (InBlock, EventPing)                  -> Nothing
  (InBlock, EventError _)               -> Nothing
  (InBlock, _)                          -> Just $ OrderingViolation
    "InBlock" (eventName event)
    "Expected content_block_delta, content_block_stop, ping, or error"

  -- BetweenBlocks: content_block_start, message_delta, message_stop, or ping
  (BetweenBlocks, EventContentBlockStart _ _) -> Nothing
  (BetweenBlocks, EventMessageDelta _ _ _)    -> Nothing
  (BetweenBlocks, EventMessageStop)           -> Nothing
  (BetweenBlocks, EventPing)                  -> Nothing
  (BetweenBlocks, EventError _)               -> Nothing
  (BetweenBlocks, _)                          -> Just $ OrderingViolation
    "BetweenBlocks" (eventName event)
    "Expected content_block_start, message_delta, message_stop, ping, or error"

  -- StreamDone: no more events expected
  (StreamDone, EventPing)  -> Nothing
  (StreamDone, _)          -> Just $ OrderingViolation
    "StreamDone" (eventName event)
    "Stream is complete; no more events expected"

-- | Human-readable name for a stream event.
eventName :: StreamEvent -> Text
eventName (EventMessageStart _)        = "message_start"
eventName (EventContentBlockStart _ _) = "content_block_start"
eventName (EventContentBlockDelta _ _) = "content_block_delta"
eventName (EventContentBlockStop _)    = "content_block_stop"
eventName (EventMessageDelta _ _ _)    = "message_delta"
eventName EventMessageStop             = "message_stop"
eventName EventPing                    = "ping"
eventName (EventError _)               = "error"

-- Helpers for updating content blocks during accumulation

-- | Update the last element of a list.
updateLastBlock :: (ContentBlock -> ContentBlock) -> [ContentBlock] -> [ContentBlock]
updateLastBlock _ []     = []
updateLastBlock f [x]    = [f x]
updateLastBlock f (x:xs) = x : updateLastBlock f xs

-- | Append text to the last text block.
appendText :: Text -> ContentBlock -> ContentBlock
appendText t (TextContent tb) =
  let TextBlock{..} = tb in TextContent TextBlock{text = tb.text <> t, ..}
appendText _ block = block

-- | Append thinking text to the last thinking block.
appendThinking :: Text -> ContentBlock -> ContentBlock
appendThinking t (ThinkingContent tb) =
  let ThinkingBlock{..} = tb in ThinkingContent ThinkingBlock{thinking = tb.thinking <> t, ..}
appendThinking _ block = block

-- | Finalize tool input by parsing accumulated JSON into the block's input field.
finalizeToolInput :: Text -> ContentBlock -> ContentBlock
finalizeToolInput json (ToolUseContent tb) =
  case Aeson.decodeStrict (encodeUtf8 json) of
    Just v  -> let ToolUseBlock{id = toolId, ..} = tb in ToolUseContent ToolUseBlock{id = toolId, input = v, ..}
    Nothing -> ToolUseContent tb
finalizeToolInput _ block = block
