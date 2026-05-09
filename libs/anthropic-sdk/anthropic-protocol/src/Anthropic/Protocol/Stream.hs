-- | SSE streaming event types.
module Anthropic.Protocol.Stream
  ( -- * Stream Events
    StreamEvent (..)

    -- * Delta Types
  , Delta (..)

    -- * Stream State (opaque)
  , StreamState (..)
  , streamPhase
  , accumulatedBlocks
  , isComplete

    -- * Stream Phase
  , StreamPhase (..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:), (.:?)
  , object, withObject
  )
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Anthropic.Types

import Anthropic.Protocol.Message (MessageResponse)

-- | Incremental content update within a content block.
data Delta
  = TextDelta       !Text
  | InputJsonDelta  !Text
  | ThinkingDelta   !Text
  | SignatureDelta  !Text
  | CitationsDelta  !Citation
  deriving stock (Eq, Show, Generic)

instance ToJSON Delta where
  toJSON (TextDelta t) = object
    [ "type" .= ("text_delta" :: Text)
    , "text" .= t
    ]
  toJSON (InputJsonDelta t) = object
    [ "type"         .= ("input_json_delta" :: Text)
    , "partial_json" .= t
    ]
  toJSON (ThinkingDelta t) = object
    [ "type"     .= ("thinking_delta" :: Text)
    , "thinking" .= t
    ]
  toJSON (SignatureDelta t) = object
    [ "type"      .= ("signature_delta" :: Text)
    , "signature" .= t
    ]
  toJSON (CitationsDelta c) = object
    [ "type"     .= ("citations_delta" :: Text)
    , "citation" .= c
    ]

  toEncoding (TextDelta t) = E.pairs $
       "type" .= ("text_delta" :: Text)
    <> "text" .= t
  toEncoding (InputJsonDelta t) = E.pairs $
       "type"         .= ("input_json_delta" :: Text)
    <> "partial_json" .= t
  toEncoding (ThinkingDelta t) = E.pairs $
       "type"     .= ("thinking_delta" :: Text)
    <> "thinking" .= t
  toEncoding (SignatureDelta t) = E.pairs $
       "type"      .= ("signature_delta" :: Text)
    <> "signature" .= t
  toEncoding (CitationsDelta c) = E.pairs $
       "type"     .= ("citations_delta" :: Text)
    <> "citation" .= c

instance FromJSON Delta where
  parseJSON = withObject "Delta" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "text_delta"       -> TextDelta      <$> o .: "text"
      "input_json_delta" -> InputJsonDelta <$> o .: "partial_json"
      "thinking_delta"   -> ThinkingDelta  <$> o .: "thinking"
      "signature_delta"  -> SignatureDelta <$> o .: "signature"
      "citations_delta"  -> CitationsDelta <$> o .: "citation"
      _                  -> fail $ "Unknown Delta type: " ++ T.unpack typ

-- | SSE stream event.
--
-- Events arrive in strict order: @message_start@, then content blocks
-- (each with start/deltas/stop), then @message_delta@, then @message_stop@.
data StreamEvent
  = EventMessageStart      !MessageResponse
    -- ^ Opening event with initial (empty-content) message.
  | EventContentBlockStart !ContentBlockIndex !ContentBlock
    -- ^ Opens a new content block at the given index.
  | EventContentBlockDelta !ContentBlockIndex !Delta
    -- ^ Incremental update to the content block at the given index.
  | EventContentBlockStop  !ContentBlockIndex
    -- ^ Closes the content block at the given index.
  | EventMessageDelta      !(Maybe StopReason) !(Maybe Text) !Usage
    -- ^ Message-level update: stop_reason, stop_sequence, cumulative usage.
  | EventMessageStop
    -- ^ Final event; stream is complete.
  | EventPing
    -- ^ Keep-alive event. Ignore.
  | EventError             !ApiError
    -- ^ Error during streaming.
  deriving stock (Eq, Show, Generic)

instance ToJSON StreamEvent where
  toJSON (EventMessageStart msg) = object
    [ "type"    .= ("message_start" :: Text)
    , "message" .= msg
    ]
  toJSON (EventContentBlockStart idx block) = object
    [ "type"          .= ("content_block_start" :: Text)
    , "index"         .= idx
    , "content_block" .= block
    ]
  toJSON (EventContentBlockDelta idx delta) = object
    [ "type"  .= ("content_block_delta" :: Text)
    , "index" .= idx
    , "delta" .= delta
    ]
  toJSON (EventContentBlockStop idx) = object
    [ "type"  .= ("content_block_stop" :: Text)
    , "index" .= idx
    ]
  toJSON (EventMessageDelta sr ss usage) = object
    [ "type"  .= ("message_delta" :: Text)
    , "delta" .= object
        (  maybe [] (\r -> ["stop_reason"   .= r]) sr
        ++ maybe [] (\s -> ["stop_sequence" .= s]) ss
        )
    , "usage" .= usage
    ]
  toJSON EventMessageStop = object
    [ "type" .= ("message_stop" :: Text) ]
  toJSON EventPing = object
    [ "type" .= ("ping" :: Text) ]
  toJSON (EventError err) = object
    [ "type"  .= ("error" :: Text)
    , "error" .= err
    ]

  toEncoding (EventMessageStart msg) = E.pairs $
       "type"    .= ("message_start" :: Text)
    <> "message" .= msg
  toEncoding (EventContentBlockStart idx block) = E.pairs $
       "type"          .= ("content_block_start" :: Text)
    <> "index"         .= idx
    <> "content_block" .= block
  toEncoding (EventContentBlockDelta idx delta) = E.pairs $
       "type"  .= ("content_block_delta" :: Text)
    <> "index" .= idx
    <> "delta" .= delta
  toEncoding (EventContentBlockStop idx) = E.pairs $
       "type"  .= ("content_block_stop" :: Text)
    <> "index" .= idx
  toEncoding (EventMessageDelta sr ss usage) = E.pairs $
       "type"  .= ("message_delta" :: Text)
    <> "delta" .= object
        (  maybe [] (\r -> ["stop_reason"   .= r]) sr
        ++ maybe [] (\s -> ["stop_sequence" .= s]) ss
        )
    <> "usage" .= usage
  toEncoding EventMessageStop = E.pairs $
    "type" .= ("message_stop" :: Text)
  toEncoding EventPing = E.pairs $
    "type" .= ("ping" :: Text)
  toEncoding (EventError err) = E.pairs $
       "type"  .= ("error" :: Text)
    <> "error" .= err

instance FromJSON StreamEvent where
  parseJSON = withObject "StreamEvent" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "message_start" ->
        EventMessageStart <$> o .: "message"
      "content_block_start" ->
        EventContentBlockStart
          <$> o .: "index"
          <*> o .: "content_block"
      "content_block_delta" ->
        EventContentBlockDelta
          <$> o .: "index"
          <*> o .: "delta"
      "content_block_stop" ->
        EventContentBlockStop <$> o .: "index"
      "message_delta" -> do
        d <- o .: "delta"
        EventMessageDelta
          <$> d .:? "stop_reason"
          <*> d .:? "stop_sequence"
          <*> o .:  "usage"
      "message_stop" -> pure EventMessageStop
      "ping"         -> pure EventPing
      "error"        -> EventError <$> o .: "error"
      _              -> fail $ "Unknown StreamEvent type: " ++ T.unpack typ

-- | Phase of stream consumption. Used to validate event ordering.
data StreamPhase
  = AwaitingStart
  | InMessage
  | InBlock
  | BetweenBlocks
  | StreamDone
  deriving stock (Eq, Show, Generic)

-- | Opaque stream state for event accumulation.
--
-- Constructors are not exported. Use 'streamPhase', 'accumulatedBlocks',
-- and 'isComplete' to inspect.
data StreamState = StreamState
  { _phase             :: !StreamPhase
  , _currentBlockIndex :: !(Maybe ContentBlockIndex)
  , _accumulatedBlocks :: ![ContentBlock]
  , _accumulatedJson   :: !(Maybe Text)
  }

-- | Current phase of the stream.
streamPhase :: StreamState -> StreamPhase
streamPhase (StreamState p _ _ _) = p

-- | Content blocks accumulated so far.
accumulatedBlocks :: StreamState -> [ContentBlock]
accumulatedBlocks (StreamState _ _ bs _) = bs

-- | Whether the stream has completed (received @message_stop@).
isComplete :: StreamState -> Bool
isComplete (StreamState p _ _ _) = p == StreamDone
