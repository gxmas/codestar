{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | QuickCheck generators for all anthropic-protocol types.
module Anthropic.Protocol.Generators () where

import Data.Aeson (object, (.=))
import Data.JsonSchema (Schema, stringSchema, integerSchema, objectSchema, required)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Anthropic.Types
import Anthropic.Protocol.Thinking
import Anthropic.Protocol.Tool
import Anthropic.Protocol.Message
import Anthropic.Protocol.Batch
import Anthropic.Protocol.Model
import Anthropic.Protocol.TokenCount
import Anthropic.Protocol.Stream
import Anthropic.Protocol.Stream.Accumulator (OrderingViolation(..))

-- Helpers

instance Arbitrary Text where
  arbitrary = shortText

shortText :: Gen Text
shortText = T.pack <$> listOf1 (elements ['a'..'z'])

posInt :: Gen Int
posInt = abs <$> arbitrary

-- anthropic-types instances needed by protocol generators

instance Arbitrary ModelId where
  arbitrary = ModelId <$> shortText

instance Arbitrary MessageId where
  arbitrary = MessageId <$> shortText

instance Arbitrary ContentBlockIndex where
  arbitrary = ContentBlockIndex . abs <$> arbitrary

instance Arbitrary BatchId where
  arbitrary = BatchId <$> shortText

instance Arbitrary Role where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary StopReason where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ServiceTierPreference where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ErrorType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ApiError where
  arbitrary = ApiError <$> arbitrary <*> shortText

instance Arbitrary CacheTTL where
  arbitrary = elements [TTL5Min, TTL1Hour]

instance Arbitrary CacheControl where
  arbitrary = CacheControl <$> arbitrary

instance Arbitrary ServerToolUsage where
  arbitrary = ServerToolUsage <$> posInt <*> posInt

instance Arbitrary CacheCreationUsage where
  arbitrary = CacheCreationUsage <$> posInt <*> posInt

instance Arbitrary Usage where
  arbitrary = Usage
    <$> posInt <*> posInt
    <*> liftArbitrary (fmap abs arbitrary)
    <*> liftArbitrary (fmap abs arbitrary)
    <*> arbitrary <*> arbitrary
    <*> liftArbitrary shortText
    <*> liftArbitrary shortText

instance Arbitrary CitationConfig where
  arbitrary = CitationConfig <$> arbitrary

instance Arbitrary Citation where
  arbitrary = oneof
    [ CharLocationCitation <$> shortText <*> posInt <*> shortText <*> posInt <*> posInt <*> liftArbitrary shortText
    , PageLocationCitation <$> shortText <*> posInt <*> shortText <*> posInt <*> posInt <*> liftArbitrary shortText
    , ContentBlockLocationCitation <$> shortText <*> posInt <*> shortText <*> posInt <*> posInt <*> liftArbitrary shortText
    , WebSearchResultLocationCitation <$> shortText <*> shortText <*> shortText <*> shortText
    , SearchResultLocationCitation <$> shortText <*> posInt <*> shortText <*> shortText <*> posInt <*> posInt
    ]

instance Arbitrary TextBlock where
  arbitrary = TextBlock <$> shortText <*> liftArbitrary (listOf1 arbitrary) <*> arbitrary

instance Arbitrary MediaType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ImageSource where
  arbitrary = oneof
    [ Base64Image <$> arbitrary <*> shortText
    , UrlImage <$> shortText
    ]

instance Arbitrary ImageBlock where
  arbitrary = ImageBlock <$> arbitrary <*> arbitrary

instance Arbitrary DocumentMediaType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DocumentSource where
  arbitrary = oneof
    [ Base64Document <$> arbitrary <*> shortText
    , UrlDocument <$> shortText
    , ContentDocument <$> shortText
    ]

instance Arbitrary DocumentBlock where
  arbitrary = DocumentBlock
    <$> arbitrary <*> liftArbitrary shortText <*> liftArbitrary shortText
    <*> arbitrary <*> arbitrary

instance Arbitrary ToolUseBlock where
  arbitrary = ToolUseBlock
    <$> shortText <*> shortText
    <*> pure (object ["key" .= ("val" :: Text)])
    <*> arbitrary

instance Arbitrary ToolResultContent where
  arbitrary = oneof
    [ ToolResultText <$> shortText
    , pure (ToolResultBlocks [object ["type" .= ("text" :: Text), "text" .= ("hi" :: Text)]])
    ]

instance Arbitrary ToolResultBlock where
  arbitrary = ToolResultBlock <$> shortText <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ThinkingBlock where
  arbitrary = ThinkingBlock <$> shortText <*> shortText

instance Arbitrary SearchResultBlock where
  arbitrary = SearchResultBlock
    <$> shortText <*> shortText <*> listOf1 arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ContentBlock where
  arbitrary = oneof
    [ TextContent <$> arbitrary
    , ImageContent <$> arbitrary
    , ToolUseContent <$> arbitrary
    , ToolResultContent <$> arbitrary
    , ThinkingContent <$> arbitrary
    , RedactedThinking <$> liftArbitrary shortText
    , DocumentContent <$> arbitrary
    , SearchResultContent <$> arbitrary
    ]

instance Arbitrary MessageContent where
  arbitrary = oneof
    [ TextMessage <$> shortText
    , BlockMessage <$> listOf1 arbitrary
    ]

instance Arbitrary SystemBlock where
  arbitrary = SystemBlock <$> shortText <*> arbitrary

instance Arbitrary SystemPrompt where
  arbitrary = oneof
    [ SimpleSystem <$> shortText
    , BlockSystem <$> listOf1 arbitrary
    ]

-- Thinking

instance Arbitrary ThinkingDisplay where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ThinkingConfig where
  arbitrary = oneof
    [ ThinkingEnabled <$> ((\n -> max 1024 (abs n)) <$> arbitrary) <*> arbitrary
    , pure ThinkingDisabled
    , ThinkingAdaptive <$> arbitrary
    ]

-- Tool

instance Arbitrary DisableParallel where
  arbitrary = DisableParallel <$> arbitrary

instance Arbitrary ToolChoice where
  arbitrary = oneof
    [ ToolAuto <$> arbitrary
    , ToolAny <$> arbitrary
    , ToolSpecific <$> shortText <*> arbitrary
    , pure ToolNone
    ]

instance Arbitrary ServerToolType where
  arbitrary = oneof
    [ pure WebSearch
    , pure WebFetch
    , pure CodeExecution
    , pure BashTool
    , pure TextEditor
    , pure MemoryTool
    , pure ToolSearchBM25
    , pure ToolSearchRegex
    , OtherServerTool <$> elements ["file_search", "custom_tool_99", "new_tool"]
    ]

instance Arbitrary ServerToolDef where
  arbitrary = ServerToolDef
    <$> arbitrary
    <*> shortText
    <*> arbitrary
    <*> liftArbitrary (pure (object ["max_uses" .= (5 :: Int)]))

instance Arbitrary CustomToolDef where
  arbitrary = CustomToolDef
    <$> shortText
    <*> liftArbitrary shortText
    <*> genSimpleSchema
    <*> arbitrary

instance Arbitrary ToolDefinition where
  arbitrary = oneof
    [ CustomTool <$> arbitrary
    , ServerTool <$> arbitrary
    ]

-- | Generate a simple but valid Schema for testing.
genSimpleSchema :: Gen Schema
genSimpleSchema = elements
  [ stringSchema
  , integerSchema
  , objectSchema
      [ required "name" stringSchema
      , required "value" integerSchema
      ]
  ]

-- Message

instance Arbitrary RequestMetadata where
  arbitrary = RequestMetadata <$> liftArbitrary shortText

instance Arbitrary Message where
  arbitrary = Message <$> arbitrary <*> arbitrary

instance Arbitrary Container where
  arbitrary = Container <$> shortText <*> shortText

instance Arbitrary MessageRequest where
  arbitrary = MessageRequest
    <$> arbitrary         -- model
    <*> listOf1 arbitrary -- messages
    <*> posInt            -- maxTokens
    <*> arbitrary         -- system
    <*> liftArbitrary (listOf1 shortText) -- stopSequences
    <*> liftArbitrary (choose (0.0, 2.0)) -- temperature
    <*> liftArbitrary (choose (0.0, 1.0)) -- topP
    <*> liftArbitrary posInt              -- topK
    <*> liftArbitrary (listOf1 arbitrary) -- tools
    <*> arbitrary         -- toolChoice
    <*> arbitrary         -- thinking
    <*> arbitrary         -- stream
    <*> arbitrary         -- metadata
    <*> arbitrary         -- serviceTier
    <*> liftArbitrary shortText -- container
    <*> arbitrary         -- cacheControl

instance Arbitrary MessageResponse where
  arbitrary = MessageResponse
    <$> arbitrary         -- id
    <*> arbitrary         -- model
    <*> listOf arbitrary  -- content
    <*> arbitrary         -- stopReason
    <*> liftArbitrary shortText -- stopSequence
    <*> arbitrary         -- usage
    <*> arbitrary         -- container

-- Batch

instance Arbitrary BatchStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary BatchCounts where
  arbitrary = BatchCounts
    <$> posInt <*> posInt <*> posInt <*> posInt <*> posInt

instance Arbitrary BatchItem where
  arbitrary = BatchItem <$> shortText <*> arbitrary

instance Arbitrary BatchResponse where
  arbitrary = BatchResponse
    <$> shortText   -- batchId
    <*> arbitrary   -- processingStatus
    <*> arbitrary   -- requestCounts
    <*> shortText   -- createdAt
    <*> liftArbitrary shortText -- endedAt
    <*> shortText   -- expiresAt
    <*> liftArbitrary shortText -- archivedAt
    <*> liftArbitrary shortText -- cancelInitiatedAt
    <*> liftArbitrary shortText -- resultsUrl

instance Arbitrary BatchResult where
  arbitrary = oneof
    [ BatchSucceeded <$> arbitrary
    , BatchErrored <$> arbitrary
    , pure BatchCanceled
    , pure BatchExpired
    ]

instance Arbitrary BatchResultItem where
  arbitrary = BatchResultItem <$> shortText <*> arbitrary

instance Arbitrary DeletedBatch where
  arbitrary = DeletedBatch <$> shortText

-- Model

instance Arbitrary ModelInfo where
  arbitrary = ModelInfo
    <$> arbitrary              -- modelId
    <*> shortText              -- displayName
    <*> shortText              -- createdAt
    <*> liftArbitrary posInt   -- maxInputTokens
    <*> liftArbitrary posInt   -- maxTokens
    <*> liftArbitrary (pure (object ["batch" .= True])) -- capabilities

-- TokenCount

instance Arbitrary TokenCountRequest where
  arbitrary = TokenCountRequest
    <$> arbitrary
    <*> listOf1 arbitrary
    <*> arbitrary
    <*> liftArbitrary (listOf1 arbitrary)
    <*> arbitrary
    <*> arbitrary

instance Arbitrary TokenCountResponse where
  arbitrary = TokenCountResponse <$> posInt

-- Stream

instance Arbitrary Delta where
  arbitrary = oneof
    [ TextDelta <$> shortText
    , InputJsonDelta <$> shortText
    , ThinkingDelta <$> shortText
    , SignatureDelta <$> shortText
    , CitationsDelta <$> arbitrary
    ]

instance Arbitrary StreamEvent where
  arbitrary = oneof
    [ EventMessageStart <$> arbitrary
    , EventContentBlockStart <$> arbitrary <*> arbitrary
    , EventContentBlockDelta <$> arbitrary <*> arbitrary
    , EventContentBlockStop <$> arbitrary
    , EventMessageDelta <$> arbitrary <*> liftArbitrary shortText <*> arbitrary
    , pure EventMessageStop
    , pure EventPing
    , EventError <$> arbitrary
    ]

instance Arbitrary StreamPhase where
  arbitrary = elements
    [AwaitingStart, InMessage, InBlock, BetweenBlocks, StreamDone]

instance Arbitrary OrderingViolation where
  arbitrary = OrderingViolation <$> shortText <*> shortText <*> shortText
