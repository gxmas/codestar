{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | QuickCheck generators for all anthropic-types types.
module Anthropic.Types.Generators () where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Anthropic.Types

-- Helpers

instance Arbitrary Text where
  arbitrary = shortText

shortText :: Gen Text
shortText = T.pack <$> listOf1 (elements ['a'..'z'])

-- Core

instance Arbitrary ModelId where
  arbitrary = ModelId <$> shortText

instance Arbitrary MessageId where
  arbitrary = MessageId <$> shortText

instance Arbitrary ContentBlockIndex where
  arbitrary = ContentBlockIndex . abs <$> arbitrary

instance Arbitrary BatchId where
  arbitrary = BatchId <$> shortText

instance Arbitrary RequestId where
  arbitrary = RequestId <$> shortText

instance Arbitrary Role where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary StopReason where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ServiceTierPreference where
  arbitrary = arbitraryBoundedEnum

-- Error

instance Arbitrary ErrorType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ApiError where
  arbitrary = ApiError <$> arbitrary <*> shortText

-- Cache

instance Arbitrary CacheTTL where
  arbitrary = elements [TTL5Min, TTL1Hour]

instance Arbitrary CacheControl where
  arbitrary = CacheControl <$> arbitrary

-- Usage

instance Arbitrary ServerToolUsage where
  arbitrary = ServerToolUsage <$> abs' <*> abs'
    where abs' = abs <$> arbitrary

instance Arbitrary CacheCreationUsage where
  arbitrary = CacheCreationUsage <$> abs' <*> abs'
    where abs' = abs <$> arbitrary

instance Arbitrary Usage where
  arbitrary = Usage
    <$> abs' <*> abs'
    <*> fmap (fmap abs) arbitrary
    <*> fmap (fmap abs) arbitrary
    <*> arbitrary <*> arbitrary
    <*> liftArbitrary shortText
    <*> liftArbitrary shortText
    where abs' = abs <$> arbitrary

-- Pagination

instance Arbitrary a => Arbitrary (Page a) where
  arbitrary = Page
    <$> listOf arbitrary
    <*> arbitrary
    <*> liftArbitrary shortText
    <*> liftArbitrary shortText

-- Content blocks

instance Arbitrary CitationConfig where
  arbitrary = CitationConfig <$> arbitrary

instance Arbitrary Citation where
  arbitrary = oneof
    [ CharLocationCitation <$> shortText <*> abs' <*> shortText <*> abs' <*> abs' <*> liftArbitrary shortText
    , PageLocationCitation <$> shortText <*> abs' <*> shortText <*> abs' <*> abs' <*> liftArbitrary shortText
    , ContentBlockLocationCitation <$> shortText <*> abs' <*> shortText <*> abs' <*> abs' <*> liftArbitrary shortText
    , WebSearchResultLocationCitation <$> shortText <*> shortText <*> shortText <*> shortText
    , SearchResultLocationCitation <$> shortText <*> abs' <*> shortText <*> shortText <*> abs' <*> abs'
    ]
    where abs' = abs <$> arbitrary

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
    <$> arbitrary
    <*> liftArbitrary shortText
    <*> liftArbitrary shortText
    <*> arbitrary
    <*> arbitrary

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
    <$> shortText <*> shortText
    <*> listOf1 arbitrary
    <*> arbitrary <*> arbitrary

-- Content union types

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
