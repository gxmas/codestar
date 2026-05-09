module Anthropic.Types.ContentSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Types
import Anthropic.Types.Generators ()

spec :: Spec
spec = do
  -- Content block types
  describe "TextBlock" $ do
    prop "roundtrip" $ \(x :: TextBlock) ->
      eitherDecode (encode x) === Right x

    it "injects type field" $ do
      let tb = TextBlock "hello" Nothing Nothing
          v  = toJSON tb
      lookupKey "type" v `shouldBe` Just "text"

  describe "Citation" $
    prop "roundtrip" $ \(x :: Citation) ->
      eitherDecode (encode x) === Right x

  describe "CitationConfig" $
    prop "roundtrip" $ \(x :: CitationConfig) ->
      eitherDecode (encode x) === Right x

  describe "MediaType" $ do
    prop "roundtrip" $ \(x :: MediaType) ->
      eitherDecode (encode x) === Right x

    it "ImageJpeg -> \"image/jpeg\"" $
      toJSON ImageJpeg `shouldBe` "image/jpeg"

  describe "ImageSource" $ do
    prop "roundtrip" $ \(x :: ImageSource) ->
      eitherDecode (encode x) === Right x

    it "Base64 wire format" $ do
      let src = Base64Image ImagePng "abc123"
          v   = toJSON src
      lookupKey "type" v `shouldBe` Just "base64"
      lookupKey "media_type" v `shouldBe` Just "image/png"

    it "URL wire format" $ do
      let src = UrlImage "https://example.com/img.png"
          v   = toJSON src
      lookupKey "type" v `shouldBe` Just "url"

  describe "ImageBlock" $
    prop "roundtrip" $ \(x :: ImageBlock) ->
      eitherDecode (encode x) === Right x

  describe "DocumentMediaType" $ do
    prop "roundtrip" $ \(x :: DocumentMediaType) ->
      eitherDecode (encode x) === Right x

    it "ApplicationPdf -> \"application/pdf\"" $
      toJSON ApplicationPdf `shouldBe` "application/pdf"

  describe "DocumentSource" $
    prop "roundtrip" $ \(x :: DocumentSource) ->
      eitherDecode (encode x) === Right x

  describe "DocumentBlock" $
    prop "roundtrip" $ \(x :: DocumentBlock) ->
      eitherDecode (encode x) === Right x

  describe "ToolUseBlock" $
    prop "roundtrip" $ \(x :: ToolUseBlock) ->
      eitherDecode (encode x) === Right x

  describe "ToolResultContent" $
    prop "roundtrip" $ \(x :: ToolResultContent) ->
      eitherDecode (encode x) === Right x

  describe "ToolResultBlock" $
    prop "roundtrip" $ \(x :: ToolResultBlock) ->
      eitherDecode (encode x) === Right x

  describe "ThinkingBlock" $
    prop "roundtrip" $ \(x :: ThinkingBlock) ->
      eitherDecode (encode x) === Right x

  describe "SearchResultBlock" $
    prop "roundtrip" $ \(x :: SearchResultBlock) ->
      eitherDecode (encode x) === Right x

  -- Union types
  describe "ContentBlock" $ do
    prop "roundtrip" $ \(x :: ContentBlock) ->
      eitherDecode (encode x) === Right x

    it "dispatches text type" $ do
      let json = "{\"type\":\"text\",\"text\":\"hello\"}"
      case decode json :: Maybe ContentBlock of
        Just (TextContent _) -> pure ()
        other -> expectationFailure $ "Expected TextContent, got: " ++ show other

    it "dispatches redacted_thinking" $ do
      let json = "{\"type\":\"redacted_thinking\"}"
      case decode json :: Maybe ContentBlock of
        Just (RedactedThinking Nothing) -> pure ()
        other -> expectationFailure $ "Expected RedactedThinking, got: " ++ show other

  describe "MessageContent" $ do
    prop "roundtrip" $ \(x :: MessageContent) ->
      eitherDecode (encode x) === Right x

    it "TextMessage is a JSON String" $
      toJSON (TextMessage "Hello") `shouldBe` Aeson.String "Hello"

    it "BlockMessage is a JSON Array" $ do
      let v = toJSON (BlockMessage [])
      case v of
        Aeson.Array _ -> pure ()
        _ -> expectationFailure $ "Expected Array, got: " ++ show v

  describe "SystemBlock" $
    prop "roundtrip" $ \(x :: SystemBlock) ->
      eitherDecode (encode x) === Right x

  describe "SystemPrompt" $ do
    prop "roundtrip" $ \(x :: SystemPrompt) ->
      eitherDecode (encode x) === Right x

    it "SimpleSystem is a JSON String" $
      toJSON (SimpleSystem "You are helpful") `shouldBe` Aeson.String "You are helpful"

    it "BlockSystem is a JSON Array" $ do
      let v = toJSON (BlockSystem [])
      case v of
        Aeson.Array _ -> pure ()
        _ -> expectationFailure $ "Expected Array, got: " ++ show v

-- Helper to extract a key from a JSON object
lookupKey :: Text -> Aeson.Value -> Maybe Aeson.Value
lookupKey k (Aeson.Object o) = KM.lookup (Key.fromText k) o
lookupKey _ _                = Nothing
