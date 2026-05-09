module Anthropic.Types.CoreSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON)
import Data.String ()
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Types
import Anthropic.Types.Generators ()

spec :: Spec
spec = do
  describe "Role" $ do
    prop "roundtrip" $ \(x :: Role) ->
      eitherDecode (encode x) === Right x

    it "User -> \"user\"" $
      toJSON User `shouldBe` "user"

    it "Assistant -> \"assistant\"" $
      toJSON Assistant `shouldBe` "assistant"

  describe "StopReason" $ do
    prop "roundtrip" $ \(x :: StopReason) ->
      eitherDecode (encode x) === Right x

    it "EndTurn -> \"end_turn\"" $
      toJSON EndTurn `shouldBe` "end_turn"

    it "MaxTokens -> \"max_tokens\"" $
      toJSON MaxTokens `shouldBe` "max_tokens"

    it "PauseTurn -> \"pause_turn\"" $
      toJSON PauseTurn `shouldBe` "pause_turn"

  describe "ServiceTierPreference" $ do
    prop "roundtrip" $ \(x :: ServiceTierPreference) ->
      eitherDecode (encode x) === Right x

    it "ServiceTierAuto -> \"auto\"" $
      toJSON ServiceTierAuto `shouldBe` "auto"

    it "StandardOnly -> \"standard\"" $
      toJSON StandardOnly `shouldBe` "standard"

  describe "IsString instances" $ do
    it "ModelId" $
      ("claude-sonnet-4-20250514" :: ModelId) `shouldBe` ModelId "claude-sonnet-4-20250514"

    it "MessageContent" $
      ("Hello" :: MessageContent) `shouldBe` TextMessage "Hello"

    it "SystemPrompt" $
      ("Be helpful" :: SystemPrompt) `shouldBe` SimpleSystem "Be helpful"
