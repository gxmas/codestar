module Anthropic.Protocol.ThinkingSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Protocol.Thinking
import Anthropic.Protocol.Generators ()

lookupKey :: Text -> Aeson.Value -> Maybe Aeson.Value
lookupKey k (Aeson.Object o) = KM.lookup (Key.fromText k) o
lookupKey _ _                = Nothing

spec :: Spec
spec = do
  describe "ThinkingDisplay" $ do
    prop "roundtrip" $ \(x :: ThinkingDisplay) ->
      eitherDecode (encode x) === Right x

    it "DisplaySummarized -> \"summarized\"" $
      toJSON DisplaySummarized `shouldBe` "summarized"

    it "DisplayOmitted -> \"omitted\"" $
      toJSON DisplayOmitted `shouldBe` "omitted"

  describe "ThinkingConfig" $ do
    prop "roundtrip" $ \(x :: ThinkingConfig) ->
      eitherDecode (encode x) === Right x

    it "ThinkingDisabled wire format" $ do
      let v = toJSON ThinkingDisabled
      lookupKey "type" v `shouldBe` Just "disabled"

    it "ThinkingEnabled wire format" $ do
      let v = toJSON (ThinkingEnabled 2048 Nothing)
      lookupKey "type" v `shouldBe` Just "enabled"
      lookupKey "budget_tokens" v `shouldBe` Just (Aeson.Number 2048)

    it "ThinkingEnabled with display" $ do
      let v = toJSON (ThinkingEnabled 2048 (Just DisplayOmitted))
      lookupKey "display" v `shouldBe` Just "omitted"

    it "ThinkingEnabled omits display when Nothing" $ do
      let v = toJSON (ThinkingEnabled 2048 Nothing)
      lookupKey "display" v `shouldBe` Nothing

    it "ThinkingAdaptive wire format" $ do
      let v = toJSON (ThinkingAdaptive Nothing)
      lookupKey "type" v `shouldBe` Just "adaptive"

    it "ThinkingAdaptive with display" $ do
      let v = toJSON (ThinkingAdaptive (Just DisplaySummarized))
      lookupKey "display" v `shouldBe` Just "summarized"

  describe "enableThinking" $ do
    it "rejects budget < 1024" $
      enableThinking 512 `shouldBe` Left "budget_tokens must be >= 1024"

    it "accepts budget >= 1024" $
      enableThinking 1024 `shouldBe` Right (ThinkingEnabled 1024 Nothing)

    it "accepts large budget" $
      enableThinking 100000 `shouldBe` Right (ThinkingEnabled 100000 Nothing)
