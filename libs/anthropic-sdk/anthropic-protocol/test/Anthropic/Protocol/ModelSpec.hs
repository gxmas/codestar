module Anthropic.Protocol.ModelSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Protocol.Model
import Anthropic.Protocol.Generators ()

lookupKey :: Text -> Aeson.Value -> Maybe Aeson.Value
lookupKey k (Aeson.Object o) = KM.lookup (Key.fromText k) o
lookupKey _ _                = Nothing

spec :: Spec
spec = do
  describe "ModelInfo" $ do
    prop "roundtrip" $ \(x :: ModelInfo) ->
      eitherDecode (encode x) === Right x

    it "injects type=model" $ do
      let mi = ModelInfo "claude-sonnet-4-20250514" "Claude Sonnet" "2024-01-01" Nothing Nothing Nothing
          v = toJSON mi
      lookupKey "type" v `shouldBe` Just "model"

    it "uses id (not model_id) in wire format" $ do
      let mi = ModelInfo "claude-sonnet-4-20250514" "Claude" "2024-01-01" Nothing Nothing Nothing
          v = toJSON mi
      lookupKey "id" v `shouldBe` Just (Aeson.String "claude-sonnet-4-20250514")
      lookupKey "model_id" v `shouldBe` Nothing

    it "validates type in FromJSON" $ do
      let json = "{\"type\":\"wrong\",\"id\":\"m\",\"display_name\":\"M\",\"created_at\":\"t\"}"
      (decode json :: Maybe ModelInfo) `shouldBe` Nothing

    it "parses valid response" $ do
      let json = "{\"type\":\"model\",\"id\":\"claude-sonnet-4-20250514\",\"display_name\":\"Claude\",\"created_at\":\"2024-01-01\"}"
      case decode json :: Maybe ModelInfo of
        Just mi -> mi.displayName `shouldBe` "Claude"
        Nothing -> expectationFailure "Failed to parse ModelInfo"

    it "omits optional fields when Nothing" $ do
      let mi = ModelInfo "m" "M" "t" Nothing Nothing Nothing
          v = toJSON mi
      lookupKey "max_input_tokens" v `shouldBe` Nothing
      lookupKey "max_tokens" v `shouldBe` Nothing
      lookupKey "capabilities" v `shouldBe` Nothing
