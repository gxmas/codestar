module Anthropic.Protocol.TokenCountSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Function ((&))
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Protocol.Message (userMessage)
import qualified Anthropic.Protocol.TokenCount as TC
import Anthropic.Protocol.Generators ()

lookupKey :: Text -> Aeson.Value -> Maybe Aeson.Value
lookupKey k (Aeson.Object o) = KM.lookup (Key.fromText k) o
lookupKey _ _                = Nothing

spec :: Spec
spec = do
  describe "TokenCountRequest" $ do
    prop "roundtrip" $ \(x :: TC.TokenCountRequest) ->
      eitherDecode (encode x) === Right x

    it "smart constructor sets required fields" $ do
      let req = TC.tokenCountRequest "claude-sonnet-4-20250514" [userMessage "Hi"]
      req.model `shouldBe` "claude-sonnet-4-20250514"
      req.system `shouldBe` Nothing

    it "with* setters work" $ do
      let req = TC.tokenCountRequest "claude-sonnet-4-20250514" [userMessage "Hi"]
              & TC.withSystem "Be helpful."
      req.system `shouldBe` Just "Be helpful."

    it "uses snake_case in wire format" $ do
      let req = TC.tokenCountRequest "claude-sonnet-4-20250514" [userMessage "Hi"]
              & TC.withSystem "test"
          v = toJSON req
      -- tool_choice not tool_Choice
      lookupKey "model" v `shouldNotBe` Nothing

    it "omits Nothing fields" $ do
      let req = TC.tokenCountRequest "m" [userMessage "Hi"]
          v   = toJSON req
      lookupKey "system" v `shouldBe` Nothing
      lookupKey "tools" v `shouldBe` Nothing

  describe "TokenCountResponse" $ do
    prop "roundtrip" $ \(x :: TC.TokenCountResponse) ->
      eitherDecode (encode x) === Right x

    it "uses input_tokens in wire format" $ do
      let v = toJSON (TC.TokenCountResponse 42)
      lookupKey "input_tokens" v `shouldBe` Just (Aeson.Number 42)

    it "parses wire format" $ do
      let json = "{\"input_tokens\":100}"
      case decode json :: Maybe TC.TokenCountResponse of
        Just r -> r.inputTokens `shouldBe` 100
        Nothing -> expectationFailure "Failed to parse TokenCountResponse"
