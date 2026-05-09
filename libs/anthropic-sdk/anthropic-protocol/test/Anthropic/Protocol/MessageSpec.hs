module Anthropic.Protocol.MessageSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Function ((&))
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Types
import Anthropic.Protocol.Message
import Anthropic.Protocol.Generators ()

lookupKey :: Text -> Aeson.Value -> Maybe Aeson.Value
lookupKey k (Aeson.Object o) = KM.lookup (Key.fromText k) o
lookupKey _ _                = Nothing

spec :: Spec
spec = do
  describe "RequestMetadata" $
    prop "roundtrip" $ \(x :: RequestMetadata) ->
      eitherDecode (encode x) === Right x

  describe "Message" $ do
    prop "roundtrip" $ \(x :: Message) ->
      eitherDecode (encode x) === Right x

    it "userMessage sets role to user" $ do
      let v = toJSON (userMessage "Hello")
      lookupKey "role" v `shouldBe` Just "user"

    it "assistantMessage sets role to assistant" $ do
      let v = toJSON (assistantMessage "Hi")
      lookupKey "role" v `shouldBe` Just "assistant"

  describe "Container" $
    prop "roundtrip" $ \(x :: Container) ->
      eitherDecode (encode x) === Right x

  describe "MessageRequest" $ do
    prop "roundtrip" $ \(x :: MessageRequest) ->
      eitherDecode (encode x) === Right x

    it "smart constructor sets required fields" $ do
      let req = messageRequest "claude-sonnet-4-20250514" [userMessage "Hello"] 1024
      req.model `shouldBe` "claude-sonnet-4-20250514"
      req.maxTokens `shouldBe` 1024
      req.system `shouldBe` Nothing
      req.temperature `shouldBe` Nothing

    it "with* setters work with &" $ do
      let req = messageRequest "claude-sonnet-4-20250514" [userMessage "Hi"] 1024
              & withTemperature 0.7
              & withSystem "Be helpful."
      req.temperature `shouldBe` Just 0.7
      req.system `shouldBe` Just "Be helpful."

    it "uses snake_case in wire format" $ do
      let req = messageRequest "claude-sonnet-4-20250514" [userMessage "Hi"] 1024
              & withTopP 0.9
          v = toJSON req
      lookupKey "max_tokens" v `shouldBe` Just (Aeson.Number 1024)
      lookupKey "top_p" v `shouldBe` Just (Aeson.Number 0.9)

    it "omits Nothing fields" $ do
      let req = messageRequest "claude-sonnet-4-20250514" [userMessage "Hi"] 1024
          v   = toJSON req
      lookupKey "temperature" v `shouldBe` Nothing
      lookupKey "tools" v `shouldBe` Nothing
      lookupKey "system" v `shouldBe` Nothing

  describe "MessageResponse" $ do
    prop "roundtrip" $ \(x :: MessageResponse) ->
      eitherDecode (encode x) === Right x

    it "injects type=message in ToJSON" $ do
      let resp = MessageResponse "msg_01" "claude-sonnet-4-20250514" [] (Just EndTurn) Nothing
                   (Usage 10 25 Nothing Nothing Nothing Nothing Nothing Nothing)
                   Nothing
          v = toJSON resp
      lookupKey "type" v `shouldBe` Just "message"
      lookupKey "role" v `shouldBe` Just "assistant"

    it "validates type in FromJSON" $ do
      let json = "{\"type\":\"wrong\",\"role\":\"assistant\",\"id\":\"msg_01\",\"model\":\"x\",\"content\":[],\"usage\":{\"input_tokens\":0,\"output_tokens\":0}}"
      (decode json :: Maybe MessageResponse) `shouldBe` Nothing

    it "validates role in FromJSON" $ do
      let json = "{\"type\":\"message\",\"role\":\"user\",\"id\":\"msg_01\",\"model\":\"x\",\"content\":[],\"usage\":{\"input_tokens\":0,\"output_tokens\":0}}"
      (decode json :: Maybe MessageResponse) `shouldBe` Nothing

    it "stop_reason is optional" $ do
      let json = "{\"type\":\"message\",\"role\":\"assistant\",\"id\":\"msg_01\",\"model\":\"x\",\"content\":[],\"usage\":{\"input_tokens\":0,\"output_tokens\":0}}"
      case decode json :: Maybe MessageResponse of
        Just resp -> resp.stopReason `shouldBe` Nothing
        Nothing   -> expectationFailure "Failed to parse MessageResponse without stop_reason"
