module Anthropic.Protocol.StreamSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Types
import Anthropic.Protocol.Stream
import Anthropic.Protocol.Generators ()

lookupKey :: Text -> Aeson.Value -> Maybe Aeson.Value
lookupKey k (Aeson.Object o) = KM.lookup (Key.fromText k) o
lookupKey _ _                = Nothing

spec :: Spec
spec = do
  describe "Delta" $ do
    prop "roundtrip" $ \(x :: Delta) ->
      eitherDecode (encode x) === Right x

    it "TextDelta wire format" $ do
      let v = toJSON (TextDelta "hello")
      lookupKey "type" v `shouldBe` Just "text_delta"
      lookupKey "text" v `shouldBe` Just (Aeson.String "hello")

    it "InputJsonDelta wire format" $ do
      let v = toJSON (InputJsonDelta "{\"key\":")
      lookupKey "type" v `shouldBe` Just "input_json_delta"
      lookupKey "partial_json" v `shouldBe` Just (Aeson.String "{\"key\":")

    it "ThinkingDelta wire format" $ do
      let v = toJSON (ThinkingDelta "reasoning")
      lookupKey "type" v `shouldBe` Just "thinking_delta"
      lookupKey "thinking" v `shouldBe` Just (Aeson.String "reasoning")

    it "SignatureDelta wire format" $ do
      let v = toJSON (SignatureDelta "sig123")
      lookupKey "type" v `shouldBe` Just "signature_delta"
      lookupKey "signature" v `shouldBe` Just (Aeson.String "sig123")

  describe "StreamEvent" $ do
    prop "roundtrip" $ \(x :: StreamEvent) ->
      eitherDecode (encode x) === Right x

    it "EventMessageStop wire format" $ do
      let v = toJSON EventMessageStop
      lookupKey "type" v `shouldBe` Just "message_stop"

    it "EventPing wire format" $ do
      let v = toJSON EventPing
      lookupKey "type" v `shouldBe` Just "ping"

    it "EventContentBlockStop wire format" $ do
      let v = toJSON (EventContentBlockStop 0)
      lookupKey "type" v `shouldBe` Just "content_block_stop"
      lookupKey "index" v `shouldBe` Just (Aeson.Number 0)

    it "EventContentBlockDelta includes delta" $ do
      let v = toJSON (EventContentBlockDelta 1 (TextDelta "hi"))
      lookupKey "type" v `shouldBe` Just "content_block_delta"
      case lookupKey "delta" v of
        Just d -> lookupKey "text" d `shouldBe` Just (Aeson.String "hi")
        Nothing -> expectationFailure "Missing delta field"

    it "EventError carries error" $ do
      let v = toJSON (EventError (ApiError RateLimitError "slow down"))
      lookupKey "type" v `shouldBe` Just "error"
      lookupKey "error" v `shouldNotBe` Nothing

    it "EventMessageDelta carries usage and delta" $ do
      let usage = Usage 10 20 Nothing Nothing Nothing Nothing Nothing Nothing
          v = toJSON (EventMessageDelta (Just EndTurn) Nothing usage)
      lookupKey "type" v `shouldBe` Just "message_delta"
      lookupKey "usage" v `shouldNotBe` Nothing
      case lookupKey "delta" v of
        Just d -> lookupKey "stop_reason" d `shouldBe` Just "end_turn"
        Nothing -> expectationFailure "Missing delta field"

  describe "StreamState accessors" $ do
    it "streamPhase returns the phase" $ do
      let st = StreamState AwaitingStart Nothing [] Nothing
      streamPhase st `shouldBe` AwaitingStart

    it "accumulatedBlocks returns blocks" $ do
      let block = TextContent (TextBlock "hi" Nothing Nothing)
          st = StreamState InMessage Nothing [block] Nothing
      accumulatedBlocks st `shouldBe` [block]

    it "isComplete is True for StreamDone" $
      isComplete (StreamState StreamDone Nothing [] Nothing) `shouldBe` True

    it "isComplete is False for other phases" $
      isComplete (StreamState InMessage Nothing [] Nothing) `shouldBe` False
