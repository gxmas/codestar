module Anthropic.Protocol.BatchSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Protocol.Batch
import Anthropic.Protocol.Generators ()

lookupKey :: Text -> Aeson.Value -> Maybe Aeson.Value
lookupKey k (Aeson.Object o) = KM.lookup (Key.fromText k) o
lookupKey _ _                = Nothing

spec :: Spec
spec = do
  describe "BatchStatus" $ do
    prop "roundtrip" $ \(x :: BatchStatus) ->
      eitherDecode (encode x) === Right x

    it "BatchInProgress -> \"in_progress\"" $
      toJSON BatchInProgress `shouldBe` "in_progress"

    it "BatchCanceling -> \"canceling\"" $
      toJSON BatchCanceling `shouldBe` "canceling"

    it "BatchEnded -> \"ended\"" $
      toJSON BatchEnded `shouldBe` "ended"

  describe "BatchCounts" $
    prop "roundtrip" $ \(x :: BatchCounts) ->
      eitherDecode (encode x) === Right x

  describe "BatchItem" $ do
    prop "roundtrip" $ \(x :: BatchItem) ->
      eitherDecode (encode x) === Right x

    it "uses custom_id in wire format" $ do
      let json = "{\"custom_id\":\"req-001\",\"params\":{\"model\":\"x\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"max_tokens\":1}}"
      case decode json :: Maybe BatchItem of
        Just item -> item.customId `shouldBe` "req-001"
        Nothing   -> expectationFailure "Failed to parse BatchItem"

  describe "BatchResponse" $ do
    prop "roundtrip" $ \(x :: BatchResponse) ->
      eitherDecode (encode x) === Right x

    it "injects type=message_batch" $ do
      let br = BatchResponse "batch_01" BatchInProgress
                (BatchCounts 5 0 0 0 0) "2024-01-01" Nothing "2024-01-02" Nothing Nothing Nothing
          v = toJSON br
      lookupKey "type" v `shouldBe` Just "message_batch"

    it "validates type in FromJSON" $ do
      let json = "{\"type\":\"wrong\",\"id\":\"b1\",\"processing_status\":\"in_progress\",\"request_counts\":{\"processing\":0,\"succeeded\":0,\"errored\":0,\"canceled\":0,\"expired\":0},\"created_at\":\"t\",\"expires_at\":\"t\"}"
      (decode json :: Maybe BatchResponse) `shouldBe` Nothing

  describe "BatchResult" $ do
    prop "roundtrip" $ \(x :: BatchResult) ->
      eitherDecode (encode x) === Right x

    it "BatchCanceled wire format" $ do
      let v = toJSON BatchCanceled
      lookupKey "type" v `shouldBe` Just "canceled"

    it "BatchExpired wire format" $ do
      let v = toJSON BatchExpired
      lookupKey "type" v `shouldBe` Just "expired"

  describe "BatchResultItem" $
    prop "roundtrip" $ \(x :: BatchResultItem) ->
      eitherDecode (encode x) === Right x

  describe "DeletedBatch" $ do
    prop "roundtrip" $ \(x :: DeletedBatch) ->
      eitherDecode (encode x) === Right x

    it "has type=message_batch_deleted" $ do
      let v = toJSON (DeletedBatch "batch_01")
      lookupKey "type" v `shouldBe` Just "message_batch_deleted"
