{-# OPTIONS_GHC -Wno-x-partial #-}

module Network.MCP.TypesSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Network.MCP.Generators ()
import Network.MCP.Types

lookupKey :: Aeson.Key -> Aeson.Value -> Maybe Aeson.Value
lookupKey k (Aeson.Object o) = KM.lookup k o
lookupKey _ _ = Nothing

spec :: Spec
spec = do
  describe "RequestId" $ do
    prop "roundtrip" $ \(x :: RequestId) ->
      eitherDecode (encode x) === Right x

    it "number encodes as JSON number" $ do
      let v = toJSON (RequestId (Right 42))
      v `shouldBe` Aeson.Number 42

    it "string encodes as JSON string" $ do
      let v = toJSON (RequestId (Left "abc"))
      v `shouldBe` Aeson.String "abc"

  describe "ProgressToken" $
    prop "roundtrip" $ \(x :: ProgressToken) ->
      eitherDecode (encode x) === Right x

  describe "Cursor" $
    prop "roundtrip" $ \(x :: Cursor) ->
      eitherDecode (encode x) === Right x

  describe "ProtocolVersion" $
    prop "roundtrip" $ \(x :: ProtocolVersion) ->
      eitherDecode (encode x) === Right x

  describe "Timestamp" $
    prop "roundtrip" $ \(x :: Timestamp) ->
      eitherDecode (encode x) === Right x

  describe "LoggingLevel" $ do
    prop "roundtrip" $ \(x :: LoggingLevel) ->
      eitherDecode (encode x) === Right x

    it "all 8 constructors" $
      length [minBound .. maxBound :: LoggingLevel] `shouldBe` 8

    it "all encode to distinct strings" $ do
      let encoded = map toJSON [minBound .. maxBound :: LoggingLevel]
      length encoded `shouldBe` length (nub encoded)

  describe "Implementation" $ do
    prop "roundtrip" $ \(x :: Implementation) ->
      eitherDecode (encode x) === Right x

    it "omits optional fields when Nothing" $ do
      let impl = Implementation "test" "1.0" Nothing Nothing
          v = toJSON impl
      lookupKey "title" v `shouldBe` Nothing
      lookupKey "description" v `shouldBe` Nothing

  describe "RPCError" $
    prop "roundtrip" $ \(x :: RPCError) ->
      eitherDecode (encode x) === Right x

  describe "MCPMessage" $ do
    prop "roundtrip" $ \(x :: MCPMessage) ->
      eitherDecode (encode x) === Right x

    it "jsonrpc field missing causes decode error" $ do
      let json = "{\"method\":\"test\"}"
      (eitherDecode json :: Either String MCPMessage) `shouldSatisfy` isLeft

    it "jsonrpc wrong version causes decode error" $ do
      let json = "{\"jsonrpc\":\"1.0\",\"method\":\"test\"}"
      (eitherDecode json :: Either String MCPMessage) `shouldSatisfy` isLeft

    it "error key -> MCPError" $ do
      let json = "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-1,\"message\":\"fail\"}}"
      case eitherDecode json :: Either String MCPMessage of
        Right (MCPError _) -> pure ()
        other -> expectationFailure $ "Expected MCPError, got " ++ show other

    it "result key -> MCPResult" $ do
      let json = "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}"
      case eitherDecode json :: Either String MCPMessage of
        Right (MCPResult _) -> pure ()
        other -> expectationFailure $ "Expected MCPResult, got " ++ show other

    it "id key (no result/error) -> MCPRequest" $ do
      let json = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}"
      case eitherDecode json :: Either String MCPMessage of
        Right (MCPRequest _) -> pure ()
        other -> expectationFailure $ "Expected MCPRequest, got " ++ show other

    it "no id/result/error -> MCPNotification" $ do
      let json = "{\"jsonrpc\":\"2.0\",\"method\":\"test\"}"
      case eitherDecode json :: Either String MCPMessage of
        Right (MCPNotification _) -> pure ()
        other -> expectationFailure $ "Expected MCPNotification, got " ++ show other

    it "unknown top-level fields do not cause decode failure" $ do
      let json = "{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"extra\":true}"
      case eitherDecode json :: Either String MCPMessage of
        Right (MCPNotification _) -> pure ()
        other -> expectationFailure $ "Expected success, got " ++ show other

  describe "JSONRPCRequest" $
    prop "roundtrip" $ \(x :: JSONRPCRequest) ->
      eitherDecode (encode x) === Right x

  describe "JSONRPCNotification" $
    prop "roundtrip" $ \(x :: JSONRPCNotification) ->
      eitherDecode (encode x) === Right x

  describe "JSONRPCResult" $
    prop "roundtrip" $ \(x :: JSONRPCResult) ->
      eitherDecode (encode x) === Right x

  describe "JSONRPCError" $
    prop "roundtrip" $ \(x :: JSONRPCError) ->
      eitherDecode (encode x) === Right x

-- Helpers
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (/= x) xs)
