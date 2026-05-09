module Network.MCP.CodecSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Network.MCP.Codec
import Network.MCP.Generators ()
import Network.MCP.Types

spec :: Spec
spec = do
  describe "McpCodec" $ do
    prop "roundtrip: decode (encode msg) == Right msg" $ \(x :: MCPMessage) ->
      decode McpCodec (fromRight' (encode McpCodec x)) === Right x

    prop "encode is always Right for well-formed messages" $ \(x :: MCPMessage) ->
      isRight (encode McpCodec x)

    it "non-JSON bytes -> Left" $ do
      let result = decode McpCodec "not json at all"
      result `shouldSatisfy` isLeft

    it "valid JSON without jsonrpc -> Left" $ do
      let result = decode McpCodec "{\"method\":\"test\"}"
      result `shouldSatisfy` isLeft

    it "valid JSON with wrong jsonrpc version -> Left" $ do
      let result = decode McpCodec "{\"jsonrpc\":\"1.0\",\"method\":\"test\"}"
      result `shouldSatisfy` isLeft

    it "valid request decodes successfully" $ do
      let json = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}"
      decode McpCodec json `shouldSatisfy` isRight

  describe "CodecErrorKind" $
    it "all 4 constructors" $
      length [minBound .. maxBound :: CodecErrorKind] `shouldBe` 4

-- Helpers
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

fromRight' :: Either a b -> b
fromRight' (Right x) = x
fromRight' _ = error "fromRight': Left"
