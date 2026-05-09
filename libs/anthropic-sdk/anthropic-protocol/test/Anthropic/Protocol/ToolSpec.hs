module Anthropic.Protocol.ToolSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.JsonSchema (stringSchema)
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Protocol.Tool
import Anthropic.Protocol.Generators ()

lookupKey :: Text -> Aeson.Value -> Maybe Aeson.Value
lookupKey k (Aeson.Object o) = KM.lookup (Key.fromText k) o
lookupKey _ _                = Nothing

spec :: Spec
spec = do
  describe "ServerToolType" $ do
    prop "roundtrip" $ \(x :: ServerToolType) ->
      eitherDecode (encode x) === Right x

    it "WebSearch -> \"web_search\"" $
      toJSON WebSearch `shouldBe` "web_search"

    it "BashTool -> \"bash\"" $
      toJSON BashTool `shouldBe` "bash"

    it "CodeExecution -> \"code_execution\"" $
      toJSON CodeExecution `shouldBe` "code_execution"

    it "OtherServerTool preserves text" $
      toJSON (OtherServerTool "new_thing") `shouldBe` "new_thing"

    it "normalizes known tools in FromJSON" $ do
      let json = "\"web_search\""
      decode json `shouldBe` Just WebSearch

    it "unknown tools become OtherServerTool" $ do
      let json = "\"future_tool\""
      decode json `shouldBe` Just (OtherServerTool "future_tool")

  describe "ToolChoice" $ do
    prop "roundtrip" $ \(x :: ToolChoice) ->
      eitherDecode (encode x) === Right x

    it "toolAuto wire format" $ do
      let v = toJSON toolAuto
      lookupKey "type" v `shouldBe` Just "auto"

    it "toolAny wire format" $ do
      let v = toJSON toolAny
      lookupKey "type" v `shouldBe` Just "any"

    it "toolNone wire format" $ do
      let v = toJSON toolNone
      lookupKey "type" v `shouldBe` Just "none"

    it "ToolSpecific includes name" $ do
      let v = toJSON (ToolSpecific "get_weather" (DisableParallel False))
      lookupKey "type" v `shouldBe` Just "tool"
      lookupKey "name" v `shouldBe` Just (Aeson.String "get_weather")

    it "disable_parallel_tool_use present when True" $ do
      let v = toJSON (ToolAuto (DisableParallel True))
      lookupKey "disable_parallel_tool_use" v `shouldBe` Just (Aeson.Bool True)

    it "disable_parallel_tool_use absent when False" $ do
      let v = toJSON (ToolAuto (DisableParallel False))
      lookupKey "disable_parallel_tool_use" v `shouldBe` Nothing

  describe "CustomToolDef" $
    prop "roundtrip" $ \(x :: CustomToolDef) ->
      eitherDecode (encode x) === Right x

  describe "ServerToolDef" $
    prop "roundtrip" $ \(x :: ServerToolDef) ->
      eitherDecode (encode x) === Right x

  describe "ToolDefinition" $ do
    prop "roundtrip" $ \(x :: ToolDefinition) ->
      eitherDecode (encode x) === Right x

    it "CustomTool has input_schema" $ do
      let td = CustomTool (customToolDef "test" stringSchema)
          v  = toJSON td
      lookupKey "input_schema" v `shouldNotBe` Nothing

    it "ServerTool has type field" $ do
      let td = ServerTool (serverToolDef WebSearch "web_search")
          v  = toJSON td
      lookupKey "type" v `shouldBe` Just "web_search"

  describe "knownServerTools" $
    it "has 8 entries" $
      length knownServerTools `shouldBe` 8
