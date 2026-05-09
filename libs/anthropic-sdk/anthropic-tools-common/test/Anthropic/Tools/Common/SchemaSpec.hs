module Anthropic.Tools.Common.SchemaSpec (spec) where

import Data.Aeson (Value(..), eitherDecode)
import qualified Data.Vector as V
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.JsonSchema (encode)
import Data.Text (Text)
import Test.Hspec

import Anthropic.Tools.Common.Schema

spec :: Spec
spec = do
  describe "ReadFileInput" $ do
    it "roundtrips through JSON" $ do
      let input = ReadFileInput { path = "/tmp/test.txt" }
      eitherDecode (Aeson.encode input) `shouldBe` Right input

    it "serializes with snake_case" $ do
      let input = ReadFileInput { path = "/tmp/test.txt" }
          val = Aeson.toJSON input
      lookupKey "path" val `shouldBe` Just (String "/tmp/test.txt")

  describe "WriteFileInput" $ do
    it "roundtrips through JSON" $ do
      let input = WriteFileInput
            { path = "/tmp/out.txt"
            , content = "hello"
            , createDirs = Just True
            }
      eitherDecode (Aeson.encode input) `shouldBe` Right input

    it "omits Nothing fields" $ do
      let input = WriteFileInput
            { path = "/tmp/out.txt"
            , content = "hello"
            , createDirs = Nothing
            }
          val = Aeson.toJSON input
      lookupKey "create_dirs" val `shouldBe` Nothing

  describe "ListDirectoryInput" $ do
    it "roundtrips through JSON" $ do
      let input = ListDirectoryInput
            { path = "/tmp"
            , recursive = Just True
            , pattern = Just "*.hs"
            }
      eitherDecode (Aeson.encode input) `shouldBe` Right input

  describe "SearchFilesInput" $ do
    it "roundtrips through JSON" $ do
      let input = SearchFilesInput
            { path = "/src"
            , pattern = "*.hs"
            , caseSensitive = Just False
            , maxResults = Just 100
            }
      eitherDecode (Aeson.encode input) `shouldBe` Right input

    it "uses snake_case for field names" $ do
      let input = SearchFilesInput
            { path = "/src"
            , pattern = "*.hs"
            , caseSensitive = Just True
            , maxResults = Just 50
            }
          val = Aeson.toJSON input
      lookupKey "case_sensitive" val `shouldBe` Just (Bool True)
      lookupKey "max_results" val `shouldBe` Just (Number 50)

  describe "ExecuteCommandInput" $ do
    it "roundtrips through JSON" $ do
      let input = ExecuteCommandInput
            { command = "echo hello"
            , workingDir = Just "/tmp"
            , env = Nothing
            }
      eitherDecode (Aeson.encode input) `shouldBe` Right input

    it "uses snake_case for working_dir" $ do
      let input = ExecuteCommandInput
            { command = "ls"
            , workingDir = Just "/home"
            , env = Nothing
            }
          val = Aeson.toJSON input
      lookupKey "working_dir" val `shouldBe` Just (String "/home")

  describe "FetchUrlInput" $ do
    it "roundtrips through JSON" $ do
      let input = FetchUrlInput
            { url = "https://example.com"
            , method = Just "POST"
            , headers = Nothing
            }
      eitherDecode (Aeson.encode input) `shouldBe` Right input

  describe "Schemas" $ do
    it "readFileSchema has type object" $ do
      let val = Data.JsonSchema.encode readFileSchema
      lookupKey "type" val `shouldBe` Just (String "object")

    it "writeFileSchema has required path and content" $ do
      let val = Data.JsonSchema.encode writeFileSchema
      case lookupKey "required" val of
        Just (Array arr) -> do
          let reqs = extractStrings arr
          reqs `shouldContain` ["path"]
          reqs `shouldContain` ["content"]
        other -> expectationFailure $ "Expected required array, got: " ++ show other

    it "executeCommandSchema has required command" $ do
      let val = Data.JsonSchema.encode executeCommandSchema
      case lookupKey "required" val of
        Just (Array arr) -> do
          let reqs = extractStrings arr
          reqs `shouldContain` ["command"]
        other -> expectationFailure $ "Expected required array, got: " ++ show other

    it "fetchUrlSchema has required url" $ do
      let val = Data.JsonSchema.encode fetchUrlSchema
      case lookupKey "required" val of
        Just (Array arr) -> do
          let reqs = extractStrings arr
          reqs `shouldContain` ["url"]
        other -> expectationFailure $ "Expected required array, got: " ++ show other

-- | Helper: look up a key in a JSON Object.
lookupKey :: Text -> Value -> Maybe Value
lookupKey key (Object o) = KM.lookup (Key.fromText key) o
lookupKey _ _ = Nothing

-- | Helper: extract Text values from a JSON Array.
extractStrings :: V.Vector Value -> [Text]
extractStrings = foldr go []
  where
    go (String t) acc = t : acc
    go _          acc = acc
