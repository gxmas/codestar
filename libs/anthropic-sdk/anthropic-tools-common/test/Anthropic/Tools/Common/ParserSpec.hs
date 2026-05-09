module Anthropic.Tools.Common.ParserSpec (spec) where

import Data.Aeson (Value(..), object, (.=))
import Data.Text (Text)
import Test.Hspec

import Anthropic.Types.Content.ToolUse (ToolUseBlock (..))
import Anthropic.Tools.Common.Parser
import Anthropic.Tools.Common.Schema (ReadFileInput (..), WriteFileInput (..))

spec :: Spec
spec = do
  describe "parseToolInput" $ do
    it "parses valid ReadFileInput" $ do
      let tub = ToolUseBlock
            { id = "tu_001"
            , name = "read_file"
            , input = object ["path" .= ("test.txt" :: Text)]
            , cacheControl = Nothing
            }
      case parseToolInput tub of
        Right (input :: ReadFileInput) ->
          input.path `shouldBe` "test.txt"
        Left err ->
          expectationFailure $ "Expected success, got: " ++ show err

    it "parses valid WriteFileInput with optional fields" $ do
      let tub = ToolUseBlock
            { id = "tu_002"
            , name = "write_file"
            , input = object
                [ "path" .= ("out.txt" :: Text)
                , "content" .= ("hello" :: Text)
                , "create_dirs" .= True
                ]
            , cacheControl = Nothing
            }
      case parseToolInput tub of
        Right (input :: WriteFileInput) -> do
          input.path `shouldBe` "out.txt"
          input.content `shouldBe` "hello"
          input.createDirs `shouldBe` Just True
        Left err ->
          expectationFailure $ "Expected success, got: " ++ show err

    it "parses WriteFileInput without optional fields" $ do
      let tub = ToolUseBlock
            { id = "tu_003"
            , name = "write_file"
            , input = object
                [ "path" .= ("out.txt" :: Text)
                , "content" .= ("hello" :: Text)
                ]
            , cacheControl = Nothing
            }
      case parseToolInput tub of
        Right (input :: WriteFileInput) ->
          input.createDirs `shouldBe` Nothing
        Left err ->
          expectationFailure $ "Expected success, got: " ++ show err

    it "returns ParseError for invalid input" $ do
      let tub = ToolUseBlock
            { id = "tu_004"
            , name = "read_file"
            , input = object ["wrong_field" .= ("value" :: Text)]
            , cacheControl = Nothing
            }
      case parseToolInput tub :: Either ParseError ReadFileInput of
        Left err -> do
          err.toolName `shouldBe` "read_file"
          err.rawInput `shouldBe` object ["wrong_field" .= ("value" :: Text)]
        Right _ ->
          expectationFailure "Expected parse error"

    it "returns ParseError for non-object input" $ do
      let tub = ToolUseBlock
            { id = "tu_005"
            , name = "read_file"
            , input = String "not an object"
            , cacheControl = Nothing
            }
      case parseToolInput tub :: Either ParseError ReadFileInput of
        Left err ->
          err.toolName `shouldBe` "read_file"
        Right _ ->
          expectationFailure "Expected parse error"
