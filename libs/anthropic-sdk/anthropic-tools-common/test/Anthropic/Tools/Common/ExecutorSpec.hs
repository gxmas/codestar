module Anthropic.Tools.Common.ExecutorSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Anthropic.Types.Content.ToolUse (ToolUseBlock (..))
import Anthropic.Types.Content.ToolResult (ToolResultBlock (..), ToolResultContent (..))
import Anthropic.Tools.Common.Executor
import Anthropic.Tools.Common.Parser (ParseError (..))

spec :: Spec
spec = do
  describe "executeReadFile" $ do
    it "reads an existing file" $ withSystemTempDirectory "test" $ \dir -> do
      let fp = dir </> "hello.txt"
      writeFile fp "hello world"
      let tub = mkToolUse "read_file" (object ["path" .= T.pack fp])
      result <- executeReadFile tub
      case result of
        Right tb -> do
          tb.toolUseId `shouldBe` "tu_test"
          tb.isError `shouldBe` Nothing
          tb.content `shouldBe` Just (ToolResultText "hello world")
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

    it "returns error for missing file" $ do
      let tub = mkToolUse "read_file" (object ["path" .= ("/nonexistent/file.txt" :: Text)])
      result <- executeReadFile tub
      case result of
        Right tb -> do
          tb.isError `shouldBe` Just True
        Left err -> expectationFailure $ "Expected IO error in result, got: " ++ show err

    it "returns parse error for invalid input" $ do
      let tub = mkToolUse "read_file" (object ["wrong" .= ("field" :: Text)])
      result <- executeReadFile tub
      case result of
        Left (ToolParseError (ParseError { toolName = tn })) ->
          tn `shouldBe` "read_file"
        other -> expectationFailure $ "Expected ToolParseError, got: " ++ show other

  describe "executeWriteFile" $ do
    it "writes a file" $ withSystemTempDirectory "test" $ \dir -> do
      let fp = dir </> "output.txt"
          tub = mkToolUse "write_file" (object
            [ "path" .= T.pack fp
            , "content" .= ("written content" :: Text)
            ])
      result <- executeWriteFile tub
      case result of
        Right tb -> do
          tb.isError `shouldBe` Nothing
          content <- readFile fp
          content `shouldBe` "written content"
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

    it "creates parent directories when requested" $ withSystemTempDirectory "test" $ \dir -> do
      let fp = dir </> "sub" </> "dir" </> "file.txt"
          tub = mkToolUse "write_file" (object
            [ "path" .= T.pack fp
            , "content" .= ("nested" :: Text)
            , "create_dirs" .= True
            ])
      result <- executeWriteFile tub
      case result of
        Right tb -> do
          tb.isError `shouldBe` Nothing
          content <- readFile fp
          content `shouldBe` "nested"
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

  describe "executeListDirectory" $ do
    it "lists directory contents" $ withSystemTempDirectory "test" $ \dir -> do
      writeFile (dir </> "a.txt") ""
      writeFile (dir </> "b.txt") ""
      let tub = mkToolUse "list_directory" (object ["path" .= T.pack dir])
      result <- executeListDirectory tub
      case result of
        Right tb -> do
          tb.isError `shouldBe` Nothing
          case tb.content of
            Just (ToolResultText txt) -> do
              T.isInfixOf "a.txt" txt `shouldBe` True
              T.isInfixOf "b.txt" txt `shouldBe` True
            other -> expectationFailure $ "Expected text content, got: " ++ show other
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

  describe "executeCommand" $ do
    it "executes a simple command" $ do
      let tub = mkToolUse "execute_command" (object ["command" .= ("echo hello" :: Text)])
      result <- executeCommand tub
      case result of
        Right tb -> do
          tb.isError `shouldBe` Nothing
          case tb.content of
            Just (ToolResultText txt) ->
              T.strip txt `shouldBe` "hello"
            other -> expectationFailure $ "Expected text content, got: " ++ show other
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

    it "reports nonzero exit code" $ do
      let tub = mkToolUse "execute_command" (object ["command" .= ("exit 42" :: Text)])
      result <- executeCommand tub
      case result of
        Right tb -> do
          tb.isError `shouldBe` Nothing
          case tb.content of
            Just (ToolResultText txt) ->
              T.isInfixOf "Exit code: 42" txt `shouldBe` True
            other -> expectationFailure $ "Expected text content, got: " ++ show other
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

-- | Helper to create a ToolUseBlock for testing.
mkToolUse :: Text -> Value -> ToolUseBlock
mkToolUse name input = ToolUseBlock
  { id = "tu_test"
  , name = name
  , input = input
  , cacheControl = Nothing
  }
