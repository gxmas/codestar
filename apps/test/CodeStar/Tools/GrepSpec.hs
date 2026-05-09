module CodeStar.Tools.GrepSpec (spec) where

import Control.Exception (IOException, SomeException, catch, try)
import Data.Aeson (Value (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import CodeStar.Tools.Gen ()
import CodeStar.Tools.Grep (grepToolHandler)
import CodeStar.Tools.Registry
  ( ToolError (..)
  , ToolHandlerDict (..)
  , ToolInput (..)
  , ToolOutput (..)
  )

spec :: Spec
spec = describe "CodeStar.Tools.Grep" $ do
  prop "invoke never throws on arbitrary ToolInput" $
    \input -> ioProperty $ do
      result <- try @SomeException (grepToolHandler.invoke input)
      pure $ case result of
        Right _ -> property True
        Left ex -> counterexample ("threw: " <> show ex) (property False)
  it "finds matches in content mode" $
    withGrepFixture $ \root -> do
      let handler = grepToolHandler
      res <-
        invoke handler $
          mkInput
            [ ("pattern", String "Alpha")
            , ("path", String (Text.pack root))
            , ("output_mode", String "content")
            ]
      case res of
        Left err -> expectationFailure ("Expected Right ToolOutput, got: " <> show err)
        Right out -> content out `shouldSatisfy` Text.isInfixOf "Alpha beta"

  it "returns no-matches sentinel when nothing matches" $
    withGrepFixture $ \root -> do
      let handler = grepToolHandler
      res <- invoke handler (mkInput [("pattern", String "not-present-needle"), ("path", String (Text.pack root))])
      res `shouldBe` Right (ToolOutput "(no matches)" False)

  it "supports case-insensitive matching" $
    withGrepFixture $ \root -> do
      let handler = grepToolHandler
      res <-
        invoke handler $
          mkInput
            [ ("pattern", String "alpha")
            , ("path", String (Text.pack root))
            , ("case_insensitive", Bool True)
            , ("output_mode", String "content")
            ]
      case res of
        Left err -> expectationFailure ("Expected Right ToolOutput, got: " <> show err)
        Right out -> content out `shouldSatisfy` Text.isInfixOf "Alpha beta"

  it "defaults to files_with_matches output mode" $
    withGrepFixture $ \root -> do
      let handler = grepToolHandler
      res <- invoke handler (mkInput [("pattern", String "Alpha"), ("path", String (Text.pack root))])
      case res of
        Left err -> expectationFailure ("Expected Right ToolOutput, got: " <> show err)
        Right out -> do
          content out `shouldSatisfy` Text.isInfixOf "a.txt"
          content out `shouldSatisfy` (not . Text.isInfixOf "Alpha beta")

  it "rejects malformed input payloads" $ do
    let handler = grepToolHandler
    r1 <- invoke handler (mkInput [])
    r2 <- invoke handler (mkInput [("pattern", Number 1)])
    r3 <- invoke handler (mkInput [("pattern", String "Alpha"), ("path", Number 1)])
    r4 <- invoke handler (mkInput [("pattern", String "Alpha"), ("glob", Number 1)])
    r5 <- invoke handler (mkInput [("pattern", String "Alpha"), ("output_mode", Number 1)])
    r6 <- invoke handler (mkInput [("pattern", String "Alpha"), ("case_insensitive", Number 1)])
    r1 `shouldSatisfy` isInvalidInput
    r2 `shouldSatisfy` isInvalidInput
    r3 `shouldSatisfy` isInvalidInput
    r4 `shouldSatisfy` isInvalidInput
    r5 `shouldSatisfy` isInvalidInput
    r6 `shouldSatisfy` isInvalidInput

mkInput :: [(Text.Text, Value)] -> ToolInput
mkInput entries = ToolInput{arguments = Map.fromList entries}

isInvalidInput :: Either ToolError ToolOutput -> Bool
isInvalidInput (Left (InvalidInput _)) = True
isInvalidInput _ = False

withGrepFixture :: (FilePath -> IO a) -> IO a
withGrepFixture action = do
  tmp <- getTemporaryDirectory
  let root = tmp </> "codestar-grep-spec"
      fp1 = root </> "a.txt"
      fp2 = root </> "b.md"
  ignoreIO (removePathForcibly root)
  createDirectoryIfMissing True root
  Text.IO.writeFile fp1 "Alpha beta\ngamma\n"
  Text.IO.writeFile fp2 "delta epsilon\n"
  out <- action root
  ignoreIO (removePathForcibly root)
  pure out

ignoreIO :: IO () -> IO ()
ignoreIO io = io `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()
