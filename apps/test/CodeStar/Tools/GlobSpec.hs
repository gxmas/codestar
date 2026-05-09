module CodeStar.Tools.GlobSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, catch, try)
import Data.Aeson (Value (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly, withCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import CodeStar.Tools.Gen ()
import CodeStar.Tools.Glob (globToolHandler)
import CodeStar.Tools.Registry
  ( ToolError (..)
  , ToolHandlerDict (..)
  , ToolInput (..)
  , ToolOutput (..)
  )

spec :: Spec
spec = describe "CodeStar.Tools.Glob" $ do
  prop "invoke never throws on arbitrary ToolInput" $
    \input -> ioProperty $ do
      result <- try @SomeException (globToolHandler.invoke input)
      pure $ case result of
        Right _ -> property True
        Left ex -> counterexample ("threw: " <> show ex) (property False)
  it "finds files matching recursive patterns" $
    withGlobFixture $ \root _older newer -> do
      let handler = globToolHandler
      res <-
        invoke handler $
          mkInput
            [ ("pattern", String "**/*.txt")
            , ("path", String (Text.pack root))
            ]
      case res of
        Left err -> expectationFailure ("Expected Right ToolOutput, got: " <> show err)
        Right out -> do
          content out `shouldSatisfy` Text.isInfixOf (Text.pack newer)
          content out `shouldSatisfy` Text.isInfixOf ".txt"

  it "sorts results by modification time descending" $
    withGlobFixture $ \root older newer -> do
      let handler = globToolHandler
      res <-
        invoke handler $
          mkInput
            [ ("pattern", String "*.txt")
            , ("path", String (Text.pack root))
            ]
      case res of
        Left err -> expectationFailure ("Expected Right ToolOutput, got: " <> show err)
        Right out -> do
          let ls = filter (not . Text.null) (Text.lines (content out))
          ls `shouldSatisfy` (not . null)
          case ls of
            [] -> expectationFailure "Expected non-empty glob output"
            firstPath : rest -> do
              firstPath `shouldBe` Text.pack newer
              case reverse (firstPath : rest) of
                [] -> expectationFailure "Expected non-empty glob output"
                lastPath : _ -> lastPath `shouldBe` Text.pack older

  it "non-recursive patterns do not match nested files" $
    withGlobFixture $ \root _ _ -> do
      let handler = globToolHandler
      res <-
        invoke handler $
          mkInput
            [ ("pattern", String "*.txt")
            , ("path", String (Text.pack root))
            ]
      case res of
        Left err -> expectationFailure ("Expected Right ToolOutput, got: " <> show err)
        Right out ->
          content out `shouldSatisfy` (not . Text.isInfixOf "inside.txt")

  it "returns no-matches sentinel for unmatched patterns" $
    withGlobFixture $ \root _ _ -> do
      let handler = globToolHandler
      res <- invoke handler (mkInput [("pattern", String "*.rs"), ("path", String (Text.pack root))])
      res `shouldBe` Right (ToolOutput "(no files matched pattern)" False)

  it "rejects malformed input payloads" $ do
    let handler = globToolHandler
    r1 <- invoke handler (mkInput [])
    r2 <- invoke handler (mkInput [("pattern", Number 1)])
    r3 <- invoke handler (mkInput [("pattern", String "**/*.txt"), ("path", Number 1)])
    r1 `shouldSatisfy` isInvalidInput
    r2 `shouldSatisfy` isInvalidInput
    r3 `shouldSatisfy` isInvalidInput

  it "uses current directory when path is omitted" $
    withGlobFixture $ \root _ _ -> do
      let handler = globToolHandler
      withCurrentDirectory root $ do
        res <- invoke handler (mkInput [("pattern", String "**/*.txt")])
        case res of
          Left err -> expectationFailure ("Expected Right ToolOutput, got: " <> show err)
          Right out -> do
            let ls = filter (not . Text.null) (Text.lines (content out))
            ls `shouldSatisfy` (not . null)
            -- Output paths are rooted at ".", so ensure they are not absolute fixture paths.
            all (\p -> not (Text.pack root `Text.isPrefixOf` p)) ls `shouldBe` True

mkInput :: [(Text.Text, Value)] -> ToolInput
mkInput entries = ToolInput{arguments = Map.fromList entries}

isInvalidInput :: Either ToolError ToolOutput -> Bool
isInvalidInput (Left (InvalidInput _)) = True
isInvalidInput _ = False

withGlobFixture :: (FilePath -> FilePath -> FilePath -> IO a) -> IO a
withGlobFixture action = do
  tmp <- getTemporaryDirectory
  let root = tmp </> "codestar-glob-spec"
      older = root </> "older.txt"
      newer = root </> "newer.txt"
      nested = root </> "nested" </> "inside.txt"
  ignoreIO (removePathForcibly root)
  createDirectoryIfMissing True (root </> "nested")
  Text.IO.writeFile older "old"
  threadDelay 1100000
  Text.IO.writeFile newer "new"
  Text.IO.writeFile nested "nested"
  out <- action root older newer
  ignoreIO (removePathForcibly root)
  pure out

ignoreIO :: IO () -> IO ()
ignoreIO io = io `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()
