module CodeStar.Tools.TestsSpec (spec) where

import Control.Exception (IOException, catch)
import Data.Aeson (Value (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec

import CodeStar.Tools.Registry
  ( ToolError (..)
  , ToolHandlerDict (..)
  , ToolInput (..)
  )
import CodeStar.Tools.Tests (testsToolHandler)

spec :: Spec
spec = describe "CodeStar.Tools.Tests" $ do
  it "fails with clear message when language markers are missing" $
    withTempWorkspace "codestar-tests-tool-nomarkers" $ \root -> do
      let handler = testsToolHandler
      res <- invoke handler (mkInput [("workspace", String (Text.pack root))])
      res `shouldSatisfy` isDetectFailure

  it "uses default workspace when omitted and still returns typed result" $ do
    let handler = testsToolHandler
    res <- invoke handler (mkInput [])
    res `shouldSatisfy` isNotInvalidInput

  it "rejects malformed optional fields" $ do
    let handler = testsToolHandler
    r1 <- invoke handler (mkInput [("workspace", Number 1)])
    r2 <- invoke handler (mkInput [("subset", Bool True)])
    r3 <- invoke handler (mkInput [("workspace", String "."), ("subset", Number 1)])
    r1 `shouldSatisfy` isInvalidInput
    r2 `shouldSatisfy` isInvalidInput
    r3 `shouldSatisfy` isInvalidInput

mkInput :: [(Text.Text, Value)] -> ToolInput
mkInput entries = ToolInput{arguments = Map.fromList entries}

isDetectFailure :: Either ToolError a -> Bool
isDetectFailure (Left (ExecutionFailed msg)) = Text.isInfixOf "Could not detect project language" msg
isDetectFailure _ = False

isNotInvalidInput :: Either ToolError a -> Bool
isNotInvalidInput (Left (InvalidInput _)) = False
isNotInvalidInput _ = True

isInvalidInput :: Either ToolError a -> Bool
isInvalidInput (Left (InvalidInput _)) = True
isInvalidInput _ = False

withTempWorkspace :: FilePath -> (FilePath -> IO a) -> IO a
withTempWorkspace name action = do
  tmp <- getTemporaryDirectory
  let root = tmp </> name
  ignoreIO (removePathForcibly root)
  createDirectoryIfMissing True root
  out <- action root
  ignoreIO (removePathForcibly root)
  pure out

ignoreIO :: IO () -> IO ()
ignoreIO io = io `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()
