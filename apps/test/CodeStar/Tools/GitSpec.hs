module CodeStar.Tools.GitSpec (spec) where

import Control.Exception (IOException, catch)
import Data.Aeson (Value (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import System.Directory
  ( createDirectoryIfMissing
  , getTemporaryDirectory
  , removePathForcibly
  , withCurrentDirectory
  )
import System.FilePath ((</>))
import Test.Hspec

import CodeStar.Tools.Git (gitToolHandler)
import CodeStar.Tools.Registry
  ( ToolError (..)
  , ToolHandlerDict (..)
  , ToolInput (..)
  , ToolOutput (..)
  )

spec :: Spec
spec = describe "CodeStar.Tools.Git" $ do
  it "returns execution error when run outside a git repo" $
    withTempDir "codestar-git-spec-status" $ do
      let handler = gitToolHandler
      res <- invoke handler (mkInput [("operation", String "status")])
      res `shouldSatisfy` isExecutionFailed

  it "accepts branch operation without requiring extra parameters" $
    withTempDir "codestar-git-spec-branch" $ do
      let handler = gitToolHandler
      res <- invoke handler (mkInput [("operation", String "branch")])
      res `shouldSatisfy` isExecutionFailed

  it "requires commit message for commit operation" $
    withTempDir "codestar-git-spec-commit" $ do
      let handler = gitToolHandler
      res <- invoke handler (mkInput [("operation", String "commit")])
      res `shouldSatisfy` isInvalidInput

  it "rejects unknown operation" $ do
    let handler = gitToolHandler
    res <- invoke handler (mkInput [("operation", String "made-up-op")])
    res `shouldSatisfy` isInvalidInput

  it "rejects malformed optional fields instead of defaulting silently" $ do
    let handler = gitToolHandler
    r1 <- invoke handler (mkInput [("operation", String "diff"), ("staged", String "yes")])
    r2 <- invoke handler (mkInput [("operation", String "log"), ("count", String "ten")])
    r3 <- invoke handler (mkInput [("operation", String "log"), ("count", Number 0)])
    r4 <- invoke handler (mkInput [("operation", String "branch"), ("branch", Number 1)])
    r5 <- invoke handler (mkInput [("operation", String "push"), ("remote", Number 1)])
    r6 <- invoke handler (mkInput [("operation", String "push"), ("branch", Number 1)])
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

isExecutionFailed :: Either ToolError ToolOutput -> Bool
isExecutionFailed (Left (ExecutionFailed msg)) =
  Text.isInfixOf "git repository" msg || Text.isInfixOf "not a git repository" msg
isExecutionFailed _ = False

withTempDir :: FilePath -> IO a -> IO a
withTempDir name action = do
  tmp <- getTemporaryDirectory
  let root = tmp </> name
  ignoreIO (removePathForcibly root)
  createDirectoryIfMissing True root
  out <- withCurrentDirectory root action
  ignoreIO (removePathForcibly root)
  pure out

ignoreIO :: IO () -> IO ()
ignoreIO io = io `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()
