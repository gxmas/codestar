module CodeStar.Tools.ReadSpec (spec) where

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
import CodeStar.Tools.Read (hasBeenRead, newReadTracker, readToolHandler)
import CodeStar.Tools.Registry
  ( ToolError (..)
  , ToolHandlerDict (..)
  , ToolInput (..)
  , ToolOutput (..)
  )

spec :: Spec
spec = describe "CodeStar.Tools.Read" $ do
  prop "invoke never throws on arbitrary ToolInput" $
    \input -> ioProperty $ do
      tracker <- newReadTracker
      let handler = readToolHandler tracker
      result <- try @SomeException (handler.invoke input)
      pure $ case result of
        Right _ -> property True
        Left ex -> counterexample ("threw: " <> show ex) (property False)
  it "tracks files as read after successful handler invocation" $
    withReadEnv $ \fp -> do
      tracker <- newReadTracker
      let handler = readToolHandler tracker
      hasBeenRead tracker fp `shouldReturn` False
      _ <- invoke handler (mkInput [("path", String (Text.pack fp))])
      hasBeenRead tracker fp `shouldReturn` True

  it "returns numbered lines and total line count" $
    withReadEnv $ \fp -> do
      tracker <- newReadTracker
      let handler = readToolHandler tracker
      res <- invoke handler (mkInput [("path", String (Text.pack fp))])
      case res of
        Left err ->
          expectationFailure ("Expected Right ToolOutput, got: " <> show err)
        Right out -> do
          truncated out `shouldBe` False
          content out `shouldSatisfy` Text.isInfixOf "1\talpha"
          content out `shouldSatisfy` Text.isInfixOf "(3 lines total)"

  it "supports offset and limit arguments" $
    withReadEnv $ \fp -> do
      tracker <- newReadTracker
      let handler = readToolHandler tracker
      res <-
        invoke handler
          ( mkInput
              [ ("path", String (Text.pack fp))
              , ("offset", Number 1)
              , ("limit", Number 1)
              ]
          )
      case res of
        Left err ->
          expectationFailure ("Expected Right ToolOutput, got: " <> show err)
        Right out -> do
          content out `shouldSatisfy` Text.isInfixOf "2\tbeta"
          content out `shouldNotSatisfy` Text.isInfixOf "1\talpha"
          content out `shouldNotSatisfy` Text.isInfixOf "3\tgamma"

  it "rejects malformed input payloads" $
    withReadEnv $ \fp -> do
      tracker <- newReadTracker
      let handler = readToolHandler tracker
          badMissing = mkInput []
          badPathType = mkInput [("path", Number 42)]
          badOffsetType = mkInput [("path", String (Text.pack fp)), ("offset", String "x")]
      r1 <- invoke handler badMissing
      r2 <- invoke handler badPathType
      r3 <- invoke handler badOffsetType
      r1 `shouldSatisfy` isInvalidInput
      r2 `shouldSatisfy` isInvalidInput
      r3 `shouldSatisfy` isInvalidInput

  it "rejects out-of-range offset and limit values" $
    withReadEnv $ \fp -> do
      tracker <- newReadTracker
      let handler = readToolHandler tracker
          badNegativeOffset = mkInput [("path", String (Text.pack fp)), ("offset", Number (-1))]
          badZeroLimit = mkInput [("path", String (Text.pack fp)), ("limit", Number 0)]
      r1 <- invoke handler badNegativeOffset
      r2 <- invoke handler badZeroLimit
      r1 `shouldSatisfy` isInvalidInput
      r2 `shouldSatisfy` isInvalidInput

  it "does not track path when read fails" $ do
    tracker <- newReadTracker
    let handler = readToolHandler tracker
        missing = "/definitely/not/here/codestar-read-missing.txt"
    res <- invoke handler (mkInput [("path", String (Text.pack missing))])
    res `shouldSatisfy` isExecutionFailed
    hasBeenRead tracker missing `shouldReturn` False

mkInput :: [(Text.Text, Value)] -> ToolInput
mkInput entries = ToolInput{arguments = Map.fromList entries}

isInvalidInput :: Either ToolError ToolOutput -> Bool
isInvalidInput (Left (InvalidInput _)) = True
isInvalidInput _ = False

isExecutionFailed :: Either ToolError ToolOutput -> Bool
isExecutionFailed (Left (ExecutionFailed _)) = True
isExecutionFailed _ = False

withReadEnv :: (FilePath -> IO a) -> IO a
withReadEnv action = do
  tmp <- getTemporaryDirectory
  let root = tmp </> "codestar-read-spec"
      fp = root </> "sample.txt"
  ignoreIO (removePathForcibly root)
  createDirectoryIfMissing True root
  Text.IO.writeFile fp "alpha\nbeta\ngamma\n"
  out <- action fp
  ignoreIO (removePathForcibly root)
  pure out

ignoreIO :: IO () -> IO ()
ignoreIO io = io `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()
