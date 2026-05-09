module CodeStar.Tools.WriteSpec (spec) where

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
import CodeStar.Tools.Registry
  ( ToolError (..)
  , ToolHandlerDict (..)
  , ToolInput (..)
  , ToolOutput (..)
  )
import CodeStar.Tools.Write (writeToolHandler)

spec :: Spec
spec = describe "CodeStar.Tools.Write" $ do
  prop "invoke never throws on arbitrary ToolInput" $
    \input -> ioProperty $ do
      let handler = writeToolHandler Nothing
      result <- try @SomeException (handler.invoke input)
      pure $ case result of
        Right _ -> property True
        Left ex -> counterexample ("threw: " <> show ex) (property False)
  it "creates a new file and returns success message" $
    withWriteEnv $ \fp -> do
      let handler = writeToolHandler Nothing
      res <-
        invoke handler $
          mkInput
            [ ("path", String (Text.pack fp))
            , ("content", String "hello from spec")
            ]
      case res of
        Left err ->
          expectationFailure ("Expected Right ToolOutput, got: " <> show err)
        Right out -> do
          content out `shouldBe` "File written."
          truncated out `shouldBe` False
          Text.IO.readFile fp `shouldReturn` "hello from spec"

  it "rejects large overwrite unless force=true" $
    withWriteEnv $ \fp -> do
      let handler = writeToolHandler Nothing
          big = Text.unlines (replicate 250 "line")
      Text.IO.writeFile fp big
      denied <-
        invoke handler $
          mkInput
            [ ("path", String (Text.pack fp))
            , ("content", String "new")
            ]
      denied `shouldSatisfy` isInvalidInput
      Text.IO.readFile fp `shouldReturn` big
      allowed <-
        invoke handler $
          mkInput
            [ ("path", String (Text.pack fp))
            , ("content", String "forced")
            , ("force", Bool True)
            ]
      allowed `shouldSatisfy` isRightResult
      Text.IO.readFile fp `shouldReturn` "forced"

  it "allows overwrite exactly at maxOverwriteLines boundary without force" $
    withWriteEnv $ \fp -> do
      let handler = writeToolHandler Nothing
          exactBoundary = Text.unlines (replicate 200 "line")
      Text.IO.writeFile fp exactBoundary
      res <-
        invoke handler $
          mkInput
            [ ("path", String (Text.pack fp))
            , ("content", String "boundary-ok")
            ]
      res `shouldSatisfy` isRightResult
      Text.IO.readFile fp `shouldReturn` "boundary-ok"

  it "returns ExecutionFailed when writing to an invalid target" $ do
    let handler = writeToolHandler Nothing
        dirPath = "/"
    res <-
      invoke handler $
        mkInput
          [ ("path", String (Text.pack dirPath))
          , ("content", String "nope")
          ]
    res `shouldSatisfy` isExecutionFailed

  it "reports InvalidInput for malformed payloads" $
    withWriteEnv $ \fp -> do
      let handler = writeToolHandler Nothing
      r1 <- invoke handler (mkInput [("path", String (Text.pack fp))])
      r2 <- invoke handler (mkInput [("path", Number 123), ("content", String "x")])
      r3 <- invoke handler (mkInput [("path", String (Text.pack fp)), ("content", String "x"), ("force", String "yes")])
      r1 `shouldSatisfy` isInvalidInput
      r2 `shouldSatisfy` isInvalidInput
      r3 `shouldSatisfy` isInvalidInput

mkInput :: [(Text.Text, Value)] -> ToolInput
mkInput entries = ToolInput{arguments = Map.fromList entries}

isInvalidInput :: Either ToolError ToolOutput -> Bool
isInvalidInput (Left (InvalidInput _)) = True
isInvalidInput _ = False

isRightResult :: Either ToolError ToolOutput -> Bool
isRightResult (Right _) = True
isRightResult _ = False

isExecutionFailed :: Either ToolError ToolOutput -> Bool
isExecutionFailed (Left (ExecutionFailed _)) = True
isExecutionFailed _ = False

withWriteEnv :: (FilePath -> IO a) -> IO a
withWriteEnv action = do
  tmp <- getTemporaryDirectory
  let root = tmp </> "codestar-write-spec"
      fp = root </> "target.txt"
  ignoreIO (removePathForcibly root)
  createDirectoryIfMissing True root
  out <- action fp
  ignoreIO (removePathForcibly root)
  pure out

ignoreIO :: IO () -> IO ()
ignoreIO io = io `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()
