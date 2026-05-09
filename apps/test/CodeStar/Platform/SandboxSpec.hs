{-# LANGUAGE OverloadedStrings #-}

module CodeStar.Platform.SandboxSpec (spec) where

import Control.Exception (IOException, catch)
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec

import CodeStar.Platform.Sandbox

spec :: Spec
spec = describe "CodeStar.Platform.Sandbox" $ do
  it "defaultSandboxConfig initializes expected defaults" $ do
    let cfg = defaultSandboxConfig "/tmp/ws"
    cfg.workspaceMount `shouldBe` "/tmp/ws"
    cfg.networkDisabled `shouldBe` True
    cfg.cpuLimit `shouldBe` "2"

  it "noSandbox succeeds for successful host command" $ do
    let sb = noSandbox "."
    res <- sb.runCommand "true"
    res `shouldBe` Right ""

  it "noSandbox returns failure on non-zero command exit" $ do
    let sb = noSandbox "."
    res <- sb.runCommand "false"
    res `shouldSatisfy` isLeft

  it "noSandbox executes commands inside configured workspace" $
    withTempSandboxDir "codestar-sandbox-spec" $ \root -> do
      let marker = root </> "marker.txt"
          sb = noSandbox root
      _ <- sb.runCommand "touch marker.txt"
      doesFileExist marker `shouldReturn` True

  it "noSandbox failure includes exit code in message" $ do
    let sb = noSandbox "."
    res <- sb.runCommand "exit 7"
    res `shouldBe` Left "exit 7"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

withTempSandboxDir :: FilePath -> (FilePath -> IO a) -> IO a
withTempSandboxDir name action = do
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
