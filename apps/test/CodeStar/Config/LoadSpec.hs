{-# LANGUAGE OverloadedStrings #-}

module CodeStar.Config.LoadSpec (spec) where

import Control.Exception (bracket_)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text qualified as Text
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

import CodeStar.Config.Load (globalSettingsDir, loadConfig, projectSettingsDir)
import CodeStar.Config.Types (ConfigError (..), RunArgs (..), TelemetryMode (..))

spec :: Spec
spec = describe "CodeStar.Config.Load" $ do
  it "projectSettingsDir appends .codestar to workspace path" $
    projectSettingsDir "/tmp/ws" `shouldBe` "/tmp/ws/.codestar"

  it "globalSettingsDir resolves a non-empty path" $ do
    dir <- globalSettingsDir
    dir `shouldSatisfy` (not . null)

  it "loadConfig reports missing API key when env provides none" $
    withClearedKeys $ do
      result <- loadConfig defaultRunArgs
      result `shouldSatisfy` hasMissingApiKeyError

  it "loadConfig succeeds when CODESTAR_API_KEY is present" $
    withEnv "CODESTAR_API_KEY" "spec-key" $
      withEnv "ANTHROPIC_API_KEY" "" $ do
        result <- loadConfig defaultRunArgs
        result `shouldSatisfy` isRightResult

defaultRunArgs :: RunArgs
defaultRunArgs =
  RunArgs
    { cliModel = Nothing
    , cliPort = Nothing
    , cliHeadless = False
    , cliTask = Nothing
    , cliWorkspace = Nothing
    , cliTelemetry = TelemetryOff
    }

hasMissingApiKeyError :: Either (NonEmpty ConfigError) a -> Bool
hasMissingApiKeyError (Left errs) = any isMissingApi (toList errs)
 where
  isMissingApi (MissingRequired msg) = Text.isInfixOf "api_key" msg
  isMissingApi _ = False
hasMissingApiKeyError _ = False

isRightResult :: Either a b -> Bool
isRightResult (Right _) = True
isRightResult _ = False

withClearedKeys :: IO a -> IO a
withClearedKeys action = do
  old1 <- lookupEnv "CODESTAR_API_KEY"
  old2 <- lookupEnv "ANTHROPIC_API_KEY"
  bracket_
    (unsetEnv "CODESTAR_API_KEY" >> unsetEnv "ANTHROPIC_API_KEY")
    (restore "CODESTAR_API_KEY" old1 >> restore "ANTHROPIC_API_KEY" old2)
    action

withEnv :: String -> String -> IO a -> IO a
withEnv key val action = do
  old <- lookupEnv key
  bracket_
    (setEnv key val)
    (restore key old)
    action

restore :: String -> Maybe String -> IO ()
restore key (Just v) = setEnv key v
restore key Nothing = unsetEnv key
