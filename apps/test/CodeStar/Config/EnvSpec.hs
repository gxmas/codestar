{-# LANGUAGE OverloadedStrings #-}

module CodeStar.Config.EnvSpec (spec) where

import Control.Exception (bracket_)
import Data.Monoid (Last (..))
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

import CodeStar.Config.Env (loadFromEnv)
import CodeStar.Config.Types qualified as CT

spec :: Spec
spec = describe "CodeStar.Config.Env" $ do
  it "reads CODESTAR_API_KEY into partial config" $
    withEnv "CODESTAR_API_KEY" "env-key-123" $ do
      pc <- loadFromEnv
      let CT.PartialConfig{CT.apiKey = k} = pc
      getLast k `shouldBe` Just (CT.ApiKey "env-key-123")

  it "prefers CODESTAR_API_KEY over ANTHROPIC_API_KEY" $
    withEnv "ANTHROPIC_API_KEY" "anthropic-fallback" $
      withEnv "CODESTAR_API_KEY" "codestar-override" $ do
        pc <- loadFromEnv
        let CT.PartialConfig{CT.apiKey = k} = pc
        getLast k `shouldBe` Just (CT.ApiKey "codestar-override")

  it "parses numeric and telemetry env values when provided" $
    withEnv "CODESTAR_SERVER_PORT" "9123" $
      withEnv "CODESTAR_TELEMETRY_MODE" "stderr" $
        withEnv "CODESTAR_TELEMETRY_METRICS_BIND_HOST" "0.0.0.0" $ do
          pc <- loadFromEnv
          let CT.PartialConfig
                { CT.server = CT.PartialServerSection{CT.port = p}
                , CT.telemetry = CT.PartialTelemetrySection{CT.mode = m, CT.metricsBindHost = mbh}
                } = pc
          getLast p `shouldBe` Just 9123
          getLast m `shouldBe` Just CT.TelemetryStderr
          getLast mbh `shouldBe` Just "0.0.0.0"

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
