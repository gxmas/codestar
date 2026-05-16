{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE RecordWildCards #-}
module CodeStar.Config.ValidateSpec (spec) where

import Data.List.NonEmpty (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Last (..))
import Test.Hspec hiding (context)

import CodeStar.Config.Types
import CodeStar.Config.Validate (resolve)

spec :: Spec
spec = describe "CodeStar.Config.Validate" $ do
  it "fails when api key is missing" $ do
    let partial = mempty :: PartialConfig
    resolve partial `shouldSatisfy` hasMissingApiKey

  it "applies defaults and resolves successfully with provided api key" $ do
    let partial = let PartialConfig{..} = mempty :: PartialConfig
                  in PartialConfig{apiKey = Last (Just (ApiKey "k")), ..}
    case resolve partial of
      Left err -> expectationFailure ("Expected Right Config, got Left: " <> show err)
      Right cfg -> do
        let Config{provider = p, apiKey = k} = cfg
        p `shouldBe` "anthropic"
        k `shouldBe` ApiKey "k"

  it "reports invalid ranges and port conflicts" $ do
    let partial =
          let PartialConfig{..} = mempty :: PartialConfig
          in PartialConfig
            { apiKey = Last (Just (ApiKey "k"))
            , server =
                PartialServerSection
                  { port = Last (Just 70000)
                  , host = Last Nothing
                  , httpTimeout = Last Nothing
                  , gracefulShutdownTimeout = Last Nothing
                  , pingInterval = Last Nothing
                  }
            , telemetry =
                PartialTelemetrySection
                  { mode = Last Nothing
                  , serviceName = Last Nothing
                  , endpoint = Last Nothing
                  , logToStderr = Last Nothing
                  , metricsEnabled = Last Nothing
                  , metricsBindHost = Last Nothing
                  , metricsPort = Last (Just (Just 70000))
                  , sampleRate = Last Nothing
                  }
            , ..
            }
    resolve partial `shouldSatisfy` hasInvalidRange

hasMissingApiKey :: Either (NonEmpty ConfigError) a -> Bool
hasMissingApiKey (Left errs) = any isMissing (toList errs)
 where
  isMissing (MissingRequired _) = True
  isMissing _ = False
hasMissingApiKey _ = False

hasInvalidRange :: Either (NonEmpty ConfigError) a -> Bool
hasInvalidRange (Left errs) = any isRange (toList errs)
 where
  isRange InvalidRange{} = True
  isRange _ = False
hasInvalidRange _ = False
