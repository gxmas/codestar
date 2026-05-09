module Anthropic.Client.ConfigSpec (spec) where

import Test.Hspec

import Anthropic.Types (ApiKey(..))
import Anthropic.Client.Config

spec :: Spec
spec = do
  describe "defaultConfig" $ do
    it "sets api key" $ do
      let ApiKey key = (defaultConfig (ApiKey "sk-test")).apiKey
      key `shouldBe` "sk-test"

    it "sets default retry policy" $ do
      let cfg = defaultConfig (ApiKey "sk-test")
      cfg.retryPolicy.maxRetries `shouldBe` 3
      cfg.retryPolicy.initialDelay `shouldBe` 500000
      cfg.retryPolicy.maxDelay `shouldBe` 30000000
      cfg.retryPolicy.jitter `shouldBe` True

    it "has no base URL override by default" $ do
      let cfg = defaultConfig (ApiKey "sk-test")
      cfg.baseUrl `shouldBe` Nothing

    it "has no beta features by default" $ do
      let cfg = defaultConfig (ApiKey "sk-test")
      cfg.betaFeatures `shouldBe` Nothing

  describe "with* setters" $ do
    it "withBaseUrl sets base URL" $ do
      let cfg = withBaseUrl "https://my-proxy.example.com" (defaultConfig (ApiKey "sk-test"))
      cfg.baseUrl `shouldBe` Just "https://my-proxy.example.com"

    it "withTimeout sets timeout" $ do
      let cfg = withTimeout 60 (defaultConfig (ApiKey "sk-test"))
      cfg.timeout `shouldBe` Just 60

    it "withBetaFeatures sets beta features" $ do
      let cfg = withBetaFeatures ["feature-1", "feature-2"] (defaultConfig (ApiKey "sk-test"))
      cfg.betaFeatures `shouldBe` Just ["feature-1", "feature-2"]

    it "withOnRetry sets retry callback" $ do
      let cfg = withOnRetry (\_ _ -> putStrLn "retry") (defaultConfig (ApiKey "sk-test"))
      -- Callback is set (can't test function equality, verify it type-checks)
      cfg.retryPolicy.maxRetries `shouldBe` 3

  describe "client lifecycle" $ do
    it "withClient creates and closes client" $ do
      let cfg = defaultConfig (ApiKey "sk-test")
      result <- withClient cfg $ \_ -> do
        pure (42 :: Int)
      result `shouldBe` 42

    it "getRateLimits returns Nothing before first request" $ do
      let cfg = defaultConfig (ApiKey "sk-test")
      withClient cfg $ \client -> do
        limits <- getRateLimits client
        limits `shouldBe` Nothing
