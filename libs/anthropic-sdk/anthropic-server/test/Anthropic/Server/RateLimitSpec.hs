module Anthropic.Server.RateLimitSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Anthropic.Types (ApiKey (..), ModelId (..), Usage (..))
import Anthropic.Server.RateLimit

spec :: Spec
spec = do
  describe "RateLimiter" $ do
    it "allows requests within limits" $ do
      limiter <- newRateLimiter RateLimitConfig
        { requestsLimit = 100
        , inputTokensLimit = 10000
        , outputTokensLimit = 5000
        }
      result <- checkRateLimit limiter (ApiKey "sk-test") (ModelId "claude") 100
      result `shouldBe` Allowed

    it "rate limits when request quota exceeded" $ do
      limiter <- newRateLimiter RateLimitConfig
        { requestsLimit = 1
        , inputTokensLimit = 10000
        , outputTokensLimit = 5000
        }
      -- First request should succeed
      result1 <- checkRateLimit limiter (ApiKey "sk-test") (ModelId "claude") 100
      result1 `shouldBe` Allowed

      -- Second request should be rate limited
      result2 <- checkRateLimit limiter (ApiKey "sk-test") (ModelId "claude") 100
      result2 `shouldSatisfy` isRateLimited

    it "rate limits when input token quota exceeded" $ do
      limiter <- newRateLimiter RateLimitConfig
        { requestsLimit = 100
        , inputTokensLimit = 50
        , outputTokensLimit = 5000
        }
      result <- checkRateLimit limiter (ApiKey "sk-test") (ModelId "claude") 100
      result `shouldSatisfy` isRateLimited

    it "tracks separate quotas per API key" $ do
      limiter <- newRateLimiter RateLimitConfig
        { requestsLimit = 1
        , inputTokensLimit = 10000
        , outputTokensLimit = 5000
        }
      -- Use up quota for key1
      _ <- checkRateLimit limiter (ApiKey "sk-key1") (ModelId "claude") 100

      -- key2 should still have quota
      result <- checkRateLimit limiter (ApiKey "sk-key2") (ModelId "claude") 100
      result `shouldBe` Allowed

    it "tracks separate quotas per model" $ do
      limiter <- newRateLimiter RateLimitConfig
        { requestsLimit = 1
        , inputTokensLimit = 10000
        , outputTokensLimit = 5000
        }
      -- Use up quota for model1
      _ <- checkRateLimit limiter (ApiKey "sk-test") (ModelId "model1") 100

      -- model2 should still have quota
      result <- checkRateLimit limiter (ApiKey "sk-test") (ModelId "model2") 100
      result `shouldBe` Allowed

    it "consumes tokens correctly" $ do
      limiter <- newRateLimiter RateLimitConfig
        { requestsLimit = 100
        , inputTokensLimit = 1000
        , outputTokensLimit = 1000
        }
      let usage = Usage
            { inputTokens = 100
            , outputTokens = 50
            , cacheCreationInputTokens = Nothing
            , cacheReadInputTokens = Nothing
            , cacheCreation = Nothing
            , serverToolUse = Nothing
            , serviceTier = Nothing
            , inferenceGeo = Nothing
            }

      _ <- checkRateLimit limiter (ApiKey "sk-test") (ModelId "claude") 100
      consumeTokens limiter (ApiKey "sk-test") (ModelId "claude") usage

      -- Tokens consumed, but should still have capacity for another small request
      result <- checkRateLimit limiter (ApiKey "sk-test") (ModelId "claude") 50
      result `shouldBe` Allowed

    it "returns rate limit headers" $ do
      limiter <- newRateLimiter RateLimitConfig
        { requestsLimit = 100
        , inputTokensLimit = 10000
        , outputTokensLimit = 5000
        }
      headers <- getRateLimitHeaders limiter (ApiKey "sk-test") (ModelId "claude")

      headers `shouldSatisfy` (not . null)
      headers `shouldSatisfy` hasHeader "anthropic-ratelimit-requests-limit"
      headers `shouldSatisfy` hasHeader "anthropic-ratelimit-input-tokens-limit"

    it "property: concurrent requests don't exceed limit" $ property $
      forAll (choose (1, 10)) $ \limit ->
        forAll (choose (10, 50)) $ \numRequests ->
          numRequests > limit ==> ioProperty $ do
            limiter <- newRateLimiter RateLimitConfig
              { requestsLimit = limit
              , inputTokensLimit = 100000
              , outputTokensLimit = 100000
              }

            results <- mapM (\_ -> checkRateLimit limiter (ApiKey "sk-test") (ModelId "claude") 10)
                            [1..numRequests]

            let allowedCount = length $ filter (== Allowed) results
            pure $ allowedCount <= limit

isRateLimited :: RateLimitResult -> Bool
isRateLimited (RateLimited _ _) = True
isRateLimited _ = False

hasHeader :: Show a => String -> [(a, b)] -> Bool
hasHeader name headers = any (\(k, _) -> show k == show name) headers
