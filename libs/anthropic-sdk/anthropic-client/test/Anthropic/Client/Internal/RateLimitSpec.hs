module Anthropic.Client.Internal.RateLimitSpec (spec) where


import qualified Data.CaseInsensitive as CI
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)
import Test.Hspec

import Anthropic.Types (RateLimitInfo(..))
import Anthropic.Client.Internal.RateLimit

spec :: Spec
spec = do
  describe "parseRateLimitHeaders" $ do
    it "parses well-formed headers" $ do
      let hdrs =
            [ (CI.mk "anthropic-ratelimit-requests-limit",       "100")
            , (CI.mk "anthropic-ratelimit-requests-remaining",   "95")
            , (CI.mk "anthropic-ratelimit-requests-reset",       "2026-04-06T12:00:00Z")
            , (CI.mk "anthropic-ratelimit-input-tokens-limit",   "10000")
            , (CI.mk "anthropic-ratelimit-input-tokens-remaining", "9500")
            , (CI.mk "anthropic-ratelimit-input-tokens-reset",   "2026-04-06T12:00:00Z")
            ]
          result = parseRateLimitHeaders hdrs
      case result of
        Just rl -> do
          rl.requestsLimit `shouldBe` 100
          rl.requestsRemaining `shouldBe` 95
          rl.inputTokensLimit `shouldBe` 10000
          rl.inputTokensRemaining `shouldBe` 9500
        Nothing -> expectationFailure "Expected Just RateLimitInfo"

    it "returns Nothing when required headers missing" $ do
      let hdrs = [(CI.mk "anthropic-ratelimit-requests-limit", "100")]
      parseRateLimitHeaders hdrs `shouldBe` Nothing

    it "parses optional output token headers" $ do
      let hdrs =
            [ (CI.mk "anthropic-ratelimit-requests-limit",          "100")
            , (CI.mk "anthropic-ratelimit-requests-remaining",      "95")
            , (CI.mk "anthropic-ratelimit-requests-reset",          "2026-04-06T12:00:00Z")
            , (CI.mk "anthropic-ratelimit-input-tokens-limit",      "10000")
            , (CI.mk "anthropic-ratelimit-input-tokens-remaining",  "9500")
            , (CI.mk "anthropic-ratelimit-input-tokens-reset",      "2026-04-06T12:00:00Z")
            , (CI.mk "anthropic-ratelimit-output-tokens-limit",     "5000")
            , (CI.mk "anthropic-ratelimit-output-tokens-remaining", "4800")
            , (CI.mk "anthropic-ratelimit-output-tokens-reset",     "2026-04-06T12:00:00Z")
            ]
          result = parseRateLimitHeaders hdrs
      case result of
        Just rl -> do
          rl.outputTokensLimit `shouldBe` Just 5000
          rl.outputTokensRemaining `shouldBe` Just 4800
        Nothing -> expectationFailure "Expected Just RateLimitInfo"

    it "parses retry-after header" $ do
      let hdrs =
            [ (CI.mk "anthropic-ratelimit-requests-limit",       "100")
            , (CI.mk "anthropic-ratelimit-requests-remaining",   "0")
            , (CI.mk "anthropic-ratelimit-requests-reset",       "2026-04-06T12:00:00Z")
            , (CI.mk "anthropic-ratelimit-input-tokens-limit",   "10000")
            , (CI.mk "anthropic-ratelimit-input-tokens-remaining", "9500")
            , (CI.mk "anthropic-ratelimit-input-tokens-reset",   "2026-04-06T12:00:00Z")
            , (CI.mk "retry-after", "60")
            ]
          result = parseRateLimitHeaders hdrs
      case result of
        Just rl -> rl.retryAfter `shouldBe` Just 60
        Nothing -> expectationFailure "Expected Just RateLimitInfo"

    it "returns Nothing for unparseable timestamps" $ do
      let hdrs =
            [ (CI.mk "anthropic-ratelimit-requests-limit",       "100")
            , (CI.mk "anthropic-ratelimit-requests-remaining",   "95")
            , (CI.mk "anthropic-ratelimit-requests-reset",       "not-a-timestamp")
            , (CI.mk "anthropic-ratelimit-input-tokens-limit",   "10000")
            , (CI.mk "anthropic-ratelimit-input-tokens-remaining", "9500")
            , (CI.mk "anthropic-ratelimit-input-tokens-reset",   "2026-04-06T12:00:00Z")
            ]
      parseRateLimitHeaders hdrs `shouldBe` Nothing

    it "returns Nothing for unparseable integers" $ do
      let hdrs =
            [ (CI.mk "anthropic-ratelimit-requests-limit",       "not-a-number")
            , (CI.mk "anthropic-ratelimit-requests-remaining",   "95")
            , (CI.mk "anthropic-ratelimit-requests-reset",       "2026-04-06T12:00:00Z")
            , (CI.mk "anthropic-ratelimit-input-tokens-limit",   "10000")
            , (CI.mk "anthropic-ratelimit-input-tokens-remaining", "9500")
            , (CI.mk "anthropic-ratelimit-input-tokens-reset",   "2026-04-06T12:00:00Z")
            ]
      parseRateLimitHeaders hdrs `shouldBe` Nothing

  describe "TVar operations" $ do
    it "newRateLimitState creates empty state" $ do
      tvar <- newRateLimitState
      val <- readRateLimits tvar
      val `shouldBe` Nothing

    it "updateRateLimits updates state" $ do
      tvar <- newRateLimitState
      let ts = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2026-04-06T12:00:00Z" :: UTCTime
          rl = RateLimitInfo 100 95 ts 10000 9500 ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      updateRateLimits tvar rl
      val <- readRateLimits tvar
      val `shouldBe` Just rl
