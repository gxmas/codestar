module Anthropic.Client.Internal.RetrySpec (spec) where

import Data.IORef (newIORef, readIORef, modifyIORef')
import Test.Hspec

import Anthropic.Client.Config (RetryPolicy(..))
import Anthropic.Client.Internal.Retry

spec :: Spec
spec = do
  describe "isRetryableStatus" $ do
    it "returns True for 429" $
      isRetryableStatus 429 `shouldBe` True

    it "returns True for 500" $
      isRetryableStatus 500 `shouldBe` True

    it "returns True for 529" $
      isRetryableStatus 529 `shouldBe` True

    it "returns False for 400" $
      isRetryableStatus 400 `shouldBe` False

    it "returns False for 404" $
      isRetryableStatus 404 `shouldBe` False

    it "returns False for 200" $
      isRetryableStatus 200 `shouldBe` False

  describe "withRetry" $ do
    it "succeeds on first try when action succeeds" $ do
      let policy = RetryPolicy 3 100000 1000000 False
          noOpCallback = \_ _ -> pure ()
      result <- withRetry policy noOpCallback (pure 42)
      result `shouldBe` (42 :: Int)

    it "does not fire callback on first attempt" $ do
      ref <- newIORef (0 :: Int)
      let policy = RetryPolicy 3 100000 1000000 False
          callback = \_ _ -> modifyIORef' ref (+1)
      _ <- withRetry policy callback (pure (42 :: Int))
      callCount <- readIORef ref
      callCount `shouldBe` 0
