module Anthropic.Types.CacheSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON, object, (.=))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Types
import Anthropic.Types.Generators ()

spec :: Spec
spec = do
  describe "CacheTTL" $ do
    prop "roundtrip" $ \(x :: CacheTTL) ->
      eitherDecode (encode x) === Right x

    it "TTL5Min -> 300" $
      toJSON TTL5Min `shouldBe` toJSON (300 :: Int)

    it "TTL1Hour -> 3600" $
      toJSON TTL1Hour `shouldBe` toJSON (3600 :: Int)

  describe "CacheControl" $ do
    prop "roundtrip" $ \(x :: CacheControl) ->
      eitherDecode (encode x) === Right x

    it "default (no TTL) wire format" $
      toJSON (CacheControl Nothing) `shouldBe` object ["type" .= ("ephemeral" :: String)]

    it "with TTL wire format" $
      toJSON (CacheControl (Just TTL1Hour))
        `shouldBe` object ["type" .= ("ephemeral" :: String), "ttl" .= (3600 :: Int)]
