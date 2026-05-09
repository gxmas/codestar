module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.ByteString as BS
import Cache.Core

main :: IO ()
main = defaultMain $ testGroup "cache-core"
  [ testProperty "cache miss returns Nothing" prop_miss
  , testProperty "cache hit returns stored value" prop_hit
  , testProperty "cacheInvalidate removes entry" prop_invalidate
  , testProperty "cacheClear empties namespace" prop_clear
  ]

prop_miss :: H.Property
prop_miss = H.property $ do
  store <- H.evalIO newCacheStore
  result <- H.evalIO (cacheGet store "ns" "missing")
  result === Nothing

prop_hit :: H.Property
prop_hit = H.property $ do
  bytes <- H.forAll (Gen.bytes (Range.linear 1 100))
  store <- H.evalIO newCacheStore
  H.evalIO (cachePut store "ns" "key" bytes Nothing)
  result <- H.evalIO (cacheGet store "ns" "key")
  result === Just bytes

prop_invalidate :: H.Property
prop_invalidate = H.property $ do
  store <- H.evalIO newCacheStore
  H.evalIO (cachePut store "ns" "key" (BS.pack [1,2,3]) Nothing)
  H.evalIO (cacheInvalidate store "ns" "key")
  result <- H.evalIO (cacheGet store "ns" "key")
  result === Nothing

prop_clear :: H.Property
prop_clear = H.property $ do
  store <- H.evalIO newCacheStore
  H.evalIO (cachePut store "ns" "k1" (BS.pack [1]) Nothing)
  H.evalIO (cachePut store "ns" "k2" (BS.pack [2]) Nothing)
  H.evalIO (cacheClear store "ns")
  stats <- H.evalIO (getCacheStats store "ns")
  stats.statEntryCount === 0
