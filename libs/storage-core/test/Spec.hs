module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.IO.Temp (withSystemTempDirectory)

import Storage.Core

main :: IO ()
main = defaultMain $ testGroup "storage-core"
  [ testProperty "round-trip putJSON/getJSON" prop_roundTrip
  , testProperty "getValue returns NotFound for missing key" prop_notFound
  , testProperty "deleteValue is idempotent" prop_deleteIdempotent
  , testProperty "listKeys finds written keys" prop_listKeys
  ]

prop_roundTrip :: H.Property
prop_roundTrip = H.property $ do
  val <- H.forAll (Gen.int Range.constantBounded)
  result <- H.evalIO $ withSystemTempDirectory "storage-test" $ \dir -> do
    store <- createStorage dir
    _ <- putJSON store "test" "key" val
    getJSON store "test" "key" :: IO (Either StorageError Int)
  case result of
    Right v -> v === val
    Left e  -> H.footnote (show e) >> H.failure

prop_notFound :: H.Property
prop_notFound = H.property $ do
  result <- H.evalIO $ withSystemTempDirectory "storage-test" $ \dir -> do
    store <- createStorage dir
    getValue store "missing-ns" "missing-key"
  case result of
    Left (NotFound _ _) -> pure ()
    other -> H.footnote (show other) >> H.failure

prop_deleteIdempotent :: H.Property
prop_deleteIdempotent = H.property $ do
  (r1, r2) <- H.evalIO $ withSystemTempDirectory "storage-test" $ \dir -> do
    store <- createStorage dir
    r1 <- deleteValue store "test" "nonexistent"
    r2 <- deleteValue store "test" "nonexistent"
    pure (r1, r2)
  r1 === Right ()
  r2 === Right ()

prop_listKeys :: H.Property
prop_listKeys = H.property $ do
  keys <- H.forAll $ Gen.list (Range.linear 1 5)
            (Gen.text (Range.linear 1 10) Gen.alphaNum)
  result <- H.evalIO $ withSystemTempDirectory "storage-test" $ \dir -> do
    store <- createStorage dir
    mapM_ (\k -> putJSON store "ns" k (42 :: Int)) keys
    listKeys store "ns"
  length result === length keys
