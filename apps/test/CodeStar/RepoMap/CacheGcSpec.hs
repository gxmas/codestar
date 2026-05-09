{-# LANGUAGE OverloadedStrings #-}

module CodeStar.RepoMap.CacheGcSpec (spec) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import Test.Hspec

import CodeStar.RepoMap.CacheGc (CacheGcReport (..), runCacheGc)
import CodeStar.Storage (StorageBackend (..))

spec :: Spec
spec = describe "CodeStar.RepoMap.CacheGc" $ do
  it "classifies malformed tag keys as stale-global" $ do
    backend <- mkBackend ["not-a-real-key"]
    report <- runCacheGc backend Nothing False
    let CacheGcReport{scannedEntries = scanned, staleEntries = stale} = report
    scanned `shouldBe` 1
    stale `shouldBe` 1

  it "deletes stale entries when doDelete=True" $ do
    deletedRef <- newIORef ([] :: [Text.Text])
    let backend =
          StorageBackend
            { get = \_ _ -> pure (Left undefined)
            , put = \_ _ _ -> pure (Right ())
            , delete = \_ k -> do
                xs <- readIORef deletedRef
                writeIORef deletedRef (k : xs)
                pure (Right ())
            , list = \_ -> pure ["not-a-real-key", "also-bad"]
            }
    report <- runCacheGc backend Nothing True
    let CacheGcReport{deletedEntries = deletedCount} = report
    deletedCount `shouldBe` 2
    deleted <- readIORef deletedRef
    length deleted `shouldBe` 2

mkBackend :: [Text.Text] -> IO StorageBackend
mkBackend keys =
  pure
    StorageBackend
      { get = \_ _ -> pure (Left undefined)
      , put = \_ _ _ -> pure (Right ())
      , delete = \_ _ -> pure (Right ())
      , list = \_ -> pure keys
      }
