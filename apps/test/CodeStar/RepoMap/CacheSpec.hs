{-# LANGUAGE OverloadedStrings #-}

module CodeStar.RepoMap.CacheSpec (spec) where

import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import CodeStar.RepoMap.Cache (RepoMapCache (..), getOrComputeTags)
import CodeStar.RepoMap.Graph (Tag (..), TagKind (..))

genTag :: Gen Tag
genTag = do
  name <- listOf1 (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> "_"))
  lineNum <- chooseInt (0, 2000)
  kind <- elements [Definition, Reference]
  pure
    Tag
      { tagFile = "dummy.hs"
      , tagName = Text.pack name
      , tagLine = lineNum
      , tagKind = kind
      }

spec :: Spec
spec =
  describe "RepoMap cache helper invariants" $ do
    it "computes once and reuses cached tags for stable mtime key" $
      property $
        forAll (listOf genTag) $ \computedTags ->
          monadicIO $ do
            cacheRef <- run (newIORef (Nothing :: Maybe [Tag]))
            computeCount <- run (newIORef (0 :: Int))
            putCount <- run (newIORef (0 :: Int))
            let cache =
                  RepoMapCache
                    { getTags = \_ _ -> readIORef cacheRef
                    , putTags = \_ _ tags -> do
                        modifyIORef' putCount (+ 1)
                        writeIORef cacheRef (Just tags)
                    , getMap = \_ -> pure Nothing
                    , putMap = \_ _ -> pure ()
                    , invalidate = \_ -> pure ()
                    }
                compute = do
                  modifyIORef' computeCount (+ 1)
                  pure computedTags
            first <- run (getOrComputeTags cache "codestar-cli/CLI.hs" compute)
            second <- run (getOrComputeTags cache "codestar-cli/CLI.hs" compute)
            c <- run (readIORef computeCount)
            p <- run (readIORef putCount)
            assert (first == computedTags)
            assert (second == computedTags)
            assert (c == 1)
            assert (p == 1)
