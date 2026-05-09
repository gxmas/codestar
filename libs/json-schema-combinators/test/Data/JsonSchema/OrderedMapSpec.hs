{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.JsonSchema.OrderedMapSpec (spec) where

import Data.JsonSchema.OrderedMap as OM
import qualified Data.Map.Strict as Map
import Prelude hiding (lookup, null)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "OrderedMap" $ do
    describe "empty" $ do
      it "has size 0" $
        OM.size (OM.empty :: OrderedMap Int Int) `shouldBe` 0

      it "is null" $
        OM.null (OM.empty :: OrderedMap Int Int) `shouldBe` True

      it "toList is []" $
        OM.toList (OM.empty :: OrderedMap Int Int) `shouldBe` []

    describe "singleton" $ do
      it "has size 1" $
        OM.size (OM.singleton ("k" :: String) (1 :: Int)) `shouldBe` 1

      it "lookup finds the value" $
        OM.lookup ("k" :: String) (OM.singleton "k" (42 :: Int)) `shouldBe` Just 42

      it "toList returns one pair" $
        OM.toList (OM.singleton ("k" :: String) (42 :: Int)) `shouldBe` [("k", 42 :: Int)]

    describe "fromList" $ do
      it "preserves insertion order" $
        OM.toList (OM.fromList [("b", 2 :: Int), ("a", 1), ("c", 3)] :: OrderedMap String Int)
          `shouldBe` [("b", 2), ("a", 1), ("c", 3)]

      it "duplicate keys: last value wins" $
        OM.lookup ("a" :: String) (OM.fromList [("a", 1 :: Int), ("a", 2)])
          `shouldBe` Just 2

      it "duplicate keys: first occurrence position preserved" $
        OM.toList (OM.fromList [("a", 1 :: Int), ("b", 2), ("a", 3)] :: OrderedMap String Int)
          `shouldBe` [("a", 3), ("b", 2)]

      it "empty list produces empty map" $
        OM.fromList ([] :: [(String, Int)]) `shouldBe` OM.empty

    describe "lookup" $ do
      it "finds existing key" $
        OM.lookup ("b" :: String) (OM.fromList [("a", 1 :: Int), ("b", 2)]) `shouldBe` Just 2

      it "returns Nothing for missing key" $
        OM.lookup ("z" :: String) (OM.fromList [("a", 1 :: Int)]) `shouldBe` Nothing

    describe "insert" $ do
      it "adds a new key at the end" $
        OM.toList (OM.insert ("c" :: String) 3 (OM.fromList [("a", 1 :: Int), ("b", 2)]))
          `shouldBe` [("a", 1), ("b", 2), ("c", 3)]

      it "replaces value for existing key, preserving position" $
        OM.toList (OM.insert ("a" :: String) 99 (OM.fromList [("a", 1 :: Int), ("b", 2)]))
          `shouldBe` [("a", 99), ("b", 2)]

    describe "Eq" $ do
      it "order-insensitive equality" $
        (OM.fromList [("a", 1 :: Int), ("b", 2)] :: OrderedMap String Int)
          `shouldBe` OM.fromList [("b", 2), ("a", 1)]

      it "different values are not equal" $
        (OM.fromList [("a", 1 :: Int)] :: OrderedMap String Int) `shouldNotBe` OM.fromList [("a", 2)]

    describe "Semigroup" $ do
      it "left-biased union" $ do
        let l = OM.fromList [("a", 1 :: Int), ("b", 2)] :: OrderedMap String Int
            r = OM.fromList [("b", 99), ("c", 3)] :: OrderedMap String Int
        OM.lookup ("b" :: String) (l <> r) `shouldBe` Just 2
        OM.toList (l <> r) `shouldBe` [("a", 1), ("b", 2), ("c", 3)]

    describe "Monoid" $ do
      it "mempty is identity" $
        (OM.fromList [("a", 1 :: Int)] :: OrderedMap String Int) <> mempty
          `shouldBe` OM.fromList [("a", 1)]

    describe "property tests" $ do
      it "fromList/toList round-trip preserves order" $
        property $ \(kvs :: [(Int, Int)]) ->
          let om = OM.fromList kvs
          in OM.fromList (OM.toList om) == om

      it "lookup agrees with Map.lookup" $
        property $ \(kvs :: [(Int, Int)]) (k :: Int) ->
          let om = OM.fromList kvs
              m  = Map.fromList kvs
          in OM.lookup k om == Map.lookup k m

      it "Monoid left identity" $
        property $ \(kvs :: [(Int, Int)]) ->
          let om = OM.fromList kvs
          in mempty <> om == om

      it "Monoid right identity" $
        property $ \(kvs :: [(Int, Int)]) ->
          let om = OM.fromList kvs
          in om <> mempty == om
