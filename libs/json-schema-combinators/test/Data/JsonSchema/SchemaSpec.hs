{-# LANGUAGE OverloadedStrings #-}

module Data.JsonSchema.SchemaSpec (spec) where

import Data.JsonSchema.Schema
import Data.JsonSchema.Schema.Internal (schemaType)
import qualified Data.Set as Set
import Test.Hspec

spec :: Spec
spec = do
  describe "SchemaType" $ do
    it "has all seven constructors" $ do
      let types = [SchemaString, SchemaNumber, SchemaInteger, SchemaBoolean,
                   SchemaArray, SchemaObject, SchemaNull]
      length types `shouldBe` 7

    it "derives Eq" $
      SchemaString `shouldBe` SchemaString

    it "derives Ord" $
      compare SchemaString SchemaNull `shouldNotBe` EQ

    it "derives Show" $
      show SchemaString `shouldBe` "SchemaString"

  describe "TypeSpec" $ do
    it "SingleType wraps a SchemaType" $
      SingleType SchemaString `shouldBe` SingleType SchemaString

    it "UnionType wraps a Set" $ do
      let ts = UnionType (Set.fromList [SchemaString, SchemaNull])
      ts `shouldBe` UnionType (Set.fromList [SchemaNull, SchemaString])

    it "derives Show" $
      show (SingleType SchemaInteger) `shouldBe` "SingleType SchemaInteger"

  describe "emptySchema" $ do
    it "has no type" $
      schemaType emptySchema `shouldBe` Nothing

    it "equals itself" $
      emptySchema `shouldBe` emptySchema
