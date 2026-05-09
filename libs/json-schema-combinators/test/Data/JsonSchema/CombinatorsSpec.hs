{-# LANGUAGE OverloadedStrings #-}

module Data.JsonSchema.CombinatorsSpec (spec) where

import Data.Aeson (Value (..))
import Data.Function ((&))
import Data.JsonSchema.Combinators
import qualified Data.JsonSchema.OrderedMap as OM
import Data.JsonSchema.Schema
import Data.JsonSchema.Schema.Internal
  ( schemaType, schemaTitle, schemaDescription, schemaMinimum, schemaMaximum
  , schemaMinLength, schemaMaxLength, schemaFormat, schemaPattern
  , schemaMinItems, schemaMaxItems, schemaUniqueItems
  , schemaProperties, schemaRequired, schemaItems, schemaEnum, schemaConst
  , schemaAllOf, schemaAnyOf, schemaOneOf, schemaNot
  , schemaRef, schemaIf, schemaThen, schemaElse
  , schemaAdditionalProperties, schemaPrefixItems
  , schemaDefault, schemaDeprecated
  , schemaComment, schemaId, schemaSchema
  , schemaDefs
  , schemaExclusiveMinimum, schemaExclusiveMaximum, schemaMultipleOf
  , schemaMinProperties, schemaMaxProperties
  )
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Primitive schemas" $ do
    it "stringSchema has type string" $
      schemaType stringSchema `shouldBe` Just (SingleType SchemaString)

    it "numberSchema has type number" $
      schemaType numberSchema `shouldBe` Just (SingleType SchemaNumber)

    it "integerSchema has type integer" $
      schemaType integerSchema `shouldBe` Just (SingleType SchemaInteger)

    it "booleanSchema has type boolean" $
      schemaType booleanSchema `shouldBe` Just (SingleType SchemaBoolean)

    it "nullSchema has type null" $
      schemaType nullSchema `shouldBe` Just (SingleType SchemaNull)

  describe "objectSchema" $ do
    it "sets type to object" $
      schemaType (objectSchema []) `shouldBe` Just (SingleType SchemaObject)

    it "collects properties" $ do
      let s = objectSchema
                [ required "name" stringSchema
                , optional "age" integerSchema
                ]
      fmap OM.keys (schemaProperties s) `shouldBe` Just ["name", "age"]

    it "derives required from tags" $ do
      let s = objectSchema
                [ required "name" stringSchema
                , optional "age" integerSchema
                ]
      schemaRequired s `shouldBe` Just ["name"]

    it "no required when all optional" $
      schemaRequired (objectSchema [optional "a" stringSchema]) `shouldBe` Nothing

    it "preserves property order" $ do
      let s = objectSchema
                [ required "z" stringSchema
                , required "a" integerSchema
                , required "m" booleanSchema
                ]
      fmap OM.keys (schemaProperties s) `shouldBe` Just ["z", "a", "m"]

  describe "arraySchema" $ do
    it "sets type to array" $
      schemaType (arraySchema stringSchema) `shouldBe` Just (SingleType SchemaArray)

    it "sets items" $
      schemaItems (arraySchema stringSchema) `shouldBe` Just stringSchema

  describe "enumSchema" $ do
    it "sets enum values" $
      schemaEnum (enumSchema [String "a", String "b"]) `shouldBe` Just [String "a", String "b"]

  describe "constSchema" $ do
    it "sets const value" $
      schemaConst (constSchema (String "x")) `shouldBe` Just (String "x")

  describe "withConst" $ do
    it "sets the const field" $
      schemaConst (stringSchema & withConst (String "x"))
        `shouldBe` Just (String "x")

    it "preserves other fields" $ do
      let s = stringSchema
                & withTitle "A title"
                & withDescription "A description"
                & withConst (String "fixed")
      schemaType s `shouldBe` Just (SingleType SchemaString)
      schemaTitle s `shouldBe` Just "A title"
      schemaDescription s `shouldBe` Just "A description"
      schemaConst s `shouldBe` Just (String "fixed")

  describe "nullable" $ do
    it "converts SingleType to UnionType with null" $
      schemaType (nullable stringSchema)
        `shouldBe` Just (UnionType (Set.fromList [SchemaString, SchemaNull]))

    it "inserts null into existing UnionType" $ do
      let s = nullable stringSchema
      schemaType (nullable s)
        `shouldBe` Just (UnionType (Set.fromList [SchemaString, SchemaNull]))

    it "sets SingleType null when no type" $
      schemaType (nullable emptySchema) `shouldBe` Just (SingleType SchemaNull)

  describe "prefixItems" $ do
    it "sets type to array" $
      schemaType (prefixItems [stringSchema, integerSchema])
        `shouldBe` Just (SingleType SchemaArray)

    it "sets prefixItems" $
      schemaPrefixItems (prefixItems [stringSchema, integerSchema])
        `shouldBe` Just [stringSchema, integerSchema]

  describe "Composition" $ do
    it "allOf wraps schemas" $
      schemaAllOf (allOf [stringSchema, integerSchema])
        `shouldBe` Just [stringSchema, integerSchema]

    it "anyOf wraps schemas" $
      schemaAnyOf (anyOf [stringSchema, integerSchema])
        `shouldBe` Just [stringSchema, integerSchema]

    it "oneOf wraps schemas" $
      schemaOneOf (oneOf [stringSchema, integerSchema])
        `shouldBe` Just [stringSchema, integerSchema]

    it "not wraps a schema" $
      schemaNot (Data.JsonSchema.Combinators.not stringSchema) `shouldBe` Just stringSchema

  describe "ref" $ do
    it "sets $ref" $
      schemaRef (ref "#/$defs/Foo") `shouldBe` Just "#/$defs/Foo"

  describe "ifThenElse" $ do
    it "sets if, then, else" $ do
      let s = ifThenElse stringSchema integerSchema booleanSchema
      schemaIf s `shouldBe` Just stringSchema
      schemaThen s `shouldBe` Just integerSchema
      schemaElse s `shouldBe` Just booleanSchema

  describe "Numeric modifiers" $ do
    it "withMinimum" $
      schemaMinimum (withMinimum 5 integerSchema) `shouldBe` Just 5

    it "withMaximum" $
      schemaMaximum (withMaximum 100 numberSchema) `shouldBe` Just 100

    it "withExclusiveMinimum" $
      schemaExclusiveMinimum (withExclusiveMinimum 0 numberSchema) `shouldBe` Just 0

    it "withExclusiveMaximum" $
      schemaExclusiveMaximum (withExclusiveMaximum 10 numberSchema) `shouldBe` Just 10

    it "withMultipleOf" $
      schemaMultipleOf (withMultipleOf 3 integerSchema) `shouldBe` Just 3

  describe "String modifiers" $ do
    it "withMinLength" $
      schemaMinLength (withMinLength 1 stringSchema) `shouldBe` Just 1

    it "withMaxLength" $
      schemaMaxLength (withMaxLength 255 stringSchema) `shouldBe` Just 255

    it "withPattern" $
      schemaPattern (withPattern "^[a-z]+$" stringSchema) `shouldBe` Just "^[a-z]+$"

    it "withFormat" $
      schemaFormat (withFormat "email" stringSchema) `shouldBe` Just "email"

  describe "Array modifiers" $ do
    it "withMinItems" $
      schemaMinItems (withMinItems 1 (arraySchema stringSchema)) `shouldBe` Just 1

    it "withMaxItems" $
      schemaMaxItems (withMaxItems 10 (arraySchema stringSchema)) `shouldBe` Just 10

    it "withUniqueItems true" $
      schemaUniqueItems (withUniqueItems True (arraySchema stringSchema)) `shouldBe` Just True

    it "withUniqueItems false" $
      schemaUniqueItems (withUniqueItems False (arraySchema stringSchema)) `shouldBe` Just False

  describe "Object modifiers" $ do
    it "withAdditionalProperties" $ do
      let s = objectSchema [] & withAdditionalProperties stringSchema
      schemaAdditionalProperties s `shouldBe` Just stringSchema

    it "withMinProperties" $
      schemaMinProperties (withMinProperties 1 (objectSchema [])) `shouldBe` Just 1

    it "withMaxProperties" $
      schemaMaxProperties (withMaxProperties 5 (objectSchema [])) `shouldBe` Just 5

  describe "Annotation modifiers" $ do
    it "withTitle" $
      schemaTitle (withTitle "Foo" stringSchema) `shouldBe` Just "Foo"

    it "withDescription" $
      schemaDescription (withDescription "A thing" stringSchema) `shouldBe` Just "A thing"

    it "withDefault" $
      schemaDefault (withDefault (String "x") stringSchema) `shouldBe` Just (String "x")

    it "withDeprecated" $
      schemaDeprecated (withDeprecated True stringSchema) `shouldBe` Just True

    it "withComment" $
      schemaComment (withComment "note" stringSchema) `shouldBe` Just "note"

  describe "Identity modifiers" $ do
    it "withId" $
      schemaId (withId "https://example.com/schema" emptySchema)
        `shouldBe` Just "https://example.com/schema"

    it "withSchema" $
      schemaSchema (withSchema "https://json-schema.org/draft/2020-12/schema" emptySchema)
        `shouldBe` Just "https://json-schema.org/draft/2020-12/schema"

    it "withDefs" $ do
      let s = emptySchema & withDefs [("Foo", stringSchema)]
      fmap OM.keys (schemaDefs s) `shouldBe` Just ["Foo"]

  describe "Composition via (&)" $ do
    it "chains modifiers left-to-right" $ do
      let s = stringSchema
                & withTitle "Name"
                & withMinLength 1
                & withMaxLength 100
      schemaTitle s `shouldBe` Just "Name"
      schemaMinLength s `shouldBe` Just 1
      schemaMaxLength s `shouldBe` Just 100

  describe "Property tests" $ do
    it "modifier idempotence: applying same modifier twice gives same result" $
      property $ \(NonNegative n) ->
        let nat = fromIntegral (n :: Int) :: Natural
            s1 = withMinLength nat stringSchema
            s2 = withMinLength nat s1
        in s1 == s2

    it "modifier commutativity: order of independent modifiers doesn't matter" $
      property $ \(NonNegative a) (NonNegative b) ->
        let na = fromIntegral (a :: Int) :: Natural
            nb = fromIntegral (b :: Int) :: Natural
            s1 = stringSchema & withMinLength na & withMaxLength nb
            s2 = stringSchema & withMaxLength nb & withMinLength na
        in s1 == s2

    it "withConst idempotence: applying same const twice gives same result" $
      property $ \(NonNegative n) ->
        let v = Number (fromIntegral (n :: Int))
            s1 = withConst v stringSchema
            s2 = withConst v s1
        in s1 == s2

    it "withConst commutes with withTitle" $
      property $ \(NonNegative n) ->
        let v = Number (fromIntegral (n :: Int))
            t = "title"
            s1 = stringSchema & withConst v & withTitle t
            s2 = stringSchema & withTitle t & withConst v
        in s1 == s2
