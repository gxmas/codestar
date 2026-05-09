{-# LANGUAGE OverloadedStrings #-}

module Data.JsonSchema.SerializationSpec (spec) where

import Data.Aeson (Value (..), toJSON, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Function ((&))
import Data.JsonSchema.Combinators
import Data.JsonSchema.Schema
import Data.JsonSchema.Schema.Internal (schemaComment, schemaMinLength, schemaType)
import Data.JsonSchema.Serialization
import qualified Data.Set as Set
import Test.Hspec

spec :: Spec
spec = do
  describe "encode" $ do
    it "emptySchema encodes to empty object" $
      encode emptySchema `shouldBe` object []

    it "stringSchema encodes type field" $
      encode stringSchema `shouldBe` object ["type" .= String "string"]

    it "object with properties" $ do
      let s = objectSchema
                [ required "name" stringSchema
                , optional "age" integerSchema
                ]
          v = encode s
      case v of
        Object o -> do
          KM.lookup "type" o `shouldBe` Just (String "object")
          KM.lookup "required" o `shouldBe` Just (toJSON [String "name"])
          case KM.lookup "properties" o of
            Just (Object props) -> do
              KM.member "name" props `shouldBe` True
              KM.member "age" props `shouldBe` True
            _ -> expectationFailure "expected properties object"
        _ -> expectationFailure "expected object"

    it "maps ref to $ref" $ do
      let v = encode (ref "#/$defs/Foo")
      case v of
        Object o -> KM.lookup "$ref" o `shouldBe` Just (String "#/$defs/Foo")
        _ -> expectationFailure "expected object"

    it "maps defs to $defs" $ do
      let s = emptySchema & withDefs [("Foo", stringSchema)]
          v = encode s
      case v of
        Object o -> KM.member "$defs" o `shouldBe` True
        _ -> expectationFailure "expected object"

    it "maps id to $id" $ do
      let v = encode (emptySchema & withId "https://example.com")
      case v of
        Object o -> KM.lookup "$id" o `shouldBe` Just (String "https://example.com")
        _ -> expectationFailure "expected object"

    it "maps schema to $schema" $ do
      let v = encode (emptySchema & withSchema "https://json-schema.org/draft/2020-12/schema")
      case v of
        Object o -> KM.lookup "$schema" o
          `shouldBe` Just (String "https://json-schema.org/draft/2020-12/schema")
        _ -> expectationFailure "expected object"

    it "maps comment to $comment" $ do
      let v = encode (stringSchema & withComment "a note")
      case v of
        Object o -> KM.lookup "$comment" o `shouldBe` Just (String "a note")
        _ -> expectationFailure "expected object"

    it "maps anchor to $anchor" $ do
      let v = encode (emptySchema & withAnchor "foo")
      case v of
        Object o -> KM.lookup "$anchor" o `shouldBe` Just (String "foo")
        _ -> expectationFailure "expected object"

    it "maps dynamicAnchor to $dynamicAnchor" $ do
      let v = encode (emptySchema & withDynamicAnchor "bar")
      case v of
        Object o -> KM.lookup "$dynamicAnchor" o `shouldBe` Just (String "bar")
        _ -> expectationFailure "expected object"

    it "maps dynamicRef to $dynamicRef" $ do
      let v = encode (emptySchema & withDynamicRef "#baz")
      case v of
        Object o -> KM.lookup "$dynamicRef" o `shouldBe` Just (String "#baz")
        _ -> expectationFailure "expected object"

    it "omits Nothing fields" $ do
      let v = encode stringSchema
      case v of
        Object o -> KM.size o `shouldBe` 1  -- only "type"
        _ -> expectationFailure "expected object"

    it "encodes UnionType as array" $ do
      let s = nullable stringSchema
          v = encode s
      case v of
        Object o -> case KM.lookup "type" o of
          Just (Array _) -> pure ()
          _ -> expectationFailure "expected type to be an array"
        _ -> expectationFailure "expected object"

    it "encodes Natural fields as numbers" $ do
      let v = encode (stringSchema & withMinLength 5)
      case v of
        Object o -> KM.lookup "minLength" o `shouldBe` Just (Number 5)
        _ -> expectationFailure "expected object"

    it "encodes ifThenElse" $ do
      let s = ifThenElse stringSchema integerSchema booleanSchema
          v = encode s
      case v of
        Object o -> do
          KM.member "if" o `shouldBe` True
          KM.member "then" o `shouldBe` True
          KM.member "else" o `shouldBe` True
        _ -> expectationFailure "expected object"

    it "encodes prefixItems" $ do
      let s = prefixItems [stringSchema, integerSchema]
          v = encode s
      case v of
        Object o -> KM.member "prefixItems" o `shouldBe` True
        _ -> expectationFailure "expected object"

  describe "decode" $ do
    it "decodes empty object to emptySchema" $
      decode (object []) `shouldBe` Right emptySchema

    it "decodes string type" $
      decode (object ["type" .= String "string"]) `shouldBe` Right stringSchema

    it "decodes $ref" $
      decode (object ["$ref" .= String "#/$defs/Foo"]) `shouldBe` Right (ref "#/$defs/Foo")

    it "decodes $id" $ do
      let v = object ["$id" .= String "https://example.com"]
      decode v `shouldBe` Right (emptySchema & withId "https://example.com")

    it "decodes $comment" $ do
      let v = object ["$comment" .= String "note"]
      case decode v of
        Right s -> schemaComment s `shouldBe` Just "note"
        Left e -> expectationFailure (show e)

    it "ignores unknown fields" $
      decode (object ["type" .= String "string", "x-custom" .= String "ignored"])
        `shouldBe` Right stringSchema

    it "returns DecodeError for non-object" $
      decode (String "not an object") `shouldBe`
        Left (DecodeError "Expected a JSON object" [])

    it "returns DecodeError with path for bad type" $
      case decode (object ["type" .= Number 42]) of
        Left (DecodeError _ path) -> path `shouldBe` ["type"]
        Right _ -> expectationFailure "expected decode error"

    it "decodes UnionType" $ do
      let v = object ["type" .= toJSON [String "string", String "null"]]
      case decode v of
        Right s -> schemaType s `shouldBe` Just (UnionType (Set.fromList [SchemaString, SchemaNull]))
        Left e -> expectationFailure (show e)

    it "decodes Natural fields" $ do
      let v = object ["type" .= String "string", "minLength" .= Number 5]
      case decode v of
        Right s -> schemaMinLength s `shouldBe` Just 5
        Left e -> expectationFailure (show e)

  describe "round-trip" $ do
    it "emptySchema round-trips" $
      decode (encode emptySchema) `shouldBe` Right emptySchema

    it "stringSchema round-trips" $
      decode (encode stringSchema) `shouldBe` Right stringSchema

    it "complex schema round-trips" $ do
      let s = objectSchema
                [ required "name" $ stringSchema & withMinLength 1
                , optional "age" $ integerSchema & withMinimum 0
                ]
                & withTitle "User"
                & withDescription "A user record"
      decode (encode s) `shouldBe` Right s

    it "nullable schema round-trips" $
      decode (encode (nullable stringSchema)) `shouldBe` Right (nullable stringSchema)

    it "schema with $ref round-trips" $
      decode (encode (ref "#/$defs/Foo")) `shouldBe` Right (ref "#/$defs/Foo")

    it "schema with $defs round-trips" $ do
      let s = emptySchema & withDefs [("Foo", stringSchema), ("Bar", integerSchema)]
      decode (encode s) `shouldBe` Right s

    it "schema with allOf round-trips" $ do
      let s = allOf [stringSchema, integerSchema]
      decode (encode s) `shouldBe` Right s

    it "schema with ifThenElse round-trips" $ do
      let s = ifThenElse stringSchema integerSchema booleanSchema
      decode (encode s) `shouldBe` Right s

    it "schema with all annotations round-trips" $ do
      let s = stringSchema
                & withTitle "T"
                & withDescription "D"
                & withDefault (String "x")
                & withExamples [String "a", String "b"]
                & withDeprecated True
                & withReadOnly False
                & withWriteOnly False
                & withComment "C"
      decode (encode s) `shouldBe` Right s

    it "schema with numeric constraints round-trips" $ do
      let s = numberSchema
                & withMinimum 0
                & withMaximum 100
                & withExclusiveMinimum (-1)
                & withExclusiveMaximum 101
                & withMultipleOf 0.5
      decode (encode s) `shouldBe` Right s

    it "schema with array constraints round-trips" $ do
      let s = arraySchema stringSchema
                & withMinItems 1
                & withMaxItems 10
                & withUniqueItems True
      decode (encode s) `shouldBe` Right s

    it "schema with identity fields round-trips" $ do
      let s = emptySchema
                & withId "https://example.com/schema"
                & withSchema "https://json-schema.org/draft/2020-12/schema"
                & withAnchor "foo"
                & withDynamicAnchor "bar"
                & withDynamicRef "#baz"
      decode (encode s) `shouldBe` Right s
