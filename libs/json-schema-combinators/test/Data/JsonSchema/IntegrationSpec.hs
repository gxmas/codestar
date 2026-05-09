{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests that build realistic JSON Schema documents using the
-- public API. Each test exercises a real-world schema pattern that a library
-- consumer would encounter, validating both ergonomics and correctness.
--
-- These tests import only from 'Data.JsonSchema' (the public surface) plus
-- 'Data.JsonSchema.Schema.Internal' for structural assertions on schema
-- fields. The (&) composition style is used throughout to demonstrate the
-- intended left-to-right workflow.
module Data.JsonSchema.IntegrationSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Function ((&))
import Data.JsonSchema
import Data.JsonSchema.Schema.Internal
  ( schemaConst, schemaDefs, schemaEnum, schemaExclusiveMinimum
  , schemaFormat, schemaItems, schemaMaximum, schemaMinItems
  , schemaMinimum, schemaAdditionalProperties, schemaPatternProperties
  , schemaProperties, schemaRef, schemaRequired, schemaSchema
  , schemaType, schemaUniqueItems
  )
import qualified Data.JsonSchema.OrderedMap as OM
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Hspec

spec :: Spec
spec = do
  describe "LLM tool-calling schema" llmToolCallingSpec
  describe "User profile with $defs and $ref" userProfileSpec
  describe "Paginated API response" apiResponseSpec
  describe "Application configuration" configFileSpec
  describe "E-commerce product (oneOf)" ecommerceProductSpec

-- ---------------------------------------------------------------------------
-- 1. LLM Tool-Calling Schema
-- ---------------------------------------------------------------------------

-- | Schema for an LLM tool call (e.g. Anthropic tool_use format).
-- Demonstrates: nested objects, withConst, withSchema, withDescription.
llmToolCallSchema :: Schema
llmToolCallSchema =
  objectSchema
    [ required "name" $ stringSchema
        & withDescription "The tool name"
    , optional "description" $ stringSchema
        & withDescription "What the tool does"
    , required "input_schema" $ objectSchema
        [ required "type" $ stringSchema
            & withConst (String "object")
        , required "properties" $ objectSchema []
        , optional "required" $ arraySchema stringSchema
        ]
    ]
  & withSchema "https://json-schema.org/draft/2020-12/schema"

llmToolCallingSpec :: Spec
llmToolCallingSpec = do
  it "builds with correct top-level structure" $ do
    schemaType llmToolCallSchema
      `shouldBe` Just (SingleType SchemaObject)
    schemaSchema llmToolCallSchema
      `shouldBe` Just "https://json-schema.org/draft/2020-12/schema"

  it "has the expected required fields" $
    schemaRequired llmToolCallSchema
      `shouldBe` Just ["name", "input_schema"]

  it "input_schema has nested required [type, properties]" $ do
    let mInputSchema = do
          props <- schemaProperties llmToolCallSchema
          OM.lookup "input_schema" props
    fmap schemaRequired mInputSchema
      `shouldBe` Just (Just ["type", "properties"])

  it "type property uses const" $ do
    let mTypeSchema = do
          props <- schemaProperties llmToolCallSchema
          inputSchema <- OM.lookup "input_schema" props
          innerProps <- schemaProperties inputSchema
          OM.lookup "type" innerProps
    fmap schemaConst mTypeSchema
      `shouldBe` Just (Just (String "object"))

  it "round-trips through encode/decode" $
    decode (encode llmToolCallSchema) `shouldBe` Right llmToolCallSchema

  it "encodes to expected JSON structure" $ do
    let json = encode llmToolCallSchema
    lookupKey "$schema" json
      `shouldBe` Just (String "https://json-schema.org/draft/2020-12/schema")
    lookupKey "type" json
      `shouldBe` Just (String "object")
    nestedLookup ["properties", "name", "type"] json
      `shouldBe` Just (String "string")

-- ---------------------------------------------------------------------------
-- 2. User Profile with $defs and $ref
-- ---------------------------------------------------------------------------

-- | Reusable address definition.
-- Demonstrates: withMinLength, withMaxLength, withPattern on strings.
addressDef :: Schema
addressDef = objectSchema
  [ required "street" stringSchema
  , required "city" stringSchema
  , required "state" $ stringSchema
      & withMinLength 2
      & withMaxLength 2
      & withPattern "^[A-Z]{2}$"
  , required "zip" $ stringSchema
      & withPattern "^\\d{5}(-\\d{4})?$"
  ]

-- | User profile referencing Address via $ref.
-- Demonstrates: ref, withDefs, nullable, enumSchema, withFormat.
userProfileSchema :: Schema
userProfileSchema =
  objectSchema
    [ required "id" $ stringSchema
        & withFormat "uuid"
    , required "email" $ stringSchema
        & withFormat "email"
    , required "name" $ stringSchema
        & withMinLength 1
    , optional "phone" $ stringSchema
        & withFormat "phone"
        & nullable
    , optional "role" $ enumSchema
        [ String "admin"
        , String "editor"
        , String "viewer"
        ]
    , required "homeAddress" $ ref "#/$defs/Address"
    , optional "workAddress" $ ref "#/$defs/Address"
        & nullable
    ]
  & withDefs [("Address", addressDef)]

userProfileSpec :: Spec
userProfileSpec = do
  it "top-level is an object with correct required fields" $ do
    schemaType userProfileSchema
      `shouldBe` Just (SingleType SchemaObject)
    schemaRequired userProfileSchema
      `shouldBe` Just ["id", "email", "name", "homeAddress"]

  it "defines Address in $defs" $ do
    let mDefs = schemaDefs userProfileSchema
    fmap OM.keys mDefs `shouldBe` Just ["Address"]

  it "Address definition has four required fields" $ do
    let mAddr = schemaDefs userProfileSchema >>= OM.lookup "Address"
    fmap schemaRequired mAddr
      `shouldBe` Just (Just ["street", "city", "state", "zip"])

  it "homeAddress uses $ref to Address" $ do
    let mHome = schemaProperties userProfileSchema >>= OM.lookup "homeAddress"
    fmap schemaRef mHome
      `shouldBe` Just (Just "#/$defs/Address")

  it "phone is nullable string" $ do
    let mPhone = schemaProperties userProfileSchema >>= OM.lookup "phone"
    case mPhone of
      Nothing -> expectationFailure "phone property not found"
      Just phone -> do
        schemaType phone `shouldSatisfy` isNullableType SchemaString
        schemaFormat phone `shouldBe` Just "phone"

  it "role uses enum" $ do
    let mRole = schemaProperties userProfileSchema >>= OM.lookup "role"
    fmap schemaEnum mRole
      `shouldBe` Just (Just [String "admin", String "editor", String "viewer"])

  it "round-trips through encode/decode" $
    decode (encode userProfileSchema) `shouldBe` Right userProfileSchema

-- ---------------------------------------------------------------------------
-- 3. Paginated API Response
-- ---------------------------------------------------------------------------

-- | Error object within an API response.
errorObjectSchema :: Schema
errorObjectSchema = objectSchema
  [ required "code" integerSchema
  , required "message" stringSchema
  , optional "field" $ stringSchema & nullable
  ]

-- | Pagination metadata.
-- Demonstrates: withMinimum, withMaximum on integers.
paginationSchema :: Schema
paginationSchema = objectSchema
  [ required "page" $ integerSchema & withMinimum 1
  , required "pageSize" $ integerSchema & withMinimum 1 & withMaximum 100
  , required "total" $ integerSchema & withMinimum 0
  ]

-- | Paginated API response wrapper.
-- Demonstrates: nested object composition, nullable arrays, withDescription.
apiResponseSchema :: Schema
apiResponseSchema = objectSchema
  [ required "data" $ arraySchema (objectSchema [])
      & withDescription "Array of result items"
  , required "pagination" paginationSchema
  , optional "errors" $ arraySchema errorObjectSchema
      & nullable
  ]

apiResponseSpec :: Spec
apiResponseSpec = do
  it "top-level has data, pagination, errors properties" $ do
    let propNames = fmap OM.keys (schemaProperties apiResponseSchema)
    propNames `shouldBe` Just ["data", "pagination", "errors"]

  it "only data and pagination are required" $
    schemaRequired apiResponseSchema
      `shouldBe` Just ["data", "pagination"]

  it "pagination.page has minimum 1" $ do
    let mPage = do
          props <- schemaProperties apiResponseSchema
          pagination <- OM.lookup "pagination" props
          innerProps <- schemaProperties pagination
          OM.lookup "page" innerProps
    fmap schemaMinimum mPage `shouldBe` Just (Just 1)

  it "pagination.pageSize has minimum 1 and maximum 100" $ do
    let mPageSize = do
          props <- schemaProperties apiResponseSchema
          pagination <- OM.lookup "pagination" props
          innerProps <- schemaProperties pagination
          OM.lookup "pageSize" innerProps
    fmap schemaMinimum mPageSize `shouldBe` Just (Just 1)
    fmap schemaMaximum mPageSize `shouldBe` Just (Just 100)

  it "errors is a nullable array of error objects" $ do
    let mErrors = schemaProperties apiResponseSchema >>= OM.lookup "errors"
    case mErrors of
      Nothing -> expectationFailure "errors property not found"
      Just errors -> do
        schemaType errors `shouldSatisfy` isNullableType SchemaArray
        case schemaItems errors of
          Nothing -> expectationFailure "errors items not set"
          Just itemSchema ->
            schemaRequired itemSchema `shouldBe` Just ["code", "message"]

  it "error object field is nullable string" $ do
    let mField = schemaProperties errorObjectSchema >>= OM.lookup "field"
    case mField of
      Nothing -> expectationFailure "field property not found"
      Just field -> schemaType field `shouldSatisfy` isNullableType SchemaString

  it "round-trips through encode/decode" $
    decode (encode apiResponseSchema) `shouldBe` Right apiResponseSchema

-- ---------------------------------------------------------------------------
-- 4. Configuration File Schema
-- ---------------------------------------------------------------------------

-- | Database configuration with conditional validation.
-- Demonstrates: withWriteOnly, withAdditionalProperties, allOf + ifThenElse.
databaseConfigSchema :: Schema
databaseConfigSchema = objectSchema
  [ required "host" stringSchema
  , required "port" $ integerSchema
      & withMinimum 1
      & withMaximum 65535
  , required "name" $ stringSchema
      & withDescription "Database name"
  , optional "username" stringSchema
  , optional "password" $ stringSchema
      & withWriteOnly True
  , optional "ssl" booleanSchema
  , optional "certPath" stringSchema
  ]

-- | Database config with conditional: if ssl is true, certPath is required.
-- Demonstrates: ifThenElse composed with allOf to layer constraints.
databaseConfigWithConditional :: Schema
databaseConfigWithConditional =
  allOf
    [ databaseConfigSchema
    , ifThenElse
        -- if: ssl property equals true
        (objectSchema [required "ssl" $ constSchema (Bool True)])
        -- then: certPath is required
        (objectSchema [required "certPath" stringSchema])
        -- else: no additional constraints
        emptySchema
    ]

-- | Logging configuration.
-- Demonstrates: enumSchema for constrained string sets.
loggingConfigSchema :: Schema
loggingConfigSchema = objectSchema
  [ required "level" $ enumSchema
      [ String "debug", String "info", String "warn", String "error" ]
  , optional "format" $ enumSchema
      [ String "json", String "text" ]
  ]

-- | Feature flags as an open-ended boolean map.
-- Demonstrates: withAdditionalProperties for typed dynamic keys.
featureFlagsSchema :: Schema
featureFlagsSchema =
  objectSchema []
    & withAdditionalProperties booleanSchema

-- | Full application configuration.
appConfigSchema :: Schema
appConfigSchema = objectSchema
  [ required "database" databaseConfigWithConditional
  , required "logging" loggingConfigSchema
  , optional "featureFlags" featureFlagsSchema
  ]

configFileSpec :: Spec
configFileSpec = do
  it "database port has range 1..65535" $ do
    let mPort = schemaProperties databaseConfigSchema >>= OM.lookup "port"
    fmap schemaMinimum mPort `shouldBe` Just (Just 1)
    fmap schemaMaximum mPort `shouldBe` Just (Just 65535)

  it "password is writeOnly" $ do
    let json = encode databaseConfigSchema
    nestedLookup ["properties", "password", "writeOnly"] json
      `shouldBe` Just (Bool True)

  it "logging level is an enum of four values" $ do
    let mLevel = schemaProperties loggingConfigSchema >>= OM.lookup "level"
    fmap schemaEnum mLevel `shouldBe`
      Just (Just [String "debug", String "info", String "warn", String "error"])

  it "featureFlags allows boolean additionalProperties" $
    schemaAdditionalProperties featureFlagsSchema
      `shouldBe` Just booleanSchema

  it "conditional schema uses if/then/else for ssl -> certPath" $ do
    let json = encode databaseConfigWithConditional
    case lookupKey "allOf" json of
      Just (Array arr) -> V.length arr `shouldBe` 2
      _ -> expectationFailure "Expected allOf with 2 elements"

  it "if-schema checks ssl: true" $ do
    let json = encode databaseConfigWithConditional
    case lookupKey "allOf" json of
      Just (Array arr) -> do
        let conditional = arr V.! 1
        case lookupKey "if" conditional of
          Just ifVal ->
            nestedLookup ["properties", "ssl", "const"] ifVal
              `shouldBe` Just (Bool True)
          Nothing -> expectationFailure "Expected 'if' in conditional schema"
      _ -> expectationFailure "Expected allOf array"

  it "round-trips through encode/decode" $
    decode (encode appConfigSchema) `shouldBe` Right appConfigSchema

-- ---------------------------------------------------------------------------
-- 5. E-commerce Product Schema (oneOf)
-- ---------------------------------------------------------------------------

-- | Physical product variant.
-- Demonstrates: constSchema for discriminator, nested objects, withExclusiveMinimum.
physicalProductSchema :: Schema
physicalProductSchema = objectSchema
  [ required "type" $ constSchema (String "physical")
  , required "weight" $ numberSchema
      & withExclusiveMinimum 0
      & withDescription "Weight in kg"
  , required "dimensions" $ objectSchema
      [ required "length" $ numberSchema & withExclusiveMinimum 0
      , required "width"  $ numberSchema & withExclusiveMinimum 0
      , required "height" $ numberSchema & withExclusiveMinimum 0
      ]
  ]

-- | Digital product variant.
digitalProductSchema :: Schema
digitalProductSchema = objectSchema
  [ required "type" $ constSchema (String "digital")
  , required "downloadUrl" $ stringSchema
      & withFormat "uri"
  , required "fileSize" $ integerSchema
      & withExclusiveMinimum 0
      & withDescription "File size in bytes"
  ]

-- | Shared product fields with $defs for variants and patternProperties
-- for extension metadata.
-- Demonstrates: withDefs, withPatternProperties, withUniqueItems, withMinItems.
productSchema :: Schema
productSchema =
  objectSchema
    [ required "name" $ stringSchema
        & withMinLength 1
    , required "price" $ numberSchema
        & withExclusiveMinimum 0
    , optional "description" stringSchema
    , required "tags" $ arraySchema stringSchema
        & withUniqueItems True
        & withMinItems 1
    ]
  & withDefs
      [ ("PhysicalProduct", physicalProductSchema)
      , ("DigitalProduct", digitalProductSchema)
      ]
  & withPatternProperties
      [ ("^x-", stringSchema) ]

-- | Full product schema: shared fields + oneOf for product type discrimination.
-- Demonstrates: allOf + oneOf + ref composition.
fullProductSchema :: Schema
fullProductSchema =
  allOf
    [ productSchema
    , oneOf
        [ ref "#/$defs/PhysicalProduct"
        , ref "#/$defs/DigitalProduct"
        ]
    ]

ecommerceProductSpec :: Spec
ecommerceProductSpec = do
  it "price has exclusiveMinimum 0" $ do
    let mPrice = schemaProperties productSchema >>= OM.lookup "price"
    fmap schemaExclusiveMinimum mPrice `shouldBe` Just (Just 0)

  it "tags has uniqueItems and minItems 1" $ do
    let mTags = schemaProperties productSchema >>= OM.lookup "tags"
    fmap schemaUniqueItems mTags `shouldBe` Just (Just True)
    fmap schemaMinItems mTags `shouldBe` Just (Just 1)

  it "defines PhysicalProduct and DigitalProduct in $defs" $ do
    let defKeys = fmap OM.keys (schemaDefs productSchema)
    defKeys `shouldBe` Just ["PhysicalProduct", "DigitalProduct"]

  it "physical product requires weight and dimensions" $
    schemaRequired physicalProductSchema
      `shouldBe` Just ["type", "weight", "dimensions"]

  it "digital product requires downloadUrl and fileSize" $
    schemaRequired digitalProductSchema
      `shouldBe` Just ["type", "downloadUrl", "fileSize"]

  it "patternProperties maps ^x- to string" $ do
    let mPP = schemaPatternProperties productSchema
    case mPP of
      Nothing -> expectationFailure "patternProperties not set"
      Just pp -> OM.lookup "^x-" pp `shouldBe` Just stringSchema

  it "fullProductSchema uses allOf with oneOf" $ do
    let json = encode fullProductSchema
    case lookupKey "allOf" json of
      Just (Array arr) -> do
        V.length arr `shouldBe` 2
        -- Second element should have oneOf
        case lookupKey "oneOf" (arr V.! 1) of
          Just (Array refs) -> V.length refs `shouldBe` 2
          _ -> expectationFailure "Expected oneOf with 2 elements"
      _ -> expectationFailure "Expected allOf array"

  it "oneOf refs point to $defs" $ do
    let json = encode fullProductSchema
    case lookupKey "allOf" json >>= vectorAt 1 >>= lookupKey "oneOf" of
      Just (Array refs) -> do
        lookupKey "$ref" (refs V.! 0)
          `shouldBe` Just (String "#/$defs/PhysicalProduct")
        lookupKey "$ref" (refs V.! 1)
          `shouldBe` Just (String "#/$defs/DigitalProduct")
      _ -> expectationFailure "Could not reach oneOf refs"

  it "round-trips through encode/decode" $ do
    decode (encode productSchema) `shouldBe` Right productSchema
    decode (encode fullProductSchema) `shouldBe` Right fullProductSchema

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

-- | Look up a top-level key in a JSON Value (assumes Object).
lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object o) = KM.lookup (Key.fromText k) o
lookupKey _ _ = Nothing

-- | Traverse nested keys in a JSON Value.
nestedLookup :: [Text] -> Value -> Maybe Value
nestedLookup [] v = Just v
nestedLookup (k:ks) v = lookupKey k v >>= nestedLookup ks

-- | Safe index into a JSON Array.
vectorAt :: Int -> Value -> Maybe Value
vectorAt i (Array arr) = arr V.!? i
vectorAt _ _ = Nothing

-- | Check whether a TypeSpec is a nullable version of the given SchemaType.
isNullableType :: SchemaType -> Maybe TypeSpec -> Bool
isNullableType t (Just (UnionType ts)) =
  Set.member SchemaNull ts && Set.member t ts
isNullableType _ _ = False
