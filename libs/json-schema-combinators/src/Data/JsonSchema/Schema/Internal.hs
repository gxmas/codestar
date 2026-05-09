{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.JsonSchema.Schema.Internal
-- Description : Raw Schema record type with all JSON Schema draft 2020-12 fields
-- Copyright   : (c) 2026 Geoffrey Noel
-- License     : MIT
--
-- __Stability: internal.__ The record field names and constructors exported
-- here are /not/ part of the stable API and may change between minor
-- versions. Prefer the combinators in "Data.JsonSchema.Combinators" for
-- constructing schemas and the re-exports in "Data.JsonSchema.Schema"
-- for the types.
--
-- This module is the escape hatch for power users who need direct record
-- access or pattern matching on schema fields.
--
-- == Schema record field groups
--
-- The 'Schema' record organises its fields into the following groups,
-- mirroring the JSON Schema draft 2020-12 specification:
--
-- * __Identity & dialect__ -- @$id@, @$schema@, @$anchor@, @$dynamicAnchor@, @$vocabulary@, @$defs@
-- * __Type__ -- @type@ (single or union)
-- * __Annotations__ -- @title@, @description@, @default@, @examples@, @deprecated@, @readOnly@, @writeOnly@, @$comment@
-- * __Numeric validation__ -- @multipleOf@, @minimum@, @exclusiveMinimum@, @maximum@, @exclusiveMaximum@
-- * __String validation__ -- @minLength@, @maxLength@, @pattern@, @format@
-- * __Array validation__ -- @items@, @prefixItems@, @contains@, @minContains@, @maxContains@, @minItems@, @maxItems@, @uniqueItems@, @unevaluatedItems@
-- * __Object validation__ -- @properties@, @patternProperties@, @additionalProperties@, @required@, @propertyNames@, @minProperties@, @maxProperties@, @dependentSchemas@, @dependentRequired@, @unevaluatedProperties@
-- * __Composition__ -- @allOf@, @anyOf@, @oneOf@, @not@
-- * __Conditionals__ -- @if@, @then@, @else@
-- * __References__ -- @$ref@, @$dynamicRef@
-- * __Content__ -- @contentEncoding@, @contentMediaType@, @contentSchema@
-- * __Enum\/const__ -- @enum@, @const@
module Data.JsonSchema.Schema.Internal
  ( -- * Schema type
    Schema (..)

    -- * Type specification
  , SchemaType (..)
  , TypeSpec (..)

    -- * Empty schema
  , emptySchema
  ) where

import Data.Aeson (Value)
import Data.JsonSchema.OrderedMap (OrderedMap)
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Text (Text)
import Numeric.Natural (Natural)

-- | JSON Schema type keywords.
data SchemaType
  = SchemaString
  | SchemaNumber
  | SchemaInteger
  | SchemaBoolean
  | SchemaArray
  | SchemaObject
  | SchemaNull
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Type specification: a single type or a union of types.
--
-- @
-- { "type": "string" }              -- SingleType SchemaString
-- { "type": ["string", "null"] }    -- UnionType (Set.fromList [SchemaString, SchemaNull])
-- @
data TypeSpec
  = SingleType !SchemaType
  | UnionType !(Set SchemaType)
  deriving (Eq, Ord, Show)

-- | A JSON Schema (draft 2020-12) document.
--
-- All fields are optional (@Maybe@). Use 'emptySchema' as the starting
-- point and the combinators in "Data.JsonSchema.Combinators" to set fields.
--
-- Prefer the combinator API over direct record construction -- the combinators
-- enforces that type-specific constraints are only applied to schemas of
-- the appropriate type.
data Schema = Schema
  { -- ** Identity & dialect

    -- | @$id@ -- the canonical URI identifier for this schema.
    schemaId              :: !(Maybe Text)
    -- | @$schema@ -- the meta-schema URI (e.g., @\"https:\/\/json-schema.org\/draft\/2020-12\/schema\"@).
  , schemaSchema          :: !(Maybe Text)
    -- | @$anchor@ -- a plain-name anchor for @$ref@ targeting within the same document.
  , schemaAnchor          :: !(Maybe Text)
    -- | @$dynamicAnchor@ -- a dynamic anchor for recursive schema extension.
  , schemaDynamicAnchor   :: !(Maybe Text)
    -- | @$vocabulary@ -- declares which vocabularies are required or optional.
  , schemaVocabulary      :: !(Maybe (OrderedMap Text Bool))
    -- | @$defs@ -- reusable schema definitions, referenced via @$ref@.
  , schemaDefs            :: !(Maybe (OrderedMap Text Schema))

    -- ** Type

    -- | @type@ -- the type constraint, either a single type or a union.
  , schemaType            :: !(Maybe TypeSpec)

    -- ** Annotations

    -- | @title@ -- a short human-readable label.
  , schemaTitle           :: !(Maybe Text)
    -- | @description@ -- a longer human-readable explanation.
  , schemaDescription     :: !(Maybe Text)
    -- | @default@ -- a default value for documentation or code generation.
  , schemaDefault         :: !(Maybe Value)
    -- | @examples@ -- sample values illustrating valid instances.
  , schemaExamples        :: !(Maybe [Value])
    -- | @deprecated@ -- marks the schema as deprecated.
  , schemaDeprecated      :: !(Maybe Bool)
    -- | @readOnly@ -- indicates the value should not be modified.
  , schemaReadOnly        :: !(Maybe Bool)
    -- | @writeOnly@ -- indicates the value should not be read (e.g., passwords).
  , schemaWriteOnly       :: !(Maybe Bool)
    -- | @$comment@ -- a comment for schema maintainers (not for end users).
  , schemaComment         :: !(Maybe Text)

    -- ** Numeric validation

    -- | @multipleOf@ -- the value must be a multiple of this number.
  , schemaMultipleOf      :: !(Maybe Scientific)
    -- | @minimum@ -- inclusive lower bound.
  , schemaMinimum         :: !(Maybe Scientific)
    -- | @exclusiveMinimum@ -- exclusive lower bound.
  , schemaExclusiveMinimum :: !(Maybe Scientific)
    -- | @maximum@ -- inclusive upper bound.
  , schemaMaximum         :: !(Maybe Scientific)
    -- | @exclusiveMaximum@ -- exclusive upper bound.
  , schemaExclusiveMaximum :: !(Maybe Scientific)

    -- ** String validation

    -- | @minLength@ -- minimum string length (in Unicode code points).
  , schemaMinLength       :: !(Maybe Natural)
    -- | @maxLength@ -- maximum string length (in Unicode code points).
  , schemaMaxLength       :: !(Maybe Natural)
    -- | @pattern@ -- an ECMA-262 regular expression the string must match.
  , schemaPattern         :: !(Maybe Text)
    -- | @format@ -- a semantic format hint (e.g., @\"email\"@, @\"date-time\"@, @\"uri\"@).
  , schemaFormat          :: !(Maybe Text)

    -- ** Array validation

    -- | @items@ -- schema for all array elements (or elements beyond @prefixItems@).
  , schemaItems           :: !(Maybe Schema)
    -- | @prefixItems@ -- positional schemas for tuple validation.
  , schemaPrefixItems     :: !(Maybe [Schema])
    -- | @contains@ -- at least one element must match this schema.
  , schemaContains        :: !(Maybe Schema)
    -- | @minContains@ -- minimum number of elements matching @contains@.
  , schemaMinContains     :: !(Maybe Natural)
    -- | @maxContains@ -- maximum number of elements matching @contains@.
  , schemaMaxContains     :: !(Maybe Natural)
    -- | @minItems@ -- minimum array length.
  , schemaMinItems        :: !(Maybe Natural)
    -- | @maxItems@ -- maximum array length.
  , schemaMaxItems        :: !(Maybe Natural)
    -- | @uniqueItems@ -- all elements must be distinct.
  , schemaUniqueItems     :: !(Maybe Bool)
    -- | @unevaluatedItems@ -- schema for items not covered by @items@ or @prefixItems@.
  , schemaUnevaluatedItems :: !(Maybe Schema)

    -- ** Object validation

    -- | @properties@ -- schemas for named properties (insertion order preserved).
  , schemaProperties      :: !(Maybe (OrderedMap Text Schema))
    -- | @patternProperties@ -- schemas keyed by regex patterns matching property names.
  , schemaPatternProperties :: !(Maybe (OrderedMap Text Schema))
    -- | @additionalProperties@ -- schema for properties not matched by @properties@ or @patternProperties@.
  , schemaAdditionalProperties :: !(Maybe Schema)
    -- | @required@ -- property names that must be present.
  , schemaRequired        :: !(Maybe [Text])
    -- | @propertyNames@ -- schema that all property names must satisfy.
  , schemaPropertyNames   :: !(Maybe Schema)
    -- | @minProperties@ -- minimum number of properties.
  , schemaMinProperties   :: !(Maybe Natural)
    -- | @maxProperties@ -- maximum number of properties.
  , schemaMaxProperties   :: !(Maybe Natural)
    -- | @dependentSchemas@ -- schemas that apply when a given property is present.
  , schemaDependentSchemas :: !(Maybe (OrderedMap Text Schema))
    -- | @dependentRequired@ -- properties that become required when a given property is present.
  , schemaDependentRequired :: !(Maybe (OrderedMap Text [Text]))
    -- | @unevaluatedProperties@ -- schema for properties not covered by @properties@, @patternProperties@, or @additionalProperties@.
  , schemaUnevaluatedProperties :: !(Maybe Schema)

    -- ** Composition

    -- | @allOf@ -- the instance must match /all/ of these schemas.
  , schemaAllOf           :: !(Maybe [Schema])
    -- | @anyOf@ -- the instance must match /at least one/ of these schemas.
  , schemaAnyOf           :: !(Maybe [Schema])
    -- | @oneOf@ -- the instance must match /exactly one/ of these schemas.
  , schemaOneOf           :: !(Maybe [Schema])
    -- | @not@ -- the instance must /not/ match this schema.
  , schemaNot             :: !(Maybe Schema)

    -- ** Conditionals

    -- | @if@ -- the condition schema for conditional validation.
  , schemaIf              :: !(Maybe Schema)
    -- | @then@ -- applied when @if@ matches.
  , schemaThen            :: !(Maybe Schema)
    -- | @else@ -- applied when @if@ does not match.
  , schemaElse            :: !(Maybe Schema)

    -- ** References

    -- | @$ref@ -- a JSON Pointer or URI reference to another schema.
  , schemaRef             :: !(Maybe Text)
    -- | @$dynamicRef@ -- a dynamic reference resolved at evaluation time.
  , schemaDynamicRef      :: !(Maybe Text)

    -- ** Content

    -- | @contentEncoding@ -- encoding of the string content (e.g., @\"base64\"@).
  , schemaContentEncoding :: !(Maybe Text)
    -- | @contentMediaType@ -- MIME type of the string content (e.g., @\"application\/json\"@).
  , schemaContentMediaType :: !(Maybe Text)
    -- | @contentSchema@ -- schema for the decoded string content.
  , schemaContentSchema   :: !(Maybe Schema)

    -- ** Enum\/const

    -- | @enum@ -- the instance must be one of these values.
  , schemaEnum            :: !(Maybe [Value])
    -- | @const@ -- the instance must be exactly this value.
  , schemaConst           :: !(Maybe Value)
  }
  deriving (Eq, Show)

-- | A schema with all fields set to 'Nothing'. This is the identity
-- element for modifier composition: for any modifier @f@,
-- @f emptySchema@ yields a schema with only that single field set.
-- All constructors in "Data.JsonSchema.Combinators" are defined
-- in terms of 'emptySchema'.
emptySchema :: Schema
emptySchema = Schema
  { schemaId = Nothing
  , schemaSchema = Nothing
  , schemaAnchor = Nothing
  , schemaDynamicAnchor = Nothing
  , schemaVocabulary = Nothing
  , schemaDefs = Nothing
  , schemaType = Nothing
  , schemaTitle = Nothing
  , schemaDescription = Nothing
  , schemaDefault = Nothing
  , schemaExamples = Nothing
  , schemaDeprecated = Nothing
  , schemaReadOnly = Nothing
  , schemaWriteOnly = Nothing
  , schemaComment = Nothing
  , schemaMultipleOf = Nothing
  , schemaMinimum = Nothing
  , schemaExclusiveMinimum = Nothing
  , schemaMaximum = Nothing
  , schemaExclusiveMaximum = Nothing
  , schemaMinLength = Nothing
  , schemaMaxLength = Nothing
  , schemaPattern = Nothing
  , schemaFormat = Nothing
  , schemaItems = Nothing
  , schemaPrefixItems = Nothing
  , schemaContains = Nothing
  , schemaMinContains = Nothing
  , schemaMaxContains = Nothing
  , schemaMinItems = Nothing
  , schemaMaxItems = Nothing
  , schemaUniqueItems = Nothing
  , schemaUnevaluatedItems = Nothing
  , schemaProperties = Nothing
  , schemaPatternProperties = Nothing
  , schemaAdditionalProperties = Nothing
  , schemaRequired = Nothing
  , schemaPropertyNames = Nothing
  , schemaMinProperties = Nothing
  , schemaMaxProperties = Nothing
  , schemaDependentSchemas = Nothing
  , schemaDependentRequired = Nothing
  , schemaUnevaluatedProperties = Nothing
  , schemaAllOf = Nothing
  , schemaAnyOf = Nothing
  , schemaOneOf = Nothing
  , schemaNot = Nothing
  , schemaIf = Nothing
  , schemaThen = Nothing
  , schemaElse = Nothing
  , schemaRef = Nothing
  , schemaDynamicRef = Nothing
  , schemaContentEncoding = Nothing
  , schemaContentMediaType = Nothing
  , schemaContentSchema = Nothing
  , schemaEnum = Nothing
  , schemaConst = Nothing
  }
