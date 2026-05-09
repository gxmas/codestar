{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.JsonSchema.Combinators
-- Description : Combinator-based JSON Schema construction
-- Copyright   : (c) 2026 Geoffrey Noel
-- License     : MIT
--
-- Build JSON Schema documents using type-safe combinators. Start with a
-- base constructor ('stringSchema', 'objectSchema', etc.) and apply
-- modifiers using @('Data.Function.&')@ for left-to-right composition.
--
-- Every modifier has the signature @arg -> Schema -> Schema@, placing the
-- 'Schema' argument last so it works naturally with @('Data.Function.&')@.
--
-- == Basic usage
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Data.Function ((&))
-- import Data.JsonSchema
--
-- userSchema :: Schema
-- userSchema = objectSchema
--   [ required "name" $ stringSchema
--       & withTitle "User name"
--       & withMinLength 1
--       & withMaxLength 100
--   , required "age"  $ integerSchema
--       & withMinimum 0
--       & withMaximum 150
--   , optional "email" $ stringSchema
--       & withFormat "email"
--   ]
-- @
--
-- == Composition keywords
--
-- @
-- petSchema :: Schema
-- petSchema = oneOf
--   [ objectSchema [required "bark" booleanSchema]
--   , objectSchema [required "meow" booleanSchema]
--   ]
-- @
--
-- == Conditional schemas
--
-- @
-- conditionalSchema :: Schema
-- conditionalSchema = ifThenElse
--   (objectSchema [required "kind" $ constSchema "dog"])
--   (objectSchema [required "bark" booleanSchema])
--   (objectSchema [required "meow" booleanSchema])
-- @
module Data.JsonSchema.Combinators
  ( -- * Property type
    -- | 'Property' tags an object property as required or optional.
    -- Use 'required' and 'optional' to construct values.
    Property (..)

    -- * Property constructors
  , required
  , optional

    -- * Primitive schemas
    -- | Each produces a schema with only @\"type\"@ set.
  , stringSchema
  , numberSchema
  , integerSchema
  , booleanSchema
  , nullSchema

    -- * Composite schemas
    -- | Schemas that combine other schemas or constrain composite types.
  , objectSchema
  , arraySchema
  , enumSchema
  , constSchema
  , nullable
  , prefixItems

    -- * Validation modifiers
    -- | Modifiers for general validation keywords (@const@, @enum@, etc.).
  , withConst

    -- * Composition
    -- | JSON Schema composition keywords. These produce schemas with
    -- only the composition field set; combine with @('Data.Function.&')@
    -- to add further constraints.
  , allOf
  , anyOf
  , oneOf
  , not

    -- * Reference
    -- | Create a @$ref@ schema pointing to a definition or external schema.
  , ref

    -- * Conditional
    -- | @if@\/@then@\/@else@ conditional schema application.
  , ifThenElse

    -- * Numeric constraint modifiers
    -- | Applicable to 'numberSchema' and 'integerSchema'.
  , withMinimum
  , withMaximum
  , withExclusiveMinimum
  , withExclusiveMaximum
  , withMultipleOf

    -- * String constraint modifiers
    -- | Applicable to 'stringSchema'.
  , withMinLength
  , withMaxLength
  , withPattern
  , withFormat

    -- * Array constraint modifiers
    -- | Applicable to 'arraySchema' and 'prefixItems'.
  , withMinItems
  , withMaxItems
  , withUniqueItems
  , withContains
  , withMinContains
  , withMaxContains
  , withUnevaluatedItems

    -- * Object constraint modifiers
    -- | Applicable to 'objectSchema'.
  , withAdditionalProperties
  , withPatternProperties
  , withPropertyNames
  , withMinProperties
  , withMaxProperties
  , withDependentSchemas
  , withDependentRequired
  , withUnevaluatedProperties

    -- * Annotation modifiers
    -- | Metadata that does not affect validation. Can be applied to any
    -- schema.
  , withTitle
  , withDescription
  , withDefault
  , withExamples
  , withDeprecated
  , withReadOnly
  , withWriteOnly
  , withComment

    -- * Identity & reference modifiers
    -- | Set identity keywords (@$id@, @$anchor@, etc.) and @$defs@.
  , withId
  , withSchema
  , withAnchor
  , withDynamicAnchor
  , withDynamicRef
  , withDefs
  , withVocabulary

    -- * Content modifiers
    -- | Content-encoding keywords for string-encoded payloads.
  , withContentEncoding
  , withContentMediaType
  , withContentSchema
  ) where

import Data.Aeson (Value)
import qualified Data.JsonSchema.OrderedMap as OM
import Data.JsonSchema.Schema.Internal
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import Data.Text (Text)
import Numeric.Natural (Natural)
import Prelude hiding (not)

-- | A property in an object schema, tagged as required or optional.
-- Construct using 'required' or 'optional'.
data Property = Property
  { propertyName     :: !Text
    -- ^ The JSON property name.
  , propertySchema   :: !Schema
    -- ^ The schema that validates this property's value.
  , propertyRequired :: !Bool
    -- ^ 'True' if the property must be present.
  }
  deriving (Eq, Show)

-- | Mark a property as required. Its name will appear in the @required@
-- array of the enclosing 'objectSchema'.
required
  :: Text
  -- ^ Property name
  -> Schema
  -- ^ Schema for the property value
  -> Property
required name s = Property name s True

-- | Mark a property as optional. It will appear in @properties@ but not
-- in the @required@ array.
optional
  :: Text
  -- ^ Property name
  -> Schema
  -- ^ Schema for the property value
  -> Property
optional name s = Property name s False

-- ---------------------------------------------------------------------------
-- Primitive schemas
-- ---------------------------------------------------------------------------

-- | @{ \"type\": \"string\" }@
--
-- A schema that validates JSON strings. Apply string modifiers such as
-- 'withMinLength', 'withMaxLength', 'withPattern', or 'withFormat'.
stringSchema :: Schema
stringSchema = emptySchema { schemaType = Just (SingleType SchemaString) }

-- | @{ \"type\": \"number\" }@
--
-- A schema that validates JSON numbers (integer or floating point).
-- Apply numeric modifiers such as 'withMinimum', 'withMaximum', or
-- 'withMultipleOf'.
numberSchema :: Schema
numberSchema = emptySchema { schemaType = Just (SingleType SchemaNumber) }

-- | @{ \"type\": \"integer\" }@
--
-- A schema that validates JSON numbers with zero fractional part.
-- Accepts the same numeric modifiers as 'numberSchema'.
integerSchema :: Schema
integerSchema = emptySchema { schemaType = Just (SingleType SchemaInteger) }

-- | @{ \"type\": \"boolean\" }@
--
-- A schema that validates JSON booleans (@true@ or @false@).
booleanSchema :: Schema
booleanSchema = emptySchema { schemaType = Just (SingleType SchemaBoolean) }

-- | @{ \"type\": \"null\" }@
--
-- A schema that only validates the JSON @null@ value.
-- See also 'nullable' for making an existing schema accept @null@.
nullSchema :: Schema
nullSchema = emptySchema { schemaType = Just (SingleType SchemaNull) }

-- ---------------------------------------------------------------------------
-- Composite schemas
-- ---------------------------------------------------------------------------

-- | Build an object schema from a list of properties.
--
-- Required properties are derived from the 'Property' tags. The
-- The property order is preserved for serialization.
--
-- @
-- objectSchema
--   [ required "name" stringSchema
--   , optional "age" integerSchema
--   ]
-- @
objectSchema :: [Property] -> Schema
objectSchema props =
  emptySchema
    { schemaType = Just (SingleType SchemaObject)
    , schemaProperties =
        Just $ OM.fromList
          [(propertyName p, propertySchema p) | p <- props]
    , schemaRequired = case [propertyName p | p <- props, propertyRequired p] of
        [] -> Nothing
        rs -> Just rs
    }

-- | @{ \"type\": \"array\", \"items\": ... }@
--
-- A homogeneous array schema. Apply array modifiers such as
-- 'withMinItems', 'withMaxItems', or 'withUniqueItems'.
--
-- @
-- tags :: Schema
-- tags = arraySchema stringSchema
--   & withMinItems 1
--   & withUniqueItems True
-- @
arraySchema
  :: Schema
  -- ^ Schema for each array element
  -> Schema
arraySchema items =
  emptySchema
    { schemaType = Just (SingleType SchemaArray)
    , schemaItems = Just items
    }

-- | @{ \"enum\": [...] }@
--
-- The instance must be one of the given values. Typically used with
-- @OverloadedStrings@ and aeson's 'Data.Aeson.String' or 'Data.Aeson.Number'.
enumSchema :: [Value] -> Schema
enumSchema vals = emptySchema { schemaEnum = Just vals }

-- | @{ \"const\": ... }@
--
-- The instance must be exactly this value.
constSchema :: Value -> Schema
constSchema val = emptySchema { schemaConst = Just val }

-- ---------------------------------------------------------------------------
-- Validation modifiers
-- ---------------------------------------------------------------------------

-- | Set the @const@ keyword on an existing schema, constraining the
-- instance to be exactly the given value.
--
-- Unlike 'constSchema' (which creates a bare @{\"const\": v}@ schema),
-- 'withConst' preserves all existing fields on the input schema. This
-- is useful when you want both a @type@ annotation and a @const@
-- constraint:
--
-- @
-- stringSchema & withConst (String "object")
-- -- { "type": "string", "const": "object" }
-- @
withConst :: Value -> Schema -> Schema
withConst v s = s { schemaConst = Just v }

-- | Make a schema nullable by adding 'SchemaNull' to the type.
--
-- * 'SingleType' t becomes @UnionType (Set.fromList [t, SchemaNull])@
-- * 'UnionType' ts gets 'SchemaNull' inserted into the set
-- * No existing type becomes @SingleType SchemaNull@
nullable :: Schema -> Schema
nullable s = case schemaType s of
  Just (SingleType t) ->
    s { schemaType = Just (UnionType (Set.fromList [t, SchemaNull])) }
  Just (UnionType ts) ->
    s { schemaType = Just (UnionType (Set.insert SchemaNull ts)) }
  Nothing ->
    s { schemaType = Just (SingleType SchemaNull) }

-- | @{ \"type\": \"array\", \"prefixItems\": [...] }@
--
-- Tuple validation: each positional element is validated against the
-- corresponding schema. Use 'withUnevaluatedItems' on the result to
-- constrain additional elements beyond the prefix.
prefixItems :: [Schema] -> Schema
prefixItems schemas =
  emptySchema
    { schemaType = Just (SingleType SchemaArray)
    , schemaPrefixItems = Just schemas
    }

-- ---------------------------------------------------------------------------
-- Composition
-- ---------------------------------------------------------------------------

-- | @{ \"allOf\": [...] }@
--
-- The instance must be valid against /all/ of the given schemas.
allOf :: [Schema] -> Schema
allOf schemas = emptySchema { schemaAllOf = Just schemas }

-- | @{ \"anyOf\": [...] }@
--
-- The instance must be valid against /at least one/ of the given schemas.
anyOf :: [Schema] -> Schema
anyOf schemas = emptySchema { schemaAnyOf = Just schemas }

-- | @{ \"oneOf\": [...] }@
--
-- The instance must be valid against /exactly one/ of the given schemas.
oneOf :: [Schema] -> Schema
oneOf schemas = emptySchema { schemaOneOf = Just schemas }

-- | @{ \"not\": ... }@
--
-- The instance must /not/ be valid against this schema.
--
-- Note: this shadows 'Prelude.not'. The module re-exports @Prelude@
-- with @not@ hidden.
not :: Schema -> Schema
not s = emptySchema { schemaNot = Just s }

-- ---------------------------------------------------------------------------
-- Reference
-- ---------------------------------------------------------------------------

-- | @{ \"$ref\": \"...\" }@
--
-- A reference to another schema by URI or JSON Pointer. Commonly used
-- with 'withDefs' for schema reuse within a document:
--
-- @
-- ref "#\/$defs\/address"
-- @
ref :: Text -> Schema
ref r = emptySchema { schemaRef = Just r }

-- ---------------------------------------------------------------------------
-- Conditional
-- ---------------------------------------------------------------------------

-- | @{ \"if\": ..., \"then\": ..., \"else\": ... }@
--
-- Conditional schema application. If the instance matches the @if@
-- schema, the @then@ schema is applied; otherwise the @else@ schema
-- is applied.
ifThenElse
  :: Schema
  -- ^ @if@ -- the condition schema
  -> Schema
  -- ^ @then@ -- applied when @if@ matches
  -> Schema
  -- ^ @else@ -- applied when @if@ does not match
  -> Schema
ifThenElse ifS thenS elseS =
  emptySchema
    { schemaIf = Just ifS
    , schemaThen = Just thenS
    , schemaElse = Just elseS
    }

-- ---------------------------------------------------------------------------
-- Numeric constraint modifiers
-- ---------------------------------------------------------------------------

-- | Set @minimum@ -- inclusive lower bound for numeric values.
withMinimum :: Scientific -> Schema -> Schema
withMinimum n s = s { schemaMinimum = Just n }

-- | Set @maximum@ -- inclusive upper bound for numeric values.
withMaximum :: Scientific -> Schema -> Schema
withMaximum n s = s { schemaMaximum = Just n }

-- | Set @exclusiveMinimum@ -- exclusive lower bound (value must be strictly greater).
withExclusiveMinimum :: Scientific -> Schema -> Schema
withExclusiveMinimum n s = s { schemaExclusiveMinimum = Just n }

-- | Set @exclusiveMaximum@ -- exclusive upper bound (value must be strictly less).
withExclusiveMaximum :: Scientific -> Schema -> Schema
withExclusiveMaximum n s = s { schemaExclusiveMaximum = Just n }

-- | Set @multipleOf@ -- the value must be an integer multiple of this number.
-- The argument must be strictly positive.
withMultipleOf :: Scientific -> Schema -> Schema
withMultipleOf n s = s { schemaMultipleOf = Just n }

-- ---------------------------------------------------------------------------
-- String constraint modifiers
-- ---------------------------------------------------------------------------

-- | Set @minLength@ -- minimum string length in Unicode code points.
withMinLength :: Natural -> Schema -> Schema
withMinLength n s = s { schemaMinLength = Just n }

-- | Set @maxLength@ -- maximum string length in Unicode code points.
withMaxLength :: Natural -> Schema -> Schema
withMaxLength n s = s { schemaMaxLength = Just n }

-- | Set @pattern@ -- an ECMA-262 regular expression the string must match.
withPattern :: Text -> Schema -> Schema
withPattern p s = s { schemaPattern = Just p }

-- | Set @format@ -- a semantic format hint.
--
-- Common values: @\"email\"@, @\"date-time\"@, @\"date\"@, @\"time\"@,
-- @\"uri\"@, @\"uuid\"@, @\"ipv4\"@, @\"ipv6\"@, @\"hostname\"@.
withFormat :: Text -> Schema -> Schema
withFormat f s = s { schemaFormat = Just f }

-- ---------------------------------------------------------------------------
-- Array constraint modifiers
-- ---------------------------------------------------------------------------

-- | Set @minItems@ -- minimum array length.
withMinItems :: Natural -> Schema -> Schema
withMinItems n s = s { schemaMinItems = Just n }

-- | Set @maxItems@ -- maximum array length.
withMaxItems :: Natural -> Schema -> Schema
withMaxItems n s = s { schemaMaxItems = Just n }

-- | Set @uniqueItems@ -- when @True@, all elements must be distinct.
withUniqueItems :: Bool -> Schema -> Schema
withUniqueItems b s = s { schemaUniqueItems = Just b }

-- | Set @contains@ -- at least one array element must match this schema.
-- Use with 'withMinContains' and 'withMaxContains' to control the count.
withContains :: Schema -> Schema -> Schema
withContains c s = s { schemaContains = Just c }

-- | Set @minContains@ -- minimum number of elements matching @contains@.
-- Only meaningful when @contains@ is set.
withMinContains :: Natural -> Schema -> Schema
withMinContains n s = s { schemaMinContains = Just n }

-- | Set @maxContains@ -- maximum number of elements matching @contains@.
-- Only meaningful when @contains@ is set.
withMaxContains :: Natural -> Schema -> Schema
withMaxContains n s = s { schemaMaxContains = Just n }

-- | Set @unevaluatedItems@ -- schema applied to array elements not
-- covered by @items@, @prefixItems@, or @contains@.
withUnevaluatedItems :: Schema -> Schema -> Schema
withUnevaluatedItems ui s = s { schemaUnevaluatedItems = Just ui }

-- ---------------------------------------------------------------------------
-- Object constraint modifiers
-- ---------------------------------------------------------------------------

-- | Set @additionalProperties@ -- schema for properties not matched by
-- @properties@ or @patternProperties@. Pass @booleanSchema@ equivalent
-- schemas (via 'constSchema') to forbid additional properties.
withAdditionalProperties :: Schema -> Schema -> Schema
withAdditionalProperties ap s = s { schemaAdditionalProperties = Just ap }

-- | Set @patternProperties@ -- schemas keyed by ECMA-262 regex patterns.
-- Properties whose names match a pattern are validated against that
-- pattern's schema. Insertion order is preserved.
withPatternProperties :: [(Text, Schema)] -> Schema -> Schema
withPatternProperties ps s =
  s { schemaPatternProperties = Just (OM.fromList ps) }

-- | Set @propertyNames@ -- a schema that all property /names/ must satisfy.
--
-- @
-- objectSchema [] & withPropertyNames (stringSchema & withPattern "^x-")
-- @
withPropertyNames :: Schema -> Schema -> Schema
withPropertyNames pn s = s { schemaPropertyNames = Just pn }

-- | Set @minProperties@ -- minimum number of properties the object must have.
withMinProperties :: Natural -> Schema -> Schema
withMinProperties n s = s { schemaMinProperties = Just n }

-- | Set @maxProperties@ -- maximum number of properties the object may have.
withMaxProperties :: Natural -> Schema -> Schema
withMaxProperties n s = s { schemaMaxProperties = Just n }

-- | Set @dependentSchemas@ -- additional schemas that apply when a
-- given property is present. Insertion order is preserved.
withDependentSchemas :: [(Text, Schema)] -> Schema -> Schema
withDependentSchemas ds s =
  s { schemaDependentSchemas = Just (OM.fromList ds) }

-- | Set @dependentRequired@ -- properties that become required when a
-- given property is present. Insertion order is preserved.
withDependentRequired :: [(Text, [Text])] -> Schema -> Schema
withDependentRequired dr s =
  s { schemaDependentRequired = Just (OM.fromList dr) }

-- | Set @unevaluatedProperties@ -- schema for properties not covered
-- by @properties@, @patternProperties@, or @additionalProperties@.
withUnevaluatedProperties :: Schema -> Schema -> Schema
withUnevaluatedProperties up s = s { schemaUnevaluatedProperties = Just up }

-- ---------------------------------------------------------------------------
-- Annotation modifiers
-- ---------------------------------------------------------------------------

-- | Set @title@ -- a short human-readable label for the schema.
withTitle :: Text -> Schema -> Schema
withTitle t s = s { schemaTitle = Just t }

-- | Set @description@ -- a longer human-readable explanation.
withDescription :: Text -> Schema -> Schema
withDescription d s = s { schemaDescription = Just d }

-- | Set @default@ -- a default value for documentation or code generation.
-- Does not affect validation.
withDefault :: Value -> Schema -> Schema
withDefault v s = s { schemaDefault = Just v }

-- | Set @examples@ -- sample values illustrating valid instances.
withExamples :: [Value] -> Schema -> Schema
withExamples es s = s { schemaExamples = Just es }

-- | Set @deprecated@ -- marks the schema or property as deprecated.
withDeprecated :: Bool -> Schema -> Schema
withDeprecated b s = s { schemaDeprecated = Just b }

-- | Set @readOnly@ -- indicates the value should not be modified by clients.
withReadOnly :: Bool -> Schema -> Schema
withReadOnly b s = s { schemaReadOnly = Just b }

-- | Set @writeOnly@ -- indicates the value should not be included in
-- responses (e.g., passwords).
withWriteOnly :: Bool -> Schema -> Schema
withWriteOnly b s = s { schemaWriteOnly = Just b }

-- | Set @$comment@ -- a comment for schema maintainers, not for end users.
withComment :: Text -> Schema -> Schema
withComment c s = s { schemaComment = Just c }

-- ---------------------------------------------------------------------------
-- Identity & reference modifiers
-- ---------------------------------------------------------------------------

-- | Set @$id@ -- the canonical URI identifier for this schema.
withId :: Text -> Schema -> Schema
withId i s = s { schemaId = Just i }

-- | Set @$schema@ -- the meta-schema URI. Typically only needed on
-- the root schema document.
--
-- @
-- rootSchema & withSchema "https:\/\/json-schema.org\/draft\/2020-12\/schema"
-- @
withSchema :: Text -> Schema -> Schema
withSchema sc s = s { schemaSchema = Just sc }

-- | Set @$anchor@ -- a plain-name anchor for use in @$ref@ URIs
-- within the same document.
withAnchor :: Text -> Schema -> Schema
withAnchor a s = s { schemaAnchor = Just a }

-- | Set @$dynamicAnchor@ -- a dynamic anchor for recursive schema
-- extension patterns.
withDynamicAnchor :: Text -> Schema -> Schema
withDynamicAnchor da s = s { schemaDynamicAnchor = Just da }

-- | Set @$dynamicRef@ -- a dynamic reference resolved at evaluation time.
withDynamicRef :: Text -> Schema -> Schema
withDynamicRef dr s = s { schemaDynamicRef = Just dr }

-- | Set @$defs@ -- reusable schema definitions. Reference them via
-- 'ref':
--
-- @
-- mySchema & withDefs
--   [ ("address", objectSchema [...])
--   , ("phone",   stringSchema & withPattern "^\\\\+[0-9]+$")
--   ]
-- @
withDefs :: [(Text, Schema)] -> Schema -> Schema
withDefs ds s = s { schemaDefs = Just (OM.fromList ds) }

-- | Set @$vocabulary@ -- declares which vocabularies are required or
-- optional for this meta-schema.
withVocabulary :: [(Text, Bool)] -> Schema -> Schema
withVocabulary vs s = s { schemaVocabulary = Just (OM.fromList vs) }

-- ---------------------------------------------------------------------------
-- Content modifiers
-- ---------------------------------------------------------------------------

-- | Set @contentEncoding@ -- the encoding of the string content
-- (e.g., @\"base64\"@, @\"quoted-printable\"@).
withContentEncoding :: Text -> Schema -> Schema
withContentEncoding ce s = s { schemaContentEncoding = Just ce }

-- | Set @contentMediaType@ -- the MIME type of the string content
-- (e.g., @\"application\/json\"@, @\"image\/png\"@).
withContentMediaType :: Text -> Schema -> Schema
withContentMediaType cm s = s { schemaContentMediaType = Just cm }

-- | Set @contentSchema@ -- a schema for the decoded string content.
-- Only meaningful when @contentMediaType@ and optionally
-- @contentEncoding@ are also set.
withContentSchema :: Schema -> Schema -> Schema
withContentSchema cs s = s { schemaContentSchema = Just cs }
