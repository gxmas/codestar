{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Data.JsonSchema.Serialization
-- Description : JSON encoding and decoding for Schema
-- Copyright   : (c) 2026 Geoffrey Noel
-- License     : MIT
--
-- Dual-approach serialization for 'Schema':
--
-- * __Explicit functions__ -- 'encode' and 'decode' give direct control
--   and structured errors.
-- * __Typeclass instances__ -- 'ToJSON' and 'FromJSON' instances for
--   'Schema' delegate to the explicit functions, so @Aeson.encode@ and
--   @Aeson.decode@ work out of the box.
--
-- == Round-trip invariant
--
-- For any schema @s@:
--
-- @
-- decode (encode s) == Right s
-- @
--
-- == Field name mapping
--
-- Haskell record fields with a @$@ prefix in their JSON Schema name:
--
-- * @schemaRef@ \<-\> @$ref@
-- * @schemaDefs@ \<-\> @$defs@
-- * @schemaId@ \<-\> @$id@
-- * @schemaSchema@ \<-\> @$schema@
-- * @schemaAnchor@ \<-\> @$anchor@
-- * @schemaDynamicAnchor@ \<-\> @$dynamicAnchor@
-- * @schemaDynamicRef@ \<-\> @$dynamicRef@
-- * @schemaComment@ \<-\> @$comment@
-- * @schemaVocabulary@ \<-\> @$vocabulary@
--
-- All other fields use camelCase matching the JSON Schema spec directly.
--
-- == Usage
--
-- @
-- import Data.JsonSchema
-- import qualified Data.Aeson as Aeson
--
-- -- Explicit
-- let json = encode mySchema
-- let result = decode json  -- :: Either DecodeError Schema
--
-- -- Via typeclass
-- let json' = Aeson.toJSON mySchema
-- let result' = Aeson.fromJSON json'  -- :: Aeson.Result Schema
-- @
module Data.JsonSchema.Serialization
  ( -- * Encoding
    -- | Convert a 'Schema' to an aeson 'Value'. Fields set to 'Nothing'
    -- are omitted from the output.
    encode

    -- * Decoding
    -- | Parse an aeson 'Value' into a 'Schema'. Unknown fields are
    -- silently ignored. Structural errors produce a 'DecodeError' with
    -- a JSON Pointer-style path to the problem.
  , decode
  , DecodeError (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.JsonSchema.OrderedMap (OrderedMap)
import qualified Data.JsonSchema.OrderedMap as OM
import Data.JsonSchema.Schema.Internal
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Natural (Natural)

-- | Structured error from 'decode'. Contains a human-readable message
-- and a path (outermost key first) indicating where in the JSON structure
-- the error occurred.
--
-- @
-- DecodeError "Expected a string" ["properties", "name", "type"]
-- @
data DecodeError = DecodeError
  { errorMessage :: !Text
    -- ^ Human-readable description of the problem.
  , errorPath    :: ![Text]
    -- ^ JSON key path from root to the error location (outermost first).
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- ToJSON / FromJSON instances for supporting types
-- ---------------------------------------------------------------------------

instance ToJSON SchemaType where
  toJSON = \case
    SchemaString  -> "string"
    SchemaNumber  -> "number"
    SchemaInteger -> "integer"
    SchemaBoolean -> "boolean"
    SchemaArray   -> "array"
    SchemaObject  -> "object"
    SchemaNull    -> "null"

instance FromJSON SchemaType where
  parseJSON = Aeson.withText "SchemaType" $ \case
    "string"  -> pure SchemaString
    "number"  -> pure SchemaNumber
    "integer" -> pure SchemaInteger
    "boolean" -> pure SchemaBoolean
    "array"   -> pure SchemaArray
    "object"  -> pure SchemaObject
    "null"    -> pure SchemaNull
    other     -> fail $ "Unknown SchemaType: " <> T.unpack other

instance ToJSON TypeSpec where
  toJSON (SingleType t) = toJSON t
  toJSON (UnionType ts) = toJSON (Set.toAscList ts)

instance FromJSON TypeSpec where
  parseJSON v@(String _) = SingleType <$> parseJSON v
  parseJSON v@(Array _)  = do
    ts <- parseJSON v
    pure (UnionType (Set.fromList ts))
  parseJSON _ = fail "TypeSpec: expected string or array of strings"

-- ---------------------------------------------------------------------------
-- Schema instances
-- ---------------------------------------------------------------------------

instance ToJSON Schema where
  toJSON = encode

instance FromJSON Schema where
  parseJSON v = case decode v of
    Right s  -> pure s
    Left err -> fail (T.unpack (errorMessage err))

-- ---------------------------------------------------------------------------
-- Encoding
-- ---------------------------------------------------------------------------

-- | Encode a 'Schema' to a JSON 'Value'.
--
-- Fields set to 'Nothing' are omitted. Keys are emitted in a logical
-- grouping order (identity, type, annotations, validation, composition,
-- references, content, enum/const).
encode :: Schema -> Value
encode s =
  Aeson.object $ catMaybes
    -- Identity & dialect
    [ opt "$id"              schemaId s
    , opt "$schema"          schemaSchema s
    , opt "$anchor"          schemaAnchor s
    , opt "$dynamicAnchor"   schemaDynamicAnchor s
    , optWith encodeOMBool "$vocabulary" schemaVocabulary s
    , optWith encodeOMSchema "$defs" schemaDefs s

    -- Type
    , opt "type"             schemaType s

    -- Annotations
    , opt "title"            schemaTitle s
    , opt "description"      schemaDescription s
    , opt "default"          schemaDefault s
    , opt "examples"         schemaExamples s
    , opt "deprecated"       schemaDeprecated s
    , opt "readOnly"         schemaReadOnly s
    , opt "writeOnly"        schemaWriteOnly s
    , opt "$comment"         schemaComment s

    -- Numeric validation
    , opt "multipleOf"       schemaMultipleOf s
    , opt "minimum"          schemaMinimum s
    , opt "exclusiveMinimum" schemaExclusiveMinimum s
    , opt "maximum"          schemaMaximum s
    , opt "exclusiveMaximum" schemaExclusiveMaximum s

    -- String validation
    , optWith encodeNatural "minLength" schemaMinLength s
    , optWith encodeNatural "maxLength" schemaMaxLength s
    , opt "pattern"          schemaPattern s
    , opt "format"           schemaFormat s

    -- Array validation
    , optWith encodeSchema "items" schemaItems s
    , optWith encodeSchemaList "prefixItems" schemaPrefixItems s
    , optWith encodeSchema "contains" schemaContains s
    , optWith encodeNatural "minContains" schemaMinContains s
    , optWith encodeNatural "maxContains" schemaMaxContains s
    , optWith encodeNatural "minItems" schemaMinItems s
    , optWith encodeNatural "maxItems" schemaMaxItems s
    , opt "uniqueItems"      schemaUniqueItems s
    , optWith encodeSchema "unevaluatedItems" schemaUnevaluatedItems s

    -- Object validation
    , optWith encodeOMSchema "properties" schemaProperties s
    , optWith encodeOMSchema "patternProperties" schemaPatternProperties s
    , optWith encodeSchema "additionalProperties" schemaAdditionalProperties s
    , opt "required"         schemaRequired s
    , optWith encodeSchema "propertyNames" schemaPropertyNames s
    , optWith encodeNatural "minProperties" schemaMinProperties s
    , optWith encodeNatural "maxProperties" schemaMaxProperties s
    , optWith encodeOMSchema "dependentSchemas" schemaDependentSchemas s
    , optWith encodeOMTextList "dependentRequired" schemaDependentRequired s
    , optWith encodeSchema "unevaluatedProperties" schemaUnevaluatedProperties s

    -- Composition
    , optWith encodeSchemaList "allOf" schemaAllOf s
    , optWith encodeSchemaList "anyOf" schemaAnyOf s
    , optWith encodeSchemaList "oneOf" schemaOneOf s
    , optWith encodeSchema "not" schemaNot s

    -- Conditionals
    , optWith encodeSchema "if" schemaIf s
    , optWith encodeSchema "then" schemaThen s
    , optWith encodeSchema "else" schemaElse s

    -- References
    , opt "$ref"             schemaRef s
    , opt "$dynamicRef"      schemaDynamicRef s

    -- Content
    , opt "contentEncoding"  schemaContentEncoding s
    , opt "contentMediaType" schemaContentMediaType s
    , optWith encodeSchema "contentSchema" schemaContentSchema s

    -- Enum/const
    , opt "enum"             schemaEnum s
    , opt "const"            schemaConst s
    ]

-- Helpers for encoding

opt :: ToJSON a => Aeson.Key -> (Schema -> Maybe a) -> Schema -> Maybe (Aeson.Key, Value)
opt key accessor schema = (key .=) <$> accessor schema

optWith :: (a -> Value) -> Aeson.Key -> (Schema -> Maybe a) -> Schema -> Maybe (Aeson.Key, Value)
optWith f key accessor schema = (key .=) . f <$> accessor schema

encodeSchema :: Schema -> Value
encodeSchema = encode

encodeSchemaList :: [Schema] -> Value
encodeSchemaList = toJSON . map encode

encodeNatural :: Natural -> Value
encodeNatural = toJSON . toInteger

encodeOMSchema :: OrderedMap Text Schema -> Value
encodeOMSchema om =
  Aeson.object
    [ Key.fromText k .= encode v
    | (k, v) <- OM.toList om
    ]

encodeOMBool :: OrderedMap Text Bool -> Value
encodeOMBool om =
  Aeson.object
    [ Key.fromText k .= v
    | (k, v) <- OM.toList om
    ]

encodeOMTextList :: OrderedMap Text [Text] -> Value
encodeOMTextList om =
  Aeson.object
    [ Key.fromText k .= v
    | (k, v) <- OM.toList om
    ]

-- ---------------------------------------------------------------------------
-- Decoding
-- ---------------------------------------------------------------------------

-- | Decode a JSON 'Value' to a 'Schema'.
--
-- Unknown fields are silently ignored. Returns a 'DecodeError' with a
-- path for structural problems.
decode :: Value -> Either DecodeError Schema
decode (Object o) = do
  sId            <- optField "$id" decodeText o
  sSchema        <- optField "$schema" decodeText o
  sAnchor        <- optField "$anchor" decodeText o
  sDynamicAnchor <- optField "$dynamicAnchor" decodeText o
  sVocabulary    <- optField "$vocabulary" decodeOMBool' o
  sDefs          <- optField "$defs" decodeOMSchema' o
  sType          <- optField "type" decodeTypeSpec o
  sTitle         <- optField "title" decodeText o
  sDescription   <- optField "description" decodeText o
  sDefault       <- optField "default" (Right) o
  sExamples      <- optField "examples" decodeValueList o
  sDeprecated    <- optField "deprecated" decodeBool o
  sReadOnly      <- optField "readOnly" decodeBool o
  sWriteOnly     <- optField "writeOnly" decodeBool o
  sComment       <- optField "$comment" decodeText o
  sMultipleOf    <- optField "multipleOf" decodeScientific o
  sMinimum       <- optField "minimum" decodeScientific o
  sExcMin        <- optField "exclusiveMinimum" decodeScientific o
  sMaximum       <- optField "maximum" decodeScientific o
  sExcMax        <- optField "exclusiveMaximum" decodeScientific o
  sMinLength     <- optField "minLength" decodeNatural o
  sMaxLength     <- optField "maxLength" decodeNatural o
  sPattern       <- optField "pattern" decodeText o
  sFormat        <- optField "format" decodeText o
  sItems         <- optField "items" decodeSchemaVal o
  sPrefixItems   <- optField "prefixItems" decodeSchemaList o
  sContains      <- optField "contains" decodeSchemaVal o
  sMinContains   <- optField "minContains" decodeNatural o
  sMaxContains   <- optField "maxContains" decodeNatural o
  sMinItems      <- optField "minItems" decodeNatural o
  sMaxItems      <- optField "maxItems" decodeNatural o
  sUniqueItems   <- optField "uniqueItems" decodeBool o
  sUnevalItems   <- optField "unevaluatedItems" decodeSchemaVal o
  sProperties    <- optField "properties" decodeOMSchema' o
  sPatternProps  <- optField "patternProperties" decodeOMSchema' o
  sAdditional    <- optField "additionalProperties" decodeSchemaVal o
  sRequired      <- optField "required" decodeTextList o
  sPropertyNames <- optField "propertyNames" decodeSchemaVal o
  sMinProps      <- optField "minProperties" decodeNatural o
  sMaxProps      <- optField "maxProperties" decodeNatural o
  sDepSchemas    <- optField "dependentSchemas" decodeOMSchema' o
  sDepRequired   <- optField "dependentRequired" decodeOMTextList' o
  sUnevalProps   <- optField "unevaluatedProperties" decodeSchemaVal o
  sAllOf         <- optField "allOf" decodeSchemaList o
  sAnyOf         <- optField "anyOf" decodeSchemaList o
  sOneOf         <- optField "oneOf" decodeSchemaList o
  sNot           <- optField "not" decodeSchemaVal o
  sIf            <- optField "if" decodeSchemaVal o
  sThen          <- optField "then" decodeSchemaVal o
  sElse          <- optField "else" decodeSchemaVal o
  sRef           <- optField "$ref" decodeText o
  sDynamicRef    <- optField "$dynamicRef" decodeText o
  sContEnc       <- optField "contentEncoding" decodeText o
  sContMedia     <- optField "contentMediaType" decodeText o
  sContSchema    <- optField "contentSchema" decodeSchemaVal o
  sEnum          <- optField "enum" decodeValueList o
  sConst         <- optField "const" (Right) o
  pure Schema
    { schemaId = sId
    , schemaSchema = sSchema
    , schemaAnchor = sAnchor
    , schemaDynamicAnchor = sDynamicAnchor
    , schemaVocabulary = sVocabulary
    , schemaDefs = sDefs
    , schemaType = sType
    , schemaTitle = sTitle
    , schemaDescription = sDescription
    , schemaDefault = sDefault
    , schemaExamples = sExamples
    , schemaDeprecated = sDeprecated
    , schemaReadOnly = sReadOnly
    , schemaWriteOnly = sWriteOnly
    , schemaComment = sComment
    , schemaMultipleOf = sMultipleOf
    , schemaMinimum = sMinimum
    , schemaExclusiveMinimum = sExcMin
    , schemaMaximum = sMaximum
    , schemaExclusiveMaximum = sExcMax
    , schemaMinLength = sMinLength
    , schemaMaxLength = sMaxLength
    , schemaPattern = sPattern
    , schemaFormat = sFormat
    , schemaItems = sItems
    , schemaPrefixItems = sPrefixItems
    , schemaContains = sContains
    , schemaMinContains = sMinContains
    , schemaMaxContains = sMaxContains
    , schemaMinItems = sMinItems
    , schemaMaxItems = sMaxItems
    , schemaUniqueItems = sUniqueItems
    , schemaUnevaluatedItems = sUnevalItems
    , schemaProperties = sProperties
    , schemaPatternProperties = sPatternProps
    , schemaAdditionalProperties = sAdditional
    , schemaRequired = sRequired
    , schemaPropertyNames = sPropertyNames
    , schemaMinProperties = sMinProps
    , schemaMaxProperties = sMaxProps
    , schemaDependentSchemas = sDepSchemas
    , schemaDependentRequired = sDepRequired
    , schemaUnevaluatedProperties = sUnevalProps
    , schemaAllOf = sAllOf
    , schemaAnyOf = sAnyOf
    , schemaOneOf = sOneOf
    , schemaNot = sNot
    , schemaIf = sIf
    , schemaThen = sThen
    , schemaElse = sElse
    , schemaRef = sRef
    , schemaDynamicRef = sDynamicRef
    , schemaContentEncoding = sContEnc
    , schemaContentMediaType = sContMedia
    , schemaContentSchema = sContSchema
    , schemaEnum = sEnum
    , schemaConst = sConst
    }
decode _ = Left (DecodeError "Expected a JSON object" [])

-- ---------------------------------------------------------------------------
-- Decode helpers
-- ---------------------------------------------------------------------------

optField
  :: Text
  -> (Value -> Either DecodeError a)
  -> KM.KeyMap Value
  -> Either DecodeError (Maybe a)
optField key parser obj =
  case KM.lookup (Key.fromText key) obj of
    Nothing -> Right Nothing
    Just v  -> case parser v of
      Right a  -> Right (Just a)
      Left (DecodeError msg path) ->
        Left (DecodeError msg (key : path))

decodeText :: Value -> Either DecodeError Text
decodeText (String t) = Right t
decodeText _ = Left (DecodeError "Expected a string" [])

decodeBool :: Value -> Either DecodeError Bool
decodeBool (Bool b) = Right b
decodeBool _ = Left (DecodeError "Expected a boolean" [])

decodeScientific :: Value -> Either DecodeError Scientific
decodeScientific (Number n) = Right n
decodeScientific _ = Left (DecodeError "Expected a number" [])

decodeNatural :: Value -> Either DecodeError Natural
decodeNatural (Number n) =
  let i = truncate n :: Integer
  in if fromInteger i == n && i >= 0
     then Right (fromInteger i)
     else Left (DecodeError "Expected a non-negative integer" [])
decodeNatural _ = Left (DecodeError "Expected a number" [])

decodeTextList :: Value -> Either DecodeError [Text]
decodeTextList (Array arr) = mapM decodeText (foldr (:) [] arr)
decodeTextList _ = Left (DecodeError "Expected an array of strings" [])

decodeValueList :: Value -> Either DecodeError [Value]
decodeValueList (Array arr) = Right (foldr (:) [] arr)
decodeValueList _ = Left (DecodeError "Expected an array" [])

decodeSchemaVal :: Value -> Either DecodeError Schema
decodeSchemaVal = decode

decodeSchemaList :: Value -> Either DecodeError [Schema]
decodeSchemaList (Array arr) = mapM decode (foldr (:) [] arr)
decodeSchemaList _ = Left (DecodeError "Expected an array of schemas" [])

decodeTypeSpec :: Value -> Either DecodeError TypeSpec
decodeTypeSpec v@(String _) = SingleType <$> decodeSchemaType v
decodeTypeSpec (Array arr) = do
  ts <- mapM decodeSchemaType (foldr (:) [] arr)
  pure (UnionType (Set.fromList ts))
decodeTypeSpec _ = Left (DecodeError "Expected a string or array for type" [])

decodeSchemaType :: Value -> Either DecodeError SchemaType
decodeSchemaType (String "string")  = Right SchemaString
decodeSchemaType (String "number")  = Right SchemaNumber
decodeSchemaType (String "integer") = Right SchemaInteger
decodeSchemaType (String "boolean") = Right SchemaBoolean
decodeSchemaType (String "array")   = Right SchemaArray
decodeSchemaType (String "object")  = Right SchemaObject
decodeSchemaType (String "null")    = Right SchemaNull
decodeSchemaType (String other)     = Left (DecodeError ("Unknown type: " <> other) [])
decodeSchemaType _ = Left (DecodeError "Expected a type string" [])

decodeOMSchema' :: Value -> Either DecodeError (OrderedMap Text Schema)
decodeOMSchema' (Object o) = do
  let kvs = KM.toList o
  pairs <- mapM (\(k, v) -> do
    s <- decode v
    pure (Key.toText k, s)) kvs
  pure (OM.fromList pairs)
decodeOMSchema' _ = Left (DecodeError "Expected an object" [])

decodeOMBool' :: Value -> Either DecodeError (OrderedMap Text Bool)
decodeOMBool' (Object o) = do
  let kvs = KM.toList o
  pairs <- mapM (\(k, v) -> do
    b <- decodeBool v
    pure (Key.toText k, b)) kvs
  pure (OM.fromList pairs)
decodeOMBool' _ = Left (DecodeError "Expected an object" [])

decodeOMTextList' :: Value -> Either DecodeError (OrderedMap Text [Text])
decodeOMTextList' (Object o) = do
  let kvs = KM.toList o
  pairs <- mapM (\(k, v) -> do
    ts <- decodeTextList v
    pure (Key.toText k, ts)) kvs
  pure (OM.fromList pairs)
decodeOMTextList' _ = Left (DecodeError "Expected an object" [])
