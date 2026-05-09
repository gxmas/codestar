{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.JsonSchema
-- Description : Type-safe JSON Schema (draft 2020-12) combinators
-- Copyright   : (c) 2026 Geoffrey Noel
-- License     : MIT
-- Maintainer  : noel.geoff@gmail.com
--
-- A combinator-based library for constructing JSON Schema (draft 2020-12)
-- documents in Haskell. Schemas are built by starting with a base
-- constructor ('stringSchema', 'objectSchema', etc.) and applying
-- modifiers using @('Data.Function.&')@ for left-to-right composition.
--
-- == Prerequisites
--
-- Enable @OverloadedStrings@ and import @Data.Function.('Data.Function.&')@:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Data.Function ((&))
-- import Data.JsonSchema
-- @
--
-- == Quick start
--
-- @
-- userSchema :: Schema
-- userSchema = objectSchema
--   [ required "name" $ stringSchema
--       & withTitle "User name"
--       & withMinLength 1
--   , required "age"  $ integerSchema
--       & withMinimum 0
--   , optional "email" $ stringSchema
--       & withFormat "email"
--   ]
-- @
--
-- == Nested objects and arrays
--
-- @
-- teamSchema :: Schema
-- teamSchema = objectSchema
--   [ required "teamName" stringSchema
--   , required "members" $ arraySchema userSchema
--       & withMinItems 1
--       & withUniqueItems True
--   , optional "metadata" $ objectSchema
--       [ optional "region" stringSchema
--       , optional "active" booleanSchema
--       ]
--   ]
-- @
--
-- == Nullable types
--
-- @
-- -- { "type": ["string", "null"] }
-- nullableString :: Schema
-- nullableString = nullable stringSchema
-- @
--
-- == Schema reuse with @$defs@ and @$ref@
--
-- @
-- apiSchema :: Schema
-- apiSchema = objectSchema
--   [ required "user" (ref "#\/$defs\/user") ]
--   & withDefs
--       [ ("user", objectSchema
--           [ required "id" integerSchema
--           , required "name" stringSchema
--           ])
--       ]
-- @
--
-- == Serialization
--
-- Convert to\/from aeson 'Data.Aeson.Value' with 'encode' and 'decode':
--
-- @
-- import qualified Data.Aeson as Aeson
--
-- json :: 'Data.Aeson.Value'
-- json = encode userSchema
--
-- roundTrip :: Either 'DecodeError' Schema
-- roundTrip = decode json
-- @
--
-- For direct record access to 'Schema' fields, import
-- "Data.JsonSchema.Schema.Internal".
module Data.JsonSchema
  ( -- * Schema type and constructors
    -- | Re-exported from "Data.JsonSchema.Schema".
    module Data.JsonSchema.Schema

    -- * Schema combinators
    -- | Re-exported from "Data.JsonSchema.Combinators". See that module for
    -- the full list of constructors and modifiers.
  , module Data.JsonSchema.Combinators

    -- * Serialization
    -- | Re-exported from "Data.JsonSchema.Serialization".
  , encode
  , decode
  , DecodeError (..)

    -- * Ordered maps
    -- | Re-exported from "Data.JsonSchema.OrderedMap". Used for
    -- @properties@, @patternProperties@, @$defs@, and similar fields
    -- that require stable key ordering.
  , OrderedMap
  ) where

import Data.JsonSchema.Combinators
import Data.JsonSchema.OrderedMap (OrderedMap)
import Data.JsonSchema.Schema
import Data.JsonSchema.Serialization (DecodeError (..), decode, encode)
