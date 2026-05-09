-- |
-- Module      : Data.JsonSchema.Schema
-- Description : JSON Schema types (public re-exports)
-- Copyright   : (c) 2026 Geoffrey Noel
-- License     : MIT
--
-- Public re-exports of the core schema types. This module exposes the
-- 'Schema' type abstractly (without record field accessors) so that
-- consumers build schemas using the combinators in
-- "Data.JsonSchema.Combinators".
--
-- For direct record access or pattern matching, import
-- "Data.JsonSchema.Schema.Internal" instead.
module Data.JsonSchema.Schema
  ( -- * Schema type
    -- | The central type representing a JSON Schema document.
    -- Construct values using 'Data.JsonSchema.Combinators.stringSchema',
    -- 'Data.JsonSchema.Combinators.objectSchema', etc.
    Schema

    -- * Type specification
    -- | 'SchemaType' enumerates the seven JSON Schema primitive types.
    -- 'TypeSpec' wraps either a single type or a union of types
    -- (e.g., @[\"string\", \"null\"]@).
  , SchemaType (..)
  , TypeSpec (..)

    -- * Empty schema
    -- | 'emptySchema' is the identity element for modifier composition:
    -- applying any modifier @f@ to 'emptySchema' yields a schema with
    -- only that single field set. All constructors are defined
    -- in terms of 'emptySchema'.
  , emptySchema
  ) where

import Data.JsonSchema.Schema.Internal
  ( Schema
  , SchemaType (..)
  , TypeSpec (..)
  , emptySchema
  )
