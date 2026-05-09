{-# LANGUAGE OverloadedStrings #-}

-- | A minimal example demonstrating how to build a JSON Schema document
-- using json-schema-combinators and print it as JSON.
--
-- Build and run (from the json-schema package directory):
--
-- @
-- cabal run examples-basic
-- @
--
-- Or load directly with:
--
-- @
-- cabal exec runghc -- examples/Basic.hs
-- @
module Main (main) where

import Data.Function ((&))
import Data.JsonSchema
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8

-- | Schema for a user profile object.
--
-- Produces:
--
-- @
-- {
--   "type": "object",
--   "properties": {
--     "name":  { "type": "string", "minLength": 1, "maxLength": 128 },
--     "email": { "type": "string", "format": "email" },
--     "age":   { "type": "integer", "minimum": 0, "maximum": 150 },
--     "bio":   { "type": ["string", "null"], "maxLength": 500 },
--     "tags":  { "type": "array", "items": { "type": "string" }, ... },
--     "address": { ... }
--   },
--   "required": ["name", "email"],
--   "title": "User Profile",
--   "description": "A registered user in the system."
-- }
-- @
userProfileSchema :: Schema
userProfileSchema = objectSchema
  [ required "name" $ stringSchema
      & withMinLength 1
      & withMaxLength 128

  , required "email" $ stringSchema
      & withFormat "email"

  , optional "age" $ integerSchema
      & withMinimum 0
      & withMaximum 150

  , optional "bio" $ nullable stringSchema
      & withMaxLength 500
      & withDescription "A short biography, or null if not provided."

  , optional "tags" $ arraySchema stringSchema
      & withMinItems 1
      & withMaxItems 10
      & withUniqueItems True

  , optional "address" addressSchema
  ]
  & withTitle "User Profile"
  & withDescription "A registered user in the system."

-- | A nested object schema reused as a property of the user profile.
addressSchema :: Schema
addressSchema = objectSchema
  [ required "street" stringSchema
  , required "city"   stringSchema
  , optional "state"  $ stringSchema
      & withMinLength 2
      & withMaxLength 2
  , required "zip"    $ stringSchema
      & withPattern "^[0-9]{5}(-[0-9]{4})?$"
  ]

main :: IO ()
main = BL8.putStrLn $ Aeson.encode $ encode userProfileSchema
