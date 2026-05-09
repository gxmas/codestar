module Main (main) where

import Test.Hspec

import qualified Data.JsonSchema.CombinatorsSpec
import qualified Data.JsonSchema.IntegrationSpec
import qualified Data.JsonSchema.OrderedMapSpec
import qualified Data.JsonSchema.SchemaSpec
import qualified Data.JsonSchema.SerializationSpec

main :: IO ()
main = hspec $ do
  describe "OrderedMap" Data.JsonSchema.OrderedMapSpec.spec
  describe "Schema" Data.JsonSchema.SchemaSpec.spec
  describe "Combinators" Data.JsonSchema.CombinatorsSpec.spec
  describe "Serialization" Data.JsonSchema.SerializationSpec.spec
  describe "Integration" Data.JsonSchema.IntegrationSpec.spec
