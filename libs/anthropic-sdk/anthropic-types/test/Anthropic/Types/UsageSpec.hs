module Anthropic.Types.UsageSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Types
import Anthropic.Types.Generators ()

spec :: Spec
spec = do
  describe "ServerToolUsage" $
    prop "roundtrip" $ \(x :: ServerToolUsage) ->
      eitherDecode (encode x) === Right x

  describe "CacheCreationUsage" $
    prop "roundtrip" $ \(x :: CacheCreationUsage) ->
      eitherDecode (encode x) === Right x

  describe "Usage" $
    prop "roundtrip" $ \(x :: Usage) ->
      eitherDecode (encode x) === Right x
