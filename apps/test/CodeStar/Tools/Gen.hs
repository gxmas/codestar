{-# OPTIONS_GHC -Wno-orphans #-}

module CodeStar.Tools.Gen
  ( arbitraryToolInput
  , genToolInput
  ) where

import Data.Aeson (Value (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Test.QuickCheck

import CodeStar.Tools.Registry (ToolInput (..))

-- | Generate a ToolInput with a mix of recognized field names and
-- random keys, with varied JSON value types.  This is designed as a
-- fuzzer: most generated inputs are structurally invalid (missing
-- required fields, wrong value types), exercising the input
-- validation paths of every tool handler.
genToolInput :: Gen ToolInput
genToolInput = ToolInput . Map.fromList <$> resize 4 (listOf genEntry)
  where
    genEntry = (,) <$> genKey <*> genVal
    genKey = frequency
      [ (3, elements ["path", "pattern", "content", "command", "old", "new",
                      "replace_all", "offset", "limit", "recursive"])
      , (1, Text.pack <$> listOf1 (elements ['a' .. 'z']))
      ]
    genVal = frequency
      [ (4, String . Text.pack <$> listOf (elements (['a' .. 'z'] ++ ['/', '.', '_'])))
      , (2, Number . fromIntegral <$> (choose (-10, 10000) :: Gen Int))
      , (1, Bool <$> arbitrary)
      , (1, pure Null)
      , (1, pure (String ""))
      ]

arbitraryToolInput :: Gen ToolInput
arbitraryToolInput = genToolInput

instance Arbitrary ToolInput where
  arbitrary = genToolInput
  shrink (ToolInput m) =
    [ToolInput (Map.deleteAt i m) | i <- [0 .. Map.size m - 1], Map.size m > 0]
