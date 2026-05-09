{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.MCP.Tasks.Generators () where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Network.MCP.Tasks
import Network.MCP.Types (RPCError (..))
import Network.MCP.Types.Capabilities (TaskStatus (..), TaskErrorKind (..), TaskError (..))

------------------------------------------------------------------------
-- Helpers (re-export helpers from mcp-core generators)
------------------------------------------------------------------------

shortText :: Gen Text
shortText = T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9']))

arbitraryIso8601 :: Gen Text
arbitraryIso8601 = do
  year  <- choose (2020, 2030 :: Int)
  month <- choose (1, 12 :: Int)
  day   <- choose (1, 28 :: Int)
  hour  <- choose (0, 23 :: Int)
  minute <- choose (0, 59 :: Int)
  sec   <- choose (0, 59 :: Int)
  pure $ T.pack $
    show year ++ "-" ++ pad month ++ "-" ++ pad day ++ "T"
    ++ pad hour ++ ":" ++ pad minute ++ ":" ++ pad sec ++ "Z"
  where
    pad n = if n < 10 then "0" ++ show n else show n

------------------------------------------------------------------------
-- Orphan instances for types from mcp-core (no test-lib exposed)
------------------------------------------------------------------------

instance Arbitrary TaskStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary RPCError where
  arbitrary = RPCError <$> arbitrary <*> shortText <*> pure Nothing

instance Arbitrary TaskErrorKind where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary TaskError where
  arbitrary = TaskError <$> arbitrary <*> shortText

------------------------------------------------------------------------
-- Task record
------------------------------------------------------------------------

instance Arbitrary Task where
  arbitrary =
    Task
      <$> shortText
      <*> arbitrary
      <*> liftArbitrary shortText
      <*> arbitraryIso8601
      <*> arbitraryIso8601
      <*> choose (1000, 3600000)
