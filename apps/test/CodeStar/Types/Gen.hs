{-# OPTIONS_GHC -Wno-orphans #-}

-- | Arbitrary instances for types in CodeStar.Types.
-- Import this module (or its instances) in any spec that generates
-- these values rather than defining local orphans.
module CodeStar.Types.Gen where

import Data.Text (Text, pack)
import Test.QuickCheck

import CodeStar.Types

-- --------------------------------------------------------------------
-- Primitive generator
-- --------------------------------------------------------------------

arbitraryTypesText :: Gen Text
arbitraryTypesText = pack <$> listOf (elements ['a' .. 'z'])

-- --------------------------------------------------------------------
-- Enum instances (all Bounded + Enum)
-- --------------------------------------------------------------------

instance Arbitrary TaskType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FailureClass where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ModelRole where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PlanningMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CheckResult where
  arbitrary = arbitraryBoundedEnum

-- --------------------------------------------------------------------
-- Composite instances
-- --------------------------------------------------------------------

instance Arbitrary Evidence where
  arbitrary =
    Evidence
      <$> arbitrary
      <*> arbitrary
      <*> listOf (listOf (elements ['a' .. 'z']))
      <*> listOf arbitraryTypesText

instance Arbitrary ControlSignal where
  arbitrary =
    oneof
      [ pure Continue
      , NeedsInput <$> arbitraryTypesText
      , Blocked <$> arbitraryTypesText
      , Done <$> arbitrary
      ]

instance Arbitrary StepOutcome where
  arbitrary =
    oneof
      [ StepSuccess <$> arbitraryTypesText
      , StepFailure <$> arbitrary <*> arbitraryTypesText
      , StepNeedsReplan <$> arbitraryTypesText
      , StepTryAlternative <$> arbitrary
      ]

instance Arbitrary ObjectiveSpec where
  arbitrary =
    ObjectiveSpec
      <$> arbitraryTypesText
      <*> listOf (listOf (elements ['a' .. 'z']))
      <*> arbitrary
