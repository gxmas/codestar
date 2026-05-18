{-# LANGUAGE RecordWildCards #-}
module CodeStar.Config.PartialSpec (spec) where

import Data.Maybe (isJust)
import Data.Monoid (Last (..))
import Data.Text qualified as Text
import Test.Hspec hiding (context)
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CodeStar.Config.Gen ()
import CodeStar.Config.Types

-- Record construction (not update) to avoid deprecated type-directed disambiguation.
partialWithProvider :: String -> PartialConfig
partialWithProvider v =
  let PartialConfig{..} = mempty :: PartialConfig
  in PartialConfig{provider = Last (Just (Text.pack v)), ..}

-- --------------------------------------------------------------------
-- Distribution helpers
-- --------------------------------------------------------------------

isSet :: Last a -> Bool
isSet = isJust . getLast

-- Count how many of the 11 top-level Last fields are populated.
countSetFieldsPC :: PartialConfig -> Int
countSetFieldsPC p = length . filter id $
  [ isSet p.provider, isSet p.modelRoles, isSet p.models, isSet p.activeModel
  , isSet p.planningMode, isSet p.sandboxMode, isSet p.workspacePath
  , isSet p.apiKey, isSet p.indexStrategy, isSet p.permissions
  , isSet p.mcpEndpoints
  ]

fieldBandPC :: PartialConfig -> String
fieldBandPC p
  | n == 0    = "empty (0/11 fields)"
  | n <= 3    = "sparse (1-3/11 fields)"
  | n <= 7    = "mixed (4-7/11 fields)"
  | otherwise = "rich (8-11/11 fields)"
  where n = countSetFieldsPC p

countSetFieldsServer :: PartialServerSection -> Int
countSetFieldsServer p = length . filter id $
  [isSet p.port, isSet p.host, isSet p.httpTimeout,
   isSet p.gracefulShutdownTimeout, isSet p.pingInterval]

countSetFieldsCompaction :: PartialCompactionSection -> Int
countSetFieldsCompaction p = length . filter id $
  [isSet p.triggerFraction, isSet p.maxContextTokens]

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = describe "PartialConfig monoid laws" $ do

  -- cover 80: with 70/30 bias, P(count >= 4) ≈ 99%; would fail at 50/50 (83%).
  prop "left identity: mempty <> p == p" $
    \p ->
      classify (fieldBandPC p == "empty (0/11 fields)") "empty" $
      classify (fieldBandPC p == "sparse (1-3/11 fields)") "sparse (1-3)" $
      classify (fieldBandPC p == "mixed (4-7/11 fields)") "mixed (4-7)" $
      classify (fieldBandPC p == "rich (8-10/11 fields)") "rich (8-10)" $
      cover 80 (countSetFieldsPC p >= 4) "at least 4 top-level fields set" $
      (mempty <> p) === (p :: PartialConfig)

  prop "right identity: p <> mempty == p" $
    \p ->
      cover 80 (countSetFieldsPC p >= 4) "at least 4 top-level fields set" $
      (p <> mempty) === (p :: PartialConfig)

  -- checkCoverage makes this a hard failure if coverage drops below threshold.
  -- With 70/30 bias P(all three rich) ≈ 97%; reverted 50/50 bias gives ≈ 57%.
  prop "associativity: (a <> b) <> c == a <> (b <> c)" $
    \a b c ->
      checkCoverage $
      classify (countSetFieldsPC a == 0) "a empty" $
      classify (countSetFieldsPC b == 0) "b empty" $
      cover 70 (countSetFieldsPC a >= 4 && countSetFieldsPC b >= 4) "a and b both rich" $
      ((a <> b) <> c) === (a <> (b <> c) :: PartialConfig)

  prop "later layer wins: a field set in b overrides a" $
    \a v ->
      let b = partialWithProvider v
          merged = a <> b
       in merged.provider === Last (Just (Text.pack v))

  prop "earlier layer preserved when later is empty" $
    \v ->
      let a = partialWithProvider v
          merged = a <> mempty
       in merged.provider === Last (Just (Text.pack v))

  describe "section monoid laws hold independently" $ do

    prop "PartialServerSection: left identity" $
      \p ->
        classify (countSetFieldsServer p == 0) "empty" $
        classify (countSetFieldsServer p >= 3) "rich (3+/5 fields)" $
        cover 60 (countSetFieldsServer p >= 2) "at least 2 server fields set" $
        (mempty <> p) === (p :: PartialServerSection)

    prop "PartialServerSection: right identity" $
      \p ->
        cover 60 (countSetFieldsServer p >= 2) "at least 2 server fields set" $
        (p <> mempty) === (p :: PartialServerSection)

    prop "PartialServerSection: associativity" $
      \a b c ->
        cover 40 (countSetFieldsServer a >= 2 && countSetFieldsServer b >= 2) "a and b both non-trivial" $
        ((a <> b) <> c) === (a <> (b <> c) :: PartialServerSection)

    prop "PartialCompactionSection: left identity" $
      \p ->
        classify (countSetFieldsCompaction p == 0) "empty" $
        classify (countSetFieldsCompaction p == 2) "fully set" $
        cover 50 (countSetFieldsCompaction p >= 1) "at least 1 compaction field set" $
        (mempty <> p) === (p :: PartialCompactionSection)

    prop "PartialCompactionSection: associativity" $
      \a b c ->
        cover 40 (countSetFieldsCompaction a >= 1 && countSetFieldsCompaction b >= 1) "a and b both non-trivial" $
        ((a <> b) <> c) === (a <> (b <> c) :: PartialCompactionSection)

    prop "PartialBudgetSection: left identity" $
      \p -> (mempty <> p) === (p :: PartialBudgetSection)

    prop "PartialBudgetSection: associativity" $
      \a b c -> ((a <> b) <> c) === (a <> (b <> c) :: PartialBudgetSection)

    prop "PartialGuardrailsSection: left identity" $
      \p -> (mempty <> p) === (p :: PartialGuardrailsSection)

    prop "PartialGuardrailsSection: associativity" $
      \a b c -> ((a <> b) <> c) === (a <> (b <> c) :: PartialGuardrailsSection)
