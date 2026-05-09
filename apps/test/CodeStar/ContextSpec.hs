module CodeStar.ContextSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CodeStar.Context
  ( ContextConfig (..)
  , TokenBudget (..)
  , computeBudget
  )

-- --------------------------------------------------------------------
-- Generators
-- --------------------------------------------------------------------

genContextConfig :: Gen ContextConfig
genContextConfig = do
  maxTok <- chooseInt (10_000, 500_000)
  rm <- chooseInt (0, 8_000)
  mem <- chooseInt (0, 4_000)
  comp <- chooseInt (0, 2_000)
  resp <- chooseInt (0, 16_000)
  pure
    ContextConfig
      { maxContextTokens = maxTok
      , repoMapReserve = rm
      , memoryReserve = mem
      , compactionReserve = comp
      , responseReserve = resp
      }

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

totalReserve :: ContextConfig -> Int -> Int
totalReserve cfg spt =
  cfg.repoMapReserve + cfg.memoryReserve + cfg.compactionReserve + cfg.responseReserve + spt

spec :: Spec
spec = describe "CodeStar.Context.computeBudget" $ do
  prop "both budgets are non-negative" $
    forAll genContextConfig $ \cfg ->
      forAll (chooseInt (0, 10_000)) $ \spt ->
        checkCoverage $
          cover 1 (totalReserve cfg spt > cfg.maxContextTokens) "reserves exceed limit" $
          cover 50 (totalReserve cfg spt <= cfg.maxContextTokens) "reserves within limit" $
          let b = computeBudget cfg spt
           in b.budgetForHistory >= 0 .&&. b.budgetForFiles >= 0

  prop "budgets sum to the remaining token pool" $
    forAll genContextConfig $ \cfg ->
      forAll (chooseInt (0, 10_000)) $ \spt ->
        let b = computeBudget cfg spt
            reserved = totalReserve cfg spt
            remaining = max 0 (cfg.maxContextTokens - reserved)
         in b.budgetForHistory + b.budgetForFiles === remaining

  prop "history budget >= file budget (75/25 split)" $
    forAll genContextConfig $ \cfg ->
      forAll (chooseInt (0, 10_000)) $ \spt ->
        checkCoverage $
          cover 50 ((computeBudget cfg spt).budgetForHistory > 0) "non-zero history budget" $
          let b = computeBudget cfg spt
           in b.budgetForHistory >= b.budgetForFiles

  prop "is monotone: larger system prompt tokens reduce both budgets" $
    forAll genContextConfig $ \cfg ->
      forAll (chooseInt (0, 5_000)) $ \s1 ->
        forAll (chooseInt (s1, s1 + 5_000)) $ \s2 ->
          checkCoverage $
            cover 40 (s2 > s1) "s2 strictly larger than s1" $
            let b1 = computeBudget cfg s1
                b2 = computeBudget cfg s2
             in counterexample
                  ( "s1=" <> show s1 <> " s2=" <> show s2
                      <> " h1=" <> show b1.budgetForHistory
                      <> " h2=" <> show b2.budgetForHistory
                  )
                  $ b2.budgetForHistory <= b1.budgetForHistory
                    .&&. b2.budgetForFiles <= b1.budgetForFiles

  prop "zero reserves give full budget to history+files" $
    forAll (chooseInt (10_000, 500_000)) $ \maxTok ->
      let cfg = ContextConfig maxTok 0 0 0 0
          b = computeBudget cfg 0
       in b.budgetForHistory + b.budgetForFiles === maxTok
