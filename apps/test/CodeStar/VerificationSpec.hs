{-# OPTIONS_GHC -Wno-orphans #-}
module CodeStar.VerificationSpec (spec) where

import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CodeStar.Types (CheckResult (..), Evidence (..))
import CodeStar.Verification
  ( VerificationConfig (..)
  , VerificationResult (..)
  , verifyEvidence
  )

-- --------------------------------------------------------------------
-- Arbitrary instances
-- --------------------------------------------------------------------

instance Arbitrary CheckResult where
  arbitrary = elements [minBound .. maxBound]
  -- Shrink toward Passed (the "best" result) to find minimal failures.
  shrink Passed     = []
  shrink NotChecked = [Passed]
  shrink Failed     = [Passed, NotChecked]

instance Arbitrary VerificationConfig where
  arbitrary = VerificationConfig <$> arbitrary <*> arbitrary
  shrink (VerificationConfig rta rap) =
    [VerificationConfig rta' rap | rta' <- shrink rta] ++
    [VerificationConfig rta rap' | rap' <- shrink rap]

instance Arbitrary Evidence where
  arbitrary = Evidence
    <$> arbitrary                                                      -- testsPass
    <*> arbitrary                                                      -- buildSucceeds
    <*> listOf (listOf1 (elements (['a'..'z'] ++ ['/','_','.'])))    -- filesVerified
    <*> listOf (Text.pack <$> listOf1 (elements ['a'..'z']))          -- regressions
  shrink (Evidence tp bs fv reg) =
    [Evidence tp' bs  fv  reg  | tp' <- shrink tp] ++
    [Evidence tp  bs' fv  reg  | bs' <- shrink bs] ++
    [Evidence tp  bs  fv' reg  | fv' <- shrinkList (const []) fv] ++
    [Evidence tp  bs  fv  reg' | reg' <- shrinkList (const []) reg]

-- --------------------------------------------------------------------
-- Semantic ranks
--
-- CheckResult's derived Ord places constructors in declaration order
-- (Passed < Failed < NotChecked), which is the opposite of their
-- semantic quality.  Use explicit ranks throughout.
-- --------------------------------------------------------------------

-- | Semantic quality of a CheckResult: higher is better.
checkRank :: CheckResult -> Int
checkRank Failed     = 0
checkRank NotChecked = 1
checkRank Passed     = 2

-- | Semantic quality of a VerificationResult: higher is better.
resultRank :: VerificationResult -> Int
resultRank (VerificationFailed _)  = 0
resultRank (VerificationPartial _) = 1
resultRank (VerificationPassed _)  = 2

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = describe "CodeStar.Verification.verifyEvidence" $ do

  -- ----------------------------------------------------------------
  -- Independence: result class depends only on testsPass + config.
  -- buildSucceeds, filesVerified, and regressions are currently
  -- unexamined by verifyEvidence; these properties pin that contract.
  -- If a future change adds a buildSucceeds check, these will fail.
  -- ----------------------------------------------------------------
  describe "result class is independent of buildSucceeds, filesVerified, regressions" $ do

    prop "two Evidence values with the same testsPass produce the same result class" $
      \cfg ev1 ev2 ->
        ev1.testsPass == ev2.testsPass ==>
          resultRank (verifyEvidence cfg ev1) === resultRank (verifyEvidence cfg ev2)

    prop "replacing other fields with minimal values does not change result class" $
      \cfg ev ->
        let ev' = ev { buildSucceeds = NotChecked, filesVerified = [], regressions = [] }
        in resultRank (verifyEvidence cfg ev) === resultRank (verifyEvidence cfg ev')

  -- ----------------------------------------------------------------
  -- Precise characterisation of each config setting.
  -- ----------------------------------------------------------------
  describe "requireAllPassed=False (relaxed)" $ do

    prop "fails iff testsPass=Failed" $
      \ev ->
        isFailed (verifyEvidence relaxedCfg ev) === (ev.testsPass == Failed)

    prop "passes iff testsPass is not Failed" $
      \ev ->
        isPassed (verifyEvidence relaxedCfg ev) === (ev.testsPass /= Failed)

  describe "requireAllPassed=True (strict)" $ do

    prop "never produces VerificationFailed" $
      \ev ->
        not (isFailed (verifyEvidence strictCfg ev))

    prop "passes iff testsPass=Passed" $
      \ev ->
        isPassed (verifyEvidence strictCfg ev) === (ev.testsPass == Passed)

    prop "is partial iff testsPass is not Passed" $
      \ev ->
        isPartial (verifyEvidence strictCfg ev) === (ev.testsPass /= Passed)

  -- ----------------------------------------------------------------
  -- Monotonicity: a higher-quality testsPass never worsens the result.
  --
  -- Semantic ordering (checkRank): Failed(0) < NotChecked(1) < Passed(2)
  -- This is the OPPOSITE of the derived Ord instance.
  -- ----------------------------------------------------------------
  describe "monotone in testsPass" $ do

    prop "improving testsPass never decreases result rank (any config)" $
      \cfg ev cr2 ->
        checkRank cr2 >= checkRank ev.testsPass ==>
          let r1 = verifyEvidence cfg ev
              r2 = verifyEvidence cfg ev{testsPass = cr2}
          in counterexample
               ( "testsPass: " <> show ev.testsPass <> " -> " <> show cr2
               <> ", rank: " <> show (resultRank r1) <> " -> " <> show (resultRank r2)
               )
               (resultRank r2 >= resultRank r1)

    prop "testsPass=Passed is globally optimal: no other value gives a better result" $
      \cfg ev ->
        let rPassed = resultRank (verifyEvidence cfg ev{testsPass = Passed})
            rAny    = resultRank (verifyEvidence cfg ev)
        in rPassed >= rAny

    prop "testsPass=Failed is globally worst: no other value gives a worse result" $
      \cfg ev ->
        let rFailed = resultRank (verifyEvidence cfg ev{testsPass = Failed})
            rAny    = resultRank (verifyEvidence cfg ev)
        in rFailed <= rAny

  -- ----------------------------------------------------------------
  -- Config comparison.
  -- ----------------------------------------------------------------
  describe "config strictness" $ do

    prop "relaxed passes a superset of what strict passes" $
      \ev ->
        isPassed (verifyEvidence strictCfg ev) ==>
          isPassed (verifyEvidence relaxedCfg ev)

    prop "strict fails a subset of what relaxed fails (strict never fails)" $
      \ev ->
        isFailed (verifyEvidence relaxedCfg ev) \/
        not (isFailed (verifyEvidence strictCfg ev))

  -- ----------------------------------------------------------------
  -- Retained example tests (concrete regression guards).
  -- ----------------------------------------------------------------
  it "requireAllPassed=True and testsPass=Failed yields VerificationPartial" $
    verifyEvidence strictCfg (mkEvidence Failed)
      `shouldSatisfy` isPartial

  it "testsPass=Failed yields VerificationFailed when requireAllPassed=False" $
    verifyEvidence relaxedCfg (mkEvidence Failed)
      `shouldBe` VerificationFailed "Test suite failed"

  prop "otherwise yields VerificationPassed" $
    forAll (elements [Passed, NotChecked]) $ \result ->
      checkCoverage $
        cover 40 (result == Passed) "Passed" $
        cover 40 (result == NotChecked) "NotChecked" $
        verifyEvidence relaxedCfg (mkEvidence result)
          `shouldSatisfy` isPassed

-- --------------------------------------------------------------------
-- Fixed configs used in example tests
-- --------------------------------------------------------------------

strictCfg :: VerificationConfig
strictCfg = VerificationConfig{runTestsAfterGroup = True, requireAllPassed = True}

relaxedCfg :: VerificationConfig
relaxedCfg = VerificationConfig{runTestsAfterGroup = True, requireAllPassed = False}

-- Minimal evidence varying only testsPass (for example tests).
mkEvidence :: CheckResult -> Evidence
mkEvidence tr =
  Evidence
    { testsPass     = tr
    , buildSucceeds = NotChecked
    , filesVerified = ["src/Main.hs"]
    , regressions   = []
    }

-- --------------------------------------------------------------------
-- Predicates
-- --------------------------------------------------------------------

isPassed :: VerificationResult -> Bool
isPassed (VerificationPassed _) = True
isPassed _ = False

isPartial :: VerificationResult -> Bool
isPartial (VerificationPartial _) = True
isPartial _ = False

isFailed :: VerificationResult -> Bool
isFailed (VerificationFailed _) = True
isFailed _ = False

-- Disjunction lifted to Property (avoids Bool/Property impedance).
(\/) :: Bool -> Bool -> Property
a \/ b = property (a || b)
infixr 2 \/
