module CodeStar.PlanningSpec (spec) where

import Data.List (nub)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CodeStar.Planning
import CodeStar.Types (StepId (..))

-- --------------------------------------------------------------------
-- Generators
-- --------------------------------------------------------------------

-- | Generate an acyclic plan: each step may only depend on steps with
-- lower indices, guaranteeing no cycles by construction.
genAcyclicPlan :: Gen Plan
genAcyclicPlan = do
  n <- chooseInt (1, 8)
  steps <- mapM (genStep n) [0 .. n - 1]
  pure (Plan steps)
 where
  genStep n i = do
    depCount <- chooseInt (0, min i 3)
    deps <- fmap nub $ vectorOf depCount $ chooseInt (0, max 0 (i - 1))
    desc <- elements ["parse", "build", "test", "deploy", "lint", "render"]
    pure $ mkStep ("s" <> Text.pack (show i)) (Text.pack desc) (map (\j -> "s" <> Text.pack (show j)) deps)

-- | All step IDs in a plan.
planStepIds :: Plan -> [StepId]
planStepIds p = map (.stepId) p.steps

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = describe "CodeStar.Planning" $ do
  describe "validatePlan" $ do
    it "emptyPlan validates" $
      validatePlan emptyPlan `shouldBe` Right ()

    it "duplicate step ids return DuplicateStepId" $
      validatePlan (Plan [mkStep "s1" "a" [], mkStep "s1" "b" []])
        `shouldBe` Left (DuplicateStepId (StepId "s1"))

    it "dependency on non-existent step returns DanglingDependency" $
      validatePlan (Plan [mkStep "s1" "a" ["missing"]])
        `shouldBe` Left (DanglingDependency (StepId "s1") (StepId "missing"))

    it "cycle A->B->C->A returns CyclicDependency" $
      case validatePlan (Plan [aStep, bStep, cStep]) of
        Left (CyclicDependency cyclePath) -> cyclePath `shouldSatisfy` (not . null)
        other -> expectationFailure ("expected CyclicDependency, got " <> show other)

    prop "acyclic plans always validate successfully" $
      forAll genAcyclicPlan $ \plan ->
        checkCoverage $
          cover 10 (length plan.steps == 1) "single step" $
          cover 25 (length plan.steps >= 2 && length plan.steps <= 4) "2-4 steps" $
          cover 25 (length plan.steps >= 5) "5+ steps" $
          cover 40 (any (not . Set.null . (.dependsOn)) plan.steps) "has dependencies" $
          validatePlan plan === Right ()

    prop "self-dependency always produces CyclicDependency" $
      forAll genAcyclicPlan $ \plan ->
        not (null plan.steps) ==>
          let step = head plan.steps
              selfDep = step{dependsOn = Set.insert step.stepId step.dependsOn}
              badPlan = Plan (selfDep : tail plan.steps)
           in case validatePlan badPlan of
                Left (CyclicDependency path) ->
                  counterexample ("cycle path: " <> show path) $
                    step.stepId `elem` path
                other ->
                  counterexample ("expected CyclicDependency, got: " <> show other) False

    prop "cycle error path contains only known step IDs" $
      forAll genAcyclicPlan $ \plan ->
        not (null plan.steps) ==>
          let step = head plan.steps
              selfDep = step{dependsOn = Set.insert step.stepId step.dependsOn}
              badPlan = Plan (selfDep : tail plan.steps)
              knownIds = Set.fromList (planStepIds badPlan)
           in case validatePlan badPlan of
                Left (CyclicDependency path) ->
                  all (`Set.member` knownIds) path
                _ -> True

  describe "fingerprintPlan" $ do
    prop "is permutation-invariant on steps" $
      forAll (shuffle baseSteps) $ \steps' ->
        checkCoverage $
          cover 50 (steps' /= baseSteps) "non-trivial permutation" $
          fingerprintPlan (Plan steps') === fingerprintPlan (Plan baseSteps)

    it "distinguishes plans with different descriptions" $
      let p1 = Plan [mkStep "s1" "implement api" []]
          p2 = Plan [mkStep "s1" "implement auth" []]
       in fingerprintPlan p1 `shouldNotBe` fingerprintPlan p2

    prop "is permutation-invariant for generated acyclic plans" $
      forAll genAcyclicPlan $ \plan ->
        forAll (shuffle plan.steps) $ \shuffled ->
          fingerprintPlan (Plan shuffled) === fingerprintPlan plan

mkStep :: Text -> Text -> [Text] -> PlanStep
mkStep sid desc deps =
  PlanStep
    { stepId = StepId sid
    , description = desc
    , contextFiles = ["src/Main.hs"]
    , dependsOn = Set.fromList (map StepId deps)
    , alternatives = []
    , status = Pending
    }

baseSteps :: [PlanStep]
baseSteps =
  [ mkStep "s1" "parse files" []
  , mkStep "s2" "build graph" ["s1"]
  , mkStep "s3" "render output" ["s2"]
  ]

aStep :: PlanStep
aStep = mkStep "a" "A" ["c"]

bStep :: PlanStep
bStep = mkStep "b" "B" ["a"]

cStep :: PlanStep
cStep = mkStep "c" "C" ["b"]
