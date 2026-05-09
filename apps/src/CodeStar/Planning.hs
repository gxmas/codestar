module CodeStar.Planning
  ( -- * Plan types
    PlanStep (..)
  , StepStatus (..)
  , Plan (..)
  , emptyPlan

    -- * Validation
  , PlanError (..)
  , validatePlan

    -- * Fingerprinting (de-duplication)
  , PlanFingerprint
  , fingerprintPlan
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

import CodeStar.Types (StepId (..))

-- --------------------------------------------------------------------
-- Plan types
-- --------------------------------------------------------------------

data StepStatus
  = Pending
  | InProgress
  | Completed
  | Skipped
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PlanStep = PlanStep
  { stepId :: !StepId
  , description :: !Text
  , contextFiles :: ![FilePath]
  -- ^ Uses: files to scope context
  , dependsOn :: !(Set StepId)
  , alternatives :: ![Text]
  -- ^ alternative approaches if this fails
  , status :: !StepStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Plan = Plan
  { steps :: ![PlanStep]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyPlan :: Plan
emptyPlan = Plan{steps = []}

-- --------------------------------------------------------------------
-- Validation
-- --------------------------------------------------------------------

data PlanError
  = DuplicateStepId StepId
  | -- | step, unknown dep
    DanglingDependency StepId StepId
  | -- | cycle path
    CyclicDependency [StepId]
  deriving stock (Eq, Show)

-- | Validate a plan: no duplicate IDs, no dangling refs, no cycles.
validatePlan :: Plan -> Either PlanError ()
validatePlan plan = do
  checkDuplicates plan.steps
  checkDangling plan.steps
  checkCycles plan.steps

checkDuplicates :: [PlanStep] -> Either PlanError ()
checkDuplicates steps =
  let ids = map (.stepId) steps
      uniq = nub ids
      duplicates = [i | i <- ids, length (filter (== i) ids) > 1]
   in if length ids == length uniq
        then Right ()
        else case duplicates of
          (dup : _) -> Left (DuplicateStepId dup)
          [] -> Right ()

checkDangling :: [PlanStep] -> Either PlanError ()
checkDangling steps =
  let idSet = Set.fromList (map (.stepId) steps)
   in case [ (s.stepId, d)
           | s <- steps
           , d <- Set.toList s.dependsOn
           , not (Set.member d idSet)
           ] of
        [] -> Right ()
        (sid, d) : _ -> Left (DanglingDependency sid d)

-- | Kahn's algorithm: detect cycles by trying to produce a topological order.
checkCycles :: [PlanStep] -> Either PlanError ()
checkCycles steps =
  let graph = Map.fromList [(s.stepId, s.dependsOn) | s <- steps]
      inDegree =
        Map.fromListWith
          (+)
          [(d, 1) | s <- steps, d <- Set.toList s.dependsOn]
      zeros =
        [ s.stepId
        | s <- steps
        , not (Map.member s.stepId inDegree)
        ]
      allIds = map (.stepId) steps
   in case kahn graph inDegree zeros allIds of
        Right _ -> Right ()
        Left cy -> Left (CyclicDependency cy)

kahn ::
  Map StepId (Set StepId) ->
  Map StepId Int ->
  [StepId] ->
  [StepId] ->
  Either [StepId] [StepId]
kahn graph inDeg queue allIds = go queue [] inDeg
 where
  go [] visited _deg =
    let remaining = filter (`notElem` visited) allIds
     in if null remaining
          then Right visited
          else Left remaining
  go (n : ns) visited deg =
    let deps = maybe [] Set.toList (Map.lookup n graph)
        newDeg =
          foldl
            ( \d p ->
                Map.adjust (subtract 1) p d
            )
            deg
            deps
        newZeros = [p | p <- deps, Map.findWithDefault 0 p newDeg == 0]
     in go (ns ++ newZeros) (visited ++ [n]) newDeg

-- --------------------------------------------------------------------
-- Fingerprinting
-- --------------------------------------------------------------------

{- | A canonical hash of a plan used to detect repeated approaches.
Two plans with the same step descriptions and context files are identical.
-}
newtype PlanFingerprint = PlanFingerprint Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

fingerprintPlan :: Plan -> PlanFingerprint
fingerprintPlan plan =
  let parts = map stepSignature plan.steps
      sig = Text.intercalate "|" (sort parts)
   in PlanFingerprint sig

stepSignature :: PlanStep -> Text
stepSignature s =
  s.description
    <> "@"
    <> Text.intercalate "," (sort (map Text.pack s.contextFiles))
