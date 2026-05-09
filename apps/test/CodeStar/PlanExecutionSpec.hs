module CodeStar.PlanExecutionSpec (spec) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec

import CodeStar.LLM.Base
import CodeStar.PlanExecution
import CodeStar.Planning (PlanStep (..))
import CodeStar.Types (ControlSignal (..), ObjectiveSpec (..), StepId (..), TaskType (..))

spec :: Spec
spec = describe "CodeStar.PlanExecution" $ do
  it "retries failed steps up to maxReplans" $ do
    arch <- scriptedTextClient ["src/A.hs"]
    planner <-
      scriptedTextClient
        [ Text.unlines
            [ "STEP: implement"
            , "ID: step-1"
            , "USES: src/A.hs"
            , "---"
            ]
        ]
    attemptsRef <- newIORef (0 :: Int)
    let cfg = defaultPlanExecutionConfig{maxReplans = 2}
        execute _ = do
          modifyIORef' attemptsRef (+ 1)
          n <- readIORef attemptsRef
          pure $ if n < 3 then NeedsInput "retry" else Continue

    signal <- runWithPlan arch planner arch cfg featureSpec Set.empty execute
    signal `shouldSatisfy` isDone
    readIORef attemptsRef `shouldReturn` 3

  it "replans until a valid plan is produced" $ do
    arch <- scriptedTextClient ["src/A.hs"]
    planner <-
      scriptedTextClient
        [ Text.unlines
            [ "STEP: invalid one"
            , "ID: duplicate"
            , "USES: src/A.hs"
            , "---"
            , "STEP: invalid two"
            , "ID: duplicate"
            , "USES: src/B.hs"
            , "---"
            ]
        , Text.unlines
            [ "STEP: valid"
            , "ID: valid-step"
            , "USES: src/A.hs"
            , "---"
            ]
        ]
    callsRef <- newIORef (0 :: Int)
    let cfg = defaultPlanExecutionConfig{maxReplans = 2}
        execute _ = modifyIORef' callsRef (+ 1) >> pure Continue

    signal <- runWithPlan arch planner arch cfg featureSpec Set.empty execute
    signal `shouldSatisfy` isDone
    readIORef callsRef `shouldReturn` 1

  it "does not re-execute completed steps in DAG mode" $ do
    arch <- scriptedTextClient ["src/A.hs, src/B.hs"]
    planner <-
      scriptedTextClient
        [ Text.unlines
            [ "STEP: first"
            , "ID: step-1"
            , "USES: src/A.hs"
            , "---"
            , "STEP: second"
            , "ID: step-2"
            , "DEPENDS: step-1"
            , "USES: src/B.hs"
            , "---"
            ]
        ]
    seenRef <- newIORef ([] :: [StepId])
    let execute step = do
          modifyIORef' seenRef (++ [step.stepId])
          pure Continue

    signal <- runWithPlanDag arch planner arch defaultPlanExecutionConfig featureSpec Set.empty execute
    signal `shouldSatisfy` isDone
    seen <- readIORef seenRef
    length seen `shouldBe` 2
    Set.fromList seen `shouldBe` Set.fromList [StepId "step-1", StepId "step-2"]

featureSpec :: ObjectiveSpec
featureSpec =
  ObjectiveSpec
    { description = "Implement feature"
    , contextFiles = []
    , taskType = Feature
    }

scriptedTextClient :: [Text] -> IO LlmClientDict
scriptedTextClient outputs = do
  ref <- newIORef outputs
  pure
    LlmClientDict
      { clientInfo = ClientInfo "test" "test-model"
      , complete = \_ -> Right . asResponse <$> popText ref
      , stream = \_ _ -> Right . asResponse <$> popText ref
      , countTokens = \_ -> pure (Right TokenCount{inputTokens = 0, outputTokens = 0})
      }

popText :: IORef [Text] -> IO Text
popText ref = do
  xs <- readIORef ref
  case xs of
    (x : rest) -> writeIORef ref rest >> pure x
    [] -> pure "STEP: fallback\nID: step-fallback\nUSES: src/Fallback.hs\n---"

asResponse :: Text -> CompletionResponse
asResponse txt =
  CompletionResponse
    { responseContent = [TextContent txt]
    , stopReason = EndTurn
    , usage = TokenCount 1 1
    }

isDone :: ControlSignal -> Bool
isDone (Done _) = True
isDone _ = False
