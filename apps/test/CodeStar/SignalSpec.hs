module CodeStar.SignalSpec (spec) where

import Test.Hspec

import CodeStar.LLM.Base (LlmError (..), ToolName (..))
import CodeStar.Signal
  ( assessControlSignal
  , assessWithHistory
  , classifyLlmError
  , classifyToolError
  , worstSignal
  )
import CodeStar.Tools.Registry (ToolError (..))
import CodeStar.Types (ControlSignal (..), FailureClass (..), StepOutcome (..))

spec :: Spec
spec = describe "CodeStar.Signal" $ do
  it "maps step outcomes to expected control signals" $ do
    assessControlSignal (StepSuccess "ok") `shouldBe` Continue
    assessControlSignal (StepFailure Validation "bad") `shouldBe` NeedsInput "Validation failure — fix inputs: bad"
    assessControlSignal (StepTryAlternative 2) `shouldBe` Continue

  it "upgrades to Blocked after 3+ consecutive failures in history" $ do
    let history =
          [ StepFailure Validation "a"
          , StepFailure Execution "b"
          , StepNeedsReplan "c"
          ]
    assessWithHistory history (StepSuccess "ignored")
      `shouldBe` Blocked "Loop detected: 3 or more consecutive failures without progress"

  it "classifies LLM and tool errors into expected failure classes" $ do
    classifyLlmError (RateLimited 1) `shouldBe` Transient
    classifyLlmError (InvalidRequest "x") `shouldBe` Validation
    classifyToolError (ToolNotFound (ToolName "x")) `shouldBe` Validation
    classifyToolError (ExecutionFailed "boom") `shouldBe` Execution

  it "worstSignal prefers highest severity" $ do
    worstSignal [Continue, NeedsInput "x", Blocked "b"]
      `shouldBe` Blocked "b"
