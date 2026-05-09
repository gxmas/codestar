module CodeStar.Tools.ShellSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.Aeson (Value (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Test.Hspec

import CodeStar.Platform.Sandbox (Sandbox (..))
import CodeStar.Tools.Registry
  ( ToolError (..)
  , ToolHandlerDict (..)
  , ToolInput (..)
  , ToolOutput (..)
  )
import CodeStar.Tools.Shell (shellToolHandler)

spec :: Spec
spec = describe "CodeStar.Tools.Shell" $ do
  it "runs foreground commands through sandbox and returns output" $ do
    let sandbox =
          Sandbox
            { runCommand = \_ -> pure (Right "hello-shell")
            , copyIn = \_ _ -> pure ()
            , copyOut = \_ _ -> pure ()
            , teardown = pure ()
            }
        handler = shellToolHandler sandbox
    res <- invoke handler (mkInput [("command", String "echo hi")])
    res `shouldBe` Right (ToolOutput "hello-shell" False)

  it "returns Timeout when command exceeds timeout_ms" $ do
    let sandbox =
          Sandbox
            { runCommand = \_ -> threadDelay 100000 >> pure (Right "late")
            , copyIn = \_ _ -> pure ()
            , copyOut = \_ _ -> pure ()
            , teardown = pure ()
            }
        handler = shellToolHandler sandbox
    res <- invoke handler (mkInput [("command", String "slow"), ("timeout_ms", Number 1)])
    res `shouldBe` Left Timeout

  it "truncates oversized output in foreground mode" $ do
    let longOut = Text.replicate 60000 "x"
        sandbox =
          Sandbox
            { runCommand = \_ -> pure (Right longOut)
            , copyIn = \_ _ -> pure ()
            , copyOut = \_ _ -> pure ()
            , teardown = pure ()
            }
        handler = shellToolHandler sandbox
    res <- invoke handler (mkInput [("command", String "big")])
    case res of
      Left err -> expectationFailure ("Expected Right ToolOutput, got: " <> show err)
      Right out -> do
        truncated out `shouldBe` True
        content out `shouldSatisfy` Text.isInfixOf "[Output truncated]"

  it "background mode returns immediate started message" $ do
    let sandbox =
          Sandbox
            { runCommand = \_ -> threadDelay 100000 >> pure (Right "done")
            , copyIn = \_ _ -> pure ()
            , copyOut = \_ _ -> pure ()
            , teardown = pure ()
            }
        handler = shellToolHandler sandbox
    res <- invoke handler (mkInput [("command", String "run"), ("background", Bool True)])
    case res of
      Left err -> expectationFailure ("Expected Right ToolOutput, got: " <> show err)
      Right out -> content out `shouldSatisfy` Text.isInfixOf "Command started in background: run"

  it "returns execution failures from sandbox in foreground mode" $ do
    let sandbox =
          Sandbox
            { runCommand = \_ -> pure (Left "boom")
            , copyIn = \_ _ -> pure ()
            , copyOut = \_ _ -> pure ()
            , teardown = pure ()
            }
        handler = shellToolHandler sandbox
    res <- invoke handler (mkInput [("command", String "explode")])
    res `shouldSatisfy` isExecutionFailed

  it "returns execution failure when sandbox throws an exception" $ do
    let sandbox =
          Sandbox
            { runCommand = \_ -> throwIO (userError "kaboom")
            , copyIn = \_ _ -> pure ()
            , copyOut = \_ _ -> pure ()
            , teardown = pure ()
            }
        handler = shellToolHandler sandbox
    res <- invoke handler (mkInput [("command", String "explode")])
    res `shouldSatisfy` isExecutionFailed

  it "rejects malformed and out-of-range input payloads" $ do
    let sandbox =
          Sandbox
            { runCommand = \_ -> pure (Right "ok")
            , copyIn = \_ _ -> pure ()
            , copyOut = \_ _ -> pure ()
            , teardown = pure ()
            }
        handler = shellToolHandler sandbox
    r1 <- invoke handler (mkInput [])
    r2 <- invoke handler (mkInput [("command", Number 1)])
    r3 <- invoke handler (mkInput [("command", String "ok"), ("timeout_ms", String "x")])
    r4 <- invoke handler (mkInput [("command", String "ok"), ("background", String "x")])
    r5 <- invoke handler (mkInput [("command", String "ok"), ("timeout_ms", Number 0)])
    r6 <- invoke handler (mkInput [("command", String "ok"), ("timeout_ms", Number (-1))])
    r7 <- invoke handler (mkInput [("command", String "ok"), ("timeout_ms", Number 1.5)])
    r1 `shouldSatisfy` isInvalidInput
    r2 `shouldSatisfy` isInvalidInput
    r3 `shouldSatisfy` isInvalidInput
    r4 `shouldSatisfy` isInvalidInput
    r5 `shouldSatisfy` isInvalidInput
    r6 `shouldSatisfy` isInvalidInput
    r7 `shouldSatisfy` isInvalidInput

mkInput :: [(Text.Text, Value)] -> ToolInput
mkInput entries = ToolInput{arguments = Map.fromList entries}

isInvalidInput :: Either ToolError ToolOutput -> Bool
isInvalidInput (Left (InvalidInput _)) = True
isInvalidInput _ = False

isExecutionFailed :: Either ToolError ToolOutput -> Bool
isExecutionFailed (Left (ExecutionFailed _)) = True
isExecutionFailed _ = False
