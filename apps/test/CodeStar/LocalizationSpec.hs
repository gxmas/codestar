{-# LANGUAGE OverloadedStrings #-}

module CodeStar.LocalizationSpec (spec) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import Test.Hspec

import CodeStar.LLM.Base
import CodeStar.Localization
import CodeStar.Types (ObjectiveSpec (..), TaskType (..))

spec :: Spec
spec = describe "CodeStar.Localization" $ do
  it "has expected default localization limits" $ do
    defaultLocalizationConfig.maxFiles `shouldBe` 5
    defaultLocalizationConfig.maxFunctions `shouldBe` 3
    defaultLocalizationConfig.maxPatches `shouldBe` 3

  it "runs full localization pipeline with mocked LLM outputs" $ do
    client <- mkClient
      [ "src/A.hs: likely bug in parser"
      , "src/A.hs:parseFoo:10-20: suspicious branch"
      , "FILE: src/A.hs\nOLD:\nold\nNEW:\nnew\nREASON: fix it\n---"
      ]
    let objective = ObjectiveSpec "Parser bug" ["src/A.hs"] Bug
    result <- localize client defaultLocalizationConfig objective "repo overview"
    case result of
      Left err -> expectationFailure ("Expected Right LocalizationResult, got Left: " <> show err)
      Right lr -> do
        length lr.lrFiles `shouldBe` 1
        length lr.lrFunctions `shouldBe` 1
        length lr.lrPatches `shouldBe` 1

  it "propagates LLM failure as Left" $ do
    let badClient = mkFailingClient "boom"
        objective = ObjectiveSpec "Parser bug" [] Bug
    result <- localize badClient defaultLocalizationConfig objective "ctx"
    result `shouldSatisfy` isLeft

  it "ignores malformed function ranges and empty function names" $ do
    client <- mkClient
      [ "src/A.hs: likely bug"
      , Text.unlines
          [ "src/A.hs:goodFn:10-20: valid"
          , "src/A.hs::10-20: missing name"
          , "src/A.hs:badRange:abc-def: invalid"
          , "src/A.hs:negRange:-1-3: invalid"
          ]
      , "FILE: src/A.hs\nOLD:\nold\nNEW:\nnew\nREASON: ok\n---"
      ]
    let objective = ObjectiveSpec "Parser bug" ["src/A.hs"] Bug
    result <- localize client defaultLocalizationConfig objective "repo overview"
    case result of
      Left err -> expectationFailure ("Expected Right LocalizationResult, got Left: " <> show err)
      Right lr -> do
        length lr.lrFunctions `shouldBe` 1
        case lr.lrFunctions of
          [fn] -> do
            fn.lnName `shouldBe` "goodFn"
            fn.lnStartLine `shouldBe` 10
            fn.lnEndLine `shouldBe` 20
          _ -> expectationFailure "Expected exactly one valid function localization"

  it "ignores malformed patch blocks with empty FILE path" $ do
    client <- mkClient
      [ "src/A.hs: likely bug"
      , "src/A.hs:parseFoo:10-20: suspicious"
      , Text.unlines
          [ "FILE: "
          , "OLD:"
          , "bad old"
          , "NEW:"
          , "bad new"
          , "REASON: empty file path should be ignored"
          , "---"
          , "FILE: src/A.hs"
          , "OLD:"
          , "old"
          , "NEW:"
          , "new"
          , "REASON: valid patch"
          , "---"
          ]
      ]
    let objective = ObjectiveSpec "Parser bug" ["src/A.hs"] Bug
    result <- localize client defaultLocalizationConfig objective "repo overview"
    case result of
      Left err -> expectationFailure ("Expected Right LocalizationResult, got Left: " <> show err)
      Right lr -> do
        length lr.lrPatches `shouldBe` 1
        case lr.lrPatches of
          [pc] -> pc.pcFile `shouldBe` "src/A.hs"
          _ -> expectationFailure "Expected exactly one valid patch candidate"

mkClient :: [Text.Text] -> IO LlmClientDict
mkClient responses = do
  ref <- newIORef responses
  pure
    LlmClientDict
      { clientInfo = ClientInfo "spec" "mock"
      , complete = \_ -> do
          xs <- readIORef ref
          case xs of
            [] -> pure (Left (ProviderError "no more responses"))
            (x : rest) -> do
              writeIORef ref rest
              pure $
                Right
                  CompletionResponse
                    { responseContent = [TextContent x]
                    , stopReason = EndTurn
                    , usage = TokenCount 0 0
                    }
      , stream = \_ _ -> pure (Left (ProviderError "unused"))
      , countTokens = \_ -> pure (Right (TokenCount 0 0))
      }

mkFailingClient :: Text.Text -> LlmClientDict
mkFailingClient msg =
  LlmClientDict
    { clientInfo = ClientInfo "spec" "failing"
    , complete = \_ -> pure (Left (ProviderError msg))
    , stream = \_ _ -> pure (Left (ProviderError msg))
    , countTokens = \_ -> pure (Right (TokenCount 0 0))
    }

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
