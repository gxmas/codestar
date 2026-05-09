{-# LANGUAGE OverloadedStrings #-}

module CodeStar.LLM.BaseSpec (spec) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Test.Hspec

import CodeStar.Config (ModelSpec (..))
import CodeStar.LLM.Base
import CodeStar.Types (ModelRole (..))

spec :: Spec
spec = describe "CodeStar.LLM.Base" $ do
  it "trivialResolver always returns the same client" $ do
    client <- mkClient "p" "m"
    let resolver = trivialResolver client
    clientInfo (resolver Architect) `shouldBe` clientInfo client
    clientInfo (resolver Coder) `shouldBe` clientInfo client

  it "withDefaults overrides completion request knobs" $ do
    seenRef <- newIORef Nothing
    base <- mkRecordingClient seenRef
    let wrapped = withDefaults (Just 0.2) (Just 0.9) (Just 123) base
        req =
          CompletionRequest
            { messages = []
            , systemPrompt = Nothing
            , tools = []
            , maxTokens = 50
            , temperature = Nothing
            , topP = Nothing
            }
    _ <- wrapped.complete req
    seen <- readIORef seenRef
    case seen of
      Nothing -> expectationFailure "Expected complete to record request"
      Just r -> do
        r.maxTokens `shouldBe` 123
        r.temperature `shouldBe` Just 0.2
        r.topP `shouldBe` Just 0.9

  it "withFallback retries on NetworkError and succeeds with fallback" $ do
    let primary =
          LlmClientDict
            { clientInfo = ClientInfo "primary" "m"
            , complete = \_ -> pure (Left (NetworkError "down"))
            , stream = \_ _ -> pure (Left (NetworkError "down"))
            , countTokens = \_ -> pure (Left (NetworkError "down"))
            }
    fallback <- mkClient "fallback" "m"
    let wrapped = withFallback primary fallback
    res <- wrapped.complete minimalReq
    res `shouldSatisfy` isRight

  it "withFallback does not fallback on non-transient errors" $ do
    fallbackCalled <- newIORef False
    let primary =
          LlmClientDict
            { clientInfo = ClientInfo "primary" "m"
            , complete = \_ -> pure (Left (ProviderError "bad request"))
            , stream = \_ _ -> pure (Left (ProviderError "bad request"))
            , countTokens = \_ -> pure (Left (ProviderError "bad request"))
            }
        fallback =
          LlmClientDict
            { clientInfo = ClientInfo "fallback" "m"
            , complete = \_ -> writeIORef fallbackCalled True >> pure (Right (CompletionResponse [] EndTurn (TokenCount 0 0)))
            , stream = \_ _ -> writeIORef fallbackCalled True >> pure (Right (CompletionResponse [] EndTurn (TokenCount 0 0)))
            , countTokens = \_ -> writeIORef fallbackCalled True >> pure (Right (TokenCount 0 0))
            }
        wrapped = withFallback primary fallback
    res <- wrapped.complete minimalReq
    res `shouldBe` Left (ProviderError "bad request")
    readIORef fallbackCalled `shouldReturn` False

  it "withFallback applies fallback to stream and countTokens as well" $ do
    streamFallbackCalled <- newIORef False
    countFallbackCalled <- newIORef False
    let primary =
          LlmClientDict
            { clientInfo = ClientInfo "primary" "m"
            , complete = \_ -> pure (Right (CompletionResponse [] EndTurn (TokenCount 0 0)))
            , stream = \_ _ -> pure (Left (NetworkError "down"))
            , countTokens = \_ -> pure (Left (RateLimited 0.1))
            }
        fallback =
          LlmClientDict
            { clientInfo = ClientInfo "fallback" "m"
            , complete = \_ -> pure (Right (CompletionResponse [] EndTurn (TokenCount 0 0)))
            , stream = \_ _ -> do
                writeIORef streamFallbackCalled True
                pure (Right (CompletionResponse [] EndTurn (TokenCount 0 0)))
            , countTokens = \_ -> do
                writeIORef countFallbackCalled True
                pure (Right (TokenCount 1 2))
            }
        wrapped = withFallback primary fallback
    _ <- wrapped.stream minimalReq (\_ -> pure ())
    _ <- wrapped.countTokens []
    readIORef streamFallbackCalled `shouldReturn` True
    readIORef countFallbackCalled `shouldReturn` True

  it "withDefaults preserves request knobs when defaults are absent" $ do
    seenRef <- newIORef Nothing
    base <- mkRecordingClient seenRef
    let wrapped = withDefaults Nothing Nothing Nothing base
        req =
          CompletionRequest
            { messages = []
            , systemPrompt = Nothing
            , tools = []
            , maxTokens = 88
            , temperature = Just 0.7
            , topP = Just 0.95
            }
    _ <- wrapped.complete req
    seen <- readIORef seenRef
    case seen of
      Nothing -> expectationFailure "Expected complete to record request"
      Just r -> do
        r.maxTokens `shouldBe` 88
        r.temperature `shouldBe` Just 0.7
        r.topP `shouldBe` Just 0.95

  it "buildResolver maps roles to model-based clients" $ do
    let roleMap =
          Map.fromList
            [ (Architect, ModelSpec "m-arch" Nothing Nothing Nothing)
            , (Coder, ModelSpec "m-code" Nothing Nothing Nothing)
            ]
    resolver <- buildResolver roleMap (\m -> mkClient "factory" m)
    modelId (clientInfo (resolver Architect)) `shouldBe` "m-arch"
    modelId (clientInfo (resolver Coder)) `shouldBe` "m-code"

  it "buildResolver reuses base clients for duplicate model names" $ do
    seenModelsRef <- newIORef ([] :: [Text.Text])
    let roleMap =
          Map.fromList
            [ (Architect, ModelSpec "m-shared" Nothing Nothing Nothing)
            , (Coder, ModelSpec "m-shared" (Just 0.1) (Just 0.8) (Just 256))
            , (Validator, ModelSpec "m-validate" Nothing Nothing Nothing)
            ]
    _ <-
      buildResolver roleMap $ \m -> do
        modifyIORef' seenModelsRef (m :)
        mkClient "factory" m
    seen <- readIORef seenModelsRef
    let uniqSeen = Map.keysSet (Map.fromList [(m, ()) | m <- seen])
    uniqSeen `shouldBe` Map.keysSet (Map.fromList [("m-shared", ()), ("m-validate", ())])

minimalReq :: CompletionRequest
minimalReq =
  CompletionRequest
    { messages = []
    , systemPrompt = Nothing
    , tools = []
    , maxTokens = 32
    , temperature = Nothing
    , topP = Nothing
    }

mkClient :: Text.Text -> Text.Text -> IO LlmClientDict
mkClient provider model =
  pure
    LlmClientDict
      { clientInfo = ClientInfo provider model
      , complete = \_ -> pure (Right (CompletionResponse [] EndTurn (TokenCount 0 0)))
      , stream = \_ _ -> pure (Right (CompletionResponse [] EndTurn (TokenCount 0 0)))
      , countTokens = \_ -> pure (Right (TokenCount 0 0))
      }

mkRecordingClient :: IORef (Maybe CompletionRequest) -> IO LlmClientDict
mkRecordingClient ref =
  pure
    LlmClientDict
      { clientInfo = ClientInfo "rec" "model"
      , complete = \req -> do
          writeIORef ref (Just req)
          pure (Right (CompletionResponse [] EndTurn (TokenCount 0 0)))
      , stream = \_ _ -> pure (Right (CompletionResponse [] EndTurn (TokenCount 0 0)))
      , countTokens = \_ -> pure (Right (TokenCount 0 0))
      }

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
