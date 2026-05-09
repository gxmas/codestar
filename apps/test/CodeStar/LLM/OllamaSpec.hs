{-# LANGUAGE OverloadedStrings #-}

module CodeStar.LLM.OllamaSpec (spec) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Test.Hspec

import CodeStar.LLM.Base
  ( ClientInfo (..)
  , CompletionRequest (..)
  , Content (..)
  , LlmClientDict (..)
  , Message (..)
  , Role (..)
  , TokenCount (..)
  )
import CodeStar.LLM.Ollama (newOllamaClient, newOllamaClientAt)

spec :: Spec
spec = describe "CodeStar.LLM.Ollama" $ do
  it "newOllamaClient sets provider/model info" $ do
    client <- newOllamaClient "llama3"
    let ClientInfo{providerName = p, modelId = m} = clientInfo client
    p `shouldBe` "ollama"
    m `shouldBe` "llama3"

  it "newOllamaClientAt sets provider/model info for custom base URL" $ do
    client <- newOllamaClientAt "http://127.0.0.1:11434" "codellama"
    let ClientInfo{providerName = p, modelId = m} = clientInfo client
    p `shouldBe` "ollama"
    m `shouldBe` "codellama"

  it "countTokens is available without network dependency" $ do
    client <- newOllamaClient "llama3"
    tok <- countTokens client [Message User [TextContent "hello"]]
    tok `shouldBe` Right (TokenCount 0 0)

  it "complete returns an error for unreachable Ollama endpoint" $ do
    client <- newOllamaClientAt "http://127.0.0.1:1" "codellama"
    res <- complete client minimalReq
    res `shouldSatisfy` isLeft

  it "stream returns an error for unreachable endpoint without emitting tokens" $ do
    client <- newOllamaClientAt "http://127.0.0.1:1" "codellama"
    sawEvent <- newIORef False
    res <- stream client minimalReq (\_ -> writeIORef sawEvent True)
    res `shouldSatisfy` isLeft
    readIORef sawEvent `shouldReturn` False

minimalReq :: CompletionRequest
minimalReq =
  CompletionRequest
    { messages = [Message User [TextContent "ping"]]
    , systemPrompt = Nothing
    , tools = []
    , maxTokens = 16
    , temperature = Nothing
    , topP = Nothing
    }

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
