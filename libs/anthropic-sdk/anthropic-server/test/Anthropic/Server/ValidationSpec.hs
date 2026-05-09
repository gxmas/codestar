module Anthropic.Server.ValidationSpec (spec) where

import Test.Hspec
import Test.QuickCheck


import Anthropic.Types (ApiKey (..), ContentBlock (..))
import Anthropic.Types.Core (ModelId (..))
import Anthropic.Types.Content.Text (TextBlock (..))
import Anthropic.Protocol.Message (messageRequest, userMessage)
import Anthropic.Protocol.Tool (ToolDefinition (..))
import Anthropic.Protocol.Thinking (ThinkingConfig (..))
import Anthropic.Protocol.Batch (BatchItem (..))
import Anthropic.Protocol.TokenCount (TokenCountRequest (..))

import Anthropic.Server.Validation

spec :: Spec
spec = do
  describe "validateMessageRequest" $ do
    it "accepts valid message request" $ do
      let req = messageRequest "claude-sonnet-4" [userMessage "Hello"] 1024
      validateMessageRequest req `shouldBe` Right ()

    it "rejects max_tokens <= 0" $ do
      let req = messageRequest "claude-sonnet-4" [userMessage "Hello"] 0
      validateMessageRequest req `shouldSatisfy` isLeft

    it "rejects max_tokens < 0" $ do
      let req = messageRequest "claude-sonnet-4" [userMessage "Hello"] (-1)
      validateMessageRequest req `shouldSatisfy` isLeft

    it "rejects empty messages list" $ do
      let req = messageRequest "claude-sonnet-4" [] 1024
      validateMessageRequest req `shouldSatisfy` isLeft

    it "property: valid max_tokens always passes" $ property $
      \(Positive tokens) ->
        let req = messageRequest "claude-sonnet-4" [userMessage "Hi"] tokens
        in validateMessageRequest req == Right ()

  describe "validateThinkingConfig" $ do
    it "accepts thinking budget >= 1024 and < max_tokens" $ do
      let config = ThinkingEnabled 1024 Nothing
      validateThinkingConfig config 2048 `shouldBe` Right ()

    it "rejects thinking budget < 1024" $ do
      let config = ThinkingEnabled 1023 Nothing
      validateThinkingConfig config 2048 `shouldSatisfy` isLeft

    it "rejects thinking budget >= max_tokens" $ do
      let config = ThinkingEnabled 2048 Nothing
      validateThinkingConfig config 2048 `shouldSatisfy` isLeft

    it "rejects thinking budget > max_tokens" $ do
      let config = ThinkingEnabled 3000 Nothing
      validateThinkingConfig config 2048 `shouldSatisfy` isLeft

    it "accepts disabled thinking" $ do
      validateThinkingConfig ThinkingDisabled 1024 `shouldBe` Right ()

    it "accepts adaptive thinking" $ do
      validateThinkingConfig (ThinkingAdaptive Nothing) 1024 `shouldBe` Right ()

    it "property: budget in valid range always passes" $ property $
      forAll (choose (2048, 100000)) $ \maxTokens ->
        let budget = 1024 + ((maxTokens - 1024) `div` 2)
            config = ThinkingEnabled budget Nothing
        in validateThinkingConfig config maxTokens == Right ()

  describe "validateHeaders" $ do
    it "accepts valid headers" $ do
      let key = ApiKey "sk-test-key"
          version = "2023-06-01"
          contentType = Just "application/json"
      validateHeaders key version contentType `shouldBe` Right ()

    it "rejects empty API key" $ do
      let key = ApiKey ""
          version = "2023-06-01"
      validateHeaders key version Nothing `shouldSatisfy` isLeft

    it "rejects API key not starting with 'sk-'" $ do
      let key = ApiKey "invalid-key"
          version = "2023-06-01"
      validateHeaders key version Nothing `shouldSatisfy` isLeft

    it "rejects empty version" $ do
      let key = ApiKey "sk-test"
          version = ""
      validateHeaders key version Nothing `shouldSatisfy` isLeft

    it "rejects invalid content-type" $ do
      let key = ApiKey "sk-test"
          version = "2023-06-01"
          contentType = Just "text/plain"
      validateHeaders key version contentType `shouldSatisfy` isLeft

  describe "validateContentBlocks" $ do
    it "accepts non-empty content blocks" $ do
      let blocks = [TextContent (TextBlock "Hello" Nothing Nothing)]
      validateContentBlocks blocks `shouldBe` Right ()

    it "rejects empty content blocks" $ do
      validateContentBlocks [] `shouldSatisfy` isLeft

  describe "validateToolDefinitions" $ do
    it "accepts reasonable number of tools" $ do
      validateToolDefinitions [] `shouldBe` Right ()

    it "rejects more than 64 tools" $ do
      let manyTools = replicate 65 (CustomTool undefined)
      validateToolDefinitions manyTools `shouldSatisfy` isLeft

  describe "validateTokenCountRequest" $ do
    it "accepts valid token count request" $ do
      let req = TokenCountRequest
            { model = ModelId "claude-sonnet-4"
            , messages = [userMessage "Hello"]
            , system = Nothing
            , tools = Nothing
            , toolChoice = Nothing
            , thinking = Nothing
            }
      validateTokenCountRequest req `shouldBe` Right ()

    it "rejects empty messages" $ do
      let req = TokenCountRequest
            { model = ModelId "claude-sonnet-4"
            , messages = []
            , system = Nothing
            , tools = Nothing
            , toolChoice = Nothing
            , thinking = Nothing
            }
      validateTokenCountRequest req `shouldSatisfy` isLeft

  describe "validateBatchRequest" $ do
    it "accepts non-empty batch" $ do
      let items = [BatchItem "custom_1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 1024)]
      validateBatchRequest items `shouldBe` Right ()

    it "rejects empty batch" $ do
      validateBatchRequest [] `shouldSatisfy` isLeft

    it "rejects batch with more than 10000 items" $ property $
      forAll (choose (10001, 15000)) $ \n ->
        let items = replicate n (BatchItem "id" (messageRequest "claude-sonnet-4" [userMessage "x"] 100))
        in validateBatchRequest items `shouldSatisfy` isLeft

-- Helper to check if result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
