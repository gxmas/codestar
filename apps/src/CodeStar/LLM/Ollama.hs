module CodeStar.LLM.Ollama
  ( newOllamaClient
  , newOllamaClientAt
  ) where

import Data.Text (Text)

import CodeStar.LLM.Base
import CodeStar.LLM.OpenAI (newCompatibleClient)

newOllamaClient :: Text -> IO LlmClientDict
newOllamaClient modelId =
  newOllamaClientAt "http://localhost:11434" modelId

newOllamaClientAt :: Text -> Text -> IO LlmClientDict
newOllamaClientAt baseUrl modelId = do
  dict <- newCompatibleClient "ollama" modelId baseUrl
  pure dict{clientInfo = ClientInfo "ollama" modelId}
