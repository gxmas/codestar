{- |
= CodeStar.LLM.Ollama — Ollama local model adapter

Ollama exposes an OpenAI-compatible Chat Completions endpoint, so this
module is a thin wrapper around 'CodeStar.LLM.OpenAI.newCompatibleClient'
that sets the default base URL to @http://localhost:11434@ and labels
the provider as @"ollama"@ in 'ClientInfo'.

Students: this is a good example of the __adapter pattern__ — you get
support for a new provider by reusing an existing protocol implementation
with a different base URL and identity.
-}
module CodeStar.LLM.Ollama
  ( newOllamaClient
  , newOllamaClientAt
  ) where

import Data.Text (Text)

import CodeStar.LLM.Base
import CodeStar.LLM.OpenAI (newCompatibleClient)

-- | Create an Ollama client connecting to @http://localhost:11434@.
newOllamaClient :: Text -> IO LlmClientDict
newOllamaClient modelId =
  newOllamaClientAt "http://localhost:11434" modelId

-- | Create an Ollama client at a custom base URL.
-- Useful for remote Ollama instances or non-standard ports.
newOllamaClientAt :: Text -> Text -> IO LlmClientDict
newOllamaClientAt baseUrl modelId = do
  dict <- newCompatibleClient "ollama" modelId baseUrl
  pure dict{clientInfo = ClientInfo "ollama" modelId}
