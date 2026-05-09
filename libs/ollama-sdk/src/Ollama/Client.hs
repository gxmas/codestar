module Ollama.Client
  ( OllamaClient
  , newClient
  , newClientAt

    -- * Re-exports from openai-sdk
  , OAI.createChatCompletion
  , OAI.streamChatCompletion
  ) where

import Data.Text (Text)

import qualified OpenAI.Client as OAI
import OpenAI.Types (ClientConfig (..))


type OllamaClient = OAI.OpenAIClient

defaultBaseUrl :: Text
defaultBaseUrl = "http://localhost:11434"

newClient :: IO OllamaClient
newClient = newClientAt defaultBaseUrl

newClientAt :: Text -> IO OllamaClient
newClientAt baseUrl = OAI.newClient ClientConfig
  { apiKey    = "ollama"
  , baseUrl   = baseUrl
  , timeoutMs = 300000
  }
