module OpenAI.Client
  ( OpenAIClient
  , newClient
  , createChatCompletion
  , streamChatCompletion
  ) where

import Data.Aeson (eitherDecodeStrict')
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (Manager)

import OpenAI.Types
import OpenAI.Internal.Http (newManager, postJSON, postJSONStreaming)
import OpenAI.Internal.SSE (SSEEvent (..))


data OpenAIClient = OpenAIClient
  { config  :: ClientConfig
  , manager :: Manager
  }

newClient :: ClientConfig -> IO OpenAIClient
newClient cfg = do
  mgr <- newManager
  pure (OpenAIClient cfg mgr)

createChatCompletion
  :: OpenAIClient
  -> ChatRequest
  -> IO (Either ClientError ChatResponse)
createChatCompletion client req =
  postJSON client.manager client.config "/v1/chat/completions"
    req { stream = False }

streamChatCompletion
  :: OpenAIClient
  -> ChatRequest
  -> (StreamChunk -> IO ())
  -> IO (Either ClientError ())
streamChatCompletion client req onChunk = do
  postJSONStreaming client.manager client.config "/v1/chat/completions"
    req { stream = True }
    handleEvent
  where
    handleEvent (SSEData txt) =
      case eitherDecodeStrict' (TE.encodeUtf8 txt) of
        Right chunk -> onChunk chunk
        Left _      -> pure ()
    handleEvent SSEDone  = pure ()
    handleEvent SSEOther = pure ()
