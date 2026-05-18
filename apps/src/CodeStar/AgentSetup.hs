module CodeStar.AgentSetup
  ( buildRegistry
  , buildSystemPrompt
  , buildClientForEntry
  , llmErrorLabel
  , mkRecorder
  ) where

import Data.Text (Text)
import Data.Text qualified as Text

import CodeStar.Config (Config (..), ApiKey (..), TelemetrySection (..), TelemetryMode (..))
import CodeStar.Config.Types (ModelEntry (..))
import CodeStar.LLM.Anthropic (newAnthropicClient)
import CodeStar.LLM.Base (LlmClientDict, LlmError (..), withDefaults)
import CodeStar.LLM.OpenAI (newOpenAIClient)
import CodeStar.Telemetry
  ( OtelSettings (..)
  , TelemetryRecorder (..)
  , jsonRecorder
  , noOpRecorder
  , otlpRecorderWithHandle
  , shutdownTelemetry
  )
import CodeStar.Platform.Sandbox (Sandbox)
import CodeStar.Tools.Edit (editToolHandler)
import CodeStar.Tools.Glob (globToolHandler)
import CodeStar.Tools.Grep (grepToolHandler)
import CodeStar.Tools.Read (ReadTracker, readToolHandler)
import CodeStar.Tools.Registry
import CodeStar.Tools.Shell (shellToolHandler)
import CodeStar.Tools.TodoList (TodoStore, todoListHandlers)

buildRegistry :: ReadTracker -> TodoStore -> Sandbox -> Maybe (FilePath -> IO ()) -> ToolRegistry
buildRegistry tracker todoStore sandbox mOnEdit =
  register (readToolHandler tracker) $
    register (editToolHandler tracker Nothing mOnEdit) $
      register globToolHandler $
        register grepToolHandler $
          register (shellToolHandler sandbox) $
            foldr register emptyRegistry (todoListHandlers todoStore)

buildSystemPrompt :: ToolRegistry -> Text
buildSystemPrompt registry =
  Text.unlines
    [ "You are CodeStar, an expert AI coding agent."
    , "Work methodically: read files before editing, validate changes,"
    , "and declare done only when you have evidence the task is complete."
    , ""
    , "## Available Tools"
    , ""
    , generateDocs registry
    ]

buildClientForEntry :: Config -> ModelEntry -> IO LlmClientDict
buildClientForEntry config entry =
  let ApiKey key = if unApiKey entry.meApiKey /= ""
                   then entry.meApiKey
                   else config.apiKey
  in case entry.meProvider of
    "anthropic" -> do
      client <- newAnthropicClient key entry.meModel
      pure (withDefaults entry.meTemperature entry.meTopP entry.meMaxTokens client)
    _ -> do
      client <- newOpenAIClient key entry.meModel
      pure (withDefaults entry.meTemperature entry.meTopP entry.meMaxTokens client)

llmErrorLabel :: LlmError -> Text
llmErrorLabel (RateLimited _)         = "RateLimited"
llmErrorLabel (AuthenticationFailed _) = "AuthenticationFailed"
llmErrorLabel (ContextTooLong _ _)    = "ContextTooLong"
llmErrorLabel (ContentFiltered _)     = "ContentFiltered"
llmErrorLabel (InvalidRequest _)      = "InvalidRequest"
llmErrorLabel (ProviderError _)       = "ProviderError"
llmErrorLabel (NetworkError _)        = "NetworkError"

mkRecorder :: TelemetrySection -> IO (TelemetryRecorder, IO ())
mkRecorder tel = case tel.mode of
  TelemetryOff -> pure (noOpRecorder, pure ())
  TelemetryStderr -> pure (jsonRecorder, pure ())
  TelemetryOtlp -> do
    (recorder, handle) <-
      otlpRecorderWithHandle
        OtelSettings
          { serviceName = tel.serviceName
          , endpoint = tel.endpoint
          , logToStderr = tel.logToStderr
          , metricsEnabled = tel.metricsEnabled
          , metricsBindHost = tel.metricsBindHost
          , metricsPort = tel.metricsPort
          , sessionId = Nothing
          , userId = Nothing
          , tracesSampleRate = tel.sampleRate
          }
    pure (recorder, shutdownTelemetry handle)
