{- |
= CodeStar.AgentSetup — shared agent wiring utilities

This module contains the __construction helpers__ that are common to both
the CLI path ("CLI.Setup") and the server path ("Server.SessionSetup").
Rather than duplicating provider-selection and registry-building logic in
both places, both call sites import from here.

== What is wired here

  * __'buildRegistry'__: assembles the built-in 'ToolRegistry' (read, edit,
    glob, grep, shell, todo list) from its component handlers.  MCP handlers
    are added on top by the call site via 'register'.

  * __'buildSystemPrompt'__: constructs the base system prompt by combining
    the agent persona with the tool documentation generated from the registry.
    This ensures the prompt is always in sync with what tools are actually
    available.

  * __'buildClientForEntry'__: selects the right LLM adapter ('Anthropic' or
    'OpenAI') based on @provider@ in the config entry, then wraps the result
    with 'withDefaults' for per-model parameter overrides.

  * __'mkRecorder'__: picks the telemetry backend based on
    @telemetry.mode@ in the config ('noOpRecorder', 'jsonRecorder', or
    'otlpRecorder') and returns the recorder plus a shutdown action.

  * __'llmErrorLabel'__: converts 'LlmError' constructors to short stable
    strings for use as telemetry attributes (error type labels).
-}
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

-- | Build the standard built-in tool registry.
-- @mOnEdit@ is an optional callback invoked after every file edit so the
-- repo-map cache can be invalidated for the changed file.
buildRegistry :: ReadTracker -> TodoStore -> Sandbox -> Maybe (FilePath -> IO ()) -> ToolRegistry
buildRegistry tracker todoStore sandbox mOnEdit =
  register (readToolHandler tracker) $
    register (editToolHandler tracker Nothing mOnEdit) $
      register globToolHandler $
        register grepToolHandler $
          register (shellToolHandler sandbox) $
            foldr register emptyRegistry (todoListHandlers todoStore)

-- | Assemble the base system prompt: agent persona + auto-generated tool docs.
-- The tool section is generated from the registry so it always matches the
-- set of tools that are actually registered.
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

-- | Construct an LLM client for the given model entry.
-- Selects the Anthropic or OpenAI adapter based on @provider@, and falls
-- back to the top-level @api_key@ from @Config@ if the entry's key is empty.
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

-- | Short stable label for an 'LlmError', used as a telemetry attribute.
llmErrorLabel :: LlmError -> Text
llmErrorLabel (RateLimited _)         = "RateLimited"
llmErrorLabel (AuthenticationFailed _) = "AuthenticationFailed"
llmErrorLabel (ContextTooLong _ _)    = "ContextTooLong"
llmErrorLabel (ContentFiltered _)     = "ContentFiltered"
llmErrorLabel (InvalidRequest _)      = "InvalidRequest"
llmErrorLabel (ProviderError _)       = "ProviderError"
llmErrorLabel (NetworkError _)        = "NetworkError"

-- | Create a 'TelemetryRecorder' from the config section.
-- Returns the recorder and a shutdown action; the caller must invoke the
-- shutdown action at process exit to flush buffered spans and metrics.
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
