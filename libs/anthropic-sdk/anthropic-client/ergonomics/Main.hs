{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Ergonomics validation gate (Phase 1).
--
-- These six call site examples from ADR-001 must compile against the real
-- types with @undefined@ implementations. Any friction at the call site
-- triggers a type redesign before Phase 2.
--
-- Consumer modules need these extensions (all standard on GHC 9.6+):
--
--   * @OverloadedStrings@ -- for string literal ergonomics
--   * @OverloadedRecordDot@ -- @msg.content@ syntax
--   * @NoFieldSelectors@ -- avoids field name clashes with functions
--
-- @DuplicateRecordFields@ is NOT needed: the @with*@ builder setters
-- (ADR-010) eliminate ambiguous record updates.
module Main where

import Data.Aeson (toJSON)
import Data.Function ((&))
import Data.JsonSchema (objectSchema, required, stringSchema, enumSchema)
import qualified Data.Text.IO as T

import Anthropic.Client

-- | Example 1: Send a simple message.
--
-- The north-star call site. Three lines of business logic.
example1_simpleMessage :: AnthropicClient -> IO ()
example1_simpleMessage client = do
  result <- createMessage client $
    messageRequest "claude-sonnet-4-20250514" [userMessage "Hello!"] 1024
  case result of
    Right msg -> mapM_ print msg.content
    Left err  -> print err

-- | Example 2: Streaming with text output.
--
-- Push-based streaming with EventHandler record update.
-- No type class dictionaries, no effect plumbing.
example2_streaming :: AnthropicClient -> IO ()
example2_streaming client = do
  let req = messageRequest "claude-sonnet-4-20250514" [userMessage "Tell me a story"] 2048
  result <- streamMessagesWith client req defaultEventHandler
    { onContentBlockDelta = \_ delta -> case delta of
        TextDelta t -> T.putStr t
        _           -> pure ()
    }
  case result of
    Right msg -> putStrLn ("\nDone: " ++ show (length msg.content) ++ " blocks")
    Left err  -> print err

-- | Example 3: Tool definition with Schema.
--
-- Custom tool with JSON Schema defined via json-schema-combinators.
-- Server tool added via ServerTool constructor.
-- Uses @&@ / @with*@ setters instead of record update syntax (ADR-010).
example3_toolDefinition :: AnthropicClient -> IO ()
example3_toolDefinition client = do
  let weatherSchema = objectSchema
        [ required "city" stringSchema
        , required "unit" $ enumSchema
            [ toJSON ("celsius" :: String)
            , toJSON ("fahrenheit" :: String)
            ]
        ]

      weatherTool = CustomTool
                  $ customToolDef "get_weather" weatherSchema
                  & withDescription "Get current weather"

      webSearchTool = ServerTool
                    $ serverToolDef WebSearch "web_search"

      msgs = [userMessage "What's the weather in London?"]
      req = messageRequest "claude-sonnet-4-20250514" msgs 1024
          & withTools [weatherTool, webSearchTool]
          & withToolChoice toolAuto
  result <- createMessage client req
  case result of
    Right msg -> mapM_ print msg.content
    Left err  -> print err

-- | Example 4: Error handling.
--
-- Pattern matching on ClientError covers all failure modes.
example4_errorHandling :: AnthropicClient -> IO ()
example4_errorHandling client = do
  let req = messageRequest "claude-sonnet-4-20250514" [userMessage "Hello"] 1024
  result <- createMessage client req
  case result of
    Left (ApiErrorResponse apiErr rl) ->
      case apiErr.errorType of
        RateLimitError  -> putStrLn $ "Rate limited: " ++ show rl
        OverloadedError -> putStrLn "API overloaded, backing off"
        _               -> putStrLn $ "API error: " ++ show apiErr.errorMessage
    Left (NetworkError _e)             -> putStrLn "Network error, reconnecting"
    Left (DeserializationError msg _raw) -> putStrLn $ "Parse error: " ++ show msg
    Left TimeoutError                   -> putStrLn "Timeout, retrying"
    Right msg                           -> mapM_ print msg.content

-- | Example 5: Proactive throttling.
--
-- Pull-based rate limit observation (ADR-003).
example5_throttling :: AnthropicClient -> IO ()
example5_throttling client = do
  limits <- getRateLimits client
  case limits of
    Just rl | rl.requestsRemaining < 5 -> putStrLn "Low on requests, waiting..."
    _ -> pure ()
  let req = messageRequest "claude-sonnet-4-20250514" [userMessage "Hello"] 1024
  result <- createMessage client req
  case result of
    Right _msg -> putStrLn "Success"
    Left err   -> print err

-- | Example 6: Observability callback.
--
-- Push-based rate limit observation via ClientConfig callback (ADR-003).
-- Uses @&@ / @with*@ setters for ClientConfig (ADR-010).
example6_observability :: IO ()
example6_observability = do
  let config = defaultConfig "sk-my-key"
             & withOnResponseMeta (\meta ->
                 putStrLn $ "Request " ++ show meta.requestId
                         ++ " | Remaining: " ++ show meta.rateLimits.requestsRemaining
               )
  withClient config $ \client -> do
    let req = messageRequest "claude-sonnet-4-20250514" [userMessage "Hello"] 1024
    result <- createMessage client req
    case result of
      Right _msg -> putStrLn "Done"
      Left err   -> print err

main :: IO ()
main = putStrLn "Ergonomics validation: all six ADR-001 call sites compile."
