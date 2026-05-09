-- | Request validation functions for Anthropic API-conforming servers.
--
-- All validation functions return @Either (NonEmpty ValidationError) ()@.
-- 'NonEmpty' enforces the invariant that if validation fails, there is
-- at least one error.
module Anthropic.Server.Validation
  ( -- * Validation Error
    ValidationError (..)

    -- * Validation Functions
  , validateMessageRequest
  , validateTokenCountRequest
  , validateBatchRequest
  , validateHeaders
  , validateThinkingConfig
  , validateContentBlocks
  , validateToolDefinitions
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

import Anthropic.Types (ApiKey (..), ContentBlock (..))
import Anthropic.Protocol.Message (MessageRequest (..))
import Anthropic.Protocol.Tool (ToolDefinition (..))
import Anthropic.Protocol.Thinking (ThinkingConfig (..))
import Anthropic.Protocol.Batch (BatchItem (..))
import Anthropic.Protocol.TokenCount (TokenCountRequest (..))

-- | A single validation error with field path and description.
data ValidationError = ValidationError
  { validationField   :: !Text
  , validationMessage :: !Text
  }
  deriving stock (Eq, Show)

-- | Validate a message creation request.
validateMessageRequest :: MessageRequest -> Either (NonEmpty ValidationError) ()
validateMessageRequest req = do
  let errors = concat
        [ validateMaxTokens req.maxTokens
        , validateMessages req.messages
        , maybe [] (validateThinkingConfigWithMaxTokens req.maxTokens) req.thinking
        , maybe [] validateToolDefs req.tools
        ]
  case errors of
    []     -> Right ()
    (e:es) -> Left (e :| es)
  where
    validateMaxTokens mt
      | mt <= 0   = [ValidationError "max_tokens" "must be greater than 0"]
      | otherwise = []

    validateMessages msgs
      | null msgs = [ValidationError "messages" "must contain at least one message"]
      | otherwise = []

    validateToolDefs tools
      | length tools > 64 = [ValidationError "tools" "cannot exceed 64 tools"]
      | otherwise = []

-- | Validate a token counting request.
validateTokenCountRequest :: TokenCountRequest -> Either (NonEmpty ValidationError) ()
validateTokenCountRequest req = do
  let errors = concat
        [ validateMessages req.messages
        , maybe [] validateToolDefs req.tools
        ]
  case errors of
    []     -> Right ()
    (e:es) -> Left (e :| es)
  where
    validateMessages msgs
      | null msgs = [ValidationError "messages" "must contain at least one message"]
      | otherwise = []

    validateToolDefs tools
      | length tools > 64 = [ValidationError "tools" "cannot exceed 64 tools"]
      | otherwise = []

-- | Validate a batch of message requests.
validateBatchRequest :: [BatchItem] -> Either (NonEmpty ValidationError) ()
validateBatchRequest items = do
  let errors = concat
        [ validateBatchSize items
        , concatMap validateBatchItem items
        ]
  case errors of
    []     -> Right ()
    (e:es) -> Left (e :| es)
  where
    validateBatchSize bs
      | null bs = [ValidationError "requests" "batch must contain at least one request"]
      | length bs > 10000 = [ValidationError "requests" "batch cannot exceed 10000 requests"]
      | otherwise = []

    validateBatchItem _item = []  -- Individual item validation would go here

-- | Validate request headers (API key, version, content-type).
validateHeaders
  :: ApiKey           -- ^ API key from @x-api-key@ header
  -> ByteString       -- ^ API version from @anthropic-version@ header
  -> Maybe ByteString -- ^ Content type
  -> Either (NonEmpty ValidationError) ()
validateHeaders (ApiKey key) version contentType = do
  let errors = concat
        [ validateApiKey key
        , validateVersion version
        , maybe [] validateContentType contentType
        ]
  case errors of
    []     -> Right ()
    (e:es) -> Left (e :| es)
  where
    validateApiKey k
      | T.null k = [ValidationError "x-api-key" "API key is required"]
      | not (T.isPrefixOf "sk-" k) = [ValidationError "x-api-key" "API key must start with 'sk-'"]
      | otherwise = []

    validateVersion v
      | BS8.null v = [ValidationError "anthropic-version" "API version header is required"]
      | otherwise = []

    validateContentType ct
      | ct /= "application/json" = [ValidationError "content-type" "must be 'application/json'"]
      | otherwise = []

-- | Validate thinking configuration against max_tokens.
validateThinkingConfig :: ThinkingConfig -> Int -> Either (NonEmpty ValidationError) ()
validateThinkingConfig config maxTokens = do
  let errors = validateThinkingConfigWithMaxTokens maxTokens config
  case errors of
    []     -> Right ()
    (e:es) -> Left (e :| es)

-- | Helper to validate thinking config and return list of errors.
validateThinkingConfigWithMaxTokens :: Int -> ThinkingConfig -> [ValidationError]
validateThinkingConfigWithMaxTokens maxTokens config =
  case config of
    ThinkingEnabled budget _display ->
      concat
        [ if budget < 1024
          then [ValidationError "thinking.budget_tokens" "must be at least 1024"]
          else []
        , if budget >= maxTokens
          then [ValidationError "thinking.budget_tokens" "must be less than max_tokens"]
          else []
        ]
    _ -> []

-- | Validate a list of content blocks.
validateContentBlocks :: [ContentBlock] -> Either (NonEmpty ValidationError) ()
validateContentBlocks blocks = do
  let errors = validateBlockCount blocks
  case errors of
    []     -> Right ()
    (e:es) -> Left (e :| es)
  where
    validateBlockCount bs
      | null bs = [ValidationError "content" "must contain at least one content block"]
      | otherwise = []

-- | Validate a list of tool definitions.
validateToolDefinitions :: [ToolDefinition] -> Either (NonEmpty ValidationError) ()
validateToolDefinitions tools = do
  let errors = concat
        [ validateToolCount tools
        , validateToolNames tools
        ]
  case errors of
    []     -> Right ()
    (e:es) -> Left (e :| es)
  where
    validateToolCount ts
      | length ts > 64 = [ValidationError "tools" "cannot exceed 64 tools"]
      | otherwise = []

    validateToolNames _tools = []  -- Could check for duplicate names, etc.
