-- | Models API operations.
module Anthropic.Client.Models
  ( -- * Models
    listModels
  , getModel
  ) where

import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Anthropic.Types (ModelId(..), Page)
import Anthropic.Client.Config (AnthropicClient, ClientError)
import Anthropic.Client.Internal.Execute (executeJsonGet)
import Anthropic.Protocol.Model (ModelInfo)

-- | List available models with pagination.
--
-- @
-- result <- listModels client Nothing Nothing Nothing
-- case result of
--   Right page -> mapM_ (print . (.modelId)) page.pageData
--   Left err   -> print err
-- @
listModels
  :: AnthropicClient
  -> Maybe Text     -- ^ @after_id@ cursor
  -> Maybe Text     -- ^ @before_id@ cursor
  -> Maybe Int      -- ^ @limit@ (1-1000, default 20)
  -> IO (Either ClientError (Page ModelInfo))
listModels client afterId beforeId limit =
  executeJsonGet client (buildPath "/v1/models" params)
  where
    params = concat
      [ maybe [] (\x -> [("after_id",  TE.encodeUtf8 x)])  afterId
      , maybe [] (\x -> [("before_id", TE.encodeUtf8 x)])  beforeId
      , maybe [] (\x -> [("limit",     BS8.pack (show x))]) limit
      ]

-- | Retrieve a single model by ID.
getModel
  :: AnthropicClient
  -> ModelId
  -> IO (Either ClientError ModelInfo)
getModel client (ModelId mid) =
  executeJsonGet client ("/v1/models/" <> TE.encodeUtf8 mid)

-- | Build a path with query parameters.
buildPath :: BS8.ByteString -> [(BS8.ByteString, BS8.ByteString)] -> BS8.ByteString
buildPath path [] = path
buildPath path ps = path <> "?" <> BS8.intercalate "&" (map (\(k, v) -> k <> "=" <> v) ps)
