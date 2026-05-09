-- | Safe JSON parsing for tool use block inputs.
--
-- Wraps aeson's 'fromJSON' with informative error context including
-- the tool name and raw input value.
module Anthropic.Tools.Common.Parser
  ( -- * Parse Error
    ParseError (..)

    -- * Parsing
  , parseToolInput
  ) where

import Data.Aeson (FromJSON, Value)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Anthropic.Types.Content.ToolUse (ToolUseBlock (..))

-- | Error from parsing a tool use block's input JSON.
data ParseError = ParseError
  { toolName :: !Text
    -- ^ Name of the tool that was being parsed.
  , rawInput :: !Value
    -- ^ The raw JSON input that failed to parse.
  , errorMsg :: !Text
    -- ^ Description of the parse failure.
  }
  deriving stock (Eq, Show, Generic)

-- | Parse a 'ToolUseBlock'\'s input field into a typed value.
--
-- Returns 'Left' with a 'ParseError' containing the tool name, raw input,
-- and error message on failure.
--
-- @
-- case parseToolInput toolUseBlock of
--   Right (input :: ReadFileInput) -> ...
--   Left err -> putStrLn $ "Failed to parse " <> err.toolName <> ": " <> err.errorMsg
-- @
parseToolInput :: FromJSON a => ToolUseBlock -> Either ParseError a
parseToolInput tub =
  case Aeson.fromJSON tub.input of
    Aeson.Success a -> Right a
    Aeson.Error msg -> Left ParseError
      { toolName = tub.name
      , rawInput = tub.input
      , errorMsg = T.pack msg
      }
