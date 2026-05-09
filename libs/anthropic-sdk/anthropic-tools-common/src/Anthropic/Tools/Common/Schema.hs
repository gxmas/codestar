-- | Input schema definitions for common tools.
--
-- Each tool has:
--
-- * A Haskell input type with 'FromJSON'/'ToJSON' instances
-- * A companion 'Schema' value for use with 'CustomToolDef'
module Anthropic.Tools.Common.Schema
  ( -- * File System Schemas
    ReadFileInput (..)
  , readFileSchema
  , WriteFileInput (..)
  , writeFileSchema
  , ListDirectoryInput (..)
  , listDirectorySchema
  , SearchFilesInput (..)
  , searchFilesSchema

    -- * Shell Schemas
  , ExecuteCommandInput (..)
  , executeCommandSchema

    -- * Network Schemas
  , FetchUrlInput (..)
  , fetchUrlSchema
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..)
  )
import Data.Aeson.Types (Options(..), camelTo2)
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import Data.JsonSchema
  ( Schema, objectSchema, stringSchema, booleanSchema, integerSchema
  , required, optional
  , withDescription
  )
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

customOptions :: Options
customOptions = Aeson.defaultOptions
  { fieldLabelModifier = camelTo2 '_'
  , omitNothingFields  = True
  }

-- ---------------------------------------------------------------------------
-- File System
-- ---------------------------------------------------------------------------

-- | Input for reading a file.
data ReadFileInput = ReadFileInput
  { path :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ReadFileInput where
  toJSON = Aeson.genericToJSON customOptions
  toEncoding = Aeson.genericToEncoding customOptions

instance FromJSON ReadFileInput where
  parseJSON = Aeson.genericParseJSON customOptions

-- | JSON Schema for 'ReadFileInput'.
readFileSchema :: Schema
readFileSchema = objectSchema
  [ required "path" $ stringSchema
      & withDescription "Absolute or relative path to the file to read"
  ]

-- | Input for writing a file.
data WriteFileInput = WriteFileInput
  { path       :: !Text
  , content    :: !Text
  , createDirs :: !(Maybe Bool)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON WriteFileInput where
  toJSON = Aeson.genericToJSON customOptions
  toEncoding = Aeson.genericToEncoding customOptions

instance FromJSON WriteFileInput where
  parseJSON = Aeson.genericParseJSON customOptions

-- | JSON Schema for 'WriteFileInput'.
writeFileSchema :: Schema
writeFileSchema = objectSchema
  [ required "path" $ stringSchema
      & withDescription "Absolute or relative path to the file to write"
  , required "content" $ stringSchema
      & withDescription "Content to write to the file"
  , optional "create_dirs" $ booleanSchema
      & withDescription "Create parent directories if they don't exist"
  ]

-- | Input for listing a directory.
data ListDirectoryInput = ListDirectoryInput
  { path      :: !Text
  , recursive :: !(Maybe Bool)
  , pattern   :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ListDirectoryInput where
  toJSON = Aeson.genericToJSON customOptions
  toEncoding = Aeson.genericToEncoding customOptions

instance FromJSON ListDirectoryInput where
  parseJSON = Aeson.genericParseJSON customOptions

-- | JSON Schema for 'ListDirectoryInput'.
listDirectorySchema :: Schema
listDirectorySchema = objectSchema
  [ required "path" $ stringSchema
      & withDescription "Absolute or relative path to the directory"
  , optional "recursive" $ booleanSchema
      & withDescription "List contents recursively"
  , optional "pattern" $ stringSchema
      & withDescription "Glob pattern to filter entries"
  ]

-- | Input for searching files.
data SearchFilesInput = SearchFilesInput
  { path          :: !Text
  , pattern       :: !Text
  , caseSensitive :: !(Maybe Bool)
  , maxResults    :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SearchFilesInput where
  toJSON = Aeson.genericToJSON customOptions
  toEncoding = Aeson.genericToEncoding customOptions

instance FromJSON SearchFilesInput where
  parseJSON = Aeson.genericParseJSON customOptions

-- | JSON Schema for 'SearchFilesInput'.
searchFilesSchema :: Schema
searchFilesSchema = objectSchema
  [ required "path" $ stringSchema
      & withDescription "Root directory to search in"
  , required "pattern" $ stringSchema
      & withDescription "Search pattern (glob or regex)"
  , optional "case_sensitive" $ booleanSchema
      & withDescription "Whether the search is case-sensitive"
  , optional "max_results" $ integerSchema
      & withDescription "Maximum number of results to return"
  ]

-- ---------------------------------------------------------------------------
-- Shell
-- ---------------------------------------------------------------------------

-- | Input for executing a shell command.
data ExecuteCommandInput = ExecuteCommandInput
  { command    :: !Text
  , workingDir :: !(Maybe Text)
  , env        :: !(Maybe (Map Text Text))
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ExecuteCommandInput where
  toJSON = Aeson.genericToJSON customOptions
  toEncoding = Aeson.genericToEncoding customOptions

instance FromJSON ExecuteCommandInput where
  parseJSON = Aeson.genericParseJSON customOptions

-- | JSON Schema for 'ExecuteCommandInput'.
executeCommandSchema :: Schema
executeCommandSchema = objectSchema
  [ required "command" $ stringSchema
      & withDescription "Shell command to execute"
  , optional "working_dir" $ stringSchema
      & withDescription "Working directory for the command"
  , optional "env" $ objectSchema []
      & withDescription "Environment variables to set"
  ]

-- ---------------------------------------------------------------------------
-- Network
-- ---------------------------------------------------------------------------

-- | Input for fetching a URL.
data FetchUrlInput = FetchUrlInput
  { url     :: !Text
  , method  :: !(Maybe Text)
  , headers :: !(Maybe (Map Text Text))
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FetchUrlInput where
  toJSON = Aeson.genericToJSON customOptions
  toEncoding = Aeson.genericToEncoding customOptions

instance FromJSON FetchUrlInput where
  parseJSON = Aeson.genericParseJSON customOptions

-- | JSON Schema for 'FetchUrlInput'.
fetchUrlSchema :: Schema
fetchUrlSchema = objectSchema
  [ required "url" $ stringSchema
      & withDescription "URL to fetch"
  , optional "method" $ stringSchema
      & withDescription "HTTP method (GET, POST, etc.)"
  , optional "headers" $ objectSchema []
      & withDescription "HTTP headers to include"
  ]
