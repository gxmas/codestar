-- | Pre-built tool definitions for file system operations.
--
-- Provides 'CustomToolDef' values ready to include in a message request's
-- tool list. No execution logic — see "Anthropic.Tools.Common.Executor"
-- for optional execution adapters.
module Anthropic.Tools.Common.FileSystem
  ( -- * Tool Definitions Record
    FileSystemTools (..)
  , fileSystemTools
  ) where

import Data.Function ((&))

import Anthropic.Protocol.Tool
  ( CustomToolDef, customToolDef
  , withDescription
  )
import Anthropic.Tools.Common.Schema
  ( readFileSchema, writeFileSchema
  , listDirectorySchema, searchFilesSchema
  )

-- | Pre-built tool definitions for file system operations.
data FileSystemTools = FileSystemTools
  { readFile      :: !CustomToolDef
  , writeFile     :: !CustomToolDef
  , listDirectory :: !CustomToolDef
  , searchFiles   :: !CustomToolDef
  }

-- | Default file system tool definitions.
fileSystemTools :: FileSystemTools
fileSystemTools = FileSystemTools
  { readFile = customToolDef "read_file" readFileSchema
      & withDescription "Read the contents of a file at the given path"
  , writeFile = customToolDef "write_file" writeFileSchema
      & withDescription "Write content to a file at the given path"
  , listDirectory = customToolDef "list_directory" listDirectorySchema
      & withDescription "List the contents of a directory"
  , searchFiles = customToolDef "search_files" searchFilesSchema
      & withDescription "Search for files matching a pattern in a directory tree"
  }
