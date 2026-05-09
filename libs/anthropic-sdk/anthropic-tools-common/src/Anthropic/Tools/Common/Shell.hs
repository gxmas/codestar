-- | Pre-built tool definition for shell command execution.
--
-- Provides a 'CustomToolDef' value ready to include in a message request's
-- tool list. No execution logic — see "Anthropic.Tools.Common.Executor"
-- for optional execution adapters.
module Anthropic.Tools.Common.Shell
  ( -- * Tool Definitions Record
    ShellTools (..)
  , shellTools
  ) where

import Data.Function ((&))

import Anthropic.Protocol.Tool
  ( CustomToolDef, customToolDef
  , withDescription
  )
import Anthropic.Tools.Common.Schema (executeCommandSchema)

-- | Pre-built tool definition for shell operations.
data ShellTools = ShellTools
  { executeCommand :: !CustomToolDef
  }

-- | Default shell tool definitions.
shellTools :: ShellTools
shellTools = ShellTools
  { executeCommand = customToolDef "execute_command" executeCommandSchema
      & withDescription "Execute a shell command and return its output"
  }
