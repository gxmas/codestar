-- | Pre-built tool definition for network operations.
--
-- Provides a 'CustomToolDef' value ready to include in a message request's
-- tool list. No execution logic — see "Anthropic.Tools.Common.Executor"
-- for optional execution adapters.
module Anthropic.Tools.Common.Network
  ( -- * Tool Definitions Record
    NetworkTools (..)
  , networkTools
  ) where

import Data.Function ((&))

import Anthropic.Protocol.Tool
  ( CustomToolDef, customToolDef
  , withDescription
  )
import Anthropic.Tools.Common.Schema (fetchUrlSchema)

-- | Pre-built tool definition for network operations.
data NetworkTools = NetworkTools
  { fetchUrl :: !CustomToolDef
  }

-- | Default network tool definitions.
networkTools :: NetworkTools
networkTools = NetworkTools
  { fetchUrl = customToolDef "fetch_url" fetchUrlSchema
      & withDescription "Fetch the contents of a URL"
  }
