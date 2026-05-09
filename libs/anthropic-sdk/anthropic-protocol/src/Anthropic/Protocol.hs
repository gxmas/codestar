-- | Re-export module for all Anthropic protocol types and operations.
--
-- A single @import Anthropic.Protocol@ gives consumers all protocol
-- definitions: message request/response, tools, thinking, streaming,
-- batch, model, and token counting types.
--
-- __Note on TokenCountRequest setters__: The @with*@ setters for
-- 'TokenCountRequest' (e.g., @withSystem@, @withTools@) are /not/
-- re-exported here because they collide with the 'MessageRequest'
-- setters of the same name. Import "Anthropic.Protocol.TokenCount"
-- qualified if you need them. See ADR-010.
--
-- __Note on StreamState__: 'StreamState' is exported opaquely from
-- this module (no constructors). Use 'streamPhase', 'accumulatedBlocks',
-- and 'isComplete' to inspect. The constructors are available from
-- "Anthropic.Protocol.Stream" for package-internal use.
module Anthropic.Protocol
  ( -- * Message Protocol
    module Anthropic.Protocol.Message

    -- * Tools
  , module Anthropic.Protocol.Tool

    -- * Thinking
  , module Anthropic.Protocol.Thinking

    -- * Batch
  , module Anthropic.Protocol.Batch

    -- * Models
  , module Anthropic.Protocol.Model

    -- * Token Counting
    -- | Re-exported types and smart constructor only.
    -- For @with*@ setters, import "Anthropic.Protocol.TokenCount" qualified.
  , TokenCountRequest (..)
  , TokenCountResponse (..)
  , tokenCountRequest

    -- * Streaming
    -- | Stream event types, delta types, and opaque stream state.
  , StreamEvent (..)
  , Delta (..)
  , StreamState
  , streamPhase
  , accumulatedBlocks
  , isComplete
  , StreamPhase (..)
  ) where

import Anthropic.Protocol.Message
import Anthropic.Protocol.Tool
import Anthropic.Protocol.Thinking
import Anthropic.Protocol.Batch
import Anthropic.Protocol.Model
import Anthropic.Protocol.TokenCount
  ( TokenCountRequest (..)
  , TokenCountResponse (..)
  , tokenCountRequest
  )
import Anthropic.Protocol.Stream
  ( StreamEvent (..)
  , Delta (..)
  , StreamState
  , streamPhase
  , accumulatedBlocks
  , isComplete
  , StreamPhase (..)
  )
