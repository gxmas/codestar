module CodeStar.Config.Convert
  ( toContextConfig
  , toCompactionConfig
  , toGuardrailConfig
  ) where

import CodeStar.Config.Types (ContextSection (..), CompactionSection (..), GuardrailsSection (..))
import CodeStar.Context (ContextConfig (..))
import CodeStar.Compaction (CompactionConfig (..))
import CodeStar.Guardrails (GuardrailConfig (..))

toContextConfig :: ContextSection -> ContextConfig
toContextConfig c = ContextConfig
  { maxContextTokens  = c.maxTokens
  , repoMapReserve    = c.repoMapReserve
  , memoryReserve     = c.memoryReserve
  , compactionReserve = c.compactionReserve
  , responseReserve   = c.responseReserve
  }

toCompactionConfig :: CompactionSection -> CompactionConfig
toCompactionConfig c = CompactionConfig
  { triggerFraction  = c.triggerFraction
  , maxContextTokens = c.maxContextTokens
  }

toGuardrailConfig :: GuardrailsSection -> GuardrailConfig
toGuardrailConfig g = GuardrailConfig
  { denyList       = g.denyList
  , allowList      = g.allowList
  , allowedPaths   = Nothing
  , secretPatterns = g.secretPatterns
  }
