{- | Placeholder interfaces for semantic (embedding-based) code search.
No implementation — build only after measuring where RepoMap + Architect
+ Localization fall short in practice.
-}
module CodeStar.SemanticSearch
  ( -- * Embedding client
    EmbeddingClient (..)

    -- * Index
  , SemanticIndex (..)

    -- * Chunks
  , CodeChunk (..)
  , ScoredChunk (..)
  ) where

import Data.Text (Text)

-- --------------------------------------------------------------------
-- Embedding client
-- --------------------------------------------------------------------

-- | Provider-agnostic embedding interface.
data EmbeddingClient = EmbeddingClient
  { embed :: Text -> IO (Either Text [Double])
  , embedBatch :: [Text] -> IO (Either Text [[Double]])
  , dimensions :: !Int
  }

-- --------------------------------------------------------------------
-- Index
-- --------------------------------------------------------------------

-- | Opaque handle to a persisted vector index.
data SemanticIndex = SemanticIndex
  { indexFile :: !FilePath
  , chunkCount :: !Int
  }

-- --------------------------------------------------------------------
-- Chunks
-- --------------------------------------------------------------------

data CodeChunk = CodeChunk
  { chunkFile :: !FilePath
  , chunkContent :: !Text
  , chunkStart :: !Int
  -- ^ start line (0-indexed)
  , chunkEnd :: !Int
  -- ^ end line (inclusive)
  }
  deriving stock (Eq, Show)

data ScoredChunk = ScoredChunk
  { chunk :: !CodeChunk
  , score :: !Double
  }
  deriving stock (Eq, Show)
