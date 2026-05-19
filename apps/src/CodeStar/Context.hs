{- |
= CodeStar.Context — system prompt and context window assembly

Every LLM request needs a context window: the system prompt (instructions,
tool docs, repo map, memory) plus the conversation history.  This module
assembles the __stable__ part — the content that does not change from turn
to turn — and computes how many tokens are left for the conversation history.

== What goes where

@
  System prompt = base instructions + repo map section + memory section
  Prepended messages = step-scoped files (from plan Uses: fields)
  Conversation history = caller's responsibility (AgentLoop)
@

The repo map and memory are injected into the system prompt rather than the
conversation so they benefit from Anthropic's prompt cache — these large
blocks are identical across turns and should be served from cache.

== Token budget

'computeBudget' subtracts reserved space for the repo map, memory, compaction
overhead, and the model's response from the total context window, then splits
the remaining tokens 75\/25 between history and step-scoped files.

Students: notice how 'assemble' takes a purely functional config and IO paths
for file loading, but returns a pure 'ContextParts'.  The IO is confined to
the file reads; the assembly logic is deterministic.
-}
module CodeStar.Context
  ( -- * Context assembly
    ContextConfig (..)
  , defaultContextConfig
  , ContextParts (..)
  , assemble

    -- * Token budget
  , TokenBudget (..)
  , computeBudget
  ) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.IO.Error (isDoesNotExistError)

import CodeStar.LLM.Base (Content (..), Message (..), Role (..))
import CodeStar.RepoMap.Render (estimateTokens)

-- --------------------------------------------------------------------
-- Config
-- --------------------------------------------------------------------

data ContextConfig = ContextConfig
  { maxContextTokens :: !Int
  -- ^ total context window
  , repoMapReserve :: !Int
  -- ^ tokens reserved for repo map
  , memoryReserve :: !Int
  -- ^ tokens reserved for memory files
  , compactionReserve :: !Int
  -- ^ tokens reserved for compaction overhead
  , responseReserve :: !Int
  -- ^ tokens reserved for the model's response
  }
  deriving stock (Eq, Show)

defaultContextConfig :: ContextConfig
defaultContextConfig =
  ContextConfig
    { maxContextTokens = 200_000
    , repoMapReserve = 4_096
    , memoryReserve = 2_048
    , compactionReserve = 1_024
    , responseReserve = 8_192
    }

-- --------------------------------------------------------------------
-- Token budget
-- --------------------------------------------------------------------

-- | How many tokens are available for each dynamic portion of the context.
data TokenBudget = TokenBudget
  { budgetForHistory :: !Int -- ^ Tokens available for conversation history.
  , budgetForFiles   :: !Int -- ^ Tokens available for step-scoped file content.
  }
  deriving stock (Eq, Show)

computeBudget :: ContextConfig -> Int -> TokenBudget
computeBudget cfg systemPromptTokens =
  let reserved =
        cfg.repoMapReserve
          + cfg.memoryReserve
          + cfg.compactionReserve
          + cfg.responseReserve
          + systemPromptTokens
      remaining = max 0 (cfg.maxContextTokens - reserved)
      histBudget = remaining * 3 `div` 4
      fileBudget = remaining - histBudget
   in TokenBudget
        { budgetForHistory = histBudget
        , budgetForFiles = fileBudget
        }

-- --------------------------------------------------------------------
-- Context parts
-- --------------------------------------------------------------------

-- | The assembled stable context, ready to be prepended to an LLM request.
data ContextParts = ContextParts
  { systemPrompt   :: !Text -- ^ Full system prompt: instructions + repo map + memory.
  , repoMapSection :: !Text -- ^ The repo map block (also embedded in systemPrompt).
  , memorySection  :: !Text -- ^ The memory block (also embedded in systemPrompt).
  , stepFiles      :: !Text -- ^ Step-scoped file content, sent as a prepended user message.
  }
  deriving stock (Show)

-- --------------------------------------------------------------------
-- Assembly
-- --------------------------------------------------------------------

{- | Assemble all stable context parts into a system-prompt block and
a list of messages to prepend to the conversation history.
The caller is responsible for appending history and the current message.
-}
assemble ::
  ContextConfig ->
  -- | base system prompt (tool docs, agent instructions)
  Text ->
  -- | rendered repo map (from RepoMap.Render)
  Text ->
  -- | memory file paths to inject
  [FilePath] ->
  -- | step-scoped files (from plan Uses: fields)
  [FilePath] ->
  IO ([Message], ContextParts)
assemble _cfg basePrompt repoMap memoryPaths scopedFiles = do
  memContents <- loadFiles memoryPaths
  scopeContents <- loadFiles scopedFiles

  let memSection = formatSection "Memory" memContents
      repoSection =
        if Text.null repoMap
          then Text.empty
          else "## Repository Map\n\n" <> repoMap <> "\n"
      scopeSection = formatSection "Context Files" scopeContents

      sysPrompt =
        Text.intercalate
          "\n\n"
          (filter (not . Text.null) [basePrompt, repoSection, memSection])

      parts =
        ContextParts
          { systemPrompt = sysPrompt
          , repoMapSection = repoSection
          , memorySection = memSection
          , stepFiles = scopeSection
          }

      msgs =
        if Text.null scopeSection
          then []
          else [Message User [TextContent scopeSection]]

  pure (msgs, parts)

{- | Load a list of files, pairing each with its content.
Files that don't exist are silently skipped.
-}
loadFiles :: [FilePath] -> IO [(FilePath, Text)]
loadFiles paths = do
  results <- mapM tryRead paths
  pure [(p, t) | Just (p, t) <- results]
 where
  tryRead p = do
    r <- try (Text.IO.readFile p) :: IO (Either IOException Text)
    case r of
      Left e -> if isDoesNotExistError e then pure Nothing else pure Nothing
      Right t -> pure (Just (p, t))

-- | Format a group of (path, content) pairs as a named section.
formatSection :: Text -> [(FilePath, Text)] -> Text
formatSection _ [] = Text.empty
formatSection header files =
  let header' = "## " <> header <> "\n\n"
      entries = map formatFile files
   in header' <> Text.intercalate "\n\n" entries

formatFile :: (FilePath, Text) -> Text
formatFile (path, content) =
  "### " <> Text.pack path <> "\n\n```\n" <> content <> "\n```"

_unusedEstimate :: Text -> Int
_unusedEstimate = estimateTokens
