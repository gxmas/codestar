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

data TokenBudget = TokenBudget
  { budgetForHistory :: !Int
  , budgetForFiles :: !Int
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

data ContextParts = ContextParts
  { systemPrompt :: !Text
  , repoMapSection :: !Text
  , memorySection :: !Text
  , stepFiles :: !Text
  -- ^ step-scoped files (Uses: from plan)
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
