module CodeStar.Compaction
  ( -- * State
    CompactionState (..)
  , emptyCompactionState

    -- * Trigger
  , CompactionConfig (..)
  , defaultCompactionConfig
  , shouldCompact

    -- * Execution
  , compact

    -- * Internal (Testing)
  , renderHistory
  , buildCompactedHistory
  ) where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text

import CodeStar.LLM.Base
  ( CompletionRequest (..)
  , CompletionResponse (..)
  , Content (..)
  , LlmClientDict (..)
  , Message (..)
  , Role (..)
  , ToolCall (..)
  , ToolName (..)
  , ToolResult (..)
  )
import CodeStar.RepoMap.Render (estimateTokens)

-- --------------------------------------------------------------------
-- State preserved across compaction
-- --------------------------------------------------------------------

{- | Durable items that must survive compaction and be injected back
into the context after the summary is inserted.
-}
data CompactionState = CompactionState
  { csRepoMap :: !Text
  -- ^ last rendered repo map
  , csTodoList :: ![Text]
  -- ^ current todo items (descriptions)
  , csMemory :: ![Text]
  -- ^ memory file paths to reload
  , csTask :: !Text
  -- ^ original task description
  }
  deriving stock (Eq, Show)

emptyCompactionState :: CompactionState
emptyCompactionState =
  CompactionState
    { csRepoMap = Text.empty
    , csTodoList = []
    , csMemory = []
    , csTask = Text.empty
    }

-- --------------------------------------------------------------------
-- Trigger
-- --------------------------------------------------------------------

data CompactionConfig = CompactionConfig
  { triggerFraction :: !Double
  -- ^ compact when context > this fraction full
  , maxContextTokens :: !Int
  }
  deriving stock (Eq, Show)

defaultCompactionConfig :: CompactionConfig
defaultCompactionConfig =
  CompactionConfig
    { triggerFraction = 0.85
    , maxContextTokens = 200_000
    }

-- | Return True when the history has grown past the trigger threshold.
shouldCompact :: CompactionConfig -> Seq Message -> Bool
shouldCompact cfg history =
  let tokens = estimateTokens (renderHistory history)
      limit = floor (fromIntegral cfg.maxContextTokens * cfg.triggerFraction)
   in tokens >= limit

-- --------------------------------------------------------------------
-- Execution
-- --------------------------------------------------------------------

{- | Replace the conversation history with a one-shot LLM summary.
The summary is produced by the Summarizer model role and becomes a
single System message. CompactionState items are appended as context.
-}
compact ::
  -- | Summarizer model client
  LlmClientDict ->
  CompactionState ->
  -- | current history
  Seq Message ->
  -- | optional user instruction for /compact
  Maybe Text ->
  IO (Either Text (Seq Message))
compact client cs history mInstruction = do
  let prompt = buildSummaryPrompt cs history mInstruction
      req =
        CompletionRequest
          { messages = [Message User [TextContent prompt]]
          , systemPrompt = Just summarySystemPrompt
          , tools = []
          , maxTokens = 4096
          , temperature = Just 0.0
          , topP = Nothing
          }
  result <- client.complete req
  case result of
    Left err -> pure (Left (Text.pack (show err)))
    Right resp ->
      let summary = extractText resp
          newHist = buildCompactedHistory cs summary
       in pure (Right newHist)

-- | Construct a summary prompt from the history and durable state.
buildSummaryPrompt :: CompactionState -> Seq Message -> Maybe Text -> Text
buildSummaryPrompt cs history mInstruction =
  Text.unlines $
    [ "Summarise the following agent conversation concisely."
    , "Preserve: what the agent was asked to do, what it did,"
    , "  which files were modified, what tools were called and their outcomes,"
    , "  any errors encountered, and the current state of the work."
    , ""
    , "Task: " <> cs.csTask
    , ""
    ]
      ++ ( case mInstruction of
             Nothing -> []
             Just ins -> ["Additional instruction: " <> ins, ""]
         )
      ++ [ "Conversation:"
         , renderHistory history
         ]

summarySystemPrompt :: Text
summarySystemPrompt =
  "You are a concise technical summariser. "
    <> "Produce a structured summary that lets the agent continue work without "
    <> "losing context. Focus on decisions made, files changed, and current state."

-- | Rebuild a minimal history from a compaction summary.
buildCompactedHistory :: CompactionState -> Text -> Seq Message
buildCompactedHistory cs summary =
  let header =
        Text.unlines
          [ "[Compacted context]"
          , ""
          , "## Summary of prior work"
          , summary
          , ""
          , if Text.null cs.csRepoMap
              then ""
              else "## Repository Map\n\n" <> cs.csRepoMap
          , if null cs.csTodoList
              then ""
              else
                "## Pending tasks\n\n"
                  <> Text.unlines (map ("- " <>) cs.csTodoList)
          ]
   in Seq.singleton (Message System [TextContent header])

-- | Render message history as plain text for summarisation input.
renderHistory :: Seq Message -> Text
renderHistory = Text.intercalate "\n\n" . map renderMsg . foldr (:) []
 where
  renderMsg msg =
    let roleTag = case msg.role of
          User -> "User"
          Assistant -> "Assistant"
          System -> "System"
     in roleTag <> ": " <> foldMap renderContent msg.content

  renderContent (TextContent t) = t
  renderContent (ToolUseContent tc) =
    "[tool:" <> tc.toolName.unToolName <> "]"
  renderContent (ToolResultContent tr) = tr.resultBody

extractText :: CompletionResponse -> Text
extractText resp = foldMap go resp.responseContent
 where
  go (TextContent t) = t
  go _ = Text.empty
