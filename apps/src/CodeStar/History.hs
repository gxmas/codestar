module CodeStar.History
  ( -- * Processor type
    HistoryProcessor
  , processHistory

    -- * Built-in processors
  , truncateObservations
  , elideOldObservations
  , cacheControlMarker
  , closedWindowTracker

    -- * Composed default chain
  , defaultChain
  ) where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text

import CodeStar.LLM.Base
  ( Content (..)
  , Message (..)
  , Role (..)
  , ToolCall (..)
  , ToolName (..)
  , ToolResult (..)
  )

-- --------------------------------------------------------------------
-- Processor type
-- --------------------------------------------------------------------

{- | A history processor transforms the message sequence before it is sent
to the LLM. Processors are pure and compose via processHistory.
-}
type HistoryProcessor = Seq Message -> Seq Message

-- | Apply a list of processors left-to-right.
processHistory :: [HistoryProcessor] -> Seq Message -> Seq Message
processHistory procs history = foldl (\h p -> p h) history procs

-- --------------------------------------------------------------------
-- TruncateObservations
-- --------------------------------------------------------------------

{- | Cap individual tool-result bodies at maxChars. Truncated results
get a notification so the model knows output was cut.
-}
truncateObservations :: Int -> HistoryProcessor
truncateObservations maxChars = fmap truncMsg
 where
  truncMsg msg = msg{content = map truncContent msg.content}

  truncContent (ToolResultContent tr) =
    if Text.length tr.resultBody <= maxChars
      then ToolResultContent tr
      else
        ToolResultContent
          tr
            { resultBody =
                Text.take maxChars tr.resultBody
                  <> "\n[...truncated at "
                  <> Text.pack (show maxChars)
                  <> " chars...]"
            }
  truncContent c = c

-- --------------------------------------------------------------------
-- ElideOldObservations
-- --------------------------------------------------------------------

{- | Replace tool-result bodies in messages older than keepLast with a
compact line count summary, freeing context for more recent content.
-}
elideOldObservations :: Int -> HistoryProcessor
elideOldObservations keepLast history =
  let n = Seq.length history
      cutoff = max 0 (n - keepLast)
   in Seq.mapWithIndex
        ( \i msg ->
            if i < cutoff then elideMsg msg else msg
        )
        history
 where
  elideMsg msg = msg{content = map elideContent msg.content}

  elideContent (ToolResultContent tr) =
    let lineCount = length (Text.lines tr.resultBody)
     in ToolResultContent
          tr
            { resultBody = "[Old output: " <> Text.pack (show lineCount) <> " lines omitted]"
            }
  elideContent c = c

-- --------------------------------------------------------------------
-- CacheControlMarker
-- --------------------------------------------------------------------

{- | Prepend a sentinel TextContent to the last N User messages so the
LLM adapter can set prompt-cache headers on those positions.
-}
cacheControlMarker :: Int -> HistoryProcessor
cacheControlMarker keepLast history =
  let n = Seq.length history
      cutoff = max 0 (n - keepLast)
   in Seq.mapWithIndex
        ( \i msg ->
            if i >= cutoff && msg.role == User
              then msg{content = cacheMarker : msg.content}
              else msg
        )
        history

-- | Sentinel recognised by the Anthropic adapter to set cache_control.
cacheMarker :: Content
cacheMarker = TextContent "\x0000cache_control"

-- --------------------------------------------------------------------
-- ClosedWindowTracker
-- --------------------------------------------------------------------

{- | Collapse stale Read-result messages for files that were subsequently
edited: replaces their body with a one-line "superseded" note.
-}
closedWindowTracker :: HistoryProcessor
closedWindowTracker history =
  let editedFiles = foldl collectEdited [] history
   in fmap (collapseStaleReads editedFiles) history
 where
  collectEdited acc msg =
    case firstToolTarget "edit" msg of
      Just name -> name : acc
      Nothing -> acc

  collapseStaleReads edited msg =
    case firstToolTarget "read" msg of
      Just name
        | name `elem` edited ->
            msg{content = map (summarise name) msg.content}
      _ -> msg

  summarise name (ToolResultContent tr) =
    ToolResultContent
      tr
        { resultBody = "[Closed window: " <> name <> " was edited; view superseded]"
        }
  summarise _ c = c

{- | Return the name of the first ToolUse content whose tool name starts
with the given prefix, or Nothing.
-}
firstToolUse :: Text -> Message -> Maybe Text
firstToolUse prefix msg = go msg.content
 where
  go [] = Nothing
  go (ToolUseContent tc : rest)
    | Text.isPrefixOf prefix (unToolName tc.toolName) =
        Just (unToolName tc.toolName)
    | otherwise = go rest
  go (_ : rest) = go rest

{- | Return a stable target key for matching read/edit operations. If tool names
include a colon-delimited target (e.g. "read:path"), use the suffix so
corresponding read/edit calls can be correlated.
-}
firstToolTarget :: Text -> Message -> Maybe Text
firstToolTarget prefix msg = fmap targetKey (firstToolUse prefix msg)
 where
  targetKey name =
    case Text.breakOn ":" name of
      (_tool, suff)
        | Text.null suff -> name
        | otherwise -> Text.drop 1 suff

-- --------------------------------------------------------------------
-- Default chain
-- --------------------------------------------------------------------

-- | Production processor chain applied before each LLM call.
defaultChain :: [HistoryProcessor]
defaultChain =
  [ truncateObservations 8_000
  , elideOldObservations 6
  , cacheControlMarker 3
  , closedWindowTracker
  ]
