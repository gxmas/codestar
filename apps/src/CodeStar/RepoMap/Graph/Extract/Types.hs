{- |
= Graph.Extract.Types — shared types for the extraction pipeline

This module defines the core vocabulary that flows through every layer of
the repo-map extraction pipeline:

  * 'Tag' — a single symbol occurrence (definition or reference) with its
    file, name, line, and kind.
  * 'TagKind' — whether the tag is a definition or a reference.
  * 'TagExtraction' — the outcome of trying to extract tags from one file.
  * 'ExtractionSkip' — why a file was intentionally skipped.
  * 'namedChildren' — Tree-sitter utility shared by all language extractors.

Keeping these types in one place avoids import cycles: the language
modules (@Haskell@, @Python@, @TypeScript@) import from here but not from
each other, and the dispatcher (@Extract@) imports from all of them.
-}
module CodeStar.RepoMap.Graph.Extract.Types
  ( TagKind (..)
  , Tag (..)
  , ExtractionSkip (..)
  , TagExtraction (..)
  , wordAt
  , isIdentChar
  , namedChildren
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Data.Word (Word32)
import GHC.Generics (Generic)
import TreeSitter (Node)
import TreeSitter qualified as TS

-- | Why a file was skipped without attempting tag extraction.
data ExtractionSkip
  = UnsupportedExtension
  -- ^ No extractor is registered for this file extension.
  | NoGrammarInstalled
  -- ^ An extractor exists for the language, but the Tree-sitter grammar
  --   library (@.so@ / @.dylib@) has not been loaded.
  deriving stock (Eq, Show)

-- | Whether a tag marks the point where a symbol is __defined__ or a point
-- where it is __used__ (referenced).  Only definitions appear in the final
-- repo map; references are used to build the symbol graph for PageRank.
data TagKind = Definition | Reference
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A symbol occurrence extracted from a source file.
data Tag = Tag
  { tagFile :: !FilePath -- ^ Absolute or workspace-relative path.
  , tagName :: !Text     -- ^ The identifier text (e.g. @"parseCliArgs"@).
  , tagLine :: !Int      -- ^ 0-based source line where the symbol appears.
  , tagKind :: !TagKind
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Result of attempting to extract tags from a source file.
data TagExtraction
  = Extracted [Tag]
  -- ^ Tag extraction succeeded; the list may be empty for files with no
  --   recognisable definitions.
  | Skipped ExtractionSkip
  -- ^ The file was intentionally skipped — no error, no tags.
  | ExtractFailed Text
  -- ^ Extraction was attempted but failed. The 'Text' payload carries a
  --   human-readable reason suitable for logging.
  deriving stock (Eq, Show)

-- | Extract the identifier word starting at column @col@ on line @row@
-- of the source line vector.  Reads characters until a non-identifier
-- character is encountered (see 'isIdentChar').
wordAt :: V.Vector Text -> Int -> Int -> Text
wordAt ls row col
  | row >= V.length ls = Text.empty
  | otherwise = Text.takeWhile isIdentChar (Text.drop col (ls V.! row))

-- | Characters that may appear inside an identifier.
-- Includes @\'@ to capture Haskell primed names like @x\'@.
isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- | Collect up to @count@ named child nodes of @node@, skipping null and
-- anonymous nodes.
--
-- The @count == 0@ guard is essential: @Word32@ arithmetic means
-- @[0 .. 0 - 1]@ wraps to @[0 .. maxBound]@, which would appear to hang.
namedChildren :: Node -> Word32 -> IO [Node]
namedChildren node count =
  if count == 0
    then pure []
    else filterNamedM 0
 where
  filterNamedM i
    | i >= count = pure []
    | otherwise = do
        child <- TS.nodeChild node i
        isNull <- TS.nodeIsNull child
        if isNull
          then filterNamedM (i + 1)
          else do
            isNamed <- TS.nodeIsNamed child
            if isNamed
              then (child :) <$> filterNamedM (i + 1)
              else filterNamedM (i + 1)
