module CodeStar.RepoMap.Graph.Extract.Types
  ( TagKind (..)
  , Tag (..)
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

data TagKind = Definition | Reference
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Tag = Tag
  { tagFile :: !FilePath
  , tagName :: !Text
  , tagLine :: !Int
  , tagKind :: !TagKind
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TagExtraction
  = Extracted [Tag]
  | SkippedUnsupported
  | SkippedNoGrammar
  | ExtractFailed
  deriving stock (Eq, Show)

wordAt :: V.Vector Text -> Int -> Int -> Text
wordAt ls row col
  | row >= V.length ls = Text.empty
  | otherwise = Text.takeWhile isIdentChar (Text.drop col (ls V.! row))

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
