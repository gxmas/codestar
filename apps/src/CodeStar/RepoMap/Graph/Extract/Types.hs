module CodeStar.RepoMap.Graph.Extract.Types
  ( TagKind (..)
  , Tag (..)
  , TagExtraction (..)
  , wordAt
  , isIdentChar
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import GHC.Generics (Generic)

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
