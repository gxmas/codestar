module CodeStar.TreeSitter
  ( -- * Types
    SyntaxResult (..)
  , SyntaxError (..)
  , ParseError (..)

    -- * Grammar Registry
  , GrammarRegistry (..)
  , loadGrammarRegistry
  , lookupLanguage
  , languageForFile
  , grammarCount
  , loadedLanguages

    -- * Validation
  , validate
  , validateWithTimeout
  ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, takeFileName)
import System.Timeout (timeout)

import TreeSitter (Language)
import TreeSitter qualified

import CodeStar.TreeSitter.Grammars
  ( GrammarSpec (..)
  , grammarByExtension
  , grammarLibPath
  , grammarsDir
  , knownGrammars
  )

-- --------------------------------------------------------------------
-- Types
-- --------------------------------------------------------------------

data SyntaxError = SyntaxError
  { line :: !Int
  , col :: !Int
  , message :: Text
  }
  deriving stock (Eq, Show)

data SyntaxResult = SyntaxResult
  { valid :: Bool
  , errors :: [SyntaxError]
  }
  deriving stock (Eq, Show)

data ParseError
  = UnsupportedLanguage Text
  | ParseTimeout
  | GrammarNotFound Text
  deriving stock (Eq, Show)

-- --------------------------------------------------------------------
-- Grammar Registry
-- --------------------------------------------------------------------

newtype GrammarRegistry = GrammarRegistry (Map Text Language)

{- | Load all available grammar dylibs from the grammars directory.
Silently skips grammars that aren't installed yet.
-}
loadGrammarRegistry :: IO GrammarRegistry
loadGrammarRegistry = do
  dir <- grammarsDir
  langs <- mapMaybe id <$> mapM (tryLoad dir) knownGrammars
  pure (GrammarRegistry (Map.fromList langs))

tryLoad :: FilePath -> GrammarSpec -> IO (Maybe (Text, Language))
tryLoad dir grammar = do
  let libPath = grammarLibPath dir grammar
      symbol = Text.unpack grammar.symbol
  exists <- doesFileExist libPath
  if not exists
    then pure Nothing
    else do
      mLang <- TreeSitter.loadLanguage libPath symbol
      pure $ case mLang of
        Nothing -> Nothing
        Just lang -> Just (grammar.language, lang)

lookupLanguage :: GrammarRegistry -> Text -> Maybe Language
lookupLanguage (GrammarRegistry m) lang = Map.lookup lang m

grammarCount :: GrammarRegistry -> Int
grammarCount (GrammarRegistry m) = Map.size m

loadedLanguages :: GrammarRegistry -> [Text]
loadedLanguages (GrammarRegistry m) = Map.keys m

-- | Derive language name from file path via extension mapping.
languageForFile :: FilePath -> Maybe Text
languageForFile path =
  let ext = Text.pack (takeExtension path)
      basename = Text.pack (takeFileName path)
      byExt = Map.lookup ext grammarByExtension
      byName = Map.lookup basename grammarByExtension
   in fmap (.language) (byExt <|> byName)

-- --------------------------------------------------------------------
-- Validation
-- --------------------------------------------------------------------

validate :: Language -> ByteString -> IO (Either ParseError SyntaxResult)
validate lang src = do
  parser <- TreeSitter.newParser
  ok <- TreeSitter.setLanguage parser lang
  if not ok
    then pure (Left (UnsupportedLanguage "failed to set language"))
    else do
      mTree <- TreeSitter.parseSource parser src
      case mTree of
        Nothing -> pure (Left ParseTimeout)
        Just treeFp -> do
          root <- TreeSitter.rootNode treeFp
          hasErr <- TreeSitter.nodeHasError root
          pure $
            Right
              SyntaxResult
                { valid = not hasErr
                , errors = []
                }

-- | Validate with a 500ms timeout.
validateWithTimeout :: Language -> ByteString -> IO (Either ParseError SyntaxResult)
validateWithTimeout lang src = do
  result <- timeout 500000 (validate lang src)
  pure $ case result of
    Nothing -> Left ParseTimeout
    Just r -> r
