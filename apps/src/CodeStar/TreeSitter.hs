{- |
= CodeStar.TreeSitter — Tree-sitter grammar registry and syntax validation

This module is the __integration point__ between codestar and the
Tree-sitter parsing library.  It provides:

  * __'GrammarRegistry'__: an in-memory map from language name to a loaded
    'Language' object.  Built at startup by 'loadGrammarRegistry' by
    scanning the grammars directory for compiled @.dylib@\/@.so@ files.

  * __Language detection__: 'languageForFile' maps file paths to language
    names via the extension table in 'Grammars'.

  * __Syntax validation__: 'validate' and 'validateWithTimeout' parse a
    source file and report whether the parse tree contains any error nodes.
    Used by the verification subsystem to check edits before committing them.

== Grammar loading

Each grammar is a compiled shared library (@libtree-sitter-<lang>.dylib@)
that exports a single @tree_sitter_<lang>@ function returning a
@TSLanguage*@.  'loadGrammarRegistry' calls 'TreeSitter.loadLanguage' on
each known grammar file that exists on disk, silently skipping missing ones
so the agent works with a partial grammar set.
-}
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

-- | A map from language name (e.g. @\"haskell\"@) to a loaded Tree-sitter
-- 'Language' object.  Constructed at startup; read-only thereafter.
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

-- | Look up a 'Language' by name.  Returns 'Nothing' if the grammar was
-- not installed or failed to load.
lookupLanguage :: GrammarRegistry -> Text -> Maybe Language
lookupLanguage (GrammarRegistry m) lang = Map.lookup lang m

-- | Number of successfully loaded grammars.
-- Used by the diagnostics subsystem to warn when the count is unexpectedly low.
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

-- | Parse @src@ with the given 'Language' and return whether the parse
-- tree is error-free.  Does not time out — use 'validateWithTimeout' for
-- untrusted or very large inputs.
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
