{- |
= CLI.FetchGrammars — download Tree-sitter grammar libraries

The repo-map feature extracts a structural summary of the codebase using
__Tree-sitter__, a fast incremental parser.  Tree-sitter needs a
language-specific grammar library (a compiled @.so@ / @.dylib@ / @.dll@)
for each language it parses.

This module implements the @fetch-grammars@ sub-command, which downloads
pre-compiled grammar libraries to the user's XDG data directory.  Users
only need to run this once (or after upgrading to a version that adds new
grammars).

Without grammars, the repo map falls back to an empty string, and the
agent's awareness of the codebase structure is significantly reduced.
-}
module CLI.FetchGrammars (runFetchGrammars) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

import CodeStar.TreeSitter.Grammars (GrammarSpec (..), fetchAllGrammars, fetchGrammar, grammarsDir, knownGrammars)

-- | Download grammar libraries.
--
-- If @mLang@ is 'Nothing', all known grammars are fetched in parallel.
-- If @mLang@ is @Just lang@, only that language is fetched.  The function
-- exits with a non-zero code if any download fails, but still reports
-- how many succeeded — partial installs are valid.
runFetchGrammars :: Maybe Text -> IO ()
runFetchGrammars mLang = do
  dir <- grammarsDir
  Text.IO.putStrLn ("Fetching grammars to " <> Text.pack dir <> " ...")
  results <- case mLang of
    Nothing -> fetchAllGrammars dir report
    Just l -> case filter (\g -> g.language == l) knownGrammars of
      [] -> Text.IO.hPutStr stderr ("Unknown: " <> l <> "\n") >> exitFailure
      (g : _) -> report l >> fmap (\r -> [(g, r)]) (fetchGrammar dir g)
  let failures = [(g, e) | (g, Left e) <- results]
      successes = length [() | (_, Right _) <- results]
  Text.IO.putStrLn ("\nDone. " <> Text.pack (show successes) <> " grammars installed.")
  if null failures
    then exitSuccess
    else do
      Text.IO.hPutStr stderr "Failed:\n"
      mapM_ (\(g, e) -> Text.IO.hPutStr stderr ("  " <> g.language <> ": " <> e <> "\n")) failures
      exitFailure

report :: Text -> IO ()
report lang = Text.IO.putStrLn ("  [→] " <> lang)
