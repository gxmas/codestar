module CLI.FetchGrammars (runFetchGrammars) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

import CodeStar.TreeSitter.Grammars (GrammarSpec (..), fetchAllGrammars, fetchGrammar, grammarsDir, knownGrammars)

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
