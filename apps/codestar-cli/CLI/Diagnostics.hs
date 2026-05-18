module CLI.Diagnostics
  ( printGrammarDiagnostics
  , printStaleFingerprintSafetyRail
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO

import CodeStar.RepoMap.CacheGc (CacheGcReport (..), runCacheGc)
import CodeStar.Storage (StorageBackend)
import CodeStar.TreeSitter.Grammars (knownGrammars)

printGrammarDiagnostics :: FilePath -> Int -> IO ()
printGrammarDiagnostics grammarDir loadedCount = do
  Text.IO.putStrLn ("Grammars dir: " <> Text.pack grammarDir)
  Text.IO.putStrLn ("Grammars loaded: " <> tshow loadedCount <> " / " <> tshow (length knownGrammars))
  mapM_ (Text.IO.putStrLn . Text.pack) (grammarWarnings grammarDir loadedCount)

grammarWarnings :: FilePath -> Int -> [String]
grammarWarnings grammarDir loadedCount
  | loadedCount == 0 =
      [ "WARNING: no grammars loaded. Repo-map extraction will skip supported files."
      , "  Remediation: run `codestar-cli fetch-grammars`."
      , "  Verify this path contains grammar libraries: " <> grammarDir
      , "  If this path is unexpected, check your XDG data directory env configuration."
      ]
  | loadedCount < max 3 (length knownGrammars `div` 4) =
      [ "WARNING: grammar load count is unexpectedly low."
      , "  Remediation: run `codestar-cli fetch-grammars` for missing languages."
      , "  Verify this path points to the grammar directory you expect: " <> grammarDir
      ]
  | otherwise = []

printStaleFingerprintSafetyRail :: StorageBackend -> Maybe FilePath -> IO ()
printStaleFingerprintSafetyRail cacheBackend mWorkspace = do
  gcReport <- runCacheGc cacheBackend mWorkspace False
  let staleGlobal = Map.findWithDefault 0 "stale-global" gcReport.staleByReason
      staleBoth = Map.findWithDefault 0 "stale-both" gcReport.staleByReason
      staleFingerprint = staleGlobal + staleBoth
  if staleFingerprint > 0
    then
      Text.IO.putStrLn
        ( "Note: detected "
            <> tshow staleFingerprint
            <> " stale repo-map cache entries from a previous extractor/query fingerprint; these entries are ignored. "
            <> "Run `codestar-cli cache-gc --delete` to clean them."
        )
    else pure ()

tshow :: Show a => a -> Text
tshow = Text.pack . show
