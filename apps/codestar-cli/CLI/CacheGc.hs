{- |
= CLI.CacheGc — repo-map cache garbage collection

The repo-map cache persists Tree-sitter parse results on disk so that
restarting the agent does not re-parse every file from scratch.  Each
cached entry is keyed by a __fingerprint__ that captures the extractor
version and query hash.  When the extractor or queries change (e.g. after
an upgrade), old entries become stale and waste disk space.

This module implements the @cache-gc@ sub-command, which:

1. Scans the cache directory and classifies entries as stale or live.
2. Optionally deletes stale entries (@--delete@).
3. Reports results as plain text or JSON (@--json@).

Students: notice how the core GC logic lives in 'CodeStar.RepoMap.CacheGc'
(a pure library module), while this module only handles CLI concerns
(argument parsing, output formatting).  This separation makes the GC logic
testable without a real CLI.
-}
module CLI.CacheGc (runCacheGcCommand) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.FilePath ((</>))

import CodeStar.Config (CacheGcArgs (..))
import CodeStar.Config.Paths qualified as Paths
import CodeStar.RepoMap.CacheGc (CacheGcReport (..), StaleEntry (..), StaleReason (..), runCacheGc)
import CodeStar.Storage (newBackend)

-- | Entry point for the @cache-gc@ sub-command.
-- Resolves the cache directory from the workspace path (or @.@ by default),
-- runs the GC scan, and prints results in the requested format.
runCacheGcCommand :: CacheGcArgs -> IO ()
runCacheGcCommand args = do
  let workspace = maybe "." id args.cgWorkspace
      cacheRoot = Paths.projectDir workspace </> "cache"
  backend <- newBackend cacheRoot
  gcReport <- runCacheGc backend args.cgWorkspace args.cgDelete
  if args.cgJson
    then BL8.putStrLn (encode gcReport)
    else printCacheGcReport args.cgDelete gcReport

-- | Format and print a 'CacheGcReport' as human-readable text.
-- When @deleted@ is 'True', the verb changes from "Stale" to "Processed"
-- so the output accurately reflects what happened.
printCacheGcReport :: Bool -> CacheGcReport -> IO ()
printCacheGcReport deleted gcReport = do
  Text.IO.putStrLn ("Scanned entries: " <> tshow gcReport.scannedEntries)
  Text.IO.putStrLn ("Stale entries: " <> tshow gcReport.staleEntries)
  Text.IO.putStrLn ("Deleted entries: " <> tshow gcReport.deletedEntries)
  mapM_ printReason
    [StaleGlobal, StaleFile, StaleBoth]
  if null gcReport.entries
    then Text.IO.putStrLn "No stale entries found."
    else do
      Text.IO.putStrLn ""
      Text.IO.putStrLn
        (if deleted then "Processed stale entries:" else "Stale entries:")
      mapM_ printEntry gcReport.entries
 where
  printReason reason =
    Text.IO.putStrLn
      (reasonLabel reason <> ": " <> tshow (Map.findWithDefault 0 (reasonLabelInline reason) gcReport.staleByReason))

  reasonLabel StaleGlobal = "  stale-global"
  reasonLabel StaleFile = "  stale-file"
  reasonLabel StaleBoth = "  stale-both"

  printEntry entry =
    Text.IO.putStrLn
      ( "  - "
          <> maybe "<unknown-path>" Text.pack entry.stalePath
          <> " ["
          <> reasonLabelInline entry.staleReason
          <> "]"
      )

  reasonLabelInline StaleGlobal = "stale-global"
  reasonLabelInline StaleFile = "stale-file"
  reasonLabelInline StaleBoth = "stale-both"

tshow :: Show a => a -> Text
tshow = Text.pack . show
