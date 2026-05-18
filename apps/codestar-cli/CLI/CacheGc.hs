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

runCacheGcCommand :: CacheGcArgs -> IO ()
runCacheGcCommand args = do
  let workspace = maybe "." id args.cgWorkspace
      cacheRoot = Paths.projectDir workspace </> "cache"
  backend <- newBackend cacheRoot
  gcReport <- runCacheGc backend args.cgWorkspace args.cgDelete
  if args.cgJson
    then BL8.putStrLn (encode gcReport)
    else printCacheGcReport args.cgDelete gcReport

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
