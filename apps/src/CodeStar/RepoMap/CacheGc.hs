{- |
= CodeStar.RepoMap.CacheGc — tag-cache garbage collection

Each extracted tag list is cached on disk keyed by the file path, its
modification time, and an __extractor fingerprint__.  The fingerprint
changes when the extractor version or grammar set changes, making old
entries __stale__.  This module scans the cache, classifies each entry,
and optionally deletes the stale ones.

A cache entry can be stale for two independent reasons:

  * __StaleGlobal__: the fingerprint does not match 'currentTagsFingerprint'.
    The entry was written by an older (or newer) extractor; the tags may no
    longer be accurate.
  * __StaleFile__: the file's modification time on disk no longer matches
    the mtime baked into the cache key.  The file has changed since the
    tags were extracted.
  * __StaleBoth__: both conditions apply.

The @cache-gc@ CLI sub-command in "CLI.CacheGc" is the public interface
to this module.
-}
module CodeStar.RepoMap.CacheGc
  ( StaleReason (..)
  , StaleEntry (..)
  , CacheGcReport (..)
  , runCacheGc
  ) where

import Data.Aeson (ToJSON, ToJSONKey)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.Directory (getModificationTime, makeAbsolute)
import Control.Exception (IOException, try)

import CodeStar.RepoMap.Cache (TagsKeyInfo (..), currentTagsFingerprint, parseTagsCacheKey)
import CodeStar.Storage (StorageBackend (..))

nsTags :: Text
nsTags = "repomap:tags"

-- | Why a cache entry is considered stale.
data StaleReason
  = StaleGlobal
  -- ^ Extractor fingerprint mismatch — entry was written by a different
  --   extractor version.  Safe to delete; the next extraction will
  --   regenerate it with the current fingerprint.
  | StaleFile
  -- ^ File modification time mismatch — the source file has changed
  --   since the tags were extracted.
  | StaleBoth
  -- ^ Both the fingerprint and the mtime are stale.
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToJSONKey)

-- | A single stale cache entry identified during a GC scan.
data StaleEntry = StaleEntry
  { staleKey    :: !Text
  -- ^ The raw storage key, used to delete the entry.
  , stalePath   :: !(Maybe FilePath)
  -- ^ The source file path decoded from the key, if parseable.
  , staleReason :: !StaleReason
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | Summary of a GC scan, returned by 'runCacheGc' and serialised as JSON
-- for the @--json@ flag of the CLI command.
data CacheGcReport = CacheGcReport
  { scannedEntries :: !Int
  -- ^ Total number of cache entries inspected.
  , staleEntries   :: !Int
  -- ^ Number of stale entries found.
  , deletedEntries :: !Int
  -- ^ Number of stale entries actually deleted (0 unless @doDelete@ was 'True').
  , staleByReason  :: !(Map StaleReason Int)
  -- ^ Count of stale entries broken down by 'StaleReason'.
  , entries        :: ![StaleEntry]
  -- ^ Full list of stale entries for human-readable display.
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | Scan the tag cache, classify entries as stale or live, and optionally
-- delete the stale ones.
--
-- @mWorkspace@: if 'Just', only entries whose path falls under that
-- directory are considered; entries from other workspaces are ignored.
-- @doDelete@: when 'True', stale entries are deleted from the store.
runCacheGc :: StorageBackend -> Maybe FilePath -> Bool -> IO CacheGcReport
runCacheGc store mWorkspace doDelete = do
  workspaceAbs <- traverse makeAbsolute mWorkspace
  keys <- store.list nsTags
  stale <- mapMaybeM (classify workspaceAbs) keys
  deleted <- if doDelete then deleteAll stale else pure 0
  pure $
    CacheGcReport
      { scannedEntries = length keys
      , staleEntries = length stale
      , deletedEntries = deleted
      , staleByReason = countReasons stale
      , entries = stale
      }
 where
  classify :: Maybe FilePath -> Text -> IO (Maybe StaleEntry)
  classify workspaceAbs key = do
    case parseTagsCacheKey key of
      Nothing ->
        if workspaceAbs == Nothing
          then pure (Just (StaleEntry key Nothing StaleGlobal))
          else pure Nothing
      Just info -> do
        matchesWorkspace <- maybe (pure True) (`pathInWorkspace` info.tkiPath) workspaceAbs
        if not matchesWorkspace
          then pure Nothing
          else do
            fileStale <- isFileStale info.tkiPath info.tkiMtime
            let globalStale = info.tkiFingerprint /= Just currentTagsFingerprint
            case (globalStale, fileStale) of
              (False, False) -> pure Nothing
              (True, False) -> pure (Just (StaleEntry key (Just info.tkiPath) StaleGlobal))
              (False, True) -> pure (Just (StaleEntry key (Just info.tkiPath) StaleFile))
              (True, True) -> pure (Just (StaleEntry key (Just info.tkiPath) StaleBoth))

  deleteAll :: [StaleEntry] -> IO Int
  deleteAll stale = do
    mapM_ (\entry -> store.delete nsTags entry.staleKey) stale
    pure (length stale)

-- | Tally stale entries by their 'StaleReason'.
countReasons :: [StaleEntry] -> Map StaleReason Int
countReasons = foldr (\entry -> Map.insertWith (+) entry.staleReason 1) Map.empty

-- | Return 'True' if the file's current mtime differs from @expectedMtime@,
-- or if the file no longer exists.
isFileStale :: FilePath -> UTCTime -> IO Bool
isFileStale path expectedMtime = do
  result <- try (getModificationTime path) :: IO (Either IOException UTCTime)
  pure $ case result of
    Left _ -> True
    Right actual -> actual /= expectedMtime

pathInWorkspace :: FilePath -> FilePath -> IO Bool
pathInWorkspace workspace path = do
  pathAbs <- makeAbsolute path
  let wsNorm = normalizePath workspace
      pathNorm = normalizePath pathAbs
      wsPrefix = if not (null wsNorm) && last wsNorm == '/' then wsNorm else wsNorm <> "/"
  pure (wsNorm == pathNorm || wsPrefix `isPrefixOf` pathNorm)

normalizePath :: FilePath -> FilePath
normalizePath = map normalizeSep
 where
  normalizeSep '\\' = '/'
  normalizeSep c = c

-- | Monadic 'Data.Maybe.mapMaybe': run an IO action for each element,
-- collect the 'Just' results, discard the 'Nothing' ones.
mapMaybeM :: (a -> IO (Maybe b)) -> [a] -> IO [b]
mapMaybeM f xs = go xs []
 where
  go [] acc = pure (reverse acc)
  go (y : ys) acc = do
    mb <- f y
    case mb of
      Nothing -> go ys acc
      Just b -> go ys (b : acc)
