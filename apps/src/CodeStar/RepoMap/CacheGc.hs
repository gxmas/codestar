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

data StaleReason
  = StaleGlobal
  | StaleFile
  | StaleBoth
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToJSONKey)

data StaleEntry = StaleEntry
  { staleKey :: !Text
  , stalePath :: !(Maybe FilePath)
  , staleReason :: !StaleReason
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data CacheGcReport = CacheGcReport
  { scannedEntries :: !Int
  , staleEntries :: !Int
  , deletedEntries :: !Int
  , staleByReason :: !(Map StaleReason Int)
  , entries :: ![StaleEntry]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

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

countReasons :: [StaleEntry] -> Map StaleReason Int
countReasons = foldr (\entry -> Map.insertWith (+) entry.staleReason 1) Map.empty

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

mapMaybeM :: (a -> IO (Maybe b)) -> [a] -> IO [b]
mapMaybeM f xs = go xs []
 where
  go [] acc = pure (reverse acc)
  go (y : ys) acc = do
    mb <- f y
    case mb of
      Nothing -> go ys acc
      Just b -> go ys (b : acc)
