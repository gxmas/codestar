module CodeStar.RepoMap.Cache
  ( -- * Cache handle
    RepoMapCache (..)
  , newRepoMapCache
  , currentTagsFingerprint
  , TagsKeyInfo (..)
  , parseTagsCacheKey

    -- * High-level helpers
  , getOrComputeTags
  , getOrComputeMap
  ) where

import Control.Exception (IOException, try)
import Control.Applicative ((<|>))
import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString.Lazy qualified as BL
import Data.Hashable (hash)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import System.Directory (getModificationTime)
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)

import CodeStar.RepoMap.Graph (Tag)
import CodeStar.Storage (StorageBackend (..))
import CodeStar.TreeSitter.Grammars (knownGrammars)

-- --------------------------------------------------------------------
-- Cache namespaces
-- --------------------------------------------------------------------

nsTags, nsMaps :: Text
nsTags = "repomap:tags"
nsMaps = "repomap:maps"

tagsKeyVersion :: Text
tagsKeyVersion = "v2"

currentTagsFingerprint :: Text
currentTagsFingerprint = Text.pack (show (abs (hash basis :: Int)))
 where
  basis :: String
  basis = show (2 :: Int, 1 :: Int, map show knownGrammars)

data TagsKeyInfo = TagsKeyInfo
  { tkiFingerprint :: !(Maybe Text)
  , tkiPath :: !FilePath
  , tkiMtime :: !UTCTime
  } deriving stock (Eq, Show)

-- --------------------------------------------------------------------
-- Cache handle
-- --------------------------------------------------------------------

data RepoMapCache = RepoMapCache
  { getTags :: FilePath -> UTCTime -> IO (Maybe [Tag])
  , putTags :: FilePath -> UTCTime -> [Tag] -> IO ()
  , getMap :: Text -> IO (Maybe Text)
  , putMap :: Text -> Text -> IO ()
  , invalidate :: FilePath -> IO ()
  }

newRepoMapCache :: StorageBackend -> RepoMapCache
newRepoMapCache store =
  RepoMapCache
    { getTags = doGetTags store
    , putTags = doPutTags store
    , getMap = doGetMap store
    , putMap = doPutMap store
    , invalidate = doInvalidate store
    }

-- --------------------------------------------------------------------
-- Tags cache: keyed by "v2:fingerprint:path:mtime" (escaped)
-- --------------------------------------------------------------------

tagsKey :: FilePath -> UTCTime -> Text
tagsKey path mtime =
  Text.intercalate
    ":"
    [ tagsKeyVersion
    , currentTagsFingerprint
    , escapeKeyComponent (Text.pack path)
    , escapeKeyComponent (Text.pack (show mtime))
    ]

parseTagsCacheKey :: Text -> Maybe TagsKeyInfo
parseTagsCacheKey key =
  parseVersioned key <|> parseLegacy key
 where
  parseVersioned t = case Text.splitOn ":" t of
    [ver, fp, rawPath, rawMtime]
      | ver == tagsKeyVersion -> do
          mt <- readMaybe (Text.unpack (unescapeKeyComponent rawMtime))
          pure $
            TagsKeyInfo
              { tkiFingerprint = Just fp
              , tkiPath = Text.unpack (unescapeKeyComponent rawPath)
              , tkiMtime = mt
              }
    _ -> Nothing

  parseLegacy t = do
    let (rawPath, suffix) = Text.breakOn ":" t
    if Text.null suffix
      then Nothing
      else do
        let rawMtime = Text.drop 1 suffix
        mt <- readMaybe (Text.unpack rawMtime)
        pure $
          TagsKeyInfo
            { tkiFingerprint = Nothing
            , tkiPath = Text.unpack rawPath
            , tkiMtime = mt
            }

doGetTags :: StorageBackend -> FilePath -> UTCTime -> IO (Maybe [Tag])
doGetTags store path mtime = do
  result <- store.get nsTags (tagsKey path mtime)
  pure $ case result of
    Left _ -> Nothing
    Right bs -> case eitherDecodeStrict' bs of
      Left _ -> Nothing
      Right tags -> Just tags

doPutTags :: StorageBackend -> FilePath -> UTCTime -> [Tag] -> IO ()
doPutTags store path mtime tags = do
  let bs = BL.toStrict (encode tags)
  _ <- store.put nsTags (tagsKey path mtime) bs
  pure ()

-- --------------------------------------------------------------------
-- Map cache: keyed by hash of (sorted files, maxTokens, mentioned)
-- --------------------------------------------------------------------

mapCacheKey :: [FilePath] -> Int -> [Text] -> Text
mapCacheKey files maxTok mentioned =
  let h = hash (sort files, maxTok, sort mentioned)
   in Text.pack (show (abs h))

doGetMap :: StorageBackend -> Text -> IO (Maybe Text)
doGetMap store key = do
  result <- store.get nsMaps key
  pure $ case result of
    Left _ -> Nothing
    Right bs -> Just (Text.pack (show bs)) -- stored as UTF-8 text bytes

doPutMap :: StorageBackend -> Text -> Text -> IO ()
doPutMap store key rendered = do
  let bs = BL.toStrict (encode rendered)
  _ <- store.put nsMaps key bs
  pure ()

-- --------------------------------------------------------------------
-- Invalidation: remove all tag-cache entries for a file
-- --------------------------------------------------------------------

doInvalidate :: StorageBackend -> FilePath -> IO ()
doInvalidate store path = do
  keys <- store.list nsTags
  let matching =
        [ key
        | key <- keys
        , maybe False (\info -> info.tkiPath == path) (parseTagsCacheKey key)
        ]
  mapM_ (\k -> store.delete nsTags k) matching

-- --------------------------------------------------------------------
-- High-level helpers
-- --------------------------------------------------------------------

{- | Return cached tags for a file, or compute them with the given action
and store the result. The cache is keyed by file modification time.
-}
getOrComputeTags ::
  RepoMapCache ->
  FilePath ->
  -- | computation (e.g. extractTags)
  IO [Tag] ->
  IO [Tag]
getOrComputeTags cache path compute = do
  mtime <- safeGetMtime path
  case mtime of
    Nothing -> compute
    Just mt -> do
      cached <- cache.getTags path mt
      case cached of
        Just tags -> pure tags
        Nothing -> do
          tags <- compute
          cache.putTags path mt tags
          pure tags

-- | Return a cached rendered repo map, or compute and store it.
getOrComputeMap ::
  RepoMapCache ->
  -- | all files in the map
  [FilePath] ->
  -- | maxTokens
  Int ->
  -- | mentioned identifiers
  [Text] ->
  -- | computation
  IO Text ->
  IO Text
getOrComputeMap cache files maxTok mentioned compute = do
  let key = mapCacheKey files maxTok mentioned
  cached <- cache.getMap key
  case cached of
    Just rendered -> pure rendered
    Nothing -> do
      rendered <- compute
      cache.putMap key rendered
      pure rendered

safeGetMtime :: FilePath -> IO (Maybe UTCTime)
safeGetMtime path = do
  result <- try (getModificationTime path) :: IO (Either IOException UTCTime)
  pure $ case result of
    Left e -> if isDoesNotExistError e then Nothing else Nothing
    Right t -> Just t

escapeKeyComponent :: Text -> Text
escapeKeyComponent =
  Text.replace "%" "%25"
    . Text.replace ":" "%3A"
    . Text.replace "/" "%2F"

unescapeKeyComponent :: Text -> Text
unescapeKeyComponent =
  Text.replace "%2F" "/"
    . Text.replace "%3A" ":"
    . Text.replace "%25" "%"
