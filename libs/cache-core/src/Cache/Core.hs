{-# LANGUAGE ScopedTypeVariables #-}

-- | In-memory LRU cache with per-namespace policies, TTL, and statistics.
module Cache.Core
  ( -- * Keys and values
    CacheNamespace (..)
  , CacheKey (..)
  , CacheEntry (..)

    -- * Policy
  , CachePolicy (..)
  , defaultCachePolicy
  , EvictionStrategy (..)

    -- * Statistics
  , CacheStats (..)

    -- * Invalidation
  , InvalidationRule (..)

    -- * Store
  , CacheStore
  , newCacheStore

    -- * Single-entry operations
  , cacheGet
  , cachePut

    -- * Bulk operations
  , cacheGetMany
  , cachePutMany

    -- * Invalidation
  , cacheInvalidate
  , cacheInvalidatePattern
  , cacheClear

    -- * Management
  , getCacheStats
  , setCachePolicy
  ) where

import Control.Concurrent.STM
  ( TVar, atomically, newTVarIO, readTVar, writeTVar
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.String (IsString)
import Data.List (minimumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
  ( UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime
  )
import GHC.Generics (Generic)

import Telemetry.Core (withSpan, AttributeValue (..))

-- ---------------------------------------------------------------------------
-- Keys and values
-- ---------------------------------------------------------------------------

newtype CacheNamespace = CacheNamespace Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString)

newtype CacheKey = CacheKey Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString)

-- ---------------------------------------------------------------------------
-- Cache entry
-- ---------------------------------------------------------------------------

data CacheEntry = CacheEntry
  { entryKey     :: !CacheKey
  , entryValue   :: !ByteString
  , entryCreated :: !UTCTime
  , entryAccessed :: !UTCTime
  , entryTtl     :: !(Maybe NominalDiffTime)
  , entrySize    :: !Int
  , entryHits    :: !Int
  }
  deriving stock (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Policy
-- ---------------------------------------------------------------------------

data EvictionStrategy
  = LRU
  | LFU
  | FIFO
  | TTL
  deriving stock (Eq, Show, Bounded, Enum, Generic)

data CachePolicy = CachePolicy
  { maxSize       :: !Int
  , maxEntries    :: !Int
  , defaultTtl    :: !NominalDiffTime
  , eviction      :: !EvictionStrategy
  }
  deriving stock (Eq, Show, Generic)

defaultCachePolicy :: CachePolicy
defaultCachePolicy = CachePolicy
  { maxSize    = 100 * 1024 * 1024
  , maxEntries = 10_000
  , defaultTtl = 3600
  , eviction   = LRU
  }

-- ---------------------------------------------------------------------------
-- Statistics
-- ---------------------------------------------------------------------------

data CacheStats = CacheStats
  { statHits       :: !Int
  , statMisses     :: !Int
  , statEvictions  :: !Int
  , statSize       :: !Int
  , statEntryCount :: !Int
  }
  deriving stock (Eq, Show, Generic)

emptyStats :: CacheStats
emptyStats = CacheStats 0 0 0 0 0

-- ---------------------------------------------------------------------------
-- Invalidation
-- ---------------------------------------------------------------------------

data InvalidationRule
  = Prefix !Text
  | Pattern !Text
  | All
  deriving stock (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Internal namespace state
-- ---------------------------------------------------------------------------

data NamespaceState = NamespaceState
  { nsEntries :: !(Map CacheKey CacheEntry)
  , nsPolicy  :: !CachePolicy
  , nsStats   :: !CacheStats
  }

emptyNamespace :: CachePolicy -> NamespaceState
emptyNamespace policy = NamespaceState Map.empty policy emptyStats

-- ---------------------------------------------------------------------------
-- Store
-- ---------------------------------------------------------------------------

newtype CacheStore = CacheStore
  { storeVar :: TVar (Map CacheNamespace NamespaceState)
  }

newCacheStore :: IO CacheStore
newCacheStore = CacheStore <$> newTVarIO Map.empty

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

getOrCreateNS :: Map CacheNamespace NamespaceState -> CacheNamespace -> NamespaceState
getOrCreateNS m ns = Map.findWithDefault (emptyNamespace defaultCachePolicy) ns m

isExpired :: UTCTime -> CacheEntry -> Bool
isExpired now entry = case entryTtl entry of
  Nothing  -> False
  Just ttl -> diffUTCTime now (entryCreated entry) >= ttl

pickVictim :: EvictionStrategy -> Map CacheKey CacheEntry -> Maybe CacheKey
pickVictim strategy entries
  | Map.null entries = Nothing
  | otherwise = Just $ case strategy of
      LRU  -> fst $ minimumByVal (comparing entryAccessed) entries
      LFU  -> fst $ minimumByVal (comparing entryHits)     entries
      FIFO -> fst $ minimumByVal (comparing entryCreated)  entries
      TTL  -> fst $ minimumByVal (comparing entryCreated)  entries

minimumByVal :: (CacheEntry -> CacheEntry -> Ordering)
             -> Map CacheKey CacheEntry
             -> (CacheKey, CacheEntry)
minimumByVal cmp m =
  minimumBy (\(_, a) (_, b) -> cmp a b) (Map.toList m)

totalSize :: Map CacheKey CacheEntry -> Int
totalSize = Map.foldl' (\acc e -> acc + entrySize e) 0

enforcePolicy :: CachePolicy -> NamespaceState -> UTCTime -> NamespaceState
enforcePolicy policy ns now =
  let (alive, expired) = Map.partition (not . isExpired now) (nsEntries ns)
      expiredCount = Map.size expired
      expiredBytes = sum [entrySize e | e <- Map.elems expired]
      stats1 = (nsStats ns)
        { statEvictions  = statEvictions (nsStats ns) + expiredCount
        , statSize       = statSize (nsStats ns) - expiredBytes
        , statEntryCount = statEntryCount (nsStats ns) - expiredCount
        }
      ns1 = ns { nsEntries = alive, nsStats = stats1 }
  in
  if eviction policy == TTL
    then ns1
    else evictToCapacity policy ns1

evictToCapacity :: CachePolicy -> NamespaceState -> NamespaceState
evictToCapacity policy ns
  | Map.size (nsEntries ns) <= maxEntries policy
  , totalSize (nsEntries ns) <= maxSize policy = ns
  | otherwise =
      case pickVictim (eviction policy) (nsEntries ns) of
        Nothing -> ns
        Just key ->
          let entry   = nsEntries ns Map.! key
              entries' = Map.delete key (nsEntries ns)
              stats' = (nsStats ns)
                { statEvictions  = statEvictions (nsStats ns) + 1
                , statSize       = statSize (nsStats ns) - entrySize entry
                , statEntryCount = statEntryCount (nsStats ns) - 1
                }
              ns' = ns { nsEntries = entries', nsStats = stats' }
          in evictToCapacity policy ns'

matchesGlob :: Text -> Text -> Bool
matchesGlob pattern key = go (T.unpack pattern) (T.unpack key)
  where
    go [] []         = True
    go ('*':ps) ks   = any (go ps) (tails ks)
    go (p:ps) (k:ks) = p == k && go ps ks
    go _  _          = False

    tails []       = [[]]
    tails s@(_:rest) = s : tails rest

-- ---------------------------------------------------------------------------
-- Single-entry operations
-- ---------------------------------------------------------------------------

cacheGet :: CacheStore -> CacheNamespace -> CacheKey -> IO (Maybe ByteString)
cacheGet store (CacheNamespace nsText) (CacheKey keyText) =
  withSpan "cache.get"
    [ ("cache.namespace", TextValue nsText)
    , ("cache.key", TextValue keyText)
    ] $ do
  let ns = CacheNamespace nsText
      key = CacheKey keyText
  now <- getCurrentTime
  result <- atomically $ do
    m <- readTVar (storeVar store)
    let nss = getOrCreateNS m ns
    case Map.lookup key (nsEntries nss) of
      Nothing -> do
        let nss' = nss { nsStats = (nsStats nss) { statMisses = statMisses (nsStats nss) + 1 } }
        writeTVar (storeVar store) (Map.insert ns nss' m)
        pure Nothing
      Just entry
        | isExpired now entry -> do
            let entries' = Map.delete key (nsEntries nss)
                stats' = (nsStats nss)
                  { statMisses     = statMisses (nsStats nss) + 1
                  , statEvictions  = statEvictions (nsStats nss) + 1
                  , statSize       = statSize (nsStats nss) - entrySize entry
                  , statEntryCount = statEntryCount (nsStats nss) - 1
                  }
                nss' = nss { nsEntries = entries', nsStats = stats' }
            writeTVar (storeVar store) (Map.insert ns nss' m)
            pure Nothing
        | otherwise -> do
            let entry' = entry { entryAccessed = now, entryHits = entryHits entry + 1 }
                nss' = nss
                  { nsEntries = Map.insert key entry' (nsEntries nss)
                  , nsStats   = (nsStats nss) { statHits = statHits (nsStats nss) + 1 }
                  }
            writeTVar (storeVar store) (Map.insert ns nss' m)
            pure (Just (entryValue entry))
  pure result

cachePut
  :: CacheStore
  -> CacheNamespace
  -> CacheKey
  -> ByteString
  -> Maybe NominalDiffTime
  -> IO ()
cachePut store (CacheNamespace nsText) (CacheKey keyText) value mTtl =
  withSpan "cache.set"
    [ ("cache.namespace", TextValue nsText)
    , ("cache.key", TextValue keyText)
    ] $ do
  let ns = CacheNamespace nsText
      key = CacheKey keyText
  now <- getCurrentTime
  atomically $ do
    m <- readTVar (storeVar store)
    let nss    = getOrCreateNS m ns
        policy = nsPolicy nss
        ttl    = case mTtl of
                   Just t  -> Just t
                   Nothing -> Just (defaultTtl policy)
        eSize  = BS.length value
        entry  = CacheEntry
          { entryKey      = key
          , entryValue    = value
          , entryCreated  = now
          , entryAccessed = now
          , entryTtl      = ttl
          , entrySize     = eSize
          , entryHits     = 0
          }
        (entries1, oldBytes) = case Map.lookup key (nsEntries nss) of
          Nothing -> (nsEntries nss, 0)
          Just e  -> (Map.delete key (nsEntries nss), entrySize e)
        entries2 = Map.insert key entry entries1
        stats1 = (nsStats nss)
          { statSize       = statSize (nsStats nss) - oldBytes + eSize
          , statEntryCount = statEntryCount (nsStats nss)
              + (if oldBytes == 0 then 1 else 0)
          }
        nss1 = nss { nsEntries = entries2, nsStats = stats1 }
        nss2 = enforcePolicy policy nss1 now
    writeTVar (storeVar store) (Map.insert ns nss2 m)

-- ---------------------------------------------------------------------------
-- Bulk operations
-- ---------------------------------------------------------------------------

cacheGetMany
  :: CacheStore
  -> CacheNamespace
  -> [CacheKey]
  -> IO (Map CacheKey ByteString)
cacheGetMany store ns keys = do
  results <- mapM (\k -> fmap (k,) <$> cacheGet store ns k) keys
  pure $ Map.fromList [pair | Just pair <- results]

cachePutMany
  :: CacheStore
  -> CacheNamespace
  -> [(CacheKey, ByteString)]
  -> IO ()
cachePutMany store ns entries =
  mapM_ (\(k, v) -> cachePut store ns k v Nothing) entries

-- ---------------------------------------------------------------------------
-- Invalidation
-- ---------------------------------------------------------------------------

cacheInvalidate :: CacheStore -> CacheNamespace -> CacheKey -> IO ()
cacheInvalidate store (CacheNamespace nsText) (CacheKey keyText) =
  withSpan "cache.evict"
    [ ("cache.namespace", TextValue nsText)
    , ("cache.key", TextValue keyText)
    ] $ do
  let ns = CacheNamespace nsText
      key = CacheKey keyText
  atomically $ do
    m <- readTVar (storeVar store)
    let nss = getOrCreateNS m ns
    case Map.lookup key (nsEntries nss) of
      Nothing    -> pure ()
      Just entry -> do
        let stats' = (nsStats nss)
              { statEvictions  = statEvictions (nsStats nss) + 1
              , statSize       = statSize (nsStats nss) - entrySize entry
              , statEntryCount = statEntryCount (nsStats nss) - 1
              }
            nss' = nss
              { nsEntries = Map.delete key (nsEntries nss)
              , nsStats   = stats'
              }
        writeTVar (storeVar store) (Map.insert ns nss' m)

cacheInvalidatePattern :: CacheStore -> CacheNamespace -> InvalidationRule -> IO Int
cacheInvalidatePattern store ns rule = atomically $ do
  m <- readTVar (storeVar store)
  let nss   = getOrCreateNS m ns
      keys  = Map.keys (nsEntries nss)
      toRemove = filter (matches rule) keys
  if null toRemove
    then pure 0
    else do
      let removed = mapMaybe (\k -> Map.lookup k (nsEntries nss)) toRemove
          removedSize  = sum [entrySize e | e <- removed]
          removedCount = length removed
          stats' = (nsStats nss)
            { statEvictions  = statEvictions (nsStats nss) + removedCount
            , statSize       = statSize (nsStats nss) - removedSize
            , statEntryCount = statEntryCount (nsStats nss) - removedCount
            }
          nss' = nss
            { nsEntries = foldr Map.delete (nsEntries nss) toRemove
            , nsStats   = stats'
            }
      writeTVar (storeVar store) (Map.insert ns nss' m)
      pure removedCount
  where
    matches All                    _ = True
    matches (Prefix p) (CacheKey k) = T.isPrefixOf p k
    matches (Pattern p) (CacheKey k) = matchesGlob p k

cacheClear :: CacheStore -> CacheNamespace -> IO ()
cacheClear store ns = atomically $ do
  m <- readTVar (storeVar store)
  let nss = getOrCreateNS m ns
      stats' = (nsStats nss)
        { statEvictions  = statEvictions (nsStats nss) + Map.size (nsEntries nss)
        , statSize       = 0
        , statEntryCount = 0
        }
      nss' = nss { nsEntries = Map.empty, nsStats = stats' }
  writeTVar (storeVar store) (Map.insert ns nss' m)

-- ---------------------------------------------------------------------------
-- Management
-- ---------------------------------------------------------------------------

getCacheStats :: CacheStore -> CacheNamespace -> IO CacheStats
getCacheStats store ns = do
  m <- atomically (readTVar (storeVar store))
  pure (nsStats (getOrCreateNS m ns))

setCachePolicy :: CacheStore -> CacheNamespace -> CachePolicy -> IO ()
setCachePolicy store ns policy = atomically $ do
  m <- readTVar (storeVar store)
  let nss  = getOrCreateNS m ns
      nss' = nss { nsPolicy = policy }
  writeTVar (storeVar store) (Map.insert ns nss' m)
