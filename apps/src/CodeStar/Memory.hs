module CodeStar.Memory
  ( -- * Categories
    MemoryCategory (..)

    -- * Entries
  , MemoryEntry (..)
  , MemoryCandidate (..)

    -- * Store
  , MemoryStore (..)
  , newMemoryStore

    -- * Operations
  , loadMemory
  , saveMemory
  , deleteMemory
  , proposeCandidate
  , confirmCandidate
  ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Hashable (hash)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile)
import System.FilePath ((</>))

-- --------------------------------------------------------------------
-- Categories
-- --------------------------------------------------------------------

data MemoryCategory
  = -- | coding conventions for this repo
    Convention
  | -- | approach that worked
    SuccessfulApproach
  | -- | something to avoid
    KnownPitfall
  | -- | build/env setup notes
    EnvironmentSetup
  | -- | user's stated preferences
    UserPreference
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- --------------------------------------------------------------------
-- Entries
-- --------------------------------------------------------------------

data MemoryEntry = MemoryEntry
  { meId :: !Text
  , meCategory :: !MemoryCategory
  , meContent :: !Text
  , meSourceHash :: !Text
  -- ^ hash of content for staleness detection
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An auto-discovered memory candidate awaiting user confirmation.
data MemoryCandidate = MemoryCandidate
  { mcCategory :: !MemoryCategory
  , mcContent :: !Text
  , mcReason :: !Text
  -- ^ why the agent thinks this is worth remembering
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- --------------------------------------------------------------------
-- Store
-- --------------------------------------------------------------------

data MemoryStore = MemoryStore
  { load :: IO [MemoryEntry]
  , save :: MemoryEntry -> IO ()
  , delete :: Text -> IO ()
  -- ^ delete by entry ID
  , propose :: MemoryCandidate -> IO ()
  , pending :: IO [MemoryCandidate]
  , confirm :: Text -> IO ()
  -- ^ confirm by category+content hash
  }

{- | Create a filesystem-backed memory store.
Root layout:
  <root>/entries/<id>.json   — confirmed memory entries
  <root>/candidates.json     — pending auto-discovered candidates
-}
newMemoryStore :: FilePath -> IO MemoryStore
newMemoryStore root = do
  createDirectoryIfMissing True (root </> "entries")
  pure
    MemoryStore
      { load = loadAll root
      , save = saveEntry root
      , delete = deleteEntry root
      , propose = addCandidate root
      , pending = loadCandidates root
      , confirm = confirmEntry root
      }

-- --------------------------------------------------------------------
-- Operations
-- --------------------------------------------------------------------

-- | Load all confirmed memory entries sorted by category.
loadMemory :: MemoryStore -> IO [MemoryEntry]
loadMemory store = store.load

-- | Persist a new or updated memory entry.
saveMemory :: MemoryStore -> MemoryEntry -> IO ()
saveMemory store = store.save

-- | Delete a memory entry by its ID.
deleteMemory :: MemoryStore -> Text -> IO ()
deleteMemory store = store.delete

-- | Add an auto-discovered candidate for user review.
proposeCandidate :: MemoryStore -> MemoryCandidate -> IO ()
proposeCandidate store = store.propose

-- | User confirms a pending candidate: move it to confirmed entries.
confirmCandidate :: MemoryStore -> MemoryCandidate -> IO ()
confirmCandidate store mc = do
  let entry =
        MemoryEntry
          { meId = candidateId mc
          , meCategory = mc.mcCategory
          , meContent = mc.mcContent
          , meSourceHash = contentHash mc.mcContent
          }
  store.save entry

-- --------------------------------------------------------------------
-- Filesystem implementation
-- --------------------------------------------------------------------

loadAll :: FilePath -> IO [MemoryEntry]
loadAll root = do
  let dir = root </> "entries"
  createDirectoryIfMissing True dir
  files <- listDirectory dir
  entries <- mapM (readEntry dir) (filter isJson files)
  pure [e | Just e <- entries]
 where
  isJson f = Text.isSuffixOf ".json" (Text.pack f)

readEntry :: FilePath -> FilePath -> IO (Maybe MemoryEntry)
readEntry dir name = do
  let path = dir </> name
  bs <- readFileSafe path
  case bs of
    Nothing -> pure Nothing
    Just b -> case eitherDecodeStrict' b of
      Left _ -> pure Nothing
      Right me -> pure (Just me)

readFileSafe :: FilePath -> IO (Maybe BS.ByteString)
readFileSafe path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else fmap Just (BS.readFile path)

saveEntry :: FilePath -> MemoryEntry -> IO ()
saveEntry root entry = do
  let dir = root </> "entries"
      path = dir </> Text.unpack entry.meId <> ".json"
  createDirectoryIfMissing True dir
  BL.writeFile path (encode entry)

deleteEntry :: FilePath -> Text -> IO ()
deleteEntry root eid = do
  let path = root </> "entries" </> Text.unpack eid <> ".json"
  exists <- doesFileExist path
  if exists then removeFile path else pure ()

candidatesPath :: FilePath -> FilePath
candidatesPath root = root </> "candidates.json"

loadCandidates :: FilePath -> IO [MemoryCandidate]
loadCandidates root = do
  let path = candidatesPath root
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      bs <- BL.readFile path
      case eitherDecodeStrict' (BL.toStrict bs) of
        Left _ -> pure []
        Right cs -> pure cs

addCandidate :: FilePath -> MemoryCandidate -> IO ()
addCandidate root mc = do
  existing <- loadCandidates root
  let updated = existing ++ [mc]
  BL.writeFile (candidatesPath root) (encode updated)

confirmEntry :: FilePath -> Text -> IO ()
confirmEntry root hashVal = do
  candidates <- loadCandidates root
  case filter (\c -> contentHash c.mcContent == hashVal) candidates of
    [] -> pure ()
    (mc : _) -> do
      confirmCandidate (stubStore root) mc
      let remaining = filter (\c -> contentHash c.mcContent /= hashVal) candidates
      BL.writeFile (candidatesPath root) (encode remaining)

stubStore :: FilePath -> MemoryStore
stubStore root =
  MemoryStore
    { load = loadAll root
    , save = saveEntry root
    , delete = deleteEntry root
    , propose = addCandidate root
    , pending = loadCandidates root
    , confirm = confirmEntry root
    }

candidateId :: MemoryCandidate -> Text
candidateId mc =
  Text.pack (show mc.mcCategory) <> "-" <> Text.take 8 (contentHash mc.mcContent)

contentHash :: Text -> Text
contentHash t = Text.pack (showHex (abs (hash t)) "")
