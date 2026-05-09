module CodeStar.Storage
  ( -- * Backend
    StorageBackend (..)

    -- * Errors
  , StorageError (..)

    -- * Namespace helpers
  , repoUserNamespace
  , sessionNamespace

    -- * Construction
  , filesystemBackend
  , newBackend

    -- * Re-exports
  , SC.Storage
  , SC.createStorage
  , SC.createDefaultStorage
  ) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text

import Storage.Core (Storage, StorageError (..))
import Storage.Core qualified as SC

import CodeStar.Types (SessionId (..), UserId (..))

-- --------------------------------------------------------------------
-- Namespace helpers
-- --------------------------------------------------------------------

{- | Derive a storage namespace from repo path and user ID.
This is the primary isolation boundary — each (repo, user) pair
gets its own namespace so entries never bleed across users.
-}
repoUserNamespace :: FilePath -> UserId -> Text
repoUserNamespace repoPath (UserId uid) =
  "repo:" <> slugify repoPath <> "/user:" <> uid

-- | Session-scoped namespace (ephemeral, cleared on session end).
sessionNamespace :: SessionId -> Text
sessionNamespace (SessionId sid) = "session:" <> sid

slugify :: FilePath -> Text
slugify = Text.map normalize . Text.pack
 where
  normalize '/' = '-'
  normalize '\\' = '-'
  normalize c = c

-- --------------------------------------------------------------------
-- Backend record-of-functions
-- --------------------------------------------------------------------

data StorageBackend = StorageBackend
  { get :: Text -> Text -> IO (Either StorageError BS.ByteString)
  , put :: Text -> Text -> BS.ByteString -> IO (Either StorageError ())
  , delete :: Text -> Text -> IO (Either StorageError ())
  , list :: Text -> IO [Text]
  }

-- --------------------------------------------------------------------
-- Filesystem backend (wraps storage-core)
-- --------------------------------------------------------------------

filesystemBackend :: Storage -> StorageBackend
filesystemBackend store =
  StorageBackend
    { get = SC.getValue store
    , put = SC.putValue store
    , delete = SC.deleteValue store
    , list = SC.listKeys store
    }

-- | Create a filesystem-backed storage backend rooted at the given path.
newBackend :: FilePath -> IO StorageBackend
newBackend root = do
  store <- SC.createStorage root
  pure (filesystemBackend store)
