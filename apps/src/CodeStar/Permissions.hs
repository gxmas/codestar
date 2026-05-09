module CodeStar.Permissions
  ( -- * Scope
    PermissionScope (..)

    -- * Store
  , PermissionStore (..)
  , newPermissionStore

    -- * Operations
  , check
  , grant
  , revoke
  ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import CodeStar.Config.Paths qualified as Paths

-- --------------------------------------------------------------------
-- Scope
-- --------------------------------------------------------------------

-- | How long a granted permission persists.
data PermissionScope
  = -- | in-memory only, lost when the session ends
    Session
  | -- | written to .codestar/settings.json
    Project
  | -- | written to ~/.codestar/settings.json
    Global
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- --------------------------------------------------------------------
-- On-disk format
-- --------------------------------------------------------------------

newtype PersistedPermissions = PersistedPermissions
  { allowedCommands :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- --------------------------------------------------------------------
-- Store
-- --------------------------------------------------------------------

data PermissionStore = PermissionStore
  { checkPerm :: Text -> IO Bool
  , grantPerm :: Text -> PermissionScope -> IO ()
  , revokePerm :: Text -> PermissionScope -> IO ()
  }

{- | Create a three-layer permission store.
Loads project and global settings at startup; session layer lives in a TVar.
-}
newPermissionStore ::
  -- | workspace root (for project settings)
  FilePath ->
  -- | global config dir (~/.codestar)
  FilePath ->
  IO PermissionStore
newPermissionStore workspace globalDir = do
  sessionVar <- newIORef Set.empty
  projectVar <- newIORef =<< loadPersistedPerms (projectSettingsPath workspace)
  globalVar <- newIORef =<< loadPersistedPerms (globalSettingsPath globalDir)

  pure
    PermissionStore
      { checkPerm = doCheck sessionVar projectVar globalVar
      , grantPerm = doGrant sessionVar projectVar globalVar workspace globalDir
      , revokePerm = doRevoke sessionVar projectVar globalVar workspace globalDir
      }

-- --------------------------------------------------------------------
-- Operations
-- --------------------------------------------------------------------

check :: PermissionStore -> Text -> IO Bool
check store = store.checkPerm

grant :: PermissionStore -> Text -> PermissionScope -> IO ()
grant store = store.grantPerm

revoke :: PermissionStore -> Text -> PermissionScope -> IO ()
revoke store = store.revokePerm

-- --------------------------------------------------------------------
-- Implementation
-- --------------------------------------------------------------------

doCheck :: IORef (Set Text) -> IORef (Set Text) -> IORef (Set Text) -> Text -> IO Bool
doCheck sessionVar projectVar globalVar cmd = do
  sessionPerms <- readIORef sessionVar
  projectPerms <- readIORef projectVar
  globalPerms <- readIORef globalVar
  pure (Set.member cmd sessionPerms || Set.member cmd projectPerms || Set.member cmd globalPerms)

doGrant ::
  IORef (Set Text) ->
  IORef (Set Text) ->
  IORef (Set Text) ->
  FilePath ->
  FilePath ->
  Text ->
  PermissionScope ->
  IO ()
doGrant sessionVar projectVar globalVar workspace globalDir cmd scope = case scope of
  Session -> atomicModifyIORef' sessionVar (\s -> (Set.insert cmd s, ()))
  Project -> do
    atomicModifyIORef' projectVar (\s -> (Set.insert cmd s, ()))
    addToSettings (projectSettingsPath workspace) cmd
  Global -> do
    atomicModifyIORef' globalVar (\s -> (Set.insert cmd s, ()))
    addToSettings (globalSettingsPath globalDir) cmd

doRevoke ::
  IORef (Set Text) ->
  IORef (Set Text) ->
  IORef (Set Text) ->
  FilePath ->
  FilePath ->
  Text ->
  PermissionScope ->
  IO ()
doRevoke sessionVar projectVar globalVar workspace globalDir cmd scope = case scope of
  Session -> atomicModifyIORef' sessionVar (\s -> (Set.delete cmd s, ()))
  Project -> do
    atomicModifyIORef' projectVar (\s -> (Set.delete cmd s, ()))
    removeFromSettings (projectSettingsPath workspace) cmd
  Global -> do
    atomicModifyIORef' globalVar (\s -> (Set.delete cmd s, ()))
    removeFromSettings (globalSettingsPath globalDir) cmd

-- --------------------------------------------------------------------
-- Settings file helpers
-- --------------------------------------------------------------------

projectSettingsPath :: FilePath -> FilePath
projectSettingsPath workspace = Paths.projectDir workspace </> "settings.json"

globalSettingsPath :: FilePath -> FilePath
globalSettingsPath globalDir = globalDir </> "settings.json"

loadPersistedPerms :: FilePath -> IO (Set Text)
loadPersistedPerms path = do
  exists <- doesFileExist path
  if not exists
    then pure Set.empty
    else do
      bs <- BS.readFile path
      case eitherDecodeStrict' bs :: Either String PersistedPermissions of
        Left _ -> pure Set.empty
        Right pp -> pure (Set.fromList pp.allowedCommands)

addToSettings :: FilePath -> Text -> IO ()
addToSettings path cmd = do
  existing <- loadPersistedPerms path
  let updated = Set.insert cmd existing
  writePerms path updated

removeFromSettings :: FilePath -> Text -> IO ()
removeFromSettings path cmd = do
  existing <- loadPersistedPerms path
  writePerms path (Set.delete cmd existing)

writePerms :: FilePath -> Set Text -> IO ()
writePerms path perms = do
  createDirectoryIfMissing True (parentDir path)
  let pp = PersistedPermissions{allowedCommands = Set.toList perms}
  BL.writeFile path (encode pp)

parentDir :: FilePath -> FilePath
parentDir = reverse . dropWhile (/= '/') . reverse
