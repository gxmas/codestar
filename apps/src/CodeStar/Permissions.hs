{- |
= CodeStar.Permissions — tool invocation permission store

Before executing a 'SideEffect' or 'LocalWrite' tool, the agent loop
checks whether the user has already approved it.  This module manages
that approval state across three layers with different persistence scopes:

@
  Session layer  — in-memory IORef, lost when the agent stops
  Project layer  — .codestar/settings.json in the workspace root
  Global layer   — ~/.codestar/settings.json in the user's home dir
@

'check' returns 'True' if a command string appears in __any__ of the three
layers.  This means a command granted globally does not need re-approving
per project, and a command granted for a session does not need to be
persisted to disk.

'grant' and 'revoke' modify the chosen layer and, for 'Project' and
'Global', persist the change to disk so it survives process restarts.

The permission key is a free-form text string — typically the tool name
plus an optional argument pattern (e.g. @"bash"@, @"bash:npm test"@).
-}
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

-- | A three-layer permission store implemented as a record of functions.
data PermissionStore = PermissionStore
  { checkPerm  :: Text -> IO Bool
  -- ^ Return 'True' if the command is allowed in any layer.
  , grantPerm  :: Text -> PermissionScope -> IO ()
  -- ^ Add the command to the specified layer (persisting if Project\/Global).
  , revokePerm :: Text -> PermissionScope -> IO ()
  -- ^ Remove the command from the specified layer.
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
