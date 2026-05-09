-- | General-purpose namespaced key-value store.
--
-- Files are stored at @{root}/{namespace}/{key}.json@.
-- The 'Storage' handle is a thin wrapper around a root directory.
-- Convenience wrappers 'putJSON' / 'getJSON' handle JSON encoding so
-- callers never touch raw bytes directly.
module Storage.Core
  ( -- * Handle
    Storage
  , createStorage
  , createDefaultStorage
  , storageRoot

    -- * Types
  , Namespace
  , Key
  , StorageError (..)

    -- * Raw byte operations
  , putValue
  , getValue
  , deleteValue
  , listKeys
  , existsKey

    -- * JSON convenience wrappers
  , putJSON
  , getJSON
  ) where

import Prelude hiding (log)

import Control.Exception (IOException, try)
import Control.Monad (forM)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeFile
  )
import System.FilePath ((</>), takeDirectory, takeExtension, dropExtension)
import qualified System.Environment

import Telemetry.Core (withSpan, AttributeValue (..))

-- ---------------------------------------------------------------------------
-- Core types
-- ---------------------------------------------------------------------------

type Namespace = Text

type Key = Text

data StorageError
  = NotFound Namespace Key
  | StorageIOError Text
  deriving stock (Eq, Show)

newtype Storage = Storage { storageRoot :: FilePath }

-- ---------------------------------------------------------------------------
-- Handle construction
-- ---------------------------------------------------------------------------

createStorage :: FilePath -> IO Storage
createStorage root = do
  createDirectoryIfMissing True root
  pure (Storage root)

createDefaultStorage :: IO Storage
createDefaultStorage = do
  home <- getEnvDefault "HOME" "."
  createStorage (home </> ".codestar")

-- ---------------------------------------------------------------------------
-- File path helpers
-- ---------------------------------------------------------------------------

valuePath :: Storage -> Namespace -> Key -> FilePath
valuePath (Storage root) ns key =
  root </> T.unpack ns </> T.unpack key <> ".json"

nsDir :: Storage -> Namespace -> FilePath
nsDir (Storage root) ns = root </> T.unpack ns

-- ---------------------------------------------------------------------------
-- Raw byte operations
-- ---------------------------------------------------------------------------

putValue :: Storage -> Namespace -> Key -> ByteString -> IO (Either StorageError ())
putValue store ns key bytes =
  withSpan "db.operation"
    [ ("db.operation", TextValue "write")
    , ("db.namespace", TextValue ns)
    ] $ do
    let path = valuePath store ns key
    createDirectoryIfMissing True (takeDirectory path)
    result <- try (BS.writeFile path bytes) :: IO (Either IOException ())
    pure $ case result of
      Left e  -> Left (StorageIOError (T.pack (show e)))
      Right _ -> Right ()

getValue :: Storage -> Namespace -> Key -> IO (Either StorageError ByteString)
getValue store ns key =
  withSpan "db.operation"
    [ ("db.operation", TextValue "read")
    , ("db.namespace", TextValue ns)
    ] $ do
    let path = valuePath store ns key
    exists <- doesFileExist path
    if not exists
      then pure (Left (NotFound ns key))
      else do
        result <- try (BS.readFile path) :: IO (Either IOException ByteString)
        pure $ case result of
          Left e      -> Left (StorageIOError (T.pack (show e)))
          Right bytes -> Right bytes

deleteValue :: Storage -> Namespace -> Key -> IO (Either StorageError ())
deleteValue store ns key =
  withSpan "db.operation"
    [ ("db.operation", TextValue "delete")
    , ("db.namespace", TextValue ns)
    ] $ do
    let path = valuePath store ns key
    exists <- doesFileExist path
    if not exists
      then pure (Right ())
      else do
        result <- try (removeFile path) :: IO (Either IOException ())
        pure $ case result of
          Left e  -> Left (StorageIOError (T.pack (show e)))
          Right _ -> Right ()

listKeys :: Storage -> Namespace -> IO [Key]
listKeys store ns = do
  let dir = nsDir store ns
  go dir ""
 where
  go :: FilePath -> FilePath -> IO [Key]
  go base prefix = do
    result <- try (listDirectory base) :: IO (Either IOException [FilePath])
    case result of
      Left _ -> pure []
      Right ents -> do
        nested <- forM ents $ \entry -> do
          let fullPath = base </> entry
              relPath =
                if null prefix
                  then entry
                  else prefix </> entry
          isDir <- doesDirectoryExist fullPath
          if isDir
            then go fullPath relPath
            else
              pure
                [ T.pack (dropExtension relPath)
                | takeExtension entry == ".json"
                ]
        pure (concat nested)

existsKey :: Storage -> Namespace -> Key -> IO Bool
existsKey store ns key = doesFileExist (valuePath store ns key)

-- ---------------------------------------------------------------------------
-- JSON convenience wrappers
-- ---------------------------------------------------------------------------

putJSON :: ToJSON a => Storage -> Namespace -> Key -> a -> IO (Either StorageError ())
putJSON store ns key val =
  putValue store ns key (LBS.toStrict (Aeson.encode val))

getJSON :: FromJSON a => Storage -> Namespace -> Key -> IO (Either StorageError a)
getJSON store ns key = do
  result <- getValue store ns key
  case result of
    Left e     -> pure (Left e)
    Right bytes ->
      case Aeson.eitherDecodeStrict bytes of
        Left msg -> pure (Left (StorageIOError (T.pack msg)))
        Right val -> pure (Right val)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

getEnvDefault :: String -> String -> IO String
getEnvDefault key def =
  either (const def) id <$>
    (try (System.Environment.getEnv key) :: IO (Either IOException String))
