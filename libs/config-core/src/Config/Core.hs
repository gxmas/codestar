{-# LANGUAGE DeriveAnyClass #-}

-- | Hierarchical configuration management.
--
-- Three-level scope hierarchy: Global < Project < Session.
-- Higher scopes override lower ones. Schema validation ensures
-- type correctness at write time.
module Config.Core
  ( -- * Scopes
    ConfigScope (..)

    -- * Values and keys
  , ConfigValue (..)
  , ConfigKey (..)

    -- * Type checking
  , ConfigType (..)

    -- * Schema and validation
  , ConfigSchema (..)
  , ValidationError (..)

    -- * Sources
  , ConfigSource (..)
  , MergedConfig (..)

    -- * Configuration handle
  , Configuration
  , newConfiguration

    -- * Read operations
  , getConfig
  , getMerged
  , getAllConfig
  , resolveConfig

    -- * Write operations
  , setConfig
  , unsetConfig

    -- * Schema management
  , registerSchema
  , validateConfig

    -- * File operations
  , loadFromFile
  , saveToFile
  ) where

import Prelude hiding (log)

import Data.Aeson
  ( ToJSON (..), FromJSON (..), ToJSONKey, FromJSONKey
  , Value (..)
  )
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Storage.Core (Storage, StorageError, Key, putJSON, getJSON)
import OTel.Log
  ( getGlobalLoggerProvider
  , getLogger
  , defaultLogRecord
  , LogBody (..)
  , SeverityNumber (..)
  , LogRecord (..)
  , emit
  )
import OTel.Attribute (AttributeValue (..), Attribute, InstrumentationScope (..))
import OTel.Attribute qualified as OTelAttr
import OTel.Context (getCurrent)

-- ---------------------------------------------------------------------------
-- Logging helper
-- ---------------------------------------------------------------------------

logMsg :: SeverityNumber -> Text -> [Attribute] -> IO ()
logMsg sev body attrs = do
  provider <- getGlobalLoggerProvider
  logger <- getLogger provider InstrumentationScope
    { scopeName = "config-core"
    , scopeVersion = Nothing
    , scopeSchemaUrl = Nothing
    , scopeAttributes = Nothing
    }
  ctx <- getCurrent
  emit logger defaultLogRecord
    { logSeverityNumber = Just sev
    , logBody = Just (LogBodyString body)
    , logAttributes = OTelAttr.fromList attrs
    , logContext = Just ctx
    }

-- ---------------------------------------------------------------------------
-- Scopes
-- ---------------------------------------------------------------------------

data ConfigScope = Global | Project | Session
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Values and keys
-- ---------------------------------------------------------------------------

data ConfigValue
  = CString !Text
  | CInt !Int
  | CBool !Bool
  | CList ![ConfigValue]
  | CMap !(Map Text ConfigValue)
  deriving stock (Eq, Show, Generic)

instance ToJSON ConfigValue where
  toJSON = \case
    CString t -> toJSON t
    CInt n    -> toJSON n
    CBool b   -> toJSON b
    CList xs  -> toJSON xs
    CMap m    -> toJSON m

instance FromJSON ConfigValue where
  parseJSON = \case
    String t  -> pure (CString t)
    Number n  -> pure (CInt (round n))
    Bool b    -> pure (CBool b)
    Array a   -> CList <$> mapM parseJSON (foldr (:) [] a)
    Object o  -> CMap . Map.fromList
                   <$> mapM (\(k, v) -> (,) (Key.toText k) <$> parseJSON v) (KM.toList o)
    Null      -> fail "ConfigValue: null not supported"

newtype ConfigKey = ConfigKey { unConfigKey :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- ---------------------------------------------------------------------------
-- Type checking
-- ---------------------------------------------------------------------------

data ConfigType = TString | TInt | TBool | TList !ConfigType | TMap
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

typeMatches :: ConfigType -> ConfigValue -> Bool
typeMatches TString  (CString _) = True
typeMatches TInt     (CInt _)    = True
typeMatches TBool    (CBool _)   = True
typeMatches (TList _) (CList _)  = True
typeMatches TMap     (CMap _)    = True
typeMatches _        _           = False

-- ---------------------------------------------------------------------------
-- Schema and validation
-- ---------------------------------------------------------------------------

data ConfigSchema = ConfigSchema
  { csKey         :: !ConfigKey
  , csType        :: !ConfigType
  , csDefault     :: !(Maybe ConfigValue)
  , csRequired    :: !Bool
  , csValidator   :: !(Maybe (ConfigValue -> Either Text ()))
  , csDescription :: !Text
  }

data ValidationError = ValidationError
  { veKey     :: !ConfigKey
  , veMessage :: !Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Sources
-- ---------------------------------------------------------------------------

data ConfigSource
  = FileSource !FilePath
  | Environment
  | DefaultSource
  | OverrideSource
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MergedConfig = MergedConfig
  { mcEffective :: !ConfigValue
  , mcSources   :: ![(ConfigScope, ConfigValue)]
  } deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Configuration handle
-- ---------------------------------------------------------------------------

data Configuration = Configuration
  { cfGlobal  :: !(IORef (Map ConfigKey ConfigValue))
  , cfProject :: !(IORef (Map ConfigKey ConfigValue))
  , cfSession :: !(IORef (Map ConfigKey ConfigValue))
  , cfSchemas :: !(IORef (Map ConfigKey ConfigSchema))
  }

newConfiguration :: IO Configuration
newConfiguration = Configuration
  <$> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

scopeRef :: Configuration -> ConfigScope -> IORef (Map ConfigKey ConfigValue)
scopeRef cfg Global  = cfGlobal cfg
scopeRef cfg Project = cfProject cfg
scopeRef cfg Session = cfSession cfg

-- ---------------------------------------------------------------------------
-- Read operations
-- ---------------------------------------------------------------------------

getConfig :: Configuration -> Maybe ConfigScope -> ConfigKey -> IO (Maybe ConfigValue)
getConfig cfg Nothing  key = getMerged cfg key
getConfig cfg (Just s) key = Map.lookup key <$> readIORef (scopeRef cfg s)

getMerged :: Configuration -> ConfigKey -> IO (Maybe ConfigValue)
getMerged cfg key = do
  s <- readIORef (cfSession cfg)
  p <- readIORef (cfProject cfg)
  g <- readIORef (cfGlobal cfg)
  pure $ Map.lookup key s
     <|> Map.lookup key p
     <|> Map.lookup key g
  where
    Nothing <|> b = b
    a       <|> _ = a

getAllConfig :: Configuration -> ConfigScope -> IO (Map ConfigKey ConfigValue)
getAllConfig cfg scope = readIORef (scopeRef cfg scope)

resolveConfig :: Configuration -> ConfigKey -> IO (Maybe (ConfigValue, ConfigScope))
resolveConfig cfg key = do
  s <- readIORef (cfSession cfg)
  p <- readIORef (cfProject cfg)
  g <- readIORef (cfGlobal cfg)
  pure $ case Map.lookup key s of
    Just v  -> Just (v, Session)
    Nothing -> case Map.lookup key p of
      Just v  -> Just (v, Project)
      Nothing -> case Map.lookup key g of
        Just v  -> Just (v, Global)
        Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- Write operations
-- ---------------------------------------------------------------------------

setConfig :: Configuration -> ConfigScope -> ConfigKey -> ConfigValue -> IO (Either ValidationError ())
setConfig cfg scope key val = do
  schemas <- readIORef (cfSchemas cfg)
  case Map.lookup key schemas of
    Nothing -> do
      modifyIORef' (scopeRef cfg scope) (Map.insert key val)
      logMsg SeverityInfo "Config override applied"
        [ ("config.key", StringValue (unConfigKey key))
        , ("config.scope", StringValue (T.pack (show scope)))
        ]
      pure (Right ())
    Just schema
      | not (typeMatches (csType schema) val) ->
          pure $ Left ValidationError
            { veKey     = key
            , veMessage = "type mismatch"
            }
      | otherwise -> case csValidator schema of
          Just validator -> case validator val of
            Left msg -> pure $ Left ValidationError { veKey = key, veMessage = msg }
            Right () -> do
              modifyIORef' (scopeRef cfg scope) (Map.insert key val)
              logMsg SeverityInfo "Config override applied"
                [ ("config.key", StringValue (unConfigKey key))
                , ("config.scope", StringValue (T.pack (show scope)))
                ]
              pure (Right ())
          Nothing -> do
            modifyIORef' (scopeRef cfg scope) (Map.insert key val)
            logMsg SeverityInfo "Config override applied"
              [ ("config.key", StringValue (unConfigKey key))
              , ("config.scope", StringValue (T.pack (show scope)))
              ]
            pure (Right ())

unsetConfig :: Configuration -> ConfigScope -> ConfigKey -> IO ()
unsetConfig cfg scope key =
  modifyIORef' (scopeRef cfg scope) (Map.delete key)

-- ---------------------------------------------------------------------------
-- Schema management
-- ---------------------------------------------------------------------------

registerSchema :: Configuration -> ConfigSchema -> IO ()
registerSchema cfg schema =
  modifyIORef' (cfSchemas cfg) (Map.insert (csKey schema) schema)

validateConfig :: Configuration -> ConfigScope -> IO [ValidationError]
validateConfig cfg scope = do
  vals    <- readIORef (scopeRef cfg scope)
  schemas <- readIORef (cfSchemas cfg)
  let errors = concatMap (checkSchema vals) (Map.elems schemas)
  pure errors
  where
    checkSchema vals schema =
      case Map.lookup (csKey schema) vals of
        Nothing
          | csRequired schema -> [ValidationError (csKey schema) "required key missing"]
          | otherwise -> []
        Just val
          | not (typeMatches (csType schema) val) ->
              [ValidationError (csKey schema) "type mismatch"]
          | otherwise -> case csValidator schema of
              Just v -> case v val of
                Left msg -> [ValidationError (csKey schema) msg]
                Right () -> []
              Nothing -> []

-- ---------------------------------------------------------------------------
-- File operations
-- ---------------------------------------------------------------------------

loadFromFile :: Configuration -> Storage -> ConfigScope -> Key -> IO (Either StorageError ())
loadFromFile cfg store scope key = do
  result <- getJSON store "config" key
  case result of
    Left err -> do
      logMsg SeverityError "Config load failed"
        [ ("config.scope", StringValue (T.pack (show scope)))
        , ("error", StringValue (T.pack (show err)))
        ]
      pure (Left err)
    Right (vals :: Map ConfigKey ConfigValue) -> do
      writeIORef (scopeRef cfg scope) vals
      logMsg SeverityInfo "Config source loaded"
        [ ("config.scope", StringValue (T.pack (show scope)))
        , ("config.source", StringValue key)
        ]
      pure (Right ())

saveToFile :: Configuration -> Storage -> ConfigScope -> Key -> IO (Either StorageError ())
saveToFile cfg store scope key = do
  vals <- readIORef (scopeRef cfg scope)
  putJSON store "config" key vals
