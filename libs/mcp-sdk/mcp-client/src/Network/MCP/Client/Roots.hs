-- |
-- Module      : Network.MCP.Client.Roots
-- Stability   : stable
--
-- Client-side roots feature: exposes a list of filesystem roots to the
-- server and notifies it when the list changes.
module Network.MCP.Client.Roots
  ( Root (..)
  , RootsProvider (..)
  , RootsFeature
  , newRootsFeature
  , attach
  , detach
  , notifyChanged
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Data.Aeson ((.=), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as E
import Data.Text (Text)

import Network.MCP.Session (Session (..))
import Network.MCP.Types.Content (URI)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | A single filesystem root exposed to the server.
data Root = Root
  { rootUri :: !URI
  , rootName :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON Root where
  toJSON r =
    Aeson.object $
      ["uri" .= r.rootUri]
        ++ maybe [] (\n -> ["name" .= n]) r.rootName
  toEncoding r =
    E.pairs $
      "uri" .= r.rootUri
        <> foldMap ("name" .=) r.rootName

instance Aeson.FromJSON Root where
  parseJSON = Aeson.withObject "Root" $ \o ->
    Root
      <$> o Aeson..: "uri"
      <*> o .:? "name"

-- | Callback interface for retrieving the current roots list.
data RootsProvider = RootsProvider
  { listRoots :: IO [Root]
  }

-- | Opaque handle for the roots feature.
data RootsFeature = RootsFeature
  { provider :: !RootsProvider
  , sessionVar :: !(TVar (Maybe Session))
  }

------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------

-- | Create a new roots feature backed by the given provider.
newRootsFeature :: RootsProvider -> IO RootsFeature
newRootsFeature p = do
  var <- newTVarIO Nothing
  pure (RootsFeature p var)

-- | Attach the feature to a session: registers the @roots/list@ handler.
attach :: RootsFeature -> Session -> IO ()
attach feat session = do
  atomically $ writeTVar feat.sessionVar (Just session)
  session.sessionOnRequest "roots/list" $ \_ _ -> do
    roots <- feat.provider.listRoots
    pure (Right (Aeson.object ["roots" .= roots]))

-- | Detach the feature from the current session.
detach :: RootsFeature -> IO ()
detach feat = atomically $ writeTVar feat.sessionVar Nothing

-- | Notify the server that the roots list has changed.
notifyChanged :: RootsFeature -> IO ()
notifyChanged feat = do
  mSession <- atomically (readTVar feat.sessionVar)
  case mSession of
    Nothing -> pure ()
    Just session -> session.sessionNotify "notifications/roots/list_changed" Nothing
