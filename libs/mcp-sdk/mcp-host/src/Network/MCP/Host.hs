-- |
-- Module      : Network.MCP.Host
-- Stability   : stable
--
-- Multi-client orchestration layer. The host manages connections to
-- MCP servers, wires client features ('SamplingFeature',
-- 'RootsFeature', 'ElicitationFeature') for each connection, and
-- gates all server-initiated requests through a 'SecurityPolicy'.
module Network.MCP.Host
  ( -- * Host
    Host
  , newHost
  , HostConfig (..)
  , SecurityPolicy (..)

    -- * Managed clients
  , ManagedClient (..)
  , connect
  , disconnect
  , clients

    -- * Events
  , onClientConnected
  , onClientDisconnected

    -- * Errors
  , HostError (..)
  , HostErrorKind (..)
  ) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Network.MCP.Client.Elicitation
  ( ElicitationAction (..)
  , ElicitationFeature
  , ElicitationHandler (..)
  )
import qualified Network.MCP.Client.Elicitation as Elicitation
import Network.MCP.Client.Roots
  ( RootsFeature
  , RootsProvider (..)
  )
import qualified Network.MCP.Client.Roots as Roots
import Network.MCP.Client.Sampling
  ( Sampler (..)
  , SamplerError (..)
  , SamplerErrorKind (..)
  , SamplingFeature
  , SamplingRequest
  )
import qualified Network.MCP.Client.Sampling as Sampling
import Network.MCP.Session
  ( CloseReason (..)
  , Session (..)
  )
import Network.MCP.Types (Implementation)

------------------------------------------------------------------------
-- SecurityPolicy
------------------------------------------------------------------------

-- | Predicate-based security policy controlling which server-initiated
-- requests are permitted. This is the root trust anchor — no request
-- reaches a feature handler without passing through the policy.
data SecurityPolicy = SecurityPolicy
  { allowSampling :: Implementation -> SamplingRequest -> Bool
  , allowRoots :: Implementation -> Bool
  , allowElicitation :: Implementation -> Text -> Bool
  }

------------------------------------------------------------------------
-- HostConfig
------------------------------------------------------------------------

-- | Configuration for a 'Host'.
data HostConfig = HostConfig
  { hostSecurityPolicy :: !SecurityPolicy
  , hostMaxClients :: !(Maybe Word)
  , hostDefaultTimeoutMs :: !(Maybe Word)
  }

------------------------------------------------------------------------
-- HostError
------------------------------------------------------------------------

-- | Classification of host errors.
data HostErrorKind
  = MaxClientsExceeded
  | ConnectFailed
  deriving stock (Eq, Show, Bounded, Enum, Generic)

-- | A host error with kind and human-readable detail.
data HostError = HostError
  { hostErrorKind :: !HostErrorKind
  , hostErrorDetail :: !Text
  }
  deriving stock (Eq, Show, Generic)

------------------------------------------------------------------------
-- ManagedClient
------------------------------------------------------------------------

-- | A client connection managed by the host.
data ManagedClient = ManagedClient
  { managedClientId :: !Text
  , managedSession :: !Session
  , managedServerInfo :: !Implementation
  , managedSamplingFeature :: !(Maybe SamplingFeature)
  , managedRootsFeature :: !(Maybe RootsFeature)
  , managedElicitationFeature :: !(Maybe ElicitationFeature)
  }

------------------------------------------------------------------------
-- Host
------------------------------------------------------------------------

-- | Opaque host handle. Manages multiple client connections and
-- applies the security policy to all server-initiated requests.
data Host = Host
  { hostConfig :: !HostConfig
  , hostClients :: !(TVar (HashMap Text ManagedClient))
  , hostNextId :: !(TVar Int)
  , hostOnConnect :: !(TVar [ManagedClient -> IO ()])
  , hostOnDisconnect :: !(TVar [ManagedClient -> CloseReason -> IO ()])
  }

-- | Create a new host with the given configuration.
newHost :: HostConfig -> IO Host
newHost config = do
  cls <- newTVarIO HM.empty
  nid <- newTVarIO 0
  onC <- newTVarIO []
  onD <- newTVarIO []
  pure Host
    { hostConfig = config
    , hostClients = cls
    , hostNextId = nid
    , hostOnConnect = onC
    , hostOnDisconnect = onD
    }

------------------------------------------------------------------------
-- Connect / Disconnect
------------------------------------------------------------------------

-- | Connect a new client to the host. The caller provides a 'Session'
-- (already connected and handshake-complete) and the server's
-- 'Implementation' info. The host wires client features with
-- security-policy gating and registers lifecycle hooks.
connect
  :: Host
  -> Session
  -> Implementation
  -> Sampler s
  => s
  -> RootsProvider
  -> ElicitationHandler
  -> IO (Either HostError ManagedClient)
connect host session serverInfo sampler rootsProvider elicitHandler = do
  -- Check max clients
  currentClients <- readTVarIO host.hostClients
  case host.hostConfig.hostMaxClients of
    Just maxC | fromIntegral (HM.size currentClients) >= maxC ->
      pure (Left HostError
        { hostErrorKind = MaxClientsExceeded
        , hostErrorDetail = "Maximum number of clients reached"
        })
    _ -> do
      -- Generate client ID
      clientId <- atomically $ do
        n <- readTVar host.hostNextId
        writeTVar host.hostNextId (n + 1)
        pure n
      let cid = "client-" <> showT clientId

      let policy = host.hostConfig.hostSecurityPolicy

      -- Wire sampling feature with policy gating
      mSampFeat <- if not (policy.allowSampling serverInfo dummySamplingRequest)
            && True  -- always wire; gate at request time
        then do
          let gatedSampler = PolicyGatedSampler policy serverInfo sampler
          feat <- Sampling.newSamplingFeature gatedSampler
          Sampling.attach feat session
          pure (Just feat)
        else do
          let gatedSampler = PolicyGatedSampler policy serverInfo sampler
          feat <- Sampling.newSamplingFeature gatedSampler
          Sampling.attach feat session
          pure (Just feat)

      -- Wire roots feature if allowed
      mRootsFeat <- if policy.allowRoots serverInfo
        then do
          feat <- Roots.newRootsFeature rootsProvider
          Roots.attach feat session
          pure (Just feat)
        else pure Nothing

      -- Wire elicitation feature with policy gating
      let gatedElicitHandler = ElicitationHandler
            { handleElicitation = \msg schema meta -> do
                let mode = "form"  -- form mode by default
                if policy.allowElicitation serverInfo mode
                  then elicitHandler.handleElicitation msg schema meta
                  else pure ElicitDecline
            }
      mElicitFeat <- do
        feat <- Elicitation.newElicitationFeature gatedElicitHandler
        Elicitation.attach feat session
        pure (Just feat)

      let managed = ManagedClient
            { managedClientId = cid
            , managedSession = session
            , managedServerInfo = serverInfo
            , managedSamplingFeature = mSampFeat
            , managedRootsFeature = mRootsFeat
            , managedElicitationFeature = mElicitFeat
            }

      -- Register in client map
      atomically $ modifyTVar' host.hostClients (HM.insert cid managed)

      -- Register onClose hook to clean up and fire disconnect callbacks
      session.sessionOnClose $ \reason -> do
        atomically $ modifyTVar' host.hostClients (HM.delete cid)
        callbacks <- readTVarIO host.hostOnDisconnect
        mapM_ (\cb -> cb managed reason) callbacks

      -- Fire connect callbacks
      connectCallbacks <- readTVarIO host.hostOnConnect
      mapM_ (\cb -> cb managed) connectCallbacks

      pure (Right managed)

-- | Disconnect a managed client. Closes the session and removes it
-- from the host's client map.
disconnect :: Host -> ManagedClient -> IO ()
disconnect host mc = do
  -- Detach features
  case mc.managedSamplingFeature of
    Just feat -> Sampling.detach feat
    Nothing -> pure ()
  case mc.managedRootsFeature of
    Just feat -> Roots.detach feat
    Nothing -> pure ()
  case mc.managedElicitationFeature of
    Just feat -> Elicitation.detach feat
    Nothing -> pure ()

  -- Remove from client map
  atomically $ modifyTVar' host.hostClients (HM.delete mc.managedClientId)

  -- Close the session (triggers onClose hook which fires disconnect callbacks)
  mc.managedSession.sessionClose

-- | List all currently connected clients.
clients :: Host -> IO [ManagedClient]
clients host = HM.elems <$> readTVarIO host.hostClients

------------------------------------------------------------------------
-- Events
------------------------------------------------------------------------

-- | Register a callback to be invoked when a client connects.
onClientConnected :: Host -> (ManagedClient -> IO ()) -> IO ()
onClientConnected host cb =
  atomically $ modifyTVar' host.hostOnConnect (++ [cb])

-- | Register a callback to be invoked when a client disconnects.
onClientDisconnected :: Host -> (ManagedClient -> CloseReason -> IO ()) -> IO ()
onClientDisconnected host cb =
  atomically $ modifyTVar' host.hostOnDisconnect (++ [cb])

------------------------------------------------------------------------
-- Internal: Policy-gated sampler
------------------------------------------------------------------------

-- | A sampler adapter that checks the security policy before
-- forwarding to the underlying sampler.
data PolicyGatedSampler s = PolicyGatedSampler
  { pgPolicy :: !SecurityPolicy
  , pgServerInfo :: !Implementation
  , pgSampler :: !s
  }

instance Sampler s => Sampler (PolicyGatedSampler s) where
  sample pgs req =
    if pgs.pgPolicy.allowSampling pgs.pgServerInfo req
      then sample pgs.pgSampler req
      else pure (Left SamplerError
        { samplerErrorKind = UserRejected
        , samplerErrorDetail = "Denied by security policy"
        })

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

showT :: Show a => a -> Text
showT x = T.pack (show x)

-- Dummy request used only for the initial capability check.
-- At connect time we don't have a real request, so we just check
-- the policy's baseline intent.
dummySamplingRequest :: SamplingRequest
dummySamplingRequest = error "dummySamplingRequest: should not be evaluated"
