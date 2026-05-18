{- |
= Server.WebSocket — WebSocket authentication and connection acceptance

This module is the __security boundary__ of the server.  Every incoming
WebSocket connection passes through here before any agent code runs.

== Authentication flow

@
  HTTP Upgrade request
       │  Authorization: Bearer <token>
       ▼
  extractBearerFromHeaders
       │  token :: Text
       ▼
  authenticate authCfg token
       │
  ┌────┴──────────────────────────────────┐
  │ Unauthenticated → rejectRequest       │  connection closed, 401-style
  │ Authenticated identity                │
  └─────────────────────┬─────────────────┘
                        ▼
                  WS.acceptRequest
                        │
                  onConnection callback
@

== Auth modes

  * __NoAuth__ (@NoAuthConfig@): every connection is accepted with a
    synthetic local identity.  Suitable for development and single-user
    deployments.
  * __JwtAuth__ (@JwtConfig@): validates a Bearer JWT against a JWKS
    endpoint.  The JWKS cache refreshes in the background so key rotations
    are handled without restarting the server.

== Ping thread

'WS.withPingThread' sends a WebSocket ping frame every 30 seconds.  This
keeps the connection alive through NAT gateways and load-balancer idle
timeouts that would otherwise silently drop quiet connections.
-}
module Server.WebSocket
  ( buildAuthConfig
  , makeWsApp
  ) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Exception (SomeException, finally, try)
import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client.TLS (newTlsManager)
import Network.WebSockets qualified as WS

import CodeStar.Config (Config (..), AgentConfig)
import CodeStar.Config.Types (AuthMode (..), JwtAuthConfig (..))
import CodeStar.Platform.Auth (AuthConfig (..), AuthResult (..), Identity (..), authenticate)
import CodeStar.Platform.Auth.Jwks (newJwksCache)
import CodeStar.Platform.Auth.Jwt (newJwtValidator, validateToken)
import CodeStar.Platform.SessionManager (SessionManager)
import CodeStar.Telemetry (TelemetryRecorder (..))
import CodeStar.Telemetry qualified as Tel

-- | Construct the 'AuthConfig' from the application config.
--
-- For 'JwtAuth', this creates an HTTPS client, starts the JWKS cache
-- (which fetches and periodically refreshes the public keys used to
-- verify JWTs), and builds a 'JwtValidator'.  The cache means that
-- key-rotation events at the identity provider do not require a server
-- restart.
buildAuthConfig :: Config -> IO AuthConfig
buildAuthConfig config = case config.authMode of
  NoAuth -> pure NoAuthConfig
  JwtAuth jcfg -> do
    manager <- newTlsManager
    cache <- newJwksCache manager jcfg.jwksSource jcfg.cacheTtlSeconds
    pure (JwtConfig (validateToken (newJwtValidator cache jcfg)))

-- | Build a 'WS.ServerApp' (a WebSocket request handler) that authenticates
-- incoming connections before forwarding them to @onConnection@.
--
-- The @shutdownVar@ 'TVar' is checked first: if the server is draining,
-- new connections are rejected immediately to avoid starting work that
-- cannot be completed.
--
-- The @onConnection@ parameter is the dependency-injection point that
-- decouples the WebSocket layer from the connection-handling logic in
-- "Server".  Passing it as a function rather than hard-coding a call makes
-- this module independently testable.
makeWsApp ::
  AgentConfig ->
  AuthConfig ->
  TelemetryRecorder ->
  SessionManager ->
  TVar Bool ->
  (AgentConfig -> TelemetryRecorder -> SessionManager -> Identity -> WS.Connection -> IO ()) ->
  WS.ServerApp
makeWsApp config authCfg recorder sessionMgr shutdownVar onConnection pending = do
  isShuttingDown <- readTVarIO shutdownVar
  if isShuttingDown
    then WS.rejectRequest pending "Server shutting down"
    else do
      let headers = WS.requestHeaders (WS.pendingRequest pending)
          token = extractBearerFromHeaders headers
      authResult <- authenticate authCfg (maybe "" id token)
      case authResult of
        Unauthenticated reason -> do
          WS.rejectRequest pending (TE.encodeUtf8 reason)
            `finally` void (try @SomeException (recorder.recordEvent Tel.EvAuthRejected{ Tel.rejectionReason = reason }))
        Authenticated identity -> do
          conn <- WS.acceptRequest pending
          WS.withPingThread conn 30 (pure ()) $
            onConnection config recorder sessionMgr identity conn

-- | Extract the Bearer token from an @Authorization@ HTTP header.
-- Returns 'Nothing' if the header is absent or not in Bearer format.
-- The token string is passed directly to 'authenticate'; no decoding
-- or validation happens here.
extractBearerFromHeaders :: WS.Headers -> Maybe Text
extractBearerFromHeaders headers =
  case lookup "Authorization" headers of
    Just val ->
      let t = TE.decodeUtf8 val
       in if Text.isPrefixOf "Bearer " t
            then Just (Text.drop 7 t)
            else Nothing
    Nothing -> Nothing
