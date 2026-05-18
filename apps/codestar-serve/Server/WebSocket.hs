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

buildAuthConfig :: Config -> IO AuthConfig
buildAuthConfig config = case config.authMode of
  NoAuth -> pure NoAuthConfig
  JwtAuth jcfg -> do
    manager <- newTlsManager
    cache <- newJwksCache manager jcfg.jwksSource jcfg.cacheTtlSeconds
    pure (JwtConfig (validateToken (newJwtValidator cache jcfg)))

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

extractBearerFromHeaders :: WS.Headers -> Maybe Text
extractBearerFromHeaders headers =
  case lookup "Authorization" headers of
    Just val ->
      let t = TE.decodeUtf8 val
       in if Text.isPrefixOf "Bearer " t
            then Just (Text.drop 7 t)
            else Nothing
    Nothing -> Nothing
