-- |
-- Module      : Network.MCP.Server.Logging
-- Stability   : stable
--
-- Logging feature for MCP servers. Registers the @logging/setLevel@
-- request handler and provides a 'Logger' for emitting log notifications
-- to the connected client.
module Network.MCP.Server.Logging
  ( LoggingFeature
  , LoggingConfig (..)
  , defaultLoggingConfig
  , newLoggingFeature
  , Logger (..)
  , attach
  , detach
  , getLogger
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

import Network.MCP.Session (Session (..))
import Network.MCP.Types (LoggingLevel (..), RPCError (..))

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

-- | Configuration for the logging feature.
data LoggingConfig = LoggingConfig
  { loggingMaxRate :: !(Maybe Int)
    -- ^ Maximum log messages per second. 'Nothing' means unlimited.
  }

-- | Default: 100 messages/second.
defaultLoggingConfig :: LoggingConfig
defaultLoggingConfig = LoggingConfig { loggingMaxRate = Just 100 }

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Opaque logging feature state.
data LoggingFeature = LoggingFeature
  { lfLevel      :: !(TVar LoggingLevel)
  , lfSession    :: !(TVar (Maybe Session))
  , lfMaxRate    :: !(Maybe Int)
  , lfRateBucket :: !(TVar (Double, UTCTime))
  }

-- | Logger bound to a 'LoggingFeature'.
data Logger = Logger
  { emit         :: LoggingLevel -> Aeson.Value -> Maybe Text -> IO ()
  , minimumLevel :: IO LoggingLevel
  }

------------------------------------------------------------------------
-- Internal param type
------------------------------------------------------------------------

newtype SetLevelParams = SetLevelParams LoggingLevel

instance Aeson.FromJSON SetLevelParams where
  parseJSON = Aeson.withObject "SetLevelParams" $ \o ->
    SetLevelParams <$> o .: "level"

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

newLoggingFeature :: LoggingConfig -> IO LoggingFeature
newLoggingFeature cfg = do
  lvTVar  <- newTVarIO LevelWarning
  sesTVar <- newTVarIO Nothing
  t0      <- getCurrentTime
  let initialTokens = fromIntegral (maybe 100 id cfg.loggingMaxRate)
  bucketTVar <- newTVarIO (initialTokens, t0)
  pure LoggingFeature
    { lfLevel      = lvTVar
    , lfSession    = sesTVar
    , lfMaxRate    = cfg.loggingMaxRate
    , lfRateBucket = bucketTVar
    }

------------------------------------------------------------------------
-- Attach / detach
------------------------------------------------------------------------

attach :: LoggingFeature -> Session -> IO ()
attach lf session = do
  atomically (writeTVar lf.lfSession (Just session))
  session.sessionOnRequest "logging/setLevel" $ \params _meta ->
    case Aeson.fromJSON params of
      Aeson.Error err ->
        pure (Left (RPCError (-32602) (T.pack $ "Invalid params: " ++ err) Nothing))
      Aeson.Success (SetLevelParams level) -> do
        atomically (writeTVar lf.lfLevel level)
        pure (Right (Aeson.object []))

detach :: LoggingFeature -> IO ()
detach lf = atomically (writeTVar lf.lfSession Nothing)

------------------------------------------------------------------------
-- Logger
------------------------------------------------------------------------

getLogger :: LoggingFeature -> Logger
getLogger lf = Logger
  { emit = emitLog lf
  , minimumLevel = atomically (readTVar lf.lfLevel)
  }

-- | Try to consume one token from the bucket. Returns True if allowed.
checkRateLimit :: LoggingFeature -> IO Bool
checkRateLimit lf = case lf.lfMaxRate of
  Nothing -> pure True
  Just maxRate -> do
    now <- getCurrentTime
    atomically $ do
      (tokens, lastTime) <- readTVar lf.lfRateBucket
      let elapsed   = realToFrac (diffUTCTime now lastTime) :: Double
          newTokens = min (fromIntegral maxRate)
                         (tokens + elapsed * fromIntegral maxRate)
      if newTokens >= 1.0
        then do
          writeTVar lf.lfRateBucket (newTokens - 1.0, now)
          pure True
        else do
          writeTVar lf.lfRateBucket (newTokens, now)
          pure False

emitLog :: LoggingFeature -> LoggingLevel -> Aeson.Value -> Maybe Text -> IO ()
emitLog lf level data_ logger = do
  minLevel <- atomically (readTVar lf.lfLevel)
  if level < minLevel
    then pure ()
    else do
      allowed <- checkRateLimit lf
      if not allowed
        then pure ()
        else do
          mSession <- atomically (readTVar lf.lfSession)
          case mSession of
            Nothing      -> pure ()
            Just session ->
              session.sessionNotify "notifications/message" (Just notifParams)
  where
    notifParams =
      Aeson.object $
        [ "level" .= level
        , "data"  .= data_
        ]
          ++ maybe [] (\l -> ["logger" .= l]) logger
