module Network.MCP.Server.LoggingSpec (spec) where

import Control.Concurrent.STM (newTVarIO, readTVarIO, atomically, modifyTVar')
import Control.Monad (replicateM_)
import qualified Data.Aeson as Aeson
import Data.IORef (newIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Test.Hspec

import Network.MCP.Server.Logging
import Network.MCP.Session (RequestHandler, Session (..))
import Network.MCP.Types
  ( Implementation (..)
  , LoggingLevel (..)
  , ProtocolVersion (..)
  )
import Network.MCP.Types.Capabilities
  ( ClientCapabilities (..)
  , NegotiatedCapabilities (..)
  , ServerCapabilities (..)
  )

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Create a stub session that counts notification/message calls.
makeStubSession :: IO (Session, IO Int)
makeStubSession = do
  countRef <- newTVarIO (0 :: Int)
  handlersRef <- newIORef (HM.empty :: HM.HashMap T.Text RequestHandler)
  let session = Session
        { sessionProtocolVersion = ProtocolVersion "2025-03-26"
        , sessionPeerInfo = Implementation "test" "0.1" Nothing Nothing
        , sessionCapabilities = NegotiatedCapabilities
            (ClientCapabilities Nothing Nothing Nothing Nothing Nothing)
            (ServerCapabilities Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
        , sessionInstructions = Nothing
        , sessionRequest = \_ _ _ -> pure (Right (Aeson.object []))
        , sessionNotify = \method _ ->
            if method == "notifications/message"
              then atomically (modifyTVar' countRef (+ 1))
              else pure ()
        , sessionCancel = \_ _ -> pure ()
        , sessionOnRequest = \method handler ->
            atomicModifyIORef' handlersRef (\m -> (HM.insert method handler m, ()))
        , sessionOnNotification = \_ _ -> pure ()
        , sessionClose = pure ()
        , sessionOnClose = \_ -> pure ()
        }
  pure (session, readTVarIO countRef)

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = describe "Logging feature" $ do
  it "newLoggingFeature with default config creates feature" $ do
    lf <- newLoggingFeature defaultLoggingConfig
    let logger = getLogger lf
    level <- logger.minimumLevel
    level `shouldBe` LevelWarning

  it "emit does not send when level is below minimum" $ do
    lf <- newLoggingFeature defaultLoggingConfig
    (session, getCount) <- makeStubSession
    attach lf session
    let logger = getLogger lf
    logger.emit LevelDebug (Aeson.String "test") Nothing
    count <- getCount
    count `shouldBe` 0

  it "emit sends when level meets minimum" $ do
    lf <- newLoggingFeature defaultLoggingConfig
    (session, getCount) <- makeStubSession
    attach lf session
    let logger = getLogger lf
    logger.emit LevelWarning (Aeson.String "test") Nothing
    count <- getCount
    count `shouldBe` 1

  it "rate limiter: emitting more messages than tokens delivers at most initial tokens + tolerance" $ do
    -- 10 tokens/second, bucket starts with 10 tokens
    lf <- newLoggingFeature (LoggingConfig { loggingMaxRate = Just 10 })
    (session, getCount) <- makeStubSession
    attach lf session
    let logger = getLogger lf
    -- Emit 20 messages at LevelWarning instantly
    replicateM_ 20 $ logger.emit LevelWarning (Aeson.String "msg") Nothing
    count <- getCount
    -- Should deliver at most 10 initial tokens + small tolerance for elapsed time
    count `shouldSatisfy` (<= 12)
    -- But should deliver at least some messages (the initial tokens)
    count `shouldSatisfy` (> 0)

  it "rate limiter disabled: Nothing allows unlimited messages" $ do
    lf <- newLoggingFeature (LoggingConfig { loggingMaxRate = Nothing })
    (session, getCount) <- makeStubSession
    attach lf session
    let logger = getLogger lf
    replicateM_ 50 $ logger.emit LevelWarning (Aeson.String "msg") Nothing
    count <- getCount
    count `shouldBe` 50

  it "detach stops notifications" $ do
    lf <- newLoggingFeature (LoggingConfig { loggingMaxRate = Nothing })
    (session, getCount) <- makeStubSession
    attach lf session
    detach lf
    let logger = getLogger lf
    logger.emit LevelWarning (Aeson.String "test") Nothing
    count <- getCount
    count `shouldBe` 0
