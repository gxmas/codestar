module Network.MCP.Transport.HttpServerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust)
import Network.HTTP.Types.Header (RequestHeaders)
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as Status
import Network.Wai (Application)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Timeout (timeout)
import Test.Hspec

import Network.MCP.Session (Session (..))
import Network.MCP.Session.Connect (connect, defaultConnectConfig)
import Network.MCP.Transport.Http.Server
  ( McpServer, McpServerConfig (..)
  , defaultMcpServerConfig, mcpApp, newMcpServer
  )
import qualified Network.MCP.Transport.Http.Streamable as Client
import Network.MCP.Types (Implementation (..), ProtocolVersion (..))

------------------------------------------------------------------------
-- Test helpers
------------------------------------------------------------------------

testServer :: (McpServerConfig -> McpServerConfig) -> IO (McpServer, Application)
testServer modify = do
  let info = Implementation "test-server" "0.1.0" Nothing Nothing
      cfg  = modify (defaultMcpServerConfig info)
  ms <- newMcpServer cfg
  pure (ms, mcpApp ms)

postJson :: Int -> LBS.ByteString -> RequestHeaders -> IO (HTTP.Response LBS.ByteString)
postJson port body extraHdrs = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  req0 <- HTTP.parseRequest ("http://localhost:" <> show port <> "/")
  let req = req0
        { HTTP.method      = "POST"
        , HTTP.requestBody = HTTP.RequestBodyLBS body
        , HTTP.requestHeaders =
            [("Content-Type", "application/json")] ++ extraHdrs
        }
  HTTP.httpLbs req manager

getReq :: Int -> RequestHeaders -> IO (HTTP.Response LBS.ByteString)
getReq port extraHdrs = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  req0 <- HTTP.parseRequest ("http://localhost:" <> show port <> "/")
  let req = req0
        { HTTP.method = "GET"
        , HTTP.requestHeaders = extraHdrs
        }
  HTTP.httpLbs req manager

deleteReq :: Int -> RequestHeaders -> IO (HTTP.Response LBS.ByteString)
deleteReq port extraHdrs = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  req0 <- HTTP.parseRequest ("http://localhost:" <> show port <> "/")
  let req = req0
        { HTTP.method = "DELETE"
        , HTTP.requestHeaders = extraHdrs
        }
  HTTP.httpLbs req manager


initRequestBody :: Int -> LBS.ByteString
initRequestBody rid = Aeson.encode $ Aeson.object
  [ "jsonrpc" .= ("2.0" :: T.Text)
  , "id"      .= rid
  , "method"  .= ("initialize" :: T.Text)
  , "params"  .= Aeson.object
      [ "protocolVersion" .= ("2025-03-26" :: T.Text)
      , "capabilities"    .= Aeson.object []
      , "clientInfo"      .= Aeson.object
          [ "name"    .= ("test-client" :: T.Text)
          , "version" .= ("0.1.0" :: T.Text)
          ]
      ]
  ]

initRequestBodyWithVersion :: Int -> T.Text -> LBS.ByteString
initRequestBodyWithVersion rid version = Aeson.encode $ Aeson.object
  [ "jsonrpc" .= ("2.0" :: T.Text)
  , "id"      .= rid
  , "method"  .= ("initialize" :: T.Text)
  , "params"  .= Aeson.object
      [ "protocolVersion" .= version
      , "capabilities"    .= Aeson.object []
      , "clientInfo"      .= Aeson.object
          [ "name"    .= ("test-client" :: T.Text)
          , "version" .= ("0.1.0" :: T.Text)
          ]
      ]
  ]

echoRequestBody :: Int -> LBS.ByteString
echoRequestBody rid = Aeson.encode $ Aeson.object
  [ "jsonrpc" .= ("2.0" :: T.Text)
  , "id"      .= rid
  , "method"  .= ("echo" :: T.Text)
  , "params"  .= Aeson.object
      [ "message" .= ("hello" :: T.Text)
      ]
  ]

-- | Extract the Mcp-Session-Id header from a response.
extractSessionId :: HTTP.Response a -> Maybe BS.ByteString
extractSessionId resp = lookup "Mcp-Session-Id" (HTTP.responseHeaders resp)

-- | Get the HTTP status code from a response.
statusCode :: HTTP.Response a -> Int
statusCode = Status.statusCode . HTTP.responseStatus

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = describe "McpServer WAI application" $ do

  -- ---- Lifecycle ----------------------------------------------------

  describe "Lifecycle" $ do

    it "POST initialize returns 200 with Mcp-Session-Id" $ do
      (_, app) <- testServer id
      testWithApplication (pure app) $ \port -> do
        resp <- postJson port (initRequestBody 1) []
        statusCode resp `shouldBe` 200
        let mSid = extractSessionId resp
        mSid `shouldSatisfy` isJust
        mSid `shouldSatisfy` (/= Just "")

    it "POST initialize returns InitializeResult with server info" $ do
      (_, app) <- testServer id
      testWithApplication (pure app) $ \port -> do
        resp <- postJson port (initRequestBody 1) []
        statusCode resp `shouldBe` 200
        let body = HTTP.responseBody resp
        case Aeson.decode body :: Maybe Aeson.Value of
          Nothing -> expectationFailure "Failed to decode response as JSON"
          Just val -> do
            -- Check result.serverInfo.name
            let mName = jsonAt val ["result", "serverInfo", "name"]
            mName `shouldBe` Just (Aeson.String "test-server")
            -- Check result.protocolVersion
            let mVersion = jsonAt val ["result", "protocolVersion"]
            mVersion `shouldBe` Just (Aeson.String "2025-03-26")

    it "POST with unknown session ID returns 404" $ do
      (_, app) <- testServer id
      testWithApplication (pure app) $ \port -> do
        -- First initialize to make sure server is alive
        _ <- postJson port (initRequestBody 1) []
        -- Now send with a made-up session ID
        resp <- postJson port (echoRequestBody 2)
          [("Mcp-Session-Id", "nonexistent-session")]
        statusCode resp `shouldBe` 404

    it "POST without session ID (non-initialize) returns 400" $ do
      (_, app) <- testServer id
      testWithApplication (pure app) $ \port -> do
        -- Send a non-initialize request with no session ID
        resp <- postJson port (echoRequestBody 1) []
        statusCode resp `shouldBe` 400

    it "DELETE terminates session; subsequent GET returns 404" $ do
      (_, app) <- testServer id
      testWithApplication (pure app) $ \port -> do
        -- Initialize to get session ID
        initResp <- postJson port (initRequestBody 1) []
        statusCode initResp `shouldBe` 200
        let Just sid = extractSessionId initResp
        -- DELETE the session
        delResp <- deleteReq port [("Mcp-Session-Id", sid)]
        statusCode delResp `shouldBe` 200
        -- GET with the same session ID should return 404
        getResp <- getReq port [("Mcp-Session-Id", sid)]
        statusCode getResp `shouldBe` 404

  -- ---- Origin validation --------------------------------------------

  describe "Origin validation" $ do

    it "Origin in allowlist is accepted" $ do
      (_, app) <- testServer (\c -> c { mscAllowedOrigins = Just ["http://localhost"] })
      testWithApplication (pure app) $ \port -> do
        resp <- postJson port (initRequestBody 1)
          [("Origin", "http://localhost")]
        statusCode resp `shouldBe` 200

    it "Origin not in allowlist is rejected with 403" $ do
      (_, app) <- testServer (\c -> c { mscAllowedOrigins = Just ["http://localhost"] })
      testWithApplication (pure app) $ \port -> do
        resp <- postJson port (initRequestBody 1)
          [("Origin", "http://evil.com")]
        statusCode resp `shouldBe` 403

    it "Missing Origin is rejected when allowlist is configured" $ do
      (_, app) <- testServer (\c -> c { mscAllowedOrigins = Just ["http://localhost"] })
      testWithApplication (pure app) $ \port -> do
        resp <- postJson port (initRequestBody 1) []
        statusCode resp `shouldBe` 403

    it "All origins accepted when no allowlist (Nothing)" $ do
      (_, app) <- testServer (\c -> c { mscAllowedOrigins = Nothing })
      testWithApplication (pure app) $ \port -> do
        resp <- postJson port (initRequestBody 1)
          [("Origin", "http://anything.com")]
        statusCode resp `shouldSatisfy` (/= 403)

  -- ---- Protocol version negotiation ---------------------------------

  describe "Protocol version negotiation" $ do

    it "Server echoes supported protocol version" $ do
      (_, app) <- testServer id
      testWithApplication (pure app) $ \port -> do
        resp <- postJson port (initRequestBodyWithVersion 1 "2024-11-05") []
        statusCode resp `shouldBe` 200
        case Aeson.decode (HTTP.responseBody resp) :: Maybe Aeson.Value of
          Nothing -> expectationFailure "Failed to decode response"
          Just val -> do
            let mVersion = jsonAt val ["result", "protocolVersion"]
            mVersion `shouldBe` Just (Aeson.String "2024-11-05")

    it "Server responds with preferred version for unsupported client version" $ do
      (_, app) <- testServer id
      testWithApplication (pure app) $ \port -> do
        resp <- postJson port (initRequestBodyWithVersion 1 "1900-01-01") []
        statusCode resp `shouldBe` 200
        case Aeson.decode (HTTP.responseBody resp) :: Maybe Aeson.Value of
          Nothing -> expectationFailure "Failed to decode response"
          Just val -> do
            let mVersion = jsonAt val ["result", "protocolVersion"]
            mVersion `shouldBe` Just (Aeson.String "2025-03-26")
            -- Should NOT be an error response
            let mError = jsonAt val ["error"]
            mError `shouldBe` Nothing

  -- ---- GET SSE ------------------------------------------------------

  describe "GET SSE" $ do

    it "GET without session ID returns 400" $ do
      (_, app) <- testServer id
      testWithApplication (pure app) $ \port -> do
        resp <- getReq port []
        statusCode resp `shouldBe` 400

    it "GET with unknown session ID returns 404" $ do
      (_, app) <- testServer id
      testWithApplication (pure app) $ \port -> do
        resp <- getReq port [("Mcp-Session-Id", "fake")]
        statusCode resp `shouldBe` 404

    it "GET returns 405 when mscSupportGetStream = False" $ do
      (_, app) <- testServer (\c -> c { mscSupportGetStream = False })
      testWithApplication (pure app) $ \port -> do
        -- Initialize first to get a session ID
        initResp <- postJson port (initRequestBody 1) []
        let Just sid = extractSessionId initResp
        resp <- getReq port [("Mcp-Session-Id", sid)]
        statusCode resp `shouldBe` 405

    it "GET with valid session ID returns 200 with text/event-stream" $ do
      (_, app) <- testServer id
      testWithApplication (pure app) $ \port -> do
        -- Initialize to get a session, then schedule a DELETE shortly
        -- after so the SSE stream terminates (unblocking readTQueue).
        initResp3 <- postJson port (initRequestBody 3) []
        let Just sid3 = extractSessionId initResp3
        -- Schedule a DELETE shortly to unblock the stream
        _ <- async $ do
          threadDelay 100_000
          _ <- deleteReq port [("Mcp-Session-Id", sid3)]
          pure ()
        -- Now issue GET — the stream should start (200 text/event-stream)
        -- and terminate shortly after the DELETE fires.
        resp <- getReq port [("Mcp-Session-Id", sid3)]
        statusCode resp `shouldBe` 200
        let ct = lookup "Content-Type" (HTTP.responseHeaders resp)
        ct `shouldSatisfy` \case
          Just v  -> "text/event-stream" `BS.isInfixOf` v
          Nothing -> False

  -- ---- Integration: client + server ---------------------------------

  describe "Integration: client + server" $ do

    it "client connect succeeds against server transport" $ do
      (_, app) <- testServer id
      testWithApplication (pure app) $ \port -> do
        t <- Client.new ("http://localhost:" <> T.pack (show port) <> "/")
        let clientInfo = Implementation "test-client" "0.1.0" Nothing Nothing
        result <- connect t (defaultConnectConfig clientInfo)
        case result of
          Left err -> expectationFailure (show err)
          Right session -> do
            session.sessionProtocolVersion `shouldBe` ProtocolVersion "2025-03-26"
            session.sessionClose

  -- ---- Handler dispatch ---------------------------------------------

  describe "Handler dispatch" $ do

    it "registered request handler is called" $ do
      handlerReady <- newEmptyMVar
      (_, app) <- testServer $ \c -> c
        { mscOnSession = \session -> do
            session.sessionOnRequest "echo" $ \params _meta ->
              pure (Right params)
            putMVar handlerReady ()
        }
      testWithApplication (pure app) $ \port -> do
        -- Initialize to get session ID
        initResp <- postJson port (initRequestBody 1) []
        statusCode initResp `shouldBe` 200
        let Just sid = extractSessionId initResp
        -- Wait for the onSession callback to register the handler
        mReady <- timeout 5_000_000 (takeMVar handlerReady)
        mReady `shouldBe` Just ()
        -- Send an echo request
        resp <- postJson port (echoRequestBody 2) [("Mcp-Session-Id", sid)]
        statusCode resp `shouldBe` 200
        case Aeson.decode (HTTP.responseBody resp) :: Maybe Aeson.Value of
          Nothing -> expectationFailure "Failed to decode response"
          Just val -> do
            -- Should be a result (not error)
            let mResult = jsonAt val ["result"]
            mResult `shouldSatisfy` isJust
            -- The result should contain our echo params
            let mMsg = jsonAt val ["result", "message"]
            mMsg `shouldBe` Just (Aeson.String "hello")

------------------------------------------------------------------------
-- JSON path helpers (minimal lens-like access for Aeson.Value)
------------------------------------------------------------------------

-- | Extract a key from an Aeson Object, or Nothing.
(^?) :: Aeson.Value -> T.Text -> Maybe Aeson.Value
(^?) (Aeson.Object o) key = KM.lookup (AesonKey.fromText key) o
(^?) _ _ = Nothing

infixl 8 ^?

-- | Nested lookup into an Aeson Value by a list of keys.
jsonAt :: Aeson.Value -> [T.Text] -> Maybe Aeson.Value
jsonAt v []     = Just v
jsonAt v (k:ks) = case v ^? k of
  Nothing -> Nothing
  Just v' -> jsonAt v' ks
