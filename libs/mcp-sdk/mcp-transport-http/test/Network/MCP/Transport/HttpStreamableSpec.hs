{-# OPTIONS_GHC -Wno-orphans #-}

module Network.MCP.Transport.HttpStreamableSpec (spec) where

-- Note: shtSessionId TVar is not exported, so session ID storage
-- is tested indirectly via header forwarding on subsequent requests.
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status200, status202, status400, status401, status404, status405, status500)
import Network.Wai (Application)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import qualified Streaming.Prelude as SP
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Network.MCP.Transport
  ( Transport (..), TransportError (..), TransportErrorKind (..) )
import Network.MCP.Transport.Http.Streamable (StreamableHttpTransport, new, newWithAuth)
import Network.MCP.Types

------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------

shortText :: Gen T.Text
shortText = T.pack <$> listOf1 (elements ['a' .. 'z'])

instance Arbitrary RequestId where
  arbitrary = RequestId <$> oneof [Left <$> shortText, Right . abs <$> arbitrary]

instance Arbitrary RPCError where
  arbitrary = RPCError <$> arbitrary <*> shortText <*> pure Nothing

instance Arbitrary MCPMessage where
  arbitrary =
    oneof
      [ MCPRequest <$> (JSONRPCRequest <$> arbitrary <*> shortText <*> pure Nothing <*> pure Nothing)
      , MCPNotification <$> (JSONRPCNotification <$> shortText <*> pure Nothing <*> pure Nothing)
      , MCPResult <$> (JSONRPCResult <$> arbitrary <*> pure (Aeson.Bool True) <*> pure Nothing)
      , MCPError <$> (JSONRPCError <$> liftArbitrary arbitrary <*> arbitrary)
      ]

------------------------------------------------------------------------
-- Test helpers
------------------------------------------------------------------------

-- | Build a simple ping request MCPMessage with the given integer id.
mkPing :: Int -> MCPMessage
mkPing n =
  MCPRequest
    JSONRPCRequest
      { requestId = RequestId (Right n)
      , requestMethod = "ping"
      , requestParams = Nothing
      , requestMeta = Nothing
      }

-- | Drain the first item from a transport's message stream with a timeout.
-- Returns Nothing on timeout.
receiveOne :: StreamableHttpTransport -> IO (Maybe (Either TransportError MCPMessage))
receiveOne t = timeout 5_000_000 $ do
  stream <- messages t
  items <- SP.toList_ (SP.take 1 stream)
  case items of
    [x] -> pure x
    _   -> error "receiveOne: unexpected item count"

-- | Drain the first n items from a transport's message stream with a timeout.
receiveN :: Int -> StreamableHttpTransport -> IO (Maybe [Either TransportError MCPMessage])
receiveN n t = timeout 5_000_000 $ do
  stream <- messages t
  SP.toList_ (SP.take n stream)

-- | WAI application that responds with a JSON-encoded MCPMessage.
jsonApp :: MCPMessage -> Application
jsonApp msg _req respond =
  respond $ Wai.responseLBS status200
    [("Content-Type", "application/json")]
    (Aeson.encode msg)

-- | WAI application that responds with an SSE stream containing
-- the given MCPMessages as event: message events.
sseApp :: [MCPMessage] -> Application
sseApp msgs _req respond =
  respond $
    Wai.responseStream status200 [("Content-Type", "text/event-stream")] $
      \write flush -> do
        mapM_ (\msg -> do
          write (Builder.byteString "event: message\ndata: "
              <> Builder.lazyByteString (Aeson.encode msg)
              <> Builder.byteString "\n\n")
          flush
          ) msgs

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = do
  -- ---- Unit tests (no server) ----------------------------------------

  describe "send on closed transport" $ do
    it "returns Left TransportClosed" $ do
      t <- new "http://localhost:0/unused"
      close t
      result <- send t (mkPing 1)
      case result of
        Left err -> err.transportErrorKind `shouldBe` TransportClosed
        Right () -> expectationFailure "expected Left TransportClosed"

  describe "close idempotency" $ do
    it "calling close twice does not deadlock" $ do
      t <- new "http://localhost:0/unused"
      r <- timeout 2_000_000 $ do
        close t
        close t
      r `shouldBe` Just ()

  -- ---- Integration tests (mock Warp server) --------------------------

  describe "JSON response" $ do
    it "server application/json response delivers message via messages stream" $ do
      let expected = mkPing 42
      testWithApplication (pure (jsonApp expected)) $ \port -> do
        t <- new ("http://localhost:" <> T.pack (show port) <> "/")
        _ <- send t (mkPing 1)
        result <- receiveOne t
        close t
        case result of
          Just (Right msg) -> msg `shouldBe` expected
          Just (Left err)  -> expectationFailure ("unexpected error: " <> show err)
          Nothing          -> expectationFailure "timed out waiting for message"

  describe "SSE stream response" $ do
    it "server text/event-stream response delivers multiple messages in order" $ do
      let msg1 = mkPing 10
          msg2 = mkPing 20
      testWithApplication (pure (sseApp [msg1, msg2])) $ \port -> do
        t <- new ("http://localhost:" <> T.pack (show port) <> "/")
        _ <- send t (mkPing 1)
        result <- receiveN 2 t
        close t
        case result of
          Just items -> items `shouldBe` [Right msg1, Right msg2]
          Nothing    -> expectationFailure "timed out waiting for messages"

  describe "Session ID stored and forwarded" $ do
    it "Mcp-Session-Id from server is stored and sent in subsequent requests" $ do
      capturedHeaders <- newIORef Nothing
      callCount <- newIORef (0 :: Int)
      let expected = mkPing 42
          app :: Application
          app req respond
            | Wai.requestMethod req == "GET" =
                respond $ Wai.responseLBS status405 [] ""
            | otherwise = do
                n <- atomicModifyIORef' callCount (\x -> (x + 1, x))
                case n of
                  0 ->
                    -- First POST: set session ID
                    respond $ Wai.responseLBS status200
                      [ ("Content-Type", "application/json")
                      , ("Mcp-Session-Id", "sess-abc")
                      ]
                      (Aeson.encode expected)
                  _ -> do
                    -- Subsequent POSTs: capture the session ID header
                    let hdr = lookup "Mcp-Session-Id" (Wai.requestHeaders req)
                    writeIORef capturedHeaders (TE.decodeUtf8Lenient <$> hdr)
                    respond $ Wai.responseLBS status200
                      [("Content-Type", "application/json")]
                      (Aeson.encode expected)
      testWithApplication (pure app) $ \port -> do
        t <- new ("http://localhost:" <> T.pack (show port) <> "/")
        -- First send: establishes session
        _ <- send t (mkPing 1)
        _ <- receiveOne t
        -- Second send: should forward session ID
        _ <- send t (mkPing 2)
        _ <- receiveOne t
        close t
        captured <- readIORef capturedHeaders
        captured `shouldBe` Just "sess-abc"

  describe "Non-2xx status" $ do
    it "server 500 response surfaces as TransportIoError in messages" $ do
      let app :: Application
          app _req respond =
            respond $ Wai.responseLBS status500 [] "internal error"
      testWithApplication (pure app) $ \port -> do
        t <- new ("http://localhost:" <> T.pack (show port) <> "/")
        _ <- send t (mkPing 1)
        result <- receiveOne t
        close t
        case result of
          Just (Left err) -> err.transportErrorKind `shouldBe` TransportIoError
          Just (Right msg) -> expectationFailure ("expected error, got message: " <> show msg)
          Nothing -> expectationFailure "timed out waiting for error"

  -- ---- Property test -------------------------------------------------

  describe "Codec roundtrip through JSON response" $ do
    prop "any MCPMessage echoed by server arrives unchanged" $ \(msg :: MCPMessage) ->
      ioProperty $ do
        testWithApplication (pure (jsonApp msg)) $ \port -> do
          t <- new ("http://localhost:" <> T.pack (show port) <> "/")
          _ <- send t (mkPing 1)
          result <- receiveOne t
          close t
          case result of
            Just (Right received) -> pure (received === msg)
            Just (Left err) -> pure (counterexample ("transport error: " <> show err) (property False))
            Nothing -> pure (counterexample "timed out" (property False))

  -- ---- Spec compliance fixes -----------------------------------------

  describe "spec compliance fixes" $ do

    -- Fix 1: 202 Accepted is silently ignored
    describe "202 Accepted" $ do
      it "send returns Right () and no message is enqueued" $ do
        let app :: Application
            app _req respond =
              respond $ Wai.responseLBS status202 [] ""
        testWithApplication (pure app) $ \port -> do
          t <- new ("http://localhost:" <> T.pack (show port) <> "/")
          result <- send t (mkPing 1)
          result `shouldBe` Right ()
          -- Nothing should appear in the messages stream within a short window
          drained <- timeout 500_000 $ do
            stream <- messages t
            SP.toList_ (SP.take 1 stream)
          close t
          drained `shouldBe` Nothing

    -- Fix 2: 404 surfaces as TransportSessionExpired
    describe "404 Not Found" $ do
      it "surfaces as TransportSessionExpired error" $ do
        let app :: Application
            app _req respond =
              respond $ Wai.responseLBS status404 [] "not found"
        testWithApplication (pure app) $ \port -> do
          t <- new ("http://localhost:" <> T.pack (show port) <> "/")
          _ <- send t (mkPing 1)
          result <- receiveOne t
          close t
          case result of
            Just (Left err) -> err.transportErrorKind `shouldBe` TransportSessionExpired
            Just (Right msg) -> expectationFailure ("expected error, got message: " <> show msg)
            Nothing -> expectationFailure "timed out waiting for error"

      it "clears the stored session ID so subsequent requests omit the header" $ do
        capturedHeaders <- newIORef Nothing
        callCount <- newIORef (0 :: Int)
        let app :: Application
            app req respond
              -- Return 405 to GET requests to stop the background GET stream loop
              -- without interfering with the POST request counter.
              | Wai.requestMethod req == "GET" =
                  respond $ Wai.responseLBS status405 [] ""
              | otherwise = do
                  n <- atomicModifyIORef' callCount (\x -> (x + 1, x))
                  case n of
                    0 ->
                      -- First POST: establish session
                      respond $ Wai.responseLBS status200
                        [ ("Content-Type", "application/json")
                        , ("Mcp-Session-Id", "sess-abc")
                        ]
                        (Aeson.encode (mkPing 42))
                    1 ->
                      -- Second POST: 404 to expire the session
                      respond $ Wai.responseLBS status404 [] "not found"
                    _ -> do
                      -- Third POST: capture whether session header is present
                      let hdr = lookup "Mcp-Session-Id" (Wai.requestHeaders req)
                      writeIORef capturedHeaders (TE.decodeUtf8Lenient <$> hdr)
                      respond $ Wai.responseLBS status200
                        [("Content-Type", "application/json")]
                        (Aeson.encode (mkPing 99))
        testWithApplication (pure app) $ \port -> do
          t <- new ("http://localhost:" <> T.pack (show port) <> "/")
          -- 1st send: establishes session via Mcp-Session-Id header
          _ <- send t (mkPing 1)
          _ <- receiveOne t
          -- 2nd send: gets 404, session should be cleared
          _ <- send t (mkPing 2)
          _ <- receiveOne t  -- drain the TransportSessionExpired error
          -- 3rd send: should NOT carry Mcp-Session-Id
          _ <- send t (mkPing 3)
          _ <- receiveOne t
          close t
          captured <- readIORef capturedHeaders
          captured `shouldBe` Nothing

    -- Fix 3: Batch JSON response decoded
    describe "batch JSON array response" $ do
      prop "two MCPMessages returned as JSON array are delivered individually in order" $
        \(msg1 :: MCPMessage, msg2 :: MCPMessage) ->
          ioProperty $ do
            let app :: Application
                app _req respond =
                  respond $ Wai.responseLBS status200
                    [("Content-Type", "application/json")]
                    (Aeson.encode [msg1, msg2])
            testWithApplication (pure app) $ \port -> do
              t <- new ("http://localhost:" <> T.pack (show port) <> "/")
              _ <- send t (mkPing 1)
              result <- receiveN 2 t
              close t
              case result of
                Just items -> pure (items === [Right msg1, Right msg2])
                Nothing -> pure (counterexample "timed out waiting for batch messages" (property False))

    -- Gap 17: 400 Bad Request produces TransportProtocolError
    describe "400 Bad Request" $ do
      it "surfaces as TransportProtocolError (not TransportSessionExpired)" $ do
        let app :: Application
            app _req respond =
              respond $ Wai.responseLBS status400 [] "bad request"
        testWithApplication (pure app) $ \port -> do
          t <- new ("http://localhost:" <> T.pack (show port) <> "/")
          _ <- send t (mkPing 1)
          result <- receiveOne t
          close t
          case result of
            Just (Left err) -> err.transportErrorKind `shouldBe` TransportProtocolError
            Just (Right msg) -> expectationFailure ("expected error, got message: " <> show msg)
            Nothing -> expectationFailure "timed out waiting for error"

      it "does not clear session ID on 400" $ do
        capturedHeaders <- newIORef Nothing
        callCount <- newIORef (0 :: Int)
        let app :: Application
            app req respond
              | Wai.requestMethod req == "GET" =
                  respond $ Wai.responseLBS status405 [] ""
              | otherwise = do
                  n <- atomicModifyIORef' callCount (\x -> (x + 1, x))
                  case n of
                    0 ->
                      -- First POST: establish session
                      respond $ Wai.responseLBS status200
                        [ ("Content-Type", "application/json")
                        , ("Mcp-Session-Id", "sess-abc")
                        ]
                        (Aeson.encode (mkPing 42))
                    1 ->
                      -- Second POST: 400
                      respond $ Wai.responseLBS status400 [] "bad request"
                    _ -> do
                      -- Third POST: capture whether session header is still present
                      let hdr = lookup "Mcp-Session-Id" (Wai.requestHeaders req)
                      writeIORef capturedHeaders (TE.decodeUtf8Lenient <$> hdr)
                      respond $ Wai.responseLBS status200
                        [("Content-Type", "application/json")]
                        (Aeson.encode (mkPing 99))
        testWithApplication (pure app) $ \port -> do
          t <- new ("http://localhost:" <> T.pack (show port) <> "/")
          -- 1st send: establishes session
          _ <- send t (mkPing 1)
          _ <- receiveOne t
          -- 2nd send: gets 400
          _ <- send t (mkPing 2)
          _ <- receiveOne t  -- drain the TransportProtocolError
          -- 3rd send: session ID should still be present
          _ <- send t (mkPing 3)
          _ <- receiveOne t
          close t
          captured <- readIORef capturedHeaders
          captured `shouldBe` Just "sess-abc"

    -- Gap 10: Session DELETE on close
    describe "session DELETE on close" $ do
      it "close sends DELETE with Mcp-Session-Id when session is established" $ do
        deleteSessionId <- newIORef (Nothing :: Maybe BS.ByteString)
        gotDelete <- newIORef False
        callCount <- newIORef (0 :: Int)
        let app :: Application
            app req respond = do
              when (Wai.requestMethod req == "DELETE") $ do
                writeIORef gotDelete True
                let sid = lookup "Mcp-Session-Id" (Wai.requestHeaders req)
                writeIORef deleteSessionId sid
              n <- atomicModifyIORef' callCount (\x -> (x + 1, x))
              case n of
                0 ->
                  -- First POST: establish session
                  respond $ Wai.responseLBS status200
                    [ ("Content-Type", "application/json")
                    , ("Mcp-Session-Id", "sess-xyz")
                    ]
                    (Aeson.encode (mkPing 42))
                _ ->
                  -- DELETE and any others: 200
                  respond $ Wai.responseLBS status200 [] ""
        testWithApplication (pure app) $ \port -> do
          t <- new ("http://localhost:" <> T.pack (show port) <> "/")
          _ <- send t (mkPing 1)
          _ <- receiveOne t
          close t
          -- Give fire-and-forget DELETE a moment to arrive
          threadDelay 500_000
          didDelete <- readIORef gotDelete
          didDelete `shouldBe` True
          sid <- readIORef deleteSessionId
          sid `shouldBe` Just "sess-xyz"

      it "close does not send DELETE when no session ID was established" $ do
        requestCount <- newIORef (0 :: Int)
        let app :: Application
            app _req respond = do
              atomicModifyIORef' requestCount (\x -> (x + 1, ()))
              respond $ Wai.responseLBS status200 [] ""
        testWithApplication (pure app) $ \port -> do
          t <- new ("http://localhost:" <> T.pack (show port) <> "/")
          close t
          threadDelay 500_000
          count <- readIORef requestCount
          count `shouldBe` 0

      it "close is still safe when DELETE fails (fire-and-forget)" $ do
        let app :: Application
            app req respond
              | Wai.requestMethod req == "DELETE" =
                  respond $ Wai.responseLBS status405 [] "method not allowed"
              | otherwise =
                  respond $ Wai.responseLBS status200
                    [ ("Content-Type", "application/json")
                    , ("Mcp-Session-Id", "sess-del")
                    ]
                    (Aeson.encode (mkPing 42))
        testWithApplication (pure app) $ \port -> do
          t <- new ("http://localhost:" <> T.pack (show port) <> "/")
          _ <- send t (mkPing 1)
          _ <- receiveOne t
          result <- timeout 3_000_000 (close t)
          result `shouldBe` Just ()

  -- ---- GET SSE stream (Gap 2) -----------------------------------------

  describe "GET SSE stream (Gap 2)" $ do

    it "GET request carries Mcp-Session-Id" $ do
      capturedSid <- newIORef (Nothing :: Maybe BS.ByteString)
      gotGet <- newIORef False
      let app :: Application
          app req respond
            | Wai.requestMethod req == "GET" = do
                let sid = lookup "Mcp-Session-Id" (Wai.requestHeaders req)
                writeIORef capturedSid sid
                writeIORef gotGet True
                -- Respond with 405 to stop reconnect loop
                respond $ Wai.responseLBS status405 [] "not allowed"
            | otherwise =
                respond $ Wai.responseLBS status200
                  [ ("Content-Type", "application/json")
                  , ("Mcp-Session-Id", "sess-get-test")
                  ]
                  (Aeson.encode (mkPing 42))
      testWithApplication (pure app) $ \port -> do
        t <- new ("http://localhost:" <> T.pack (show port) <> "/")
        _ <- send t (mkPing 1)
        _ <- receiveOne t
        -- Wait for the background GET thread to fire
        r <- timeout 5_000_000 $ do
          let waitForGet = do
                got <- readIORef gotGet
                if got then pure ()
                else threadDelay 50_000 >> waitForGet
          waitForGet
        close t
        r `shouldBe` Just ()
        sid <- readIORef capturedSid
        sid `shouldBe` Just "sess-get-test"

    it "server-pushed notification arrives in messages stream" $ do
      let pushNotif = MCPNotification
            JSONRPCNotification
              { notificationMethod = "notifications/test"
              , notificationParams = Nothing
              , notificationMeta = Nothing
              }
          postResp = mkPing 42
          app :: Application
          app req respond
            | Wai.requestMethod req == "GET" =
                respond $ Wai.responseStream status200
                  [("Content-Type", "text/event-stream")]
                  $ \write flush -> do
                      write (Builder.byteString "event: message\ndata: "
                          <> Builder.lazyByteString (Aeson.encode pushNotif)
                          <> Builder.byteString "\n\n")
                      flush
            | otherwise =
                respond $ Wai.responseLBS status200
                  [ ("Content-Type", "application/json")
                  , ("Mcp-Session-Id", "sess-push")
                  ]
                  (Aeson.encode postResp)
      testWithApplication (pure app) $ \port -> do
        t <- new ("http://localhost:" <> T.pack (show port) <> "/")
        _ <- send t (mkPing 1)
        result <- receiveN 2 t
        close t
        case result of
          Just items -> do
            items `shouldContain` [Right postResp]
            items `shouldContain` [Right pushNotif]
          Nothing -> expectationFailure "timed out waiting for messages"

    it "Last-Event-ID sent on reconnect" $ do
      capturedLastId <- newIORef (Nothing :: Maybe BS.ByteString)
      getCount <- newIORef (0 :: Int)
      let pushNotif = MCPNotification
            JSONRPCNotification
              { notificationMethod = "notifications/reconnect"
              , notificationParams = Nothing
              , notificationMeta = Nothing
              }
          app :: Application
          app req respond
            | Wai.requestMethod req == "GET" = do
                n <- atomicModifyIORef' getCount (\x -> (x + 1, x))
                case n of
                  0 ->
                    -- First GET: send one SSE event with id, then EOF
                    respond $ Wai.responseStream status200
                      [("Content-Type", "text/event-stream")]
                      $ \write flush -> do
                          write (Builder.byteString "id: evt-42\nevent: message\ndata: "
                              <> Builder.lazyByteString (Aeson.encode pushNotif)
                              <> Builder.byteString "\n\n")
                          flush
                  _ -> do
                    -- Second GET: capture Last-Event-ID, respond 405 to stop
                    let lastId = lookup "Last-Event-ID" (Wai.requestHeaders req)
                    writeIORef capturedLastId lastId
                    respond $ Wai.responseLBS status405 [] "not allowed"
            | otherwise =
                respond $ Wai.responseLBS status200
                  [ ("Content-Type", "application/json")
                  , ("Mcp-Session-Id", "sess-lastid")
                  ]
                  (Aeson.encode (mkPing 42))
      testWithApplication (pure app) $ \port -> do
        t <- new ("http://localhost:" <> T.pack (show port) <> "/")
        _ <- send t (mkPing 1)
        _ <- receiveOne t
        -- Wait for first GET + reconnect + second GET
        r <- timeout 5_000_000 $ do
          let waitForSecondGet = do
                n <- readIORef getCount
                if n >= 2 then pure ()
                else threadDelay 50_000 >> waitForSecondGet
          waitForSecondGet
        close t
        r `shouldBe` Just ()
        lastId <- readIORef capturedLastId
        lastId `shouldBe` Just "evt-42"

    it "405 to GET does not cause error or retry" $ do
      getCount <- newIORef (0 :: Int)
      let app :: Application
          app req respond
            | Wai.requestMethod req == "GET" = do
                atomicModifyIORef' getCount (\x -> (x + 1, ()))
                respond $ Wai.responseLBS status405 [] "not allowed"
            | otherwise =
                respond $ Wai.responseLBS status200
                  [ ("Content-Type", "application/json")
                  , ("Mcp-Session-Id", "sess-405")
                  ]
                  (Aeson.encode (mkPing 42))
      testWithApplication (pure app) $ \port -> do
        t <- new ("http://localhost:" <> T.pack (show port) <> "/")
        _ <- send t (mkPing 1)
        _ <- receiveOne t
        -- Give GET thread time to fire and be rejected
        threadDelay 300_000
        -- No transport error should be in the messages stream
        drained <- timeout 100_000 $ do
          stream <- messages t
          SP.toList_ (SP.take 1 stream)
        close t
        drained `shouldBe` Nothing
        count <- readIORef getCount
        count `shouldBe` 1

  -- ---- Bearer token auth (2D) ------------------------------------------

  describe "bearer token auth (2D)" $ do

    it "Authorization header is sent on POST when token is available" $ do
      capturedAuth <- newIORef (Nothing :: Maybe BS.ByteString)
      callCount <- newIORef (0 :: Int)
      let app :: Application
          app req respond
            | Wai.requestMethod req == "GET" =
                respond $ Wai.responseLBS status405 [] ""
            | otherwise = do
                n <- atomicModifyIORef' callCount (\x -> (x + 1, x))
                case n of
                  0 ->
                    -- First POST: return 401 to trigger refresh
                    respond $ Wai.responseLBS status401 [] "unauthorized"
                  _ -> do
                    -- Retry POST: capture Authorization header
                    let auth = lookup "Authorization" (Wai.requestHeaders req)
                    writeIORef capturedAuth auth
                    respond $ Wai.responseLBS status200
                      [("Content-Type", "application/json")]
                      (Aeson.encode (mkPing 42))
      testWithApplication (pure app) $ \port -> do
        t <- newWithAuth
          ("http://localhost:" <> T.pack (show port) <> "/")
          (pure (Right "test-token"))
        _ <- send t (mkPing 1)
        _ <- receiveOne t
        close t
        auth <- readIORef capturedAuth
        auth `shouldBe` Just "Bearer test-token"

    it "no Authorization header when token is Nothing initially" $ do
      capturedAuth <- newIORef (Nothing :: Maybe BS.ByteString)
      let app :: Application
          app req respond
            | Wai.requestMethod req == "GET" =
                respond $ Wai.responseLBS status405 [] ""
            | otherwise = do
                -- Capture Authorization header from request 0 (before any 401)
                let auth = lookup "Authorization" (Wai.requestHeaders req)
                writeIORef capturedAuth auth
                respond $ Wai.responseLBS status200
                  [("Content-Type", "application/json")]
                  (Aeson.encode (mkPing 42))
      testWithApplication (pure app) $ \port -> do
        t <- newWithAuth
          ("http://localhost:" <> T.pack (show port) <> "/")
          (pure (Right "tok"))
        _ <- send t (mkPing 1)
        _ <- receiveOne t
        close t
        auth <- readIORef capturedAuth
        -- No token was set initially, so first request has no Authorization
        auth `shouldBe` Nothing

    it "401 with no auth configured surfaces as TransportIoError" $ do
      let app :: Application
          app _req respond =
            respond $ Wai.responseLBS status401 [] "unauthorized"
      testWithApplication (pure app) $ \port -> do
        t <- new ("http://localhost:" <> T.pack (show port) <> "/")
        _ <- send t (mkPing 1)
        result <- receiveOne t
        close t
        case result of
          Just (Left err) -> err.transportErrorKind `shouldBe` TransportIoError
          Just (Right msg) -> expectationFailure ("expected error, got: " <> show msg)
          Nothing -> expectationFailure "timed out"

    it "401 refresh failure surfaces as TransportIoError" $ do
      let app :: Application
          app _req respond =
            respond $ Wai.responseLBS status401 [] "unauthorized"
      testWithApplication (pure app) $ \port -> do
        t <- newWithAuth
          ("http://localhost:" <> T.pack (show port) <> "/")
          (pure (Left "no token"))
        _ <- send t (mkPing 1)
        result <- receiveOne t
        close t
        case result of
          Just (Left err) -> err.transportErrorKind `shouldBe` TransportIoError
          Just (Right msg) -> expectationFailure ("expected error, got: " <> show msg)
          Nothing -> expectationFailure "timed out"

    it "Authorization header is sent on GET stream when token is set" $ do
      capturedGetAuth <- newIORef (Nothing :: Maybe BS.ByteString)
      gotGet <- newIORef False
      callCount <- newIORef (0 :: Int)
      let app :: Application
          app req respond
            | Wai.requestMethod req == "GET" = do
                let auth = lookup "Authorization" (Wai.requestHeaders req)
                writeIORef capturedGetAuth auth
                writeIORef gotGet True
                respond $ Wai.responseLBS status405 [] "not allowed"
            | otherwise = do
                n <- atomicModifyIORef' callCount (\x -> (x + 1, x))
                case n of
                  0 ->
                    -- First POST: return 401 to trigger refresh and set token
                    respond $ Wai.responseLBS status401 [] "unauthorized"
                  _ ->
                    -- Retry POST: success with session ID to start GET stream
                    respond $ Wai.responseLBS status200
                      [ ("Content-Type", "application/json")
                      , ("Mcp-Session-Id", "sess-auth")
                      ]
                      (Aeson.encode (mkPing 42))
      testWithApplication (pure app) $ \port -> do
        t <- newWithAuth
          ("http://localhost:" <> T.pack (show port) <> "/")
          (pure (Right "get-tok"))
        _ <- send t (mkPing 1)
        _ <- receiveOne t
        -- Wait for GET thread to fire
        r <- timeout 5_000_000 $ do
          let waitForGet = do
                got <- readIORef gotGet
                if got then pure ()
                else threadDelay 50_000 >> waitForGet
          waitForGet
        close t
        r `shouldBe` Just ()
        auth <- readIORef capturedGetAuth
        auth `shouldBe` Just "Bearer get-tok"
