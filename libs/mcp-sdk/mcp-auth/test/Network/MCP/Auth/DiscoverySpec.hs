module Network.MCP.Auth.DiscoverySpec (spec) where

import qualified Data.Aeson as Aeson
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Network.MCP.Auth.Discovery

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Build a localhost base URL from a port number.
localUrl :: Int -> T.Text
localUrl port = "http://localhost:" <> T.pack (show port) <> "/v1/mcp"

-- | A valid discovery JSON document.
validDiscoveryJson :: Int -> Aeson.Value
validDiscoveryJson port =
  let origin = "http://localhost:" <> show port
  in Aeson.object
    [ "issuer"                 Aeson..= (origin :: String)
    , "authorization_endpoint" Aeson..= (origin <> "/oauth/authorize")
    , "token_endpoint"         Aeson..= (origin <> "/oauth/token")
    ]

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "discoverMetadata" $ do

    it "returns metadata from valid discovery document" $ do
      let app :: Int -> Application
          app port _req respond =
            respond $ Wai.responseLBS status200
              [("Content-Type", "application/json")]
              (Aeson.encode (validDiscoveryJson port))
      mgr <- newManager defaultManagerSettings
      -- We need the port to build the expected JSON, so we use a two-phase
      -- approach: the app captures its own port via IORef.
      portRef <- newIORef (0 :: Int)
      let wrappedApp req respond = do
            p <- readIORef portRef
            app p req respond
      testWithApplication (pure wrappedApp) $ \port -> do
        writeIORef portRef port
        result <- discoverMetadata mgr (localUrl port) "2025-03-26"
        case result of
          Left err   -> expectationFailure ("unexpected error: " <> T.unpack err)
          Right meta -> do
            let origin = "http://localhost:" <> show port
            meta.asmIssuer `shouldBe` T.pack origin
            meta.asmAuthorizationEndpoint `shouldBe` T.pack (origin <> "/oauth/authorize")
            meta.asmTokenEndpoint `shouldBe` T.pack (origin <> "/oauth/token")

    it "falls back to default endpoints when server returns 404" $ do
      let app :: Application
          app _req respond =
            respond $ Wai.responseLBS status404 [] "not found"
      mgr <- newManager defaultManagerSettings
      testWithApplication (pure app) $ \port -> do
        result <- discoverMetadata mgr (localUrl port) "2025-03-26"
        case result of
          Left err   -> expectationFailure ("unexpected error: " <> T.unpack err)
          Right meta -> do
            meta.asmAuthorizationEndpoint `shouldSatisfy` T.isSuffixOf "/authorize"

    it "falls back when JSON is invalid" $ do
      let app :: Application
          app _req respond =
            respond $ Wai.responseLBS status200
              [("Content-Type", "application/json")]
              "this is not json"
      mgr <- newManager defaultManagerSettings
      testWithApplication (pure app) $ \port -> do
        result <- discoverMetadata mgr (localUrl port) "2025-03-26"
        case result of
          Left err   -> expectationFailure ("unexpected error: " <> T.unpack err)
          Right meta ->
            meta.asmAuthorizationEndpoint `shouldSatisfy` T.isSuffixOf "/authorize"

    it "sends MCP-Protocol-Version header" $ do
      capturedHeader <- newIORef (Nothing :: Maybe T.Text)
      let app :: Application
          app req respond = do
            let hdr = lookup "MCP-Protocol-Version" (Wai.requestHeaders req)
            writeIORef capturedHeader (TE.decodeUtf8Lenient <$> hdr)
            respond $ Wai.responseLBS status404 [] "not found"
      mgr <- newManager defaultManagerSettings
      testWithApplication (pure app) $ \port -> do
        _ <- discoverMetadata mgr (localUrl port) "2025-03-26"
        captured <- readIORef capturedHeader
        captured `shouldBe` Just "2025-03-26"

  describe "fallbackMetadata" $ do

    it "constructs correct default paths" $ do
      case fallbackMetadata "https://api.example.com/v1/mcp" of
        Left err   -> expectationFailure ("unexpected error: " <> T.unpack err)
        Right meta -> do
          meta.asmAuthorizationEndpoint `shouldBe` "https://api.example.com/authorize"
          meta.asmTokenEndpoint `shouldBe` "https://api.example.com/token"
          meta.asmRegistrationEndpoint `shouldBe` Just "https://api.example.com/register"
          meta.asmIssuer `shouldBe` "https://api.example.com"

    it "rejects invalid URL" $ do
      case fallbackMetadata "not-a-url" of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for invalid URL"
