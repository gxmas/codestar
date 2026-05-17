module Network.MCP.Auth.FlowSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status400)
import Network.Wai (Application)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Network.MCP.Auth.Flow

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

sampleConfig :: Int -> AuthConfig
sampleConfig port = AuthConfig
  { acClientId = "my-client-id"
  , acClientSecret = Nothing
  , acRedirectUri = "http://localhost:9999/callback"
  , acScopes = ["openid", "profile"]
  , acAuthorizationEndpoint = "http://localhost:" <> T.pack (show port) <> "/authorize"
  , acTokenEndpoint = "http://localhost:" <> T.pack (show port) <> "/token"
  }

sampleConfigNoScope :: Int -> AuthConfig
sampleConfigNoScope port = (sampleConfig port) { acScopes = [] }

samplePkce :: PkceChallenge
samplePkce = PkceChallenge
  { pkceVerifier  = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
  , pkceChallenge = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
  , pkceMethod    = "S256"
  }

-- | Newtype for generating safe state strings (alphanumeric).
newtype SafeText = SafeText T.Text
  deriving stock (Eq, Show)

instance Arbitrary SafeText where
  arbitrary = SafeText . T.pack <$> listOf1 (elements (['a'..'z'] ++ ['0'..'9']))

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "buildAuthorizationUrl" $ do

    it "includes response_type=code" $ do
      let url = buildAuthorizationUrl (sampleConfig 0) samplePkce "state123"
      url `shouldSatisfy` T.isInfixOf "response_type=code"

    it "includes code_challenge and code_challenge_method=S256" $ do
      let url = buildAuthorizationUrl (sampleConfig 0) samplePkce "state123"
      url `shouldSatisfy` T.isInfixOf "code_challenge="
      url `shouldSatisfy` T.isInfixOf "code_challenge_method=S256"

    it "includes client_id and redirect_uri" $ do
      let url = buildAuthorizationUrl (sampleConfig 0) samplePkce "state123"
      url `shouldSatisfy` T.isInfixOf "client_id=my-client-id"
      url `shouldSatisfy` T.isInfixOf "redirect_uri="

    it "includes scope when non-empty" $ do
      let cfg = (sampleConfig 0) { acScopes = ["openid"] }
          url = buildAuthorizationUrl cfg samplePkce "state123"
      url `shouldSatisfy` T.isInfixOf "scope=openid"

    it "omits scope when empty" $ do
      let url = buildAuthorizationUrl (sampleConfigNoScope 0) samplePkce "state123"
      url `shouldSatisfy` (not . T.isInfixOf "scope=")

    prop "is a valid URL prefix (starts with authorization endpoint)" $ \(SafeText st) ->
      let cfg = sampleConfig 0
          url = buildAuthorizationUrl cfg samplePkce st
      in T.isPrefixOf cfg.acAuthorizationEndpoint url

  describe "exchangeCode" $ do

    it "sends grant_type=authorization_code" $ do
      capturedBody <- newIORef BS.empty
      let app :: Application
          app req respond = do
            body <- Wai.consumeRequestBodyStrict req
            writeIORef capturedBody (LBS.toStrict body)
            respond $ Wai.responseLBS status200
              [("Content-Type", "application/json")]
              "{\"access_token\":\"tok\",\"token_type\":\"Bearer\"}"
      mgr <- newManager defaultManagerSettings
      testWithApplication (pure app) $ \port -> do
        _ <- exchangeCode mgr (sampleConfig port) samplePkce "auth-code-123"
        body <- readIORef capturedBody
        body `shouldSatisfy` BS.isInfixOf "grant_type=authorization_code"

    it "includes code_verifier in request" $ do
      capturedBody <- newIORef BS.empty
      let app :: Application
          app req respond = do
            body <- Wai.consumeRequestBodyStrict req
            writeIORef capturedBody (LBS.toStrict body)
            respond $ Wai.responseLBS status200
              [("Content-Type", "application/json")]
              "{\"access_token\":\"tok\",\"token_type\":\"Bearer\"}"
      mgr <- newManager defaultManagerSettings
      testWithApplication (pure app) $ \port -> do
        _ <- exchangeCode mgr (sampleConfig port) samplePkce "auth-code-123"
        body <- readIORef capturedBody
        body `shouldSatisfy` BS.isInfixOf "code_verifier="

    it "returns Right TokenSet on success" $ do
      let app :: Application
          app _req respond =
            respond $ Wai.responseLBS status200
              [("Content-Type", "application/json")]
              "{\"access_token\":\"tok\",\"token_type\":\"Bearer\",\"expires_in\":3600}"
      mgr <- newManager defaultManagerSettings
      testWithApplication (pure app) $ \port -> do
        result <- exchangeCode mgr (sampleConfig port) samplePkce "auth-code-123"
        case result of
          Left err -> expectationFailure ("unexpected error: " <> T.unpack err)
          Right ts -> do
            ts.tsAccessToken `shouldBe` "tok"
            ts.tsTokenType `shouldBe` "Bearer"
            ts.tsExpiresIn `shouldBe` Just 3600

    it "returns Left on HTTP error" $ do
      let app :: Application
          app _req respond =
            respond $ Wai.responseLBS status400 [] "bad request"
      mgr <- newManager defaultManagerSettings
      testWithApplication (pure app) $ \port -> do
        result <- exchangeCode mgr (sampleConfig port) samplePkce "auth-code-123"
        case result of
          Left _  -> pure ()
          Right _ -> expectationFailure "expected Left on HTTP 400"
