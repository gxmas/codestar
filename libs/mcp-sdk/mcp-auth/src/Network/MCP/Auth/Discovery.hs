module Network.MCP.Auth.Discovery
  ( AuthServerMetadata (..)
  , discoverMetadata
  , fallbackMetadata
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as Status
import Network.URI (parseURI, uriAuthority, uriPort, uriRegName, uriScheme)

-- | OAuth 2.0 Authorization Server Metadata (RFC 8414).
data AuthServerMetadata = AuthServerMetadata
  { asmIssuer :: !Text
  , asmAuthorizationEndpoint :: !Text
  , asmTokenEndpoint :: !Text
  , asmRegistrationEndpoint :: !(Maybe Text)
  , asmJwksUri :: !(Maybe Text)
  , asmScopesSupported :: !(Maybe [Text])
  , asmResponseTypesSupported :: !(Maybe [Text])
  , asmGrantTypesSupported :: !(Maybe [Text])
  , asmCodeChallengeMethodsSupported :: !(Maybe [Text])
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AuthServerMetadata where
  parseJSON = Aeson.withObject "AuthServerMetadata" $ \o ->
    AuthServerMetadata
      <$> o .:  "issuer"
      <*> o .:  "authorization_endpoint"
      <*> o .:  "token_endpoint"
      <*> o .:? "registration_endpoint"
      <*> o .:? "jwks_uri"
      <*> o .:? "scopes_supported"
      <*> o .:? "response_types_supported"
      <*> o .:? "grant_types_supported"
      <*> o .:? "code_challenge_methods_supported"

-- | Derive the authorization base URL (origin) from a URL by discarding
-- the path component. E.g. "https://api.example.com/v1/mcp" →
-- "https://api.example.com".
originOf :: Text -> Either Text Text
originOf url =
  case parseURI (T.unpack url) of
    Nothing -> Left ("Invalid URL: " <> url)
    Just uri ->
      case uriAuthority uri of
        Nothing -> Left ("URL has no authority: " <> url)
        Just auth ->
          let scheme = uriScheme uri  -- includes trailing ":"
              host   = uriRegName auth
              port   = uriPort auth   -- "" or ":NNN"
          in Right (T.pack (scheme <> "//" <> host <> port))

-- | Build fallback AuthServerMetadata using the default endpoint paths
-- (/authorize, /token, /register) relative to the authorization base URL.
-- Called when the discovery document is not available.
fallbackMetadata :: Text -> Either Text AuthServerMetadata
fallbackMetadata baseUrl =
  case originOf baseUrl of
    Left err -> Left err
    Right origin -> Right AuthServerMetadata
      { asmIssuer                      = origin
      , asmAuthorizationEndpoint       = origin <> "/authorize"
      , asmTokenEndpoint               = origin <> "/token"
      , asmRegistrationEndpoint        = Just (origin <> "/register")
      , asmJwksUri                     = Nothing
      , asmScopesSupported             = Nothing
      , asmResponseTypesSupported      = Nothing
      , asmGrantTypesSupported         = Nothing
      , asmCodeChallengeMethodsSupported = Nothing
      }

-- | Fetch OAuth 2.0 Authorization Server Metadata from the MCP server's
-- origin (RFC 8414). On discovery failure (non-200 or parse error), falls
-- back to default endpoint paths per the MCP spec MUST requirement.
discoverMetadata
  :: HTTP.Manager
  -> Text   -- ^ MCP server base URL
  -> Text   -- ^ MCP protocol version (e.g. "2025-03-26"), sent as
            --   MCP-Protocol-Version header per spec SHOULD
  -> IO (Either Text AuthServerMetadata)
discoverMetadata manager baseUrl protocolVersion =
  case originOf baseUrl of
    Left err -> pure (Left err)
    Right origin -> do
      let discoveryUrl = T.unpack origin <> "/.well-known/oauth-authorization-server"
      result <- try $ do
        req0 <- HTTP.parseRequest discoveryUrl
        let req = req0
              { HTTP.requestHeaders =
                  [ ("Accept", "application/json")
                  , ("MCP-Protocol-Version", TE.encodeUtf8 protocolVersion)
                  ]
              }
        HTTP.httpLbs req manager
        :: IO (Either SomeException (HTTP.Response LBS.ByteString))
      case result of
        Left _ ->
          -- Network error: fall back to default endpoints
          pure (fallbackMetadata baseUrl)
        Right resp ->
          if Status.statusIsSuccessful (HTTP.responseStatus resp)
            then case Aeson.eitherDecode' (HTTP.responseBody resp) of
              Right meta -> pure (Right meta)
              Left _     -> pure (fallbackMetadata baseUrl)
            else
              -- Non-200 (including 404): fall back to default endpoints
              pure (fallbackMetadata baseUrl)
