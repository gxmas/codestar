module Network.MCP.Auth.Registration
  ( ClientMetadata (..)
  , defaultClientMetadata
  , ClientRegistration (..)
  , registerClient
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as Status

-- | Client metadata sent to the registration endpoint (RFC 7591).
data ClientMetadata = ClientMetadata
  { cmRedirectUris :: ![Text]
  , cmClientName :: !(Maybe Text)
  , cmTokenEndpointAuthMethod :: !(Maybe Text)
  , cmGrantTypes :: !(Maybe [Text])
  , cmResponseTypes :: !(Maybe [Text])
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON ClientMetadata where
  toJSON cm =
    Aeson.object $
      ["redirect_uris" .= cm.cmRedirectUris]
        ++ maybe [] (\v -> ["client_name" .= v]) cm.cmClientName
        ++ maybe [] (\v -> ["token_endpoint_auth_method" .= v]) cm.cmTokenEndpointAuthMethod
        ++ maybe [] (\v -> ["grant_types" .= v]) cm.cmGrantTypes
        ++ maybe [] (\v -> ["response_types" .= v]) cm.cmResponseTypes

-- | Default client metadata for MCP clients (authorization_code + PKCE,
-- no client secret).
defaultClientMetadata :: [Text] -> ClientMetadata
defaultClientMetadata redirectUris =
  ClientMetadata
    { cmRedirectUris = redirectUris
    , cmClientName = Just "MCP Client"
    , cmTokenEndpointAuthMethod = Just "none" -- PKCE public client
    , cmGrantTypes = Just ["authorization_code"]
    , cmResponseTypes = Just ["code"]
    }

-- | Registration response from the authorization server.
data ClientRegistration = ClientRegistration
  { crClientId :: !Text
  , crClientSecret :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON ClientRegistration where
  parseJSON = Aeson.withObject "ClientRegistration" $ \o ->
    ClientRegistration
      <$> o .: "client_id"
      <*> o .:? "client_secret"

-- | Register a client dynamically with the authorization server (RFC 7591).
registerClient ::
  HTTP.Manager ->
  -- | registration_endpoint URL
  Text ->
  ClientMetadata ->
  IO (Either Text ClientRegistration)
registerClient manager endpoint meta = do
  let body = Aeson.encode meta
  result <-
    try
      ( do
          req0 <- HTTP.parseRequest (T.unpack endpoint)
          let req =
                req0
                  { HTTP.method = "POST"
                  , HTTP.requestBody = HTTP.RequestBodyLBS body
                  , HTTP.requestHeaders =
                      [ ("Content-Type", "application/json")
                      , ("Accept", "application/json")
                      ]
                  }
          HTTP.httpLbs req manager
      ) ::
      IO (Either SomeException (HTTP.Response LBS.ByteString))
  case result of
    Left ex -> pure (Left (T.pack ("Registration request failed: " <> show ex)))
    Right resp ->
      if not (Status.statusIsSuccessful (HTTP.responseStatus resp))
        then
          pure
            ( Left
                ( "Registration HTTP error: "
                    <> T.pack (show (Status.statusCode (HTTP.responseStatus resp)))
                )
            )
        else case Aeson.eitherDecode' (HTTP.responseBody resp) of
          Left err -> pure (Left (T.pack ("Registration parse error: " <> err)))
          Right reg -> pure (Right reg)
