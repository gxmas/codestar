module Network.MCP.Auth.Flow
  ( AuthConfig (..)
  , TokenSet (..)
  , buildAuthorizationUrl
  , exchangeCode
  , PkceChallenge (..)
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
import Network.HTTP.Types (renderSimpleQuery)
import qualified Network.HTTP.Types.Status as Status
import Network.MCP.Auth.Pkce (PkceChallenge (..))

-- | Configuration for the OAuth 2.1 authorization code flow.
data AuthConfig = AuthConfig
  { acClientId :: !Text
  , acClientSecret :: !(Maybe Text)
  -- ^ Nothing for public clients
  , acRedirectUri :: !Text
  , acScopes :: ![Text]
  , acAuthorizationEndpoint :: !Text
  , acTokenEndpoint :: !Text
  }
  deriving stock (Eq, Show, Generic)

-- | An OAuth token set returned from the token endpoint.
data TokenSet = TokenSet
  { tsAccessToken :: !Text
  , tsTokenType :: !Text
  , tsExpiresIn :: !(Maybe Int)
  , tsRefreshToken :: !(Maybe Text)
  , tsScope :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON TokenSet where
  parseJSON = Aeson.withObject "TokenSet" $ \o ->
    TokenSet
      <$> o .: "access_token"
      <*> o .: "token_type"
      <*> o .:? "expires_in"
      <*> o .:? "refresh_token"
      <*> o .:? "scope"

-- | Build the authorization URL to open in the user's browser.
buildAuthorizationUrl ::
  AuthConfig ->
  PkceChallenge ->
  -- | state (anti-CSRF)
  Text ->
  -- | the URL to open in the browser
  Text
buildAuthorizationUrl cfg pkce state =
  let params =
        [ ("response_type", TE.encodeUtf8 ("code" :: Text))
        , ("client_id", TE.encodeUtf8 cfg.acClientId)
        , ("redirect_uri", TE.encodeUtf8 cfg.acRedirectUri)
        , ("state", TE.encodeUtf8 state)
        , ("code_challenge", TE.encodeUtf8 pkce.pkceChallenge)
        , ("code_challenge_method", TE.encodeUtf8 pkce.pkceMethod)
        ]
          ++ if null cfg.acScopes
            then []
            else [("scope", TE.encodeUtf8 (T.unwords cfg.acScopes))]
      qs = TE.decodeUtf8Lenient (renderSimpleQuery True params)
   in cfg.acAuthorizationEndpoint <> qs

-- | Exchange an authorization code for tokens at the token endpoint.
exchangeCode ::
  HTTP.Manager ->
  AuthConfig ->
  PkceChallenge ->
  -- | authorization code from redirect
  Text ->
  IO (Either Text TokenSet)
exchangeCode manager cfg pkce code = do
  let params =
        [ ("grant_type", "authorization_code")
        , ("code", TE.encodeUtf8 code)
        , ("redirect_uri", TE.encodeUtf8 cfg.acRedirectUri)
        , ("client_id", TE.encodeUtf8 cfg.acClientId)
        , ("code_verifier", TE.encodeUtf8 pkce.pkceVerifier)
        ]
      body = renderSimpleQuery False params
  result <-
    try
      ( do
          req0 <- HTTP.parseRequest (T.unpack cfg.acTokenEndpoint)
          let req =
                req0
                  { HTTP.method = "POST"
                  , HTTP.requestBody = HTTP.RequestBodyLBS (LBS.fromStrict body)
                  , HTTP.requestHeaders =
                      [ ("Content-Type", "application/x-www-form-urlencoded")
                      , ("Accept", "application/json")
                      ]
                  }
          HTTP.httpLbs req manager
      ) ::
      IO (Either SomeException (HTTP.Response LBS.ByteString))
  case result of
    Left ex -> pure (Left (T.pack ("Token exchange failed: " <> show ex)))
    Right resp ->
      if not (Status.statusIsSuccessful (HTTP.responseStatus resp))
        then
          pure
            ( Left
                ( "Token exchange HTTP error: "
                    <> T.pack (show (Status.statusCode (HTTP.responseStatus resp)))
                )
            )
        else case Aeson.eitherDecode' (HTTP.responseBody resp) of
          Left err -> pure (Left (T.pack ("Token parse error: " <> err)))
          Right ts -> pure (Right ts)
