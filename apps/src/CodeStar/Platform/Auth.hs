{- |
= CodeStar.Platform.Auth — authentication and identity

This module defines the __authentication boundary__ of the server.  Every
incoming WebSocket connection is validated here before any agent code runs.

== Auth modes

  * __NoAuthConfig__: every connection gets the anonymous identity.
    Suitable for local CLI use and development.
  * __ApiKeyConfig__: the caller supplies a validation function
    (@Text -> Bool@).  Useful for simple shared-secret deployments.
  * __JwtConfig__: the caller supplies a full validation function
    (@Text -> IO AuthResult@), typically built from 'Auth.Jwt.validateToken'.
    Suitable for multi-tenant production deployments.

The mode is selected at startup from the TOML config and never changes,
which means 'authenticate' is a pure dispatch with no hidden state.

== Identity

'Identity' is the result of a successful authentication.  It carries the
@userId@, @orgId@, and @roles@ that the rest of the system uses for
multi-tenancy, rate limiting, and permission checks.
-}
module CodeStar.Platform.Auth
  ( -- * Identity
    Identity (..)

    -- * Auth modes
  , AuthConfig (..)
  , AuthResult (..)

    -- * Validation
  , authenticate
  , noAuth
  , extractBearer
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

import CodeStar.Types (OrgId (..), UserId (..))

-- --------------------------------------------------------------------
-- Identity
-- --------------------------------------------------------------------

-- | The authenticated identity of a connected client.
-- Passed through the request handling pipeline and attached to every
-- session and telemetry span.
data Identity = Identity
  { userId :: !UserId    -- ^ Unique user identifier.
  , orgId  :: !OrgId     -- ^ Organisation the user belongs to.
  , roles  :: ![Text]    -- ^ Role list for future authorisation checks.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- --------------------------------------------------------------------
-- Auth config
-- --------------------------------------------------------------------

-- | Selects the authentication strategy for an incoming connection.
data AuthConfig
  = NoAuthConfig
  -- ^ No authentication — accept all connections with the anonymous identity.
  | ApiKeyConfig !(Text -> Bool)
  -- ^ Validate the Bearer token with the supplied predicate.
  | JwtConfig !(Text -> IO AuthResult)
  -- ^ Validate the Bearer token as a JWT using the supplied IO action.
  --   Build this with 'Auth.Jwt.validateToken'.

data AuthResult
  = Authenticated !Identity
  | -- | reason
    Unauthenticated !Text
  deriving stock (Eq, Show)

-- --------------------------------------------------------------------
-- Validation
-- --------------------------------------------------------------------

{- | Validate a bearer token string from the Authorization header.
Expected format: "Bearer <token>"
-}
authenticate :: AuthConfig -> Text -> IO AuthResult
authenticate NoAuthConfig _ = pure (Authenticated anonymousIdentity)
authenticate (ApiKeyConfig validate) header =
  case extractBearer header of
    Nothing -> pure (Unauthenticated "Missing or malformed Authorization header")
    Just token ->
      if validate token
        then pure (Authenticated (apiKeyIdentity token))
        else pure (Unauthenticated "Invalid API key")
authenticate (JwtConfig validate) header =
  case extractBearer header of
    Nothing -> pure (Unauthenticated "Missing or malformed Authorization header")
    Just token -> validate token

-- | Always return the anonymous identity. Used in CLI / local mode.
noAuth :: IO AuthResult
noAuth = pure (Authenticated anonymousIdentity)

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

-- | Extract the token string from an @Authorization: Bearer <token>@ header.
-- Returns 'Nothing' if the header is absent, malformed, or the token is empty.
extractBearer :: Text -> Maybe Text
extractBearer header =
  let prefix = "Bearer "
      token = Text.drop (Text.length prefix) header
   in if Text.isPrefixOf prefix header && not (Text.null token)
        then Just token
        else Nothing

anonymousIdentity :: Identity
anonymousIdentity =
  Identity
    { userId = UserId "anonymous"
    , orgId = OrgId "local"
    , roles = ["user"]
    }

{- | Derive a synthetic identity from an API key.
In production this would look up the key in a database.
-}
apiKeyIdentity :: Text -> Identity
apiKeyIdentity token =
  Identity
    { userId = UserId ("api-" <> Text.take 8 token)
    , orgId = OrgId "default"
    , roles = ["user"]
    }
