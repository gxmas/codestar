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

data Identity = Identity
  { userId :: !UserId
  , orgId :: !OrgId
  , roles :: ![Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- --------------------------------------------------------------------
-- Auth config
-- --------------------------------------------------------------------

data AuthConfig
  = NoAuthConfig
  | ApiKeyConfig !(Text -> Bool)
  | JwtConfig !(Text -> IO AuthResult)

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
