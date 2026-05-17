module Network.MCP.Auth.Pkce
  ( PkceChallenge (..)
  , generatePkce
  , verifyChallenge
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base64.URL as B64U
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified System.Entropy as Entropy

-- | A PKCE code verifier / challenge pair.
data PkceChallenge = PkceChallenge
  { pkceVerifier :: !Text
  -- ^ code_verifier (send to token endpoint)
  , pkceChallenge :: !Text
  -- ^ code_challenge (send to authorization endpoint)
  , pkceMethod :: !Text
  -- ^ always "S256"
  }
  deriving stock (Eq, Show, Generic)

-- | Generate a fresh PKCE verifier/challenge pair using 32 bytes of
-- cryptographic randomness.
generatePkce :: IO PkceChallenge
generatePkce = do
  -- 32 random bytes -> 43-char base64url verifier
  randomBytes <- Entropy.getEntropy 32
  let verifier = stripPadding (TE.decodeUtf8Lenient (B64U.encode randomBytes))
      challenge = computeChallenge verifier
  pure
    PkceChallenge
      { pkceVerifier = verifier
      , pkceChallenge = challenge
      , pkceMethod = "S256"
      }

-- | Compute S256 challenge from a verifier string.
computeChallenge :: Text -> Text
computeChallenge verifier =
  let hashBytes = SHA256.hash (TE.encodeUtf8 verifier)
   in stripPadding (TE.decodeUtf8Lenient (B64U.encode hashBytes))

-- | Strip trailing '=' padding from base64url.
stripPadding :: Text -> Text
stripPadding = T.dropWhileEnd (== '=')

-- | Verify that a verifier produces the expected S256 challenge.
-- Useful for property tests.
verifyChallenge :: Text -> Text -> Bool
verifyChallenge verifier challenge = computeChallenge verifier == challenge
