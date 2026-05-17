module CodeStar.Platform.Auth.JwtSpec (spec) where

import Crypto.Hash (SHA256 (..))
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Crypto.PubKey.RSA (generate)
import Crypto.PubKey.RSA.PKCS15 qualified as PKCS15
import Crypto.PubKey.RSA.Types qualified as RSA
import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64URL
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word8)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Test.Hspec

import CodeStar.Config.Types (JwtAuthConfig (..), JwksSource (..))
import CodeStar.Platform.Auth (AuthResult (..), Identity (..))
import CodeStar.Platform.Auth.Jwks (newJwksCache)
import CodeStar.Platform.Auth.Jwt (JwtValidator, newJwtValidator, validateToken)
import CodeStar.Types (OrgId (..), UserId (..))

spec :: Spec
spec = describe "JWT validation" $ do

  it "accepts a valid HS256 token" $ do
    let secret = "test-secret-key-for-hmac-256-ok"
    v <- mkHmacValidator secret Nothing Nothing defaultClaims
    now <- getPOSIXTime
    let claims = object
          [ "sub" .= ("user-123" :: Text)
          , "exp" .= (round (now + 3600) :: Int)
          ]
        token = signHs256 secret claims
    result <- validateToken v token
    result `shouldBe` Authenticated Identity
      { userId = UserId "user-123"
      , orgId  = OrgId "default"
      , roles  = ["user"]
      }

  it "rejects an expired token" $ do
    let secret = "test-secret-key-for-hmac-256-ok"
    v <- mkHmacValidator secret Nothing Nothing defaultClaims
    now <- getPOSIXTime
    let claims = object
          [ "sub" .= ("user-123" :: Text)
          , "exp" .= (round (now - 100) :: Int)
          ]
        token = signHs256 secret claims
    result <- validateToken v token
    result `shouldBe` Unauthenticated "JWT expired"

  it "rejects a token with wrong HMAC secret" $ do
    let secret1 = "correct-secret-key-for-testing!"
        secret2 = "wrong---secret-key-for-testing!"
    v <- mkHmacValidator secret1 Nothing Nothing defaultClaims
    now <- getPOSIXTime
    let claims = object
          [ "sub" .= ("user-123" :: Text)
          , "exp" .= (round (now + 3600) :: Int)
          ]
        token = signHs256 secret2 claims
    result <- validateToken v token
    result `shouldBe` Unauthenticated "Invalid signature"

  it "rejects a token missing the sub claim" $ do
    let secret = "test-secret-key-for-hmac-256-ok"
    v <- mkHmacValidator secret Nothing Nothing defaultClaims
    now <- getPOSIXTime
    let claims = object
          [ "exp" .= (round (now + 3600) :: Int)
          ]
        token = signHs256 secret claims
    result <- validateToken v token
    result `shouldBe` Unauthenticated "Missing claim: sub"

  it "validates issuer when configured" $ do
    let secret = "test-secret-key-for-hmac-256-ok"
    v <- mkHmacValidator secret (Just "https://auth.example.com/") Nothing defaultClaims
    now <- getPOSIXTime
    let claims = object
          [ "sub" .= ("user-123" :: Text)
          , "iss" .= ("https://wrong-issuer.com/" :: Text)
          , "exp" .= (round (now + 3600) :: Int)
          ]
        token = signHs256 secret claims
    result <- validateToken v token
    result `shouldBe` Unauthenticated "Issuer mismatch"

  it "validates audience when configured" $ do
    let secret = "test-secret-key-for-hmac-256-ok"
    v <- mkHmacValidator secret Nothing (Just "codestar") defaultClaims
    now <- getPOSIXTime
    let claims = object
          [ "sub" .= ("user-123" :: Text)
          , "aud" .= ("wrong-audience" :: Text)
          , "exp" .= (round (now + 3600) :: Int)
          ]
        token = signHs256 secret claims
    result <- validateToken v token
    result `shouldBe` Unauthenticated "Audience mismatch"

  it "extracts custom claim names" $ do
    let secret = "test-secret-key-for-hmac-256-ok"
        claimCfg = ClaimConfig "user_id" "organization" "perms"
    v <- mkHmacValidator secret Nothing Nothing claimCfg
    now <- getPOSIXTime
    let claims = object
          [ "user_id" .= ("custom-user" :: Text)
          , "organization" .= ("my-org" :: Text)
          , "perms" .= (["admin", "write"] :: [Text])
          , "exp" .= (round (now + 3600) :: Int)
          ]
        token = signHs256 secret claims
    result <- validateToken v token
    result `shouldBe` Authenticated Identity
      { userId = UserId "custom-user"
      , orgId  = OrgId "my-org"
      , roles  = ["admin", "write"]
      }

  it "defaults roles to [\"user\"] when claim is absent" $ do
    let secret = "test-secret-key-for-hmac-256-ok"
    v <- mkHmacValidator secret Nothing Nothing defaultClaims
    now <- getPOSIXTime
    let claims = object
          [ "sub" .= ("user-123" :: Text)
          , "exp" .= (round (now + 3600) :: Int)
          ]
        token = signHs256 secret claims
    result <- validateToken v token
    case result of
      Authenticated ident -> ident.roles `shouldBe` ["user"]
      other -> expectationFailure ("Expected Authenticated, got: " <> show other)

  it "accepts a valid RS256 token" $ do
    (pub, priv) <- generate 256 65537
    v <- mkRsaValidator pub Nothing Nothing defaultClaims
    now <- getPOSIXTime
    let claims = object
          [ "sub" .= ("rsa-user" :: Text)
          , "exp" .= (round (now + 3600) :: Int)
          ]
        token = signRs256 priv claims
    result <- validateToken v token
    result `shouldBe` Authenticated Identity
      { userId = UserId "rsa-user"
      , orgId  = OrgId "default"
      , roles  = ["user"]
      }

  it "rejects a malformed token" $ do
    let secret = "test-secret-key-for-hmac-256-ok"
    v <- mkHmacValidator secret Nothing Nothing defaultClaims
    result <- validateToken v "not.a.valid.jwt"
    result `shouldBe` Unauthenticated "Malformed JWT"

  it "rejects a token with only two parts" $ do
    let secret = "test-secret-key-for-hmac-256-ok"
    v <- mkHmacValidator secret Nothing Nothing defaultClaims
    result <- validateToken v "header.payload"
    result `shouldBe` Unauthenticated "Malformed JWT"

-- --------------------------------------------------------------------
-- Test helpers
-- --------------------------------------------------------------------

data ClaimConfig = ClaimConfig
  { ccUserId :: Text
  , ccOrgId  :: Text
  , ccRoles  :: Text
  }

defaultClaims :: ClaimConfig
defaultClaims = ClaimConfig "sub" "org_id" "roles"

mkHmacValidator :: ByteString -> Maybe Text -> Maybe Text -> ClaimConfig -> IO (JwtValidator)
mkHmacValidator secret iss aud cc = do
  mgr <- newManager defaultManagerSettings
  cache <- newJwksCache mgr (JwksHmacSecret (TE.decodeUtf8 secret)) 300
  let cfg = JwtAuthConfig
        { jwksSource      = JwksHmacSecret (TE.decodeUtf8 secret)
        , issuer          = iss
        , audience        = aud
        , claimUserId     = cc.ccUserId
        , claimOrgId      = cc.ccOrgId
        , claimRoles      = cc.ccRoles
        , cacheTtlSeconds = 300
        }
  pure (newJwtValidator cache cfg)

mkRsaValidator :: RSA.PublicKey -> Maybe Text -> Maybe Text -> ClaimConfig -> IO (JwtValidator)
mkRsaValidator pub iss aud cc = do
  mgr <- newManager defaultManagerSettings
  let jwksJson = buildRsaJwks pub
  cache <- newJwksCache mgr (JwksInline jwksJson) 300
  let cfg = JwtAuthConfig
        { jwksSource      = JwksInline jwksJson
        , issuer          = iss
        , audience        = aud
        , claimUserId     = cc.ccUserId
        , claimOrgId      = cc.ccOrgId
        , claimRoles      = cc.ccRoles
        , cacheTtlSeconds = 300
        }
  pure (newJwtValidator cache cfg)

signHs256 :: ByteString -> Value -> Text
signHs256 secret payload =
  let header = object ["alg" .= ("HS256" :: Text), "typ" .= ("JWT" :: Text)]
      headerB64 = b64UrlEncode (LBS.toStrict (encode header))
      payloadB64 = b64UrlEncode (LBS.toStrict (encode payload))
      sigInput = headerB64 <> "." <> payloadB64
      HMAC digest = hmac secret sigInput :: HMAC SHA256
      sig = b64UrlEncode (BA.convert digest)
  in TE.decodeUtf8 (headerB64 <> "." <> payloadB64 <> "." <> sig)

signRs256 :: RSA.PrivateKey -> Value -> Text
signRs256 priv payload =
  let header = object ["alg" .= ("RS256" :: Text), "typ" .= ("JWT" :: Text)]
      headerB64 = b64UrlEncode (LBS.toStrict (encode header))
      payloadB64 = b64UrlEncode (LBS.toStrict (encode payload))
      sigInput = headerB64 <> "." <> payloadB64
  in case PKCS15.sign Nothing (Just SHA256) priv sigInput of
    Left _err -> error "RSA signing failed in test"
    Right sig ->
      TE.decodeUtf8 (headerB64 <> "." <> payloadB64 <> "." <> b64UrlEncode sig)

b64UrlEncode :: ByteString -> ByteString
b64UrlEncode = B64URL.encodeUnpadded

buildRsaJwks :: RSA.PublicKey -> Text
buildRsaJwks pub =
  let n = integerToB64Url (RSA.public_n pub)
      e = integerToB64Url (RSA.public_e pub)
      jwks = object
        [ "keys" .=
          [ object
            [ "kty" .= ("RSA" :: Text)
            , "kid" .= ("test-key" :: Text)
            , "n" .= n
            , "e" .= e
            ]
          ]
        ]
  in TE.decodeUtf8 (LBS.toStrict (encode jwks))

integerToB64Url :: Integer -> Text
integerToB64Url n =
  let bytes = integerToBytes n
  in TE.decodeUtf8 (B64URL.encodeUnpadded (BS.pack bytes))

integerToBytes :: Integer -> [Word8]
integerToBytes 0 = [0]
integerToBytes n = reverse (go n)
  where
    go 0 = []
    go x = let (q, r) = x `divMod` 256 in fromIntegral r : go q
