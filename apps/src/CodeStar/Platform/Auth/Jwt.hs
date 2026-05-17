module CodeStar.Platform.Auth.Jwt
  ( JwtValidator (..)
  , newJwtValidator
  , validateToken
  ) where

import Crypto.Hash (SHA256 (..))
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Crypto.PubKey.RSA.PKCS15 qualified as PKCS15
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64URL
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Vector qualified as V

import CodeStar.Config.Types (JwtAuthConfig (..))
import CodeStar.Platform.Auth (AuthResult (..), Identity (..))
import CodeStar.Platform.Auth.Jwks (Jwk (..), JwksCache, getKeys)
import CodeStar.Types (OrgId (..), UserId (..))

data JwtValidator = JwtValidator
  { jwksCache :: !JwksCache
  , config    :: !JwtAuthConfig
  }

newJwtValidator :: JwksCache -> JwtAuthConfig -> JwtValidator
newJwtValidator = JwtValidator

validateToken :: JwtValidator -> Text -> IO AuthResult
validateToken v token = do
  case splitJwt (TE.encodeUtf8 token) of
    Nothing -> pure (Unauthenticated "Malformed JWT")
    Just (headerB64, payloadB64, sigB64) -> do
      case (b64UrlDecode headerB64, b64UrlDecode payloadB64, b64UrlDecode sigB64) of
        (Just headerBytes, Just payloadBytes, Just sigBytes) ->
          case (Aeson.eitherDecodeStrict headerBytes, Aeson.eitherDecodeStrict payloadBytes) of
            (Right header, Right claims) -> do
              keys <- getKeys v.jwksCache
              let signedData = headerB64 <> "." <> payloadB64
                  alg = lookupText "alg" header
              case alg of
                Just "HS256" -> verifyHmac v keys signedData sigBytes claims
                Just "RS256" -> verifyRsa v keys signedData sigBytes claims
                Just a       -> pure (Unauthenticated ("Unsupported algorithm: " <> a))
                Nothing      -> pure (Unauthenticated "Missing alg in JWT header")
            _ -> pure (Unauthenticated "Malformed JWT payload")
        _ -> pure (Unauthenticated "Malformed JWT encoding")

verifyHmac :: JwtValidator -> [Jwk] -> ByteString -> ByteString -> KM.KeyMap Value -> IO AuthResult
verifyHmac v keys signedData sig claims = do
  let hmacKeys = [k | HmacKey k <- keys]
      isValid = any (\k -> hmacSha256 k signedData `constEq` sig) hmacKeys
  if isValid
    then validateClaims v.config claims
    else pure (Unauthenticated "Invalid signature")

verifyRsa :: JwtValidator -> [Jwk] -> ByteString -> ByteString -> KM.KeyMap Value -> IO AuthResult
verifyRsa v keys signedData sig claims = do
  let rsaKeys = [pk | RsaKey _ pk <- keys]
      isValid = any (\pk -> PKCS15.verify (Just SHA256) pk signedData sig) rsaKeys
  if isValid
    then validateClaims v.config claims
    else pure (Unauthenticated "Invalid signature")

validateClaims :: JwtAuthConfig -> KM.KeyMap Value -> IO AuthResult
validateClaims cfg claims = do
  now <- getPOSIXTime
  let expResult = case lookupNumber "exp" claims of
        Nothing -> Right ()
        Just expTime
          | realToFrac now > expTime -> Left "JWT expired"
          | otherwise -> Right ()
      issResult = case cfg.issuer of
        Nothing -> Right ()
        Just expectedIss -> case lookupText "iss" claims of
          Just iss | iss == expectedIss -> Right ()
          _ -> Left "Issuer mismatch"
      audResult = case cfg.audience of
        Nothing -> Right ()
        Just expectedAud -> case KM.lookup (Key.fromText "aud") claims of
          Just (String aud) | aud == expectedAud -> Right ()
          Just (Array arr) | any (\case String a -> a == expectedAud; _ -> False) (V.toList arr) -> Right ()
          _ -> Left "Audience mismatch"
  case expResult >> issResult >> audResult of
    Left err -> pure (Unauthenticated err)
    Right () -> pure (extractIdentity cfg claims)

extractIdentity :: JwtAuthConfig -> KM.KeyMap Value -> AuthResult
extractIdentity cfg claims =
  let uid = lookupText cfg.claimUserId claims
      oid = lookupText cfg.claimOrgId claims
      rs  = lookupRoles cfg.claimRoles claims
  in case uid of
    Nothing ->
      case lookupText "sub" claims of
        Just sub -> Authenticated Identity
          { userId = UserId sub
          , orgId  = OrgId (maybe "default" id oid)
          , roles  = maybe ["user"] id rs
          }
        Nothing -> Unauthenticated ("Missing claim: " <> cfg.claimUserId)
    Just u -> Authenticated Identity
      { userId = UserId u
      , orgId  = OrgId (maybe "default" id oid)
      , roles  = maybe ["user"] id rs
      }

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

splitJwt :: ByteString -> Maybe (ByteString, ByteString, ByteString)
splitJwt bs = case BS.split (fromIntegral (fromEnum '.')) bs of
  [h, p, s] -> Just (h, p, s)
  _         -> Nothing

b64UrlDecode :: ByteString -> Maybe ByteString
b64UrlDecode bs =
  case B64URL.decodeUnpadded bs of
    Right r -> Just r
    Left _  ->
      case B64URL.decode bs of
        Right r -> Just r
        Left _  -> Nothing

hmacSha256 :: ByteString -> ByteString -> ByteString
hmacSha256 key msg =
  let HMAC digest = hmac key msg :: HMAC SHA256
  in BA.convert digest

constEq :: ByteString -> ByteString -> Bool
constEq = BA.constEq

lookupText :: Text -> KM.KeyMap Value -> Maybe Text
lookupText key km = case KM.lookup (Key.fromText key) km of
  Just (String t) -> Just t
  _               -> Nothing

lookupNumber :: Text -> KM.KeyMap Value -> Maybe Double
lookupNumber key km = case KM.lookup (Key.fromText key) km of
  Just (Number n) -> Just (realToFrac n)
  _               -> Nothing

lookupRoles :: Text -> KM.KeyMap Value -> Maybe [Text]
lookupRoles key km = case KM.lookup (Key.fromText key) km of
  Just (Array arr) -> Just [t | String t <- V.toList arr]
  _                -> Nothing
