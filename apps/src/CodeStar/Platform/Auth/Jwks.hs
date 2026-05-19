{- |
= Platform.Auth.Jwks — JWKS key cache

A JSON Web Key Set (JWKS) is a JSON document published by an identity
provider that lists the public keys used to sign JWTs.  Clients fetch this
document and use the keys to verify incoming tokens.

This module provides a __caching layer__ around JWKS fetching so that:

  * Key lookups are fast (in-memory, no HTTP round trip).
  * The cache is refreshed when the TTL expires so key rotations are
    picked up automatically without restarting the server.
  * If a refresh fails, the stale keys are served rather than failing all
    authentication — a graceful degradation strategy.
  * Concurrent refresh requests are serialised by a mutex (@MVar@) so only
    one HTTP call is in flight at a time.

Three key sources are supported:

  * @JwksUri@ — fetch from an HTTPS endpoint (e.g. Keycloak, Auth0).
  * @JwksInline@ — a literal JSON JWKS string in the config.
  * @JwksHmacSecret@ — a shared secret for HS256 tokens (single-server use).
-}
module CodeStar.Platform.Auth.Jwks
  ( JwksCache
  , Jwk (..)
  , newJwksCache
  , getKeys
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, try)
import Crypto.PubKey.RSA.Types qualified as RSA
import Data.Aeson (FromJSON (..), Value (..), (.:), (.:?), withObject)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody)

import CodeStar.Config.Types (JwksSource (..))

-- | A single JSON Web Key.  Only RSA and HMAC keys are supported;
-- EC and other key types are silently skipped during parsing.
data Jwk
  = RsaKey !Text !RSA.PublicKey -- ^ RSA public key with its key ID (@kid@).
  | HmacKey !ByteString         -- ^ HMAC shared secret (for HS256 tokens).
  deriving stock (Show)

data JwksCache = JwksCache
  { cacheRef  :: !(IORef (Maybe ([Jwk], UTCTime)))
  , fetchLock :: !(MVar ())
  , source    :: !JwksSource
  , ttlSecs   :: !Int
  , httpMgr   :: !Manager
  }

-- | Create a new JWKS cache.
-- @mgr@ is an HTTPS connection manager; @src@ is the key source;
-- @ttl@ is the cache TTL in seconds.
newJwksCache :: Manager -> JwksSource -> Int -> IO JwksCache
newJwksCache mgr src ttl = do
  ref <- newIORef Nothing
  lock <- newMVar ()
  pure JwksCache
    { cacheRef  = ref
    , fetchLock = lock
    , source    = src
    , ttlSecs   = ttl
    , httpMgr   = mgr
    }

-- | Return the current set of keys, refreshing from the source if the
-- cache has expired.  Never blocks the caller for longer than one HTTPS
-- round trip; stale keys are served if the refresh fails.
getKeys :: JwksCache -> IO [Jwk]
getKeys cache = do
  cached <- readIORef cache.cacheRef
  now <- getCurrentTime
  case cached of
    Just (ks, ts)
      | not (isExpired cache.ttlSecs now ts) -> pure ks
    _ -> refresh cache cached

refresh :: JwksCache -> Maybe ([Jwk], UTCTime) -> IO [Jwk]
refresh cache stale = withMVar cache.fetchLock $ \_ -> do
  cached <- readIORef cache.cacheRef
  now <- getCurrentTime
  case cached of
    Just (ks, ts)
      | not (isExpired cache.ttlSecs now ts) -> pure ks
    _ -> do
      result <- try (fetchKeys cache.httpMgr cache.source)
      case result of
        Right ks -> do
          writeIORef cache.cacheRef (Just (ks, now))
          pure ks
        Left (_ex :: SomeException) ->
          case stale of
            Just (ks, _) -> pure ks
            Nothing      -> fetchKeys cache.httpMgr cache.source

isExpired :: Int -> UTCTime -> UTCTime -> Bool
isExpired ttl now ts = diffUTCTime now ts > fromIntegral ttl

fetchKeys :: Manager -> JwksSource -> IO [Jwk]
fetchKeys _mgr (JwksHmacSecret sec) =
  pure [HmacKey (TE.encodeUtf8 sec)]
fetchKeys _mgr (JwksInline raw) =
  case Aeson.eitherDecode (LBS.fromStrict (TE.encodeUtf8 raw)) of
    Right (JwkSet ks) -> pure ks
    Left err -> fail ("Invalid inline JWKS: " <> err)
fetchKeys mgr (JwksUri uri) = do
  req <- parseRequest (Text.unpack uri)
  resp <- httpLbs req mgr
  case Aeson.eitherDecode (responseBody resp) of
    Right (JwkSet ks) -> pure ks
    Left err -> fail ("Failed to parse JWKS from " <> show uri <> ": " <> err)

-- --------------------------------------------------------------------
-- JWK JSON parsing
-- --------------------------------------------------------------------

newtype JwkSet = JwkSet [Jwk]

instance FromJSON JwkSet where
  parseJSON = withObject "JwkSet" $ \o -> do
    keys <- o .: "keys"
    jwks <- mapM parseJwk keys
    pure (JwkSet [k | Just k <- jwks])

parseJwk :: Value -> Parser (Maybe Jwk)
parseJwk = withObject "Jwk" $ \o -> do
  kty <- o .: "kty" :: Parser Text
  kid <- o .:? "kid" :: Parser (Maybe Text)
  case kty of
    "RSA" -> do
      n <- o .: "n"
      e <- o .: "e"
      case rsaPublicKey n e of
        Just pk -> pure (Just (RsaKey (maybe "" id kid) pk))
        Nothing -> pure Nothing
    _ -> pure Nothing

rsaPublicKey :: Text -> Text -> Maybe RSA.PublicKey
rsaPublicKey nB64 eB64 = do
  nBytes <- b64UrlDecode nB64
  eBytes <- b64UrlDecode eB64
  let n = bytesToInteger nBytes
      e = bytesToInteger eBytes
      size = (fromIntegral (integerLog2 n) + 8) `div` 8
  Just RSA.PublicKey
    { RSA.public_size = size
    , RSA.public_n = n
    , RSA.public_e = e
    }

b64UrlDecode :: Text -> Maybe ByteString
b64UrlDecode t =
  let padded = case Text.length t `mod` 4 of
        2 -> t <> "=="
        3 -> t <> "="
        _ -> t
      std = Text.map (\c -> if c == '-' then '+' else if c == '_' then '/' else c) padded
  in case B64.decode (TE.encodeUtf8 std) of
    Right bs -> Just bs
    Left _   -> Nothing

bytesToInteger :: ByteString -> Integer
bytesToInteger = foldl' (\acc b -> acc * 256 + fromIntegral b) 0 . BS.unpack

integerLog2 :: Integer -> Int
integerLog2 n
  | n <= 0    = 0
  | otherwise = go 0 n
  where
    go !acc 1 = acc
    go !acc x = go (acc + 1) (x `div` 2)
