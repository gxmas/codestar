module OpenAI.Internal.Http
  ( newManager
  , postJSON
  , postJSONStreaming
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON, FromJSON, encode, eitherDecode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client hiding (newManager)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

import OpenAI.Types (ApiErrorDetail (..), ClientConfig (..), ClientError (..), ApiError (..))
import OpenAI.Internal.SSE (parseSSELine, SSEEvent (..))


newManager :: IO Manager
newManager = HC.newManager tlsManagerSettings


postJSON
  :: (ToJSON req, FromJSON resp)
  => Manager
  -> ClientConfig
  -> Text
  -> req
  -> IO (Either ClientError resp)
postJSON mgr cfg path body = do
  result <- try @SomeException $ do
    initReq <- parseRequest (Text.unpack (cfg.baseUrl <> path))
    let req = initReq
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Authorization", "Bearer " <> TE.encodeUtf8 cfg.apiKey)
              ]
          , requestBody = RequestBodyLBS (encode body)
          }
    resp <- httpLbs req mgr
    let status = statusCode (responseStatus resp)
        respBody = responseBody resp
    if status >= 200 && status < 300
      then case eitherDecode respBody of
        Left err  -> pure (Left (DeserializationError (Text.pack err)))
        Right val -> pure (Right val)
      else case eitherDecode respBody of
        Left _        -> pure (Left (ApiErrorResponse status (mkErrDetail (LBS.toStrict respBody))))
        Right apiErr  -> pure (Left (ApiErrorResponse status (apiErr :: ApiError).error))
  pure $ case result of
    Left ex  -> Left (NetworkError (Text.pack (show ex)))
    Right r  -> r

  where
    mkErrDetail bs = ApiErrorDetail
      { message = TE.decodeUtf8Lenient bs
      , type_   = Nothing
      }


postJSONStreaming
  :: ToJSON req
  => Manager
  -> ClientConfig
  -> Text
  -> req
  -> (SSEEvent -> IO ())
  -> IO (Either ClientError ())
postJSONStreaming mgr cfg path body onEvent = do
  result <- try @SomeException $ do
    initReq <- parseRequest (Text.unpack (cfg.baseUrl <> path))
    let req = initReq
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Authorization", "Bearer " <> TE.encodeUtf8 cfg.apiKey)
              , ("Accept", "text/event-stream")
              ]
          , requestBody = RequestBodyLBS (encode body)
          }
    withResponse req mgr $ \resp -> do
      let status = statusCode (responseStatus resp)
      if status >= 200 && status < 300
        then consumeSSE (responseBody resp) onEvent
        else pure ()
  pure $ case result of
    Left ex -> Left (NetworkError (Text.pack (show ex)))
    Right _ -> Right ()

consumeSSE :: BodyReader -> (SSEEvent -> IO ()) -> IO ()
consumeSSE reader onEvent = go mempty
  where
    go buf = do
      chunk <- brRead reader
      if BC.null chunk
        then pure ()
        else do
          let (lines_, remainder) = splitLines (buf <> chunk)
          mapM_ (onEvent . parseSSELine) lines_
          go remainder

    splitLines bs =
      let allLines = BC.splitWith (== '\n') bs
      in case reverse allLines of
        []     -> ([], mempty)
        (r:ls) -> (reverse ls, r)
