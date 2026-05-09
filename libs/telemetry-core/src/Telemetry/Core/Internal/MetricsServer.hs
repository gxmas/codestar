module Telemetry.Core.Internal.MetricsServer
  ( startMetricsServer
  ) where

import Control.Concurrent.Async (Async, async)
import Data.String (fromString)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp

startMetricsServer :: Text -> Int -> IO ByteString -> IO (Int, Async ())
startMetricsServer host port getMetrics = do
  (actualPort, socket) <- Warp.openFreePort
  let usePort = if port == 0 then actualPort else port
      settings =
        Warp.setPort usePort
          $ Warp.setHost (fromString (Text.unpack host))
          $ Warp.defaultSettings
  if port == 0
    then do
      handle <- async $ Warp.runSettingsSocket settings socket (metricsApp getMetrics)
      pure (actualPort, handle)
    else do
      handle <- async $ Warp.runSettings settings (metricsApp getMetrics)
      pure (usePort, handle)

metricsApp :: IO ByteString -> Application
metricsApp getMetrics req respond =
  case rawPathInfo req of
    "/metrics" -> do
      body <- getMetrics
      respond $ responseLBS status200 [("Content-Type", "text/plain; version=0.0.4")] body
    _ ->
      respond $ responseLBS status404 [] "Not found"
