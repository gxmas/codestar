module CodeStar.Transport.WebSocket
  ( websocketSend
  , websocketRecv
  ) where

import Control.Exception (catch)
import Data.ByteString.Lazy qualified as BL
import Network.WebSockets qualified as WS

websocketSend :: WS.Connection -> BL.ByteString -> IO ()
websocketSend conn = WS.sendTextData conn

websocketRecv :: WS.Connection -> IO (Maybe BL.ByteString)
websocketRecv conn =
  (Just <$> WS.receiveData conn)
    `catch` \(_ :: WS.ConnectionException) -> pure Nothing
