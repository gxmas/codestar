{- |
= CodeStar.Transport.WebSocket — WebSocket byte-level adapters

Thin wrappers that adapt a 'WS.Connection' to the
@(BL.ByteString -> IO (), IO (Maybe BL.ByteString))@ pair that
'CodeStar.Transport.JsonRpc.jsonRpcTransport' expects.

  * 'websocketSend' sends a lazy 'ByteString' as a WebSocket text frame.
  * 'websocketRecv' receives the next frame, returning 'Nothing' on any
    'WS.ConnectionException' (normal close, abnormal close, or network error).
    Returning 'Nothing' causes the JSON-RPC listen loop to exit cleanly.
-}
module CodeStar.Transport.WebSocket
  ( websocketSend
  , websocketRecv
  ) where

import Control.Exception (catch)
import Data.ByteString.Lazy qualified as BL
import Network.WebSockets qualified as WS

-- | Send bytes to the peer as a WebSocket text frame.
websocketSend :: WS.Connection -> BL.ByteString -> IO ()
websocketSend conn = WS.sendTextData conn

-- | Receive the next frame from the peer.  Returns 'Nothing' on connection
-- close or any WebSocket exception, signalling EOF to the listen loop.
websocketRecv :: WS.Connection -> IO (Maybe BL.ByteString)
websocketRecv conn =
  (Just <$> WS.receiveData conn)
    `catch` \(_ :: WS.ConnectionException) -> pure Nothing
