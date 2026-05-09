module CodeStar.Transport.WebSocketSpec (spec) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, finally, try)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BLC8
import Network.Socket qualified as NS
import Network.WebSockets qualified as WS
import System.Timeout (timeout)
import Test.Hspec

import CodeStar.Transport.WebSocket (websocketRecv, websocketSend)

spec :: Spec
spec = describe "CodeStar.Transport.WebSocket" $ do
  it "websocketSend forwards text frames to peer" $ do
    let payload = BLC8.pack "ping-from-client"
    receivedRef <- newEmptyMVar
    withServerClient
      (\conn -> (WS.receiveData conn :: IO ByteString) >>= putMVar receivedRef)
      (\port -> withClientRetry port (\conn -> websocketSend conn payload))
    received <- takeMVar receivedRef
    received `shouldBe` payload

  it "websocketRecv returns Just payload when a frame arrives" $ do
    let payload = BLC8.pack "hello-from-server"
    receivedRef <- newEmptyMVar
    withServerClient
      (\conn -> websocketSend conn payload)
      (\port -> withClientRetry port (\conn -> websocketRecv conn >>= putMVar receivedRef))
    received <- takeMVar receivedRef
    received `shouldBe` Just payload

  it "websocketRecv returns Nothing when the connection closes" $ do
    receivedRef <- newEmptyMVar
    withServerClient
      (\conn -> WS.sendClose conn (BLC8.pack "bye"))
      (\port -> withClientRetry port (\conn -> websocketRecv conn >>= putMVar receivedRef))
    received <- takeMVar receivedRef
    received `shouldBe` Nothing

withServerClient :: (WS.Connection -> IO ()) -> (Int -> IO ()) -> IO ()
withServerClient serverAction clientAction = do
  port <- getFreePort
  serverDone <- newEmptyMVar
  tid <- forkIO $
    WS.runServer "127.0.0.1" port $ \pendingConn -> do
      conn <- WS.acceptRequest pendingConn
      serverAction conn
      putMVar serverDone ()
  ( do
      clientAction port
      done <- timeout 1000000 (takeMVar serverDone)
      done `shouldBe` Just ()
    )
    `finally` killThread tid

withClientRetry :: Int -> WS.ClientApp () -> IO ()
withClientRetry port action = go (50 :: Int)
 where
  go 0 = fail "Unable to connect test WebSocket client"
  go n = do
    result <- try (WS.runClient "127.0.0.1" port "/" action) :: IO (Either SomeException ())
    case result of
      Right _ -> pure ()
      Left _ -> do
        threadDelay 20000
        go (n - 1)

getFreePort :: IO Int
getFreePort = NS.withSocketsDo $ do
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  NS.bind sock (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
  NS.listen sock 1
  addr <- NS.getSocketName sock
  NS.close sock
  case addr of
    NS.SockAddrInet p _ -> pure (fromIntegral p)
    _ -> fail "expected IPv4 socket address"
