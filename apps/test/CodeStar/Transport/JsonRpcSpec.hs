module CodeStar.Transport.JsonRpcSpec (spec) where

import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC8
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Text (Text)
import Test.Hspec

import CodeStar.AgentLoop (AgentEvent (..))
import CodeStar.Transport.JsonRpc
  ( encodeNotification
  , jsonRpcTransport
  , sessionApproveMethod
  , sessionCompactMethod
  , sessionRejectMethod
  , sessionRespondMethod
  , sessionStartMethod
  , sessionStopMethod
  )
import CodeStar.Transport.Types
  ( AgentEventEnvelope (..)
  , Command (..)
  , CommandResult (..)
  )
import CodeStar.Transport.Types qualified as TT
import CodeStar.Types (SessionId (..))

spec :: Spec
spec = describe "CodeStar.Transport.JsonRpc" $ do
  describe "encodeNotification" $ do
    it "encodes event with expected JSON-RPC method" $ do
      let env = AgentEventEnvelope (SessionId "s1") (AgentToken "hello")
      v <- decodeJsonValue (encodeNotification env)
      extractMethod v `shouldBe` "agent.token"

  describe "command dispatch" $ do
    it "dispatches each command variant to the expected handler payload" $ do
      assertDispatch
        sessionStartMethod
        (object ["sessionId" .= ("s1" :: Text), "task" .= ("do work" :: Text)])
        (CmdStart (SessionId "s1") "do work")
      assertDispatch
        sessionRespondMethod
        (object ["sessionId" .= ("s2" :: Text), "response" .= ("continue" :: Text)])
        (CmdRespond (SessionId "s2") "continue")
      assertDispatch
        sessionApproveMethod
        (object ["sessionId" .= ("s3" :: Text)])
        (CmdApprove (SessionId "s3"))
      assertDispatch
        sessionRejectMethod
        (object ["sessionId" .= ("s4" :: Text), "reason" .= ("no" :: Text)])
        (CmdReject (SessionId "s4") "no")
      assertDispatch
        sessionCompactMethod
        (object ["sessionId" .= ("s5" :: Text), "instruction" .= ("shorten" :: Text)])
        (CmdCompact (SessionId "s5") (Just "shorten"))
      assertDispatch
        sessionCompactMethod
        (object ["sessionId" .= ("s5b" :: Text)])
        (CmdCompact (SessionId "s5b") Nothing)
      assertDispatch
        sessionStopMethod
        (object ["sessionId" .= ("s6" :: Text)])
        (CmdStop (SessionId "s6"))

    it "unknown methods return JSON-RPC error response (no crash)" $ do
      sentRef <- newIORef ([] :: [BL.ByteString])
      transport <- mkTransport [mkRequest "session.unknown" (object [])] sentRef
      writeHandler transport (\_ -> pure CmdOk)
      transport.listen
      sent <- readIORef sentRef
      sent `shouldSatisfy` (not . null)
      BLC8.unpack (BLC8.concat sent) `shouldSatisfy` isInfixOf "\"error\""

    it "malformed params types return JSON-RPC error and do not dispatch command" $ do
      sentRef <- newIORef ([] :: [BL.ByteString])
      cmdRef <- newIORef Nothing
      let badReq =
            encode
              ( object
                  [ "jsonrpc" .= ("2.0" :: Text)
                  , "id" .= (1 :: Int)
                  , "method" .= sessionStartMethod
                  , "params" .= object ["sessionId" .= (123 :: Int), "task" .= ("ok" :: Text)]
                  ]
              )
      transport <- mkTransport [badReq] sentRef
      writeHandler transport (\cmd -> writeIORef cmdRef (Just cmd) >> pure CmdOk)
      transport.listen
      dispatched <- readIORef cmdRef
      dispatched `shouldBe` Nothing
      sent <- readIORef sentRef
      sent `shouldSatisfy` (not . null)
      BLC8.unpack (BLC8.concat sent) `shouldSatisfy` isInfixOf "\"error\""

    it "continues processing after malformed frame and handles next valid request" $ do
      sentRef <- newIORef ([] :: [BL.ByteString])
      cmdRef <- newIORef Nothing
      let badJson = BLC8.pack "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"session.start\",\"params\":"
          goodReq = mkRequest sessionApproveMethod (object ["sessionId" .= ("s-ok" :: Text)])
      transport <- mkTransport [badJson, goodReq] sentRef
      writeHandler transport (\cmd -> writeIORef cmdRef (Just cmd) >> pure CmdOk)
      transport.listen
      dispatched <- readIORef cmdRef
      dispatched `shouldBe` Just (CmdApprove (SessionId "s-ok"))
      sent <- readIORef sentRef
      BLC8.unpack (BLC8.concat sent) `shouldSatisfy` isInfixOf "\"error\""

  describe "batch framing" $ do
    it "batch encoding/decoding of notifications round-trips" $ do
      let envs =
            [ AgentEventEnvelope (SessionId "s1") (AgentToken "a")
            , AgentEventEnvelope (SessionId "s2") (AgentProgress "p")
            ]
      notifVals <- mapM (decodeJsonValue . encodeNotification) envs
      let batch = encode notifVals
      decoded <- eitherFail (eitherDecode @[Value] batch)
      decoded `shouldBe` notifVals

  describe "transport sendEvent" $ do
    it "writes encoded notification bytes to send sink" $ do
      sentRef <- newIORef ([] :: [BL.ByteString])
      transport <- mkTransport [] sentRef
      transport.sendEvent (AgentEventEnvelope (SessionId "s-send") (AgentProgress "working"))
      sent <- readIORef sentRef
      case sent of
        [] -> expectationFailure "Expected sendEvent to emit at least one frame"
        firstFrame : _ -> do
          v <- decodeJsonValue firstFrame
          extractMethod v `shouldBe` "agent.progress"

assertDispatch :: Text -> Value -> Command -> Expectation
assertDispatch method params expected = do
  sentRef <- newIORef ([] :: [BL.ByteString])
  cmdRef <- newIORef Nothing
  transport <- mkTransport [mkRequest method params] sentRef
  writeHandler transport $ \cmd -> do
    writeIORef cmdRef (Just cmd)
    pure CmdOk
  transport.listen
  got <- readIORef cmdRef
  got `shouldBe` Just expected

mkRequest :: Text -> Value -> BL.ByteString
mkRequest method params =
  encode
    ( object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= (1 :: Int)
        , "method" .= method
        , "params" .= params
        ]
    )

mkTransport :: [BL.ByteString] -> IORef [BL.ByteString] -> IO TT.AgentTransportDict
mkTransport inputs sentRef = do
  inRef <- newIORef inputs
  jsonRpcTransport
    (\bs -> modifyIORef' sentRef (++ [bs]))
    (nextInput inRef)

nextInput :: IORef [BL.ByteString] -> IO (Maybe BL.ByteString)
nextInput ref = do
  xs <- readIORef ref
  case xs of
    [] -> pure Nothing
    (x : rest) -> writeIORef ref rest >> pure (Just x)

writeHandler :: TT.AgentTransportDict -> (Command -> IO CommandResult) -> IO ()
writeHandler transport = transport.onCommand

decodeJsonValue :: BL.ByteString -> IO Value
decodeJsonValue bs = eitherFail (eitherDecode bs)

extractMethod :: Value -> Text
extractMethod (Object o) =
  case KM.lookup "method" o of
    Just (String m) -> m
    _ -> ""
extractMethod _ = ""

eitherFail :: Either String a -> IO a
eitherFail (Left err) = expectationFailure err >> fail err
eitherFail (Right x) = pure x
