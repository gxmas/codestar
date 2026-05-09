{-# LANGUAGE OverloadedStrings #-}

module Test.Network.JsonRpc.CodecSpec (spec) where

import Data.Aeson (Value (..), eitherDecode')
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as LBS
import Network.JsonRpc.Codec
import Network.JsonRpc.Codec.Internal ()
import Network.JsonRpc.Types
import Test.Hspec
import Test.Network.JsonRpc.Generators ()
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Round-trip properties" $ do
    it "parse . serialize recovers the original message" $ property $
      \msg -> case parse (serialize msg) of
        SingleMessage msg' -> msg' == (msg :: Message)
        _ -> False

    it "serialize produces valid JSON" $ property $
      \msg ->
        let bs = serialize (msg :: Message)
         in case eitherDecode' bs :: Either String Value of
              Right _ -> True
              Left _ -> False

  describe "Spec examples" $ do
    -- 10.1: Positional params
    it "parses a request with positional params" $ do
      let input = "{\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":[42,23],\"id\":1}"
      case parse (LBS.pack input) of
        SingleMessage (MsgRequest req) -> do
          requestMethod req `shouldBe` "subtract"
          requestId req `shouldBe` Just (IdNum 1)
        other -> expectationFailure ("expected SingleMessage MsgRequest, got: " ++ show other)

    -- 10.2: Named params
    it "parses a request with named params" $ do
      let input = "{\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":{\"subtrahend\":23,\"minuend\":42},\"id\":3}"
      case parse (LBS.pack input) of
        SingleMessage (MsgRequest req) -> do
          requestMethod req `shouldBe` "subtract"
          requestId req `shouldBe` Just (IdNum 3)
        other -> expectationFailure ("expected SingleMessage MsgRequest, got: " ++ show other)

    -- 10.3: Notification
    it "parses a notification" $ do
      let input = "{\"jsonrpc\":\"2.0\",\"method\":\"update\",\"params\":[1,2,3,4,5]}"
      case parse (LBS.pack input) of
        SingleMessage (MsgNotification notif) ->
          notificationMethod notif `shouldBe` "update"
        other -> expectationFailure ("expected MsgNotification, got: " ++ show other)

    -- 10.4: Method not found response
    it "parses an error response" $ do
      let input = "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32601,\"message\":\"Method not found\"},\"id\":\"1\"}"
      case parse (LBS.pack input) of
        SingleMessage (MsgResponse (ResponseFailure f)) -> do
          errorCode (failureError f) `shouldBe` (-32601)
          failureId f `shouldBe` Just (IdText "1")
        other -> expectationFailure ("expected ResponseFailure, got: " ++ show other)

    -- 10.5: Invalid JSON
    it "returns ParseFailed for invalid JSON" $ do
      let input = "{\"jsonrpc\": \"2.0\", \"method\": \"foobar, \"params\": \"bar\", \"baz]"
      case parse (LBS.pack input) of
        ParseFailed e -> errorCode e `shouldBe` (-32700)
        other -> expectationFailure ("expected ParseFailed, got: " ++ show other)

    -- 10.8: Empty batch
    it "returns ParseFailed for empty batch" $ do
      case parse "[]" of
        ParseFailed e -> errorCode e `shouldBe` (-32600)
        other -> expectationFailure ("expected ParseFailed, got: " ++ show other)

    -- 10.9: Batch of non-objects
    it "returns InvalidElement for non-object batch elements" $ do
      case parse "[1, 2, 3]" of
        BatchMessage elems ->
          all isInvalidElement elems `shouldBe` True
        other -> expectationFailure ("expected BatchMessage, got: " ++ show other)

    -- 10.10: Mixed batch
    it "parses a mixed batch" $ do
      let input =
            LBS.pack $
              concat
                [ "["
                , "{\"jsonrpc\":\"2.0\",\"method\":\"sum\",\"params\":[1,2,4],\"id\":\"1\"},"
                , "{\"jsonrpc\":\"2.0\",\"method\":\"notify_hello\",\"params\":[7]},"
                , "{\"foo\":\"boo\"}"
                , "]"
                ]
      case parse input of
        BatchMessage elems -> length elems `shouldBe` 3
        other -> expectationFailure ("expected BatchMessage, got: " ++ show other)

  describe "Edge cases" $ do
    it "absent params -> NoParams" $ do
      let input = "{\"jsonrpc\":\"2.0\",\"method\":\"foo\",\"id\":1}"
      case parse (LBS.pack input) of
        SingleMessage (MsgRequest req) ->
          requestParams req `shouldBe` NoParams
        other -> expectationFailure ("expected MsgRequest, got: " ++ show other)

    it "params: [] -> ParamsByPosition empty" $ do
      let input = "{\"jsonrpc\":\"2.0\",\"method\":\"foo\",\"params\":[],\"id\":1}"
      case parse (LBS.pack input) of
        SingleMessage (MsgRequest req) ->
          requestParams req `shouldBe` ParamsByPosition mempty
        other -> expectationFailure ("expected MsgRequest, got: " ++ show other)

    it "params: {} -> ParamsByName empty" $ do
      let input = "{\"jsonrpc\":\"2.0\",\"method\":\"foo\",\"params\":{},\"id\":1}"
      case parse (LBS.pack input) of
        SingleMessage (MsgRequest req) ->
          requestParams req `shouldBe` ParamsByName mempty
        other -> expectationFailure ("expected MsgRequest, got: " ++ show other)

    it "id: null -> Request with Nothing id" $ do
      let input = "{\"jsonrpc\":\"2.0\",\"method\":\"foo\",\"id\":null}"
      case parse (LBS.pack input) of
        SingleMessage (MsgRequest req) ->
          requestId req `shouldBe` Nothing
        other -> expectationFailure ("expected MsgRequest with null id, got: " ++ show other)

    it "missing jsonrpc field -> invalid" $ do
      let input = "{\"method\":\"foo\",\"id\":1}"
      case parse (LBS.pack input) of
        SingleMessage (MsgResponse (ResponseFailure _)) -> pure ()
        other -> expectationFailure ("expected failure, got: " ++ show other)

    it "bare primitive at top level -> ParseFailed" $ do
      case parse "42" of
        ParseFailed _ -> pure ()
        other -> expectationFailure ("expected ParseFailed, got: " ++ show other)

  describe "Serialization" $ do
    it "jsonrpc field is always 2.0" $ property $
      \msg ->
        let bs = serialize (msg :: Message)
         in case eitherDecode' bs of
              Right (Object o) -> case KM.lookup "jsonrpc" o of
                Just (String "2.0") -> True
                _ -> False
              _ -> False

    it "serializeResponse produces valid JSON" $ property $
      \resp ->
        let bs = serializeResponse (resp :: Response)
         in case eitherDecode' bs :: Either String Value of
              Right _ -> True
              Left _ -> False

    it "serializeBatch produces a JSON array" $ do
      let msgs = [MsgRequest (Request "m" NoParams (Just (IdNum 1)))]
      let bs = serializeBatch msgs
      case eitherDecode' bs :: Either String Value of
        Right (Array _) -> pure ()
        other -> expectationFailure ("expected JSON array, got: " ++ show other)

isInvalidElement :: BatchElement -> Bool
isInvalidElement (InvalidElement _) = True
isInvalidElement _ = False
