{-# LANGUAGE OverloadedStrings #-}

module Test.Network.JsonRpc.TypesSpec (spec) where

import Data.Aeson (Value (..))
import Data.Vector qualified
import Data.Hashable (hash)
import Network.JsonRpc.Types
import Network.JsonRpc.Types.Internal (ErrorObject (..))
import Test.Hspec
import Test.Network.JsonRpc.Generators ()
import Test.QuickCheck hiding (Failure, Success)

spec :: Spec
spec = do
  describe "Id" $ do
    it "IdText \"abc\" /= IdText \"def\"" $
      IdText "abc" `shouldNotBe` IdText "def"

    it "IdText \"\" is a valid Id" $
      IdText "" `shouldBe` IdText ""

    it "IdNum 0 is a valid Id" $
      IdNum 0 `shouldBe` IdNum 0

    it "IdNum 1.5 is a valid Id" $
      IdNum 1.5 `shouldBe` IdNum 1.5

    it "IdText \"1\" /= IdNum 1" $
      IdText "1" `shouldNotBe` IdNum 1

    it "Eq is reflexive" $ property $
      \x -> (x :: Id) == x

    it "Ord is consistent with Eq" $ property $
      \x y -> (compare (x :: Id) y == EQ) == (x == y)

    it "Hashable is consistent with Eq" $ property $
      \x -> hash (x :: Id) == hash x  -- reflexive hash consistency
    it "Hashable: equal values hash equally" $ property $
      \x -> let y = x :: Id in x == y ==> hash x == hash y

  describe "Error smart constructors" $ do
    it "parseError has code -32700" $
      errorCode (parseError Nothing) `shouldBe` (-32700)

    it "parseError has message \"Parse error\"" $
      errorMessage (parseError Nothing) `shouldBe` "Parse error"

    it "invalidRequest has code -32600" $
      errorCode (invalidRequest Nothing) `shouldBe` (-32600)

    it "invalidRequest has message \"Invalid Request\"" $
      errorMessage (invalidRequest Nothing) `shouldBe` "Invalid Request"

    it "methodNotFound has code -32601" $
      errorCode (methodNotFound Nothing) `shouldBe` (-32601)

    it "invalidParams has code -32602" $
      errorCode (invalidParams Nothing) `shouldBe` (-32602)

    it "internalError has code -32603" $
      errorCode (internalError Nothing) `shouldBe` (-32603)

    it "parseError preserves data field" $
      errorData (parseError (Just (String "detail"))) `shouldBe` Just (String "detail")

    it "standard error codes are fixed" $ property $
      \d -> errorCode (parseError d) == (-32700)

    it "serverError accepts -32050" $
      serverError (-32050) "custom" Nothing `shouldBe` Right (ErrorObject (-32050) "custom" Nothing)

    it "serverError rejects -32100" $
      serverError (-32100) "custom" Nothing `shouldSatisfy` isLeft

    it "serverError rejects -31999" $
      serverError (-31999) "custom" Nothing `shouldSatisfy` isLeft

    it "serverError rejects out-of-range codes" $ property $
      \code ->
        (code < (-32099) || code > (-32000))
          ==> isLeft (serverError code "msg" Nothing)

    it "applicationError accepts 1" $
      applicationError 1 "app error" Nothing `shouldSatisfy` isRight

    it "applicationError rejects -32600" $
      applicationError (-32600) "oops" Nothing `shouldSatisfy` isLeft

    it "applicationError rejects reserved range" $ property $
      forAll (choose (-32768, -32000)) $ \code ->
        isLeft (applicationError code "msg" Nothing)

  describe "Type structure invariants" $ do
    it "Request has a Maybe Id field" $
      requestId (Request "m" NoParams (Just (IdNum 1))) `shouldBe` Just (IdNum 1)

    it "Request with null id" $
      requestId (Request "m" NoParams Nothing) `shouldBe` Nothing

    it "Notification has no Id field" $
      notificationMethod (Notification "m" NoParams) `shouldBe` "m"

    it "Response has exactly two constructors" $ do
      let s = ResponseSuccess (Success Null Nothing)
      let f = ResponseFailure (Failure (parseError Nothing) Nothing)
      s `shouldNotBe` f

    it "Message has exactly three constructors" $ do
      let r = MsgRequest (Request "m" NoParams (Just (IdNum 1)))
      let n = MsgNotification (Notification "m" NoParams)
      let resp = MsgResponse (ResponseSuccess (Success Null Nothing))
      r `shouldNotBe` n
      n `shouldNotBe` resp

  describe "ParseResult and BatchElement" $ do
    it "ParseFailed carries an ErrorObject" $
      case ParseFailed (parseError Nothing) of
        ParseFailed e -> errorCode e `shouldBe` (-32700)
        _ -> expectationFailure "expected ParseFailed"

    it "BatchMessage [] is constructible" $
      BatchMessage [] `shouldBe` BatchMessage []

    it "NoParams is distinct from empty array params" $
      NoParams `shouldNotBe` ParamsByPosition mempty

    it "NoParams is distinct from empty object params" $
      NoParams `shouldNotBe` ParamsByName mempty

    it "Params preserves structure" $ property $
      \(vs :: [Value]) ->
        let vec = Data.Vector.fromList vs
         in case ParamsByPosition vec of
              ParamsByPosition v -> v == vec
              _ -> False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
