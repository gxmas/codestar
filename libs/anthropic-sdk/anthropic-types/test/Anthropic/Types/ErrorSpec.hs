module Anthropic.Types.ErrorSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON, object, (.=))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Types
import Anthropic.Types.Generators ()

spec :: Spec
spec = do
  describe "ErrorType" $ do
    prop "roundtrip" $ \(x :: ErrorType) ->
      eitherDecode (encode x) === Right x

    it "InvalidRequestError -> \"invalid_request_error\"" $
      toJSON InvalidRequestError `shouldBe` "invalid_request_error"

    it "RateLimitError -> \"rate_limit_error\"" $
      toJSON RateLimitError `shouldBe` "rate_limit_error"

    it "InternalApiError -> \"api_error\"" $
      toJSON InternalApiError `shouldBe` "api_error"

  describe "ApiError" $ do
    prop "roundtrip" $ \(x :: ApiError) ->
      eitherDecode (encode x) === Right x

    it "wire format" $
      toJSON (ApiError InvalidRequestError "bad request")
        `shouldBe` object ["type" .= ("invalid_request_error" :: String), "message" .= ("bad request" :: String)]
