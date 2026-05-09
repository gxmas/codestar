module Anthropic.Client.Internal.HttpSpec (spec) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import Network.HTTP.Client (method, requestHeaders, requestBody, RequestBody(..))
import Test.Hspec

import Anthropic.Types (ApiKey(..))
import Anthropic.Client.Internal.Http

spec :: Spec
spec = do
  describe "buildGetRequest" $ do
    it "sets GET method" $ do
      let req = buildGetRequest (ApiKey "sk-test") "https://api.anthropic.com" "/v1/models" Nothing Nothing Nothing
      method req `shouldBe` "GET"

    it "injects x-api-key header" $ do
      let req = buildGetRequest (ApiKey "sk-test-key") "https://api.anthropic.com" "/v1/models" Nothing Nothing Nothing
          hdrs = requestHeaders req
      lookup (CI.mk "x-api-key") hdrs `shouldBe` Just "sk-test-key"

    it "injects anthropic-version header" $ do
      let req = buildGetRequest (ApiKey "sk-test") "https://api.anthropic.com" "/v1/models" Nothing Nothing Nothing
          hdrs = requestHeaders req
      lookup (CI.mk "anthropic-version") hdrs `shouldBe` Just apiVersion

    it "injects content-type header" $ do
      let req = buildGetRequest (ApiKey "sk-test") "https://api.anthropic.com" "/v1/models" Nothing Nothing Nothing
          hdrs = requestHeaders req
      lookup (CI.mk "content-type") hdrs `shouldBe` Just "application/json"

    it "includes anthropic-beta header when beta features provided" $ do
      let req = buildGetRequest (ApiKey "sk-test") "https://api.anthropic.com" "/v1/models"
                  Nothing (Just ["feature-1", "feature-2"]) Nothing
          hdrs = requestHeaders req
      lookup (CI.mk "anthropic-beta") hdrs `shouldBe` Just "feature-1,feature-2"

    it "includes additional default headers" $ do
      let extraHdrs = [(BS8.pack "X-Custom", BS8.pack "value")]
          req = buildGetRequest (ApiKey "sk-test") "https://api.anthropic.com" "/v1/models"
                  (Just extraHdrs) Nothing Nothing
          hdrs = requestHeaders req
      lookup (CI.mk "X-Custom") hdrs `shouldBe` Just "value"

  describe "buildRequest" $ do
    it "sets POST method" $ do
      let req = buildRequest (ApiKey "sk-test") "https://api.anthropic.com" "/v1/messages"
                  Nothing Nothing Nothing ("test" :: Text)
      method req `shouldBe` "POST"

    it "sets request body" $ do
      let req = buildRequest (ApiKey "sk-test") "https://api.anthropic.com" "/v1/messages"
                  Nothing Nothing Nothing ("test" :: Text)
      case requestBody req of
        RequestBodyLBS _ -> pure ()  -- Expected
        _                -> expectationFailure "Expected RequestBodyLBS"

  describe "buildDeleteRequest" $ do
    it "sets DELETE method" $ do
      let req = buildDeleteRequest (ApiKey "sk-test") "https://api.anthropic.com" "/v1/batches/123"
                  Nothing Nothing Nothing
      method req `shouldBe` "DELETE"
