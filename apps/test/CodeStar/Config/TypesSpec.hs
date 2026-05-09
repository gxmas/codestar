{-# LANGUAGE OverloadedStrings #-}

module CodeStar.Config.TypesSpec (spec) where

import Data.Aeson (decode)
import Test.Hspec

import CodeStar.Config.Types

spec :: Spec
spec = describe "CodeStar.Config.Types" $ do
  it "redacts ApiKey in Show instance" $
    show (ApiKey "secret-value") `shouldBe` "ApiKey \"<redacted>\""

  it "parses IndexStrategy and McpTransport enums from JSON text" $ do
    decode "\"repomap\"" `shouldBe` Just RepoMapIndex
    decode "\"semantic\"" `shouldBe` Just SemanticIndex
    decode "\"stdio\"" `shouldBe` Just StdioTransport
    decode "\"http\"" `shouldBe` Just HttpTransport

  it "rejects unknown enum values during JSON decode" $ do
    (decode "\"bogus-index\"" :: Maybe IndexStrategy) `shouldBe` Nothing
    (decode "\"bogus-transport\"" :: Maybe McpTransport) `shouldBe` Nothing
