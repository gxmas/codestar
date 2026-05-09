{-# LANGUAGE OverloadedStrings #-}

module CodeStar.Platform.AuthSpec (spec) where

import Test.Hspec

import CodeStar.Platform.Auth
import CodeStar.Types (UserId (..))

spec :: Spec
spec = describe "CodeStar.Platform.Auth" $ do
  it "noAuth returns authenticated anonymous identity" $ do
    res <- noAuth
    case res of
      Unauthenticated e -> expectationFailure ("Expected Authenticated, got: " <> show e)
      Authenticated ident -> do
        show ident `shouldSatisfy` (not . null)

  it "NoAuthConfig ignores header and authenticates" $ do
    res <- authenticate NoAuthConfig "anything"
    res `shouldSatisfy` isAuthenticated

  it "ApiKeyConfig accepts valid bearer token" $ do
    let cfg = ApiKeyConfig (== "secret-token")
    res <- authenticate cfg "Bearer secret-token"
    res `shouldSatisfy` isAuthenticated

  it "ApiKeyConfig rejects malformed or invalid header" $ do
    let cfg = ApiKeyConfig (== "secret-token")
    r1 <- authenticate cfg "secret-token"
    r2 <- authenticate cfg "Bearer wrong"
    r1 `shouldSatisfy` isUnauthenticated
    r2 `shouldSatisfy` isUnauthenticated

  it "ApiKeyConfig rejects empty bearer token" $ do
    let cfg = ApiKeyConfig (== "")
    res <- authenticate cfg "Bearer "
    res `shouldSatisfy` isUnauthenticated

  it "ApiKeyConfig derives stable user prefix from token" $ do
    let cfg = ApiKeyConfig (== "1234567890abcdef")
    res <- authenticate cfg "Bearer 1234567890abcdef"
    case res of
      Unauthenticated e -> expectationFailure ("Expected Authenticated, got: " <> show e)
      Authenticated ident ->
        ident.userId `shouldBe` UserId "api-12345678"

isAuthenticated :: AuthResult -> Bool
isAuthenticated Authenticated{} = True
isAuthenticated _ = False

isUnauthenticated :: AuthResult -> Bool
isUnauthenticated Unauthenticated{} = True
isUnauthenticated _ = False
