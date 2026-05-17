module Network.MCP.Auth.PkceSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Network.MCP.Auth.Pkce

spec :: Spec
spec = do
  describe "generatePkce" $ do
    it "produces a verifier of 43 characters" $ do
      pkce <- generatePkce
      T.length pkce.pkceVerifier `shouldBe` 43

    it "method is always S256" $ do
      pkce <- generatePkce
      pkce.pkceMethod `shouldBe` "S256"

    it "challenge is 43 characters (SHA-256 base64url no-padding)" $ do
      pkce <- generatePkce
      T.length pkce.pkceChallenge `shouldBe` 43

    it "verifier and challenge are different" $ do
      pkce <- generatePkce
      pkce.pkceVerifier `shouldNotBe` pkce.pkceChallenge

    it "verifyChallenge confirms verifier-challenge pairing" $ do
      pkce <- generatePkce
      verifyChallenge pkce.pkceVerifier pkce.pkceChallenge `shouldBe` True

    it "two generatePkce calls produce different verifiers" $ do
      pkce1 <- generatePkce
      pkce2 <- generatePkce
      pkce1.pkceVerifier `shouldNotBe` pkce2.pkceVerifier

  describe "verifyChallenge" $ do
    it "wrong verifier fails verification" $ do
      pkce <- generatePkce
      verifyChallenge "wrong-verifier-value-wrong" pkce.pkceChallenge `shouldBe` False

    -- Known test vector from RFC 7636 Appendix B:
    -- verifier  = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
    -- challenge = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
    it "RFC 7636 Appendix B test vector" $ do
      let verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
          challenge = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
      verifyChallenge verifier challenge `shouldBe` True
