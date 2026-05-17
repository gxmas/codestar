module Network.MCP.Types.CapabilitiesSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Network.MCP.Generators ()
import Network.MCP.Types.Capabilities

lookupKey :: Aeson.Key -> Aeson.Value -> Maybe Aeson.Value
lookupKey k (Aeson.Object o) = KM.lookup k o
lookupKey _ _ = Nothing

spec :: Spec
spec = do
  describe "ClientCapabilities" $
    prop "roundtrip" $ \(x :: ClientCapabilities) ->
      eitherDecode (encode x) === Right x

  describe "ServerCapabilities" $ do
    prop "roundtrip" $ \(x :: ServerCapabilities) ->
      eitherDecode (encode x) === Right x

    it "logging Just () encodes as {}" $ do
      let sc =
            ServerCapabilities Nothing Nothing Nothing (Just ()) Nothing Nothing Nothing
          v = toJSON sc
      lookupKey "logging" v `shouldBe` Just (Aeson.object [])

    it "logging Nothing omits the key" $ do
      let sc =
            ServerCapabilities Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          v = toJSON sc
      lookupKey "logging" v `shouldBe` Nothing

  describe "SamplingCapability" $ do
    prop "roundtrip" $ \(x :: SamplingCapability) ->
      eitherDecode (encode x) === Right x

    it "serialises as empty object" $ do
      let v = toJSON SamplingCapability
      v `shouldBe` Aeson.object []

    -- Fix 3: SamplingCapability serialises as {} — verify the JSON
    -- structure, not just round-trip equality (which is trivially true
    -- for a nullary constructor).
    it "toJSON produces exactly {} (Aeson.object [])" $
      toJSON SamplingCapability `shouldBe` Aeson.object []

    it "ClientCapabilities with sampling encodes sampling as {}" $ do
      let cc = ClientCapabilities Nothing (Just SamplingCapability) Nothing Nothing Nothing
          v  = toJSON cc
      lookupKey "sampling" v `shouldBe` Just (Aeson.object [])

    it "ClientCapabilities sampling key contains no extra keys" $ do
      let cc = ClientCapabilities Nothing (Just SamplingCapability) Nothing Nothing Nothing
          v  = toJSON cc
      case lookupKey "sampling" v of
        Just (Aeson.Object o) -> KM.size o `shouldBe` 0
        other -> expectationFailure $
          "Expected sampling to be an empty object, got: " ++ show other

  describe "NegotiatedCapabilities" $
    prop "roundtrip" $ \(x :: NegotiatedCapabilities) ->
      eitherDecode (encode x) === Right x

  describe "TaskStatus" $ do
    prop "roundtrip" $ \(x :: TaskStatus) ->
      eitherDecode (encode x) === Right x

    it "all 5 constructors" $
      length [minBound .. maxBound :: TaskStatus] `shouldBe` 5

  describe "TaskErrorKind" $
    prop "roundtrip" $ \(x :: TaskErrorKind) ->
      eitherDecode (encode x) === Right x

  describe "TaskError" $
    prop "roundtrip" $ \(x :: TaskError) ->
      eitherDecode (encode x) === Right x

  describe "empty object decodes" $
    it "empty {} decodes to all-Nothing ClientCapabilities" $ do
      let json = "{}"
      case eitherDecode json :: Either String ClientCapabilities of
        Right cc -> do
          cc `shouldBe` ClientCapabilities Nothing Nothing Nothing Nothing Nothing
        Left err -> expectationFailure $ "Decode failed: " ++ err
