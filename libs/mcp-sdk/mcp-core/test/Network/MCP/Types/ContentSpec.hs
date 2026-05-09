module Network.MCP.Types.ContentSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Network.MCP.Generators ()
import Network.MCP.Types.Content

lookupKey :: Aeson.Key -> Aeson.Value -> Maybe Aeson.Value
lookupKey k (Aeson.Object o) = KM.lookup k o
lookupKey _ _ = Nothing

spec :: Spec
spec = do
  describe "URI" $
    prop "roundtrip" $ \(x :: URI) ->
      eitherDecode (encode x) === Right x

  describe "Role" $ do
    prop "roundtrip" $ \(x :: Role) ->
      eitherDecode (encode x) === Right x

    it "RoleUser -> user" $
      toJSON RoleUser `shouldBe` "user"

    it "RoleAssistant -> assistant" $
      toJSON RoleAssistant `shouldBe` "assistant"

  describe "IconTheme" $
    prop "roundtrip" $ \(x :: IconTheme) ->
      eitherDecode (encode x) === Right x

  describe "Annotations" $
    prop "roundtrip" $ \(x :: Annotations) ->
      eitherDecode (encode x) === Right x

  describe "Icon" $
    prop "roundtrip" $ \(x :: Icon) ->
      eitherDecode (encode x) === Right x

  describe "TextContent" $
    prop "roundtrip" $ \(x :: TextContent) ->
      eitherDecode (encode x) === Right x

  describe "ImageContent" $
    prop "roundtrip" $ \(x :: ImageContent) ->
      eitherDecode (encode x) === Right x

  describe "AudioContent" $
    prop "roundtrip" $ \(x :: AudioContent) ->
      eitherDecode (encode x) === Right x

  describe "ResourceLink" $
    prop "roundtrip" $ \(x :: ResourceLink) ->
      eitherDecode (encode x) === Right x

  describe "TextResource" $
    prop "roundtrip" $ \(x :: TextResource) ->
      eitherDecode (encode x) === Right x

  describe "BlobResource" $
    prop "roundtrip" $ \(x :: BlobResource) ->
      eitherDecode (encode x) === Right x

  describe "ResourceContents" $ do
    prop "roundtrip" $ \(x :: ResourceContents) ->
      eitherDecode (encode x) === Right x

    it "text resource has 'text' key, no 'blob'" $ do
      let r = TextResource (URI "file:///x") Nothing "hello"
          v = toJSON (ResourceText r)
      lookupKey "text" v `shouldNotBe` Nothing
      lookupKey "blob" v `shouldBe` Nothing

    it "blob resource has 'blob' key, no 'text'" $ do
      let r = BlobResource (URI "file:///x") Nothing "raw"
          v = toJSON (ResourceBlob r)
      lookupKey "blob" v `shouldNotBe` Nothing
      lookupKey "text" v `shouldBe` Nothing

  describe "EmbeddedResource" $
    prop "roundtrip" $ \(x :: EmbeddedResource) ->
      eitherDecode (encode x) === Right x

  describe "ContentBlock" $ do
    prop "roundtrip" $ \(x :: ContentBlock) ->
      eitherDecode (encode x) === Right x

    it "ContentText has type 'text'" $ do
      let v = toJSON (ContentText (TextContent "hi" Nothing))
      lookupKey "type" v `shouldBe` Just "text"

    it "ContentImage has type 'image'" $ do
      let v = toJSON (ContentImage (ImageContent "raw" "image/png" Nothing))
      lookupKey "type" v `shouldBe` Just "image"

    it "ContentAudio has type 'audio'" $ do
      let v = toJSON (ContentAudio (AudioContent "raw" "audio/wav" Nothing))
      lookupKey "type" v `shouldBe` Just "audio"

    it "ContentLink has type 'resource_link'" $ do
      let v = toJSON (ContentLink (ResourceLink (URI "x") "name" Nothing Nothing))
      lookupKey "type" v `shouldBe` Just "resource_link"

    it "ContentEmbedded has type 'resource'" $ do
      let r = EmbeddedResource (ResourceText (TextResource (URI "x") Nothing "t")) Nothing
          v = toJSON (ContentEmbedded r)
      lookupKey "type" v `shouldBe` Just "resource"
