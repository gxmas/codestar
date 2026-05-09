module Anthropic.Types.PaginationSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON, object, (.=))
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Anthropic.Types
import Anthropic.Types.Generators ()

spec :: Spec
spec = do
  describe "Page" $ do
    prop "roundtrip (Page Text)" $ \(x :: Page Text) ->
      eitherDecode (encode x) === Right x

    it "wire format field names" $ do
      let p = Page
            { pageData = ["a" :: Text, "b"]
            , hasMore  = True
            , firstId  = Just "id1"
            , lastId   = Just "id2"
            }
      toJSON p `shouldBe` object
        [ "data"     .= (["a", "b"] :: [Text])
        , "has_more" .= True
        , "first_id" .= ("id1" :: Text)
        , "last_id"  .= ("id2" :: Text)
        ]
