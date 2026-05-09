module CodeStar.LLM.OpenAISpec (spec) where

import Data.Aeson (Value (..), encode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.QuickCheck

import OpenAI.Types qualified as OT

import CodeStar.LLM.Base
  ( Content (..)
  , Message (..)
  , Role (..)
  , ToolCall (..)
  , ToolCallId (..)
  , ToolName (..)
  , ToolResult (..)
  )
import CodeStar.LLM.Gen (stripMarkers)
import CodeStar.LLM.OpenAI (cacheMarkerText, toOAIMessage)

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

{- | Extract the text body produced by toOAIMessage. Always TextContent in
this adapter.
-}
oaiText :: OT.Message -> Text
oaiText m = case m.content of
  OT.TextContent t -> t
  OT.PartsContent _ -> error "unexpected PartsContent from toOAIMessage"

marker :: Content
marker = TextContent cacheMarkerText

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = describe "CodeStar.LLM.OpenAI" $ do
  describe "regression: marker must not leak into prompts" $
    it "user message [marker, TextContent 'hello world'] yields plain text" $ do
      let msg = Message User [marker, TextContent "hello world"]
          out = oaiText (toOAIMessage msg)
      out `shouldBe` "hello world"
      BL8.unpack (encode out) `shouldNotContain` "\\u0000"
      BL8.unpack (encode out) `shouldNotContain` "cache_control"

  describe "toOAIMessage" $ do
    it "the resulting text never contains the cache marker substring" $
      property $ \role contents ->
        let msg = Message role (marker : contents)
            t = oaiText (toOAIMessage msg)
         in counterexample (show t) $
              not (cacheMarkerText `Text.isInfixOf` t)

    it "the resulting text never contains a literal NUL character" $
      property $ \role contents ->
        let msg = Message role (marker : contents)
            t = oaiText (toOAIMessage msg)
         in counterexample (show t) $ not (Text.any (== '\NUL') t)

    it "marker is content-equivalent: stripping markers gives the same text" $
      property $ \role contents ->
        let withMarker = Message role contents
            withoutMarker = Message role (stripMarkers contents)
         in oaiText (toOAIMessage withMarker)
              === oaiText (toOAIMessage withoutMarker)

    it "marker placement (front, back, middle) does not change the output" $
      property $ \role c cs ->
        let front = Message role (marker : c : cs)
            back = Message role (c : cs <> [marker])
            middle = Message role ([c, marker] <> cs)
            out m = oaiText (toOAIMessage m)
         in out front === out back .&&. out back === out middle

    it "non-text content is silently dropped (text-only adapter)" $ do
      let toolCall = ToolUseContent (ToolCall (ToolCallId "toolu_xyz") (ToolName "glob") (Object mempty))
          toolRes = ToolResultContent (ToolResult (ToolCallId "toolu_xyz") "result" False)
          msg = Message User [toolCall, toolRes, TextContent "after"]
      oaiText (toOAIMessage msg) `shouldBe` "after"

  describe "translate equals concatenation of non-marker text contents" $
    it "matches the documented mconcat-of-text-blocks semantic" $
      property $ \role contents ->
        let msg = Message role contents
            expected =
              Text.concat
                [t | TextContent t <- contents, t /= cacheMarkerText]
         in oaiText (toOAIMessage msg) === expected
