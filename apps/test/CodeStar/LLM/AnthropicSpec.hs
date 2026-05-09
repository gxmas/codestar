module CodeStar.LLM.AnthropicSpec (spec) where

import Data.Aeson (Value (..), encode)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Test.Hspec
import Test.QuickCheck

import Anthropic.Protocol.Message qualified as AP
import Data.Aeson.Key qualified as AesonKey

import CodeStar.LLM.Anthropic (cacheMarkerText, toAnthropicContent, toAnthropicMessage)
import CodeStar.LLM.Base
  ( Content (..)
  , Message (..)
  , Role (..)
  , ToolCall (..)
  , ToolCallId (..)
  , ToolName (..)
  , ToolResult (..)
  )
import CodeStar.LLM.Gen (arbitraryText, stripMarkers)

-- --------------------------------------------------------------------
-- Helpers: peek into the wire-level representation by serializing to JSON
-- --------------------------------------------------------------------

-- | Encode an Anthropic Message to a JSON Value.
asJson :: AP.Message -> Value
asJson = Aeson.toJSON

{- | Pull the @content@ field of a serialized Message. Returns Left for
TextMessage (string), Right for BlockMessage (array of blocks).
-}
contentField :: Value -> Either Text [Value]
contentField (Object o) =
  case KM.lookup "content" o of
    Just (String t) -> Left t
    Just (Array vs) -> Right (V.toList vs)
    other -> error ("contentField: unexpected: " <> show other)
contentField v = error ("contentField: not an object: " <> show v)

blockType :: Value -> Maybe Text
blockType (Object o) = case KM.lookup "type" o of
  Just (String t) -> Just t
  _ -> Nothing
blockType _ = Nothing

hasCacheControl :: Value -> Bool
hasCacheControl (Object o) = KM.member "cache_control" o
hasCacheControl _ = False

cacheControlValue :: Value -> Maybe Value
cacheControlValue (Object o) = KM.lookup "cache_control" o
cacheControlValue _ = Nothing

-- --------------------------------------------------------------------
-- Cache marker helper that mirrors what cacheControlMarker produces.
-- --------------------------------------------------------------------

marker :: Content
marker = TextContent cacheMarkerText

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = describe "CodeStar.LLM.Anthropic" $ do
  describe "regression: bug from f83e433" $
    it "user message [marker, ToolResult] serializes with tool_result first and no NUL" $ do
      let trId = ToolCallId "toolu_01JuTvVmdSfWcWYyKLvNpSRC"
          trMsg =
            Message
              User
              [ marker
              , ToolResultContent (ToolResult trId "(no files matched pattern)" False)
              ]
          json = asJson (toAnthropicMessage trMsg)
      -- The marker text must never appear as a wire-level value.
      BL8.unpack (encode json) `shouldNotContainAny` ["\NUL", "\\u0000"]
      -- The first wire block must be a tool_result, with cache_control set.
      case contentField json of
        Right (b : _) -> do
          blockType b `shouldBe` Just "tool_result"
          hasCacheControl b `shouldBe` True
        other -> expectationFailure ("expected block array, got: " <> show other)

  describe "toAnthropicMessage with marker prefix" $ do
    it "never emits a text block whose text starts with NUL" $
      property $ \role contents ->
        let msg = Message (forSafe role) (marker : contents)
            blocks = either (const []) id (contentField (asJson (toAnthropicMessage msg)))
         in counterexample (show blocks) $
              all (not . textHasNul) blocks

    it "first content block carries cache_control when followed by a real block" $
      property $ \role c cs ->
        let msg = Message (forSafe role) (marker : c : cs)
         in case contentField (asJson (toAnthropicMessage msg)) of
              Right (b : _) ->
                counterexample ("first block: " <> show b) $
                  cacheControlValue b
                    === Just (Aeson.object [("type", String "ephemeral")])
              other -> counterexample (show other) (property False)

    it "preserves the type of the block following the marker" $
      property $ \role c cs ->
        let withMarker = Message (forSafe role) (marker : c : cs)
            withoutMarker = Message (forSafe role) (c : cs)
            -- TextMessage (string body) is the wire shape for a single
            -- TextContent; treat that as a "text" block for comparison.
            firstType m = case contentField (asJson (toAnthropicMessage m)) of
              Left _ -> Just ("text" :: Text)
              Right (b : _) -> blockType b
              Right [] -> Nothing
         in firstType withMarker === firstType withoutMarker

    it "trailing-only marker is dropped (no stray NUL text block)" $
      property $ \role contents ->
        let msg = Message (forSafe role) (contents <> [marker])
            blocks = either (const []) id (contentField (asJson (toAnthropicMessage msg)))
         in counterexample (show blocks) $ all (not . textHasNul) blocks

  describe "toAnthropicMessage without marker" $ do
    it "produces no cache_control fields anywhere" $
      property $ \role contents ->
        let msg = Message (forSafe role) (stripMarkers contents)
            blocks = either (const []) id (contentField (asJson (toAnthropicMessage msg)))
         in counterexample (show blocks) $ all (not . hasCacheControl) blocks

    it "single TextContent collapses to a JSON string body" $
      property $ \role -> forAll arbitraryText $ \t ->
        let msg = Message (forSafe role) [TextContent t]
         in case contentField (asJson (toAnthropicMessage msg)) of
              Left t' -> t' === t
              Right _ -> property False

    it "marker-stripped translation matches direct translation up to cache_control" $
      property $ \role contents ->
        let withMarker = Message (forSafe role) (marker : contents)
            withoutMarker = Message (forSafe role) (stripMarkers contents)
            -- Compare on a normalized form: TextMessage is lifted to a single
            -- text block so the BlockMessage path (forced by cache_control)
            -- can match it once cache_control is stripped.
            normalized m =
              case contentField (asJson (toAnthropicMessage m)) of
                Left t -> [Aeson.object [("type", String "text"), ("text", String t)]]
                Right bs -> bs
         in stripCacheControl (normalized withMarker) === normalized withoutMarker

  describe "toAnthropicContent boundary cases" $ do
    it "single marker + single TextContent yields a one-block BlockMessage with cache_control" $
      let mc = toAnthropicContent [marker, TextContent "hello"]
          json = Aeson.toJSON mc
       in case json of
            Array vs | V.length vs == 1 -> do
              let b = V.head vs
              blockType b `shouldBe` Just "text"
              hasCacheControl b `shouldBe` True
            _ -> expectationFailure ("expected 1-block array, got: " <> show json)

    it "tool_use ids match their tool_result counterparts in serialized output" $
      property $
        forAll ((,) <$> arbitraryText <*> arbitraryText) $ \(name, body) ->
          let tcid = ToolCallId "toolu_abc12345"
              useMsg =
                Message
                  Assistant
                  [ToolUseContent (ToolCall tcid (ToolName name) (Object mempty))]
              resMsg =
                Message
                  User
                  [marker, ToolResultContent (ToolResult tcid body False)]
              useJson = asJson (toAnthropicMessage useMsg)
              resJson = asJson (toAnthropicMessage resMsg)
           in case (contentField useJson, contentField resJson) of
                (Right (u : _), Right (r : _)) ->
                  (idField "id" u, idField "tool_use_id" r)
                    === (Just "toolu_abc12345", Just "toolu_abc12345")
                _ -> property False

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

{- | Collapse System role to User for Arbitrary cases — toAnthropicMessage
errors on System, since System is filtered earlier in the pipeline.
-}
forSafe :: Role -> Role
forSafe System = User
forSafe r = r

-- | True if the block is a text block whose text contains a NUL char.
textHasNul :: Value -> Bool
textHasNul (Object o) = case (KM.lookup "type" o, KM.lookup "text" o) of
  (Just (String "text"), Just (String t)) -> Text.any (== '\NUL') t
  _ -> False
textHasNul _ = False

-- | Remove cache_control fields from every block in a list.
stripCacheControl :: [Value] -> [Value]
stripCacheControl = map go
 where
  go (Object o) = Object (KM.delete "cache_control" o)
  go v = v

idField :: Text -> Value -> Maybe Text
idField k (Object o) = case KM.lookup (AesonKey.fromText k) o of
  Just (String t) -> Just t
  _ -> Nothing
idField _ _ = Nothing

shouldNotContainAny :: (HasCallStack) => String -> [String] -> Expectation
shouldNotContainAny haystack needles =
  case mapMaybe (\n -> if n `isInfixOf` haystack then Just n else Nothing) needles of
    [] -> pure ()
    hs ->
      expectationFailure
        ( "payload contains forbidden substrings: "
            <> show hs
            <> "\npayload: "
            <> haystack
        )
 where
  isInfixOf needle hay
    | null needle = True
    | otherwise =
        let n = length needle
         in any (\i -> take n (drop i hay) == needle) [0 .. length hay - n]
