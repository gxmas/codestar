{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shared QuickCheck generators for codestar LLM types.
module CodeStar.LLM.Gen
  ( arbitraryText
  , arbitraryNonMarkerContent
  , genHistory
  , markersIn
  , stripMarkers
  ) where

import Data.Aeson (Value (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Test.QuickCheck

import CodeStar.LLM.Base
  ( Content (..)
  , Message (..)
  , Role (..)
  , ToolCall (..)
  , ToolCallId (..)
  , ToolName (..)
  , ToolResult (..)
  )

-- The cache_control sentinel used by CodeStar.History.cacheMarker. Kept here
-- so generators can avoid producing colliding text by accident.
cacheMarkerText :: Text
cacheMarkerText = "\x0000cache_control"

-- --------------------------------------------------------------------
-- Text and tool identifiers
-- --------------------------------------------------------------------

-- | Plain ASCII text, never matching the cache marker.
arbitraryText :: Gen Text
arbitraryText =
  (Text.pack <$> listOf (elements alphabet))
    `suchThat` (/= cacheMarkerText)
 where
  alphabet = ['a' .. 'z'] ++ ['0' .. '9'] ++ " ."

instance Arbitrary ToolName where
  arbitrary = ToolName . Text.pack <$> listOf1 (elements ['a' .. 'z'])
  shrink (ToolName t) =
    [ ToolName t'
    | t' <- map Text.pack (shrink (Text.unpack t))
    , not (Text.null t')
    , Text.all (\c -> c >= 'a' && c <= 'z') t'
    ]

instance Arbitrary ToolCallId where
  arbitrary = do
    suffix <- vectorOf 8 (elements (['a' .. 'z'] ++ ['0' .. '9']))
    pure (ToolCallId (Text.pack ("toolu_" <> suffix)))
  shrink (ToolCallId tid) =
    [ ToolCallId (Text.pack ("toolu_" <> suffix))
    | suffix <- shrink suffixPart
    , not (null suffix)
    , all (\c -> (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')) suffix
    ]
   where
    raw = Text.unpack tid
    suffixPart = case dropPrefix "toolu_" raw of
      Just s -> s
      Nothing -> raw
    dropPrefix [] ys = Just ys
    dropPrefix _ [] = Nothing
    dropPrefix (x : xs) (y : ys)
      | x == y = dropPrefix xs ys
      | otherwise = Nothing

-- --------------------------------------------------------------------
-- Content
-- --------------------------------------------------------------------

instance Arbitrary ToolCall where
  arbitrary =
    ToolCall
      <$> arbitrary
      <*> arbitrary
      <*> pure (Object mempty)
  shrink tc =
    [tc{toolCallId = tcid} | tcid <- shrink tc.toolCallId]
      ++ [tc{toolName = tn} | tn <- shrink tc.toolName]

instance Arbitrary ToolResult where
  arbitrary =
    ToolResult
      <$> arbitrary
      <*> arbitraryText
      <*> arbitrary
  shrink tr =
    [tr{toolResultId = tcid} | tcid <- shrink tr.toolResultId]
      ++ [tr{resultBody = body} | body <- shrinkTextNonMarker tr.resultBody]
      ++ [tr{isError = e} | e <- shrink tr.isError]

-- | Generates Content that is never the cache_control sentinel.
arbitraryNonMarkerContent :: Gen Content
arbitraryNonMarkerContent =
  oneof
    [ TextContent <$> arbitraryText
    , ToolUseContent <$> arbitrary
    , ToolResultContent <$> arbitrary
    ]

instance Arbitrary Content where
  arbitrary = arbitraryNonMarkerContent
  shrink (TextContent t) =
    [TextContent t' | t' <- shrinkTextNonMarker t]
  shrink (ToolUseContent tc) =
    [ToolUseContent tc' | tc' <- shrink tc]
  shrink (ToolResultContent tr) =
    [ToolResultContent tr' | tr' <- shrink tr]

-- --------------------------------------------------------------------
-- Messages and history
-- --------------------------------------------------------------------

instance Arbitrary Role where
  arbitrary = elements [User, Assistant, System]

instance Arbitrary Message where
  arbitrary =
    Message
      <$> arbitrary
      <*> listOf arbitraryNonMarkerContent
  shrink m =
    [m{role = r} | r <- shrink m.role]
      ++ [m{content = c} | c <- shrink m.content]

-- | Generates a small message history.
genHistory :: Gen [Message]
genHistory = resize 6 (listOf arbitrary)

-- --------------------------------------------------------------------
-- Marker helpers
-- --------------------------------------------------------------------

-- | Count cache markers in a content list.
markersIn :: [Content] -> Int
markersIn = length . filter isMarker
 where
  isMarker (TextContent t) = t == cacheMarkerText
  isMarker _ = False

-- | Drop every cache marker from a content list.
stripMarkers :: [Content] -> [Content]
stripMarkers = filter (not . isMarker)
 where
  isMarker (TextContent t) = t == cacheMarkerText
  isMarker _ = False

shrinkTextNonMarker :: Text -> [Text]
shrinkTextNonMarker t =
  [ t'
  | t' <- map Text.pack (shrink (Text.unpack t))
  , t' /= cacheMarkerText
  ]
