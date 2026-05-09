module CodeStar.HistorySpec (spec) where

import Data.Aeson (Value (..), toJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.QuickCheck

import Anthropic.Protocol.Message qualified as AP

import CodeStar.History
  ( cacheControlMarker
  , closedWindowTracker
  , defaultChain
  , elideOldObservations
  , processHistory
  , truncateObservations
  )
import CodeStar.LLM.Anthropic (cacheMarkerText, toAnthropicMessage)
import CodeStar.LLM.Base
  ( Content (..)
  , Message (..)
  , Role (..)
  , ToolCall (..)
  , ToolCallId (..)
  , ToolName (..)
  , ToolResult (..)
  )
import CodeStar.LLM.Gen (arbitraryNonMarkerContent, genHistory, markersIn)

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

isMarkerContent :: Content -> Bool
isMarkerContent (TextContent t) = t == cacheMarkerText
isMarkerContent _ = False

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = do
  describe "CodeStar.History.processors" $ do
    describe "truncateObservations" $ do
      it "never leaves ToolResult bodies longer than n + truncation overhead" $
        property $
          forAll (chooseInt (1, 200)) $ \n ->
            forAll genHistory $ \history ->
              let processed = toList (truncateObservations n (Seq.fromList history))
                  maxAllowed = n + truncationOverhead n
               in conjoin
                    [ case c of
                        ToolResultContent tr ->
                          counterexample
                            ("body length: " <> show (lengthText tr.resultBody))
                            (lengthText tr.resultBody <= maxAllowed)
                        _ -> property True
                    | msg <- processed
                    , c <- msg.content
                    ]

    describe "elideOldObservations" $ do
      it "leaves last k messages unchanged" $
        property $
          forAll (chooseInt (0, 10)) $ \k ->
            forAll genHistory $ \history ->
              let processed = toList (elideOldObservations k (Seq.fromList history))
                  tailOriginal = takeLast k history
                  tailProcessed = takeLast k processed
               in tailProcessed === tailOriginal

    describe "closedWindowTracker" $ do
      it "collapses read results only when a subsequent edit exists" $ do
        let readMsg =
              Message
                Assistant
                [ ToolUseContent (ToolCall (ToolCallId "toolu_read_1") (ToolName "read:fileA.hs") (Object mempty))
                , ToolResultContent (toolResult "toolu_read_1" "original read output")
                ]
            editMsg =
              Message
                Assistant
                [ ToolUseContent (ToolCall (ToolCallId "toolu_edit_1") (ToolName "edit:fileA.hs") (Object mempty))
                ]
            unrelatedRead =
              Message
                Assistant
                [ ToolUseContent (ToolCall (ToolCallId "toolu_read_2") (ToolName "read:fileB.hs") (Object mempty))
                , ToolResultContent (toolResult "toolu_read_2" "untouched read output")
                ]
            input = Seq.fromList [readMsg, editMsg, unrelatedRead]
            out = toList (closedWindowTracker input)
            readOut1 = extractToolResultBody (out !! 0)
            readOut2 = extractToolResultBody (out !! 2)
        readOut1 `shouldSatisfy` Text.isInfixOf "[Closed window:"
        readOut2 `shouldBe` "untouched read output"

    describe "defaultChain" $ do
      it "does not increase history length and preserves system messages" $
        property $
          forAll genHistory $ \history ->
            let input = Seq.fromList history
                out = processHistory defaultChain input
                sysIn = [m | m <- history, m.role == System]
                sysOut = [m | m <- toList out, m.role == System]
             in Seq.length out <= Seq.length input
                  .&&. sysOut === sysIn

  it "marks user messages whose position is within the last K of the history" $
    property $ \(Positive k) -> forAllShrink genHistory shrink $ \history ->
      let processed = toList (cacheControlMarker k (Seq.fromList history))
          n = length history
          cutoff = max 0 (n - k)
          expected =
            length
              [() | (i, m) <- zip [0 ..] history, i >= cutoff, m.role == User]
          marked = length [m | m <- processed, m.role == User, hasLeadingMarker m.content]
       in checkCoverage $
            classify (n > k) "history longer than keepLast" $
              classify (n <= k) "history not longer than keepLast" $
                cover 5 (n > k) "longer-than-keepLast cases" $
                  marked === expected

  it "non-User messages are returned unchanged" $
    property $ \(Positive k) -> forAllShrink genHistory shrink $ \history ->
      let processed = toList (cacheControlMarker k (Seq.fromList history))
          nonUserOriginal = [m | m <- history, m.role /= User]
          nonUserOut = [m | m <- processed, m.role /= User]
       in nonUserOriginal === nonUserOut

  it "removing the leading marker recovers the original content" $
    property $ \(Positive k) -> forAllShrink genHistory shrink $ \history ->
      let processed = toList (cacheControlMarker k (Seq.fromList history))
          unwound = map (\m -> m{content = stripLeadingMarker m.content}) processed
       in unwound === history

  it "adds at most one marker per user message" $
    property $ \(Positive k) -> forAllShrink genHistory shrink $ \history ->
      let processed = toList (cacheControlMarker k (Seq.fromList history))
       in conjoin
            [ counterexample ("too many markers: " <> show m) (markersIn m.content <= 1)
            | m <- processed
            ]

  it "marker is always the first content block when present" $
    property $ \(Positive k) -> forAllShrink genHistory shrink $ \history ->
      let processed = toList (cacheControlMarker k (Seq.fromList history))
       in conjoin
            [ case m.content of
                [] -> property True
                (c : _) | isMarkerContent c -> property True
                cs ->
                  counterexample
                    ("marker not first: " <> show cs)
                    (markersIn cs === 0)
            | m <- processed
            ]

  it "k = 0 leaves all messages untouched" $
    property $
      forAllShrink genHistory shrink $ \history ->
        toList (cacheControlMarker 0 (Seq.fromList history)) === history

  -- ----------------------------------------------------------------
  -- Multi-round marker accumulation (regression for cache_control > 4)
  -- ----------------------------------------------------------------

  describe "multi-round marker accumulation" $ do
    it "processing raw history never exceeds keepLast cache_control blocks" $
      property $
        forAllShrink genToolConversation shrink $ \(k, rounds) ->
          let history = simulateRounds k rounds
              processed = toList (cacheControlMarker k (Seq.fromList history))
              nonSystem = filter (\m -> m.role /= System) processed
              wireBlocks = concatMap (messageWireBlocks . toAnthropicMessage) nonSystem
              ccCount = length (filter hasCacheControlBlock wireBlocks)
           in counterexample
                ( "cache_control count: "
                    <> show ccCount
                    <> ", keepLast: "
                    <> show k
                    <> ", rounds: "
                    <> show (length rounds)
                )
                (ccCount <= k)

    it "re-processing already-processed history can exceed keepLast (documents old bug)" $
      property $
        forAllShrink (genToolConversation `suchThat` hasEnoughRounds) shrink $ \(k, rounds) ->
          let history = simulateRoundsBuggy k rounds
              processed = toList (cacheControlMarker k (Seq.fromList history))
              nonSystem = filter (\m -> m.role /= System) processed
              wireBlocks = concatMap (messageWireBlocks . toAnthropicMessage) nonSystem
              ccCount = length (filter hasCacheControlBlock wireBlocks)
           in counterexample
                ("cache_control count: " <> show ccCount <> ", keepLast: " <> show k)
                (ccCount > k)

    it "markers do not accumulate when raw history is preserved across rounds" $
      property $
        forAllShrink genToolConversation shrink $ \(k, rounds) ->
          let history = simulateRounds k rounds
              totalMarkers = sum [markersIn m.content | m <- history]
           in counterexample
                ("total markers in stored history: " <> show totalMarkers)
                (totalMarkers === 0)

    it "cache_control count stays at most 4 with the production keepLast=3" $
      property $
        forAllShrink genManyRounds shrink $ \rounds ->
          let k = 3
              history = simulateRounds k rounds
              processed = toList (cacheControlMarker k (Seq.fromList history))
              nonSystem = filter (\m -> m.role /= System) processed
              wireBlocks = concatMap (messageWireBlocks . toAnthropicMessage) nonSystem
              ccCount = length (filter hasCacheControlBlock wireBlocks)
           in counterexample
                ( "cache_control count: "
                    <> show ccCount
                    <> " after "
                    <> show (length rounds)
                    <> " rounds"
                )
                (ccCount <= 4)

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

hasLeadingMarker :: [Content] -> Bool
hasLeadingMarker (c : _) = isMarkerContent c
hasLeadingMarker _ = False

stripLeadingMarker :: [Content] -> [Content]
stripLeadingMarker (c : rest) | isMarkerContent c = rest
stripLeadingMarker cs = cs

-- --------------------------------------------------------------------
-- Multi-round simulation helpers
-- --------------------------------------------------------------------

{- | A single round of the agent loop: the assistant's response content
and the user's tool result content.
-}
data Round = Round
  { assistantContent :: [Content]
  , toolResultContent :: [Content]
  }
  deriving stock (Show)

instance Arbitrary Round where
  arbitrary = genRound
  shrink (Round assistant result) =
    [Round assistant' result | assistant' <- shrink assistant]
      ++ [Round assistant result' | result' <- shrink result]

-- | Generate a (keepLast, rounds) pair for property tests.
genToolConversation :: Gen (Int, [Round])
genToolConversation = do
  k <- choose (1, 5)
  n <- choose (1, 8)
  rounds <- vectorOf n arbitrary
  pure (k, rounds)

-- | Generate 4-10 rounds — enough to trigger the old bug with keepLast=3.
genManyRounds :: Gen [Round]
genManyRounds = do
  n <- choose (4, 10)
  vectorOf n arbitrary

genRound :: Gen Round
genRound = do
  tcId <- arbitrary :: Gen ToolCallId
  name <- arbitrary :: Gen ToolName
  body <- listOf1 arbitraryNonMarkerContent
  resultBody <- listOf1 arbitraryNonMarkerContent
  pure
    Round
      { assistantContent = [ToolUseContent (ToolCall tcId name (Object mempty))] <> body
      , toolResultContent = resultBody
      }

hasEnoughRounds :: (Int, [Round]) -> Bool
hasEnoughRounds (k, rounds) = length rounds > k + 1

{- | Simulate multiple agent-loop rounds using the FIXED approach:
markers are applied to a processed copy; the raw (marker-free) history
is what persists across rounds.
-}
simulateRounds :: Int -> [Round] -> [Message]
simulateRounds _ [] =
  [ Message System [TextContent "system prompt"]
  , Message User [TextContent "initial task"]
  ]
simulateRounds k rounds = go initial rounds
 where
  initial =
    [ Message System [TextContent "system prompt"]
    , Message User [TextContent "initial task"]
    ]
  go rawHistory [] = rawHistory
  go rawHistory (r : rest) =
    let _processed = toList (cacheControlMarker k (Seq.fromList rawHistory))
        rawHistory' =
          rawHistory
            ++ [Message Assistant r.assistantContent]
            ++ [Message User r.toolResultContent]
     in go rawHistory' rest

{- | Simulate the OLD buggy approach: the processed history (with markers)
is stored and reprocessed on the next round.
-}
simulateRoundsBuggy :: Int -> [Round] -> [Message]
simulateRoundsBuggy _ [] =
  [ Message System [TextContent "system prompt"]
  , Message User [TextContent "initial task"]
  ]
simulateRoundsBuggy k rounds = go initial rounds
 where
  initial =
    [ Message System [TextContent "system prompt"]
    , Message User [TextContent "initial task"]
    ]
  go history [] = history
  go history (r : rest) =
    let processed = toList (cacheControlMarker k (Seq.fromList history))
        history' =
          processed
            ++ [Message Assistant r.assistantContent]
            ++ [Message User r.toolResultContent]
     in go history' rest

-- --------------------------------------------------------------------
-- Wire-level inspection helpers
-- --------------------------------------------------------------------

-- | Extract wire-level content blocks from a serialized Anthropic message.
messageWireBlocks :: AP.Message -> [Value]
messageWireBlocks msg =
  case toJSON msg of
    Object o -> case KM.lookup "content" o of
      Just (Array vs) -> mapMaybe asObjectValue (toList vs)
      Just (String _) -> []
      _ -> []
    _ -> []
 where
  asObjectValue (Object ob) = Just (Object ob)
  asObjectValue _ = Nothing

-- | Check whether a wire-level block carries cache_control.
hasCacheControlBlock :: Value -> Bool
hasCacheControlBlock (Object o) = KM.member "cache_control" o
hasCacheControlBlock _ = False

lengthText :: Text -> Int
lengthText = Text.length

truncationOverhead :: Int -> Int
truncationOverhead n =
  Text.length ("\n[...truncated at " <> Text.pack (show n) <> " chars...]")

takeLast :: Int -> [a] -> [a]
takeLast k xs
  | k <= 0 = []
  | otherwise = drop (max 0 (length xs - k)) xs

toolResult :: Text -> Text -> ToolResult
toolResult tid body = ToolResult (ToolCallId tid) body False

extractToolResultBody :: Message -> Text
extractToolResultBody msg =
  case [tr.resultBody | ToolResultContent tr <- msg.content] of
    (b : _) -> b
    [] -> ""
