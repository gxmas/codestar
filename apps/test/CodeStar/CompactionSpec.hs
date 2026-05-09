{-# OPTIONS_GHC -Wno-orphans #-}

module CodeStar.CompactionSpec (spec) where

import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CodeStar.Compaction
  ( CompactionConfig (..)
  , CompactionState (..)
  , buildCompactedHistory
  , renderHistory
  , shouldCompact
  )
import CodeStar.LLM.Base (Content (..), Message (..), Role (..))
import CodeStar.LLM.Gen (arbitraryNonMarkerContent, arbitraryText)
import CodeStar.RepoMap.Render (estimateTokens)

-- --------------------------------------------------------------------
-- Generators
-- --------------------------------------------------------------------

genCompactionConfig :: Gen CompactionConfig
genCompactionConfig = do
  frac <- choose (0.5, 0.99)
  cap <- chooseInt (1_000, 500_000)
  pure CompactionConfig{triggerFraction = frac, maxContextTokens = cap}

instance Arbitrary CompactionConfig where
  arbitrary = genCompactionConfig
  shrink cfg =
    [ cfg{triggerFraction = frac}
    | frac <- shrink cfg.triggerFraction
    , frac >= 0.5
    , frac <= 0.99
    ]
      ++ [ cfg{maxContextTokens = cap}
         | cap <- shrink cfg.maxContextTokens
         , cap >= 1_000
         ]

genCompactionState :: Gen CompactionState
genCompactionState =
  CompactionState
    <$> arbitraryText
    <*> listOf arbitraryText
    <*> listOf arbitraryText
    <*> arbitraryText

instance Arbitrary CompactionState where
  arbitrary = genCompactionState
  shrink cs =
    [cs{csRepoMap = m} | m <- shrinkText cs.csRepoMap]
      ++ [cs{csTodoList = todos} | todos <- shrinkTextList cs.csTodoList]
      ++ [cs{csMemory = mem} | mem <- shrinkTextList cs.csMemory]
      ++ [cs{csTask = task} | task <- shrinkText cs.csTask]

genMessage :: Gen Message
genMessage =
  Message
    <$> elements [User, Assistant, System]
    <*> listOf arbitraryNonMarkerContent

genHistory :: Gen [Message]
genHistory = resize 6 (listOf genMessage)

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

classifyHistLen :: [a] -> Property -> Property
classifyHistLen hist =
  classify (null hist) "empty" .
  classify (length hist == 1) "1 message" .
  classify (length hist >= 2 && length hist <= 4) "2-4 messages" .
  classify (length hist >= 5) "5+ messages"

spec :: Spec
spec = do
  describe "shouldCompact" $ do
    it "empty history never triggers compaction" $
      property $
        forAllShrink genCompactionConfig shrink $ \cfg ->
          not (shouldCompact cfg Seq.empty)

    prop "if compaction triggers, rendered tokens exceed trigger threshold" $
      forAllShrink genCompactionConfig shrink $ \cfg ->
        forAllShrink genHistory shrink $ \hist ->
          checkCoverage $
            classify (shouldCompact cfg (Seq.fromList hist)) "compaction triggered" $
            classifyHistLen hist $
            let rendered = renderHistory (Seq.fromList hist)
                tokens = estimateTokens rendered
                limit = floor @Double (fromIntegral cfg.maxContextTokens * cfg.triggerFraction)
                compacted = shouldCompact cfg (Seq.fromList hist)
             in counterexample ("tokens=" <> show tokens <> " limit=" <> show limit) $
                  (not compacted) || tokens >= limit

  describe "renderHistory" $ do
    prop "eventually triggers compaction when messages keep being appended" $
      forAllShrink genLivenessCompactionConfig shrinkCompactLivenessConfig $ \cfg ->
        let msg = Message User [TextContent (Text.replicate 64 "x")]
            initial = Seq.singleton msg
            histories = take 256 (iterate (Seq.|> msg) initial)
            triggers = any (shouldCompact cfg) histories
         in counterexample ("failed to trigger within 256 appends, cfg=" <> show cfg) triggers

    it "empty history produces empty text" $
      renderHistory Seq.empty `shouldBe` Text.empty

    prop "each message contributes to the output" $
      forAllShrink (listOf1 genMessage) shrink $ \hist ->
        classifyHistLen hist $
        property $ not (Text.null (renderHistory (Seq.fromList hist)))

  describe "buildCompactedHistory" $ do
    prop "returns exactly one System message" $
      forAllShrink genCompactionState shrink $ \cs ->
        forAllShrink arbitraryText shrinkText $ \summary ->
          checkCoverage $
            cover 30 (not (Text.null cs.csRepoMap)) "non-empty repo map" $
            cover 30 (not (null cs.csTodoList)) "non-empty todo list" $
            let result = buildCompactedHistory cs summary
             in Seq.length result === 1
                  .&&. (Seq.index result 0).role === System

    prop "embeds the summary text" $
      forAllShrink genCompactionState shrink $ \cs ->
        forAllShrink (arbitraryText `suchThat` (not . Text.null)) shrinkText $ \summary ->
          let msg = Seq.index (buildCompactedHistory cs summary) 0
              body = foldMap contentText msg.content
           in counterexample (Text.unpack body) $
                Text.isInfixOf summary body

    prop "embeds the repo map when non-empty" $
      forAllShrink genCompactionState shrink $ \cs ->
        forAllShrink arbitraryText shrinkText $ \summary ->
          not (Text.null cs.csRepoMap) ==>
            let msg = Seq.index (buildCompactedHistory cs summary) 0
                body = foldMap contentText msg.content
             in Text.isInfixOf cs.csRepoMap body

    prop "embeds pending tasks when non-empty" $
      forAllShrink genCompactionState shrink $ \cs ->
        forAllShrink arbitraryText shrinkText $ \summary ->
          not (null cs.csTodoList) && all (not . Text.null) cs.csTodoList ==>
            let msg = Seq.index (buildCompactedHistory cs summary) 0
                body = foldMap contentText msg.content
             in all (\t -> Text.isInfixOf t body) cs.csTodoList

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

contentText :: Content -> Text
contentText (TextContent t) = t
contentText _ = Text.empty

shrinkText :: Text -> [Text]
shrinkText = map Text.pack . shrink . Text.unpack

shrinkTextList :: [Text] -> [[Text]]
shrinkTextList = shrinkList shrinkText

genLivenessCompactionConfig :: Gen CompactionConfig
genLivenessCompactionConfig = do
  frac <- choose (0.5, 0.99)
  cap <- chooseInt (128, 1_000)
  pure CompactionConfig{triggerFraction = frac, maxContextTokens = cap}

shrinkCompactLivenessConfig :: CompactionConfig -> [CompactionConfig]
shrinkCompactLivenessConfig cfg =
  [ cfg{triggerFraction = frac}
  | frac <- shrink cfg.triggerFraction
  , frac >= 0.5
  , frac <= 0.99
  ]
    ++ [ cfg{maxContextTokens = cap}
       | cap <- shrink cfg.maxContextTokens
       , cap >= 128
       , cap <= 1_000
       ]
