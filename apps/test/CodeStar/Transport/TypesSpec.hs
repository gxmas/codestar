{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CodeStar.Transport.TypesSpec (spec) where

import Data.Aeson (FromJSON, ToJSON, decode, eitherDecode, encode)
import Data.ByteString.Lazy.Char8 qualified as BLC8
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CodeStar.AgentLoop (AgentEvent (..), ApprovalDecision (..))
import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Transport.Types
  ( AgentEventEnvelope (..)
  , Command (..)
  , CommandResult (..)
  )
import CodeStar.Types
  ( CheckResult (..)
  , ControlSignal (..)
  , Evidence (..)
  , SessionId (..)
  )

-- --------------------------------------------------------------------
-- Orphan Eq instances (test-only, needed for round-trip comparison)
-- --------------------------------------------------------------------

deriving instance Eq AgentEvent
deriving instance Eq AgentEventEnvelope

-- --------------------------------------------------------------------
-- Generators and Arbitrary instances
-- --------------------------------------------------------------------

genSessionId :: Gen SessionId
genSessionId = SessionId . Text.pack <$> listOf1 (elements ['a' .. 'z'])

genText :: Gen Text
genText = Text.pack <$> listOf (elements ['a' .. 'z'])

genNonEmptyText :: Gen Text
genNonEmptyText = Text.pack <$> listOf1 (elements ['a' .. 'z'])

genToolName :: Gen ToolName
genToolName = ToolName . Text.pack <$> listOf1 (elements ['a' .. 'z'])

instance Arbitrary SessionId where
  arbitrary = genSessionId
  shrink (SessionId t) = [SessionId (Text.pack s) | s <- shrink (Text.unpack t), not (null s)]

instance Arbitrary Command where
  arbitrary = oneof
    [ CmdStart    <$> genSessionId <*> genText
    , CmdRespond  <$> genSessionId <*> genText
    , CmdApprove  <$> genSessionId
    , CmdReject   <$> genSessionId <*> genNonEmptyText
    , CmdCompact  <$> genSessionId <*> oneof [pure Nothing, Just <$> genText]
    , CmdStop     <$> genSessionId
    ]

instance Arbitrary CommandResult where
  arbitrary = oneof [pure CmdOk, CmdErr <$> genText]

instance Arbitrary ApprovalDecision where
  arbitrary = oneof [pure Approved, Rejected <$> genNonEmptyText]
  shrink Approved = []
  shrink (Rejected r) = Approved : [Rejected r' | r' <- shrinkText r, not (Text.null r')]

-- All AgentEvent variants that round-trip exactly through JSON.
-- AgentDone is intentionally lossy (parses to Done emptyEvidence regardless
-- of what was encoded), so it is excluded from exact round-trip tests.
genRoundTrippableEvent :: Gen AgentEvent
genRoundTrippableEvent = oneof
  [ AgentToken              <$> genText
  , AgentToolCall           <$> genToolName <*> genText
  , AgentToolResult         <$> genToolName <*> genText
  , AgentApprovalRequired   <$> genToolName <*> genText
  , pure AgentCompacting
  , AgentProgress           <$> genText
  , AgentCostUpdate         <$> (getNonNegative <$> arbitrary)
                            <*> (getNonNegative <$> arbitrary)
  , AgentError              <$> genText
  ]

-- All AgentEvent variants, including the lossy AgentDone.
genAnyEvent :: Gen AgentEvent
genAnyEvent = frequency
  [ (8, genRoundTrippableEvent)
  , (1, pure (AgentDone (Done emptyEvidence)))
  ]
  where
    emptyEvidence = Evidence NotChecked NotChecked [] []

instance Arbitrary AgentEvent where
  arbitrary = genAnyEvent

instance Arbitrary AgentEventEnvelope where
  arbitrary = AgentEventEnvelope <$> genSessionId <*> genRoundTrippableEvent

shrinkText :: Text -> [Text]
shrinkText = map Text.pack . filter (not . null) . shrink . Text.unpack

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = describe "CodeStar.Transport.Types" $ do

  -- ----------------------------------------------------------------
  -- Property-based round-trip tests
  -- ----------------------------------------------------------------

  describe "round-trip properties" $ do

    prop "Command round-trips through JSON for all variants" $
      \(cmd :: Command) ->
        decode (encode cmd) === Just cmd

    prop "CommandResult round-trips through JSON" $
      \(cr :: CommandResult) ->
        decode (encode cr) === Just cr

    prop "ApprovalDecision round-trips through JSON" $
      \(ad :: ApprovalDecision) ->
        decode (encode ad) === Just ad

    prop "non-Done AgentEvents round-trip through JSON" $
      forAll genRoundTrippableEvent $ \ev ->
        decode (encode ev) === Just ev

    prop "all AgentEvent variants encode to decodable JSON (including lossy Done)" $
      forAll genAnyEvent $ \ev ->
        counterexample ("encoded: " <> show (encode ev)) $
          decode @AgentEvent (encode ev) =/= Nothing

    prop "AgentEventEnvelope round-trips and preserves session ID" $
      \(env :: AgentEventEnvelope) ->
        case decode @AgentEventEnvelope (encode env) of
          Nothing  -> counterexample "decode returned Nothing" False
          Just env' -> env'.envSessionId === env.envSessionId
                  .&&. env'.envEvent === env.envEvent

  -- ----------------------------------------------------------------
  -- Existing example-based tests (retained as regression guards)
  -- ----------------------------------------------------------------

  it "round-trips all command variants through JSON" $ do
    map roundtrip
      [ CmdStart (SessionId "s1") "task"
      , CmdRespond (SessionId "s1") "ok"
      , CmdApprove (SessionId "s1")
      , CmdReject (SessionId "s1") "no"
      , CmdCompact (SessionId "s1") Nothing
      , CmdCompact (SessionId "s1") (Just "tighten")
      , CmdStop (SessionId "s1")
      ]
      `shouldSatisfy` and

  it "round-trips command results through JSON" $ do
    roundtrip CmdOk `shouldBe` True
    roundtrip (CmdErr "boom") `shouldBe` True

  it "round-trips event envelopes for simple events" $ do
    let env = AgentEventEnvelope (SessionId "s2") (AgentToken "token")
        decoded = decode (encode env) :: Maybe AgentEventEnvelope
    decoded `shouldSatisfy` maybe False (\v -> v.envSessionId == SessionId "s2")

  it "round-trips event envelope tool call payload fields" $ do
    let args = "{\"path\":\"src/Main.hs\",\"replaceAll\":false}"
        env = AgentEventEnvelope (SessionId "s-tool") (AgentToolCall (ToolName "edit") args)
        decoded = decode (encode env) :: Maybe AgentEventEnvelope
    case decoded of
      Just v -> do
        v.envSessionId `shouldBe` SessionId "s-tool"
        case v.envEvent of
          AgentToolCall (ToolName name) parsedArgs -> do
            name `shouldBe` "edit"
            parsedArgs `shouldBe` args
          _ -> expectationFailure "Expected AgentToolCall after decode"
      Nothing -> expectationFailure "Failed to decode tool-call event envelope"

  it "parses done-event JSON into Done with default evidence" $ do
    let original =
          AgentEventEnvelope
            (SessionId "s3")
            ( AgentDone
                (Done (Evidence Passed Passed ["A.hs"] ["none"]))
            )
        decoded = decode (encode original) :: Maybe AgentEventEnvelope
    case decoded of
      Nothing -> expectationFailure "Failed to decode encoded done envelope"
      Just env -> case env.envEvent of
        AgentDone (Done ev) -> do
          ev.testsPass `shouldBe` NotChecked
          ev.buildSucceeds `shouldBe` NotChecked
        _ -> expectationFailure "Expected AgentDone after decode"

  it "round-trips approval decisions through JSON" $ do
    roundtrip Approved `shouldBe` True
    roundtrip (Rejected "not-safe") `shouldBe` True

  it "rejects unknown AgentEvent type during decode" $ do
    let raw = "{\"sessionId\":\"s9\",\"event\":{\"type\":\"mystery\"}}"
        decoded = eitherDecode (BLC8.pack raw) :: Either String AgentEventEnvelope
    decoded `shouldSatisfy` isLeft

  it "rejects rejected approval decision without reason" $ do
    let raw = "{\"decision\":\"rejected\"}"
        decoded = eitherDecode (BLC8.pack raw) :: Either String ApprovalDecision
    decoded `shouldSatisfy` isLeft

  it "rejects rejected approval decision with empty or whitespace reason" $ do
    let emptyReason = eitherDecode (BLC8.pack "{\"decision\":\"rejected\",\"reason\":\"\"}") :: Either String ApprovalDecision
        spaceReason = eitherDecode (BLC8.pack "{\"decision\":\"rejected\",\"reason\":\"   \"}") :: Either String ApprovalDecision
    emptyReason `shouldSatisfy` isLeft
    spaceReason `shouldSatisfy` isLeft

roundtrip :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
roundtrip x = decode (encode x) == Just x

isLeft :: Either a b -> Bool
isLeft Left{} = True
isLeft _ = False
