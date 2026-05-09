{-# OPTIONS_GHC -Wno-orphans #-}

module CodeStar.GuardrailsSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CodeStar.Guardrails
  ( GuardrailConfig (..)
  , GuardrailDecision (..)
  , containsSecret
  , defaultGuardrailConfig
  , evaluate
  , tierDecision
  , violatesScope
  )
import CodeStar.LLM.Base (ToolCall (..), ToolCallId (..), ToolName (..))
import CodeStar.Tools.Registry (RiskTier (..), emptyRegistry)

-- --------------------------------------------------------------------
-- Generators
-- --------------------------------------------------------------------

genToolName :: Gen Text
genToolName =
  elements
    ["read", "write", "shell", "edit", "glob", "grep", "custom_tool", "my_tool"]

genToolCall :: Gen ToolCall
genToolCall =
  ToolCall
    <$> (ToolCallId . Text.pack . ("toolu_" <>) <$> vectorOf 8 (elements ['a' .. 'z']))
    <*> (ToolName <$> genToolName)
    <*> genStructuredArgs

genToolCallWithName :: Text -> Gen ToolCall
genToolCallWithName name =
  ToolCall
    <$> (ToolCallId . Text.pack . ("toolu_" <>) <$> vectorOf 8 (elements ['a' .. 'z']))
    <*> pure (ToolName name)
    <*> genStructuredArgs

genStructuredArgs :: Gen Value
genStructuredArgs =
  frequency
    [ (4, Object <$> genObjectArg)
    , (2, String <$> genSmallText)
    , (1, pure (String "password=abc123"))
    ]

genObjectArg :: Gen (KM.KeyMap Value)
genObjectArg = do
  path <- elements ["src/Main.hs", "src/CodeStar/Guardrails.hs", "/tmp/outside.txt"]
  payload <- genSmallText
  pure (fromListPairs [("path", String path), ("content", String payload)])

genSmallText :: Gen Text
genSmallText = Text.pack <$> listOf (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ "/_-."))

fromListPairs :: [(Text, Value)] -> KM.KeyMap Value
fromListPairs = KM.fromList . map (\(k, v) -> (Key.fromText k, v))

fromListPairsValue :: [(Text, Value)] -> Value
fromListPairsValue = Object . fromListPairs

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = describe "CodeStar.Guardrails" $ do
  describe "evaluate" $ do
    it "covers all decision branches with generated calls" $
      property $
        forAllShrink genToolCall shrinkToolCall $ \tc ->
          let cfg =
                defaultGuardrailConfig
                  { allowList = Set.singleton "read"
                  , denyList = Set.singleton "shell"
                  , allowedPaths = Just (Set.singleton "src/")
                  }
              decision = evaluate cfg emptyRegistry tc
              branch = decisionBranch decision
           in checkCoverage $
                cover 10 (branch == "deny") "deny branch" $
                  cover 10 (branch == "allow") "allow branch" $
                    cover 10 (branch == "secret") "secret branch" $
                      cover 10 (branch == "scope") "scope branch" $
                        cover 10 (branch == "tier") "tier branch" $
                          property True

    it "deny list always produces Deny" $
      property $
        forAllShrink genToolCall shrinkToolCall $ \tc ->
          let name = unToolName tc.toolName
              cfg = defaultGuardrailConfig{denyList = Set.singleton name}
           in isDeny (evaluate cfg emptyRegistry tc)

    it "deny list takes precedence over allow list" $
      property $
        forAllShrink genToolCall shrinkToolCall $ \tc ->
          let name = unToolName tc.toolName
              cfg =
                defaultGuardrailConfig
                  { denyList = Set.singleton name
                  , allowList = Set.singleton name
                  }
           in isDeny (evaluate cfg emptyRegistry tc)

    it "allow list produces Allow when not on deny list" $
      property $
        forAllShrink genToolCall shrinkToolCall $ \tc ->
          let name = unToolName tc.toolName
              cfg =
                defaultGuardrailConfig
                  { denyList = Set.empty
                  , allowList = Set.singleton name
                  , secretPatterns = []
                  }
           in evaluate cfg emptyRegistry tc === Allow

    it "enforces policy order: deny > allow > secrets > scope > tier" $ do
      let baseWriteCall =
            ToolCall
              (ToolCallId "toolu_order01")
              (ToolName "write")
              ( fromListPairsValue
                  [ ("path", String "/tmp/outside.txt")
                  , ("content", String "token=abc")
                  ]
              )
          denyCfg =
            defaultGuardrailConfig
              { denyList = Set.singleton "write"
              , allowList = Set.singleton "write"
              , allowedPaths = Just (Set.singleton "src/")
              }
          allowCfg =
            defaultGuardrailConfig
              { denyList = Set.empty
              , allowList = Set.singleton "write"
              , allowedPaths = Just (Set.singleton "src/")
              }
          secretCfg =
            defaultGuardrailConfig
              { denyList = Set.empty
              , allowList = Set.empty
              , allowedPaths = Just (Set.singleton "src/")
              }
          scopeCfg =
            defaultGuardrailConfig
              { denyList = Set.empty
              , allowList = Set.empty
              , allowedPaths = Just (Set.singleton "src/")
              , secretPatterns = []
              }
          tierCfg =
            defaultGuardrailConfig
              { denyList = Set.empty
              , allowList = Set.empty
              , allowedPaths = Nothing
              , secretPatterns = []
              }
          scopeCall =
            baseWriteCall
              { arguments =
                  fromListPairsValue
                    [ ("path", String "/tmp/outside.txt")
                    , ("content", String "safe")
                    ]
              }
          tierCall = ToolCall (ToolCallId "toolu_order02") (ToolName "custom_tool") (String "safe")

      decisionBranch (evaluate denyCfg emptyRegistry baseWriteCall) `shouldBe` "deny"
      decisionBranch (evaluate allowCfg emptyRegistry baseWriteCall) `shouldBe` "allow"
      decisionBranch (evaluate secretCfg emptyRegistry baseWriteCall) `shouldBe` "secret"
      decisionBranch (evaluate scopeCfg emptyRegistry scopeCall) `shouldBe` "scope"
      decisionBranch (evaluate tierCfg emptyRegistry tierCall) `shouldBe` "tier"

    it "secret patterns in arguments produce Deny" $
      property $
        forAll (elements (secretPatterns defaultGuardrailConfig)) $ \pat ->
          forAllShrink (genToolCallWithName "my_tool") shrinkToolCall $ \tc ->
            let tc' = tc{arguments = String ("prefix " <> pat <> " suffix")}
                cfg =
                  defaultGuardrailConfig
                    { denyList = Set.empty
                    , allowList = Set.empty
                    }
             in isDeny (evaluate cfg emptyRegistry tc')

  describe "metamorphic properties" $ do
    prop "evaluate is pure: same inputs always produce the same decision" $
      forAllShrink genToolCall shrinkToolCall $ \tc ->
        let cfg = defaultGuardrailConfig
         in evaluate cfg emptyRegistry tc === evaluate cfg emptyRegistry tc

    prop "expanding the allow list never downgrades an Allow decision" $
      -- If a tool call is allowed, adding more tools to the allow list
      -- cannot change that to Deny or RequireApproval.
      forAllShrink genToolCall shrinkToolCall $ \tc ->
        forAll genToolName $ \extra ->
          let cfg = defaultGuardrailConfig{denyList = Set.empty, secretPatterns = []}
              cfg' = cfg{allowList = Set.insert extra cfg.allowList}
           in (evaluate cfg emptyRegistry tc == Allow)
                ==> evaluate cfg' emptyRegistry tc === Allow

    prop "adding a tool to the deny list never turns Deny into Allow" $
      forAllShrink genToolCall shrinkToolCall $ \tc ->
        forAll genToolName $ \extra ->
          let cfg = defaultGuardrailConfig
              cfg' = cfg{denyList = Set.insert extra cfg.denyList}
           in isDeny (evaluate cfg emptyRegistry tc)
                ==> isDeny (evaluate cfg' emptyRegistry tc)

    prop "deny list membership is sufficient for Deny regardless of allow list" $
      -- A tool in the deny list is denied even if it is also in the allow list.
      forAllShrink genToolCall shrinkToolCall $ \tc ->
        let name = unToolName tc.toolName
            cfg =
              defaultGuardrailConfig
                { denyList = Set.singleton name
                , allowList = Set.singleton name
                , secretPatterns = []
                }
         in isDeny (evaluate cfg emptyRegistry tc)

    prop "removing a tool from the deny list never introduces a new Deny" $
      -- If a tool is not on the deny list, removing more things from it
      -- cannot make it denied.
      forAllShrink genToolCall shrinkToolCall $ \tc ->
        forAll genToolName $ \extra ->
          let name = unToolName tc.toolName
              cfg =
                defaultGuardrailConfig
                  { denyList = Set.singleton extra
                  , secretPatterns = []
                  }
           in name /= extra
                ==> not (isDeny (evaluate cfg emptyRegistry tc))
                ==> not (isDeny (evaluate cfg{denyList = Set.empty} emptyRegistry tc))

    prop "secret in arguments causes Deny when tool is not in the allow list" $
      -- The allow list takes priority over secret detection (policy order:
      -- deny > allow > secrets > scope > tier).  If the tool is not in
      -- the allow list, a secret in its arguments must produce Deny.
      forAll (elements (secretPatterns defaultGuardrailConfig)) $ \pat ->
        forAllShrink (genToolCallWithName "unlisted_tool") shrinkToolCall $ \tc ->
          let tc' = tc{arguments = String ("data: " <> pat <> " value")}
              cfg =
                defaultGuardrailConfig
                  { denyList = Set.empty
                  , allowList = Set.empty
                  }
           in isDeny (evaluate cfg emptyRegistry tc')

    prop "allowedPaths=Nothing is strictly more permissive than any path restriction" $
      -- Removing the path restriction can never turn Allow into Deny.
      forAllShrink genToolCall shrinkToolCall $ \tc ->
        let cfg = defaultGuardrailConfig{secretPatterns = []}
            cfgNoPath = cfg{allowedPaths = Nothing}
         in not (isDeny (evaluate cfgNoPath emptyRegistry tc))
              ==> not (isDeny (evaluate cfg emptyRegistry tc))

  describe "containsSecret" $ do
    it "is case-insensitive" $
      property $
        forAll (elements ["password", "secret", "api_key", "token"]) $ \pat ->
          containsSecret defaultGuardrailConfig (Text.toUpper pat)
            === containsSecret defaultGuardrailConfig pat

    it "returns True when a known pattern is present" $
      property $
        forAll (elements (secretPatterns defaultGuardrailConfig)) $ \pat ->
          containsSecret defaultGuardrailConfig ("some " <> pat <> " text")

    it "returns False on text with no secret patterns" $
      not (containsSecret defaultGuardrailConfig "hello world foo bar")

  describe "violatesScope" $ do
    it "rejects paths outside allowedPaths when restriction is present" $
      let cfg = defaultGuardrailConfig{allowedPaths = Just (Set.fromList ["src/", "test/"])}
       in violatesScope cfg "write" "{\"path\":\"/tmp/outside.txt\"}" `shouldBe` True

    it "always permits when allowedPaths is Nothing" $
      let cfg = defaultGuardrailConfig{allowedPaths = Nothing}
       in violatesScope cfg "write" "{\"path\":\"/tmp/outside.txt\"}" `shouldBe` False

  describe "tierDecision" $ do
    it "ReadOnly always returns Allow" $
      property $
        forAll genToolName $ \name ->
          tierDecision ReadOnly name === Allow

    it "LocalWrite always returns Allow" $
      property $
        forAll genToolName $ \name ->
          tierDecision LocalWrite name === Allow

    it "SideEffect always returns RequireApproval" $
      property $
        forAll genToolName $ \name ->
          isRequireApproval (tierDecision SideEffect name)

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

isDeny :: GuardrailDecision -> Bool
isDeny (Deny _) = True
isDeny _ = False

isRequireApproval :: GuardrailDecision -> Bool
isRequireApproval (RequireApproval _) = True
isRequireApproval _ = False

shrinkToolCall :: ToolCall -> [ToolCall]
shrinkToolCall tc =
  [ tc{toolCallId = ToolCallId tid'}
  | tid' <- map Text.pack (shrink (Text.unpack tc.toolCallId.unToolCallId))
  , not (Text.null tid')
  ]
    ++ [ tc{toolName = ToolName n'}
       | n' <- map Text.pack (shrink (Text.unpack tc.toolName.unToolName))
       , not (Text.null n')
       ]
    ++ [tc{arguments = v} | v <- shrinkValue tc.arguments]

shrinkValue :: Value -> [Value]
shrinkValue (String t) = [String t' | t' <- shrinkTxt t]
shrinkValue (Object o) = [Object (fromListPairs pairs) | pairs <- shrinkPairs (toPairs o)]
 where
  toPairs = map (\(k, v) -> (Key.toText k, v)) . KM.toList
shrinkValue _ = []

shrinkPairs :: [(Text, Value)] -> [[(Text, Value)]]
shrinkPairs = shrinkList shrinkPair
 where
  shrinkPair (k, v) =
    [(k', v) | k' <- shrinkTxt k]
      ++ [(k, v') | v' <- shrinkValue v]

shrinkTxt :: Text -> [Text]
shrinkTxt = map Text.pack . shrink . Text.unpack

decisionBranch :: GuardrailDecision -> Text
decisionBranch Allow = "allow"
decisionBranch (RequireApproval _) = "tier"
decisionBranch (Deny reason)
  | "deny list" `Text.isInfixOf` reason = "deny"
  | "secret" `Text.isInfixOf` Text.toLower reason = "secret"
  | "outside the allowed workspace scope" `Text.isInfixOf` reason = "scope"
  | otherwise = "deny"
