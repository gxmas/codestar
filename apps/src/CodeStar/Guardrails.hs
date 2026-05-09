module CodeStar.Guardrails
  ( -- * Decision
    GuardrailDecision (..)

    -- * Policy engine
  , GuardrailConfig (..)
  , defaultGuardrailConfig
  , evaluate

    -- * Internal (Testing)
  , containsSecret
  , violatesScope
  , isWriteTool
  , tierDecision
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import CodeStar.LLM.Base (ToolCall (..), ToolName (..))
import CodeStar.Tools.Registry (RiskTier (..), ToolDefinition (..), ToolRegistry, listTools)

-- --------------------------------------------------------------------
-- Decision
-- --------------------------------------------------------------------

data GuardrailDecision
  = Allow
  | -- | reason shown to user in the approval prompt
    RequireApproval !Text
  | -- | reason; tool call is blocked entirely
    Deny !Text
  deriving stock (Eq, Show)

-- --------------------------------------------------------------------
-- Config
-- --------------------------------------------------------------------

data GuardrailConfig = GuardrailConfig
  { denyList :: !(Set Text)
  -- ^ tool names that are always blocked
  , allowList :: !(Set Text)
  -- ^ tool names that are auto-approved
  , allowedPaths :: !(Maybe (Set FilePath))
  -- ^ restrict writes to these paths
  , secretPatterns :: ![Text]
  -- ^ regexes / substrings signalling secrets
  }
  deriving stock (Eq, Show)

defaultGuardrailConfig :: GuardrailConfig
defaultGuardrailConfig =
  GuardrailConfig
    { denyList =
        Set.fromList
          [ "shell" -- shell commands require explicit allow-listing
          ]
    , allowList =
        Set.fromList
          [ "read"
          , "glob"
          , "grep"
          , "todo_read"
          , "todo_write"
          ]
    , allowedPaths = Nothing -- no path restriction by default
    , secretPatterns = ["password", "secret", "api_key", "token", "private_key"]
    }

-- --------------------------------------------------------------------
-- Policy engine
-- --------------------------------------------------------------------

{- | Evaluate a tool call against all policies in priority order:
  1. Deny list  — hard block
  2. Allow list — auto-approve
  3. Risk tier  — ReadOnly → Allow, LocalWrite → RequireApproval if paths
                  restricted, SideEffect → RequireApproval
  4. Secret detection — block if arguments contain known secret patterns
  5. Scope policy — block writes outside allowedPaths
-}
evaluate ::
  GuardrailConfig ->
  ToolRegistry ->
  ToolCall ->
  GuardrailDecision
evaluate cfg reg tc =
  let name = unToolName tc.toolName
      argText = Text.pack (show tc.arguments)
      mDef = lookupDef reg tc.toolName
      tier = maybe SideEffect (.riskTier) mDef
   in if Set.member name cfg.denyList
        then Deny ("Tool '" <> name <> "' is on the deny list")
        else
          if Set.member name cfg.allowList
            then Allow
            else
              if containsSecret cfg argText
                then Deny "Arguments contain a potential secret or credential"
                else
                  if violatesScope cfg name argText
                    then Deny "Write target is outside the allowed workspace scope"
                    else tierDecision tier name

tierDecision :: RiskTier -> Text -> GuardrailDecision
tierDecision ReadOnly _ = Allow
tierDecision LocalWrite _ = Allow
tierDecision SideEffect name =
  RequireApproval ("Tool '" <> name <> "' has side effects outside the workspace")

containsSecret :: GuardrailConfig -> Text -> Bool
containsSecret cfg txt =
  any (\pat -> Text.isInfixOf pat (Text.toLower txt)) cfg.secretPatterns

violatesScope :: GuardrailConfig -> Text -> Text -> Bool
violatesScope cfg name argText =
  case cfg.allowedPaths of
    Nothing -> False
    Just paths ->
      isWriteTool name
        && not (any (\p -> Text.isInfixOf (Text.pack p) argText) (Set.toList paths))

isWriteTool :: Text -> Bool
isWriteTool name = name `elem` ["write", "edit", "shell", "git"]

lookupDef :: ToolRegistry -> ToolName -> Maybe ToolDefinition
lookupDef reg tname = case filter (\d -> d.name == tname) (listTools reg) of
  (d : _) -> Just d
  [] -> Nothing
