{- |
= CodeStar.Tools.Registry — tool registration and dispatch

The tool registry is the __table of contents__ for everything the agent
can do.  The agent loop queries it to get tool schemas (sent to the LLM
so it knows what tools are available) and calls it to execute tool
invocations that the LLM requests.

== Risk tiers

Each tool declares a 'RiskTier' that determines whether user approval is
required before execution:

  * 'ReadOnly' — safe to run without confirmation (file reads, searches).
  * 'LocalWrite' — modifies the local filesystem (writes, edits).
  * 'SideEffect' — has external effects (shell commands, network calls).

The permission system in "CodeStar.Permissions" uses risk tiers to decide
whether to ask the user before dispatching.

== Design: record-of-functions

'ToolHandlerDict' is a record-of-functions so that all tools share the
same type and can be stored in a homogeneous @Map@.  The alternative —
a sum type — would require modifying a central dispatch function every
time a new tool is added.

== Adding a new tool

1. Write a module in @CodeStar/Tools/@ exporting a @ToolHandlerDict@.
2. Call 'register' with that dict in "CodeStar.AgentSetup.buildRegistry".
3. The tool is automatically included in the schema sent to the LLM and in
   the dispatch table used by 'CodeStar.AgentLoop'.
-}
module CodeStar.Tools.Registry
  ( -- * Tool Definition
    RiskTier (..)
  , ToolDefinition (..)
  , ToolInput (..)
  , ToolOutput (..)
  , ToolError (..)

    -- * Tool Handler
  , ToolHandlerDict (..)

    -- * Registry
  , ToolRegistry
  , emptyRegistry
  , register
  , dispatch
  , listTools
  , generateDocs

    -- * Input Extraction
  , extractText
  , extractInt
  , extractBool
  ) where

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.JsonSchema (Schema)
import Data.JsonSchema.Serialization (encode)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

import CodeStar.LLM.Base (ToolName (..))

-- --------------------------------------------------------------------
-- Tool Definition
-- --------------------------------------------------------------------

-- | How dangerous a tool invocation is.  Used by the permission system to
-- decide whether to pause and ask the user before executing the tool.
data RiskTier = ReadOnly | LocalWrite | SideEffect
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Metadata about a tool, sent to the LLM as part of the tool-use prompt.
data ToolDefinition = ToolDefinition
  { name        :: ToolName -- ^ The name the LLM uses to invoke this tool.
  , description :: Text     -- ^ Natural-language description for the LLM.
  , parameters  :: Schema   -- ^ JSON Schema describing the tool's arguments.
  , riskTier    :: RiskTier -- ^ Risk level for permission checking.
  }
  deriving stock (Show, Generic)

data ToolInput = ToolInput
  { arguments :: Map Text Value
  }
  deriving stock (Eq, Show, Generic)

data ToolOutput = ToolOutput
  { content :: Text
  , truncated :: Bool
  }
  deriving stock (Eq, Show, Generic)

data ToolError
  = ToolNotFound ToolName
  | InvalidInput Text
  | ExecutionFailed Text
  | Timeout
  | PolicyDenied Text
  deriving stock (Eq, Show, Generic)

-- --------------------------------------------------------------------
-- Tool Handler (record-of-functions)
-- --------------------------------------------------------------------

data ToolHandlerDict = ToolHandlerDict
  { definition :: ToolDefinition
  , invoke :: ToolInput -> IO (Either ToolError ToolOutput)
  }

-- --------------------------------------------------------------------
-- Registry
-- --------------------------------------------------------------------

newtype ToolRegistry = ToolRegistry (Map ToolName ToolHandlerDict)

emptyRegistry :: ToolRegistry
emptyRegistry = ToolRegistry Map.empty

register :: ToolHandlerDict -> ToolRegistry -> ToolRegistry
register handler (ToolRegistry m) =
  ToolRegistry (Map.insert handler.definition.name handler m)

dispatch :: ToolRegistry -> ToolName -> ToolInput -> IO (Either ToolError ToolOutput)
dispatch (ToolRegistry m) toolName input =
  case Map.lookup toolName m of
    Nothing -> pure (Left (ToolNotFound toolName))
    Just handler -> handler.invoke input

listTools :: ToolRegistry -> [ToolDefinition]
listTools (ToolRegistry m) = map (\h -> h.definition) (Map.elems m)

{- | Generate tool documentation for system prompt injection.
Follows SWE-agent's {{command_docs}} format: name, description,
parameter table, and approval requirement for SideEffect tools.
-}
generateDocs :: ToolRegistry -> Text
generateDocs registry =
  Text.intercalate "\n\n" (map formatTool (listTools registry))

formatTool :: ToolDefinition -> Text
formatTool def =
  Text.unlines $
    concat
      [ ["### " <> unToolName def.name]
      , [def.description]
      , renderParams def.parameters
      , riskNote def.riskTier
      ]

renderParams :: Schema -> [Text]
renderParams schema =
  case encode schema of
    Object km ->
      case KM.lookup "properties" km of
        Just (Object props) ->
          let required = case KM.lookup "required" km of
                Just (Array rs) -> [k | String k <- foldr (:) [] rs]
                _ -> []
              rows = map (renderParam required) (KM.toList props)
           in "Parameters:" : rows
        _ -> []
    _ -> []

renderParam :: [Text] -> (Aeson.Key, Value) -> Text
renderParam required (key, schema) =
  let name = Key.toText key
      isReq = name `elem` required
      desc = case schema of
        Object km -> case KM.lookup "description" km of
          Just (String d) -> d
          _ -> ""
        _ -> ""
      typeStr = case schema of
        Object km -> case KM.lookup "type" km of
          Just (String t) -> t
          _ -> "any"
        _ -> "any"
      req = if isReq then " (required)" else " (optional)"
   in "  " <> name <> " [" <> typeStr <> req <> "]: " <> desc

riskNote :: RiskTier -> [Text]
riskNote SideEffect = ["Requires explicit approval before execution."]
riskNote _ = []

-- --------------------------------------------------------------------
-- Input Extraction
-- --------------------------------------------------------------------

extractText :: Text -> ToolInput -> Either ToolError Text
extractText key input = case Map.lookup key input.arguments of
  Just (String t) -> Right t
  Just _ -> Left (InvalidInput (key <> ": expected string"))
  Nothing -> Left (InvalidInput (key <> ": missing"))

extractInt :: Text -> ToolInput -> Either ToolError (Maybe Int)
extractInt key input = case Map.lookup key input.arguments of
  Just (Number n) ->
    let i = truncate n
     in if fromIntegral i == n
          then Right (Just i)
          else Left (InvalidInput (key <> ": expected integer"))
  Just _ -> Left (InvalidInput (key <> ": expected integer"))
  Nothing -> Right Nothing

extractBool :: Text -> ToolInput -> Either ToolError Bool
extractBool key input = case Map.lookup key input.arguments of
  Just (Bool b) -> Right b
  Just _ -> Left (InvalidInput (key <> ": expected boolean"))
  Nothing -> Right False
