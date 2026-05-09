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

data RiskTier = ReadOnly | LocalWrite | SideEffect
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

data ToolDefinition = ToolDefinition
  { name :: ToolName
  , description :: Text
  , parameters :: Schema
  , riskTier :: RiskTier
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
