{-# LANGUAGE RecordWildCards #-}

-- | Tool definition, tool choice, and server tool types.
module Anthropic.Protocol.Tool
  ( -- * Tool Definition
    ToolDefinition (..)
  , CustomToolDef (..)
  , customToolDef

    -- ** CustomToolDef Setters
  , withDescription
  , withCustomToolCacheControl

    -- * Server Tools
  , ServerToolType (..)
  , knownServerTools
  , ServerToolDef (..)
  , serverToolDef

    -- ** ServerToolDef Setters
  , withServerToolCacheControl
  , withExtraConfig

    -- * Tool Choice
  , ToolChoice (..)
  , DisableParallel (..)
  , toolAuto
  , toolAny
  , toolNone

    -- * Schema Conversion
    -- | Re-exported from @json-schema-combinators@ for converting
    -- 'Data.Aeson.Value' to 'Schema' at the boundary.
  , schemaFromValue
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:), (.:?)
  , Value, object, withObject, withText
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Parser)
import Data.JsonSchema (Schema, DecodeError, decode, encode)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Anthropic.Types (CacheControl)

-- | Convert an aeson 'Value' to a 'Schema'.
--
-- Use this at the boundary when loading schemas from external sources
-- (files, TH derivation, other libraries). The conversion is explicit
-- and fallible — invalid JSON Schema values produce a 'DecodeError'.
--
-- Re-exported from @json-schema-combinators@.
schemaFromValue :: Value -> Either DecodeError Schema
schemaFromValue = decode

-- | Whether to disable parallel tool use.
newtype DisableParallel = DisableParallel { unDisableParallel :: Bool }
  deriving newtype (Eq, Show)

-- | Tool choice strategy.
--
-- Controls how the model uses tools in a request.
data ToolChoice
  = ToolAuto     !DisableParallel
  | ToolAny      !DisableParallel
  | ToolSpecific !Text !DisableParallel
    -- ^ Force use of a specific tool by name.
  | ToolNone
  deriving stock (Eq, Show, Generic)

instance ToJSON ToolChoice where
  toJSON (ToolAuto (DisableParallel dp)) = object $
    [ "type" .= ("auto" :: Text) ]
    ++ [ "disable_parallel_tool_use" .= True | dp ]
  toJSON (ToolAny (DisableParallel dp)) = object $
    [ "type" .= ("any" :: Text) ]
    ++ [ "disable_parallel_tool_use" .= True | dp ]
  toJSON (ToolSpecific name' (DisableParallel dp)) = object $
    [ "type" .= ("tool" :: Text)
    , "name" .= name'
    ]
    ++ [ "disable_parallel_tool_use" .= True | dp ]
  toJSON ToolNone = object
    [ "type" .= ("none" :: Text) ]

  toEncoding (ToolAuto (DisableParallel dp)) = E.pairs $
       "type" .= ("auto" :: Text)
    <> if dp then "disable_parallel_tool_use" .= True else mempty
  toEncoding (ToolAny (DisableParallel dp)) = E.pairs $
       "type" .= ("any" :: Text)
    <> if dp then "disable_parallel_tool_use" .= True else mempty
  toEncoding (ToolSpecific name' (DisableParallel dp)) = E.pairs $
       "type" .= ("tool" :: Text)
    <> "name" .= name'
    <> if dp then "disable_parallel_tool_use" .= True else mempty
  toEncoding ToolNone = E.pairs $
    "type" .= ("none" :: Text)

instance FromJSON ToolChoice where
  parseJSON = withObject "ToolChoice" $ \o -> do
    typ <- o .: "type" :: Parser Text
    dp <- DisableParallel <$> o .:? "disable_parallel_tool_use" .!= False
    case typ of
      "auto" -> pure $ ToolAuto dp
      "any"  -> pure $ ToolAny dp
      "tool" -> ToolSpecific <$> o .: "name" <*> pure dp
      "none" -> pure ToolNone
      _      -> fail $ "Unknown ToolChoice type: " ++ T.unpack typ
    where
      (.!=) :: Parser (Maybe a) -> a -> Parser a
      p .!= def' = fmap (maybe def' id) p

-- | Smart constructor: auto tool choice with parallel enabled.
toolAuto :: ToolChoice
toolAuto = ToolAuto (DisableParallel False)

-- | Smart constructor: any tool, parallel enabled.
toolAny :: ToolChoice
toolAny = ToolAny (DisableParallel False)

-- | Smart constructor: no tool use.
toolNone :: ToolChoice
toolNone = ToolNone

-- | Server-provided tool types.
--
-- Includes an 'OtherServerTool' catch-all for forward compatibility.
-- When Anthropic adds a new server tool, existing code continues to
-- deserialize without error. See ADR-005.
data ServerToolType
  = WebSearch
  | WebFetch
  | CodeExecution
  | BashTool
  | TextEditor
  | MemoryTool
  | ToolSearchBM25
  | ToolSearchRegex
  | OtherServerTool !Text
    -- ^ Forward-compatible catch-all for unrecognized server tools.
  deriving stock (Eq, Ord, Show, Generic)

-- | Wire text for a server tool type.
serverToolToWire :: ServerToolType -> Text
serverToolToWire WebSearch       = "web_search"
serverToolToWire WebFetch        = "web_fetch"
serverToolToWire CodeExecution   = "code_execution"
serverToolToWire BashTool        = "bash"
serverToolToWire TextEditor      = "text_editor"
serverToolToWire MemoryTool      = "memory"
serverToolToWire ToolSearchBM25  = "tool_search_bm25"
serverToolToWire ToolSearchRegex = "tool_search_regex"
serverToolToWire (OtherServerTool t) = t

-- | Parse wire text to a server tool type, normalizing known tools.
serverToolFromWire :: Text -> ServerToolType
serverToolFromWire "web_search"        = WebSearch
serverToolFromWire "web_fetch"         = WebFetch
serverToolFromWire "code_execution"    = CodeExecution
serverToolFromWire "bash"              = BashTool
serverToolFromWire "text_editor"       = TextEditor
serverToolFromWire "memory"            = MemoryTool
serverToolFromWire "tool_search_bm25"  = ToolSearchBM25
serverToolFromWire "tool_search_regex" = ToolSearchRegex
serverToolFromWire other               = OtherServerTool other

instance ToJSON ServerToolType where
  toJSON = Aeson.String . serverToolToWire
  toEncoding = E.text . serverToolToWire

instance FromJSON ServerToolType where
  parseJSON = withText "ServerToolType" $ pure . serverToolFromWire

-- | All known server tool types (excludes 'OtherServerTool').
knownServerTools :: [ServerToolType]
knownServerTools =
  [ WebSearch, WebFetch, CodeExecution, BashTool
  , TextEditor, MemoryTool, ToolSearchBM25, ToolSearchRegex
  ]

-- | Server tool definition with type-specific configuration.
--
-- Use 'serverToolDef' smart constructor + @with*@ setters:
--
-- @
-- let tool = serverToolDef WebSearch "web_search"
--          & withExtraConfig (object ["allowed_domains" .= ["example.com"]])
-- @
data ServerToolDef = ServerToolDef
  { toolType     :: !ServerToolType
  , name         :: !Text
  , cacheControl :: !(Maybe CacheControl)
  , extraConfig  :: !(Maybe Value)
    -- ^ Tool-specific configuration (allowed_domains, max_uses, etc.)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ServerToolDef where
  toJSON sd = object $
    [ "type"         .= sd.toolType
    , "name"         .= sd.name
    ]
    ++ maybe [] (\cc -> ["cache_control" .= cc]) sd.cacheControl
    ++ maybe [] (\ec -> ["extra_config" .= ec]) sd.extraConfig

  toEncoding sd = E.pairs $
       "type"         .= sd.toolType
    <> "name"         .= sd.name
    <> foldMap ("cache_control" .=) sd.cacheControl
    <> foldMap ("extra_config" .=) sd.extraConfig

instance FromJSON ServerToolDef where
  parseJSON = withObject "ServerToolDef" $ \o ->
    ServerToolDef
      <$> o .:  "type"
      <*> o .:  "name"
      <*> o .:? "cache_control"
      <*> o .:? "extra_config"

-- | Smart constructor for 'ServerToolDef' with required fields only.
serverToolDef :: ServerToolType -> Text -> ServerToolDef
serverToolDef ty n = ServerToolDef
  { toolType     = ty
  , name         = n
  , cacheControl = Nothing
  , extraConfig  = Nothing
  }

-- | Set the cache control on a server tool definition.
withServerToolCacheControl :: CacheControl -> ServerToolDef -> ServerToolDef
withServerToolCacheControl x t = let ServerToolDef{..} = t in ServerToolDef{cacheControl = Just x, ..}

-- | Set tool-specific configuration (allowed_domains, max_uses, etc.).
withExtraConfig :: Value -> ServerToolDef -> ServerToolDef
withExtraConfig x t = let ServerToolDef{..} = t in ServerToolDef{extraConfig = Just x, ..}

-- | A custom tool definition with a JSON Schema for input validation.
--
-- Use 'customToolDef' smart constructor + @with*@ setters:
--
-- @
-- let tool = customToolDef "get_weather" mySchema
--          & withDescription "Get current weather"
-- @
--
-- The @inputSchema@ field uses 'Schema' from @json-schema-combinators@
-- for type-safe schema construction. Build schemas using combinators
-- like 'Data.JsonSchema.Combinators.objectSchema' and
-- 'Data.JsonSchema.Combinators.required', or convert from 'Value' at
-- the boundary via 'schemaFromValue'. See ADR-006.
data CustomToolDef = CustomToolDef
  { name         :: !Text
  , description  :: !(Maybe Text)
  , inputSchema  :: !Schema
    -- ^ JSON Schema for tool input, built with @json-schema-combinators@.
  , cacheControl :: !(Maybe CacheControl)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CustomToolDef where
  toJSON cd = object $
    [ "name"         .= cd.name
    , "input_schema" .= encode cd.inputSchema
    ]
    ++ maybe [] (\d  -> ["description"   .= d])  cd.description
    ++ maybe [] (\cc -> ["cache_control" .= cc]) cd.cacheControl

  toEncoding cd = E.pairs $
       "name"         .= cd.name
    <> "input_schema" .= encode cd.inputSchema
    <> foldMap ("description"   .=) cd.description
    <> foldMap ("cache_control" .=) cd.cacheControl

instance FromJSON CustomToolDef where
  parseJSON = withObject "CustomToolDef" $ \o -> do
    schemaVal <- o .: "input_schema"
    schema <- case decode schemaVal of
      Right s  -> pure s
      Left err -> fail $ "Invalid input_schema: " ++ show err
    CustomToolDef
      <$> o .:  "name"
      <*> o .:? "description"
      <*> pure schema
      <*> o .:? "cache_control"

-- | Smart constructor for 'CustomToolDef' with required fields only.
customToolDef :: Text -> Schema -> CustomToolDef
customToolDef n s = CustomToolDef
  { name         = n
  , description  = Nothing
  , inputSchema  = s
  , cacheControl = Nothing
  }

-- | Set the tool description.
withDescription :: Text -> CustomToolDef -> CustomToolDef
withDescription x t = let CustomToolDef{..} = t in CustomToolDef{description = Just x, ..}

-- | Set the cache control on a custom tool definition.
withCustomToolCacheControl :: CacheControl -> CustomToolDef -> CustomToolDef
withCustomToolCacheControl x t = let CustomToolDef{..} = t in CustomToolDef{cacheControl = Just x, ..}

-- | A tool definition: either a custom tool or a server-provided tool.
data ToolDefinition
  = CustomTool  !CustomToolDef
  | ServerTool  !ServerToolDef
  deriving stock (Eq, Show, Generic)

instance ToJSON ToolDefinition where
  toJSON (CustomTool cd) = case toJSON cd of
    Aeson.Object o -> Aeson.Object o
    v              -> v
  toJSON (ServerTool sd) = toJSON sd

  toEncoding (CustomTool cd) = toEncoding cd
  toEncoding (ServerTool sd) = toEncoding sd

instance FromJSON ToolDefinition where
  parseJSON = withObject "ToolDefinition" $ \o -> do
    -- Server tools have a "type" field matching a ServerToolType.
    -- Custom tools have an "input_schema" field.
    hasSchema <- fmap (maybe False (const True)) (o .:? "input_schema" :: Parser (Maybe Value))
    if hasSchema
      then CustomTool <$> parseJSON (Aeson.Object o)
      else ServerTool <$> parseJSON (Aeson.Object o)
