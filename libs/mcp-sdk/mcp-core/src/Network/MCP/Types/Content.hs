-- |
-- Module      : Network.MCP.Types.Content
-- Stability   : stable
--
-- Content block types and resource types for MCP.
module Network.MCP.Types.Content
  ( -- * Content blocks
    ContentBlock (..)
  , TextContent (..)
  , ImageContent (..)
  , AudioContent (..)
  , ResourceLink (..)
  , EmbeddedResource (..)

    -- * Resource contents
  , ResourceContents (..)
  , TextResource (..)
  , BlobResource (..)

    -- * Annotations
  , Annotations (..)
  , ToolAnnotations (..)
  , Role (..)

    -- * Icons
  , Icon (..)
  , IconTheme (..)

    -- * URI
  , URI (..)

    -- * URI validation
  , validateUri
  , defaultUriSchemeAllowlist
  ) where

import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Encoding as E
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Network.MCP.Types (Timestamp)

------------------------------------------------------------------------
-- URI
------------------------------------------------------------------------

-- | Opaque URI, no validation at this layer.
newtype URI = URI Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

-- | Default allowed URI schemes for resource validation.
-- An empty list means all schemes are permitted (the default).
-- Operators may supply a non-empty list to restrict which schemes
-- are accepted by their server.
defaultUriSchemeAllowlist :: [Text]
defaultUriSchemeAllowlist = []

-- | Validate a URI text against an allowed-scheme list.
-- Rejects: empty strings, no scheme, disallowed scheme (when allowlist
-- is non-empty), and path traversal (".." segments in file:// URIs).
-- An empty allowlist accepts all schemes.
validateUri :: [Text] -> Text -> Either Text URI
validateUri allowedSchemes t
  | T.null t = Left "URI must not be empty"
  | otherwise =
      case T.breakOn ":" t of
        (_, "") -> Left "URI has no scheme"
        (scheme, rest) ->
          if not (null allowedSchemes) && scheme `notElem` allowedSchemes
            then Left ("URI scheme '" <> scheme <> "' is not allowed")
            else if scheme == "file"
              then
                let path = T.drop (T.length "://") rest
                    segs = T.splitOn "/" path
                in if ".." `elem` segs
                  then Left "URI contains path traversal ('..') segment"
                  else Right (URI t)
              else Right (URI t)

------------------------------------------------------------------------
-- Role
------------------------------------------------------------------------

-- | Participant role.
data Role = RoleUser | RoleAssistant
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance Aeson.ToJSON Role where
  toJSON RoleUser = "user"
  toJSON RoleAssistant = "assistant"
  toEncoding RoleUser = E.text "user"
  toEncoding RoleAssistant = E.text "assistant"

instance Aeson.FromJSON Role where
  parseJSON = Aeson.withText "Role" $ \case
    "user" -> pure RoleUser
    "assistant" -> pure RoleAssistant
    other -> fail $ "Unknown Role: " ++ T.unpack other

------------------------------------------------------------------------
-- IconTheme
------------------------------------------------------------------------

-- | Icon theme preference.
data IconTheme = IconLight | IconDark
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance Aeson.ToJSON IconTheme where
  toJSON IconLight = "light"
  toJSON IconDark = "dark"
  toEncoding IconLight = E.text "light"
  toEncoding IconDark = E.text "dark"

instance Aeson.FromJSON IconTheme where
  parseJSON = Aeson.withText "IconTheme" $ \case
    "light" -> pure IconLight
    "dark" -> pure IconDark
    other -> fail $ "Unknown IconTheme: " ++ T.unpack other

------------------------------------------------------------------------
-- Annotations
------------------------------------------------------------------------

-- | Content annotations for audience targeting and priority.
data Annotations = Annotations
  { annotationsAudience :: !(Maybe [Role])
  , annotationsPriority :: !(Maybe Double)
  , annotationsLastModified :: !(Maybe Timestamp)
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON Annotations where
  toJSON ann =
    Aeson.object $
      maybe [] (\a -> ["audience" .= a]) ann.annotationsAudience
        ++ maybe [] (\p -> ["priority" .= p]) ann.annotationsPriority
        ++ maybe [] (\t -> ["lastModified" .= t]) ann.annotationsLastModified
  toEncoding ann =
    E.pairs $
      foldMap ("audience" .=) ann.annotationsAudience
        <> foldMap ("priority" .=) ann.annotationsPriority
        <> foldMap ("lastModified" .=) ann.annotationsLastModified

instance Aeson.FromJSON Annotations where
  parseJSON = Aeson.withObject "Annotations" $ \o ->
    Annotations
      <$> o .:? "audience"
      <*> o .:? "priority"
      <*> o .:? "lastModified"

------------------------------------------------------------------------
-- ToolAnnotations
------------------------------------------------------------------------

-- | Behavioral hints for tool annotations (per MCP spec).
data ToolAnnotations = ToolAnnotations
  { toolTitle       :: !(Maybe Text)
  , toolReadOnly    :: !(Maybe Bool)
  , toolDestructive :: !(Maybe Bool)
  , toolIdempotent  :: !(Maybe Bool)
  , toolOpenWorld   :: !(Maybe Bool)
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON ToolAnnotations where
  toJSON a =
    Aeson.object $
      maybe [] (\v -> ["title"           .= v]) a.toolTitle
        ++ maybe [] (\v -> ["readOnlyHint"    .= v]) a.toolReadOnly
        ++ maybe [] (\v -> ["destructiveHint" .= v]) a.toolDestructive
        ++ maybe [] (\v -> ["idempotentHint"  .= v]) a.toolIdempotent
        ++ maybe [] (\v -> ["openWorldHint"   .= v]) a.toolOpenWorld
  toEncoding a =
    E.pairs $
      foldMap ("title"           .=) a.toolTitle
        <> foldMap ("readOnlyHint"    .=) a.toolReadOnly
        <> foldMap ("destructiveHint" .=) a.toolDestructive
        <> foldMap ("idempotentHint"  .=) a.toolIdempotent
        <> foldMap ("openWorldHint"   .=) a.toolOpenWorld

instance Aeson.FromJSON ToolAnnotations where
  parseJSON = Aeson.withObject "ToolAnnotations" $ \o ->
    ToolAnnotations
      <$> o .:? "title"
      <*> o .:? "readOnlyHint"
      <*> o .:? "destructiveHint"
      <*> o .:? "idempotentHint"
      <*> o .:? "openWorldHint"

------------------------------------------------------------------------
-- Icon
------------------------------------------------------------------------

-- | Application or resource icon.
data Icon = Icon
  { iconSrc :: !URI
  , iconMimeType :: !(Maybe Text)
  , iconSizes :: !(Maybe [Text])
  , iconTheme :: !(Maybe IconTheme)
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON Icon where
  toJSON ic =
    Aeson.object $
      ["src" .= ic.iconSrc]
        ++ maybe [] (\m -> ["mimeType" .= m]) ic.iconMimeType
        ++ maybe [] (\s -> ["sizes" .= s]) ic.iconSizes
        ++ maybe [] (\t -> ["theme" .= t]) ic.iconTheme
  toEncoding ic =
    E.pairs $
      "src" .= ic.iconSrc
        <> foldMap ("mimeType" .=) ic.iconMimeType
        <> foldMap ("sizes" .=) ic.iconSizes
        <> foldMap ("theme" .=) ic.iconTheme

instance Aeson.FromJSON Icon where
  parseJSON = Aeson.withObject "Icon" $ \o ->
    Icon
      <$> o .: "src"
      <*> o .:? "mimeType"
      <*> o .:? "sizes"
      <*> o .:? "theme"

------------------------------------------------------------------------
-- Resource contents
------------------------------------------------------------------------

-- | Resource data, either text or binary blob.
data ResourceContents
  = ResourceText !TextResource
  | ResourceBlob !BlobResource
  deriving stock (Eq, Show)

-- | Text-based resource.
data TextResource = TextResource
  { textResUri :: !URI
  , textResMimeType :: !(Maybe Text)
  , textResText :: !Text
  }
  deriving stock (Eq, Show, Generic)

-- | Binary resource (base64 on the wire).
data BlobResource = BlobResource
  { blobResUri :: !URI
  , blobResMimeType :: !(Maybe Text)
  , blobResBlob :: !ByteString
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON TextResource where
  toJSON r =
    Aeson.object $
      [ "uri" .= r.textResUri
      , "text" .= r.textResText
      ]
        ++ maybe [] (\m -> ["mimeType" .= m]) r.textResMimeType

instance Aeson.FromJSON TextResource where
  parseJSON = Aeson.withObject "TextResource" $ \o ->
    TextResource <$> o .: "uri" <*> o .:? "mimeType" <*> o .: "text"

instance Aeson.ToJSON BlobResource where
  toJSON r =
    Aeson.object $
      [ "uri" .= r.blobResUri
      , "blob" .= decodeUtf8 (B64.encode r.blobResBlob)
      ]
        ++ maybe [] (\m -> ["mimeType" .= m]) r.blobResMimeType

instance Aeson.FromJSON BlobResource where
  parseJSON = Aeson.withObject "BlobResource" $ \o -> do
    uri <- o .: "uri"
    mime <- o .:? "mimeType"
    b64Text <- o .: "blob" :: Parser Text
    case B64.decode (encodeUtf8 b64Text) of
      Left err -> fail $ "Invalid base64 in blob: " ++ err
      Right bs -> pure (BlobResource uri mime bs)

instance Aeson.ToJSON ResourceContents where
  toJSON (ResourceText r) = Aeson.toJSON r
  toJSON (ResourceBlob r) = Aeson.toJSON r

instance Aeson.FromJSON ResourceContents where
  parseJSON = Aeson.withObject "ResourceContents" $ \o ->
    if KM.member "blob" o
      then ResourceBlob <$> Aeson.parseJSON (Aeson.Object o)
      else ResourceText <$> Aeson.parseJSON (Aeson.Object o)

------------------------------------------------------------------------
-- Content types
------------------------------------------------------------------------

-- | Text content block.
data TextContent = TextContent
  { textValue :: !Text
  , textAnnotations :: !(Maybe Annotations)
  }
  deriving stock (Eq, Show, Generic)

-- | Image content block (base64 data on the wire).
data ImageContent = ImageContent
  { imageData :: !ByteString
  , imageMimeType :: !Text
  , imageAnnotations :: !(Maybe Annotations)
  }
  deriving stock (Eq, Show, Generic)

-- | Audio content block (base64 data on the wire).
data AudioContent = AudioContent
  { audioData :: !ByteString
  , audioMimeType :: !Text
  , audioAnnotations :: !(Maybe Annotations)
  }
  deriving stock (Eq, Show, Generic)

-- | A link to a resource.
data ResourceLink = ResourceLink
  { linkUri :: !URI
  , linkName :: !Text
  , linkMimeType :: !(Maybe Text)
  , linkAnnotations :: !(Maybe Annotations)
  }
  deriving stock (Eq, Show, Generic)

-- | An embedded resource with its contents inline.
data EmbeddedResource = EmbeddedResource
  { embeddedResource :: !ResourceContents
  , embeddedAnnotations :: !(Maybe Annotations)
  }
  deriving stock (Eq, Show, Generic)

------------------------------------------------------------------------
-- ContentBlock
------------------------------------------------------------------------

-- | A block of content, discriminated by the @\"type\"@ field.
data ContentBlock
  = ContentText !TextContent
  | ContentImage !ImageContent
  | ContentAudio !AudioContent
  | ContentLink !ResourceLink
  | ContentEmbedded !EmbeddedResource
  deriving stock (Eq, Show)

------------------------------------------------------------------------
-- ToJSON for content types
------------------------------------------------------------------------

instance Aeson.ToJSON TextContent where
  toJSON tc =
    Aeson.object $
      [ "type" .= ("text" :: Text)
      , "text" .= tc.textValue
      ]
        ++ maybe [] (\a -> ["annotations" .= a]) tc.textAnnotations

instance Aeson.ToJSON ImageContent where
  toJSON ic =
    Aeson.object $
      [ "type" .= ("image" :: Text)
      , "data" .= decodeUtf8 (B64.encode ic.imageData)
      , "mimeType" .= ic.imageMimeType
      ]
        ++ maybe [] (\a -> ["annotations" .= a]) ic.imageAnnotations

instance Aeson.ToJSON AudioContent where
  toJSON ac =
    Aeson.object $
      [ "type" .= ("audio" :: Text)
      , "data" .= decodeUtf8 (B64.encode ac.audioData)
      , "mimeType" .= ac.audioMimeType
      ]
        ++ maybe [] (\a -> ["annotations" .= a]) ac.audioAnnotations

instance Aeson.ToJSON ResourceLink where
  toJSON rl =
    Aeson.object $
      [ "type" .= ("resource_link" :: Text)
      , "uri" .= rl.linkUri
      , "name" .= rl.linkName
      ]
        ++ maybe [] (\m -> ["mimeType" .= m]) rl.linkMimeType
        ++ maybe [] (\a -> ["annotations" .= a]) rl.linkAnnotations

instance Aeson.ToJSON EmbeddedResource where
  toJSON er =
    Aeson.object $
      [ "type" .= ("resource" :: Text)
      , "resource" .= er.embeddedResource
      ]
        ++ maybe [] (\a -> ["annotations" .= a]) er.embeddedAnnotations

instance Aeson.ToJSON ContentBlock where
  toJSON = \case
    ContentText tc -> Aeson.toJSON tc
    ContentImage ic -> Aeson.toJSON ic
    ContentAudio ac -> Aeson.toJSON ac
    ContentLink rl -> Aeson.toJSON rl
    ContentEmbedded er -> Aeson.toJSON er

------------------------------------------------------------------------
-- FromJSON for content types
------------------------------------------------------------------------

instance Aeson.FromJSON TextContent where
  parseJSON = Aeson.withObject "TextContent" $ \o ->
    TextContent <$> o .: "text" <*> o .:? "annotations"

instance Aeson.FromJSON ImageContent where
  parseJSON = Aeson.withObject "ImageContent" $ \o -> do
    b64Text <- o .: "data" :: Parser Text
    case B64.decode (encodeUtf8 b64Text) of
      Left err -> fail $ "Invalid base64 in image data: " ++ err
      Right bs -> ImageContent bs <$> o .: "mimeType" <*> o .:? "annotations"

instance Aeson.FromJSON AudioContent where
  parseJSON = Aeson.withObject "AudioContent" $ \o -> do
    b64Text <- o .: "data" :: Parser Text
    case B64.decode (encodeUtf8 b64Text) of
      Left err -> fail $ "Invalid base64 in audio data: " ++ err
      Right bs -> AudioContent bs <$> o .: "mimeType" <*> o .:? "annotations"

instance Aeson.FromJSON ResourceLink where
  parseJSON = Aeson.withObject "ResourceLink" $ \o ->
    ResourceLink <$> o .: "uri" <*> o .: "name" <*> o .:? "mimeType" <*> o .:? "annotations"

instance Aeson.FromJSON EmbeddedResource where
  parseJSON = Aeson.withObject "EmbeddedResource" $ \o ->
    EmbeddedResource <$> o .: "resource" <*> o .:? "annotations"

instance Aeson.FromJSON ContentBlock where
  parseJSON = Aeson.withObject "ContentBlock" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "text" -> ContentText <$> Aeson.parseJSON (Aeson.Object o)
      "image" -> ContentImage <$> Aeson.parseJSON (Aeson.Object o)
      "audio" -> ContentAudio <$> Aeson.parseJSON (Aeson.Object o)
      "resource_link" -> ContentLink <$> Aeson.parseJSON (Aeson.Object o)
      "resource" -> ContentEmbedded <$> Aeson.parseJSON (Aeson.Object o)
      other -> fail $ "Unknown ContentBlock type: " ++ T.unpack other
