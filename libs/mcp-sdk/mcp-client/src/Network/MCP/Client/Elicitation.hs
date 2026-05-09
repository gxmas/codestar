-- |
-- Module      : Network.MCP.Client.Elicitation
-- Stability   : stable
--
-- Client-side elicitation feature: handles server requests for
-- structured user input.
module Network.MCP.Client.Elicitation
  ( PrimitiveSchema (..)
  , PrimFormat (..)
  , ElicitationSchema (..)
  , ElicitationAction (..)
  , ElicitationHandler (..)
  , ElicitationFeature
  , newElicitationFeature
  , attach
  , detach
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, writeTVar)
import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Encoding as E
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T

import Network.MCP.Session (RequestMeta, Session (..))
import qualified Network.MCP.Types as Types

------------------------------------------------------------------------
-- PrimFormat
------------------------------------------------------------------------

-- | Format constraints for string schemas.
data PrimFormat
  = FormatEmail
  | FormatUri
  | FormatDate
  | FormatDateTime
  deriving stock (Eq, Show)

instance Aeson.ToJSON PrimFormat where
  toJSON = \case
    FormatEmail -> "email"
    FormatUri -> "uri"
    FormatDate -> "date"
    FormatDateTime -> "date-time"
  toEncoding = \case
    FormatEmail -> E.text "email"
    FormatUri -> E.text "uri"
    FormatDate -> E.text "date"
    FormatDateTime -> E.text "date-time"

instance Aeson.FromJSON PrimFormat where
  parseJSON = Aeson.withText "PrimFormat" $ \case
    "email" -> pure FormatEmail
    "uri" -> pure FormatUri
    "date" -> pure FormatDate
    "date-time" -> pure FormatDateTime
    other -> fail $ "Unknown PrimFormat: " ++ T.unpack other

------------------------------------------------------------------------
-- PrimitiveSchema
------------------------------------------------------------------------

-- | A JSON-schema primitive, discriminated by @type@ (and presence of @enum@).
data PrimitiveSchema
  = PrimString
      { primMinLength :: !(Maybe Word)
      , primMaxLength :: !(Maybe Word)
      , primFormat :: !(Maybe PrimFormat)
      , primStrDefault :: !(Maybe Text)
      }
  | PrimNumber
      { primMinimum :: !(Maybe Double)
      , primMaximum :: !(Maybe Double)
      , primNumDefault :: !(Maybe Double)
      }
  | PrimInteger
      { primIntMinimum :: !(Maybe Int)
      , primIntMaximum :: !(Maybe Int)
      , primIntDefault :: !(Maybe Int)
      }
  | PrimBoolean
      { primBoolDefault :: !(Maybe Bool)
      }
  | PrimEnum
      { primEnumValues :: ![Text]
      , primEnumDefault :: !(Maybe Text)
      }
  deriving stock (Eq, Show)

instance Aeson.ToJSON PrimitiveSchema where
  toJSON = \case
    PrimString minLen maxLen fmt def ->
      Aeson.object $
        ["type" .= ("string" :: Text)]
          ++ maybe [] (\v -> ["minLength" .= v]) minLen
          ++ maybe [] (\v -> ["maxLength" .= v]) maxLen
          ++ maybe [] (\v -> ["format" .= v]) fmt
          ++ maybe [] (\v -> ["default" .= v]) def
    PrimNumber mn mx def ->
      Aeson.object $
        ["type" .= ("number" :: Text)]
          ++ maybe [] (\v -> ["minimum" .= v]) mn
          ++ maybe [] (\v -> ["maximum" .= v]) mx
          ++ maybe [] (\v -> ["default" .= v]) def
    PrimInteger mn mx def ->
      Aeson.object $
        ["type" .= ("integer" :: Text)]
          ++ maybe [] (\v -> ["minimum" .= v]) mn
          ++ maybe [] (\v -> ["maximum" .= v]) mx
          ++ maybe [] (\v -> ["default" .= v]) def
    PrimBoolean def ->
      Aeson.object $
        ["type" .= ("boolean" :: Text)]
          ++ maybe [] (\v -> ["default" .= v]) def
    PrimEnum vals def ->
      Aeson.object $
        [ "type" .= ("string" :: Text)
        , "enum" .= vals
        ]
          ++ maybe [] (\v -> ["default" .= v]) def
  toEncoding = \case
    PrimString minLen maxLen fmt def ->
      E.pairs $
        "type" .= ("string" :: Text)
          <> foldMap ("minLength" .=) minLen
          <> foldMap ("maxLength" .=) maxLen
          <> foldMap ("format" .=) fmt
          <> foldMap ("default" .=) def
    PrimNumber mn mx def ->
      E.pairs $
        "type" .= ("number" :: Text)
          <> foldMap ("minimum" .=) mn
          <> foldMap ("maximum" .=) mx
          <> foldMap ("default" .=) def
    PrimInteger mn mx def ->
      E.pairs $
        "type" .= ("integer" :: Text)
          <> foldMap ("minimum" .=) mn
          <> foldMap ("maximum" .=) mx
          <> foldMap ("default" .=) def
    PrimBoolean def ->
      E.pairs $
        "type" .= ("boolean" :: Text)
          <> foldMap ("default" .=) def
    PrimEnum vals def ->
      E.pairs $
        "type" .= ("string" :: Text)
          <> "enum" .= vals
          <> foldMap ("default" .=) def

instance Aeson.FromJSON PrimitiveSchema where
  parseJSON = Aeson.withObject "PrimitiveSchema" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "string" -> do
        mEnum <- o .:? "enum"
        case mEnum of
          Just vals ->
            PrimEnum vals <$> o .:? "default"
          Nothing ->
            PrimString
              <$> o .:? "minLength"
              <*> o .:? "maxLength"
              <*> o .:? "format"
              <*> o .:? "default"
      "number" ->
        PrimNumber
          <$> o .:? "minimum"
          <*> o .:? "maximum"
          <*> o .:? "default"
      "integer" ->
        PrimInteger
          <$> o .:? "minimum"
          <*> o .:? "maximum"
          <*> o .:? "default"
      "boolean" ->
        PrimBoolean <$> o .:? "default"
      other -> fail $ "Unknown PrimitiveSchema type: " ++ T.unpack other

------------------------------------------------------------------------
-- ElicitationSchema
------------------------------------------------------------------------

-- | A structured schema describing the fields to elicit from the user.
data ElicitationSchema = ElicitationSchema
  { elicitProperties :: !(HashMap Text PrimitiveSchema)
  , elicitRequired :: !(Maybe [Text])
  , elicitTitle :: !(Maybe Text)
  , elicitDescription :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ElicitationSchema where
  toJSON es =
    Aeson.object $
      ["properties" .= es.elicitProperties]
        ++ maybe [] (\v -> ["required" .= v]) es.elicitRequired
        ++ maybe [] (\v -> ["title" .= v]) es.elicitTitle
        ++ maybe [] (\v -> ["description" .= v]) es.elicitDescription
  toEncoding es =
    E.pairs $
      "properties" .= es.elicitProperties
        <> foldMap ("required" .=) es.elicitRequired
        <> foldMap ("title" .=) es.elicitTitle
        <> foldMap ("description" .=) es.elicitDescription

instance Aeson.FromJSON ElicitationSchema where
  parseJSON = Aeson.withObject "ElicitationSchema" $ \o ->
    ElicitationSchema
      <$> o .: "properties"
      <*> o .:? "required"
      <*> o .:? "title"
      <*> o .:? "description"

------------------------------------------------------------------------
-- ElicitationAction
------------------------------------------------------------------------

-- | The user's response to an elicitation request.
data ElicitationAction
  = ElicitAccept !(HashMap Text Aeson.Value)
  | ElicitDecline
  | ElicitCancel
  deriving stock (Eq, Show)

instance Aeson.ToJSON ElicitationAction where
  toJSON = \case
    ElicitAccept content ->
      Aeson.object ["action" .= ("accept" :: Text), "content" .= content]
    ElicitDecline ->
      Aeson.object ["action" .= ("decline" :: Text)]
    ElicitCancel ->
      Aeson.object ["action" .= ("cancel" :: Text)]
  toEncoding = \case
    ElicitAccept content ->
      E.pairs $
        "action" .= ("accept" :: Text)
          <> "content" .= content
    ElicitDecline ->
      E.pairs $ "action" .= ("decline" :: Text)
    ElicitCancel ->
      E.pairs $ "action" .= ("cancel" :: Text)

instance Aeson.FromJSON ElicitationAction where
  parseJSON = Aeson.withObject "ElicitationAction" $ \o -> do
    action <- o .: "action" :: Parser Text
    case action of
      "accept" -> ElicitAccept <$> o .: "content"
      "decline" -> pure ElicitDecline
      "cancel" -> pure ElicitCancel
      other -> fail $ "Unknown ElicitationAction: " ++ T.unpack other

------------------------------------------------------------------------
-- ElicitationHandler
------------------------------------------------------------------------

-- | Callback interface for handling elicitation requests.
data ElicitationHandler = ElicitationHandler
  { handleElicitation :: Text -> ElicitationSchema -> RequestMeta -> IO ElicitationAction
  }

------------------------------------------------------------------------
-- ElicitationFeature
------------------------------------------------------------------------

-- | Opaque handle for the elicitation feature.
data ElicitationFeature = ElicitationFeature
  { handler :: !ElicitationHandler
  , sessionVar :: !(TVar (Maybe Session))
  }

------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------

-- | Create a new elicitation feature backed by the given handler.
newElicitationFeature :: ElicitationHandler -> IO ElicitationFeature
newElicitationFeature h = do
  var <- newTVarIO Nothing
  pure (ElicitationFeature h var)

-- | Attach the feature to a session: registers the @elicitation/create@ handler.
attach :: ElicitationFeature -> Session -> IO ()
attach feat session = do
  atomically $ writeTVar feat.sessionVar (Just session)
  session.sessionOnRequest "elicitation/create" $ \params meta -> do
    case Aeson.fromJSON params of
      Aeson.Error err -> pure (Left (mkRPCError err))
      Aeson.Success (ElicitParams (msg, schema)) -> do
        action <- feat.handler.handleElicitation msg schema meta
        pure (Right (Aeson.toJSON action))
  where
    mkRPCError msg = Types.RPCError
      { Types.rpcErrorCode = -32602
      , Types.rpcErrorMessage = T.pack msg
      , Types.rpcErrorData = Nothing
      }

-- | Detach the feature from the current session.
detach :: ElicitationFeature -> IO ()
detach feat = atomically $ writeTVar feat.sessionVar Nothing

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- Newtype wrapper to parse the elicitation/create params object.
newtype ElicitParams = ElicitParams (Text, ElicitationSchema)

instance Aeson.FromJSON ElicitParams where
  parseJSON = Aeson.withObject "elicitation/create params" $ \o ->
    fmap ElicitParams $
      (,)
        <$> o .: "message"
        <*> o .: "requestedSchema"
