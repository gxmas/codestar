-- |
-- Module      : Network.MCP.Client.Sampling
-- Stability   : stable
--
-- Client-side sampling feature: handles server requests to generate LLM
-- completions on behalf of the server.
module Network.MCP.Client.Sampling
  ( SamplingMessage (..)
  , SamplingContent (..)
  , ToolUseContent (..)
  , ToolResultContent (..)
  , ModelPreferences (..)
  , ModelHint (..)
  , ToolChoice (..)
  , ToolChoiceMode (..)
  , SamplingRequest (..)
  , SamplingResult (..)
  , StopReason (..)
  , Sampler (..)
  , SamplerError (..)
  , SamplerErrorKind (..)
  , SamplingFeature
  , newSamplingFeature
  , attach
  , detach
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, writeTVar)
import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Encoding as E
import Data.Text (Text)
import qualified Data.Text as T

import Network.MCP.Session (Session (..))
import Network.MCP.Types (RPCError (..))
import Network.MCP.Types.Content
  ( AudioContent (..)
  , ContentBlock (..)
  , ImageContent (..)
  , Role (..)
  , TextContent (..)
  )

------------------------------------------------------------------------
-- ToolUseContent
------------------------------------------------------------------------

-- | A tool-use block: server asks the model to call a tool.
data ToolUseContent = ToolUseContent
  { toolUseId :: !Text
  , toolUseName :: !Text
  , toolUseInput :: !Aeson.Value
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ToolUseContent where
  toJSON t =
    Aeson.object
      [ "type" .= ("tool_use" :: Text)
      , "id" .= t.toolUseId
      , "name" .= t.toolUseName
      , "input" .= t.toolUseInput
      ]
  toEncoding t =
    E.pairs $
      "type" .= ("tool_use" :: Text)
        <> "id" .= t.toolUseId
        <> "name" .= t.toolUseName
        <> "input" .= t.toolUseInput

instance Aeson.FromJSON ToolUseContent where
  parseJSON = Aeson.withObject "ToolUseContent" $ \o ->
    ToolUseContent
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "input"

------------------------------------------------------------------------
-- ToolResultContent
------------------------------------------------------------------------

-- | A tool-result block: the model returns the output of a tool call.
data ToolResultContent = ToolResultContent
  { toolResultId :: !Text
  , toolResultContent :: ![ContentBlock]
  , toolResultIsError :: !(Maybe Bool)
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ToolResultContent where
  toJSON r =
    Aeson.object $
      [ "type" .= ("tool_result" :: Text)
      , "toolUseId" .= r.toolResultId
      , "content" .= r.toolResultContent
      ]
        ++ maybe [] (\e -> ["isError" .= e]) r.toolResultIsError
  toEncoding r =
    E.pairs $
      "type" .= ("tool_result" :: Text)
        <> "toolUseId" .= r.toolResultId
        <> "content" .= r.toolResultContent
        <> foldMap ("isError" .=) r.toolResultIsError

instance Aeson.FromJSON ToolResultContent where
  parseJSON = Aeson.withObject "ToolResultContent" $ \o ->
    ToolResultContent
      <$> o .: "toolUseId"
      <*> o .: "content"
      <*> o .:? "isError"

------------------------------------------------------------------------
-- SamplingContent
------------------------------------------------------------------------

-- | Content that can appear in a sampling message (subset of ContentBlock).
data SamplingContent
  = SamplingText !TextContent
  | SamplingImage !ImageContent
  | SamplingAudio !AudioContent
  deriving stock (Eq, Show)

instance Aeson.ToJSON SamplingContent where
  toJSON = \case
    SamplingText tc -> Aeson.toJSON tc
    SamplingImage ic -> Aeson.toJSON ic
    SamplingAudio ac -> Aeson.toJSON ac

instance Aeson.FromJSON SamplingContent where
  parseJSON = Aeson.withObject "SamplingContent" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "text" -> SamplingText <$> Aeson.parseJSON (Aeson.Object o)
      "image" -> SamplingImage <$> Aeson.parseJSON (Aeson.Object o)
      "audio" -> SamplingAudio <$> Aeson.parseJSON (Aeson.Object o)
      other -> fail $ "Unknown SamplingContent type: " ++ T.unpack other

------------------------------------------------------------------------
-- SamplingMessage
------------------------------------------------------------------------

-- | A message in a sampling conversation.
data SamplingMessage
  = SamplingUserMessage !SamplingContent
  | SamplingAssistantMessage !SamplingContent
  | SamplingToolUse ![ToolUseContent]
  | SamplingToolResult ![ToolResultContent]
  deriving stock (Eq, Show)

instance Aeson.ToJSON SamplingMessage where
  toJSON = \case
    SamplingUserMessage c ->
      Aeson.object ["role" .= RoleUser, "content" .= c]
    SamplingAssistantMessage c ->
      Aeson.object ["role" .= RoleAssistant, "content" .= c]
    SamplingToolUse uses ->
      Aeson.object ["role" .= RoleAssistant, "content" .= uses]
    SamplingToolResult results ->
      Aeson.object ["role" .= RoleUser, "content" .= results]

instance Aeson.FromJSON SamplingMessage where
  parseJSON = Aeson.withObject "SamplingMessage" $ \o -> do
    role <- o .: "role" :: Parser Role
    contentVal <- o .: "content"
    case role of
      RoleUser ->
        -- content could be a single SamplingContent or a list of ToolResultContent
        case contentVal of
          Aeson.Array _ ->
            SamplingToolResult <$> Aeson.parseJSON contentVal
          _ ->
            SamplingUserMessage <$> Aeson.parseJSON contentVal
      RoleAssistant ->
        case contentVal of
          Aeson.Array _ ->
            SamplingToolUse <$> Aeson.parseJSON contentVal
          _ ->
            SamplingAssistantMessage <$> Aeson.parseJSON contentVal

------------------------------------------------------------------------
-- ModelHint / ModelPreferences
------------------------------------------------------------------------

-- | A hint about which model to prefer.
data ModelHint = ModelHint
  { hintName :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ModelHint where
  toJSON h =
    Aeson.object $ maybe [] (\n -> ["name" .= n]) h.hintName
  toEncoding h =
    E.pairs $ foldMap ("name" .=) h.hintName

instance Aeson.FromJSON ModelHint where
  parseJSON = Aeson.withObject "ModelHint" $ \o ->
    ModelHint <$> o .:? "name"

-- | Model selection preferences.
data ModelPreferences = ModelPreferences
  { prefHints :: !(Maybe [ModelHint])
  , prefCostPriority :: !(Maybe Double)
  , prefSpeedPriority :: !(Maybe Double)
  , prefIntelligencePriority :: !(Maybe Double)
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ModelPreferences where
  toJSON mp =
    Aeson.object $
      maybe [] (\h -> ["hints" .= h]) mp.prefHints
        ++ maybe [] (\c -> ["costPriority" .= c]) mp.prefCostPriority
        ++ maybe [] (\s -> ["speedPriority" .= s]) mp.prefSpeedPriority
        ++ maybe [] (\i -> ["intelligencePriority" .= i]) mp.prefIntelligencePriority
  toEncoding mp =
    E.pairs $
      foldMap ("hints" .=) mp.prefHints
        <> foldMap ("costPriority" .=) mp.prefCostPriority
        <> foldMap ("speedPriority" .=) mp.prefSpeedPriority
        <> foldMap ("intelligencePriority" .=) mp.prefIntelligencePriority

instance Aeson.FromJSON ModelPreferences where
  parseJSON = Aeson.withObject "ModelPreferences" $ \o ->
    ModelPreferences
      <$> o .:? "hints"
      <*> o .:? "costPriority"
      <*> o .:? "speedPriority"
      <*> o .:? "intelligencePriority"

------------------------------------------------------------------------
-- ToolChoice
------------------------------------------------------------------------

-- | How the model should choose tools.
data ToolChoiceMode
  = ToolChoiceAuto
  | ToolChoiceRequired
  | ToolChoiceNone
  deriving stock (Eq, Show)

instance Aeson.ToJSON ToolChoiceMode where
  toJSON = \case
    ToolChoiceAuto -> "auto"
    ToolChoiceRequired -> "required"
    ToolChoiceNone -> "none"
  toEncoding = \case
    ToolChoiceAuto -> E.text "auto"
    ToolChoiceRequired -> E.text "required"
    ToolChoiceNone -> E.text "none"

instance Aeson.FromJSON ToolChoiceMode where
  parseJSON = Aeson.withText "ToolChoiceMode" $ \case
    "auto" -> pure ToolChoiceAuto
    "required" -> pure ToolChoiceRequired
    "none" -> pure ToolChoiceNone
    other -> fail $ "Unknown ToolChoiceMode: " ++ T.unpack other

-- | Tool selection configuration.
data ToolChoice = ToolChoice
  { toolChoiceMode :: !ToolChoiceMode
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ToolChoice where
  toJSON tc =
    Aeson.object ["mode" .= tc.toolChoiceMode]
  toEncoding tc =
    E.pairs $ "mode" .= tc.toolChoiceMode

instance Aeson.FromJSON ToolChoice where
  parseJSON = Aeson.withObject "ToolChoice" $ \o ->
    ToolChoice <$> o .: "mode"

------------------------------------------------------------------------
-- SamplingRequest
------------------------------------------------------------------------

-- | A request to generate an LLM completion.
data SamplingRequest = SamplingRequest
  { sampReqMessages :: ![SamplingMessage]
  , sampReqModelPreferences :: !(Maybe ModelPreferences)
  , sampReqSystemPrompt :: !(Maybe Text)
  , sampReqMaxTokens :: !Word
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON SamplingRequest where
  toJSON sr =
    Aeson.object $
      [ "messages" .= sr.sampReqMessages
      , "maxTokens" .= sr.sampReqMaxTokens
      ]
        ++ maybe [] (\mp -> ["modelPreferences" .= mp]) sr.sampReqModelPreferences
        ++ maybe [] (\sp -> ["systemPrompt" .= sp]) sr.sampReqSystemPrompt
  toEncoding sr =
    E.pairs $
      "messages" .= sr.sampReqMessages
        <> "maxTokens" .= sr.sampReqMaxTokens
        <> foldMap ("modelPreferences" .=) sr.sampReqModelPreferences
        <> foldMap ("systemPrompt" .=) sr.sampReqSystemPrompt

instance Aeson.FromJSON SamplingRequest where
  parseJSON = Aeson.withObject "SamplingRequest" $ \o ->
    SamplingRequest
      <$> o .: "messages"
      <*> o .:? "modelPreferences"
      <*> o .:? "systemPrompt"
      <*> o .: "maxTokens"

------------------------------------------------------------------------
-- StopReason / SamplingResult
------------------------------------------------------------------------

-- | The reason generation stopped.
data StopReason
  = StopEndTurn
  | StopToolUse
  | StopMaxTokens
  | StopStopSequence
  deriving stock (Eq, Show)

instance Aeson.ToJSON StopReason where
  toJSON = \case
    StopEndTurn -> "end_turn"
    StopToolUse -> "tool_use"
    StopMaxTokens -> "max_tokens"
    StopStopSequence -> "stop_sequence"
  toEncoding = \case
    StopEndTurn -> E.text "end_turn"
    StopToolUse -> E.text "tool_use"
    StopMaxTokens -> E.text "max_tokens"
    StopStopSequence -> E.text "stop_sequence"

instance Aeson.FromJSON StopReason where
  parseJSON = Aeson.withText "StopReason" $ \case
    "end_turn" -> pure StopEndTurn
    "tool_use" -> pure StopToolUse
    "max_tokens" -> pure StopMaxTokens
    "stop_sequence" -> pure StopStopSequence
    other -> fail $ "Unknown StopReason: " ++ T.unpack other

-- | The result of a sampling request.
data SamplingResult = SamplingResult
  { sampResRole :: !Role
  , sampResContent :: !SamplingContent
  , sampResModel :: !Text
  , sampResStopReason :: !StopReason
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON SamplingResult where
  toJSON sr =
    Aeson.object
      [ "role" .= sr.sampResRole
      , "content" .= sr.sampResContent
      , "model" .= sr.sampResModel
      , "stopReason" .= sr.sampResStopReason
      ]
  toEncoding sr =
    E.pairs $
      "role" .= sr.sampResRole
        <> "content" .= sr.sampResContent
        <> "model" .= sr.sampResModel
        <> "stopReason" .= sr.sampResStopReason

instance Aeson.FromJSON SamplingResult where
  parseJSON = Aeson.withObject "SamplingResult" $ \o ->
    SamplingResult
      <$> o .: "role"
      <*> o .: "content"
      <*> o .: "model"
      <*> o .: "stopReason"

------------------------------------------------------------------------
-- Sampler / SamplerError
------------------------------------------------------------------------

-- | Classification of sampler failures.
data SamplerErrorKind
  = UserRejected
  | ModelUnavailable
  | SamplerInternal
  deriving stock (Eq, Show)

-- | A sampler failure with kind and detail.
data SamplerError = SamplerError
  { samplerErrorKind :: !SamplerErrorKind
  , samplerErrorDetail :: !Text
  }
  deriving stock (Eq, Show)

-- | Interface for generating LLM completions.
class Sampler s where
  sample :: s -> SamplingRequest -> IO (Either SamplerError SamplingResult)

------------------------------------------------------------------------
-- SamplingFeature
------------------------------------------------------------------------

-- | Existential wrapper so the feature can hold any 'Sampler'.
data SomeSampler = forall s. Sampler s => SomeSampler s

-- | Opaque handle for the sampling feature.
data SamplingFeature = SamplingFeature
  { sampler :: !SomeSampler
  , sessionVar :: !(TVar (Maybe Session))
  }

------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------

-- | Create a new sampling feature backed by the given sampler.
newSamplingFeature :: Sampler s => s -> IO SamplingFeature
newSamplingFeature s = do
  var <- newTVarIO Nothing
  pure (SamplingFeature (SomeSampler s) var)

-- | Attach the feature to a session: registers the @sampling/createMessage@ handler.
attach :: SamplingFeature -> Session -> IO ()
attach feat session = do
  atomically $ writeTVar feat.sessionVar (Just session)
  session.sessionOnRequest "sampling/createMessage" $ \params _meta -> do
    case Aeson.fromJSON params of
      Aeson.Error err ->
        pure (Left RPCError
          { rpcErrorCode = -32602
          , rpcErrorMessage = T.pack err
          , rpcErrorData = Nothing
          })
      Aeson.Success req ->
        case feat.sampler of
          SomeSampler s -> do
            result <- sample s req
            case result of
              Left SamplerError{samplerErrorKind = UserRejected, samplerErrorDetail = detail} ->
                pure (Left RPCError
                  { rpcErrorCode = -1
                  , rpcErrorMessage = "User rejected sampling request"
                  , rpcErrorData = if T.null detail then Nothing else Just (Aeson.String detail)
                  })
              Left SamplerError{samplerErrorDetail = detail} ->
                pure (Left RPCError
                  { rpcErrorCode = -32603
                  , rpcErrorMessage = detail
                  , rpcErrorData = Nothing
                  })
              Right samplingResult ->
                pure (Right (Aeson.toJSON samplingResult))

-- | Detach the feature from the current session.
detach :: SamplingFeature -> IO ()
detach feat = atomically $ writeTVar feat.sessionVar Nothing
