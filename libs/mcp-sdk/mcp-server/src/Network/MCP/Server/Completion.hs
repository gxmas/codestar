-- |
-- Module      : Network.MCP.Server.Completion
-- Stability   : stable
--
-- Completion feature for MCP servers. Registers the
-- @completion/complete@ request handler.
module Network.MCP.Server.Completion
  ( -- * Feature
    CompletionFeature
  , newCompletionFeature
  , attach
  , detach

    -- * Types
  , CompletionRef (..)
  , CompletionArgument (..)
  , CompletionResult (..)
  , CompletionProvider (..)
  ) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , newTVarIO
  , writeTVar
  )
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T

import Network.MCP.Session (RequestMeta (..), Session (..))
import Network.MCP.Types (RPCError (..))

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | A reference to the entity being completed (prompt or resource).
data CompletionRef
  = RefPrompt   !Text
  | RefResource !Text
  deriving stock (Eq, Show)

instance Aeson.ToJSON CompletionRef where
  toJSON (RefPrompt name) =
    Aeson.object ["type" .= ("ref/prompt" :: Text), "name" .= name]
  toJSON (RefResource uri) =
    Aeson.object ["type" .= ("ref/resource" :: Text), "uri" .= uri]
  toEncoding (RefPrompt name) =
    E.pairs ("type" .= ("ref/prompt" :: Text) <> "name" .= name)
  toEncoding (RefResource uri) =
    E.pairs ("type" .= ("ref/resource" :: Text) <> "uri" .= uri)

instance Aeson.FromJSON CompletionRef where
  parseJSON = Aeson.withObject "CompletionRef" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "ref/prompt"   -> RefPrompt   <$> o .: "name"
      "ref/resource" -> RefResource <$> o .: "uri"
      other          -> fail $ "Unknown CompletionRef type: " ++ T.unpack other

-- | The argument being completed.
data CompletionArgument = CompletionArgument
  { compArgName  :: !Text
  , compArgValue :: !Text
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON CompletionArgument where
  toJSON a =
    Aeson.object ["name" .= a.compArgName, "value" .= a.compArgValue]
  toEncoding a =
    E.pairs ("name" .= a.compArgName <> "value" .= a.compArgValue)

instance Aeson.FromJSON CompletionArgument where
  parseJSON = Aeson.withObject "CompletionArgument" $ \o ->
    CompletionArgument <$> o .: "name" <*> o .: "value"

-- | The result of a completion request.
data CompletionResult = CompletionResult
  { compValues  :: ![Text]
  , compTotal   :: !(Maybe Word)
  , compHasMore :: !Bool
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON CompletionResult where
  toJSON r =
    Aeson.object $
      [ "values"  .= r.compValues
      , "hasMore" .= r.compHasMore
      ]
        ++ maybe [] (\t -> ["total" .= t]) r.compTotal
  toEncoding r =
    E.pairs $
      "values" .= r.compValues
        <> "hasMore" .= r.compHasMore
        <> foldMap ("total" .=) r.compTotal

instance Aeson.FromJSON CompletionResult where
  parseJSON = Aeson.withObject "CompletionResult" $ \o ->
    CompletionResult
      <$> o .:  "values"
      <*> o Aeson..:? "total"
      <*> o .:  "hasMore"

------------------------------------------------------------------------
-- Provider typeclass
------------------------------------------------------------------------

-- | Typeclass for completion providers.
class CompletionProvider p where
  complete
    :: p
    -> CompletionRef
    -> CompletionArgument
    -> RequestMeta
    -> IO (Either RPCError CompletionResult)

------------------------------------------------------------------------
-- Feature
------------------------------------------------------------------------

-- | Opaque completion feature, wrapping an existential provider.
-- The provider field is existentially quantified; access via pattern match.
data CompletionFeature = forall p. CompletionProvider p =>
  CompletionFeature !p !(TVar (Maybe Session))

newCompletionFeature :: CompletionProvider p => p -> IO CompletionFeature
newCompletionFeature provider = do
  sesTVar <- newTVarIO Nothing
  pure (CompletionFeature provider sesTVar)

attach :: CompletionFeature -> Session -> IO ()
attach cf@(CompletionFeature _ sesTVar) session = do
  atomically (writeTVar sesTVar (Just session))
  session.sessionOnRequest "completion/complete" (handleComplete cf)

detach :: CompletionFeature -> IO ()
detach (CompletionFeature _ sesTVar) = atomically (writeTVar sesTVar Nothing)

------------------------------------------------------------------------
-- Handler
------------------------------------------------------------------------

maxValues :: Int
maxValues = 100

handleComplete
  :: CompletionFeature
  -> Aeson.Value
  -> RequestMeta
  -> IO (Either RPCError Aeson.Value)
handleComplete (CompletionFeature provider _sesTVar) params meta =
  case Aeson.fromJSON params of
    Aeson.Error err ->
      pure (Left (RPCError (-32602) (T.pack $ "Invalid params: " ++ err) Nothing))
    Aeson.Success (CompleteParams ref arg) -> do
      result <- complete provider ref arg meta
      case result of
        Left err -> pure (Left err)
        Right r  -> do
          let (values', hasMore') = capValues r.compValues r.compHasMore
              r' = r { compValues = values', compHasMore = hasMore' }
          pure (Right (Aeson.object ["completion" .= r']))
  where
    capValues vs hasMore
      | length vs > maxValues = (take maxValues vs, True)
      | otherwise             = (vs, hasMore)

------------------------------------------------------------------------
-- Internal param type
------------------------------------------------------------------------

data CompleteParams = CompleteParams !CompletionRef !CompletionArgument

instance Aeson.FromJSON CompleteParams where
  parseJSON = Aeson.withObject "CompleteParams" $ \o ->
    CompleteParams
      <$> o .: "ref"
      <*> o .: "argument"
