-- | Batch API types.
module Anthropic.Protocol.Batch
  ( -- * Batch Request
    BatchItem (..)

    -- * Batch Response
  , BatchResponse (..)
  , BatchStatus (..)
  , BatchCounts (..)
  , BatchResultItem (..)
  , BatchResult (..)
  , DeletedBatch (..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:), (.:?)
  , object, withObject, withText
  , defaultOptions, genericToJSON, genericToEncoding, genericParseJSON
  )
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Options(..), Parser, camelTo2)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Anthropic.Types (ApiError)
import Anthropic.Protocol.Message (MessageRequest, MessageResponse)

customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_'
  , omitNothingFields = True
  }

-- | Batch processing status.
data BatchStatus
  = BatchInProgress
  | BatchCanceling
  | BatchEnded
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance ToJSON BatchStatus where
  toJSON BatchInProgress = "in_progress"
  toJSON BatchCanceling  = "canceling"
  toJSON BatchEnded      = "ended"
  toEncoding BatchInProgress = E.text "in_progress"
  toEncoding BatchCanceling  = E.text "canceling"
  toEncoding BatchEnded      = E.text "ended"

instance FromJSON BatchStatus where
  parseJSON = withText "BatchStatus" $ \case
    "in_progress" -> pure BatchInProgress
    "canceling"   -> pure BatchCanceling
    "ended"       -> pure BatchEnded
    other         -> fail $ "Unknown BatchStatus: " ++ T.unpack other

-- | Request counts within a batch.
data BatchCounts = BatchCounts
  { processing :: !Int
  , succeeded  :: !Int
  , errored    :: !Int
  , canceled   :: !Int
  , expired    :: !Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BatchCounts where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON BatchCounts where
  parseJSON = genericParseJSON customOptions

-- | A single item in a batch request.
data BatchItem = BatchItem
  { customId :: !Text
  , params   :: !MessageRequest
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BatchItem where
  toJSON bi = object
    [ "custom_id" .= bi.customId
    , "params"    .= bi.params
    ]
  toEncoding bi = E.pairs $
       "custom_id" .= bi.customId
    <> "params"    .= bi.params

instance FromJSON BatchItem where
  parseJSON = withObject "BatchItem" $ \o ->
    BatchItem
      <$> o .: "custom_id"
      <*> o .: "params"

-- | Batch response from the API.
data BatchResponse = BatchResponse
  { batchId          :: !Text
  , processingStatus :: !BatchStatus
  , requestCounts    :: !BatchCounts
  , createdAt        :: !Text
  , endedAt          :: !(Maybe Text)
  , expiresAt        :: !Text
  , archivedAt       :: !(Maybe Text)
  , cancelInitiatedAt :: !(Maybe Text)
  , resultsUrl       :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BatchResponse where
  toJSON br = object $
    [ "type"               .= ("message_batch" :: Text)
    , "id"                 .= br.batchId
    , "processing_status"  .= br.processingStatus
    , "request_counts"     .= br.requestCounts
    , "created_at"         .= br.createdAt
    , "expires_at"         .= br.expiresAt
    ]
    ++ maybe [] (\v -> ["ended_at"            .= v]) br.endedAt
    ++ maybe [] (\v -> ["archived_at"         .= v]) br.archivedAt
    ++ maybe [] (\v -> ["cancel_initiated_at" .= v]) br.cancelInitiatedAt
    ++ maybe [] (\v -> ["results_url"         .= v]) br.resultsUrl

  toEncoding br = E.pairs $
       "type"               .= ("message_batch" :: Text)
    <> "id"                 .= br.batchId
    <> "processing_status"  .= br.processingStatus
    <> "request_counts"     .= br.requestCounts
    <> "created_at"         .= br.createdAt
    <> "expires_at"         .= br.expiresAt
    <> foldMap ("ended_at"            .=) br.endedAt
    <> foldMap ("archived_at"         .=) br.archivedAt
    <> foldMap ("cancel_initiated_at" .=) br.cancelInitiatedAt
    <> foldMap ("results_url"         .=) br.resultsUrl

instance FromJSON BatchResponse where
  parseJSON = withObject "BatchResponse" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "message_batch" -> pure ()
      _               -> fail $ "Expected type 'message_batch', got: " ++ T.unpack typ
    BatchResponse
      <$> o .:  "id"
      <*> o .:  "processing_status"
      <*> o .:  "request_counts"
      <*> o .:  "created_at"
      <*> o .:? "ended_at"
      <*> o .:  "expires_at"
      <*> o .:? "archived_at"
      <*> o .:? "cancel_initiated_at"
      <*> o .:? "results_url"

-- | Result of a single batch item.
data BatchResult
  = BatchSucceeded !MessageResponse
  | BatchErrored   !ApiError
  | BatchCanceled
  | BatchExpired
  deriving stock (Eq, Show, Generic)

instance ToJSON BatchResult where
  toJSON (BatchSucceeded msg) = object
    [ "type"    .= ("succeeded" :: Text)
    , "message" .= msg
    ]
  toJSON (BatchErrored err) = object
    [ "type"  .= ("errored" :: Text)
    , "error" .= err
    ]
  toJSON BatchCanceled = object
    [ "type" .= ("canceled" :: Text) ]
  toJSON BatchExpired = object
    [ "type" .= ("expired" :: Text) ]

  toEncoding (BatchSucceeded msg) = E.pairs $
       "type"    .= ("succeeded" :: Text)
    <> "message" .= msg
  toEncoding (BatchErrored err) = E.pairs $
       "type"  .= ("errored" :: Text)
    <> "error" .= err
  toEncoding BatchCanceled = E.pairs $
    "type" .= ("canceled" :: Text)
  toEncoding BatchExpired = E.pairs $
    "type" .= ("expired" :: Text)

instance FromJSON BatchResult where
  parseJSON = withObject "BatchResult" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "succeeded" -> BatchSucceeded <$> o .: "message"
      "errored"   -> BatchErrored   <$> o .: "error"
      "canceled"  -> pure BatchCanceled
      "expired"   -> pure BatchExpired
      _           -> fail $ "Unknown BatchResult type: " ++ T.unpack typ

-- | A single result item from batch results JSONL.
data BatchResultItem = BatchResultItem
  { customId :: !Text
  , result   :: !BatchResult
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BatchResultItem where
  toJSON bri = object
    [ "custom_id" .= bri.customId
    , "result"    .= bri.result
    ]
  toEncoding bri = E.pairs $
       "custom_id" .= bri.customId
    <> "result"    .= bri.result

instance FromJSON BatchResultItem where
  parseJSON = withObject "BatchResultItem" $ \o ->
    BatchResultItem
      <$> o .: "custom_id"
      <*> o .: "result"

-- | Response from batch deletion.
data DeletedBatch = DeletedBatch
  { batchId :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DeletedBatch where
  toJSON db = object
    [ "id"   .= db.batchId
    , "type" .= ("message_batch_deleted" :: Text)
    ]
  toEncoding db = E.pairs $
       "id"   .= db.batchId
    <> "type" .= ("message_batch_deleted" :: Text)

instance FromJSON DeletedBatch where
  parseJSON = withObject "DeletedBatch" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "message_batch_deleted" -> pure ()
      _ -> fail $ "Expected type 'message_batch_deleted', got: " ++ T.unpack typ
    DeletedBatch <$> o .: "id"
