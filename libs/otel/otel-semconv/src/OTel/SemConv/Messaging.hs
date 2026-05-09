-- | Messaging semantic conventions (semconv 1.27.0).
module OTel.SemConv.Messaging
  ( messagingSystem
  , messagingDestinationName
  , messagingDestinationTemplate
  , messagingDestinationTemporary
  , messagingDestinationAnonymous
  , messagingDestinationPartitionId
  , messagingMessageId
  , messagingMessageConversationId
  , messagingMessageBodySize
  , messagingMessageEnvelopeSize
  , messagingOperationName
  , messagingOperationType
  , messagingBatchMessageCount
  , messagingClientId
  , messagingConsumerGroupName
  ) where

import Data.Text (Text)

-- | @messaging.system@
messagingSystem :: Text
messagingSystem = "messaging.system"

-- | @messaging.destination.name@
messagingDestinationName :: Text
messagingDestinationName = "messaging.destination.name"

-- | @messaging.destination.template@
messagingDestinationTemplate :: Text
messagingDestinationTemplate = "messaging.destination.template"

-- | @messaging.destination.temporary@
messagingDestinationTemporary :: Text
messagingDestinationTemporary = "messaging.destination.temporary"

-- | @messaging.destination.anonymous@
messagingDestinationAnonymous :: Text
messagingDestinationAnonymous = "messaging.destination.anonymous"

-- | @messaging.destination.partition.id@
messagingDestinationPartitionId :: Text
messagingDestinationPartitionId = "messaging.destination.partition.id"

-- | @messaging.message.id@
messagingMessageId :: Text
messagingMessageId = "messaging.message.id"

-- | @messaging.message.conversation_id@
messagingMessageConversationId :: Text
messagingMessageConversationId = "messaging.message.conversation_id"

-- | @messaging.message.body.size@
messagingMessageBodySize :: Text
messagingMessageBodySize = "messaging.message.body.size"

-- | @messaging.message.envelope.size@
messagingMessageEnvelopeSize :: Text
messagingMessageEnvelopeSize = "messaging.message.envelope.size"

-- | @messaging.operation.name@
messagingOperationName :: Text
messagingOperationName = "messaging.operation.name"

-- | @messaging.operation.type@
messagingOperationType :: Text
messagingOperationType = "messaging.operation.type"

-- | @messaging.batch.message_count@
messagingBatchMessageCount :: Text
messagingBatchMessageCount = "messaging.batch.message_count"

-- | @messaging.client_id@
messagingClientId :: Text
messagingClientId = "messaging.client_id"

-- | @messaging.consumer.group.name@
messagingConsumerGroupName :: Text
messagingConsumerGroupName = "messaging.consumer.group.name"
