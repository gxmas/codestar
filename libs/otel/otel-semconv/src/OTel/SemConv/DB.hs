-- | Database semantic conventions (semconv 1.27.0).
module OTel.SemConv.DB
  ( dbSystem
  , dbNamespace
  , dbQueryText
  , dbOperationName
  , dbCollectionName
  , dbOperationBatchSize
  , dbResponseStatusCode
  , dbInstanceId
  ) where

import Data.Text (Text)

-- | @db.system@
dbSystem :: Text
dbSystem = "db.system"

-- | @db.namespace@
dbNamespace :: Text
dbNamespace = "db.namespace"

-- | @db.query.text@
dbQueryText :: Text
dbQueryText = "db.query.text"

-- | @db.operation.name@
dbOperationName :: Text
dbOperationName = "db.operation.name"

-- | @db.collection.name@
dbCollectionName :: Text
dbCollectionName = "db.collection.name"

-- | @db.operation.batch.size@
dbOperationBatchSize :: Text
dbOperationBatchSize = "db.operation.batch.size"

-- | @db.response.status_code@
dbResponseStatusCode :: Text
dbResponseStatusCode = "db.response.status_code"

-- | @db.instance.id@
dbInstanceId :: Text
dbInstanceId = "db.instance.id"
