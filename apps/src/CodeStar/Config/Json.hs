module CodeStar.Config.Json
  ( parseJsonConfig
  ) where

import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.:?))
import Data.ByteString (ByteString)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

import CodeStar.Config.Types

parseJsonConfig :: ByteString -> Either Text PartialConfig
parseJsonConfig bs =
  case eitherDecodeStrict' bs of
    Right (LegacyPartialConfig pc) -> Right pc
    Left err -> Left (Text.pack err)

newtype LegacyPartialConfig = LegacyPartialConfig PartialConfig

instance FromJSON LegacyPartialConfig where
  parseJSON = withObject "PartialConfig" $ \o -> do
    provider     <- o .:? "provider"
    planningMode <- o .:? "planningMode"
    indexStrat   <- o .:? "indexStrategy"
    permissions  <- o .:? "permissions"
    mcpEndpoints <- o .:? "mcpEndpoints"
    memCfg       <- o .:? "memoryConfig"

    -- Legacy flat budget fields
    maxSteps     <- o .:? "maxSteps"
    sessMax      <- o .:? "sessionTokenMax"
    dayMax       <- o .:? "dailyTokenMax"

    pure $ LegacyPartialConfig $ PartialConfig
      (Last provider)
      (Last Nothing)
      (Last Nothing)
      (Last planningMode)
      (Last Nothing)
      mempty
      (Last Nothing)
      (Last Nothing)
      (Last indexStrat)
      (Last permissions)
      (Last mcpEndpoints)
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      (PartialBudgetSection
        (Last maxSteps)
        (Last (fmap Just sessMax))
        (Last (fmap Just dayMax))
      )
      mempty
      (maybe mempty legacyMemory memCfg)

-- | Convert legacy MemoryConfig JSON object to PartialMemorySection
legacyMemory :: LegacyMemoryConfig -> PartialMemorySection
legacyMemory m =
  PartialMemorySection
    (Last m.lmEnabled)
    (Last m.lmMaxEntries)
    (Last m.lmAutoDiscover)

data LegacyMemoryConfig = LegacyMemoryConfig
  { lmEnabled      :: Maybe Bool
  , lmMaxEntries   :: Maybe Int
  , lmAutoDiscover :: Maybe Bool
  } deriving stock (Generic)

instance FromJSON LegacyMemoryConfig where
  parseJSON = withObject "LegacyMemoryConfig" $ \o ->
    LegacyMemoryConfig
      <$> o .:? "enabled"
      <*> o .:? "maxEntries"
      <*> o .:? "autoDiscover"
