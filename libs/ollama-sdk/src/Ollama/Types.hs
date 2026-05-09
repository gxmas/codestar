module Ollama.Types
  ( OllamaModel (..)
  , OllamaModelList (..)
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)


data OllamaModel = OllamaModel
  { name       :: Text
  , modifiedAt :: Text
  , size       :: Int
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON)

newtype OllamaModelList = OllamaModelList
  { models :: [OllamaModel]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON)
