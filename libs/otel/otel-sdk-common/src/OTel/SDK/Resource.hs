-- | Resource types representing the entity producing telemetry.
module OTel.SDK.Resource
  ( -- * Resource
    Resource
  , create
  , empty
  , detect
  , merge
  , getAttributes
  , getSchemaUrl

    -- * Resource Detector
  , ResourceDetector (..)
  , SomeResourceDetector (..)

    -- * Errors
  , ResourceMergeError (..)
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, catch)
import Data.Text (Text)
import OTel.Attribute (Attribute, Attributes, emptyAttributes, fromList)


-- | An immutable collection of attributes describing the entity producing
-- telemetry. Resources are created once and attached to a 'TracerProvider'
-- or 'MeterProvider' for the lifetime of that provider.
data Resource = Resource
  { _resourceAttributes :: Attributes
  , _resourceSchemaUrl :: !(Maybe Text)
  } deriving stock (Eq, Show)


-- | Create a 'Resource' from explicit attributes and an optional schema URL.
create :: [Attribute] -> Maybe Text -> Resource
create attrs schemaUrl = Resource (fromList attrs) schemaUrl


-- | An empty resource with no attributes and no schema URL.
empty :: Resource
empty = Resource emptyAttributes Nothing


-- | Get the resource's attributes.
getAttributes :: Resource -> Attributes
getAttributes = _resourceAttributes


-- | Get the resource's schema URL, if set.
getSchemaUrl :: Resource -> Maybe Text
getSchemaUrl = _resourceSchemaUrl


-- | Error returned when merging two resources that have conflicting
-- (non-equal) schema URLs.
data ResourceMergeError = ResourceMergeError
  { mergeSchemaUrl1 :: !Text
  , mergeSchemaUrl2 :: !Text
  } deriving stock (Eq, Show)


-- | Merge two resources. The second ("updating") resource's attributes take
-- precedence on key conflicts. If both resources have schema URLs set and
-- they differ, a 'ResourceMergeError' is returned.
--
-- Schema URL precedence: the updating resource's URL is preferred; the old
-- resource's URL is used as a fallback.
merge :: Resource -> Resource -> Either ResourceMergeError Resource
merge old new_ = case (getSchemaUrl old, getSchemaUrl new_) of
  (Just s1, Just s2)
    | s1 /= s2 -> Left (ResourceMergeError s1 s2)
  _ ->
    Right Resource
      { _resourceAttributes = getAttributes new_ <> getAttributes old
        -- 'Attributes' Semigroup is left-biased ('Map.union'), so putting
        -- @new_@ on the left ensures its values win on key conflict.
      , _resourceSchemaUrl = getSchemaUrl new_ <|> getSchemaUrl old
      }


-- | Interface for detecting resource attributes from the environment.
-- Implementations may read environment variables, query system metadata,
-- or perform other IO to discover resource attributes.
class ResourceDetector d where
  detectResource :: d -> IO Resource


-- | Existential wrapper so heterogeneous detectors can be collected in a list.
data SomeResourceDetector = forall d. ResourceDetector d => SomeResourceDetector d


-- | Run multiple detectors and merge results left-to-right. Each subsequent
-- detector's resource is the "updating" resource, so later detectors'
-- attributes override earlier ones on key conflict.
--
-- Detectors that throw exceptions are silently suppressed (treated as
-- returning 'empty'). If a schema URL conflict arises during merging,
-- the conflicting detector's result is dropped and the accumulator is kept.
detect :: [SomeResourceDetector] -> IO Resource
detect detectors = do
  resources <- mapM runDetector detectors
  pure (foldl' mergeOrKeep empty resources)
  where
    mergeOrKeep acc r = case merge acc r of
      Right merged -> merged
      Left _ -> acc


runDetector :: SomeResourceDetector -> IO Resource
runDetector (SomeResourceDetector d) =
  detectResource d `catch` \(_ :: SomeException) -> pure empty
