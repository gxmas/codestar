-- | Built-in resource detectors for common environment attributes.
module OTel.SDK.Resource.Detectors
  ( -- * SDK resource detector
    SdkResourceDetector (..)
  , sdkResourceDetector
    -- * Process resource detector
  , ProcessResourceDetector (..)
  , processResourceDetector
    -- * Host resource detector
  , HostResourceDetector (..)
  , hostResourceDetector
    -- * OS resource detector
  , OsResourceDetector (..)
  , osResourceDetector
    -- * Environment resource detector
  , EnvironmentResourceDetector (..)
  , environmentResourceDetector
    -- * Default resource
  , defaultResource
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (getExecutablePath, getProgName, lookupEnv)
import System.Info (arch, os)

import OTel.Attribute (AttributeValue (..), lookup)
import Prelude hiding (lookup)
import OTel.SDK.Resource
  ( Resource, ResourceDetector (..), SomeResourceDetector (..)
  , create, detect, getAttributes, merge
  )


-- --------------------------------------------------------------------------
-- SDK resource detector
-- --------------------------------------------------------------------------

-- | Produces the three mandatory SDK telemetry attributes:
-- @telemetry.sdk.name@, @telemetry.sdk.version@, @telemetry.sdk.language@.
data SdkResourceDetector = SdkResourceDetector

-- | Constructor for the SDK resource detector.
sdkResourceDetector :: SdkResourceDetector
sdkResourceDetector = SdkResourceDetector

instance ResourceDetector SdkResourceDetector where
  detectResource _ = pure $ create
    [ ("telemetry.sdk.name",     StringValue "opentelemetry-haskell")
    , ("telemetry.sdk.version",  StringValue "0.1.0.0")
    , ("telemetry.sdk.language", StringValue "haskell")
    ]
    Nothing


-- --------------------------------------------------------------------------
-- Process resource detector
-- --------------------------------------------------------------------------

-- | Detects process attributes available from @base@ without the @unix@
-- package. Process PID is omitted (requires @unix@).
data ProcessResourceDetector = ProcessResourceDetector

-- | Constructor for the process resource detector.
processResourceDetector :: ProcessResourceDetector
processResourceDetector = ProcessResourceDetector

instance ResourceDetector ProcessResourceDetector where
  detectResource _ = do
    progName <- getProgName
    execPath <- getExecutablePath
    pure $ create
      [ ("process.executable.name", StringValue (Text.pack progName))
      , ("process.executable.path", StringValue (Text.pack execPath))
      ]
      Nothing


-- --------------------------------------------------------------------------
-- Host resource detector
-- --------------------------------------------------------------------------

-- | Detects host architecture via 'System.Info.arch'.
data HostResourceDetector = HostResourceDetector

-- | Constructor for the host resource detector.
hostResourceDetector :: HostResourceDetector
hostResourceDetector = HostResourceDetector

instance ResourceDetector HostResourceDetector where
  detectResource _ = pure $ create
    [ ("host.arch", StringValue (Text.pack arch)) ]
    Nothing


-- --------------------------------------------------------------------------
-- OS resource detector
-- --------------------------------------------------------------------------

-- | Detects operating system type via 'System.Info.os'.
data OsResourceDetector = OsResourceDetector

-- | Constructor for the OS resource detector.
osResourceDetector :: OsResourceDetector
osResourceDetector = OsResourceDetector

instance ResourceDetector OsResourceDetector where
  detectResource _ = pure $ create
    [ ("os.type", StringValue (Text.pack os)) ]
    Nothing


-- --------------------------------------------------------------------------
-- Environment resource detector
-- --------------------------------------------------------------------------

-- | Reads @OTEL_SERVICE_NAME@ and @OTEL_RESOURCE_ATTRIBUTES@ environment
-- variables to populate resource attributes.
data EnvironmentResourceDetector = EnvironmentResourceDetector

-- | Constructor for the environment resource detector.
environmentResourceDetector :: EnvironmentResourceDetector
environmentResourceDetector = EnvironmentResourceDetector

instance ResourceDetector EnvironmentResourceDetector where
  detectResource _ = do
    mServiceName <- lookupEnv "OTEL_SERVICE_NAME"
    mResAttrs    <- lookupEnv "OTEL_RESOURCE_ATTRIBUTES"
    let serviceNameAttrs = case mServiceName of
          Nothing -> []
          Just n  -> [("service.name", StringValue (Text.pack n))]
        envAttrs = case mResAttrs of
          Nothing  -> []
          Just raw ->
            let parsed = parseResourceAttributes (Text.pack raw)
            -- OTEL_SERVICE_NAME always wins; drop any service.name from OTEL_RESOURCE_ATTRIBUTES
            in case mServiceName of
                 Just _  -> filter ((/= "service.name") . fst) parsed
                 Nothing -> parsed
    pure $ create (serviceNameAttrs <> envAttrs) Nothing


-- | Parse @key=value,key=value@ format. Entries with no @=@ are skipped.
-- Whitespace around keys and values is trimmed.
parseResourceAttributes :: Text -> [(Text, AttributeValue)]
parseResourceAttributes raw =
  [ (Text.strip k, StringValue (Text.strip v))
  | entry <- Text.splitOn "," raw
  , let (k, rest) = Text.breakOn "=" entry
  , not (Text.null (Text.strip k))
  , not (Text.null rest)
  , let v = Text.drop 1 rest
  ]


-- --------------------------------------------------------------------------
-- Default resource
-- --------------------------------------------------------------------------

-- | Detect a resource that always includes SDK telemetry attributes.
-- Additional detectors are run after 'SdkResourceDetector'; their attributes
-- take precedence on key conflict. If no detector sets @service.name@, it
-- defaults to @unknown_service:\<progname\>@ per the OTel specification.
defaultResource :: [SomeResourceDetector] -> IO Resource
defaultResource extra = do
  r <- detect (SomeResourceDetector SdkResourceDetector : extra)
  case lookup "service.name" (getAttributes r) of
    Just _  -> pure r
    Nothing -> do
      progName <- getProgName
      let fallback = create
            [("service.name", StringValue ("unknown_service:" <> Text.pack progName))]
            Nothing
      -- Both resources have no schema URL so merge always succeeds
      pure $ case merge r fallback of
               Right merged -> merged
               Left  _      -> r
