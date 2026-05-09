-- | Resource semantic conventions (semconv 1.27.0).
module OTel.SemConv.Resource
  ( -- * Service
    serviceName
  , serviceVersion
  , serviceNamespace
  , serviceInstanceId
    -- * Telemetry SDK
  , telemetrySdkName
  , telemetrySdkVersion
  , telemetrySdkLanguage
  , telemetryDistroName
  , telemetryDistroVersion
    -- * Deployment
  , deploymentEnvironmentName
    -- * Host
  , hostName
  , hostId
  , hostType
  , hostArch
  , hostImageId
  , hostImageName
  , hostImageVersion
    -- * OS
  , osType
  , osDescription
  , osName
  , osVersion
  , osBuildId
    -- * Process
  , processPid
  , processExecutableName
  , processExecutablePath
  , processCommand
  , processCommandArgs
  , processCommandLine
  , processOwner
  , processParentPid
  , processRuntimeName
  , processRuntimeVersion
  , processRuntimeDescription
    -- * Cloud
  , cloudProvider
  , cloudAccountId
  , cloudRegion
  , cloudAvailabilityZone
  , cloudPlatform
    -- * Container
  , containerName
  , containerId
  , containerRuntime
  , containerImageName
  , containerImageTag
  , containerImageId
  , containerCommandLine
    -- * Kubernetes
  , k8sClusterName
  , k8sNamespaceName
  , k8sPodName
  , k8sPodUid
  , k8sContainerName
  , k8sDeploymentName
  , k8sNodeName
  , k8sNodeUid
  ) where

import Data.Text (Text)

-- Service

-- | @service.name@
serviceName :: Text
serviceName = "service.name"

-- | @service.version@
serviceVersion :: Text
serviceVersion = "service.version"

-- | @service.namespace@
serviceNamespace :: Text
serviceNamespace = "service.namespace"

-- | @service.instance.id@
serviceInstanceId :: Text
serviceInstanceId = "service.instance.id"

-- Telemetry SDK

-- | @telemetry.sdk.name@
telemetrySdkName :: Text
telemetrySdkName = "telemetry.sdk.name"

-- | @telemetry.sdk.version@
telemetrySdkVersion :: Text
telemetrySdkVersion = "telemetry.sdk.version"

-- | @telemetry.sdk.language@
telemetrySdkLanguage :: Text
telemetrySdkLanguage = "telemetry.sdk.language"

-- | @telemetry.distro.name@
telemetryDistroName :: Text
telemetryDistroName = "telemetry.distro.name"

-- | @telemetry.distro.version@
telemetryDistroVersion :: Text
telemetryDistroVersion = "telemetry.distro.version"

-- Deployment

-- | @deployment.environment.name@
deploymentEnvironmentName :: Text
deploymentEnvironmentName = "deployment.environment.name"

-- Host

-- | @host.name@
hostName :: Text
hostName = "host.name"

-- | @host.id@
hostId :: Text
hostId = "host.id"

-- | @host.type@
hostType :: Text
hostType = "host.type"

-- | @host.arch@
hostArch :: Text
hostArch = "host.arch"

-- | @host.image.id@
hostImageId :: Text
hostImageId = "host.image.id"

-- | @host.image.name@
hostImageName :: Text
hostImageName = "host.image.name"

-- | @host.image.version@
hostImageVersion :: Text
hostImageVersion = "host.image.version"

-- OS

-- | @os.type@
osType :: Text
osType = "os.type"

-- | @os.description@
osDescription :: Text
osDescription = "os.description"

-- | @os.name@
osName :: Text
osName = "os.name"

-- | @os.version@
osVersion :: Text
osVersion = "os.version"

-- | @os.build_id@
osBuildId :: Text
osBuildId = "os.build_id"

-- Process

-- | @process.pid@
processPid :: Text
processPid = "process.pid"

-- | @process.executable.name@
processExecutableName :: Text
processExecutableName = "process.executable.name"

-- | @process.executable.path@
processExecutablePath :: Text
processExecutablePath = "process.executable.path"

-- | @process.command@
processCommand :: Text
processCommand = "process.command"

-- | @process.command_args@
processCommandArgs :: Text
processCommandArgs = "process.command_args"

-- | @process.command_line@
processCommandLine :: Text
processCommandLine = "process.command_line"

-- | @process.owner@
processOwner :: Text
processOwner = "process.owner"

-- | @process.parent_pid@
processParentPid :: Text
processParentPid = "process.parent_pid"

-- | @process.runtime.name@
processRuntimeName :: Text
processRuntimeName = "process.runtime.name"

-- | @process.runtime.version@
processRuntimeVersion :: Text
processRuntimeVersion = "process.runtime.version"

-- | @process.runtime.description@
processRuntimeDescription :: Text
processRuntimeDescription = "process.runtime.description"

-- Cloud

-- | @cloud.provider@
cloudProvider :: Text
cloudProvider = "cloud.provider"

-- | @cloud.account.id@
cloudAccountId :: Text
cloudAccountId = "cloud.account.id"

-- | @cloud.region@
cloudRegion :: Text
cloudRegion = "cloud.region"

-- | @cloud.availability_zone@
cloudAvailabilityZone :: Text
cloudAvailabilityZone = "cloud.availability_zone"

-- | @cloud.platform@
cloudPlatform :: Text
cloudPlatform = "cloud.platform"

-- Container

-- | @container.name@
containerName :: Text
containerName = "container.name"

-- | @container.id@
containerId :: Text
containerId = "container.id"

-- | @container.runtime@
containerRuntime :: Text
containerRuntime = "container.runtime"

-- | @container.image.name@
containerImageName :: Text
containerImageName = "container.image.name"

-- | @container.image.tag@
containerImageTag :: Text
containerImageTag = "container.image.tag"

-- | @container.image.id@
containerImageId :: Text
containerImageId = "container.image.id"

-- | @container.command_line@
containerCommandLine :: Text
containerCommandLine = "container.command_line"

-- Kubernetes

-- | @k8s.cluster.name@
k8sClusterName :: Text
k8sClusterName = "k8s.cluster.name"

-- | @k8s.namespace.name@
k8sNamespaceName :: Text
k8sNamespaceName = "k8s.namespace.name"

-- | @k8s.pod.name@
k8sPodName :: Text
k8sPodName = "k8s.pod.name"

-- | @k8s.pod.uid@
k8sPodUid :: Text
k8sPodUid = "k8s.pod.uid"

-- | @k8s.container.name@
k8sContainerName :: Text
k8sContainerName = "k8s.container.name"

-- | @k8s.deployment.name@
k8sDeploymentName :: Text
k8sDeploymentName = "k8s.deployment.name"

-- | @k8s.node.name@
k8sNodeName :: Text
k8sNodeName = "k8s.node.name"

-- | @k8s.node.uid@
k8sNodeUid :: Text
k8sNodeUid = "k8s.node.uid"
