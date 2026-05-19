{- |
= Platform.Sandbox — shell command execution environment

The 'Sandbox' type abstracts __where and how__ shell commands are run when
the agent calls the @shell@ tool.

== Two implementations

  * __'noSandbox'__: runs commands directly on the host using @sh -c@,
    cwd set to the workspace.  Used in the CLI and for trusted local
    sessions where isolation is not required.

  * __'dockerSandbox'__: starts a Docker container with the workspace
    mounted read-write, CPU/memory/network limits applied, and runs
    commands via @docker exec@.  Provides process, filesystem, and network
    isolation for multi-tenant or untrusted workloads.

== Design: record-of-functions

'Sandbox' is a record of IO actions rather than a type class.  This keeps
the agent loop free of type parameters and makes it trivial to build
test doubles.  The @runCommand@ field is what the @shell@ tool calls;
@copyIn@\/@copyOut@ and @teardown@ are lifecycle management.
-}
module CodeStar.Platform.Sandbox
  ( -- * Config
    SandboxConfig (..)
  , defaultSandboxConfig

    -- * Handle
  , Sandbox (..)

    -- * Construction
  , noSandbox
  , dockerSandbox
  ) where

import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import System.Process.Typed
  ( ExitCode (..)
  , proc
  , readProcessStdout_
  , runProcess
  , runProcess_
  , setWorkingDir
  )

-- --------------------------------------------------------------------
-- Config
-- --------------------------------------------------------------------

data SandboxConfig = SandboxConfig
  { imageTag :: !Text
  , workspaceMount :: !FilePath
  , cpuLimit :: !Text
  , memoryLimit :: !Text
  , networkDisabled :: !Bool
  }
  deriving stock (Eq, Show)

defaultSandboxConfig :: FilePath -> SandboxConfig
defaultSandboxConfig workspace =
  SandboxConfig
    { imageTag = "codestar:latest"
    , workspaceMount = workspace
    , cpuLimit = "2"
    , memoryLimit = "2g"
    , networkDisabled = True
    }

-- --------------------------------------------------------------------
-- Handle
-- --------------------------------------------------------------------

-- | A handle to a command execution environment.
data Sandbox = Sandbox
  { runCommand :: Text -> IO (Either Text Text)
  -- ^ Execute a shell command.  Returns @Right ""@ on success or
  --   @Left errorMsg@ on non-zero exit.
  , copyIn  :: FilePath -> FilePath -> IO ()
  -- ^ Copy a file from the host into the sandbox environment.
  , copyOut :: FilePath -> FilePath -> IO ()
  -- ^ Copy a file out of the sandbox to the host.
  , teardown :: IO ()
  -- ^ Tear down the sandbox (stop container, release resources).
  }

-- --------------------------------------------------------------------
-- No-op (host) sandbox
-- --------------------------------------------------------------------

-- | Create a sandbox that runs commands directly on the host.
-- @workspace@ is set as the working directory for every command.
-- @copyIn@\/@copyOut@ and @teardown@ are no-ops.
noSandbox :: FilePath -> Sandbox
noSandbox workspace =
  Sandbox
    { runCommand = runOnHost workspace
    , copyIn = \_ _ -> pure ()
    , copyOut = \_ _ -> pure ()
    , teardown = pure ()
    }

runOnHost :: FilePath -> Text -> IO (Either Text Text)
runOnHost workspace cmd = do
  let p = setWorkingDir workspace (proc "sh" ["-c", Text.unpack cmd])
  result <- runProcess p
  case result of
    ExitSuccess -> pure (Right "")
    ExitFailure n -> pure (Left ("exit " <> Text.pack (show n)))

-- --------------------------------------------------------------------
-- Docker sandbox
-- --------------------------------------------------------------------

{- | Start a Docker container with the workspace mounted and return a
Sandbox handle. Commands run via 'docker exec'; teardown stops the
container. Returns Left on Docker failure.
-}
dockerSandbox :: SandboxConfig -> IO (Either Text Sandbox)
dockerSandbox cfg = do
  r <- startContainer cfg
  case r of
    Left err -> pure (Left err)
    Right containerId ->
      pure $
        Right
          Sandbox
            { runCommand = dockerExec containerId
            , copyIn = dockerCopyIn containerId
            , copyOut = dockerCopyOut containerId
            , teardown = dockerTeardown containerId
            }

startContainer :: SandboxConfig -> IO (Either Text Text)
startContainer cfg = do
  let args =
        concat
          [ ["run", "-d", "--rm"]
          , ["--cpus", Text.unpack cfg.cpuLimit]
          , ["--memory", Text.unpack cfg.memoryLimit]
          , if cfg.networkDisabled then ["--network", "none"] else []
          , ["-v", cfg.workspaceMount <> ":/workspace:rw"]
          , ["-w", "/workspace"]
          , [Text.unpack cfg.imageTag, "sleep", "infinity"]
          ]
  result <- runProcess (proc "docker" args)
  case result of
    ExitFailure n -> pure (Left ("docker run: exit " <> Text.pack (show n)))
    ExitSuccess -> do
      bs <- readProcessStdout_ (proc "docker" ["ps", "-lq"])
      pure (Right (Text.strip (decodeBS bs)))

dockerExec :: Text -> Text -> IO (Either Text Text)
dockerExec cid cmd = do
  result <- runProcess (proc "docker" ["exec", Text.unpack cid, "sh", "-c", Text.unpack cmd])
  case result of
    ExitSuccess -> pure (Right "")
    ExitFailure n -> pure (Left ("docker exec: exit " <> Text.pack (show n)))

dockerCopyIn :: Text -> FilePath -> FilePath -> IO ()
dockerCopyIn cid src dst =
  runProcess_ (proc "docker" ["cp", src, Text.unpack cid <> ":" <> dst])

dockerCopyOut :: Text -> FilePath -> FilePath -> IO ()
dockerCopyOut cid src dst =
  runProcess_ (proc "docker" ["cp", Text.unpack cid <> ":" <> src, dst])

dockerTeardown :: Text -> IO ()
dockerTeardown cid =
  runProcess_ (proc "docker" ["stop", Text.unpack cid])

decodeBS :: BL.ByteString -> Text
decodeBS = TE.decodeUtf8 . BL.toStrict
