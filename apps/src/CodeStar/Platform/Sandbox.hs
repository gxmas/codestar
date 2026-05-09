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

data Sandbox = Sandbox
  { runCommand :: Text -> IO (Either Text Text)
  , copyIn :: FilePath -> FilePath -> IO ()
  , copyOut :: FilePath -> FilePath -> IO ()
  , teardown :: IO ()
  }

-- --------------------------------------------------------------------
-- No-op (host) sandbox
-- --------------------------------------------------------------------

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
