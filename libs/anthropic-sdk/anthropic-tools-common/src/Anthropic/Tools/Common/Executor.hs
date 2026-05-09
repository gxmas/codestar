{-# LANGUAGE ScopedTypeVariables #-}
-- | Optional execution adapters for common tools.
--
-- Each executor parses the 'ToolUseBlock' input, performs IO, and
-- returns a 'ToolResultBlock'. On success the result contains
-- 'ToolResultText'; on failure it sets @isError = True@ with the
-- error message.
--
-- __No validation or safety logic is provided.__ Applications should
-- wrap these executors with their own security constraints (path
-- allowlists, command blocklists, sandboxing, etc.).
module Anthropic.Tools.Common.Executor
  ( -- * Error Type
    ExecutionError (..)

    -- * File System Executors
  , executeReadFile
  , executeWriteFile
  , executeListDirectory
  , executeSearchFiles

    -- * Shell Executor
  , executeCommand

    -- * Network Executor
  , executeFetchUrl
  ) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import System.Directory
  ( listDirectory, doesDirectoryExist, createDirectoryIfMissing
  , getDirectoryContents
  )
import System.FilePath ((</>), takeDirectory, makeRelative)
import System.Process (readCreateProcessWithExitCode, shell, CreateProcess(..))
import System.Exit (ExitCode(..))
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Data.CaseInsensitive as CI

import Anthropic.Types.Content.ToolUse (ToolUseBlock (..))
import Anthropic.Types.Content.ToolResult (ToolResultBlock (..), ToolResultContent (..))

import Anthropic.Tools.Common.Parser (ParseError (..), parseToolInput)
import Anthropic.Tools.Common.Schema

-- | Errors that can occur during tool execution.
data ExecutionError
  = ToolParseError !ParseError
    -- ^ The tool input JSON could not be parsed.
  | ToolIOError !Text
    -- ^ An IO operation failed.
  deriving stock (Eq, Show, Generic)

-- | Build a successful 'ToolResultBlock' with text content.
successResult :: Text -> ToolUseBlock -> ToolResultBlock
successResult txt tub = ToolResultBlock
  { toolUseId    = tub.id
  , content      = Just (ToolResultText txt)
  , isError      = Nothing
  , cacheControl = Nothing
  }

-- | Build an error 'ToolResultBlock'.
errorResult :: Text -> ToolUseBlock -> ToolResultBlock
errorResult msg tub = ToolResultBlock
  { toolUseId    = tub.id
  , content      = Just (ToolResultText msg)
  , isError      = Just True
  , cacheControl = Nothing
  }

-- | Run an IO action, catching 'IOException' and wrapping results.
runIO :: ToolUseBlock -> IO Text -> IO (Either ExecutionError ToolResultBlock)
runIO tub action = do
  result <- try action :: IO (Either IOException Text)
  case result of
    Right txt -> pure $ Right (successResult txt tub)
    Left err  -> pure $ Right (errorResult (T.pack (show err)) tub)

-- | Execute a file read operation.
--
-- Reads the file at the given path and returns its contents.
executeReadFile :: ToolUseBlock -> IO (Either ExecutionError ToolResultBlock)
executeReadFile tub =
  case parseToolInput tub of
    Left err -> pure $ Left (ToolParseError err)
    Right (input :: ReadFileInput) ->
      runIO tub $ TIO.readFile (T.unpack input.path)

-- | Execute a file write operation.
--
-- Writes content to the file at the given path. Optionally creates
-- parent directories.
executeWriteFile :: ToolUseBlock -> IO (Either ExecutionError ToolResultBlock)
executeWriteFile tub =
  case parseToolInput tub of
    Left err -> pure $ Left (ToolParseError err)
    Right (input :: WriteFileInput) ->
      runIO tub $ do
        case input.createDirs of
          Just True -> createDirectoryIfMissing True (takeDirectory (T.unpack input.path))
          _         -> pure ()
        TIO.writeFile (T.unpack input.path) input.content
        pure $ "Successfully wrote to " <> input.path

-- | Execute a directory listing operation.
--
-- Lists directory contents, optionally recursively.
executeListDirectory :: ToolUseBlock -> IO (Either ExecutionError ToolResultBlock)
executeListDirectory tub =
  case parseToolInput tub of
    Left err -> pure $ Left (ToolParseError err)
    Right (input :: ListDirectoryInput) ->
      runIO tub $ do
        let dir = T.unpack input.path
        entries <- case input.recursive of
          Just True -> listRecursive dir dir
          _         -> System.Directory.listDirectory dir
        pure $ T.intercalate "\n" (map T.pack entries)

-- | Recursively list directory contents.
listRecursive :: FilePath -> FilePath -> IO [FilePath]
listRecursive root dir = do
  entries <- getDirectoryContents dir
  let entries' = filter (\e -> e /= "." && e /= "..") entries
  fmap concat $ mapM (\e -> do
    let full = dir </> e
    isDir <- doesDirectoryExist full
    if isDir
      then do
        sub <- listRecursive root full
        pure (makeRelative root full : sub)
      else pure [makeRelative root full]
    ) entries'

-- | Execute a file search operation.
--
-- Searches for files matching a pattern under a root directory.
-- Simple substring matching on file names.
executeSearchFiles :: ToolUseBlock -> IO (Either ExecutionError ToolResultBlock)
executeSearchFiles tub =
  case parseToolInput tub of
    Left err -> pure $ Left (ToolParseError err)
    Right (input :: SearchFilesInput) ->
      runIO tub $ do
        let dir = T.unpack input.path
            pat = input.pattern
            caseSens = case input.caseSensitive of
              Just False -> False
              _          -> True
            maxRes = case input.maxResults of
              Just n  -> n
              Nothing -> 1000
        allFiles <- listRecursive dir dir
        let matches = take maxRes $ filter (matchFile caseSens pat) allFiles
        pure $ T.intercalate "\n" (map T.pack matches)
  where
    matchFile caseSens pat fp =
      let name = T.pack fp
      in if caseSens
         then pat `T.isInfixOf` name
         else T.toLower pat `T.isInfixOf` T.toLower name

-- | Execute a shell command.
--
-- Runs the command via the system shell and returns stdout/stderr.
executeCommand :: ToolUseBlock -> IO (Either ExecutionError ToolResultBlock)
executeCommand tub =
  case parseToolInput tub of
    Left err -> pure $ Left (ToolParseError err)
    Right (input :: ExecuteCommandInput) ->
      runIO tub $ do
        let proc = (shell (T.unpack input.command))
              { cwd = T.unpack <$> input.workingDir
              , env = fmap (map (\(k, v) -> (T.unpack k, T.unpack v)) . Map.toList) input.env
              }
        (exitCode, stdout, stderr) <- readCreateProcessWithExitCode proc ""
        let output = T.pack stdout <> if null stderr then "" else "\nSTDERR:\n" <> T.pack stderr
        pure $ case exitCode of
          ExitSuccess   -> output
          ExitFailure n -> output <> "\nExit code: " <> T.pack (show n)

-- | Execute a URL fetch operation.
--
-- Performs an HTTP request and returns the response body as text.
executeFetchUrl :: ToolUseBlock -> IO (Either ExecutionError ToolResultBlock)
executeFetchUrl tub =
  case parseToolInput tub of
    Left err -> pure $ Left (ToolParseError err)
    Right (input :: FetchUrlInput) ->
      runIO tub $ do
        manager <- HTTP.newManager TLS.tlsManagerSettings
        initReq <- HTTP.parseRequest (T.unpack input.url)
        let method' = maybe "GET" TE.encodeUtf8 input.method
            hdrs = case input.headers of
              Nothing -> []
              Just m  -> map (\(k, v) -> (CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v))
                             (Map.toList m)
            req = initReq
              { HTTP.method = method'
              , HTTP.requestHeaders = hdrs ++ HTTP.requestHeaders initReq
              }
        resp <- HTTP.httpLbs req manager
        pure $ TE.decodeUtf8 (LBS.toStrict (HTTP.responseBody resp))
