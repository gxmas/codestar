module CodeStar.Tools.Shell
  ( shellToolHandler
  ) where

import Control.Concurrent.Async (async)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (SomeException, bracket_, try)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.JsonSchema
  ( booleanSchema
  , integerSchema
  , objectSchema
  , optional
  , required
  , stringSchema
  , withDescription
  , withMinimum
  )
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Platform.Sandbox (Sandbox (..))
import CodeStar.Tools.Registry

-- Process spawning bulkhead: limit concurrent subprocesses
maxConcurrent :: Int
maxConcurrent = 64

{-# NOINLINE processSem #-}
processSem :: QSem
processSem = unsafePerformIO (newQSem maxConcurrent)

maxOutputChars :: Int
maxOutputChars = 50000

shellToolHandler :: Sandbox -> ToolHandlerDict
shellToolHandler sandbox =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName "shell"
          , description =
              "Execute a shell command. Output is truncated at "
                <> Text.pack (show maxOutputChars)
                <> " characters. "
                <> "Use background=true for long-running commands."
          , parameters =
              objectSchema
                [ required "command" (stringSchema & withDescription "Shell command to execute")
                , optional "timeout_ms" (integerSchema & withDescription "Timeout in milliseconds (default: 30000)" & withMinimum 1)
                , optional "background" (booleanSchema & withDescription "Run in background, return immediately (default: false)")
                ]
          , riskTier = SideEffect
          }
    , invoke = invokeShell sandbox
    }

invokeShell :: Sandbox -> ToolInput -> IO (Either ToolError ToolOutput)
invokeShell sandbox input =
  case parseShellInput input of
    Left err -> pure (Left err)
    Right (cmd, timeoutMs, background) ->
      if background
        then runBackground sandbox cmd
        else runForeground sandbox cmd timeoutMs

parseShellInput :: ToolInput -> Either ToolError (Text, Int, Bool)
parseShellInput input = do
  cmd <- extractText "command" input
  ms <- case Map.lookup "timeout_ms" input.arguments of
    Nothing -> Right 30000
    Just _ -> do
      maybeMs <- extractInt "timeout_ms" input
      let n = maybe 30000 id maybeMs
      if n < 1
        then Left (InvalidInput "timeout_ms: must be >= 1")
        else Right n
  bg <- extractBool "background" input
  pure (cmd, ms, bg)

runForeground :: Sandbox -> Text -> Int -> IO (Either ToolError ToolOutput)
runForeground sandbox cmd timeoutMs = do
  result <- try @SomeException $ timeout (timeoutMs * 1000) (withBulkhead $ sandbox.runCommand cmd)
  pure $ case result of
    Left ex -> Left (ExecutionFailed (Text.pack (show ex)))
    Right Nothing -> Left (CodeStar.Tools.Registry.Timeout)
    Right (Just (Left err)) -> Left (ExecutionFailed err)
    Right (Just (Right output)) ->
      let truncated = Text.length output > maxOutputChars
          content =
            Text.take maxOutputChars output
              <> if truncated then "\n[Output truncated]" else ""
       in Right ToolOutput{content, truncated}

runBackground :: Sandbox -> Text -> IO (Either ToolError ToolOutput)
runBackground sandbox cmd = do
  _ <- async $ withBulkhead $ sandbox.runCommand cmd
  pure $
    Right
      ToolOutput
        { content = "Command started in background: " <> cmd
        , truncated = False
        }

withBulkhead :: IO a -> IO a
withBulkhead action =
  bracket_
    (waitQSem processSem)
    (signalQSem processSem)
    action
