module Main where

import Control.Concurrent.Async (link, withAsync)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result (..), Value (..), eitherDecode, encode, fromJSON, object, (.=))
import Data.Aeson.KeyMap ((!?))
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Console.Haskeline
  ( InputT
  , defaultSettings
  , getInputLine
  , outputStrLn
  , runInputT
  )
import System.Exit (exitSuccess)
import System.IO (BufferMode (..), hFlush, hSetBuffering, stderr, stdout)

import Network.WebSockets qualified as WS

import CodeStar.AgentLoop (AgentEvent (..))
import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Transport.Types (AgentEventEnvelope (..))
import CodeStar.Transport.WebSocket (websocketRecv, websocketSend)
import CodeStar.Types (ControlSignal (..), SessionId (..))

-- --------------------------------------------------------------------
-- Entry point
-- --------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  let host = "localhost"
      port = 8080
      path = "/agent"
      headers = []

  Text.IO.putStrLn $
    "[codestar-client] connecting to ws://"
      <> Text.pack host
      <> ":"
      <> Text.pack (show port)
      <> Text.pack path
      <> "..."

  WS.runClientWith
    host
    port
    path
    WS.defaultConnectionOptions
    headers
    clientApp

-- --------------------------------------------------------------------
-- Client application
-- --------------------------------------------------------------------

clientApp :: WS.Connection -> IO ()
clientApp conn = do
  costRef <- newIORef (0 :: Int, 0 :: Int)
  sessionRef <- newIORef (Nothing :: Maybe SessionId)
  reqIdRef <- newIORef (1 :: Int)

  Text.IO.putStrLn "[codestar-client] connected."

  withAsync (receiverLoop conn costRef) $ \receiver -> do
    link receiver
    runInputT defaultSettings (replLoop conn costRef sessionRef reqIdRef)

-- --------------------------------------------------------------------
-- Receiver loop
-- --------------------------------------------------------------------

receiverLoop :: WS.Connection -> IORef (Int, Int) -> IO ()
receiverLoop conn costRef = go
 where
  go = do
    mMsg <- websocketRecv conn
    case mMsg of
      Nothing -> Text.IO.putStrLn "\n[connection closed]"
      Just msg -> do
        case extractEnvelope msg of
          Just envelope -> renderEvent costRef envelope.envEvent
          Nothing -> pure ()
        go

extractEnvelope :: BL.ByteString -> Maybe AgentEventEnvelope
extractEnvelope msg = case eitherDecode msg of
  Right envelope -> Just (envelope :: AgentEventEnvelope)
  Left _ -> case eitherDecode msg of
    Right (Object obj) -> case obj !? "params" of
      Just params -> case fromJSON params of
        Success envelope -> Just envelope
        _ -> Nothing
      Nothing -> Nothing
    _ -> Nothing

-- --------------------------------------------------------------------
-- REPL
-- --------------------------------------------------------------------

replLoop ::
  WS.Connection ->
  IORef (Int, Int) ->
  IORef (Maybe SessionId) ->
  IORef Int ->
  InputT IO ()
replLoop conn costRef sessionRef reqIdRef = do
  mLine <- getInputLine "\ncodestar> "
  case mLine of
    Nothing -> outputStrLn "Disconnected."
    Just line ->
      let input = Text.strip (Text.pack line)
       in if Text.null input
            then replLoop conn costRef sessionRef reqIdRef
            else handleClientInput conn costRef sessionRef reqIdRef input

handleClientInput ::
  WS.Connection ->
  IORef (Int, Int) ->
  IORef (Maybe SessionId) ->
  IORef Int ->
  Text ->
  InputT IO ()
handleClientInput conn costRef sessionRef reqIdRef input
  | Text.isPrefixOf "/" input = handleSlash conn costRef sessionRef reqIdRef input
  | otherwise = do
      mSid <- liftIO (readIORef sessionRef)
      case mSid of
        Nothing -> do
          let sid = SessionId "session-0"
          liftIO $ writeIORef sessionRef (Just sid)
          liftIO $
            sendRpcRequest
              conn
              reqIdRef
              "session.start"
              (object ["sessionId" .= sid, "task" .= input])
          outputStrLn "[Starting session...]"
        Just sid ->
          liftIO $
            sendRpcRequest
              conn
              reqIdRef
              "session.respond"
              (object ["sessionId" .= sid, "response" .= input])
      replLoop conn costRef sessionRef reqIdRef

handleSlash ::
  WS.Connection ->
  IORef (Int, Int) ->
  IORef (Maybe SessionId) ->
  IORef Int ->
  Text ->
  InputT IO ()
handleSlash conn costRef sessionRef reqIdRef cmd = case Text.words cmd of
  ["/approve"] -> do
    withSession sessionRef $ \sid ->
      liftIO $
        sendRpcRequest
          conn
          reqIdRef
          "session.approve"
          (object ["sessionId" .= sid])
    replLoop conn costRef sessionRef reqIdRef
  ("/reject" : ws) -> do
    let reason = if null ws then "user rejected" else Text.unwords ws
    withSession sessionRef $ \sid ->
      liftIO $
        sendRpcRequest
          conn
          reqIdRef
          "session.reject"
          (object ["sessionId" .= sid, "reason" .= reason])
    replLoop conn costRef sessionRef reqIdRef
  ["/stop"] -> do
    withSession sessionRef $ \sid ->
      liftIO $
        sendRpcRequest
          conn
          reqIdRef
          "session.stop"
          (object ["sessionId" .= sid])
    outputStrLn "[Session stopped]"
    liftIO $ writeIORef sessionRef Nothing
    replLoop conn costRef sessionRef reqIdRef
  ["/compact"] -> do
    withSession sessionRef $ \sid ->
      liftIO $
        sendRpcRequest
          conn
          reqIdRef
          "session.compact"
          (object ["sessionId" .= sid])
    replLoop conn costRef sessionRef reqIdRef
  ("/compact" : ws) -> do
    withSession sessionRef $ \sid ->
      liftIO $
        sendRpcRequest
          conn
          reqIdRef
          "session.compact"
          (object ["sessionId" .= sid, "instruction" .= Text.unwords ws])
    replLoop conn costRef sessionRef reqIdRef
  ["/cost"] -> do
    (inTok, outTok) <- liftIO (readIORef costRef)
    outputStrLn ("Input tokens:  " <> show inTok)
    outputStrLn ("Output tokens: " <> show outTok)
    replLoop conn costRef sessionRef reqIdRef
  ["/quit"] -> do
    outputStrLn "Bye."
    liftIO exitSuccess
  ["/help"] -> do
    outputStrLn "Commands:"
    outputStrLn "  <task>            start a session with this task"
    outputStrLn "  <text>            respond to agent input request"
    outputStrLn "  /approve          approve pending tool call"
    outputStrLn "  /reject [reason]  reject pending tool call"
    outputStrLn "  /stop             stop current session"
    outputStrLn "  /compact [instr]  compact server-side history"
    outputStrLn "  /cost             show accumulated token counts"
    outputStrLn "  /quit             disconnect and exit"
    outputStrLn "  /help             this help"
    replLoop conn costRef sessionRef reqIdRef
  _ -> do
    outputStrLn ("Unknown: " <> Text.unpack cmd)
    replLoop conn costRef sessionRef reqIdRef

withSession :: IORef (Maybe SessionId) -> (SessionId -> InputT IO ()) -> InputT IO ()
withSession ref action = do
  mSid <- liftIO (readIORef ref)
  case mSid of
    Nothing -> outputStrLn "[No active session]"
    Just sid -> action sid

-- --------------------------------------------------------------------
-- JSON-RPC request sending
-- --------------------------------------------------------------------

sendRpcRequest :: WS.Connection -> IORef Int -> Text -> Value -> IO ()
sendRpcRequest conn reqIdRef method params = do
  reqId <- atomicModifyIORef' reqIdRef (\n -> (n + 1, n))
  let request =
        object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= reqId
          , "method" .= method
          , "params" .= params
          ]
  websocketSend conn (encode request)

-- --------------------------------------------------------------------
-- Event rendering
-- --------------------------------------------------------------------

renderEvent :: IORef (Int, Int) -> AgentEvent -> IO ()
renderEvent costRef = \case
  AgentToken t ->
    Text.IO.putStr t >> hFlush stdout
  AgentToolCall (ToolName n) args ->
    Text.IO.putStrLn ("\n  -> " <> n <> " " <> Text.take 120 args)
  AgentToolResult (ToolName n) res ->
    Text.IO.putStrLn ("  <- " <> n <> ": " <> Text.take 200 res)
  AgentApprovalRequired (ToolName n) r ->
    Text.IO.putStrLn
      ( "\n[approval required] "
          <> n
          <> ": "
          <> r
          <> "\n  Type /approve or /reject [reason]"
      )
  AgentCompacting ->
    Text.IO.putStrLn "\n[compacting...]"
  AgentProgress msg ->
    Text.IO.putStrLn ("[progress] " <> msg)
  AgentCostUpdate i o ->
    atomicModifyIORef' costRef (\(ci, co) -> ((ci + i, co + o), ()))
  AgentDone signal ->
    Text.IO.putStrLn ("\n[done: " <> signalText signal <> "]")
  AgentError msg ->
    Text.IO.putStrLn ("[error] " <> msg)

signalText :: ControlSignal -> Text
signalText = \case
  Done _ -> "done"
  Continue -> "continue"
  NeedsInput q -> "needs-input: " <> q
  Blocked r -> "blocked: " <> r
