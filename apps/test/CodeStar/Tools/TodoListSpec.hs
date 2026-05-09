module CodeStar.Tools.TodoListSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Test.Hspec

import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Tools.Registry
  ( ToolError (..)
  , ToolDefinition (..)
  , ToolHandlerDict (..)
  , ToolInput (..)
  , ToolOutput (..)
  )
import CodeStar.Tools.TodoList (newTodoStore, todoListHandlers)

spec :: Spec
spec = describe "CodeStar.Tools.TodoList" $ do
  it "registers read and write handlers" $ do
    store <- newTodoStore
    let hs = todoListHandlers store
        names = map (unToolName . (.name) . definition) hs
    names `shouldContain` ["todo_write", "todo_read"]

  it "writes JSON todos then reads them back" $ do
    store <- newTodoStore
    let writeH = requireHandler "todo_write" (todoListHandlers store)
        readH = requireHandler "todo_read" (todoListHandlers store)
        payload = "[{\"id\":\"t1\",\"description\":\"first\",\"status\":\"pending\"}]"
    writeRes <- invoke writeH (mkInput [("items", Aeson.String payload)])
    writeRes `shouldSatisfy` isRightResult
    readRes <- invoke readH (mkInput [])
    case readRes of
      Left err -> expectationFailure ("Expected Right ToolOutput, got: " <> show err)
      Right out ->
        Aeson.eitherDecodeStrict' (TE.encodeUtf8 out.content) `shouldSatisfy` isRightDecode

  it "reads empty list before any writes" $ do
    store <- newTodoStore
    let readH = requireHandler "todo_read" (todoListHandlers store)
    readRes <- invoke readH (mkInput [])
    readRes `shouldBe` Right (ToolOutput "[]" False)

  it "rejects invalid todo JSON payloads" $ do
    store <- newTodoStore
    let writeH = requireHandler "todo_write" (todoListHandlers store)
    res <- invoke writeH (mkInput [("items", Aeson.String "{invalid json")])
    res `shouldSatisfy` isInvalidInput

  it "rejects unknown todo status values" $ do
    store <- newTodoStore
    let writeH = requireHandler "todo_write" (todoListHandlers store)
        badStatus = "[{\"id\":\"t1\",\"description\":\"first\",\"status\":\"stuck\"}]"
    res <- invoke writeH (mkInput [("items", Aeson.String badStatus)])
    res `shouldSatisfy` isInvalidInput

mkInput :: [(Text.Text, Aeson.Value)] -> ToolInput
mkInput entries = ToolInput{arguments = Map.fromList entries}

requireHandler :: Text.Text -> [ToolHandlerDict] -> ToolHandlerDict
requireHandler nm hs =
  case filter ((== ToolName nm) . (.name) . definition) hs of
    h : _ -> h
    [] -> error ("Missing handler: " <> Text.unpack nm)

isRightResult :: Either ToolError a -> Bool
isRightResult (Right _) = True
isRightResult _ = False

isInvalidInput :: Either ToolError a -> Bool
isInvalidInput (Left (InvalidInput _)) = True
isInvalidInput _ = False

isRightDecode :: Either String [Aeson.Value] -> Bool
isRightDecode (Right vals) = not (null vals)
isRightDecode _ = False
