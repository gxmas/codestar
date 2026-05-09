module CodeStar.Tools.RegistrySpec (spec) where

import Data.Aeson (Value (..))
import Data.Function ((&))
import Data.JsonSchema (objectSchema, required, stringSchema, withDescription)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Test.Hspec

import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Tools.Registry

spec :: Spec
spec = describe "CodeStar.Tools.Registry" $ do
  it "dispatches to registered handlers" $ do
    let handler = mkHandler "echo" ReadOnly "returns payload" (\_ -> pure (Right (ToolOutput "ok" False)))
        reg = register handler emptyRegistry
    dispatch reg (ToolName "echo") (ToolInput Map.empty) `shouldReturn` Right (ToolOutput "ok" False)

  it "returns ToolNotFound for unregistered tools" $ do
    let reg = emptyRegistry
    dispatch reg (ToolName "missing") (ToolInput Map.empty) `shouldReturn` Left (ToolNotFound (ToolName "missing"))

  it "listTools reflects the latest registered definition on duplicate name" $ do
    let h1 = mkHandler "dup" ReadOnly "old" (\_ -> pure (Right (ToolOutput "old" False)))
        h2 = mkHandler "dup" LocalWrite "new" (\_ -> pure (Right (ToolOutput "new" False)))
        reg = register h2 (register h1 emptyRegistry)
        defs = listTools reg
    length defs `shouldBe` 1
    map riskTier defs `shouldBe` [LocalWrite]
    map description defs `shouldBe` ["new"]

  it "dispatches duplicate names to the latest registered handler implementation" $ do
    let h1 = mkHandler "dup" ReadOnly "old" (\_ -> pure (Right (ToolOutput "old" False)))
        h2 = mkHandler "dup" ReadOnly "new" (\_ -> pure (Right (ToolOutput "new" False)))
        reg = register h2 (register h1 emptyRegistry)
    dispatch reg (ToolName "dup") (ToolInput Map.empty) `shouldReturn` Right (ToolOutput "new" False)

  it "generateDocs includes parameter descriptions and side-effect approval note" $ do
    let sideEffectHandler =
          mkHandler
            "danger"
            SideEffect
            "dangerous operation"
            (\_ -> pure (Right (ToolOutput "done" False)))
        docs = generateDocs (register sideEffectHandler emptyRegistry)
    docs `shouldSatisfy` Text.isInfixOf "### danger"
    docs `shouldSatisfy` Text.isInfixOf "dangerous operation"
    docs `shouldSatisfy` Text.isInfixOf "payload [string (required)]"
    docs `shouldSatisfy` Text.isInfixOf "Requires explicit approval before execution."

  it "extract helpers validate and parse input fields correctly" $ do
    let good =
          ToolInput
            ( Map.fromList
                [ ("text", String "hello")
                , ("count", Number 4)
                , ("force", Bool True)
                ]
            )
        missing = ToolInput Map.empty
    extractText "text" good `shouldBe` Right "hello"
    extractInt "count" good `shouldBe` Right (Just 4)
    extractBool "force" good `shouldBe` Right True
    extractBool "force" missing `shouldBe` Right False
    extractText "text" missing `shouldSatisfy` isInvalidInput

  it "extractInt rejects non-integer numeric values" $ do
    let nonInteger = ToolInput (Map.fromList [("count", Number 1.7)])
    extractInt "count" nonInteger `shouldSatisfy` isInvalidInput

mkHandler :: Text.Text -> RiskTier -> Text.Text -> (ToolInput -> IO (Either ToolError ToolOutput)) -> ToolHandlerDict
mkHandler nm tier desc impl =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName nm
          , description = desc
          , parameters =
              objectSchema
                [ required "payload" (stringSchema & withDescription "payload")
                ]
          , riskTier = tier
          }
    , invoke = impl
    }

isInvalidInput :: Either ToolError a -> Bool
isInvalidInput (Left (InvalidInput _)) = True
isInvalidInput _ = False
