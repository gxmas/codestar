module CodeStar.Tools.EditSpec (spec) where

import Control.Exception (IOException, catch)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Aeson (Value (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)

import CodeStar.Tools.Edit (checkReplacement, editToolHandler, replaceFirst)
import CodeStar.Tools.Read (ReadTracker, newReadTracker, readToolHandler)
import CodeStar.Tools.Registry

-- --------------------------------------------------------------------
-- Generators
-- --------------------------------------------------------------------

arbitrarySmallText :: Gen Text
arbitrarySmallText = Text.pack <$> listOf (elements ['a' .. 'd'])

genPresentPair :: Gen (Text, Text)
genPresentPair = do
  prefix <- arbitrarySmallText
  needle <- arbitrarySmallText `suchThat` (not . Text.null)
  suffix <- arbitrarySmallText
  pure (prefix <> needle <> suffix, needle)

genAbsentPair :: Gen (Text, Text)
genAbsentPair = do
  haystack <- arbitrarySmallText
  needle <-
    (Text.pack <$> listOf1 (elements ['x' .. 'z']))
      `suchThat` (\n -> not (Text.isInfixOf n haystack))
  pure (haystack, needle)

genUniquePair :: Gen (Text, Text)
genUniquePair = do
  prefix <- Text.pack <$> listOf (elements ['a' .. 'c'])
  needle <- Text.pack <$> listOf1 (elements ['x' .. 'z'])
  suffix <- Text.pack <$> listOf (elements ['a' .. 'c'])
  pure (prefix <> needle <> suffix, needle)

-- Generate (content, old, new) where old appears exactly once.
-- Disjoint alphabets guarantee uniqueness: old uses ['x'..'z'],
-- surrounding content uses ['a'..'d'].
genUniqueEdit :: Gen (Text, Text, Text)
genUniqueEdit = do
  prefix <- Text.pack <$> listOf  (elements ['a' .. 'd'])
  old    <- Text.pack <$> listOf1 (elements ['x' .. 'z'])
  suffix <- Text.pack <$> listOf  (elements ['a' .. 'd'])
  new    <- arbitrarySmallText
  pure (prefix <> old <> suffix, old, new)

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = do
  describe "replaceFirst" $ do
    it "classifies occurrence-count buckets for generated inputs" $
      property $
        forAll (Text.singleton <$> elements ['a' .. 'd']) $ \needle ->
          forAll arbitrarySmallText $ \content ->
            let occ = Text.count needle content
             in checkCoverage $
                  classify (occ == 0) "0 occurrences" $
                    classify (occ == 1) "1 occurrence" $
                      classify (occ > 1) "many occurrences" $
                        cover 10 (occ == 0) "zero occurrence cases" $
                          cover 8 (occ == 1) "single occurrence cases" $
                            cover 10 (occ > 1) "many occurrence cases" $
                              property True

    it "returns content unchanged when old is absent" $
      property $
        forAll genAbsentPair $ \(content, old) ->
          replaceFirst old "REPLACEMENT" content === content

    it "replaces old with itself as identity" $
      property $
        forAll genPresentPair $ \(content, old) ->
          replaceFirst old old content === content

    it "result contains the replacement text when old is present" $
      property $
        forAll genPresentPair $ \(content, old) ->
          forAll arbitrarySmallText $ \new ->
            not (Text.null new) ==>
              Text.isInfixOf new (replaceFirst old new content)

    it "agrees with Text.replace when old occurs exactly once" $
      property $
        forAll genUniquePair $ \(content, old) ->
          forAll arbitrarySmallText $ \new ->
            Text.count old content == 1 ==>
              replaceFirst old new content === Text.replace old new content

    it "replaces only the first occurrence" $
      property $
        forAll (arbitrarySmallText `suchThat` (not . Text.null)) $ \needle ->
          forAll arbitrarySmallText $ \content ->
            let result = replaceFirst needle "" content
                (prefix, match) = Text.breakOn needle content
                expected =
                  if Text.null match
                    then content
                    else prefix <> Text.drop (Text.length needle) match
             in counterexample ("content=" <> show content <> " result=" <> show result) $
                  result === expected

    it "never crashes when old is non-empty" $
      property $
        forAll (arbitrarySmallText `suchThat` (not . Text.null)) $ \old ->
          forAll arbitrarySmallText $ \new ->
            forAll arbitrarySmallText $ \content ->
              replaceFirst old new content `seq` True

    it "documents empty-old behavior: no-op to avoid partial Text APIs" $
      property $
        forAll arbitrarySmallText $ \content ->
          forAll arbitrarySmallText $ \new ->
            replaceFirst "" new content === content

    it "old longer than haystack leaves content unchanged" $
      property $
        forAll arbitrarySmallText $ \content ->
          forAll (arbitrarySmallText `suchThat` (\old -> Text.length old > Text.length content)) $ \old ->
            replaceFirst old "X" content === content

    it "handles overlapping occurrences by replacing first match only" $
      replaceFirst "aa" "X" "aaaa" `shouldBe` "Xaa"

  describe "checkReplacement" $ do
    it "returns Left when old is absent from content" $
      property $
        forAll genAbsentPair $ \(content, old) ->
          forAll arbitrary $ \replAll ->
            isLeft (checkReplacement old replAll content)

    it "returns Right when old appears exactly once" $
      property $
        forAll genUniquePair $ \(content, old) ->
          isRight (checkReplacement old False content)

    it "returns Left for duplicates when replaceAll is False" $
      property $
        forAll arbitrarySmallText $ \prefix ->
          forAll (arbitrarySmallText `suchThat` (not . Text.null)) $ \needle ->
            forAll arbitrarySmallText $ \middle ->
              forAll arbitrarySmallText $ \suffix ->
                let content = prefix <> needle <> middle <> needle <> suffix
                 in Text.count needle content > 1 ==>
                      isLeft (checkReplacement needle False content)

    it "returns Right for duplicates when replaceAll is True" $
      property $
        forAll genPresentPair $ \(content, old) ->
          Text.count old content >= 1 ==>
            isRight (checkReplacement old True content)

    it "documents empty-old validation behavior" $ do
      checkReplacement "" False "abc" `shouldSatisfy` isLeft
      checkReplacement "" True "abc" `shouldSatisfy` isLeft

  describe "editToolHandler integration" $ do

    -- Core unification property: for any generated file content with a
    -- unique occurrence of old, the tool handler's on-disk result matches
    -- the pure replaceFirst function exactly.
    prop "result on disk matches replaceFirst for generated unique edits" $
      forAll genUniqueEdit $ \(content, old, new) ->
        monadicIO $ do
          (actual, expected) <- run $ withEditEnv $ \fp _tracker handler readHandler -> do
            Text.IO.writeFile fp content
            _ <- readHandler.invoke (mkInput [("path", String (Text.pack fp))])
            editResult <- handler.invoke
              (mkInput [ ("path", String (Text.pack fp))
                       , ("old",  String old)
                       , ("new",  String new)
                       ])
            case editResult of
              Left err -> fail ("Unexpected edit failure: " <> show err)
              Right _  -> do
                actual <- Text.IO.readFile fp
                pure (actual, replaceFirst old new content)
          monitor $ counterexample $
            "content=" <> show content <> " old=" <> show old <> " new=" <> show new
            <> "\n  expected: " <> show expected
            <> "\n  actual:   " <> show actual
          assert (actual == expected)

    -- With replace_all=True the handler must agree with Text.replace.
    prop "replace_all=True result on disk matches Text.replace" $
      forAll genPresentPair $ \(content, old) ->
      forAll arbitrarySmallText $ \new ->
        monadicIO $ do
          (actual, expected) <- run $ withEditEnv $ \fp _tracker handler readHandler -> do
            Text.IO.writeFile fp content
            _ <- readHandler.invoke (mkInput [("path", String (Text.pack fp))])
            editResult <- handler.invoke
              (mkInput [ ("path",        String (Text.pack fp))
                       , ("old",         String old)
                       , ("new",         String new)
                       , ("replace_all", Bool True)
                       ])
            case editResult of
              Left err -> fail ("Unexpected failure: " <> show err)
              Right _  -> do
                actual <- Text.IO.readFile fp
                pure (actual, Text.replace old new content)
          monitor $ counterexample $
            "content=" <> show content <> " old=" <> show old <> " new=" <> show new
          assert (actual == expected)

    -- The unit-level checkReplacement guard must also hold at the handler level.
    prop "returns InvalidInput when old is absent from file content" $
      forAll genAbsentPair $ \(content, old) ->
        monadicIO $ do
          result <- run $ withEditEnv $ \fp _tracker handler readHandler -> do
            Text.IO.writeFile fp content
            _ <- readHandler.invoke (mkInput [("path", String (Text.pack fp))])
            handler.invoke
              (mkInput [ ("path", String (Text.pack fp))
                       , ("old",  String old)
                       , ("new",  String "replacement")
                       ])
          assert (isInvalidInput result)

    it "requires file to be read before editing" $
      withEditEnv $ \fp _tracker handler _readHandler -> do
        Text.IO.writeFile fp "hello world"
        res <-
          handler.invoke
            ( mkInput
                [ ("path", String (Text.pack fp))
                , ("old", String "hello")
                , ("new", String "hi")
                ]
            )
        res `shouldSatisfy` isInvalidInput

    it "malformed inputs return InvalidInput and do not crash" $
      withEditEnv $ \fp _tracker handler _readHandler -> do
        Text.IO.writeFile fp "hello world"
        let missingOld =
              mkInput
                [ ("path", String (Text.pack fp))
                , ("new", String "x")
                ]
            wrongPathType =
              mkInput
                [ ("path", Number 123)
                , ("old", String "hello")
                , ("new", String "x")
                ]
        res1 <- handler.invoke missingOld
        res2 <- handler.invoke wrongPathType
        res1 `shouldSatisfy` isInvalidInput
        res2 `shouldSatisfy` isInvalidInput

    it "successful edits return stable output format" $
      withEditEnv $ \fp _tracker handler readHandler -> do
        Text.IO.writeFile fp "hello world"
        _ <- readHandler.invoke (mkInput [("path", String (Text.pack fp))])
        result <-
          handler.invoke
            ( mkInput
                [ ("path", String (Text.pack fp))
                , ("old", String "hello")
                , ("new", String "hi")
                ]
            )
        case result of
          Right out -> do
            out.truncated `shouldBe` False
            out.content `shouldBe` "Edit applied."
          Left err ->
            expectationFailure ("Expected Right ToolOutput, got: " <> show err)

    it "documents non-idempotence for one-shot edits" $
      withEditEnv $ \fp _tracker handler readHandler -> do
        Text.IO.writeFile fp "abc"
        _ <- readHandler.invoke (mkInput [("path", String (Text.pack fp))])
        first <-
          handler.invoke
            ( mkInput
                [ ("path", String (Text.pack fp))
                , ("old", String "a")
                , ("new", String "x")
                ]
            )
        second <-
          handler.invoke
            ( mkInput
                [ ("path", String (Text.pack fp))
                , ("old", String "a")
                , ("new", String "x")
                ]
            )
        first `shouldSatisfy` isRightResult
        second `shouldSatisfy` isInvalidInput

    it "handles edge cases: empty new, large payload, and special characters" $
      withEditEnv $ \fp _tracker handler readHandler -> do
        let maxText = Text.replicate 20000 "x"
            special = "line1\nline2\temoji-ish-ascii-:-)\nquote:\"ok\""
        Text.IO.writeFile fp ("START " <> maxText <> " MID " <> special <> " END")
        _ <- readHandler.invoke (mkInput [("path", String (Text.pack fp))])
        eraseRes <-
          handler.invoke
            ( mkInput
                [ ("path", String (Text.pack fp))
                , ("old", String "START ")
                , ("new", String "")
                ]
            )
        eraseRes `shouldSatisfy` isRightResult
        largeRes <-
          handler.invoke
            ( mkInput
                [ ("path", String (Text.pack fp))
                , ("old", String maxText)
                , ("new", String "Y")
                ]
            )
        largeRes `shouldSatisfy` isRightResult
        specialRes <-
          handler.invoke
            ( mkInput
                [ ("path", String (Text.pack fp))
                , ("old", String special)
                , ("new", String "normalized")
                ]
            )
        specialRes `shouldSatisfy` isRightResult

    it "invokes onEdit callback only after successful edit" $ do
      tmp <- getTemporaryDirectory
      let root = tmp </> "codestar-edit-spec-callback"
          fp = root </> "target.txt"
      ignoreIO (removePathForcibly root)
      createDirectoryIfMissing True root
      tracker <- newReadTracker
      calledRef <- newIORef False
      let onEdit _ = writeIORef calledRef True
          readHandler = readToolHandler tracker
          editHandler = editToolHandler tracker Nothing (Just onEdit)
      Text.IO.writeFile fp "hello world"
      -- Fails before read; callback should not fire.
      bad <-
        editHandler.invoke
          ( mkInput
              [ ("path", String (Text.pack fp))
              , ("old", String "hello")
              , ("new", String "hi")
              ]
          )
      bad `shouldSatisfy` isInvalidInput
      readIORef calledRef `shouldReturn` False
      _ <- readHandler.invoke (mkInput [("path", String (Text.pack fp))])
      good <-
        editHandler.invoke
          ( mkInput
              [ ("path", String (Text.pack fp))
              , ("old", String "hello")
              , ("new", String "hi")
              ]
          )
      good `shouldSatisfy` isRightResult
      readIORef calledRef `shouldReturn` True
      ignoreIO (removePathForcibly root)

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

mkInput :: [(Text, Value)] -> ToolInput
mkInput entries = ToolInput{arguments = Map.fromList entries}

withEditEnv ::
  (FilePath -> ReadTracker -> ToolHandlerDict -> ToolHandlerDict -> IO a) ->
  IO a
withEditEnv action = do
  tmp <- getTemporaryDirectory
  let root = tmp </> "codestar-edit-spec"
      fp = root </> "target.txt"
  ignoreIO (removePathForcibly root)
  createDirectoryIfMissing True root
  tracker <- newReadTracker
  let readHandler = readToolHandler tracker
      editHandler = editToolHandler tracker Nothing Nothing
  out <- action fp tracker editHandler readHandler
  ignoreIO (removePathForcibly root)
  pure out

isInvalidInput :: Either ToolError ToolOutput -> Bool
isInvalidInput (Left (InvalidInput _)) = True
isInvalidInput _ = False

isRightResult :: Either ToolError ToolOutput -> Bool
isRightResult (Right _) = True
isRightResult _ = False

ignoreIO :: IO () -> IO ()
ignoreIO io = io `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()
