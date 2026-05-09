module Network.MCP.Server.ToolsSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Network.MCP.Server.Tools
import Network.MCP.Types (Cursor (..))

------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------

validChars :: [Char]
validChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_."

newtype ValidName = ValidName T.Text deriving (Show)

instance Arbitrary ValidName where
  arbitrary = do
    len  <- choose (1, 128)
    cs   <- vectorOf len (elements validChars)
    pure (ValidName (T.pack cs))

newtype TooLongName = TooLongName T.Text deriving (Show)

instance Arbitrary TooLongName where
  arbitrary = do
    len <- choose (129, 200)
    cs  <- vectorOf len (elements validChars)
    pure (TooLongName (T.pack cs))

newtype NameWithBadChar = NameWithBadChar T.Text deriving (Show)

instance Arbitrary NameWithBadChar where
  arbitrary = do
    -- One forbidden character in a short name
    badChar <- elements " !@#$%^&*()+=[]{};:',<>?\"\\|`~"
    prefix  <- resize 10 (listOf (elements validChars))
    suffix  <- resize 10 (listOf (elements validChars))
    pos     <- choose (0, length prefix)
    let cs = take pos prefix ++ [badChar] ++ suffix
    pure (NameWithBadChar (T.pack cs))

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = do
  -- ── isValidName ───────────────────────────────────────────────────────
  describe "isValidName" $ do
    prop "valid names (1-128 chars from [A-Za-z0-9_-.]) are accepted" $
      \(ValidName n) -> isValidName n === True

    prop "names longer than 128 chars are rejected" $
      \(TooLongName n) -> isValidName n === False

    it "empty name is rejected" $
      isValidName "" `shouldBe` False

    prop "names with forbidden characters are rejected" $
      \(NameWithBadChar n) -> not (T.null n) ==> isValidName n === False

    it "name of exactly 128 valid chars is accepted" $
      isValidName (T.replicate 128 "a") `shouldBe` True

    it "name of exactly 129 valid chars is rejected" $
      isValidName (T.replicate 129 "a") `shouldBe` False

  -- ── encodeCursor / decodeCursor roundtrip ─────────────────────────────
  describe "cursor roundtrip" $ do
    prop "decodeCursor (encodeCursor n) == Just n for non-negative n" $
      \(NonNegative n) ->
        decodeCursor (encodeCursor (n :: Int)) === Just n

    it "decodeCursor returns Nothing for non-base64 text" $
      decodeCursor (Cursor "!!! not base64 !!!") `shouldBe` Nothing

    it "decodeCursor returns Nothing for base64-valid but non-numeric payload" $ do
      -- base64 of "hello"
      decodeCursor (Cursor "aGVsbG8=") `shouldBe` Nothing

    it "decodeCursor returns Nothing for empty cursor" $
      decodeCursor (Cursor "") `shouldBe` Nothing

  -- ── paginate correctness ──────────────────────────────────────────────
  describe "paginate" $ do
    prop "first page never exceeds page size (50)" $
      \(items :: [Int]) ->
        let (page, _) = paginate items Nothing
        in length page <= 50

    prop "collecting all pages recovers the full list" $
      \(items :: [Int]) ->
        collectAllPages items === items

    prop "last page has no next cursor" $
      \(items :: [Int]) ->
        -- A list of at most 50 items fits in one page, so no next cursor.
        length items <= 50 ==>
          let (_, cur) = paginate items Nothing
          in cur === Nothing

    prop "cursor chain is consistent: second page picks up where first ended" $
      \(items :: [Int]) ->
        let (page1, mcur) = paginate items Nothing
        in case mcur of
          Nothing  -> property True  -- only one page, nothing further to check
          Just cur ->
            let (page2, _) = paginate items (Just cur)
            in page1 ++ page2 === take (50 + length page2) items

    it "empty list produces empty page and no cursor" $
      paginate ([] :: [Int]) Nothing `shouldBe` ([], Nothing)

    it "exactly 50 items: full page, no cursor" $
      let items = [1 .. 50 :: Int]
          (page, cur) = paginate items Nothing
      in do
        page `shouldBe` items
        cur  `shouldBe` Nothing

    it "51 items: first page has 50, second has 1" $ do
      let items = [1 .. 51 :: Int]
          (p1, mc1) = paginate items Nothing
      length p1 `shouldBe` 50
      case mc1 of
        Nothing -> expectationFailure "expected a cursor after 51 items"
        Just c1 -> do
          let (p2, c2) = paginate items (Just c1)
          p2 `shouldBe` [51]
          c2 `shouldBe` Nothing

------------------------------------------------------------------------
-- Helper
------------------------------------------------------------------------

collectAllPages :: [a] -> [a]
collectAllPages items = go Nothing []
  where
    go cursor acc =
      let (page, next) = paginate items cursor
      in case next of
        Nothing  -> acc ++ page
        Just cur -> go (Just cur) (acc ++ page)
