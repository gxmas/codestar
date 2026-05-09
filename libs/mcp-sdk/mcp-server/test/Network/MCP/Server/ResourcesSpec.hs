module Network.MCP.Server.ResourcesSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Network.MCP.Server.Resources
import Network.MCP.Types (Cursor (..))
import Network.MCP.Types.Content (URI (..))

------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------

-- Simple text safe for use as URI path segments (no braces, no spaces).
newtype SafeText = SafeText { safeText :: T.Text } deriving (Show)

instance Arbitrary SafeText where
  arbitrary = SafeText . T.pack <$>
    resize 15 (listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ ":/_.-")))

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = do
  -- ── templateSegments ──────────────────────────────────────────────────
  describe "templateSegments" $ do
    it "template with no variables yields one segment (the whole string)" $
      templateSegments "resource://exact" `shouldBe` ["resource://exact"]

    it "empty template yields no segments" $
      templateSegments "" `shouldBe` []

    it "single variable with non-empty suffix: prefix and suffix" $
      templateSegments "resource://{id}/data" `shouldBe` ["resource://", "/data"]

    -- The implementation drops trailing empty segments: a variable at the
    -- end of the template produces no suffix segment.
    it "two variables: only prefix and inter-variable literal (trailing empty dropped)" $
      templateSegments "{a}/middle/{b}" `shouldBe` ["", "/middle/"]

    it "variable at start with suffix: empty prefix and suffix segment" $
      templateSegments "{id}/rest" `shouldBe` ["", "/rest"]

    it "variable at end: only the prefix literal (trailing empty dropped)" $
      templateSegments "prefix/{id}" `shouldBe` ["prefix/"]

    -- SafeText generates non-empty text, so suffix is always non-empty,
    -- meaning no trailing empty-segment edge case fires here.
    prop "single variable with non-empty suffix: exactly [prefix, suffix]" $
      \(SafeText prefix) (SafeText varName) (SafeText suffix) ->
        let tpl  = prefix <> "{" <> varName <> "}" <> suffix
        in templateSegments tpl === [prefix, suffix]

  -- ── matchesTemplate ───────────────────────────────────────────────────
  describe "matchesTemplate" $ do
    it "exact match (no variables): URI equals template text" $
      matchesTemplate (URI "resource://exact") "resource://exact" `shouldBe` True

    it "exact mismatch (no variables): different text" $
      matchesTemplate (URI "resource://other") "resource://exact" `shouldBe` False

    it "single-variable template matches URI with non-empty variable content" $
      matchesTemplate (URI "resource://foo/data") "resource://{id}/data" `shouldBe` True

    it "single-variable template matches URI with numeric variable content" $
      matchesTemplate (URI "resource://123/data") "resource://{id}/data" `shouldBe` True

    it "single-variable template: URI missing suffix does not match" $
      matchesTemplate (URI "resource://foo") "resource://{id}/data" `shouldBe` False

    it "single-variable template: URI missing prefix does not match" $
      matchesTemplate (URI "other://foo/data") "resource://{id}/data" `shouldBe` False

    it "variable at start: any URI ending with suffix matches" $
      matchesTemplate (URI "anything/suffix") "{id}/suffix" `shouldBe` True

    -- A variable at the END of the template causes templateSegments to drop
    -- the trailing empty segment, leaving only [prefix]. The match then
    -- requires the URI to equal the prefix exactly (single-segment case).
    it "variable at end: URI equals the prefix literal (single-segment exact match)" $
      matchesTemplate (URI "prefix/") "prefix/{id}" `shouldBe` True

    it "variable at end: URI longer than prefix does not match (degenerate to exact)" $
      matchesTemplate (URI "prefix/anything") "prefix/{id}" `shouldBe` False

    it "URI too short to accommodate both prefix and suffix does not match" $
      -- prefix "ab", suffix "cd", but URI is only "abcd" (length 4 = 2+2, borderline)
      matchesTemplate (URI "abcd") "ab{x}cd" `shouldBe` True

    it "URI shorter than prefix+suffix does not match" $
      -- prefix "abc", suffix "xyz", URI "abcx" (length 4 < 3+3=6)
      matchesTemplate (URI "abcx") "abc{x}xyz" `shouldBe` False

    prop "template without variables matches only the exact URI" $
      \(SafeText t) ->
        -- A template with no { } is an exact literal — it matches the URI
        -- iff the URI text equals the template text.
        let noVars = not (T.any (== '{') t)
        in noVars ==>
          ( matchesTemplate (URI t) t === True
          .&&. matchesTemplate (URI (t <> "extra")) t === False
          )

    prop "URI built from template variable slot always matches the template" $
      \(SafeText prefix) (SafeText varContent) (SafeText suffix) ->
        not (T.null varContent) ==>
          let tpl = prefix <> "{var}" <> suffix
              uri = URI (prefix <> varContent <> suffix)
          in matchesTemplate uri tpl === True

  -- ── cursor roundtrip (Resources shares same implementation as Tools) ──
  describe "cursor roundtrip" $ do
    prop "decodeCursor (encodeCursor n) == Just n for non-negative n" $
      \(NonNegative n) ->
        decodeCursor (encodeCursor (n :: Int)) === Just n

    it "decodeCursor returns Nothing for invalid input" $
      decodeCursor (Cursor "!!!") `shouldBe` Nothing

  -- ── paginate (Resources shares same implementation as Tools) ──────────
  describe "paginate" $ do
    prop "collecting all pages recovers the full list" $
      \(items :: [Int]) ->
        collectAllPages items === items

    prop "no page exceeds 50 items" $
      \(items :: [Int]) ->
        let (page, _) = paginate items Nothing
        in length page <= 50

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
