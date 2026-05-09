module CodeStar.SemanticSearchSpec (spec) where

import Test.Hspec

import CodeStar.SemanticSearch

-- SemanticSearch is an intentional stub — the module comment says:
-- "No implementation — build only after measuring where RepoMap +
--  Architect + Localization fall short in practice."
-- The tests below verify the stub types compile and that the
-- module's documented intent is preserved.  They are marked pending
-- so they show as yellow in CI rather than false-green.

spec :: Spec
spec = describe "CodeStar.SemanticSearch (stub)" $ do
  it "stub types compile and have the expected fields" $ do
    -- Verify the interface is stable even while unimplemented.
    let idx = SemanticIndex{indexFile = "/tmp/idx.bin", chunkCount = 0}
    idx.indexFile `shouldBe` "/tmp/idx.bin"
    let chunk = CodeChunk "A.hs" "x = 1" 0 1
    chunk.chunkFile `shouldBe` "A.hs"

  it "embedding-based retrieval is not yet implemented" $ do
    -- When this is implemented, replace with real retrieval tests:
    -- property tests over embedding similarity, round-trip indexing,
    -- concurrent query safety, and recall@k benchmarks.
    pending
