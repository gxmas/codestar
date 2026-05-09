module CodeStar.Tools.MCPSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import CodeStar.Config (McpEndpoint (..), McpTransport (..))
import CodeStar.Tools.MCP (connectMcpEndpoints)

spec :: Spec
spec = describe "CodeStar.Tools.MCP" $ do
  it "returns empty list for empty endpoint config" $ do
    handlers <- connectMcpEndpoints []
    length handlers `shouldBe` 0

  it "skips endpoints that fail to connect" $ do
    let badEp =
          McpEndpoint
            { endpointName = "bad"
            , command = "definitely-not-a-real-command-xyz"
            , args = []
            , env = Map.empty
            , transport = StdioTransport
            }
    handlers <- connectMcpEndpoints [badEp]
    length handlers `shouldBe` 0

  it "fails gracefully for unreachable HTTP transport endpoints" $ do
    let badHttp =
          McpEndpoint
            { endpointName = "http-bad"
            , command = "http://127.0.0.1:1"
            , args = []
            , env = Map.empty
            , transport = HttpTransport
            }
    handlers <- connectMcpEndpoints [badHttp]
    length handlers `shouldBe` 0
