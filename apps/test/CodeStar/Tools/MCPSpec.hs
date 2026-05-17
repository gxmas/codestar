module CodeStar.Tools.MCPSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import CodeStar.Config (McpEndpoint (..), McpTransport (..))
import CodeStar.Telemetry (noOpRecorder)
import CodeStar.Tools.MCP (connectMcpEndpoints)

spec :: Spec
spec = describe "CodeStar.Tools.MCP" $ do
  it "returns empty list for empty endpoint config" $ do
    handlers <- connectMcpEndpoints noOpRecorder []
    length handlers `shouldBe` 0

  it "skips endpoints that fail to connect" $ do
    let badEp =
          McpEndpoint
            { endpointName = "bad"
            , command = "definitely-not-a-real-command-xyz"
            , args = []
            , env = Map.empty
            , transport = StdioTransport
            , auth = Nothing
            }
    handlers <- connectMcpEndpoints noOpRecorder [badEp]
    length handlers `shouldBe` 0

  it "fails gracefully for unreachable HTTP transport endpoints" $ do
    let badHttp =
          McpEndpoint
            { endpointName = "http-bad"
            , command = "http://127.0.0.1:1"
            , args = []
            , env = Map.empty
            , transport = HttpTransport
            , auth = Nothing
            }
    handlers <- connectMcpEndpoints noOpRecorder [badHttp]
    length handlers `shouldBe` 0
