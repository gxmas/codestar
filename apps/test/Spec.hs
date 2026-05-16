module Main where

import Test.Hspec

import CodeStar.AgentTelemetrySpec qualified
import CodeStar.AgentLoopSpec qualified
import CodeStar.CompactionSpec qualified
import CodeStar.Config.DefaultsSpec qualified
import CodeStar.Config.EnvSpec qualified
import CodeStar.Config.JsonSpec qualified
import CodeStar.Config.LoadSpec qualified
import CodeStar.Config.MigrateSpec qualified
import CodeStar.Config.PartialSpec qualified
import CodeStar.Config.PathsSpec qualified
import CodeStar.Config.TomlSpec qualified
import CodeStar.Config.TypesSpec qualified
import CodeStar.Config.ValidateSpec qualified
import CodeStar.ConfigSpec qualified
import CodeStar.ContextSpec qualified
import CodeStar.GuardrailsSpec qualified
import CodeStar.HistorySpec qualified
import CodeStar.LlmRetrySpec qualified
import CodeStar.LlmRetryFieldSpec qualified
import CodeStar.LLM.AnthropicSpec qualified
import CodeStar.LLM.BaseSpec qualified
import CodeStar.LLM.OllamaSpec qualified
import CodeStar.LLM.OpenAISpec qualified
import CodeStar.LocalizationSpec qualified
import CodeStar.MemorySpec qualified
import CodeStar.PermissionsSpec qualified
import CodeStar.PlanExecutionSpec qualified
import CodeStar.PlanTelemetrySpec qualified
import CodeStar.PlanTelemetryExtraSpec qualified
import CodeStar.PlanningSpec qualified
import CodeStar.Platform.AuthSpec qualified
import CodeStar.Platform.CostTrackerSpec qualified
import CodeStar.Platform.SandboxSpec qualified
import CodeStar.Platform.SessionManagerSpec qualified
import CodeStar.RepoMap.CacheGcSpec qualified
import CodeStar.RepoMap.CacheSpec qualified
import CodeStar.RepoMap.Graph.Extract.HaskellSpec qualified
import CodeStar.RepoMap.Graph.Extract.PythonSpec qualified
import CodeStar.RepoMap.Graph.Extract.TypeScriptSpec qualified
import CodeStar.RepoMap.Graph.Extract.TypesSpec qualified
import CodeStar.RepoMap.Graph.ExtractSpec qualified
import CodeStar.RepoMap.GraphSpec qualified
import CodeStar.RepoMap.RenderSpec qualified
import CodeStar.RepoMap.WorkerSpec qualified
import CodeStar.SamplingSpec qualified
import CodeStar.SemanticSearchSpec qualified
import CodeStar.SignalSpec qualified
import CodeStar.SpanAttributeSpec qualified
import CodeStar.StorageSpec qualified
import CodeStar.ServerSpanSafetySpec qualified
import CodeStar.SessionLifecycleSpec qualified
import CodeStar.TelemetrySpec qualified
import CodeStar.TelemetrySpanSpec qualified
import OTel.ContextThreadSpec qualified
import CodeStar.Tools.EditSpec qualified
import CodeStar.Tools.GitSpec qualified
import CodeStar.Tools.GlobSpec qualified
import CodeStar.Tools.GrepSpec qualified
import CodeStar.Tools.MCPSpec qualified
import CodeStar.Tools.ReadSpec qualified
import CodeStar.Tools.RegistrySpec qualified
import CodeStar.Tools.ShellSpec qualified
import CodeStar.Tools.TestsSpec qualified
import CodeStar.Tools.TodoListSpec qualified
import CodeStar.Tools.WriteSpec qualified
import CodeStar.Transport.JsonRpcSpec qualified
import CodeStar.Transport.TypesSpec qualified
import CodeStar.Transport.WebSocketSpec qualified
import CodeStar.TreeSitter.GrammarsSpec qualified
import CodeStar.TreeSitterSpec qualified
import CodeStar.TypesSpec qualified
import CodeStar.VerificationSpec qualified

main :: IO ()
main = hspec $ do
  CodeStar.AgentTelemetrySpec.spec
  CodeStar.AgentLoopSpec.spec
  CodeStar.CompactionSpec.spec
  CodeStar.Config.DefaultsSpec.spec
  CodeStar.Config.EnvSpec.spec
  CodeStar.Config.JsonSpec.spec
  CodeStar.Config.LoadSpec.spec
  CodeStar.Config.MigrateSpec.spec
  CodeStar.Config.PartialSpec.spec
  CodeStar.Config.PathsSpec.spec
  CodeStar.Config.TomlSpec.spec
  CodeStar.Config.TypesSpec.spec
  CodeStar.Config.ValidateSpec.spec
  CodeStar.ConfigSpec.spec
  CodeStar.ContextSpec.spec
  CodeStar.GuardrailsSpec.spec
  CodeStar.HistorySpec.spec
  CodeStar.LlmRetrySpec.spec
  CodeStar.LlmRetryFieldSpec.spec
  CodeStar.LLM.AnthropicSpec.spec
  CodeStar.LLM.BaseSpec.spec
  CodeStar.LLM.OllamaSpec.spec
  CodeStar.LLM.OpenAISpec.spec
  CodeStar.LocalizationSpec.spec
  CodeStar.MemorySpec.spec
  CodeStar.PermissionsSpec.spec
  CodeStar.PlanExecutionSpec.spec
  CodeStar.PlanTelemetrySpec.spec
  CodeStar.PlanTelemetryExtraSpec.spec
  CodeStar.PlanningSpec.spec
  CodeStar.Platform.AuthSpec.spec
  CodeStar.Platform.CostTrackerSpec.spec
  CodeStar.Platform.SandboxSpec.spec
  CodeStar.Platform.SessionManagerSpec.spec
  CodeStar.RepoMap.CacheGcSpec.spec
  CodeStar.RepoMap.CacheSpec.spec
  CodeStar.RepoMap.Graph.Extract.HaskellSpec.spec
  CodeStar.RepoMap.Graph.Extract.PythonSpec.spec
  CodeStar.RepoMap.Graph.Extract.TypeScriptSpec.spec
  CodeStar.RepoMap.Graph.Extract.TypesSpec.spec
  CodeStar.RepoMap.Graph.ExtractSpec.spec
  CodeStar.RepoMap.GraphSpec.spec
  CodeStar.RepoMap.RenderSpec.spec
  CodeStar.RepoMap.WorkerSpec.spec
  CodeStar.SamplingSpec.spec
  CodeStar.SemanticSearchSpec.spec
  CodeStar.SignalSpec.spec
  CodeStar.SpanAttributeSpec.spec
  CodeStar.StorageSpec.spec
  CodeStar.ServerSpanSafetySpec.spec
  CodeStar.SessionLifecycleSpec.spec
  CodeStar.TelemetrySpec.spec
  CodeStar.TelemetrySpanSpec.spec
  OTel.ContextThreadSpec.spec
  CodeStar.Tools.EditSpec.spec
  CodeStar.Tools.GitSpec.spec
  CodeStar.Tools.GlobSpec.spec
  CodeStar.Tools.GrepSpec.spec
  CodeStar.Tools.MCPSpec.spec
  CodeStar.Tools.ReadSpec.spec
  CodeStar.Tools.RegistrySpec.spec
  CodeStar.Tools.ShellSpec.spec
  CodeStar.Tools.TestsSpec.spec
  CodeStar.Tools.TodoListSpec.spec
  CodeStar.Tools.WriteSpec.spec
  CodeStar.Transport.JsonRpcSpec.spec
  CodeStar.Transport.TypesSpec.spec
  CodeStar.Transport.WebSocketSpec.spec
  CodeStar.TreeSitter.GrammarsSpec.spec
  CodeStar.TreeSitterSpec.spec
  CodeStar.TypesSpec.spec
  CodeStar.VerificationSpec.spec
