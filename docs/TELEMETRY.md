# CodeStar Telemetry Reference

CodeStar exports telemetry over OpenTelemetry using three signal types:

- **Traces** — spans describing the latency and structure of every operation
- **Metrics** — counters, histograms, gauges, and up/down counters exported via Prometheus
- **Logs** — structured log records emitted for significant lifecycle events

All three signals share the same OTel resource, so `session.id` and `user.id`
carry across every signal type for a given session.

---

## Configuration

Telemetry is controlled by the `[telemetry]` section of the CodeStar config.

| Setting | Description | Default |
|---------|-------------|---------|
| `mode` | `off` \| `stderr` \| `otlp` | `off` |
| `service_name` | OTel `service.name` resource attribute | `"codestar"` |
| `endpoint` | OTLP/HTTP endpoint URL | OTEL_EXPORTER_OTLP_ENDPOINT env var |
| `metrics_enabled` | Whether to start the Prometheus HTTP server | `false` |
| `metrics_bind_host` | Prometheus listener bind host | `"0.0.0.0"` |
| `metrics_port` | Prometheus listener port | `9464` |
| `sample_rate` | Trace sample rate, 0.0–1.0. At 1.0 uses AlwaysOnSampler | `1.0` |
| `log_to_stderr` | Mirror OTLP log records to stderr | `false` |

**Sampling note:** When `sample_rate` is below 1.0, sampling is head-based
(`TraceIdRatioBasedSampler` wrapped in `ParentBasedSampler`). This means errors
can be dropped before you know they are errors. Recommended practice: keep
`sample_rate = 1.0` and apply tail-based sampling at the collector level using
the `sampling.priority` attribute set on `agent.turn` spans.

---

## Spans

Spans are exported via OTLP/HTTP. All spans share the `codestar`
instrumentation scope.

### Transport — `apps/codestar-serve/Server.hs`

#### `ws.connection`

One span per WebSocket connection, covering the full connection lifetime from
TCP accept to close.

| Attribute | Type | Description |
|-----------|------|-------------|
| `user.id` | string | Identity of the authenticated user |

#### `ws.command`

One span per JSON-RPC command received on a connection.

| Attribute | Type | Description |
|-----------|------|-------------|
| `command.type` | string | `start` \| `respond` \| `approve` \| `reject` \| `compact` \| `stop` |
| `user.id` | string | Identity of the authenticated user |

---

### Agent lifecycle — `apps/codestar-serve/Server.hs`, `apps/src/CodeStar/AgentLoop.hs`

#### `agent.turn`

Root span for the full execution of a `CmdStart` command. Parent of all
`agent.step` spans. This span runs asynchronously on the session worker thread
and inherits context from the `ws.command` span via explicit context
propagation (`getCurrent` / `attach` before the thread fork).

| Attribute | Type | When set | Description |
|-----------|------|----------|-------------|
| `session.id` | string | On start | Session identifier |
| `user.id` | string | On start | User identifier |
| `task` | string | On start | Task description, truncated to 200 characters |
| `outcome` | string | On completion | `continue` \| `needs_input` \| `blocked` \| `done` |
| `sampling.priority` | string | On `Blocked` or exception | `"1"` — hint for collector-side tail sampler to retain this trace |

Span status is set to `Error` when the agent throws an unhandled exception.

#### `agent.step`

One span per iteration of the inner agent loop (`stepAgent`). Covers one LLM
call plus all tool executions that result from it. Child of `agent.turn`.

| Attribute | Type | When set | Description |
|-----------|------|----------|-------------|
| `session.id` | string | On start | Session identifier |
| `step.number` | int64 | On start | Zero-based step index within the current turn |
| `turn.number` | int64 | On start | One-based turn index within the session |

#### `agent.compaction`

Emitted when history compaction runs inside `stepAgent`. Child of `agent.step`.

| Attribute | Type | When set | Description |
|-----------|------|----------|-------------|
| `session.id` | string | On start | Session identifier |
| `history.len_before` | int64 | On start | Number of messages before compaction |
| `history.len_after` | int64 | On completion | Number of messages after compaction |

#### `llm.call`

One span per LLM API call. Child of `agent.step`. Covers the full round-trip
including streaming.

| Attribute | Type | When set | Description |
|-----------|------|----------|-------------|
| `role` | string | On start | Model role: `"coder"` |
| `session.id` | string | On start | Session identifier |
| `user.id` | string | On start | User identifier |
| `model.id` | string | On start | Model identifier string (e.g. `claude-sonnet-4-5`) |
| `step.number` | int64 | On start | Step index within the turn |
| `turn.number` | int64 | On start | Turn index within the session |
| `error.type` | string | On LLM error | Constructor name of `LlmError`: `RateLimited` \| `AuthenticationFailed` \| `ContextTooLong` \| `ContentFiltered` \| `InvalidRequest` \| `ProviderError` \| `NetworkError` |
| `retry_after.seconds` | string | On `RateLimited` | The retry-after hint from the provider |

Span status is set to `Error` when the LLM call fails.

#### `agent.verify`

Emitted when one or more files were modified by tool calls in a step and
tree-sitter syntax verification runs. Child of `agent.step`.

| Attribute | Type | When set | Description |
|-----------|------|----------|-------------|
| `session.id` | string | On start | Session identifier |
| `files.count` | int64 | On start | Number of modified files being checked |
| `verify.outcome` | string | On completion | `passed` \| `failed` \| `partial` |

#### `mcp.tool_call`

One span per MCP tool invocation. Child of the `agent.step` span active at
call time.

| Attribute | Type | When set | Description |
|-----------|------|----------|-------------|
| `mcp.endpoint` | string | On start | MCP endpoint name as configured |
| `mcp.tool_name` | string | On start | Unqualified tool name (without endpoint prefix) |

Span status is set to `Error` when the MCP call returns an error response.

---

### Plan execution — `apps/src/CodeStar/PlanExecution.hs`

Plan spans are only emitted when using `runAgentWithList` or
`runAgentWithPlan`. All four plan spans are children of the `agent.turn` span.

#### `plan.localize`

Emitted only for `Bug`-type tasks. Calls the Architect LLM to identify the
files and functions most likely to contain the bug.

| Attribute | Type | When set | Description |
|-----------|------|----------|-------------|
| `task.type` | string | On start | Always `"Bug"` |
| `suspicious_file_count` | int64 | On completion | Number of files flagged by localization |
| `suspicious_function_count` | int64 | On completion | Number of functions flagged |

Span status set to `Error` when localization fails; execution continues.

#### `plan.architect`

Calls the Architect LLM to determine which files are relevant to the task.

| Attribute | Type | When set | Description |
|-----------|------|----------|-------------|
| `task.type` | string | On start | e.g. `"Feature"` \| `"Bug"` \| `"Refactor"` |

Span status set to `Error` when the Architect call fails; the turn terminates
with `Blocked`.

#### `plan.planner`

Calls the Planner LLM to generate the step-by-step execution plan, retrying
up to `maxReplans` times if the plan fails validation.

| Attribute | Type | When set | Description |
|-----------|------|----------|-------------|
| `task.type` | string | On start | Task type |
| `validation_retries` | int64 | On completion | Number of replan attempts before a valid plan was produced |
| `step_count` | int64 | On completion | Number of steps in the final accepted plan |

Span status set to `Error` when all replan attempts fail.

#### `plan.execute`

Covers the execution of all plan steps. Each step calls `runAgent` internally,
which emits its own `agent.turn` → `agent.step` subtree.

| Attribute | Type | When set | Description |
|-----------|------|----------|-------------|
| `plan.step_count` | int64 | On start | Total number of steps in the plan |
| `task.type` | string | On start | Task type |
| `execution_mode` | string | On start | `"list"` (sequential) or `"dag"` (dependency-ordered) |
| `outcome` | string | On completion | `continue` \| `needs_input` \| `blocked` \| `done` |

Span status set to `Error` when an unhandled exception escapes step execution.

---

## Metrics

All metrics are exposed on the Prometheus HTTP server (default port 9464) and
are also exported via OTLP when an endpoint is configured. All metrics use the
`codestar` instrumentation scope.

### Counters

Counters are monotonically increasing. In Prometheus they are exposed as
`_total` suffixed metrics.

#### `codestar.tool.calls`

Incremented on every tool call completion (success or failure).

| Label | Description |
|-------|-------------|
| `tool.name` | Tool name as registered in the tool registry |
| `tool.is_success` | `true` \| `false` |
| `file.path` | File path argument, when present (e.g. for `edit`, `read`) |
| `error.message` | Error message, truncated to 200 chars, when `tool.is_success=false` |

#### `codestar.llm.calls`

Incremented on every completed LLM call (success only — failed calls do not
increment this counter).

| Label | Description |
|-------|-------------|
| `model.role` | `Coder` \| `Architect` \| `Summarizer` |
| `model.id` | Model identifier string |
| `step.number` | Step index within the turn |
| `turn.number` | Turn index within the session |
| `llm.cache_creation_tokens` | Tokens written to the Anthropic prompt cache |
| `llm.cache_read_tokens` | Tokens read from the Anthropic prompt cache |
| `llm.is_cache_hit` | `true` if any cache read tokens were used |

#### `codestar.control_signals`

Incremented each time the agent loop reaches a terminal signal.

| Label | Description |
|-------|-------------|
| `signal` | `continue` \| `needs_input` \| `blocked` \| `done` |

#### `codestar.plans.generated`

Incremented each time a plan passes validation and begins execution.

| Label | Description |
|-------|-------------|
| `task.type` | e.g. `Feature` \| `Bug` \| `Refactor` |

#### `codestar.compactions`

Incremented each time history compaction successfully completes.

| Label | Description |
|-------|-------------|
| `session.id` | Session identifier |

#### `codestar.guardrail.denials`

Incremented for each tool call that the guardrail policy explicitly denies
(does not increment for `RequireApproval`).

| Label | Description |
|-------|-------------|
| `tool.name` | Name of the denied tool |
| `guardrail.decision` | Always `"deny"` for this counter |
| `guardrail.reason` | The configured deny reason string |
| `session.id` | Session identifier |
| `user.id` | User identifier |

#### `codestar.ws.command.count`

Incremented for each JSON-RPC command processed.

| Label | Description |
|-------|-------------|
| `command.type` | `start` \| `respond` \| `approve` \| `reject` \| `compact` \| `stop` |
| `session.id` | Session identifier |
| `user.id` | User identifier |
| `success` | `true` \| `false` |

#### `codestar.mcp.calls`

Incremented for each MCP tool call.

| Label | Description |
|-------|-------------|
| `mcp.endpoint` | Configured endpoint name |
| `mcp.tool_name` | Unqualified tool name |
| `success` | `true` \| `false` |

#### `codestar.budget.exhaustions`

Incremented the first time a session's token budget is exceeded.

| Label | Description |
|-------|-------------|
| `limit_type` | The budget limit reason string from the cost tracker |
| `session.id` | Session identifier |
| `user.id` | User identifier |
| `total_tokens` | Cumulative token count at the point of exhaustion |

#### `codestar.verification.failures`

Incremented each time tree-sitter syntax verification fails for a modified file.

| Label | Description |
|-------|-------------|
| `error.type` | The verification failure reason, or `"unknown"` |
| `verify.files` | Comma-separated list of verified file paths |
| `verify.syntax_ok` | Always `false` for this counter |
| `verify.outcome` | Always `"failed"` for this counter |
| `session.id` | Session identifier |

---

### Histograms

#### `codestar.tool.duration`

Wall-clock duration of each tool call in milliseconds. Same label set as
`codestar.tool.calls`.

#### `codestar.llm.input_tokens`

Input token count per LLM call. Same label set as `codestar.llm.calls`.

#### `codestar.llm.output_tokens`

Output token count per LLM call. Same label set as `codestar.llm.calls`.

#### `codestar.llm.duration_ms`

Wall-clock duration per LLM call in milliseconds. Same label set as
`codestar.llm.calls`.

#### `codestar.llm.cache_read_tokens`

Cache read token count per LLM call. Same label set as `codestar.llm.calls`.
Zero when the Anthropic prompt cache was not consulted.

#### `codestar.plan.steps`

Number of steps in each generated plan. No labels.

#### `codestar.compaction.duration_ms`

Wall-clock duration of each compaction operation in milliseconds.

| Label | Description |
|-------|-------------|
| `session.id` | Session identifier |

#### `codestar.ws.command.duration_ms`

Wall-clock duration of each JSON-RPC command in milliseconds. Same label set
as `codestar.ws.command.count`.

#### `codestar.mcp.duration_ms`

Wall-clock duration of each MCP tool call in milliseconds. Same label set as
`codestar.mcp.calls`.

#### `codestar.verification.duration_ms`

Wall-clock duration of each verification pass in milliseconds.

| Label | Description |
|-------|-------------|
| `verify.files` | Comma-separated list of verified file paths |
| `verify.syntax_ok` | `true` \| `false` |
| `verify.outcome` | `passed` \| `failed` \| `partial` |
| `verify.reason` | Failure reason, when `verify.outcome=failed` |
| `session.id` | Session identifier |

---

### Gauges

Gauges represent a point-in-time value. In Prometheus they do not carry a
`_total` suffix.

**Note on cardinality:** All gauges below are labeled with `session.id`. In a
high-throughput multi-tenant deployment this creates one time series per active
session. For deployments with many short-lived sessions this may require
Prometheus cardinality management (recording rules, relabelling drops).

#### `codestar.compaction.ratio`

Ratio of history length after compaction to before: `len_after / len_before`.
Values near 0.0 indicate aggressive compaction; values near 1.0 indicate
compaction had little effect.

| Label | Description |
|-------|-------------|
| `session.id` | Session identifier |

#### `codestar.session.input_tokens`

Cumulative input tokens used by this session since its last `EvCostUpdate`.
Updated after every LLM call.

| Label | Description |
|-------|-------------|
| `session.id` | Session identifier |

#### `codestar.session.output_tokens`

Cumulative output tokens used by this session. Updated after every LLM call.

| Label | Description |
|-------|-------------|
| `session.id` | Session identifier |

#### `codestar.session.cost_usd`

Estimated cumulative cost of this session in USD. Updated after every LLM call.

| Label | Description |
|-------|-------------|
| `session.id` | Session identifier |

#### `codestar.session.history_tokens`

Estimated token count of the current conversation history, sampled after each
agent step. Uses the `len/4` heuristic (`estimateTokens`), not actual
tokenization.

| Label | Description |
|-------|-------------|
| `session.id` | Session identifier |

---

### UpDownCounters

UpDownCounters track a value that can increase and decrease. Exposed as plain
gauges in Prometheus.

#### `codestar.sessions.active`

Number of agent sessions currently running (worker thread alive). Incremented
when `CmdStart` spawns a session thread, decremented when the thread exits.

No labels. Both increment and decrement use `emptyAttributes` to ensure they
operate on the same Prometheus time series.

#### `codestar.ws.connections_active`

Number of WebSocket connections currently open. Incremented on accept,
decremented on close.

No labels.

---

## Structured Logs

Structured log records are exported via OTLP/HTTP using the same log pipeline
as traces. All records include the ambient OTel context (trace ID, span ID)
from the span active at the time of emission, allowing log records to be
correlated with traces in backends that support this.

Log records use the `codestar` instrumentation scope.

### Session lifecycle

#### `session.created` — SeverityInfo

Emitted when a new agent session begins executing.

| Attribute | Value |
|-----------|-------|
| `session.id` | Session identifier |
| `user.id` | User identifier |

#### `session.terminated` — SeverityInfo

Emitted when a session worker thread exits, regardless of outcome.

| Attribute | Value |
|-----------|-------|
| `session.id` | Session identifier |
| `user.id` | User identifier |
| `termination.reason` | `"done"` \| `"blocked"` \| `"needs_input"` \| `"continue"` \| `"cancelled"` \| `"error"` |

#### `auth.rejected` — SeverityWarn

Emitted when the authentication check rejects a WebSocket connection.

| Attribute | Value |
|-----------|-------|
| `error.message` | The rejection reason from the auth provider |

### Compaction

#### `compaction.failed: {error}` — SeverityWarn

Emitted when the Summarizer LLM call fails during compaction. The session
continues with the original history.

| Attribute | Value |
|-----------|-------|
| `error.message` | The compaction error description |
| `history.len` | Number of messages in history at the time of failure |
| `session.id` | Session identifier |

### Verification

#### `verification: {outcome}` — SeverityInfo or SeverityWarn

Emitted after each tree-sitter verification pass. Severity is `SeverityWarn`
when `verify.outcome = "failed"`, otherwise `SeverityInfo`.

| Attribute | Value |
|-----------|-------|
| `verify.files` | Comma-separated list of checked file paths |
| `verify.syntax_ok` | `true` \| `false` |
| `verify.outcome` | `passed` \| `failed` \| `partial` |
| `verify.reason` | Failure description (only when `verify.outcome = "failed"`) |
| `session.id` | Session identifier |

### LLM retries

#### `llm.retry: {error}` — SeverityWarn

Emitted each time the retry wrapper retries a transient LLM error
(`RateLimited` or `NetworkError`). Does not fire for non-transient errors.

| Attribute | Value |
|-----------|-------|
| `error.type` | The error message from the LLM client |
| `retry.attempt` | Zero-based attempt index |
| `retry.after_hint_ms` | Milliseconds until retry, from the provider hint (0 for non-rate-limit errors) |
| `session.id` | Session identifier (empty string when retrying from the CLI) |

### Tool calls

#### `tool.start` — SeverityDebug

Emitted when the agent dispatches a tool call, before guardrail evaluation.

| Attribute | Value |
|-----------|-------|
| `tool.name` | Tool name |

### Guardrails

#### `guardrail.decision` — SeverityInfo

Emitted for every tool call that passes through guardrail evaluation,
regardless of the decision.

| Attribute | Value |
|-----------|-------|
| `tool.name` | Tool name |
| `guardrail.decision` | `allow` \| `deny` \| `require_approval` |
| `guardrail.reason` | Reason string (empty for `allow`) |
| `session.id` | Session identifier |
| `user.id` | User identifier |

### Budget

#### `budget.exhausted` — SeverityWarn

Emitted once when a session first exceeds its token budget.

| Attribute | Value |
|-----------|-------|
| `limit_type` | The budget limit reason from the cost tracker |
| `session.id` | Session identifier |
| `user.id` | User identifier |
| `total_tokens` | Cumulative token count at exhaustion |

### WebSocket connections

#### `ws.connection.closed` — SeverityInfo

Emitted when a WebSocket connection closes normally.

| Attribute | Value |
|-----------|-------|
| `user.id` | User identifier |
| `duration_ms` | Connection lifetime in milliseconds |

### MCP endpoints

#### `mcp.connect.success` — SeverityInfo

Emitted after successfully connecting to an MCP endpoint at session startup.

| Attribute | Value |
|-----------|-------|
| `mcp.endpoint` | Configured endpoint name |
| `tool_count` | Number of tools discovered from this endpoint |

#### `mcp.connect.failed` — SeverityWarn

Emitted when an MCP endpoint connection fails. The endpoint is skipped;
session startup continues without it.

| Attribute | Value |
|-----------|-------|
| `mcp.endpoint` | Configured endpoint name |
| `error.message` | Connection error description |

---

## Library Spans

The following spans are emitted by CodeStar's internal libraries. They appear
in the same traces as application spans and share the OTel context propagated
from the calling code.

### Anthropic SDK — `libs/anthropic-sdk/`

These spans follow the [OpenTelemetry GenAI semantic conventions](https://opentelemetry.io/docs/specs/semconv/gen-ai/).

#### `gen_ai.chat`

One span per message creation or streaming request to the Anthropic API.
Emitted by `Anthropic.Client.Messages` and `Anthropic.Client.Streaming`.

| Attribute | Value |
|-----------|-------|
| `gen_ai.system` | `"anthropic"` |

#### `gen_ai.token_count`

Emitted by `Anthropic.Client.Messages` for token-count API calls.

| Attribute | Value |
|-----------|-------|
| `gen_ai.system` | `"anthropic"` |

#### `gen_ai.batch`

Emitted by `Anthropic.Client.Batches` for batch message creation requests.

| Attribute | Value |
|-----------|-------|
| `gen_ai.system` | `"anthropic"` |

---

### JSON-RPC SDK — `libs/json-rpc-sdk/`

These spans follow the [OpenTelemetry RPC semantic conventions](https://opentelemetry.io/docs/specs/semconv/rpc/).

#### `rpc.client.request`

One span per outbound JSON-RPC request from the client. The CodeStar server
uses this when making JSON-RPC calls to MCP endpoints.

| Attribute | Value |
|-----------|-------|
| `rpc.system` | `"jsonrpc"` |
| `rpc.method` | The JSON-RPC method name |

#### `rpc.client.notify`

One span per outbound JSON-RPC notification (fire-and-forget, no response).

| Attribute | Value |
|-----------|-------|
| `rpc.system` | `"jsonrpc"` |
| `rpc.method` | The JSON-RPC method name |

#### `rpc.server.handle`

One span per inbound JSON-RPC request on the server side. Emitted by the
JSON-RPC server used in MCP endpoint servers.

| Attribute | Value |
|-----------|-------|
| `rpc.system` | `"jsonrpc"` |
| `rpc.method` | The JSON-RPC method name |

---

### Cache — `libs/cache-core/`

#### `cache.get`

One span per cache lookup.

#### `cache.set`

One span per cache write.

#### `cache.evict`

One span per cache eviction.

---

### Storage — `libs/storage-core/`

#### `db.operation`

One span per storage operation.

| Attribute | Value |
|-----------|-------|
| `db.operation` | `"read"` \| `"write"` \| `"delete"` |

---

## Span Hierarchy

A complete trace for a single `CmdStart` command looks like this:

```
ws.connection
└── ws.command (command.type=start)
    └── agent.turn
        └── agent.step (step.number=0)
        │   ├── agent.compaction          [if history threshold exceeded]
        │   │   └── gen_ai.chat           [Summarizer LLM call]
        │   ├── llm.call                  [Coder LLM call]
        │   │   └── gen_ai.chat           [Anthropic SDK span]
        │   ├── mcp.tool_call             [if an MCP tool was called]
        │   │   └── rpc.client.request    [JSON-RPC to MCP endpoint]
        │   └── agent.verify              [if files were modified]
        └── agent.step (step.number=1)
            └── ...
```

For plan-based execution (`runAgentWithList` / `runAgentWithPlan`):

```
agent.turn
├── plan.localize    [Bug tasks only]
├── plan.architect
├── plan.planner
└── plan.execute
    └── agent.turn   [one per plan step, via runAgent]
        └── agent.step
            └── ...
```
