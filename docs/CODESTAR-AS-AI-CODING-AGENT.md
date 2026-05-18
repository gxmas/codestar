# CodeStar as a Pedagogical Tool for AI Coding Agents

CodeStar is a production AI coding agent written in Haskell. Its architecture maps
each major concept in agent design to a clearly named module, its types make
contracts explicit, and its test suite documents the invariants the system must
maintain. This makes it well-suited for teaching the engineering of AI coding agents
from first principles through production concerns.

---

## The Core Insight to Establish First

Before any code: **an AI coding agent is a control loop, not a chatbot.** The LLM is
a component inside a loop — it produces tool calls, tools execute, results feed back,
repeat. The LLM never "does" anything directly; it only reasons and requests.
Everything else in the system exists to support, constrain, and observe that loop.

```
loop:
  call LLM with history
  if tool calls → execute tools → append results → repeat
  if no tool calls → done
```

Once students have that mental model, every subsequent component has an obvious role.

---

## Phase 1: The Primitive Loop

**Concept:** The smallest thing that deserves to be called an agent.

**Entry point:** `apps/src/CodeStar/AgentLoop.hs` — `loop` → `stepAgent` →
`callLlm` → `processResponse` → `executeToolCalls`.

Strip away everything else. The anatomy is:

1. Call the LLM with the current message history
2. If the response contains tool calls, execute each one and append the results to
   history
3. Repeat until the LLM produces no tool calls

**What to build:** A toy version with a single tool (`read_file`) and a hardcoded
system prompt. No planning, no compaction, no guardrails. Students implement
`stepAgent` themselves.

**What CodeStar shows:** `apps/src/CodeStar/LLM/Base.hs` — the `LlmClientDict`
interface makes clear that the LLM is just a function:
`CompletionRequest → IO (Either LlmError CompletionResponse)`. `withRetry` in the
same file shows that the agent does not care which LLM it is talking to. The
abstraction is the lesson.

**Key question to pose:** *What does the loop need to remember between steps?*
Answer: just the `Seq Message` history. This motivates `SessionState`.

---

## Phase 2: The Tool System

**Concept:** Tools are the agent's only way to affect the world. How you design the
tool interface determines what the agent can and cannot do.

**Entry point:** `apps/src/CodeStar/Tools/Registry.hs`

Study the `ToolDefinition` / `ToolHandlerDict` split:

- The **definition** is what the LLM sees: name, description, JSON schema
- The **handler** is what the runtime executes

These must match but are deliberately separate.

**What to build:** Add a second tool (`write_file`). Students write the handler and
the schema. When the schema is wrong, the LLM fails to call the tool correctly — this
is the lesson. Schema design is a skill.

**CodeStar extension — MCP:** `apps/src/CodeStar/Tools/MCP.hs` shows that tools do
not have to be local. The same `ToolHandlerDict` interface wraps remote MCP tool
servers, prefixing each tool with the endpoint name (e.g., `filesystem.read_file`).
Students who understand the abstraction find MCP obvious; students who do not are
confused.

**Key question:** *Who decides which tools the agent gets?* The registry is assembled
at session startup in `apps/codestar-serve/Server.hs`. Tools are not dynamic — the
agent cannot give itself new tools mid-session.

---

## Phase 3: The System Prompt and Context Window

**Concept:** The system prompt is not just instructions — it is all the context the
LLM has about its world. Managing what fits is a real engineering problem.

**Entry point:** `apps/src/CodeStar/Context.hs` — `assemble`

Walk through `assemble`: it takes a token budget and fills it with system
instructions, repo map, memory, and compaction summaries in priority order. When
things do not fit, lower-priority items are trimmed.

**What to demonstrate:** Show the repo map (`apps/src/CodeStar/RepoMap/`). This is
PageRank over a symbol graph derived from tree-sitter parses. The agent gets a
condensed view of the codebase, not the full source. Ask students: *what would happen
if you gave the agent the full codebase?* Answer: context overflow, cost explosion,
degraded reasoning.

**The trade-off triangle:** context size × cost × quality. Every context management
decision is navigating this triangle.

**CodeStar-specific:** `estimateTokens` in
`apps/src/CodeStar/RepoMap/Render.hs` uses a `len/4` heuristic. Have students
measure how wrong this is versus real tokenization, and discuss why "good enough"
matters here.

---

## Phase 4: Compaction

**Concept:** Long-running agents exhaust their context window. Compaction is the
agent compressing its own memory.

**Entry point:** `apps/src/CodeStar/Compaction.hs` — `shouldCompact` → `compact`

The key insight: **the agent uses a second LLM call (the Summarizer role) to
summarize its own history.** This is the agent's only form of forgetting. The
`CompactionConfig` controls when this triggers (`triggerFraction` of
`maxContextTokens`).

**The hard problem to surface:** What must survive compaction? The task description,
stored in `CompactionState.csTask`. Students who have not thought about this ask "why
does the agent forget what it was doing?" — that is when compaction semantics become
concrete.

**Connect to real failure modes:** What happens if compaction fires at the wrong time?
What if the summary loses a key constraint? This is where students appreciate that
compaction is lossy and why the trigger threshold matters.

---

## Phase 5: Guardrails and Safety

**Concept:** The agent will try to do exactly what you ask and exactly what the LLM
decides. Both of these are wrong sometimes. You need a policy layer between intent and
execution.

**Entry point:** `apps/src/CodeStar/Guardrails.hs`

Three outcomes from `evaluate`:

- `Allow` — proceed
- `Deny reason` — refuse and return the reason as tool output
- `RequireApproval reason` — pause and wait for human confirmation

`RequireApproval` is particularly instructive: it shows that human-in-the-loop is a
first-class mode, not a fallback. The agent loop in `AgentLoop.hs:executeTool` wires
this to the session's approval MVar.

**What to demonstrate:** Configure a deny list that blocks `rm -rf`. Show the agent
attempting a shell command, hitting the guardrail, then adapting its approach.
Students see that guardrails shape behaviour without modifying the LLM or the tools.

**Connect to `riskTier`:** Tool definitions in `Registry.hs` declare themselves
`ReadOnly` or `SideEffect`. The guardrail system can apply different policies by tier.
Ask: *should `grep` require approval?* Students disagree, which is the point — policy
is a design choice.

---

## Phase 6: Planning

**Concept:** Some tasks are too complex for a single agent turn. Planning decomposes a
task into steps, each of which the agent loop executes independently.

**Entry point:** `apps/src/CodeStar/PlanExecution.hs` — `runWithPlan`,
`runWithPlanDag`

Three phases:

1. **Architect** — which files are relevant to this task?
2. **Planner** — what sequence of steps will complete it?
3. **Execute** — run each step through the agent loop

`runWithPlan` executes steps sequentially. `runWithPlanDag` schedules steps by
dependency graph, allowing parallelism when steps are independent.

**The key question:** *Who validates the plan?* `phase2PlannerValidated` retries up
to `maxReplans` times if the generated plan fails validation. Students realise the
planner itself can be unreliable and the system must handle that.

**What to build:** Students write a custom `executeStep` function. When it fails
halfway through a plan, what happens? This surfaces the replan loop and the
`triedFps` fingerprint set, which prevents the planner from repeating approaches
that have already failed.

---

## Phase 7: Verification

**Concept:** The agent must check its own work. Syntax errors are detectable without
running the code.

**Entry point:** `apps/src/CodeStar/Verification.hs`

After any file-modifying tool call, tree-sitter parses the result. If it fails, the
agent receives the error as a tool result and continues — it does not stop. This is a
deliberate design choice: verification provides signal, not enforcement.

Three outcomes from `verify`:

- `VerificationPassed evidence` → emit `Done` signal
- `VerificationFailed reason` → return the reason as feedback, emit `Continue`
- `VerificationPartial` → pass the original signal through

**Discussion:** *Should syntax failure stop the agent?* CodeStar returns `Continue`
on failure (`AgentLoop.hs:verifyStepResult`). The agent might fix it in the next
step. Students who expect hard enforcement are surprised; use this to discuss the
difference between hard constraints (guardrails) and soft feedback (verification).

---

## Phase 8: Observability

**Concept:** An agent you cannot observe is an agent you cannot debug or trust.
Telemetry is not optional.

**Entry point:** `apps/src/CodeStar/Telemetry.hs`, `libs/otel/`

Use the observability audit on CodeStar as a case study. Walk through the span
hierarchy:

```
ws.connection
  └── ws.command
        └── agent.turn
              └── agent.step
                    ├── agent.compaction
                    ├── llm.call
                    ├── mcp.tool_call
                    └── agent.verify
```

Show what happens when a step is slow: without `agent.step` spans, you cannot tell
whether the slowness is in the LLM call, tool execution, or compaction.

**The sampling lesson:** The distinction between head-based and tail-based sampling.
Head-based sampling discards traces before you know whether they contain errors. This
was a real bug in this codebase: `TraceIdRatioBasedSampler` would drop 90% of error
traces when the sample rate was below 1.0.

**Key metrics to introduce:**

| Metric | Type | What it tells you |
|--------|------|-------------------|
| `codestar.llm.duration_ms` | Histogram | Where latency comes from |
| `codestar.session.history_tokens` | Gauge | Context growth pressure |
| `codestar.compaction.duration_ms` | Histogram | Cost of forgetting |
| `codestar.budget.exhaustions` | Counter | When the agent hits limits |
| `codestar.ws.connections_active` | UpDownCounter | Server load |

**What to demonstrate:** Run the agent against a real task with OTLP export to Jaeger
or Honeycomb. Show students what a trace looks like. Then break something and show
that without spans you cannot find it.

---

## Phase 9: Multi-Session and Transport

**Concept:** A real agent system handles many simultaneous users. Session isolation
is non-trivial.

**Entry point:** `apps/codestar-serve/Server.hs`,
`apps/src/CodeStar/Platform/SessionManager.hs`

Each session owns:

- An input `MVar` for `CmdRespond`
- An approval `MVar` for `CmdApprove` / `CmdReject`
- Its own worker thread
- Its own `CostTracker`
- Its own `SessionStatus` TVar

`handleConnection` → `async` thread → `bracket_` is the full lifecycle. The async
thread inherits the OTel context from the calling thread via `getCurrent` /
`attach` — without this, `agent.turn` would be a root span disconnected from
`ws.command`.

**The async exception lesson:** The race condition between `startSpan` returning and
`finally` installing the cleanup handler is a real bug that was found and fixed in
this codebase. Show the property-based test that caught it
(`apps/test/CodeStar/ServerSpanSafetySpec.hs`), and the fix using `mask`. Property-
based testing finds concurrency bugs that unit tests miss.

---

## Recommended Sequence

| Phase | Core Concept | Entry Point |
|-------|--------------|-------------|
| 1 | LLM-as-component, the turn loop | `AgentLoop.hs:stepAgent` |
| 2 | Tool abstraction and schema design | `Tools/Registry.hs` |
| 3 | Context window as a resource | `Context.hs:assemble` |
| 4 | Compaction / lossy memory | `Compaction.hs:compact` |
| 5 | Guardrails and policy | `Guardrails.hs` |
| 6 | Multi-step planning | `PlanExecution.hs:runWithPlan` |
| 7 | Verification as feedback | `Verification.hs` |
| 8 | Observability | `Telemetry.hs` + OTel traces |
| 9 | Session isolation, concurrency | `Server.hs` + `SessionManager.hs` |

---

## What Makes CodeStar Particularly Good for This

**Types as contracts.** Haskell types make every interface explicit. `LlmClientDict`,
`ToolHandlerDict`, `TelemetryRecorder` — each is a record of functions. Students
cannot ignore what a component takes and returns, and cannot call something without
knowing its type.

**The test suite documents invariants.** Reading the specs is a way into the
architecture:

- `ServerSpanSafetySpec` — span lifecycle under async exceptions
- `AgentTelemetrySpec` — what events the agent loop must emit and when
- `LlmRetrySpec` — retry callback semantics
- `OTel/ContextThreadSpec` — thread-local context isolation
- `PlanTelemetrySpec` — span balance across plan execution phases

**Multiple backends for everything.** Three LLM providers (Anthropic, OpenAI, Ollama).
Three telemetry backends (no-op, JSON-to-stderr, OTLP+Prometheus). Two execution
modes (sequential list, dependency DAG). Students see that good abstractions make
backends interchangeable.

**Real failure modes are documented.** The compaction memory loss, the head-based
sampling bug, the async exception span race, the UpDownCounter label mismatch — these
are real bugs that were found and fixed, with the fixes visible in the commit history.
Teaching with a codebase that has documented failure modes is more valuable than
teaching with a pristine example.

**The observability audit.** An independent review of the codebase's telemetry gaps
produced a prioritised list of missing spans, metrics, and structured logs. The audit,
the fixes, and the subsequent code review are all available as a worked example of how
to instrument a system you did not write.

---

## Exercises by Phase

### Phase 1
- Implement a minimal agent loop with one tool and a counter that stops at 10 steps
- What happens when the tool always returns an error? Does the agent loop forever?
- Measure how many LLM calls a simple task requires

### Phase 2
- Write a `list_directory` tool. What should its schema look like?
- Deliberately write an ambiguous schema. Observe the LLM misuse it
- Wrap a real CLI tool (e.g., `git log`) as an agent tool

### Phase 3
- Measure the token cost of including the full repo map versus the condensed version
- What happens to task quality when the context budget is cut in half?
- Add a new component to `assemble` with lower priority than the repo map

### Phase 4
- Trigger compaction manually by setting a very low `triggerFraction`
- After compaction, ask the agent to recall a detail from early in the session
- Add a field to `CompactionState` that must survive compaction

### Phase 5
- Add a guardrail that denies any shell command containing `sudo`
- Implement a `RequireApproval` policy for all `write_file` calls
- Show how the agent adapts its plan when a tool is denied

### Phase 6
- Generate a plan for a simple refactoring task. Read the raw plan output
- Intentionally produce an invalid plan. Observe the retry loop
- Compare the execution trace of `runWithPlan` vs `runWithPlanDag` for the same task

### Phase 7
- Introduce a syntax error into a file and observe the verification event
- Modify `verifyStepResult` to stop the agent on failure instead of continuing
- Measure how often verification triggers on real tasks

### Phase 8
- Export traces to Jaeger. Find the slowest span in a real agent run
- Change the sample rate to 0.1 and try to find a failed run in the traces
- Add a new `AgentEvent` variant and wire it through to a Prometheus metric

### Phase 9
- Run two simultaneous sessions and verify they do not share history
- Kill a session mid-run with `CmdStop`. What state is the session in afterward?
- Write a property-based test for a new span lifecycle in `AgentLoop.hs`
