# Hand-Off Prompt

> **DO NOT COMMIT THIS FILE.** It is listed in `.gitignore` and is intentionally
> kept local only. Update it as work progresses between sessions.

Use this prompt to resume work in a new Claude context session.

---

We are implementing a fully spec-compliant OpenTelemetry library in Haskell.

**Design artifacts:** `~/Perso/design/otel/`
**Implementation:** `~/Perso/software/otel/`
**Construction plan:** `~/Perso/design/otel/implementation/construction-plan.md`

Read the construction plan before doing anything else. It contains the governance rules, roles, verification gates, and the full build order with step statuses.

## Current status

**Steps 6.0, 6.1, and 6.2 are complete.**

**Resume at: Phase 6, Step 6.3 — Configuration file integration test**

## CRITICAL: Agent probe required before any work

Before doing any work in the new session:
1. Probe `don-stewart`, `john-hughes`, and `ben-sigelman` with small tasks
2. If ANY spawn fails with 429, stop and report to the human operator
3. Do NOT proceed until all three roles are staffed

## Governance (summary)

- NO deviation from the plan without explicit human confirmation
- Roles: don-stewart (implements), john-hughes (tests), ben-sigelman (spec)
- All 4 verification gates before any step is `complete`

## Step 6.3 deliverables (from construction plan)

- Test: parse a comprehensive OTel config file and construct all providers
- Test: environment variable overrides work end-to-end
- Test: config-constructed pipeline produces and exports telemetry correctly

## What is now complete

**Phase 1–5:** All library packages (otel-api, otel-sdk-*, otel-exporter-*,
otel-proto, otel-semconv, otel-sdk-config, demo).

**Phase 6 so far:**
- 6.0: Deferred item triage; D-HTTP-408 one-line fix
- 6.1: `otel-integration-tests` — 4 cross-signal tests (all signals, log-trace
  correlation, shared resource, shutdown sequence)
- 6.2: D9 + D-HTTP-JSON mandatory fixes + 6 e2e pipeline tests

## Step 6.2 key changes (commit 356dab8)

**D9 fix:**
- gRPC: `parseTraceExportResult` etc. now `IO ExportResult`; inspects
  `partial_success` and warns to stderr when `rejected_*` > 0
- HTTP: `doHttpExport` gains `checkPs :: ByteString -> IO ()` parameter;
  signal-specific checkers `warnTracePartialSuccess` etc. decode the response

**D-HTTP-JSON fix:**
- Enum values: `spanKindStr`/`statusCodeStr`/`temporalityStr`/`severityNumberStr`
  return string names per proto3 JSON spec (e.g., `"SPAN_KIND_INTERNAL"` not `1`)
- int64/uint64: all timestamp and count fields encoded as JSON strings
- Bytes: `LogBodyBytes` now base64 (was hex); traceId/spanId still hex per OTLP spec

**Known limitation from 6.2:**
- HTTP/JSON mode: `warnTracePartialSuccess` tries to protobuf-decode the
  response; fails silently for JSON-mode responses. Partial_success warning
  not emitted when `ContentType = Json`. Not a spec violation (exporter still
  returns ExportSuccess correctly). Document in module Haddock.

**E2E tests (6 tests in `otel-e2e-tests` suite):**
Docker Hub was unreachable; tests use mock OTLP servers:
gRPC round-trip, HTTP protobuf, HTTP JSON (string enum assertion), gzip,
gRPC partial_success, HTTP partial_success. All pass.

## Key infrastructure notes

- `otel-integration-tests` package has TWO test suites:
  - `otel-integration-tests-test` (cross-signal, 4 tests)
  - `otel-e2e-tests` (pipeline e2e, 6 tests)
  - Run both: `cabal test otel-integration-tests otel-e2e-tests`
- Prometheus test suite requires `-threaded` (already configured)
- Build: `cabal build all` — `cabal clean` first if threaded-runtime errors appear
- semconv version: 1.27.0

## Remaining backlog (non-blocking)

From Step 6.0 triage:
- D3 (jitter formula additive), D4 (Int overflow), D6 (AsDouble only),
  D7 (temporality env var), D-HTTP-RetryAfterDate, D-HTTP-EnvVarTests,
  D-PROM-ContentType — all documented, no compliance risk
- HTTP/JSON partial_success not inspected (see above)
- Step 6.2 Docker-based OTel Collector test: deferred (Docker Hub unreachable);
  framework is written but would need `docker pull` when network is available
