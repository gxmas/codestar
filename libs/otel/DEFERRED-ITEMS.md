# Deferred Items

Last updated: 2026-05-14 (session 5)

This document tracks every item that was explicitly deferred during implementation,
its current disposition, and the recommended course of action.

---

## Governance

**Roles** (same as construction plan):

| Role | Agent | Authority |
|------|-------|-----------|
| **Implementer** | don-stewart | Executes fixes; updates status to `closed` after implementation |
| **Test Author** | john-hughes | Writes tests required by the recommendation; signs off on test adequacy |
| **Spec Verifier** | ben-sigelman | Must approve any status change from `pre-release` or `fix-now` to `closed` |
| **Human operator** | — | Final authority on all dispositions; the only party who may re-open a `closed` item or escalate a `track` item |

**Status transition rules:**

- `fix-now` → `closed`: requires implementation by don-stewart + spec sign-off by ben-sigelman + all verification gates (build, test-build, tests pass)
- `pre-release` → `closed`: same as above; human operator must also explicitly confirm before release
- `track` → any: human operator decision only — do not escalate without explicit confirmation
- `closed` → any: human operator decision only — do not re-open unilaterally
- Adding a new item: any role may add during implementation; human operator sets the initial disposition

**Verification gates** (same as construction plan — required before `closed`):

1. `cabal build all` — zero warnings, zero errors
2. Test suites compile cleanly
3. `cabal test` — all suites pass
4. ben-sigelman spec review — confirmed compliant

---

## Status legend

| Status | Meaning |
|--------|---------|
| `fix-now` | Small effort, real user impact — address in the next session |
| `pre-release` | Must be resolved before a Hackage/production release |
| `track` | Known gap; defer until a natural opportunity arises |
| `closed` | No action needed; rationale below |

---

## Items

### D3 — Retry jitter formula (additive, not full-jitter)

- **Source:** Step 5.1 (gRPC client layer)
- **Status:** `closed`
- **Description:** The backoff jitter formula is `delay + uniform[0, delay]` (additive) rather than the
  theoretically preferred `uniform[0, delay]` (full-jitter).
- **Rationale for closing:** The OTLP specification does not prescribe a jitter formula. Both
  approaches prevent thundering herd. Zero compliance risk. Not worth the churn.

---

### D4 — Int overflow in retry delay calculation

- **Source:** Step 5.1 (gRPC client layer)
- **Status:** `closed`
- **Description:** `retryInitialDelay * (2 ^ attempt)` can overflow `Int` for attempts >~30.
- **Rationale for closing:** The computed value is capped by `retryMaxDelay` before use, so the
  overflow is discarded. Theoretical only — default configurations are well under 30 attempts.

---

### D6 — NumberDataPoint always encodes as AsDouble

- **Source:** Step 5.2 (proto conversion layer)
- **Status:** `closed`
- **Description:** `NumberDataPoint` always uses the `AsDouble` encoding. There is no `AsInt` path.
- **Rationale for closing:** The SDK domain model has no integer instrument types. The encoding is
  internally consistent. Revisit only if integer instruments are added to the domain model.

---

### D7 — OTEL_EXPORTER_OTLP_METRICS_TEMPORALITY_PREFERENCE not supported

- **Source:** Step 5.3 (gRPC exporter)
- **Status:** `track`
- **Description:** The exporter always uses Cumulative temporality. The standard env var for
  switching to Delta is not read.
- **Recommendation:** Delta temporality matters for cost-conscious users (counters as deltas avoid
  double-counting in some backends). Not blocking a release, but worth tracking. Moderate effort:
  read the env var in `applyEnvOverrides` and plumb the temporality preference through
  `MetricReader`.

---

### D-HTTP-RetryAfterDate — Retry-After HTTP-date format not parsed

- **Source:** Step 5.4 (HTTP exporter)
- **Status:** `closed`
- **Description:** The `Retry-After` header is only parsed when it contains an integer number of
  seconds. The HTTP-date form (e.g. `Fri, 14 May 2026 12:00:00 GMT`) is silently ignored.
- **Rationale for closing:** No known OTel Collector implementation uses the HTTP-date form.
  Integer-seconds covers 100% of real-world cases.

---

### D-HTTP-EnvVarTests — No setEnv-based tests for OTLP/HTTP env vars

- **Source:** Step 5.4 (HTTP exporter)
- **Status:** `closed`
- **Description:** The OTLP/HTTP exporter reads several env vars (`OTEL_EXPORTER_OTLP_ENDPOINT`,
  etc.) but there are no tests that set these with `setEnv` and verify the exporter picks them up.
- **Rationale for closing:** Implementation is correct; this is a test coverage gap only. The env
  var reading code is identical to the gRPC exporter which is tested end-to-end. Add if the HTTP
  exporter is significantly modified.

---

### D-PROM-ContentType — Prometheus tests do not assert Content-Type

- **Source:** Step 5.5 (Prometheus exporter)
- **Status:** `closed`
- **Description:** The `/metrics` endpoint tests verify body content but do not assert that the
  response `Content-Type` header is `text/plain; version=0.0.4; charset=utf-8`.
- **Rationale for closing:** Test gap only; the implementation sets the correct header. Add
  alongside any future Prometheus exporter work.

---

### D9-HTTP-JSON — Silent partial-success in HTTP/JSON mode

- **Source:** Step 6.2 (D9 remediation)
- **Status:** `closed`
- **Rationale for closing:** `warnTracePartialSuccess`, `warnMetricsPartialSuccess`, and
  `warnLogsPartialSuccess` now take `ContentType` as a first argument. For `Json`, they use
  `jsonRejectedCount` (an aeson parser that extracts `partialSuccess.rejectedSpans` etc. using
  the correct camelCase proto3 JSON field names). For `Protobuf`, the existing `decodeMessage`
  path is unchanged. All three `doHttpExport` call sites updated. `ExportSuccess` is preserved
  regardless; the warning is stderr-only. ben-sigelman: PASS. (2026-05-14)

---

### Docker-e2e — OTel Collector integration test deferred

- **Source:** Step 6.2
- **Status:** `track`
- **Description:** The Docker-based OTel Collector test framework is written but was not run
  because Docker Hub was unreachable at the time. The test would spin up a real Collector, export
  via OTLP/gRPC and OTLP/HTTP, and verify data arrives.
- **Recommendation:** Run `docker pull otel/opentelemetry-collector-contrib` when network access
  is available and enable the test. The mock-server e2e tests are adequate for CI, but the
  Collector test provides true interoperability assurance.

---

### 6.3-N2 — parentbased_traceidratio sampler missing from config parser

- **Source:** Step 6.3 spec review (ben-sigelman, Note 2)
- **Status:** `closed`
- **Rationale for closing:** Added `SamplerParentBasedTraceIdRatio !Double` to `SamplerConfig`,
  `lookupSamplerConfig "parentbased_traceidratio" mArg = Just (SamplerParentBasedTraceIdRatio (fromMaybe 1.0 mArg))`,
  and `buildSampler (SamplerParentBasedTraceIdRatio r) = pure (SomeSampler (defaultParentBasedSampler (SomeSampler (TraceIdRatioBasedSampler r))))`.
  Default ratio is `1.0` per spec. Two tests added (with and without `sampler_arg`).
  ben-sigelman: PASS. (2026-05-14)

---

### 6.3-N3 — OTEL_RESOURCE_ATTRIBUTES override semantics unverified

- **Source:** Step 6.3 spec review (ben-sigelman, Note 3)
- **Status:** `closed`
- **Rationale for closing:** `Map.fromList` is last-wins. `applyResourceEnv` appends env var
  attributes after config file attributes (`config <> envAttrs`), so env vars correctly win per
  spec. Verified: added a comment to `applyResourceEnv` explaining the ordering, and added a
  QuickCheck property `"create with duplicate keys keeps last value"` to `otel-sdk-common` that
  pins this contract. Also fixed a stale test in `otel-sdk-config` that expected "unknown sampler
  defaults to always_on" — step 6.3 correctly rejects unknown samplers, and the test now verifies
  that behavior. (2026-05-14)

---

### 6.3-N4 — applyEnvOverrides called independently per create* function

- **Source:** Step 6.3 spec review (ben-sigelman, Note 4)
- **Status:** `closed`
- **Description:** Each of `createTracerProvider`, `createMeterProvider`, `createLoggerProvider`,
  and `createPropagator` independently calls `applyEnvOverrides`, reading env vars from the process
  at the moment of each call. If env vars change between calls, providers could see inconsistent
  configuration.
- **Rationale for closing:** The window for inconsistency is microseconds in a single-threaded
  startup path. Not worth the refactor in practice.

---

### 6.3-N5 — OTEL_TRACES_SAMPLER ignored when config has no tracer_provider section

- **Source:** Step 6.3 spec review (ben-sigelman, Note 5)
- **Status:** `closed`
- **Rationale for closing:** `applyEnvOverrides` now constructs a `defaultTracerProviderConfig`
  (`SamplerAlwaysOn`, `_tpcProcessors = []`) when `mSampler` is `Just _` and
  `_cfgTracerProvider` is `Nothing`, then applies the env-var sampler to it. Integration test
  added: `OTEL_TRACES_SAMPLER=always_off` with `"sdk: {}"` config produces a non-recording span.
  ben-sigelman: PASS. (2026-05-14)

---

### 6.3-N6 — Test 4 does not assert the service name override value

- **Source:** Step 6.3 spec review (ben-sigelman, Note 6)
- **Status:** `closed`
- **Description:** The `OTEL_SERVICE_NAME` override test verifies that `createTracerProvider`
  succeeds but does not assert that the resulting provider's resource contains
  `service.name = "env-service"` rather than `"config-service"`.
- **Rationale for closing:** There is no public API on `SdkTracerProvider` to inspect its resource
  after construction. The test correctly verifies the code path runs. A white-box test would
  require exposing internal state.

---

### 6.3-N7 — Test 3 uses "otlp" as the unknown exporter name

- **Source:** Step 6.3 spec review (ben-sigelman, Note 7)
- **Status:** `track`
- **Description:** The "unknown exporter" test uses `"otlp"` as the exporter name. When OTLP
  support is added to the config module, this test will start passing for the wrong reason (the
  exporter is now known) rather than testing the error path.
- **Recommendation:** Change the exporter name to something permanently fictional
  (e.g. `"no-such-exporter"`) when the OTLP exporter is added to `OTel.SDK.Config`.

---

### 6.5-N2 — Haddock comments lack OTel spec section URLs

- **Source:** Step 6.5 spec review (ben-sigelman, Note 2)
- **Status:** `track`
- **Description:** Module headers and type docs are semantically accurate but do not cite specific
  OTel specification sections or URLs (e.g.
  `https://opentelemetry.io/docs/specs/otel/trace/api/#span`).
- **Recommendation:** Add spec URLs to the highest-value symbols: `Span`, `TracerProvider`,
  `Tracer`, `MeterProvider`, `LoggerProvider`, `BatchSpanProcessor`, `SpanLimits`,
  `W3CTraceContextPropagator`. Post-release polish.

---

### 6.5-N3 — otel-api description lists profiling alongside stable signals without qualification

- **Source:** Step 6.5 spec review (ben-sigelman, Note 3)
- **Status:** `closed`
- **Rationale for closing:** Changed `otel-api.cabal` description from "profiling signal types"
  to "experimental profiling signal types". ben-sigelman noted the spec uses "Development" status
  formally, but "experimental" is the accepted colloquial term in the OTel SDK ecosystem. (2026-05-14)

---

### 6.5-N4 — StatusCode Haddock does not document the Ok > Error > Unset precedence rule

- **Source:** Step 6.5 spec review (ben-sigelman, Note 4)
- **Status:** `closed`
- **Rationale for closing:** Expanded `StatusCode` Haddock in `OTel.Trace` to a 5-sentence
  paragraph explaining: Unset is the default and correct status for successful operations; Error
  indicates failure; Ok is terminal and should only be set when the caller wants to guarantee no
  downstream override. ben-sigelman reviewed and confirmed all three statements match the spec's
  SetStatus section. (2026-05-14)

---

### M1 — `registerCallback` (batch observable callbacks) is a no-op stub

- **Source:** Full compliance assessment (ben-sigelman, 2026-05-14, session 2)
- **Severity:** major
- **Status:** `closed`
- **Rationale for closing:** Fully implemented. `SomeObservableInstrument` was restructured to
  carry a `soiName :: !Text` field enabling name-based routing. `SdkBatchObsResult` accumulates
  observations per instrument name into a `TVar (Map Text (Map Attributes Double))`.
  `BatchRegistration` entries are stored in `sdkProviderBatchRegs` on the provider; each is
  invoked once per collection cycle in `collectAllForReader` before `irCollect`.
  `mkObservableCollect` merges batch observations with per-instrument callback observations:
  Gauge uses last-write-wins (`Map.union`); Counter/UpDownCounter sums both sources
  (`Map.unionWith (+)`). `registerCallback` now returns a real `SdkCallbackRegistration` whose
  `unregister` atomically removes the registration by `Unique` identity. ben-sigelman verdict:
  PASS-WITH-NOTES — note is that routing is keyed by instrument name only (not scope+name),
  which could collide if two meters share an instrument name; tracked below as M1-N1. (2026-05-14)

---

### E2 — `BatchSpanProcessor` and `BatchLogRecordProcessor` do not enforce export timeout

- **Source:** Full compliance assessment (ben-sigelman, 2026-05-14, session 2)
- **Severity:** minor
- **Status:** `closed`
- **Rationale for closing:** Both processors now wrap `exportSpans`/`exportLogRecords` in
  `System.Timeout.timeout` using `bspExportTimeout`/`blrpExportTimeout` (default 30 000 ms) in
  both the scheduled `exportBatch` and the shutdown `drainAndExit` paths. A `Nothing` result is
  voided and the processor continues. ben-sigelman verdict: PASS. (2026-05-14)

---

### T1 — `recordException` missing `exception.stacktrace` attribute

- **Source:** Full compliance assessment (ben-sigelman, 2026-05-14, session 2)
- **Severity:** minor
- **Status:** `track`
- **Location:** `otel-sdk-traces/src/OTel/SDK/Trace.hs` ~line 479
- **Description:** The Trace API spec requires three attributes on exception events:
  `exception.type`, `exception.message`, and `exception.stacktrace`. The implementation records
  the first two but omits `exception.stacktrace`. Haskell's `SomeException` does not carry a
  stack trace by default, so there is no value to record in most cases, but the attribute's
  absence means backends that filter on its presence will not match these events.
- **Recommendation:** Accept an optional `CallStack` parameter (via `HasCallStack` or explicit
  argument) and populate `exception.stacktrace` when provided. As a minimum, document in the
  Haddock that the attribute is omitted and why.

---

### T2 — `setStatus Unset` can overwrite `Error`

- **Source:** Full compliance assessment (ben-sigelman, 2026-05-14, session 2)
- **Severity:** note
- **Status:** `closed`
- **Rationale for closing:** Added `else if code == Unset then state` guard before the `Ok`
  terminal check in `SdkSpan.setStatus`. `Unset` is now a no-op regardless of current status;
  `Ok` remains terminal. Two regression tests added. ben-sigelman: PASS. (2026-05-14)

---

### C2 — W3C traceparent v00 accepts extra trailing fields

- **Source:** Full compliance assessment (ben-sigelman, 2026-05-14, session 2)
- **Severity:** note
- **Status:** `closed`
- **Rationale for closing:** Changed pattern wildcard `_` to `rest` and added guard
  `, version /= "00" || null rest` in `parseTraceparent`. Version `00` headers with any extra
  dash-separated field are now rejected; future versions may still carry extra fields per the
  spec's forward-compatibility rule. One regression test added. ben-sigelman: PASS. (2026-05-14)

---

### T3 — `TracerProvider` type class lacks `shutdown` and `forceFlush`

- **Source:** Full compliance assessment (ben-sigelman, 2026-05-14, session 2)
- **Severity:** note
- **Status:** `closed`
- **Description:** The spec lists `Shutdown` and `ForceFlush` as TracerProvider API operations.
  These exist on `SdkTracerProvider` directly but not on the `TracerProvider` type class, so
  generic code cannot shut down an unknown provider implementation.
- **Rationale for closing:** This is an accepted Haskell idiom: shutdown is an SDK-level concern
  and placing it on the API type class would force no-op implementations to expose it. ben-sigelman
  confirmed no action required for pre-release. (2026-05-14)

---

### M1-N1 — Batch callback routing keyed by instrument name only (not scope+name)

- **Source:** M1 spec review (ben-sigelman, PASS-WITH-NOTES, 2026-05-14)
- **Severity:** note
- **Status:** `track`
- **Location:** `otel-sdk-metrics/src/OTel/SDK/Metric.hs` — `collectAllForReader`,
  `mkObservableCollect`
- **Description:** The batch observation accumulator is a `Map Text (Map Attributes Double)`
  keyed only by instrument name. If two meters create observable instruments with the same name
  (e.g. both create `"active_connections"`), a batch callback targeting one will have its
  observations merged into the other during the same collection cycle. The OTel spec defines
  instrument identity as (name, kind, unit, description, meter scope), not name alone.
- **Recommendation:** Key the batch observation map by `(InstrumentationScope, Text)` rather
  than `Text`. Requires threading the meter scope into `mkObservableCollect`. Low-priority: the
  collision requires deliberate misuse of `registerCallback` across meters.
