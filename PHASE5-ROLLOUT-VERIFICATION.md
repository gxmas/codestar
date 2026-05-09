# Phase 5 Rollout Verification

This checklist tracks the Phase 5 verification matrix and the minimal safety rail behavior.

## Minimal safety rail

- [x] On extractor/query fingerprint mismatch, stale cache entries are treated as non-authoritative.
- [x] Startup output surfaces a concise note when stale fingerprint entries are detected.
- [x] Output includes deterministic remediation: `codestar-cli cache-gc --delete`.
- [x] No silent background cache deletion is performed.

## Verification matrix

### macOS

- [x] Normal environment (grammars available): build and RepoMap tests pass.
- [x] Env/path mismatch diagnostics: grammar dir and warning/remediation output is shown.

### Linux

- [ ] Normal environment.
- [ ] Stale-cache scenario from prior fingerprint.

### Query mode

- [x] Embedded default from arbitrary CWD.
- [x] Env override with embedded fallback.
- [x] Strict env override fails loudly on missing/invalid files.

## Commands run locally (this branch)

- `cabal build codestar-cli repo-map`
- `cabal test codestar-test --test-options='--match "RepoMap"'`

