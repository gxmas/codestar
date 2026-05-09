# Query-Driven Language Extraction Prompt

Goal: Add high-quality RepoMap symbol extraction for `<LANGUAGE>` in this codebase.

## Scope and current layout

- Extraction API is re-exported from `CodeStar.RepoMap.Graph`.
- Extraction implementation lives in `CodeStar.RepoMap.Graph.Extract`.
- Specialized paths exist for:
  - `CodeStar.RepoMap.Graph.Extract.Haskell`
  - `CodeStar.RepoMap.Graph.Extract.TypeScript`
- Shared tag types live in `CodeStar.RepoMap.Graph.Extract.Types`.
- Grammar discovery/mapping lives in:
  - `CodeStar.TreeSitter`
  - `CodeStar.TreeSitter.Grammars`
- Worker pre-filters files using `codeExtensions` in `CodeStar.RepoMap.Worker`.

## Execution checklist

1) Confirm baseline flow
- Verify dispatch location: `extractTagsDetailed` in `CodeStar.RepoMap.Graph.Extract`.
- Verify ranking consumers: `buildSymbolGraph` + `pageRank`.
- Verify query source mode behavior (`CODESTAR_QUERIES_DIR`, `CODESTAR_QUERIES_STRICT`).

2) Add grammar support (required)
- Add `<LANGUAGE>` to `knownGrammars` in `CodeStar.TreeSitter.Grammars`:
  - language key, repo, version, symbol, extensions, scanner/subdir metadata.
- Ensure extension is covered by `grammarByExtension` (derived from `knownGrammars`).
- Add extension to `codeExtensions` in `CodeStar.RepoMap.Worker`.
- Install grammar with:
  - `codestar-cli fetch-grammars`
  - or `codestar-cli fetch-grammars <language>`

3) Pick extraction strategy
- If `<LANGUAGE>` is listed in `design/LANGUAGE-TAGS.md`, mirror Aider's extracted categories as the default target (definition/reference kinds) unless there is a clear project-specific reason to diverge.
- For common targets, default categories are:
  - `python`: defs `class,function`; refs `call`
  - `go`: defs `function,method,type`; refs `call,type`
  - `rust`: defs `class,function,interface,macro,method,module`; refs `call,implementation`
- Start with generic extraction path.
- If quality is noisy, add `CodeStar.RepoMap.Graph.Extract.<Language>` and dispatch branch in `extractTagsDetailed`.

4) Add query-driven extraction (if specialized)
- Add `queries/<language>-definitions.scm`.
- Capture declaration-head names only.
- Use explicit `.name` captures (for example `@def.function.name`).
- Add capture allowlist filtering in extractor.
- Emit tags from identifier-like direct nodes only (unless justified).
- Keep fallback safe: query failure must not break unsupported/other languages.

5) Validate
- Build and run tests.
- Run repo-map/indexing and inspect extraction quality.
- Verify unsupported/no-grammar behavior is unchanged.
- Verify cache behavior remains correct.

6) Add regression tests
- Add at least one `<LANGUAGE>` fixture with top-level and local declarations.
- Assert expected declarations are present.
- Assert locals/noise are absent or significantly reduced.
- Keep tests deterministic and small.

## Constraints

- Do not regress Haskell/TypeScript extraction behavior.
- Preserve generic fallback for non-specialized languages.
- Keep changes minimal and aligned with current module split.
- If query capability is missing, add the smallest viable API surface and explain it.

## Deliverables

- Changed files list.
- Short rationale per file.
- Sample `/repomap` excerpt for `<LANGUAGE>`.
- Before/after extraction quality notes.
- Follow-up recommendations.
