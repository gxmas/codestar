# RepoMap

A PageRank-based code index that selects the most relevant symbols (functions, classes, types) from the repository and injects them into the LLM's system prompt. It gives the model a "map" of the codebase without consuming the entire context window.

## Core Modules

| Module | Role |
|--------|------|
| `apps/src/CodeStar/RepoMap/Graph.hs` | Symbol graph + PageRank ranking |
| `apps/src/CodeStar/RepoMap/Graph/Extract.hs` | Tree-sitter tag extraction per language |
| `apps/src/CodeStar/RepoMap/Render.hs` | Token-budgeted rendering |
| `apps/src/CodeStar/RepoMap/Worker.hs` | Background indexing loop |
| `apps/src/CodeStar/RepoMap/Cache.hs` | mtime-based cache layer |
| `apps/src/CodeStar/RepoMap/CacheGc.hs` | Stale entry garbage collection |

## Internal Pipeline

### 1. Tag Extraction

Tree-sitter parses each file. Language-specific queries (Haskell, Python, TypeScript) or a generic AST walker classify nodes as `Definition` or `Reference`. Each produces:

```haskell
data Tag = Tag
  { tagFile :: !FilePath
  , tagName :: !Text
  , tagLine :: !Int
  , tagKind :: !TagKind  -- Definition | Reference
  }
```

### 2. Symbol Graph

All tags are assembled into a `SymbolGraph`:

- `sgFiles` — set of all indexed files
- `sgEdges` — sparse adjacency list (file → files it references)
- `sgSymbols` — map from symbol name to files defining it

Edges point FROM referencing files TO defining files, capturing cross-file dependencies.

### 3. PageRank

Personalised PageRank (damping=0.85, 100 iterations) ranks files by importance. Personalization boosts:

- **Open/chat files**: 50x weight
- **Mentioned identifiers**: 10x weight (resolved via symbol map)
- **Base score**: 1.0 for all files

The adjacency matrix normalizes contribution by out-degree: `weight = 1 / out_degree(from)` for each reference edge.

### 4. Rendering

Definitions are sorted by rank, then a binary search finds the largest prefix fitting the token budget (heuristic: 1 token ~ 4 characters). Output format:

```
path/to/file.py:
    functionName
    ClassName

path/to/other.hs:
    someFunction
    SomeType
```

`RenderConfig` controls:
- `maxTokens` — hard budget (default 4096)
- `tolerance` — how close to fill the budget (default 0.15 = within 15%)

## Wiring Into the Agent

### Overview: Key Data Carriers

| Type | Field | Purpose |
|------|-------|---------|
| `AgentEnv` | `envCompState :: CompactionState` | Holds repo map for the session lifetime |
| `CompactionState` | `csRepoMap :: !Text` | The rendered repo map text |
| `SessionState` | `ssCompState :: CompactionState` | Threaded through each agent turn |
| `PlanExecutionConfig` | `repoContext :: !Text` | Carries repo map into plan execution |
| `CompletionRequest` | `systemPrompt :: Maybe Text` | Final delivery to LLM (contains repo map) |

---

### 1. Session Startup — Initial Construction

**Entry point**: `Server.hs:304`

```
handleCommand (CmdStart ...)           -- Server.hs:265
  └─ repoMapText <- buildRepoMapSafe   -- Server.hs:304
```

**Function**: `buildRepoMapSafe` (`Server.hs:564-581`)

```haskell
buildRepoMapSafe :: GrammarRegistry -> RepoMapCache -> FilePath -> IO Text
```

Implementation:
1. `listWorkspaceFiles workspacePath` — recursively lists all files (`Server.hs:553-562`)
2. For each file: `getOrComputeTags repoCache f (extractTags grammarReg f src)` (`Server.hs:570`)
3. `let graph = buildSymbolGraph allTags` (`Server.hs:571`)
4. `scores = pageRank graph [] [] defaultWeights` (`Server.hs:572`) — no personalization on first run
5. `renderRepoMap allTags scores graph defaultRenderConfig{maxTokens = 4096}` (`Server.hs:574-578`)
6. Returns empty `Text` on timeout (5s deadline, `Server.hs:581`)

---

### 2. Context Assembly — Injecting Into System Prompt

**Entry point**: `Server.hs:329-335`

```
handleCommand (CmdStart ...)
  └─ buildRepoMapSafe ...              -- produces repoMapText
  └─ assemble ctxCfg basePrompt repoMapText memPaths []  -- Server.hs:330
```

**Function**: `assemble` (`Context.hs:97-137`)

```haskell
assemble ::
  ContextConfig ->
  Text ->          -- base system prompt
  Text ->          -- rendered repo map
  [FilePath] ->    -- memory file paths
  [FilePath] ->    -- step-scoped files
  IO ([Message], ContextParts)
```

Assembly logic (`Context.hs:113-122`):

```haskell
repoSection =
  if Text.null repoMap
    then Text.empty
    else "## Repository Map\n\n" <> repoMap <> "\n"

sysPrompt =
  Text.intercalate "\n\n"
    (filter (not . Text.null) [basePrompt, repoSection, memSection])
```

Returns `ctxParts.systemPrompt` containing the full system prompt with repo map section.

---

### 3. Storage in AgentEnv

**Location**: `Server.hs:360`

```haskell
envCompState = emptyCompactionState{csRepoMap = repoMapText}
```

The `AgentEnv` is the long-lived environment for the session. The repo map is stored in its `CompactionState` and inherited by all subsequent turns.

---

### 4. Agent Loop — Threading Through Turns

**File**: `AgentLoop.hs`

**SessionState** (`AgentLoop.hs:228-242`):

```haskell
data SessionState = SessionState
  { ssHistory   :: !(Seq Message)
  , ssCompState :: !CompactionState   -- carries csRepoMap
  , ssTurnCount :: !Int
  }
```

**Initialization** (`AgentLoop.hs:258-264`):

```haskell
sessionFromEnv :: AgentEnv -> SessionState
sessionFromEnv env = SessionState
  { ssHistory   = Seq.empty
  , ssCompState = env.envCompState    -- inherits repo map from AgentEnv
  , ssTurnCount = 0
  }
```

**Per-turn flow** (`AgentLoop.hs:298`):

```
runAgentTurn env session
  └─ session.ssCompState.csRepoMap is available throughout
  └─ maybeCompact env session        -- preserves csRepoMap
  └─ callLlm env sysPrompt ...      -- sysPrompt contains repo map
```

---

### 5. LLM Call — Final Delivery

**Function**: `callLlm` (`AgentLoop.hs:497-520`)

```haskell
callLlm :: AgentEnv -> Text -> Int -> Int -> Seq Message -> Seq Message
        -> IO (Either ControlSignal (CompletionResponse, Seq Message))
```

**Call site** (`AgentLoop.hs:431`):

```haskell
llmResult <- callLlm env sysPrompt turn.tsStep session'.ssTurnCount
              session'.ssHistory processedHistory
```

Where `sysPrompt` was set at `Server.hs:336`:

```haskell
let sysPrompt = ctxParts.systemPrompt  -- contains "## Repository Map\n\n..."
```

**Request construction** (`AgentLoop.hs:502-509`):

```haskell
req = CompletionRequest
  { messages     = toList processedHistory
  , systemPrompt = Just sysPrompt      -- repo map delivered here
  , tools        = toolSchemas env.envTools
  , maxTokens    = 8192
  , temperature  = Nothing
  , topP         = Nothing
  }
```

Sent to LLM via `client.stream req` (`AgentLoop.hs:519`).

---

### 6. Compaction — Preservation Across Context Compression

**Trigger**: `maybeCompact` (`AgentLoop.hs:449-483`)

```
maybeCompact env session
  └─ shouldCompact env.envCompaction session.ssHistory  -- AgentLoop.hs:451
  └─ compact client session.ssCompState session.ssHistory mInstruction  -- AgentLoop.hs:460
```

**Function**: `compact` (`Compaction.hs:98-124`)

```haskell
compact ::
  LlmClientDict ->
  CompactionState ->        -- carries csRepoMap
  Seq Message ->
  Maybe Text ->
  IO (Either Text (Seq Message))
```

**Re-injection after compaction** (`Compaction.hs:152-171`):

```haskell
buildCompactedHistory :: CompactionState -> Text -> Seq Message
buildCompactedHistory cs summary =
  let header = Text.unlines
        [ "[Compacted context]"
        , ""
        , "## Summary of prior work"
        , summary
        , ""
        , if Text.null cs.csRepoMap
            then ""
            else "## Repository Map\n\n" <> cs.csRepoMap   -- re-injected here
        , ...
        ]
   in Seq.singleton (Message System [TextContent header])
```

The repo map is read from `CompactionState` and written into the compacted history as a `System` message, ensuring continuity.

---

### 7. Plan Execution — Repo Map in Multi-Step Plans

**Functions**: `runAgentWithList` (`AgentLoop.hs:355`) and `runAgentWithPlan` (`AgentLoop.hs:372`)

```haskell
runAgentWithList env sysPrompt spec triedFps = do
  client <- readIORef env.envLlm
  let cfg = defaultPlanExecutionConfig{repoContext = env.envCompState.csRepoMap}
  ...
  runWithPlan env.envTelemetry client client client cfg spec triedFps executeStep
```

**Type** (`PlanExecution.hs:59-64`):

```haskell
data PlanExecutionConfig = PlanExecutionConfig
  { maxReplans  :: !Int
  , repoContext :: !Text    -- repo map stored here
  }
```

**Usage** (`PlanExecution.hs:108`):

```haskell
lr <- localize arch defaultLocalizationConfig spec cfg.repoContext
```

The `repoContext` provides architectural context to the `localize` function during plan generation.

---

### 8. File Edit Hook — Cache Invalidation

**Hook setup** (`Server.hs:308`):

```haskell
let onEdit = Just (repoCache.invalidate)
    registry = foldr register (buildRegistry tracker todoStore sandbox onEdit) mcpHandlers
```

**Trigger in edit tool** (`Tools/Edit.hs:57-75`):

```haskell
invokeEdit :: ReadTracker -> Maybe TreeSitter.Language -> Maybe (FilePath -> IO ())
           -> ToolInput -> IO (Either ToolError ToolOutput)
```

Call site (`Edit.hs:68-74`):

```haskell
result <- applyEdit mLang input.path input.old input.new input.replaceAll content
case result of
  Right _ -> do
    case mOnEdit of
      Just onEdit -> onEdit (Text.unpack input.path)   -- triggers invalidation
      Nothing     -> pure ()
```

**Cache invalidation** (`RepoMap/Cache.hs:161-169`):

```haskell
doInvalidate :: StorageBackend -> FilePath -> IO ()
doInvalidate store path = do
  keys <- store.list nsTags
  let matching =
        [ key
        | key <- keys
        , maybe False (\info -> info.tkiPath == path) (parseTagsCacheKey key)
        ]
  mapM_ (\k -> store.delete nsTags k) matching
```

Removes all cached tag entries for the modified file. Next repo map rebuild will re-extract tags for that file.

---

### 9. Background Worker — Async Updates (Defined but Not Currently Wired)

**File**: `RepoMap/Worker.hs`

The worker module is fully implemented but **not currently instantiated** in `Server.hs`. The initial repo map is built synchronously via `buildRepoMapSafe` only.

**Type** (`Worker.hs:86-97`):

```haskell
data RepoMapWorker = RepoMapWorker
  { rwState    :: !(TVar WorkerState)
  , rwUpdates  :: !(TChan Text)       -- broadcast channel for updates
  , rwThreadId :: !ThreadId
  }
```

**State** (`Worker.hs:60-69`):

```haskell
data WorkerState = WorkerState
  { wsTags         :: !(Map FilePath [Tag])
  , wsRendered     :: !Text              -- current rendered map
  , wsLastRebuild  :: !UTCTime
  , wsPendingFiles :: !(Set FilePath)
  }
```

**Update broadcast** (`Worker.hs:343-358`):

```haskell
rebuildGraph = do
  state <- atomically $ readTVar stateVar
  let allTags  = concat (Map.elems state.wsTags)
      graph    = buildSymbolGraph allTags
      scores   = pageRank graph [] [] defaultWeights
      rendered = renderRepoMap allTags scores graph renderCfg
  atomically $ do
    modifyTVar' stateVar $ \s ->
      s{wsRendered = rendered, wsLastRebuild = now, wsPendingFiles = Set.empty}
    writeTChan updates rendered           -- broadcast to subscribers
```

**Access API**:
- `getCurrentMap :: RepoMapWorker -> IO Text` (`Worker.hs:165`)
- `subscribeToUpdates :: RepoMapWorker -> IO (TChan Text)` (`Worker.hs:186`)

---

### Call Stack Summary

```
SERVER STARTUP
  Server.hs:265  handleCommand CmdStart
  Server.hs:301    grammarReg <- loadGrammarRegistry
  Server.hs:302    repoCache  <- newRepoMapCache storageBackend
  Server.hs:304    repoMapText <- buildRepoMapSafe grammarReg repoCache workspacePath
  Server.hs:330    (_, ctxParts) <- assemble ctxCfg basePrompt repoMapText memPaths []
  Server.hs:336    let sysPrompt = ctxParts.systemPrompt
  Server.hs:360    let env = AgentEnv{envCompState = emptyCompactionState{csRepoMap = repoMapText}, ...}

EACH AGENT TURN
  AgentLoop.hs:298   runAgentTurn env session
  AgentLoop.hs:262     session = sessionFromEnv env  (first turn only)
  AgentLoop.hs:449     maybeCompact env session
  AgentLoop.hs:431     callLlm env sysPrompt ...
  AgentLoop.hs:504       CompletionRequest{systemPrompt = Just sysPrompt}
  AgentLoop.hs:519       client.stream req

COMPACTION (when history exceeds budget)
  AgentLoop.hs:460   compact client session.ssCompState history mInstruction
  Compaction.hs:152    buildCompactedHistory cs summary
  Compaction.hs:164      "## Repository Map\n\n" <> cs.csRepoMap  -- re-injected

FILE EDIT
  Edit.hs:72         onEdit (Text.unpack input.path)
  Cache.hs:161         doInvalidate store path  -- removes cached tags

PLAN EXECUTION
  AgentLoop.hs:358   let cfg = defaultPlanExecutionConfig{repoContext = env.envCompState.csRepoMap}
  AgentLoop.hs:362   runWithPlan ... cfg spec ...
  PlanExecution.hs:108  localize arch locCfg spec cfg.repoContext
```

## Background Worker (Worker.hs)

A separate thread continuously indexes the repository:

1. Processes files in batches of 10
2. Checks mtime vs cache; re-extracts only if changed
3. Every >= 2000ms (configurable), rebuilds the full graph and re-renders
4. Broadcasts updates via `TChan`

Slow indexing never blocks the LLM call — the agent uses whatever map is currently ready.

### Processing Flow Per File

1. Check if file has a supported extension
2. Get mtime, check cache
3. If cached with same mtime: use cached tags
4. If not cached or file changed: extract tags, store in cache
5. Mark file as pending rebuild

## Caching

### Tag Cache

- **Key format**: `v2:{fingerprint}:{path}:{mtime}`
- **Fingerprint**: hash of extractor version + known grammars list
- **Invalidation**: file edit or mtime change

### Rendered Map Cache

- **Key**: `hash(sorted files, maxTokens, mentioned identifiers)`
- **Invalidation**: different inputs produce different key

### Garbage Collection (CacheGc.hs)

Detects stale entries by:
- **Global stale**: fingerprint mismatch (extractor version changed)
- **File stale**: file mtime differs from cached mtime
- **Both**: both conditions true

Background sweep removes stale entries and reports statistics.

## Configuration

Defaults in `Config/Defaults.hs`:

```toml
[repomap]
rebuild_interval_ms = 2000   # minimum ms between rebuilds
rm_max_tokens = 4096         # token budget for rendered map
batch_size = 10              # files per worker batch

[context]
max_tokens = 200000          # total context window
repo_map_reserve = 4096      # reserved for repo map
memory_reserve = 2048        # reserved for memory
compaction_reserve = 1024    # compaction overhead
response_reserve = 8192      # reserved for model output
```

Environment variables:
- `CODESTAR_QUERIES_DIR` — override tree-sitter query file location (else embedded)
- `CODESTAR_QUERIES_STRICT` — strict mode (fail if query files missing)

## Supported Languages

`.hs`, `.py`, `.js`, `.ts`, `.tsx`, `.rs`, `.go`, `.c`, `.cpp`, `.java`, `.rb`, `.swift`, `.kt`, `.scala`, `.ex`, `.exs`, `.lua`, `.pl`, `.r`, `.R`

Languages with dedicated tree-sitter queries: Haskell, Python, TypeScript. All others use the generic AST walker fallback.

## CLI Tool (apps/repo-map/Main.hs)

A standalone executable for debugging and inspection:

| Flag | Purpose |
|------|---------|
| `--index [DIR]` | Extract and count tags per file |
| `--file FILE` | Inspect single file tags |
| `--dump-ast FILE` | Print parse tree |
| `--check-grammar LANG` | Verify grammar loading |
| `--cache-stats [DIR]` | Show cache hit rates |
| `--pagerank [DIR]` | Show PageRank scores |
| `--render [DIR]` | Show rendered repo map as sent to LLM |
| `--focus FILE` | Personalise as if FILE is open (repeatable) |
| `--max-tokens N` | Override token budget |
| `--lang LANG` | Filter by language |

## Data Flow Summary

```
Session Startup:
  listWorkspaceFiles()
    -> extractTags (per file, with cache lookup)
    -> buildSymbolGraph(all tags)
    -> pageRank(graph, [], [])       -- no personalization initially
    -> renderRepoMap(tags, scores, graph, config)
    -> repoMapText

Context Assembly:
  repoMapText
    -> "## Repository Map\n\n" <> repoMapText
    -> injected into system prompt

Agent Loop:
  LLM call with system prompt (includes repo map)
    -> tool execution
    -> if file modified: repoCache.invalidate(path)

Compaction:
  history too long?
    -> compact(history) -> summary
    -> re-inject csRepoMap into compacted context

Background Worker:
  workerLoop():
    processFile(path):
      check mtime -> cache lookup
      if miss: extractTags() -> cache.putTags()
    every 2s: rebuildGraph():
      all tags -> buildSymbolGraph() -> pageRank() -> renderRepoMap()
      -> broadcast update
```

## Design Decisions

1. **PageRank for relevance** — files that are heavily imported/referenced rank higher, surfacing architectural "hubs" rather than leaf nodes.
2. **Personalization without rebuild** — boosting open files and mentioned identifiers adjusts ranking without reconstructing the graph.
3. **Token budget binary search** — fills exactly as much as allowed with no waste and no overflow.
4. **Compaction-safe** — the map outlives context truncation so the model never loses codebase awareness mid-session.
5. **Async/non-blocking** — background worker means the first LLM call doesn't wait for full indexing to complete.
6. **Graceful degradation** — unsupported languages fall back to generic AST walk; missing grammars skip those files entirely.
