# Prerequisites

System tools required to build the otel-haskell project.

## Required

| Tool | Minimum Version | Install |
|------|----------------|---------|
| GHC | 9.10.x | [ghcup](https://www.haskell.org/ghcup/) |
| Cabal | 3.14.x | [ghcup](https://www.haskell.org/ghcup/) |
| protoc | 3.x / 25.x+ | `brew install protobuf` (macOS) or [releases](https://github.com/protocolbuffers/protobuf/releases) |
| proto-lens-protoc | latest | `cabal install proto-lens-protoc` |

## Verify Installation

```bash
ghc --version          # should show 9.10.x
cabal --version        # should show 3.14.x
protoc --version       # should show libprotoc 25.x or later
proto-lens-protoc --version  # should be on PATH after cabal install
```

## Build

```bash
cabal update
cabal build all
cabal test all
```
