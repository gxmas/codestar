# json-rpc-sdk

Haskell implementation of [JSON-RPC 2.0](https://www.jsonrpc.org/specification), plus JSON Schema combinators used throughout the stack.

## Packages

| Package | Description |
|---|---|
| `json-schema-combinators` | Type-safe JSON Schema (draft 2020-12) combinators for describing and serialising schemas |
| `json-rpc-core` | Core types (`Request`, `Response`, `Error`, `Id`), codec, and transport typeclass |
| `json-rpc-client` | Client with request tracking and batch support |
| `json-rpc-server` | Server with pure method dispatch and applicative parameter parsing |
| `json-rpc` | Convenience facade re-exporting the full stack |

## Dependency order

```
json-schema-combinators
json-rpc-core
  ├── json-rpc-client
  └── json-rpc-server
        └── json-rpc  (facade)
```

## Usage

Most consumers should depend on `json-rpc` for the full stack, or on `json-rpc-client` / `json-rpc-server` individually when only one side is needed.
