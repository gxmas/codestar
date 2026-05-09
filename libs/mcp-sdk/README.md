# MCP SDK for Haskell

A Haskell implementation of the [Model Context Protocol](https://modelcontextprotocol.io/) (MCP), providing type-safe building blocks for MCP servers, clients, and hosts.

## Architecture

The SDK is organised as a multi-package Cabal project with strict layering:

```
mcp-host          Multi-client orchestration & security policy
mcp-server        Server features: tools, resources, prompts, logging, completion
mcp-client        Client features: sampling, roots, elicitation
mcp-tasks         Long-running task tracking with TTL expiration
mcp-session       Session lifecycle, request dispatch, capability negotiation
mcp-transport     Transport typeclass (send/receive/close)
  mcp-transport-stdio   Stdio transport
  mcp-transport-http    HTTP + SSE transport
mcp-core          Core types, JSON-RPC envelopes, content blocks, codec
```

## Packages

| Package | Description |
|---------|-------------|
| **mcp-core** | Core MCP types (messages, content blocks, capabilities) and JSON codec |
| **mcp-transport** | `Transport` typeclass abstracting message send/receive over streams |
| **mcp-transport-stdio** | Stdio-based transport implementation |
| **mcp-transport-http** | HTTP + Server-Sent Events transport implementation |
| **mcp-session** | Session record with request/notification dispatch and capability negotiation |
| **mcp-server** | Server-side feature modules: tools, resources, prompts, logging, completion |
| **mcp-client** | Client-side feature modules: sampling, roots, elicitation |
| **mcp-tasks** | Task store with status tracking, TTL expiration, and session integration |
| **mcp-host** | Multi-client host with security policies and client lifecycle management |

## Features

### Server

- **Tools** -- Register tools with JSON Schema parameters, annotations (read-only, destructive, idempotent), and dynamic registration/unregistration. Handles `tools/list` and `tools/call`.
- **Resources** -- Expose URI-addressable resources and templates with subscription support. Handles `resources/list`, `resources/read`, `resources/subscribe`.
- **Prompts** -- Register parameterised prompt templates. Handles `prompts/list` and `prompts/get`.
- **Logging** -- Syslog-level logging with `logging/setLevel` support.
- **Completion** -- Argument auto-completion for prompts and resources via `completion/complete`.

### Client

- **Sampling** -- Respond to server-initiated LLM sampling requests with model preferences, tool use, and stop reasons.
- **Roots** -- Expose filesystem roots to the server with change notifications.
- **Elicitation** -- Structured user input collection via forms with typed schemas.

### Host

- Multi-client management with connect/disconnect lifecycle hooks.
- Security policy enforcement via predicates.
- Feature wiring for sampling, roots, and elicitation across clients.

### Tasks

- Track long-running operations with status (Working, InputRequired, Completed, Failed, Cancelled).
- STM-backed task store with configurable TTL expiration.

## Building

Requires GHC 9.4+ (GHC2021).

```bash
cabal build all
```

## Testing

Each package includes HSpec + QuickCheck test suites:

```bash
cabal test all
```

## Key Design Decisions

- **Records over typeclasses** for feature modules -- enables straightforward composition without orphan instances.
- **STM-based concurrency** throughout -- all mutable state uses `TVar`/`TMVar` for composable atomicity.
- **Streaming transport** via the `streaming` library for backpressure-aware message consumption.
- **Layered error types** -- `TransportError`, `CodecError`, `SessionError`, and `RPCError` each cover a distinct failure domain.
- **Attach/detach lifecycle** -- features bind to sessions and clean up on close.

## License

MIT
