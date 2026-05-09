# anthropic-sdk

Haskell library suite for the [Anthropic Messages API](https://docs.anthropic.com/en/api/messages).

## Packages

| Package | Description |
|---|---|
| `anthropic-types` | Shared types: content blocks, cache control, usage, pagination, errors |
| `anthropic-protocol` | Protocol layer: request/response shapes, streaming events, token counting, tool and batch schemas |
| `anthropic-client` | HTTP client: authentication, rate-limit headers, retry logic, streaming via SSE |
| `anthropic-server` | Server contract: SSE emitter, rate-limit validation, service-tier and batch handling |
| `anthropic-tools-common` | Pre-built tool definitions: file system, shell, network, and tool executor |

## Dependency order

```
anthropic-types
  └── anthropic-protocol
        ├── anthropic-client
        └── anthropic-server
              └── anthropic-tools-common
```

## Usage

Add the packages you need to your `build-depends`. Most consumers need `anthropic-client` for making API calls and `anthropic-types` for working with the response types.
