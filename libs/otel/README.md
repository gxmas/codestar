# otel-haskell

A spec-compliant OpenTelemetry SDK for Haskell.

## Packages

| Package | Purpose |
|---------|---------|
| `otel-api` | OpenTelemetry API — traces, metrics, logs, propagation |
| `otel-sdk-traces` | SDK trace implementation with samplers and processors |
| `otel-sdk-metrics` | SDK metrics implementation with aggregation and views |
| `otel-sdk-logs` | SDK logging implementation |
| `otel-sdk-config` | File-based SDK configuration (YAML) |
| `otel-semconv` | Semantic conventions (v1.27.0) |
| `otel-exporter-otlp-grpc` | OTLP/gRPC exporter for all signals |
| `otel-exporter-otlp-http` | OTLP/HTTP exporter (protobuf and JSON) |
| `otel-exporter-prometheus` | Prometheus metrics exporter |
| `otel-exporter-console` | Console exporter for debugging |
| `otel-exporter-inmemory` | In-memory exporter for testing |

## Quickstart

```haskell
import OTel.Context (root)
import OTel.Trace (Span (..), Tracer (..), TracerProvider (..), end, defaultSpanConfig)
import OTel.SDK.Trace
import OTel.SDK.Trace.Export (SomeSpanExporter (..))
import OTel.SDK.Trace.Processor
import OTel.Exporter.Console
import OTel.Attribute (InstrumentationScope (..))

main :: IO ()
main = do
  exporter <- newConsoleSpanExporter
  proc     <- newSimpleSpanProcessor (SomeSpanExporter exporter)
  provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerProcessors = [SomeSpanProcessor proc] }
  tracer <- getTracer provider (InstrumentationScope "my-app" (Just "1.0.0") Nothing Nothing)
  span1  <- startSpan tracer "my-operation" root defaultSpanConfig
  -- ... do work ...
  end span1 Nothing
  _ <- shutdown provider
  pure ()
```

## Requirements

- GHC 9.8+
- Cabal 3.10+

## License

BSD-3-Clause
