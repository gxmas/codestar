#!/usr/bin/env bash
set -euo pipefail

# Regenerate Haskell proto-lens types from opentelemetry-proto definitions.
# Requires: protoc, proto-lens-protoc on PATH.
#
# Usage: ./scripts/gen-proto.sh

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PROTO_DIR="$ROOT_DIR/proto/opentelemetry-proto"
OUT_DIR="$ROOT_DIR/otel-proto/src"

if ! command -v protoc &>/dev/null; then
  echo "Error: protoc not found on PATH" >&2
  exit 1
fi

if ! command -v proto-lens-protoc &>/dev/null; then
  echo "Error: proto-lens-protoc not found on PATH" >&2
  echo "Install with: cabal install proto-lens-protoc" >&2
  exit 1
fi

echo "Cleaning generated files..."
rm -rf "$OUT_DIR/Proto"

echo "Generating Haskell proto types..."
protoc \
  --plugin=protoc-gen-haskell="$(which proto-lens-protoc)" \
  --haskell_out="$OUT_DIR" \
  -I "$PROTO_DIR" \
  "$PROTO_DIR/opentelemetry/proto/common/v1/common.proto" \
  "$PROTO_DIR/opentelemetry/proto/resource/v1/resource.proto" \
  "$PROTO_DIR/opentelemetry/proto/trace/v1/trace.proto" \
  "$PROTO_DIR/opentelemetry/proto/metrics/v1/metrics.proto" \
  "$PROTO_DIR/opentelemetry/proto/logs/v1/logs.proto" \
  "$PROTO_DIR/opentelemetry/proto/collector/trace/v1/trace_service.proto" \
  "$PROTO_DIR/opentelemetry/proto/collector/metrics/v1/metrics_service.proto" \
  "$PROTO_DIR/opentelemetry/proto/collector/logs/v1/logs_service.proto"

echo "Generated modules:"
find "$OUT_DIR/Proto" -name "*.hs" | sort
echo ""
echo "Done. Remember to update otel-proto.cabal exposed-modules if new files were added."
