# Changelog

## 0.1.0.0 -- 2026-04-05

Initial release.

- Full JSON Schema draft 2020-12 keyword coverage (~45 keywords)
- Combinator API with `(&)` composition: primitive, composite, composition,
  conditional, and reference constructors plus constraint, annotation,
  identity, and content modifiers
- Lossless round-trip JSON serialization (`encode`/`decode`) with structured
  `DecodeError` including JSON Pointer paths
- `ToJSON`/`FromJSON` instances for seamless aeson integration
- `OrderedMap` for insertion-order-preserving properties, `$defs`,
  `patternProperties`, `dependentSchemas`, `dependentRequired`, and
  `$vocabulary`
- Property-based and unit tests (161 examples)
