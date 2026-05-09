# Aider Language Tag Queries

This document summarizes what Aider extracts per language from its tree-sitter tag query files in:

- `aider/queries/tree-sitter-languages/*-tags.scm`
- Source: [Aider query directory](https://github.com/Aider-AI/aider/tree/main/aider/queries/tree-sitter-languages)

Notes:

- `defs` means definition capture categories.
- `refs` means reference capture categories.
- `other` lists non-definition/reference helper captures used in some queries.
- Snapshot based on Aider `main` as observed on 2026-05-09.

## Languages

- `c`
  - defs: `class`, `function`, `type`
  - refs: none
  - other: none

- `c_sharp`
  - defs: `class`, `interface`, `method`, `module`
  - refs: `class`, `interface`, `send`
  - other: none

- `cpp`
  - defs: `class`, `function`, `method`, `type`
  - refs: none
  - other: `scope`

- `dart`
  - defs: `class`, `enum`, `extension`, `function`, `method`, `mixin`, `type`
  - refs: `call`, `class`
  - other: `name`

- `elisp`
  - defs: `function`
  - refs: `function`
  - other: none

- `elixir`
  - defs: `function`, `module`
  - refs: `call`, `module`
  - other: `ignore`

- `elm`
  - defs: `function`, `module`, `type`, `union`
  - refs: `function`, `type`, `union`
  - other: none

- `fortran`
  - defs: `class`, `function`
  - refs: none
  - other: none

- `go`
  - defs: `function`, `method`, `type`
  - refs: `call`, `type`
  - other: `doc`

- `haskell`
  - defs: `function`, `type`
  - refs: none
  - other: none

- `hcl`
  - defs: `local`, `module`, `output`, `provider`, `resource`, `variable`
  - refs: `data`, `local`, `module`, `resource`, `variable`
  - other: `block_type`, `data_source_type`, `ref_type`, `resource_type`

- `java`
  - defs: `class`, `interface`, `method`
  - refs: `call`, `class`, `implementation`
  - other: none

- `javascript`
  - defs: `class`, `function`, `method`
  - refs: `call`, `class`
  - other: `doc`

- `julia`
  - defs: `class`, `constant`, `function`, `macro`, `method`, `module`
  - refs: `call`, `export`, `module`, `type`
  - other: none

- `kotlin`
  - defs: `class`, `function`, `object`
  - refs: `call`, `type`
  - other: none

- `matlab`
  - defs: `class`, `function`
  - refs: `call`
  - other: none

- `ocaml`
  - defs: `class`, `function`, `interface`, `method`, `module`
  - refs: `call`, `class`, `implementation`, `module`
  - other: `doc`

- `ocaml_interface`
  - defs: `class`, `enum_variant`, `field`, `function`, `interface`, `method`, `module`, `type`
  - refs: `call`, `class`, `enum_variant`, `field`, `implementation`, `module`, `type`
  - other: `doc`, `name`

- `php`
  - defs: `class`, `function`
  - refs: `call`, `class`
  - other: none

- `python`
  - defs: `class`, `function`
  - refs: `call`
  - other: none

- `ql`
  - defs: `class`, `function`, `method`, `module`
  - refs: `call`, `type`
  - other: none

- `ruby`
  - defs: `class`, `method`, `module`
  - refs: `call`
  - other: `doc`, `ignore`

- `rust`
  - defs: `class`, `function`, `interface`, `macro`, `method`, `module`
  - refs: `call`, `implementation`
  - other: none

- `scala`
  - defs: `class`, `enum`, `function`, `interface`, `module`, `object`, `property`, `type`, `variable`
  - refs: `call`, `class`, `interface`
  - other: none

- `typescript`
  - defs: `class`, `enum`, `function`, `interface`, `method`, `module`, `type`
  - refs: `class`, `type`
  - other: none

- `zig`
  - defs: `constant`, `function`, `variable`
  - refs: none
  - other: none
