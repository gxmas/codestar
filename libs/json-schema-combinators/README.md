# json-schema-combinators

[![CI](https://github.com/gnoel5/json-schema-combinators/actions/workflows/ci.yml/badge.svg)](https://github.com/gnoel5/json-schema-combinators/actions/workflows/ci.yml)

Type-safe combinators for constructing [JSON Schema (draft 2020-12)](https://json-schema.org/specification) documents in Haskell. Schemas are built by starting with a base constructor and applying modifiers with `(&)` for left-to-right composition.

## Install

Add to your `.cabal` file:

```cabal
build-depends: json-schema-combinators
```

## Quick start

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Function ((&))
import Data.JsonSchema

userSchema :: Schema
userSchema = objectSchema
  [ required "name" $ stringSchema
      & withTitle "User name"
      & withMinLength 1
  , required "age"  $ integerSchema
      & withMinimum 0
  , optional "email" $ stringSchema
      & withFormat "email"
  ]
```

## More examples

### Nested objects and arrays

```haskell
teamSchema :: Schema
teamSchema = objectSchema
  [ required "teamName" stringSchema
  , required "members" $ arraySchema userSchema
      & withMinItems 1
      & withUniqueItems True
  , optional "metadata" $ objectSchema
      [ optional "region" stringSchema
      , optional "active" booleanSchema
      ]
  ]
```

### Nullable types

```haskell
-- { "type": ["string", "null"] }
nullableString :: Schema
nullableString = nullable stringSchema
```

### Schema reuse with `$defs` and `$ref`

```haskell
apiSchema :: Schema
apiSchema = objectSchema
  [ required "user" (ref "#/$defs/user") ]
  & withDefs
      [ ("user", objectSchema
          [ required "id" integerSchema
          , required "name" stringSchema
          ])
      ]
```

### Composition

```haskell
petSchema :: Schema
petSchema = oneOf
  [ objectSchema [required "bark" booleanSchema]
  , objectSchema [required "meow" booleanSchema]
  ]
```

### Conditional schemas

```haskell
conditionalSchema :: Schema
conditionalSchema = ifThenElse
  (objectSchema [required "kind" $ constSchema "dog"])
  (objectSchema [required "bark" booleanSchema])
  (objectSchema [required "meow" booleanSchema])
```

## Serialization

Convert to/from aeson `Value` with `encode` and `decode`:

```haskell
import qualified Data.Aeson as Aeson

json :: Value
json = encode userSchema

roundTrip :: Either DecodeError Schema
roundTrip = decode json
```

`ToJSON` and `FromJSON` instances are also provided, so `Aeson.encode` and `Aeson.decode` work out of the box.

## API summary

### Constructors

| Function | Produces |
|---|---|
| `stringSchema` | `{ "type": "string" }` |
| `numberSchema` | `{ "type": "number" }` |
| `integerSchema` | `{ "type": "integer" }` |
| `booleanSchema` | `{ "type": "boolean" }` |
| `nullSchema` | `{ "type": "null" }` |
| `objectSchema [Property]` | `{ "type": "object", "properties": {...} }` |
| `arraySchema schema` | `{ "type": "array", "items": {...} }` |
| `enumSchema [Value]` | `{ "enum": [...] }` |
| `constSchema value` | `{ "const": ... }` |
| `nullable schema` | Adds `"null"` to the type |
| `prefixItems [Schema]` | Tuple validation via `prefixItems` |

### Composition

`allOf`, `anyOf`, `oneOf`, `not`, `ref`, `ifThenElse`

### Modifiers

All modifiers have the signature `arg -> Schema -> Schema`, designed for use with `(&)`.

| Category | Modifiers |
|---|---|
| **Numeric** | `withMinimum`, `withMaximum`, `withExclusiveMinimum`, `withExclusiveMaximum`, `withMultipleOf` |
| **String** | `withMinLength`, `withMaxLength`, `withPattern`, `withFormat` |
| **Array** | `withMinItems`, `withMaxItems`, `withUniqueItems`, `withContains`, `withMinContains`, `withMaxContains`, `withUnevaluatedItems` |
| **Object** | `withAdditionalProperties`, `withPatternProperties`, `withPropertyNames`, `withMinProperties`, `withMaxProperties`, `withDependentSchemas`, `withDependentRequired`, `withUnevaluatedProperties` |
| **Annotation** | `withTitle`, `withDescription`, `withDefault`, `withExamples`, `withDeprecated`, `withReadOnly`, `withWriteOnly`, `withComment` |
| **Identity** | `withId`, `withSchema`, `withAnchor`, `withDynamicAnchor`, `withDynamicRef`, `withDefs`, `withVocabulary` |
| **Content** | `withContentEncoding`, `withContentMediaType`, `withContentSchema` |
| **Validation** | `withConst` |

## Modules

| Module | Description |
|---|---|
| `Data.JsonSchema` | Top-level re-export module (start here) |
| `Data.JsonSchema.Combinators` | All constructors and modifiers |
| `Data.JsonSchema.Schema` | `Schema`, `SchemaType`, `TypeSpec`, `emptySchema` (abstract) |
| `Data.JsonSchema.Schema.Internal` | Full `Schema` record with field accessors (escape hatch) |
| `Data.JsonSchema.Serialization` | `encode`, `decode`, `DecodeError`, `ToJSON`/`FromJSON` instances |
| `Data.JsonSchema.OrderedMap` | Insertion-order-preserving associative container |

## GHC compatibility

Tested with GHC 9.6.7, 9.8.4, and 9.10.1.

## License

MIT -- see [LICENSE](LICENSE).
