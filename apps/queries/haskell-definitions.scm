; Haskell declaration-name captures for RepoMap.
; Node types verified against tree-sitter-haskell v0.23.1 AST output.

; value/function declarations — name node is always `variable`
(function (variable) @def.function.name)
(bind (variable) @def.bind.name)
(signature (variable) @def.signature.name)

; type-level declarations — name node is `name`
(data_type (name) @def.type.name)
(newtype (name) @def.type.name)
(type_synomym (name) @def.type.name)
(type_family (name) @def.type.name)

; class and instance declarations
(class (name) @def.class.name)
(instance (name) @def.instance.name)

; foreign imports — name lives inside a nested signature
(foreign_import (signature (variable) @def.foreign_import.name))

; Module declaration: capture all module_id components.
; Anchored inside `header` so it never fires on module paths inside imports.
(header (module (module_id) @def.module.name))

; Import references: all components of the imported module path.
; The last component (e.g. "Graph" from "import CodeStar.RepoMap.Graph")
; matches the definition tag emitted by the module's own header pattern.
(import (module (module_id) @ref.import.module))
