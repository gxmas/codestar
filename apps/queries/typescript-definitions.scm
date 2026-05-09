; TypeScript declaration-name captures for RepoMap.
; Focus on declaration heads, avoid local variable references.

; functions
(function_declaration
  name: (identifier) @def.function.name)
(generator_function_declaration
  name: (identifier) @def.function.name)

; classes and methods
(class_declaration
  name: (type_identifier) @def.class.name)
(abstract_class_declaration
  name: (type_identifier) @def.class.name)
(method_definition
  name: (property_identifier) @def.method.name)
(method_definition
  name: (private_property_identifier) @def.method.name)

; type-level declarations
(interface_declaration
  name: (type_identifier) @def.interface.name)
(type_alias_declaration
  name: (type_identifier) @def.type.name)
(enum_declaration
  name: (identifier) @def.enum.name)
(enum_declaration
  name: (type_identifier) @def.enum.name)

; namespaces/modules
(internal_module
  name: (identifier) @def.namespace.name)
