# generic/field-name-propagation Specification

## Purpose

Defines how the `standard_name` and `long_name` descriptive metadata of a coupled
`ESMF_Field` state item is retained and resolved independently at each connection
endpoint (Export, Import, and any intermediate transform), instead of being
collapsed to a single field-wide value.

## Requirements

### Requirement: Each connection endpoint retains its own declared field name metadata
When a component declares a `standard_name` and/or `long_name` for one of its own
Field state items (import, export, or internal), that declared value SHALL be
retrievable from that specific state item's connection point, independent of any
`standard_name`/`long_name` declared by other components sharing the same underlying
field through a connection.

#### Scenario: Export and connected Import each declare their own name
- **WHEN** an Export field `E` with `standard_name: "E name"` is connected to an
  Import field `I` with `standard_name: "I name"` on a different component, and both
  fields reach allocation-complete status
- **THEN** reading `standard_name` from `E`'s own state returns `"E name"`
- **THEN** reading `standard_name` from `I`'s own state returns `"I name"`

#### Scenario: Same behavior applies to long_name
- **WHEN** an Export field `E` with `long_name: "E long name"` is connected to an
  Import field `I` with `long_name: "I long name"`, and both fields reach
  allocation-complete status
- **THEN** reading `long_name` from `E`'s own state returns `"E long name"`
- **THEN** reading `long_name` from `I`'s own state returns `"I long name"`

### Requirement: Unassigned metadata is inherited from the connection predecessor
When a connection endpoint does not declare its own `standard_name` and/or
`long_name`, that endpoint SHALL report the value declared by its connection
predecessor (the Export/source it connects to, or, in a chain of transform hops, the
prior hop) instead of an empty or generic placeholder value. Propagation SHALL be
one-directional: a predecessor's own declared value SHALL NOT be altered by a
downstream endpoint's declaration or lack thereof.

#### Scenario: Import does not declare a name
- **WHEN** an Export field `E` with `standard_name: "E name"` is connected to an
  Import field `I` that declares no `standard_name`, and both fields reach
  allocation-complete status
- **THEN** reading `standard_name` from `I`'s own state returns `"E name"`

#### Scenario: Export's own declared name is unaffected by a downstream Import
- **WHEN** an Export field `E` with `standard_name: "E name"` is connected to an
  Import field `I` with `standard_name: "I name"`
- **THEN** reading `standard_name` from `E`'s own state still returns `"E name"`
  (unchanged by the Import's declaration)

#### Scenario: Name propagates through an intermediate transform
- **WHEN** an Export field `E` with `standard_name: "E name"` is connected to an
  Import field `I` that requires a unit-conversion or regrid transform, and the
  transform's intermediate field declares no `standard_name` of its own
- **THEN** reading `standard_name` from the transformed field at `I`'s connection
  point returns `"E name"`

#### Scenario: No value assigned anywhere in the chain
- **WHEN** neither an Export field `E` nor a connected Import field `I` declares a
  `standard_name` or `long_name` anywhere along the connection chain
- **THEN** reading `standard_name` or `long_name` from either `E`'s or `I`'s state
  returns a well-defined fallback value (`"unknown"`) rather than an error

### Requirement: Field name metadata is only defined once a field is allocation-complete
`standard_name`/`long_name` metadata SHALL only be considered defined for a Field
state item once that item has reached `ESMF_FIELDSTATUS_COMPLETE`. Querying this
metadata on a field that is not yet complete (for example, an unconnected import
that remains merely grid-set, or an export with no downstream consumer) is not a
supported operation and SHALL NOT be relied upon by callers.

#### Scenario: Querying an unconnected, incomplete import
- **WHEN** a component declares an import that is never satisfied by any connection
  and therefore never reaches `ESMF_FIELDSTATUS_COMPLETE`
- **THEN** the system does not guarantee any particular `standard_name`/`long_name`
  value for that field, and test/verification code SHALL only assert on this
  metadata for fields confirmed to be `ESMF_FIELDSTATUS_COMPLETE`

### Requirement: Expression-derived exports retain their own declared name metadata
When a component declares a `standard_name` and/or `long_name` for an export state item
whose value is computed from an `expression` (rather than being a plain Field), that
declared value SHALL survive the item being connected to a consumer - whether via an
explicit connection declaration or an implicit same-name match - and SHALL be retrievable
from the consumer's end of that connection once the item reaches allocation-complete
status. It SHALL NOT be silently discarded in favor of a nameless value supplied by the
consumer.

#### Scenario: Expression export consumed via an implicit same-name connection
- **WHEN** a component declares an export `E` computed from an expression (for example
  `E: {expression: "A+B", standard_name: "foo", long_name: "bar", ...}`), and a sibling
  component (for example a History output collection) references `E` by name without
  declaring its own `standard_name`/`long_name`, and `E` reaches allocation-complete status
- **THEN** reading `standard_name` from `E`'s connection point at the consumer returns
  `"foo"`
- **THEN** reading `long_name` from `E`'s connection point at the consumer returns `"bar"`

#### Scenario: Expression export consumed via an explicit connection
- **WHEN** a component declares an export `E` computed from an expression with its own
  `standard_name`/`long_name`, and a `connections:` entry explicitly wires `E` to an import
  on another component that declares no `standard_name`/`long_name` of its own
- **THEN** reading `standard_name`/`long_name` from the import's connection point returns
  the values declared on `E`
