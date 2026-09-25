## ADDED Requirements

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
