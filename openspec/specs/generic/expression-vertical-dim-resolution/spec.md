# generic/expression-vertical-dim-resolution Specification

## Purpose

Defines how an `expression` state item's vertical dim spec is resolved via
connection mirroring when not explicitly declared, and how consistency between
the expression's referenced inputs (and its own resolved value) is checked on
a best-effort basis, so that declaring `vertical_dim_spec` on the expression
item itself is only required when it references no variables to potentially
mirror through a connection.

## Requirements

### Requirement: An expression's unspecified vertical dim spec resolves via connection mirroring
When an `expression` item's formula references one or more export state items,
and the `expression` item's own configuration does not declare a
`vertical_dim_spec`, the system SHALL treat the item's vertical dim spec as
unresolved (mirrored) rather than falsely resolved, so that connecting the
expression item to another state item resolves its vertical dim spec (stagger,
vertical grid, and alignment) from that connection.

#### Scenario: Expression connects to an import with vertical dim spec NONE
- **WHEN** an `expression` item's formula references export items `A`, `B`, and
  `C`, the `expression` item itself declares no `vertical_dim_spec`, and the
  `expression` item is connected to an import that declares
  `vertical_dim_spec: NONE`
- **THEN** component initialization succeeds
- **THEN** the `expression` item's resolved vertical dim spec is `NONE`

#### Scenario: Explicitly declared vertical dim spec is unaffected
- **WHEN** an `expression` item's own configuration declares
  `vertical_dim_spec: NONE` explicitly
- **THEN** the item's vertical dim spec resolves to the explicitly declared
  value, exactly as before this capability existed

### Requirement: Consistency with referenced inputs is checked on a best-effort basis
When an `expression` item's formula references two or more export state items,
or references one or more export state items whose resolved vertical stagger
can be compared against the expression item's own resolved vertical stagger,
the system SHALL check, at the point the expression's arithmetic wiring is
constructed, whether the referenced items that already have a resolved
vertical stagger at that time agree with each other and with the expression
item's own resolved vertical stagger. A referenced item whose vertical stagger
is not yet resolved at that time SHALL be skipped for this check rather than
treated as an error.

#### Scenario: Mismatched, already-resolved inputs are rejected with a clear error
- **WHEN** an `expression` item's formula references export item `A` with a
  resolved vertical stagger of `NONE` and export item `D` with a resolved
  vertical stagger of `CENTER`, both already resolved at the time the
  expression's arithmetic wiring is constructed
- **THEN** component initialization fails
- **THEN** the reported error identifies both `A` and `D` and their conflicting
  vertical staggers

#### Scenario: A referenced input that disagrees with the expression's own resolved value is rejected
- **WHEN** an `expression` item's own vertical dim spec has resolved (via
  mirroring or explicit declaration) to `CENTER`, and a referenced export item
  has an already-resolved vertical stagger of `NONE` at the time the
  expression's arithmetic wiring is constructed
- **THEN** component initialization fails
- **THEN** the reported error identifies the conflict between the expression's
  resolved value and the referenced item's resolved value

#### Scenario: A referenced input that is not yet resolved does not block initialization
- **WHEN** an `expression` item's formula references an export item whose
  vertical stagger is not yet resolved at the time the expression's arithmetic
  wiring is constructed
- **THEN** that referenced item is skipped for the consistency check
- **THEN** initialization does not fail solely because that item's vertical
  stagger was not yet resolved at that time

### Requirement: Expressions with no referenced state items are unaffected
When an `expression` item's formula references no export state items (for
example, a formula composed only of literal constants), the system SHALL NOT
force the item's vertical dim spec to an unresolved (mirrored) state, and
SHALL NOT perform the consistency check. The `expression` item's vertical dim
spec resolution follows the same rules that apply to it independent of this
capability, unchanged by this change.

#### Scenario: Constant expression with no declared vertical dim spec
- **WHEN** an `expression` item's formula references no export state items, and
  the `expression` item declares no `vertical_dim_spec`
- **THEN** this capability does not influence the outcome; behavior is exactly
  as it was before this capability was introduced
