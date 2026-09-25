## Purpose

Defines how an `expression` state item's vertical dim spec is resolved
directly from its referenced variables when not explicitly declared, and how
consistency between the expression's referenced inputs (and its own resolved
value, if any) is checked on a best-effort basis, so that declaring
`vertical_dim_spec` on the expression item itself is only required when it
references no variables to resolve from.

## ADDED Requirements

### Requirement: An expression's unspecified vertical dim spec resolves directly from its referenced variables
When an `expression` item's formula references one or more export state
items, and the `expression` item's own configuration does not declare a
`vertical_dim_spec`, the system SHALL treat the item's own vertical dim spec
as a candidate for resolution rather than a fixed, authoritative value, and
SHALL adopt the vertical stagger (and vertical grid, if the agreeing stagger
requires one) found among its referenced variables, as soon as at least one
of them is itself already resolved and none of the currently-resolved ones
disagree. This resolution is attempted both as early as possible (when the
item is constructed) and, if not yet possible then, again at the point the
expression's arithmetic wiring is constructed - by then, any later,
unrelated resolution of the item's vertical dim spec (for example a
component-level vertical grid resource becoming available) has already
happened, so this later attempt is authoritative.

#### Scenario: Expression resolves from referenced variables that already agree
- **WHEN** an `expression` item's formula references export items `A` and `B`,
  both of which have a resolved vertical stagger of `NONE`, and the
  `expression` item itself declares no `vertical_dim_spec`
- **THEN** component initialization succeeds
- **THEN** the `expression` item's own vertical dim spec resolves to `NONE`

#### Scenario: Explicitly declared vertical dim spec is unaffected
- **WHEN** an `expression` item's own configuration declares
  `vertical_dim_spec: NONE` explicitly
- **THEN** the item's vertical dim spec resolves to the explicitly declared
  value, exactly as before this capability existed

### Requirement: Consistency with referenced inputs is checked on a best-effort basis
When an `expression` item's formula references two or more export state items,
or references one or more export state items whose resolved vertical stagger
can be compared against the expression item's own resolved (or explicitly
declared) vertical stagger, the system SHALL check, both when the item is
constructed and again at the point the expression's arithmetic wiring is
constructed, whether the referenced items that already have a resolved
vertical stagger at that time agree with each other and with the expression
item's own resolved or declared vertical stagger. A referenced item whose
vertical stagger is not yet resolved at that time - including one not yet
present in the registry at all - SHALL be skipped for this check rather than
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
- **WHEN** an `expression` item's own vertical dim spec has resolved (from its
  referenced variables or explicit declaration) to `CENTER`, and a referenced
  export item has an already-resolved vertical stagger of `NONE` at the time
  the expression's arithmetic wiring is constructed
- **THEN** component initialization fails
- **THEN** the reported error identifies the conflict between the expression's
  resolved value and the referenced item's resolved value

#### Scenario: A referenced input that is not yet resolved does not block initialization
- **WHEN** an `expression` item's formula references an export item whose
  vertical stagger is not yet resolved - or not yet even registered - at the
  time the expression's arithmetic wiring is constructed
- **THEN** that referenced item is skipped for the consistency check
- **THEN** initialization does not fail solely because that item's vertical
  stagger was not yet resolved (or not yet registered) at that time

### Requirement: Expressions with no referenced state items are unaffected
When an `expression` item's formula references no export state items (for
example, a formula composed only of literal constants), the system SHALL NOT
attempt to resolve the item's vertical dim spec from anything, and SHALL NOT
perform the consistency check. The `expression` item's vertical dim spec
resolution follows the same rules that apply to it independent of this
capability, unchanged by this change.

#### Scenario: Constant expression with no declared vertical dim spec
- **WHEN** an `expression` item's formula references no export state items, and
  the `expression` item declares no `vertical_dim_spec`
- **THEN** this capability does not influence the outcome; behavior is exactly
  as it was before this capability was introduced
