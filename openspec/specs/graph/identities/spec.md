# Identities Specification

## Purpose

Provides encapsulated, generated identity types (`NodeId` and siblings)
used as authoritative map keys throughout the graph-neutral core, so no
client code can construct or forge an identity from a raw integer.

## Requirements

### Requirement: NodeId encapsulation
`NodeId` SHALL be an encapsulated derived type wrapping a private integer
value. No public API SHALL allow construction of a `NodeId` from a raw
integer, nor allow inspection of its raw integer value.

#### Scenario: No raw-integer construction path
- **WHEN** client code attempts to construct a `NodeId` other than via
  `NodeIdGenerator%next()` or the distinguished `INVALID_NodeId` /
  `DEFAULT_...` parameters
- **THEN** no such public constructor, structure constructor, or
  accessor exists

### Requirement: NodeId generation is fresh and non-repeating
`NodeIdGenerator%next()` SHALL return a `NodeId` distinct from every
`NodeId` previously returned by the same generator instance within its
lifetime.

#### Scenario: Sequential distinct ids
- **WHEN** `next()` is called N times on one generator
- **THEN** all N returned `NodeId` values are pairwise distinct (`/=`)

### Requirement: NodeId exhaustion is detected, not silently wrapped
When a `NodeIdGenerator` cannot produce a fresh id because its internal
counter would overflow, it SHALL report exhaustion via an optional
`status` argument rather than wrapping around and silently reusing or
duplicating a prior value.

#### Scenario: Exhaustion reported
- **WHEN** `next()` is called on a generator whose internal counter is at
  its maximum representable value
- **THEN** `next()` returns `INVALID_NodeId` and, if `status` is present,
  sets it to a nonzero value

#### Scenario: No silent wraparound
- **WHEN** a generator is at exhaustion
- **THEN** `next()` does not return a `NodeId` equal to any previously
  issued value

### Requirement: Sibling ID types share the same contract
`DependencyNetworkId`, `PortId`, and `CallbackInterfaceId` SHALL each
provide the same encapsulation, generation, exhaustion-detection,
comparison (`==`, `/=`, `<`), `is_valid()`, and `to_string()` contract as
`NodeId`, generated from one shared template rather than hand-written
per type.

#### Scenario: Sibling type contract parity
- **WHEN** any of `DependencyNetworkId`, `PortId`, `CallbackInterfaceId`
  is exercised through its generator, comparison operators, `is_valid()`,
  and `to_string()`
- **THEN** behavior matches the `NodeId` scenarios above with no
  type-specific deviation

### Requirement: Distinguished invalid value
Each ID type SHALL provide a distinguished `INVALID_<TypeName>` value for
which `is_valid()` returns `.false.`, distinct from any value returned by
`next()`.

#### Scenario: Invalid value is invalid
- **WHEN** `is_valid()` is called on `INVALID_NodeId` (or the sibling
  types' equivalents)
- **THEN** it returns `.false.`

#### Scenario: Generated values are valid
- **WHEN** `is_valid()` is called on any `NodeId` returned by
  `next()` before exhaustion
- **THEN** it returns `.true.`

### Requirement: Default-valued instance where required
`DependencyNetworkId` SHALL additionally provide a distinguished default
value (distinct from `INVALID_DependencyNetworkId`) for use as
`ComponentGraph`'s default network id.

#### Scenario: Default id is valid and distinct
- **WHEN** the default `DependencyNetworkId` instance is compared against
  `INVALID_DependencyNetworkId`
- **THEN** they are unequal, and `is_valid()` on the default instance
  returns `.true.`

### Requirement: Ordering and equality operators
Each ID type SHALL provide `==`, `/=`, and `<` operators with the usual
consistency properties (irreflexive `<`, antisymmetric, transitive;
`==`/`/=` are complements).

#### Scenario: Operator consistency
- **WHEN** two distinct ids A and B produced by the same generator are
  compared
- **THEN** exactly one of `A < B` or `B < A` holds, and `A == B` is
  `.false.`

### Requirement: String representation
Each ID type SHALL provide `to_string()` returning a non-empty string for
any valid id and an empty string for an invalid id.

#### Scenario: Valid id string representation
- **WHEN** `to_string()` is called on a valid id
- **THEN** it returns a non-empty, deterministic string for that id's
  value

#### Scenario: Invalid id string representation
- **WHEN** `to_string()` is called on `INVALID_<TypeName>`
- **THEN** it returns an empty string

### Requirement: Map-key compatibility without box types
ID types SHALL be usable directly as keys in gFTL map containers holding
polymorphic mapped values, without an intermediate "Box" wrapper type
introduced solely to smuggle polymorphic values through non-polymorphic
containers.

#### Scenario: Direct polymorphic-valued map usage
- **WHEN** a gFTL map is instantiated with an ID type as key and a
  polymorphic type as mapped value
- **THEN** no Box/wrapper indirection is required for the mapped value
