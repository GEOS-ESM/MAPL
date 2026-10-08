## ADDED Requirements

### Requirement: GraphStateItem carries a sparse characteristics map
`GraphStateItem` SHALL contain a sparse map from a characteristic type tag
(`graph/state-item-characteristics`) to a `StateItemCharacteristic`. The
map SHALL contain entries only for characteristics meaningful for the
item's allocated kind and that have actually been established or
deferred — absence of a key SHALL be distinguished from presence with
invalid status; a reader MUST NOT treat the two as interchangeable.

#### Scenario: Map starts empty
- **WHEN** a `GraphStateItem` is newly created with no characteristics
  established
- **THEN** its characteristics map contains no entries

#### Scenario: Established characteristic is retrievable by its type tag
- **WHEN** a characteristic has been established for a `GraphStateItem`
  under a given type tag
- **THEN** querying the map for that type tag returns that
  characteristic, and a type tag never established remains absent from
  the map rather than returning an invalid-status placeholder

#### Scenario: A characteristic whose own kind does not match the map key is rejected
- **WHEN** an attempt is made to establish a characteristic under a type
  tag that does not match that characteristic's own kind
- **THEN** the system rejects the attempt explicitly and the map is left
  unchanged, rather than storing the mismatched pairing

### Requirement: GraphStateItem exposes its characteristic-ordering strategy
`GraphStateItem` SHALL expose a method returning the order in which
mismatching characteristics' reconciling Transforms should be chained,
for use by an extension-chain-building algorithm
(`graph/state-item-characteristics`). This ordering SHALL vary by the
item's own kind.

#### Scenario: Ordering query reflects the item's own kind
- **WHEN** the ordering method is queried on two `GraphStateItem`s of
  different kinds, each with the same two mismatched characteristics
- **THEN** each item's own kind-specific order is returned, independent
  of the other item's kind
