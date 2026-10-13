## ADDED Requirements

### Requirement: A characteristic can build a Transform resolving a mismatch, or must fail explicitly
A concrete `StateItemCharacteristic` subclass SHALL support building a
`Transform` that resolves a detected mismatch between its own
established value and a goal of the same concrete subclass, given only
the two characteristic values themselves — with no graph, node-identity,
or `ComponentGraph` concept involved in that construction. A subclass
with no real conversion implemented yet SHALL fail this operation
explicitly rather than returning a no-op or stub `Transform`.

#### Scenario: A units mismatch builds a real converting Transform
- **WHEN** a physical-units characteristic's established value is found
  mismatched against a goal physical-units characteristic, and a real
  conversion between the two is implemented
- **THEN** building a `Transform` for that pair succeeds and returns a
  `Transform` that performs the corresponding unit conversion

#### Scenario: A type-kind mismatch builds a real converting Transform
- **WHEN** a type-kind characteristic's established value is found
  mismatched against a goal type-kind characteristic, and a real
  conversion between the two is implemented
- **THEN** building a `Transform` for that pair succeeds and returns a
  `Transform` that performs the corresponding precision cast

#### Scenario: An axis with no real provider fails explicitly rather than returning a stub
- **WHEN** building a `Transform` is attempted for a mismatch on a
  characteristic axis with no real conversion implemented yet
- **THEN** the operation fails explicitly, and no `Transform` — stub,
  no-op, or otherwise — is returned

#### Scenario: Building a Transform requires no graph or node-identity concept
- **WHEN** a `Transform` is built for a mismatched pair
- **THEN** the construction uses only the two characteristic values
  themselves, with no `ComponentGraph`, `NodeId`, or other graph
  -structural reference required as an input
