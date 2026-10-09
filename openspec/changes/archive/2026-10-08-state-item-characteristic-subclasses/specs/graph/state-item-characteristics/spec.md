## ADDED Requirements

### Requirement: Known concrete characteristic subclasses are not limited to units, type-kind, and geometry
The set of known concrete `StateItemCharacteristic` subclasses SHALL also
include a vertical-grid axis, an attributes axis, an ungridded-dimensions
axis, a quantity-type axis, a conservation axis, a normalization axis, and a
standard-name axis. Each SHALL extend exactly one of the value or reference
kinds, consistent with the existing value/reference branching requirement,
and each SHALL be classified correctly: the vertical-grid axis as a
reference characteristic (it points at a shared graph node, the same way
the geometry axis does); the remaining six as value characteristics (each
holds its own value inline, with no identity or sharing beyond its owning
state item). Introducing these SHALL NOT require any change to the sparse
characteristics map's structure or the mismatch-detection algorithm's
iteration logic, per the existing "a new subclass needs no structural
change elsewhere" requirement.

#### Scenario: A vertical-grid characteristic is a reference characteristic
- **WHEN** a vertical-grid characteristic is constructed
- **THEN** it is an instance of the reference kind and holds a reference to
  an ordinary graph node with its own identity, rather than holding a
  vertical-grid value inline

#### Scenario: Two state items share one vertical-grid reference
- **WHEN** two `GraphStateItem`s both declare a vertical-grid characteristic
  referencing the same underlying node
- **THEN** both characteristics resolve to the same referenced node, using
  the same sharing mechanism already established for reference
  characteristics — no new identity mechanism is required

#### Scenario: Attributes, ungridded-dimensions, quantity-type, conservation, normalization, and standard-name characteristics are value characteristics
- **WHEN** an attributes, ungridded-dimensions, quantity-type, conservation,
  normalization, or standard-name characteristic is constructed
- **THEN** each is an instance of the value kind, carries its own value
  inline, and has no identity shared with any other `GraphStateItem`'s
  characteristic

#### Scenario: Each new axis has its own stable type tag
- **WHEN** the type tag for any of the seven new concrete subclasses is
  queried
- **THEN** it returns the same value every time, and that value is distinct
  from every other registered subclass's type tag, including the three
  already known before this capability's own extension
