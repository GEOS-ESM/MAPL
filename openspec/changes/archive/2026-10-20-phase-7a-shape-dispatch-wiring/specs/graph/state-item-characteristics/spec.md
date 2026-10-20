## ADDED Requirements

### Requirement: A shape-dispatch characteristic bridges a bracket export into the plain field shape an import requires
The known set of concrete `StateItemCharacteristic` subclasses SHALL
also include an item-class/shape axis, comparing each side's own
declared item class (field, vector, bracket, or vector-bracket) rather
than a value. It SHALL be a value characteristic, consistent with the
existing value/reference branching requirement (`graph/state-item-
characteristics`'s own "StateItemCharacteristic is abstract and
branches into value and reference kinds" requirement) — it holds its
own declared item class inline, with no identity or sharing beyond its
owning state item. Introducing it SHALL NOT require any change to the
sparse characteristics map's structure or the mismatch-detection
algorithm's iteration logic, per the existing "a new subclass needs no
structural change elsewhere" requirement.

#### Scenario: Shape-dispatch characteristic is a value characteristic
- **WHEN** a shape-dispatch characteristic is constructed
- **THEN** it is an instance of the value kind, carries its own
  declared item class inline, and has no identity shared with any
  other `GraphStateItem`'s characteristic

#### Scenario: Matching item classes report no mismatch
- **WHEN** an export's and a connected import's shape-dispatch
  characteristics both declare the same item class (e.g. both field,
  or both vector)
- **THEN** the shape-dispatch characteristic reports no mismatch for
  that pair

#### Scenario: Bracket export against field import reports a mismatch
- **WHEN** an export's shape-dispatch characteristic declares the
  bracket item class and a connected import's declares the plain field
  item class
- **THEN** the shape-dispatch characteristic reports a mismatch for
  that pair

#### Scenario: Vector-bracket export against vector import reports a mismatch
- **WHEN** an export's shape-dispatch characteristic declares the
  vector-bracket item class and a connected import's declares the
  plain vector item class
- **THEN** the shape-dispatch characteristic reports a mismatch for
  that pair — building a Transform for it fails explicitly (see below),
  since real vector-bracket-to-vector conversion is out of this
  change's scope

#### Scenario: Each pairing other than bracket-to-field fails explicitly
- **WHEN** an export's and import's shape-dispatch characteristics
  mismatch on a pairing other than bracket-to-field (for example,
  vector-bracket-to-vector, field-to-vector, or any pairing involving a
  state-typed item)
- **THEN** building a Transform for that pairing fails explicitly,
  consistent with the existing "an axis with no real provider fails
  explicitly rather than returning a stub" requirement

### Requirement: The shape-dispatch characteristic's Transform performs real time interpolation, not a shape-preserving copy
Building a `Transform` for a bracket-to-field shape mismatch SHALL
succeed and SHALL return a `Transform` that performs genuine time
interpolation between the bracket's held time levels into the field's
single-time-level shape — consistent with the existing "a units
mismatch builds a real converting Transform"/"a type-kind mismatch
builds a real converting Transform" precedent for other axes with a
real provider.

#### Scenario: A bracket-to-field Transform interpolates between bracket members
- **WHEN** a Transform built for a bracket-to-field shape mismatch is
  executed against a bracket holding two or more time-level members
- **THEN** the resulting field's values are a genuine time
  interpolation of the bracket's own held members, not a copy of a
  single member and not left unset
