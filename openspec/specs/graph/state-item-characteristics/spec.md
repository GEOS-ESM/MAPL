# State Item Characteristics Specification

## Purpose

Gives each `GraphStateItem` a first-class, per-characteristic model of its
mismatch-relevant axes (units, type/kind, geometry, ...), so mismatch
detection, reconciling-Transform ordering, cross-item sharing, and
structural-vs-content change propagation are each handled uniformly through
one interface instead of ad hoc, per-axis logic.

## Requirements

### Requirement: StateItemCharacteristic is abstract and branches into value and reference kinds
`StateItemCharacteristic` SHALL be an abstract type. It SHALL branch into
two abstract intermediate kinds: a value kind, which holds its value
inline with no identity beyond its owning `GraphStateItem` and is never
shared, and a reference kind, which holds a reference to a shared node
elsewhere in the graph rather than a value of its own. A concrete
characteristic subclass SHALL extend exactly one of these two kinds, never
`StateItemCharacteristic` directly.

#### Scenario: A units or type-kind characteristic is a value characteristic
- **WHEN** a physical-units or type-kind characteristic is constructed
- **THEN** it is an instance of the value kind, carries its value inline,
  and has no identity shared with any other `GraphStateItem`'s
  characteristic

#### Scenario: A geometry characteristic is a reference characteristic
- **WHEN** a geometry characteristic is constructed
- **THEN** it is an instance of the reference kind and holds a reference
  to an ordinary graph node with its own identity, rather than holding a
  geometry value inline

#### Scenario: Reference characteristic references a node with real identity
- **WHEN** a reference characteristic's referenced node is inspected
- **THEN** that node has its own ordinary node identity and revision,
  distinct from the referencing `GraphStateItem`'s own identity

### Requirement: Every characteristic carries a status
A `StateItemCharacteristic` SHALL carry a status value from a fixed
enumeration with at least these meanings: no meaningful value yet
(invalid); fully resolved and authoritative (specified); not yet
resolved, will be made to match a source once connected (mirrored);
connection permitted without reconciliation despite a known or possible
mismatch (unchecked, an explicit, diagnostically-distinguished opt-out of
validation); and processing deferred to a later, known point (deferred,
distinct from invalid in that a reason and future resolution point are
both known).

#### Scenario: Newly constructed characteristic is invalid
- **WHEN** a `StateItemCharacteristic` is constructed with no value or
  reference yet established
- **THEN** its status reports the invalid value

#### Scenario: Unchecked status is distinguishable in diagnostics
- **WHEN** a characteristic's status is unchecked
- **THEN** any diagnostic reporting that status identifies it distinctly
  from specified or mirrored, never conflating an explicit opt-out with a
  confirmed or pending match

#### Scenario: Reference characteristic status transitions are reflected, not silent
- **WHEN** a reference characteristic's status changes away from
  specified as a result of its referenced node's value changing
- **THEN** the change is reflected through this capability's propagation
  mechanism (see the structural-reset requirement below), not left for a
  caller to separately discover

### Requirement: A stable type tag identifies each characteristic subclass
Each concrete `StateItemCharacteristic` subclass SHALL have exactly one
associated, stable type-tag value, assigned once and never reused or
renumbered for a different subclass. This type tag SHALL be the key type
for the sparse map on `GraphStateItem` (see `graph/state-item`).
Introducing a new concrete subclass SHALL require registering exactly one
new type-tag value and SHALL NOT require any change to
`GraphStateItem`'s structure or to the detection/ordering algorithms
below beyond that registration.

#### Scenario: Each concrete subclass has one stable type tag
- **WHEN** the type tag for a concrete `StateItemCharacteristic` subclass
  is queried
- **THEN** it returns the same value every time, and that value is
  distinct from every other registered subclass's type tag

#### Scenario: A new characteristic subclass needs no structural change elsewhere
- **WHEN** a new concrete `StateItemCharacteristic` subclass is introduced
  with its own new type-tag value
- **THEN** no existing concrete subclass, the sparse map's structure, or
  the detection algorithm's iteration logic requires modification to
  accommodate it

### Requirement: Mismatch detection iterates all characteristics without branching on value-vs-reference kind
An algorithm that detects mismatches between two `GraphStateItem`s'
characteristic sets SHALL iterate every entry through the common
`StateItemCharacteristic` interface only, and SHALL NOT branch on whether
a given entry is a value or reference characteristic — that distinction
is internal to each subclass, invisible to detection.

#### Scenario: Detection does not special-case kind
- **WHEN** mismatch detection runs over two `GraphStateItem`s whose
  characteristic sets include both value and reference characteristics
- **THEN** every entry is inspected through the same interface, and the
  detection outcome does not depend on which entries are value vs.
  reference characteristics beyond each entry's own mismatch comparison

### Requirement: Transform-insertion order is delegated to a per-kind strategy
When two or more characteristics mismatch, `GraphStateItem` SHALL expose
the order in which their reconciling Transforms are to be chained. This
order SHALL be determined by delegating to a strategy that varies by the
`GraphStateItem`'s own kind, not by any single characteristic subclass
acting as an arbiter over the others.

#### Scenario: Ordering is independent of detection order
- **WHEN** two or more characteristics are found mismatched, in some
  arbitrary iteration order
- **THEN** the order in which their reconciling Transforms should be
  chained is determined by the `GraphStateItem`'s kind-specific strategy,
  not by the order mismatches happened to be discovered in

#### Scenario: Two different kinds may order the same pair of characteristics differently
- **WHEN** two `GraphStateItem`s of different kinds both mismatch on the
  same two characteristics
- **THEN** each kind's own strategy determines that kind's chaining
  order independently; one kind's order is not assumed to apply to the
  other

### Requirement: A reference characteristic may be shared across multiple state items
Multiple `GraphStateItem`s' characteristic sets MAY hold a reference
characteristic referencing the same underlying node. A value
characteristic SHALL NOT be shared this way.

#### Scenario: Two state items share one geometry reference
- **WHEN** two `GraphStateItem`s both declare a geometry characteristic
  referencing the same underlying node
- **THEN** both characteristics resolve to the same referenced node, and
  no new identity mechanism is required to express the sharing

#### Scenario: Value characteristics are never shared
- **WHEN** two `GraphStateItem`s each have their own units or type-kind
  characteristic with the same value
- **THEN** each instance remains independent — no sharing mechanism
  applies, even though their values happen to coincide

### Requirement: Structural correctness from a shared-characteristic change is resolved eagerly
Changing a shared reference characteristic's underlying value SHALL be
performed through a dedicated, synchronous mutator operation that, within
one call: updates the shared value; resets every direct structural
dependent within the same owning graph to the invalid state; and advances
the shared node's revision. This mutator SHALL NOT invoke any method node
(no user code, no callback dispatch, no child-component method call) —
it is restricted to pure structural resets, so it is safe to call from
any phase or nesting level.

#### Scenario: Dependents are reset within the same call
- **WHEN** the mutator is called to change a shared characteristic's
  value
- **THEN** before the call returns, every direct structural dependent of
  the referenced node within the same owning graph has been reset to the
  invalid state, and the referenced node's own revision has advanced

#### Scenario: Mutator never invokes a method node
- **WHEN** the mutator walks structural dependents to reset them
- **THEN** no method node anywhere in that walk is invoked, regardless of
  what calling context (phase, nesting) the mutator was itself called
  from

#### Scenario: Immediate reuse after mutation sees correctly-shaped storage
- **WHEN** code that just called the mutator immediately inspects a
  direct structural dependent's storage, before any further graph
  traversal occurs
- **THEN** that dependent's storage already reflects the structural
  change (e.g. matches the new shared value's shape), without requiring
  a separate update request first

### Requirement: Content correctness from a shared-characteristic change remains lazy
A structural dependent reset by the mutator above SHALL have its content
(actual values, as opposed to storage shape) resolved by the existing
demand-driven update mechanism the next time that content is actually
requested, not synchronously by the mutator itself.

#### Scenario: Content is not recomputed by the mutator call itself
- **WHEN** the mutator changes a shared characteristic's value
- **THEN** no dependent's content-producing Transform is executed as
  part of that same call; execution happens only on a later, ordinary
  demand-driven update request

#### Scenario: Next demand-driven request produces correct content
- **WHEN** a dependent's content is requested after a prior mutator call
  reset it
- **THEN** the demand-driven update mechanism recognizes it as stale
  (via the advanced revision) and executes whatever Transform chain is
  needed to produce correct content before returning a value

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
