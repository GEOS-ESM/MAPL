# vertical-grid Specification

## Purpose

Defines `VerticalGrid` as an ordinary, graph-visible `GraphStateItem` —
a nested-state item whose members are the individual physical-dimension
coordinate-set Fields — and defines how a mismatch between two vertical
grids is resolved by an explicit dimension-overlap check instead of a
single all-or-nothing identity comparison, giving legacy's
already-resolved per-component vertical grid real graph structure
without re-deriving it.

## Requirements

### Requirement: A vertical grid is carried as a nested-state item with per-dimension coordinate-set members
A vertical grid value SHALL be represented as the nested-state component
of an ordinary `GraphStateItem`, tagged with the vertical-grid variant.
Each of its members SHALL itself be an ordinary, independently
graph-visible coordinate-set item, addressed within the vertical grid by
the physical dimension it represents (for example `"pressure"` or
`"height"`), not by a fixed pair of names.

#### Scenario: Vertical grid item exposes its coordinate sets by physical dimension
- **WHEN** a vertical-grid `GraphStateItem` is constructed from a
  component's supported physical dimensions
- **THEN** each supported physical dimension is retrievable as a member
  of that item, addressed by its physical-dimension name

#### Scenario: Vertical grid item is queryable as an ordinary state item
- **WHEN** a vertical-grid `GraphStateItem`'s variant is queried
- **THEN** it reports the vertical-grid variant, and the item is
  otherwise reachable through the same node-identity lookup as any other
  advertised state item

#### Scenario: A coordinate-set member is an ordinary, independently addressable item
- **WHEN** one physical-dimension member of a vertical-grid item is
  retrieved
- **THEN** it resolves to an ordinary state item with its own node
  identity, independently reachable from the vertical-grid item that
  groups it

### Requirement: A vertical grid's own units are found through its coordinate-set Fields, not stored redundantly
A vertical grid SHALL NOT carry physical units as a property of its own;
the units for a given physical dimension SHALL be found through the
units already carried by that dimension's coordinate-set item.

#### Scenario: Units are read from the coordinate-set item, not the vertical grid
- **WHEN** the physical units for one of a vertical grid's supported
  dimensions are requested
- **THEN** the result is the units already carried by that dimension's
  coordinate-set item, and the vertical grid itself exposes no separate,
  independently-settable units property

### Requirement: An already-resolved per-component vertical grid is given graph structure without re-resolving it
When a component already has a vertical grid resolved through existing
(non-graph) mechanisms, that outcome SHALL be represented as a
vertical-grid `GraphStateItem` without altering which vertical grid, or
which coordinate sets, that component ends up with.

#### Scenario: A component's already-resolved vertical grid becomes graph-visible
- **WHEN** a component that already has a resolved vertical grid is
  processed
- **THEN** a vertical-grid item appears in that component's graph whose
  supported physical dimensions and coordinate-set Fields match the
  already-resolved outcome exactly

#### Scenario: A component with no resolved vertical grid gets no vertical-grid item
- **WHEN** a component has no vertical grid resolved through existing
  mechanisms
- **THEN** no vertical-grid item is created for that component

### Requirement: Two vertical grids match when they declare the identical set of physical dimensions
When comparing a vertical-grid export against a vertical-grid import
that both reference a vertical-grid graph item, the two SHALL be
considered matching if and only if they reference the same item.
Otherwise, when both sides hold a vertical grid object, the two SHALL be considered
matching if and only if they are the same grid (same id) or the export
grid reports that it matches the import grid (the legacy vertical-grid
`matches` rule; for example two basic grids with the same number of
levels match). When either side holds no grid object, the two SHALL be
considered matching if and only if they carry the same grid id or, with
no id, declare exactly the same set of physical dimensions as
coordinate-set members.

#### Scenario: Identical dimension sets match
- **WHEN** a vertical-grid export and a vertical-grid import that hold no
  grid object declare exactly the same set of physical dimensions
- **THEN** resolution reports them as matching

#### Scenario: Different dimension sets do not match
- **WHEN** a vertical-grid export and a vertical-grid import declare
  different sets of physical dimensions
- **THEN** resolution reports them as not matching, and proceeds to the
  dimension-adaptability check below rather than wiring them directly

#### Scenario: Two separately built grids that the legacy rule calls equal match
- **WHEN** a vertical-grid export and import hold two distinct basic grid
  objects (different ids) with the same number of levels
- **THEN** resolution reports them as matching and builds no
  vertical-grid step

#### Scenario: Two separately built fixed-level grids with identical levels do not match
- **WHEN** a vertical-grid export and import hold two distinct fixed-level
  grid objects (different ids) with the same physical dimension and
  identical levels
- **THEN** resolution reports them as not matching (a fixed-level grid
  matches another grid object only if that grid is a basic grid with the
  same level count) and builds a vertical column regrid step

### Requirement: A mismatched vertical-grid connection is resolved by a dimension-overlap check, not a single undifferentiated mismatch
When a vertical-grid export and a vertical-grid import do not match,
resolution SHALL compute the set of physical dimensions the export and
the import have in common before reporting an outcome:

- If exactly **one** physical dimension overlaps, that dimension SHALL be
  identified as the dimension through which adaptation would occur.
- If **zero** dimensions overlap, this SHALL be reported as an explicit,
  distinguishable "incompatible" outcome.
- If **more than one** dimension overlaps, this SHALL be reported as an
  explicit, distinguishable "ambiguous" outcome, since the connection
  cannot silently guess which dimension to adapt through.

This requirement covers the case where an import declares a single
required vertical coordinate system; an import declaring multiple
acceptable coordinate systems is out of scope.

#### Scenario: Exactly one overlapping dimension is identified
- **WHEN** a mismatched vertical-grid export and import share exactly one
  physical dimension
- **THEN** resolution identifies that single dimension as the
  adaptation candidate, distinguishably from the "incompatible" and
  "ambiguous" outcomes below

#### Scenario: No overlapping dimension is reported as incompatible
- **WHEN** a mismatched vertical-grid export and import share no physical
  dimension
- **THEN** resolution reports an explicit "incompatible" outcome, not a
  silent failure to wire and not a generic unqualified mismatch

#### Scenario: More than one overlapping dimension is reported as ambiguous
- **WHEN** a mismatched vertical-grid export and import share more than
  one physical dimension
- **THEN** resolution reports an explicit "ambiguous" outcome, not a
  silent guess at which dimension to use

### Requirement: A vertical-grid mismatch delegates to extension-chain reporting rather than wiring directly
Once a mismatch's adaptation dimension has been identified (or the
mismatch has been reported as incompatible or ambiguous), resolution
SHALL delegate to the same extension-reuse chain-creation path used for
any other mismatched pair rather than wiring the export and import
directly or silently dropping the connection. For a field or vector item
with a single adaptation dimension, where both grids carry level
coordinate values for that dimension, the vertical-grid step SHALL be a
column regrid that also reverses the level order when the two
orientations differ; for a vector it regrids every member with one set of
weights. Two basic grids with different level counts SHALL be reported
incompatible. A model grid, a side with no grid object, an item that is
neither a field nor a vector (a bracket is a field by this step; a plain
field bundle and a vector-bracket are not), or a non-center vertical
stagger SHALL be reported as an explicit unsupported
vertical-grid step at connect time.

#### Scenario: A resolvable single-dimension mismatch between fixed-level grids builds a column regrid
- **WHEN** a mismatched field export and import have fixed-level vertical
  grids that share exactly one physical dimension
- **THEN** resolution builds a vertical column regrid step, and that step
  also covers any vertical orientation mismatch between the two items

#### Scenario: A vector with a single-dimension mismatch builds a bundle column regrid
- **WHEN** a mismatched vector export and import have fixed-level
  vertical grids that share exactly one physical dimension
- **THEN** resolution builds one vertical column regrid step on the
  bundle; the output members are created at compute with the import's
  level count, vertical grid, stagger and alignment

#### Scenario: A plain field bundle with a vertical-grid mismatch is unsupported
- **WHEN** a mismatched plain field-bundle or vector-bracket export and
  import have fixed-level vertical grids that share one physical dimension
- **THEN** resolution reports the vertical-grid step unsupported and
  builds no step

#### Scenario: A resolvable single-dimension mismatch still fails explicitly when no provider exists
- **WHEN** a mismatched vertical-grid export and import identify exactly
  one adaptation dimension, and at least one grid is a model grid or has
  no grid object
- **THEN** resolution reports an explicit, diagnosable unsupported
  vertical-grid step rather than wiring the connection directly

#### Scenario: A non-center stagger across a vertical-grid mismatch fails at connect time
- **WHEN** a field export and import differ in vertical grid and the
  item's vertical stagger is not center
- **THEN** resolution reports the vertical-grid step unsupported at
  connect time

### Requirement: A coordinate-set item's horizontal geometry is identifiable through its owning component
Each coordinate-set member of a vertical-grid item SHALL belong to the
same component scope as that component's own horizontal-geometry item,
so that the horizontal geometry a coordinate set is defined on is always
identifiable by looking up that shared scope, without requiring a
separate reference to be declared.

#### Scenario: Coordinate-set item's geometry is identifiable via shared component scope
- **WHEN** a component has both a horizontal-geometry item and a
  vertical-grid item with coordinate-set members
- **THEN** each coordinate-set member and the component's
  horizontal-geometry item are both reachable from that same component's
  own scope, independently of the vertical-grid item that groups the
  coordinate-set members

### Requirement: An orientation-only vertical mismatch is resolved by a level flip
When a field or vector export and import share a vertical grid but
differ only in vertical orientation, resolution SHALL build a step that
reverses the level order (of every member, for a vector). An item that is
neither a field nor a vector with that mismatch SHALL be reported as an
explicit unsupported orientation step.

#### Scenario: Same grid, opposite orientation
- **WHEN** a field export with downward orientation connects to a field
  import with upward orientation on the same vertical grid
- **THEN** resolution builds one orientation step and the import sees the
  levels in reversed order

#### Scenario: Same grid, opposite orientation, vector
- **WHEN** a vector export with downward orientation connects to a vector
  import with upward orientation on the same vertical grid
- **THEN** resolution builds one orientation step on the bundle and every
  member of the import sees the levels in reversed order
