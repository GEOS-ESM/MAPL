## MODIFIED Requirements

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

### Requirement: A vertical-grid mismatch delegates to extension-chain reporting rather than wiring directly
Once a mismatch's adaptation dimension has been identified (or the
mismatch has been reported as incompatible or ambiguous), resolution
SHALL delegate to the same extension-reuse chain-creation path used for
any other mismatched pair rather than wiring the export and import
directly or silently dropping the connection. For a field item with a
single adaptation dimension, where both grids carry level coordinate
values for that dimension, the vertical-grid step SHALL be a column
regrid that also reverses the level order when the two orientations
differ. Two basic grids with different level counts SHALL be reported
incompatible. A model grid, a side with no grid object, a non-field
item, an R8 item, or a non-center vertical stagger SHALL be reported as
an explicit unsupported vertical-grid step at connect time.

#### Scenario: A resolvable single-dimension mismatch between fixed-level grids builds a column regrid
- **WHEN** a mismatched field export and import have fixed-level vertical
  grids that share exactly one physical dimension
- **THEN** resolution builds a vertical column regrid step, and that step
  also covers any vertical orientation mismatch between the two items

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

## ADDED Requirements

### Requirement: An orientation-only vertical mismatch is resolved by a level flip
When a field export and import share a vertical grid but differ only in
vertical orientation, resolution SHALL build a step that reverses the
level order. A non-field item with that mismatch SHALL be reported as an
explicit unsupported orientation step.

#### Scenario: Same grid, opposite orientation
- **WHEN** a field export with downward orientation connects to a field
  import with upward orientation on the same vertical grid
- **THEN** resolution builds one orientation step and the import sees the
  levels in reversed order
