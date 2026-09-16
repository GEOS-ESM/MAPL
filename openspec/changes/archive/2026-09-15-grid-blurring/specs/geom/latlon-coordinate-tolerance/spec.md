## Purpose

Defines how MAPL decides whether two file-based LatLon grids represent
the same physical grid when their coordinate values differ only by a
small amount, so that geoms, RouteHandles, and regridders can be reused
across files whose coordinates are numerically noisy but semantically
identical.

## ADDED Requirements

### Requirement: Tolerant LatLon coordinate comparison scaled by grid spacing
When comparing a new LatLon grid specification that is not yet present
in a geom cache/registry against an already-registered LatLon grid
specification, the system SHALL treat their longitude and latitude
coordinate arrays (centers and corners) as equal if every corresponding
pair of values differs by no more than an absolute threshold, computed
as the new grid's own declared coordinate tolerance (a dimensionless
fraction) multiplied by the minimum spacing between adjacent coordinate
centers on that new grid's own axis - rather than requiring
bitwise-identical values and rather than treating the declared tolerance
as an absolute coordinate difference.

#### Scenario: Coordinates within tolerance are treated as equal
- **WHEN** an already-registered LatLon grid and a new LatLon grid have
  the same number of gridpoints and decomposition, and every
  corresponding center/corner coordinate value differs by an amount
  less than or equal to (the new grid's declared tolerance fraction
  multiplied by the minimum spacing between adjacent centers on the new
  grid's own axis)
- **THEN** the grids are reported as equal

#### Scenario: Coordinates outside tolerance are treated as different
- **WHEN** an already-registered LatLon grid and a new LatLon grid have
  the same number of gridpoints and decomposition, but at least one
  corresponding center or corner coordinate value differs by more than
  (the new grid's declared tolerance fraction multiplied by the minimum
  spacing between adjacent centers on the new grid's own axis)
- **THEN** the grids are reported as not equal

#### Scenario: Non-coordinate mismatches are still rejected first
- **WHEN** two LatLon grids differ in number of gridpoints or in
  decomposition
- **THEN** the grids are reported as not equal regardless of coordinate
  tolerance, without requiring a coordinate-by-coordinate comparison

#### Scenario: A single-gridpoint axis has no defined spacing
- **WHEN** the new grid's coordinate axis being compared has fewer than
  two points, so no adjacent-center spacing can be computed
- **THEN** the effective absolute tolerance for that axis is zero,
  regardless of any nonzero tolerance fraction the new grid declares,
  and comparison for that axis is strict/bitwise

### Requirement: Comparison is directional, not symmetric
When a new LatLon grid specification is looked up against an
already-registered one, the system SHALL use only the new
("candidate") grid's own declared coordinate tolerance and its own
coordinate spacing to decide whether the two are close enough to be
treated as equal. The already-registered grid's own declared tolerance
SHALL NOT be consulted for that lookup - it already made its own
accept/reject decision, using its own tolerance, at the time it was
itself registered. Consequently, comparing grid X (as the new
candidate) against already-registered grid Y need not yield the same
result as comparing grid Y (as the new candidate) against
already-registered grid X.

#### Scenario: Only the candidate's tolerance governs a lookup
- **WHEN** a new grid with a declared coordinate tolerance is looked up
  against an already-registered grid with a different (or no) declared
  coordinate tolerance, and the coordinate difference is within the new
  grid's own tolerance-scaled threshold
- **THEN** the grids are reported as equal, regardless of what
  tolerance the already-registered grid declared

#### Scenario: Swapping which grid is the candidate can change the result
- **WHEN** two grids' coordinates differ by a fixed nonzero amount, one
  grid declares a tolerance covering that amount and the other declares
  no tolerance (or one insufficient to cover it)
- **THEN** looking up the grid with the covering tolerance as the new
  candidate against the other (already registered) reports the grids as
  equal, while looking up the other grid as the new candidate against
  the first (already registered) reports the grids as not equal

### Requirement: Default tolerance preserves strict comparison
When a new grid's file metadata does not declare a coordinate
tolerance, the system SHALL use a tolerance of zero for that grid when
it acts as a candidate in a lookup, so it is only considered equal to
an already-registered grid when their coordinates match exactly. This
preserves existing (pre-change) comparison behavior for grids that do
not opt in.

#### Scenario: File without tolerance attribute compares strictly
- **WHEN** a LatLon grid is constructed from file metadata that does not
  declare a coordinate tolerance, and is looked up as a new candidate
  against an already-registered grid
- **THEN** that lookup uses a tolerance of zero

### Requirement: Tolerance sourced from a generic file metadata attribute
The system SHALL determine a LatLon grid's coordinate tolerance
(expressed as a dimensionless fraction of that grid's own coordinate
spacing, per the scaling requirement above) from an optional,
generically-named attribute on the file metadata used to construct that
grid, read through the metadata's existing generic attribute-access
mechanism. The geom layer SHALL NOT require any tolerance-specific
method on the file metadata type, and SHALL NOT care which caller set
the attribute or how its value was derived - without requiring callers
of grid-lookup or grid-comparison operations to supply the tolerance as
an explicit argument.

#### Scenario: File metadata declares a coordinate tolerance
- **WHEN** a LatLon grid is constructed from file metadata that declares
  a non-negative coordinate tolerance attribute (however that attribute
  came to be set)
- **THEN** that declared value is used, as a fraction of this grid's own
  coordinate spacing, as the tolerance for lookups where this grid is
  the new candidate

### Requirement: Tolerant comparison scoped to LatLon grids
The tolerant coordinate comparison behavior SHALL apply only to LatLon
grid specifications. Comparison behavior for other grid types (e.g.
cubed-sphere/Mesh, LocStream, EASE) SHALL remain unchanged (exact
comparison).

#### Scenario: Non-LatLon grid comparison is unaffected
- **WHEN** two grids of a type other than LatLon (e.g. Mesh, LocStream,
  or EASE) are compared for equality
- **THEN** the comparison result is unaffected by any coordinate
  tolerance mechanism introduced for LatLon grids
</content>
