# latlon-coordinate-tolerance Specification

## Purpose

Defines how MAPL decides whether two file-based LatLon grids represent
the same physical grid when their coordinate values differ only by a
small amount, so that geoms, RouteHandles, and regridders can be reused
across files whose coordinates are numerically noisy but semantically
identical.

## Requirements

### Requirement: Tolerant LatLon coordinate comparison
When comparing two LatLon grid specifications, the system SHALL treat
their longitude and latitude coordinate arrays (centers and corners) as
equal if every corresponding pair of values differs by no more than the
applicable coordinate tolerance, rather than requiring bitwise-identical
values.

#### Scenario: Coordinates within tolerance are treated as equal
- **WHEN** two LatLon grids have the same number of gridpoints and
  decomposition, and every corresponding center/corner coordinate value
  differs by an amount less than or equal to the applicable tolerance
- **THEN** the grids are reported as equal

#### Scenario: Coordinates outside tolerance are treated as different
- **WHEN** two LatLon grids have the same number of gridpoints and
  decomposition, but at least one corresponding center or corner
  coordinate value differs by more than the applicable tolerance
- **THEN** the grids are reported as not equal

#### Scenario: Non-coordinate mismatches are still rejected first
- **WHEN** two LatLon grids differ in number of gridpoints or in
  decomposition
- **THEN** the grids are reported as not equal regardless of coordinate
  tolerance, without requiring a coordinate-by-coordinate comparison

### Requirement: Default tolerance preserves strict comparison
When a file does not declare a coordinate tolerance, the system SHALL
use a tolerance of zero, so that grids from that file are only
considered equal to another grid when their coordinates match exactly.
This preserves existing (pre-change) comparison behavior for files that
do not opt in.

#### Scenario: File without tolerance attribute compares strictly
- **WHEN** a LatLon grid is constructed from file metadata that does not
  declare a coordinate tolerance
- **THEN** that grid is compared against other grids using a tolerance
  of zero

### Requirement: Tolerance sourced from a generic file metadata attribute
The system SHALL determine a LatLon grid's coordinate tolerance from an
optional, generically-named attribute on the file metadata used to
construct that grid, read through the metadata's existing generic
attribute-access mechanism. The geom layer SHALL NOT require any
tolerance-specific method on the file metadata type, and SHALL NOT care
which caller set the attribute or how its value was derived - without
requiring callers of grid-lookup or grid-comparison operations to supply
the tolerance as an explicit argument.

#### Scenario: File metadata declares a coordinate tolerance
- **WHEN** a LatLon grid is constructed from file metadata that declares
  a non-negative coordinate tolerance attribute (however that attribute
  came to be set)
- **THEN** that declared value is used as the tolerance for subsequent
  comparisons involving that grid

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
