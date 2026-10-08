## Purpose

Defines `RouteHandleKey`, the semantic key that identifies a specific
`RouteHandle` request (source/destination geometry plus the
regrid-relevant settings that affect its result), and defines how that
key is used to locate a previously-created `RouteHandle` for reuse
instead of creating a redundant one.

## ADDED Requirements

### Requirement: A RouteHandleKey identifies source geometry, destination geometry, and the regrid-relevant settings that affect the result
A `RouteHandleKey` SHALL carry, at minimum: a reference to the source
geometry, a reference to the destination geometry, the regridding
method, mask settings, extrapolation settings, normalization settings,
and any other ESMF regrid-store setting that can change which
`RouteHandle` is produced for an otherwise-identical geometry pair.

#### Scenario: A key identifies its source and destination geometry
- **WHEN** a `RouteHandleKey` is constructed for a given source geometry
  and destination geometry
- **THEN** both the source and destination geometry it was constructed
  with are retrievable from the key

#### Scenario: A key identifies its regrid method
- **WHEN** a `RouteHandleKey` is constructed with a given regridding
  method
- **THEN** that regridding method is retrievable from the key

### Requirement: Two keys for the same geometry pair but different regrid-relevant settings are distinct
A `RouteHandleKey` SHALL distinguish two requests that share the same
source and destination geometry but differ in regridding method, masks,
extrapolation settings, normalization settings, or any other included
ESMF regrid-store setting. Keys MUST NOT be collapsed to the geometry
pair alone.

#### Scenario: Same geometry pair, different regrid method, are distinct keys
- **WHEN** two `RouteHandleKey`s are constructed for the identical
  source/destination geometry pair but with different regridding
  methods (for example linear vs. conservative)
- **THEN** the two keys are reported as distinct, not equivalent

#### Scenario: Same geometry pair, same settings, are the same key
- **WHEN** two `RouteHandleKey`s are constructed separately for the
  identical source/destination geometry pair and identical regrid
  settings
- **THEN** the two keys are reported as equivalent

### Requirement: A RouteHandleKey can be used to locate a previously-registered RouteHandle for reuse
A `RouteHandleKey` SHALL be usable as a lookup key against a
graph-neutral semantic index so that a caller can determine whether a
`RouteHandle` satisfying that key already exists before creating a new
one. The index SHALL only locate the previously-registered identity; it
SHALL NOT be a second place where that identity is owned.

#### Scenario: A registered key is found on lookup
- **WHEN** a `RouteHandleKey` has been registered against a resource
  identity in the semantic index
- **THEN** looking up that same key returns that resource identity

#### Scenario: An unregistered key is not found
- **WHEN** a `RouteHandleKey` has not been registered in the semantic
  index
- **THEN** looking it up reports no match, rather than a false reuse

#### Scenario: The index is not an alternate ownership record
- **WHEN** a resource identity registered under a `RouteHandleKey` is
  removed from its owning store
- **THEN** the semantic index alone does not keep that identity usable —
  the index is a locator only, not an independent source of truth
