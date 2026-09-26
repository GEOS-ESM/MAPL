# horizontal-geometry Specification

## Purpose

Defines horizontal geometry (`Grid`/`Mesh`/`LocStream`/`XGrid`) as an
ordinary, graph-visible `GraphStateItem` — how it is constructed, how a
component exposes it, and how it is matched and wired across component
boundaries — giving legacy's already-resolved geometry connectivity
(own/from-ancestor/from-child) real graph structure without any
per-case special-casing, and reusing the same mismatch-detection and
extension-chain-delegation machinery ordinary connection resolution
already uses for other mismatched items.

## Requirements

### Requirement: Geometry is carried as an incomplete geometry-proxy field
A horizontal geometry value SHALL be represented as the field component
of an ordinary `GraphStateItem`, holding an `ESMF_Field` that has a grid
assigned but no data array allocated, and tagged with the geometry
variant. This item SHALL NOT carry a data array at any point while it is
playing the geometry-proxy role.

#### Scenario: Geometry item has a grid but no allocated data
- **WHEN** a geometry `GraphStateItem` is constructed
- **THEN** its underlying field reports a grid/mesh/location-stream/xgrid
  assignment but reports no allocated data array

#### Scenario: Geometry item is queryable as an ordinary state item
- **WHEN** a geometry `GraphStateItem`'s variant is queried
- **THEN** it reports the geometry variant, and the item is otherwise
  reachable through the same node-identity lookup as any other advertised
  state item

### Requirement: Geometry proxy fields are exposed under a reserved name, hidden from user states
A component that provides or receives horizontal geometry SHALL make the
geometry proxy field available under a reserved, framework-owned name,
kept separate from that component's user-facing import, export, and
internal states.

#### Scenario: Geometry proxy is absent from user-facing states
- **WHEN** a component's user-facing import, export, or internal state is
  inspected for advertised items
- **THEN** no geometry proxy field appears among them

#### Scenario: Geometry proxy is reachable through the reserved name
- **WHEN** the framework looks up a component's geometry proxy field by
  its reserved name
- **THEN** the lookup succeeds whenever that component has been given a
  geometry to provide or has received one through connection resolution

### Requirement: Geometry is wired for each single-source case without per-case special-casing
Geometry connectivity (which component provides its own geometry, which
receives it from an ancestor, which receives it from a child) is decided
exactly once, by existing component-hierarchy configuration, before any
geometry graph structure is built. Building that structure SHALL apply
uniformly to each of the following single-source cases, with no
per-case branching in how the resulting graph item is matched or wired:

- a component that provides its own geometry (no import needed)
- a component receiving geometry exported by an ancestor
- a component receiving geometry exported by a child

#### Scenario: A component providing its own geometry needs no import resolution
- **WHEN** a component constructs and exports its own geometry proxy
  field
- **THEN** that component's geometry item is available without any
  connection being resolved for it

#### Scenario: A component receives geometry from an ancestor
- **WHEN** a component's geometry import is connected to an ancestor's
  geometry export through the ordinary connection-resolution path
- **THEN** the descendant's geometry item resolves to the same underlying
  grid/mesh/location-stream/xgrid handle as the ancestor's

#### Scenario: A component receives geometry from a child
- **WHEN** a component's geometry import is connected to a child's
  published geometry export through the ordinary connection-resolution
  path
- **THEN** the parent's geometry item resolves to the same underlying
  grid/mesh/location-stream/xgrid handle as the child's

### Requirement: Two geometry items match when they wrap the identical underlying handle
When ordinary connection resolution compares a geometry export against a
geometry import, the two SHALL be considered matching if and only if they
wrap the identical underlying `Grid`/`Mesh`/`LocStream`/`XGrid` handle.

#### Scenario: Identical handle matches
- **WHEN** a geometry export and a geometry import wrap the same
  underlying grid/mesh/location-stream/xgrid handle
- **THEN** ordinary connection resolution reports them as matching and
  wires a dependency edge directly between them

#### Scenario: Distinct handles do not match
- **WHEN** a geometry export and a geometry import wrap different
  underlying grid/mesh/location-stream/xgrid handles
- **THEN** ordinary connection resolution reports them as not matching
  and does not wire a dependency edge directly between them

### Requirement: A geometry mismatch is reported through extension-chain delegation, not silently mis-wired
When a geometry export and import do not match, resolution SHALL
delegate to the same extension-reuse chain-creation path used for any
other mismatched pair (including that capability's existing "no
registered provider fails explicitly" behavior), rather than wiring them
directly or silently dropping the connection.

#### Scenario: Mismatched geometry delegates instead of wiring directly
- **WHEN** a geometry export and a geometry import do not match
- **THEN** resolution delegates to extension-reuse's chain-creation path
  rather than adding a dependency edge directly between the export's node
  and the import's node
