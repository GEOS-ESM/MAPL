## Why

Phase 1-2 of the graph-neutral core (`ComponentGraph`, `GraphNode` hierarchy,
`DependencyNetwork`, identities) is implemented and tested under
`superstructure/generic/graph/`, but nothing in the MAPL component hierarchy
owns or references it yet. `OuterMetaComponent` — today's stand-in for the
spec's `OuterComponent` — already has most of REQ-HIER-002's shape (own
`GriddedComponentDriver`, per-child `GriddedComponentDriverMap`,
framework-managed states) but has no `ComponentGraph` field, no public-port
storage distinct from `StateRegistry`/`ComponentSpec`, and no place to hold
parent-local proxy nodes for a child's published ports. Per
`docs/graph/spec/20-implementation-roadmap.md` §20.4.1, this is sub-change
3a: the ownership-shape and encapsulation-boundary plumbing that later
sub-changes (3b GraphBuilder wiring, 3c extension reuse, 3d visualization
enrichment) depend on. It must land before any of those, and does not
require `StateRegistry` integration or connection-resolution logic itself.

## What Changes

- Add a `ComponentGraph` field to `OuterMetaComponent`, initialized during
  `init_meta`/`new_outer_meta`, exposed via a new read-only accessor
  (`get_component_graph`), satisfying REQ-HIER-002's "one local
  `ComponentGraph`" and REQ-HIER-004's "exactly one local `ComponentGraph`
  per `OuterComponent`" requirements.
- No new port-map or proxy-node-map types are needed: `ComponentGraph`
  (Phase 1-2, already implemented) already owns `PortIdNodeIdMap`-backed
  `import_ports`/`export_ports` tables (REQ-HIER-002's "public ports") and
  a `child_bindings` table (`add_child_port_binding`/
  `get_child_port_binding`, PortId -> NodeId, requiring the `NodeId` be
  graph-owned) that is exactly REQ-HIER-006's proxy-node storage
  mechanism. Attaching a `ComponentGraph` to `OuterMetaComponent` makes
  both available for free. This change adds no code path that populates
  `child_bindings` with actual proxy nodes — that population logic is
  `GraphBuilder`'s job (sub-change 3b); 3a only makes the storage
  reachable from the component hierarchy.
- Enforce the parent-may/child-may-not encapsulation boundary
  (REQ-HIER-003, REQ-HIER-005) at the API surface: no accessor is added
  that lets a child reach its parent's `ComponentGraph` (or the parent's
  `OuterMetaComponent` at all), and no accessor is added that exposes a
  child's internal `NodeId`s, `DependencyNetwork`, or raw `ComponentGraph`
  handle to its parent — a parent may reach a child's driver and (once 3b
  populates it) published ports, which are already exposed via
  `GriddedComponentDriverMap`/`get_child`.
- No change to `GraphBuilder`, `StateRegistry` advertising, connection
  resolution, or existing coupler behavior. No change to
  `ComponentGraph`'s own graph-neutrality (REQ-CG-002 unaffected — the new
  `OuterMetaComponent` field depends on `ComponentGraph`, not the reverse).

## Capabilities

### New Capabilities
- `graph/component-hierarchy`: `OuterComponent`/`OuterMetaComponent`
  ownership shape (per-child driver, one local `ComponentGraph`,
  framework-managed states, public ports), the parent-may/child-may-not
  encapsulation boundary, and the proxy-node storage mechanism
  (REQ-HIER-001..006).

### Modified Capabilities
(none — `component-graph`, `graph-node-hierarchy`, `dependency-network`,
etc. are consumed as-is, unmodified, by the new `OuterMetaComponent` field)

## Impact

- **Affected code**: `superstructure/generic/OuterMetaComponent.F90` (new
  type-bound field + accessor declaration), a new submodule file under
  `superstructure/generic/OuterMetaComponent/` implementing the accessor
  and graph initialization in `new_outer_meta`/`init_meta`, and
  `superstructure/generic/API.F90` if the new accessor needs to be
  re-exported.
- **New Fortran types**: none. Reuses `ComponentGraph` (and its internal
  `PortIdNodeIdMap`-based port/binding tables) from
  `superstructure/generic/graph/` as-is.
- **Dependencies**: `OuterMetaComponent` gains a compile-time dependency on
  `mapl_ComponentGraph_mod`; no new external dependency.
- **Tests**: new pFUnit tests under `superstructure/generic/tests/` (or
  alongside existing `OuterMetaComponent` tests, if any) verifying: every
  `OuterMetaComponent` owns a valid `ComponentGraph`; two sibling
  `OuterMetaComponent`s have distinct `ComponentGraph` instances (registering
  a node in one does not appear in the other); the graph's public-port and
  child-binding tables are reachable through the new accessor and contain
  no populated entries from this change alone (population is out of
  scope); no new API allows a child to reach its parent's
  `OuterMetaComponent`, `ComponentGraph`, or internal identities.
- **Out of scope**: `GraphBuilder` advertising/connection-resolution logic
  (3b), extension reuse (3c), visualization enrichment (3d), and any
  change to existing imperative-coupler behavior.
