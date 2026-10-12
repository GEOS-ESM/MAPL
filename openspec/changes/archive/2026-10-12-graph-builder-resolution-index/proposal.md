## Why

`GraphBuilder`'s existing cross-boundary mechanisms only reach exactly
one hierarchy level: `component_for`/`get_child_component_graph` (3b,
REQ-GB-002 carve-out) resolve `"<self>"` or a direct child name only;
`get_or_make_local_node_id`'s proxy-chaining likewise only bridges one
parent/child boundary per call. Two real needs cannot be met by a
single-level reach:

1. **Cross-level ordinary connection resolution.** Every real
   `connect_all`-style fixture in this repo (`statistics`,
   `statistics_real`, `history_1`, `history_wildcard`, `extdata_1`,
   confirmed during 3b's own task 7.1) declares a connection naming two
   components whose *own* `var_specs`/items are empty — the real items
   live on a further descendant. Legacy resolves this because
   `StateRegistry`'s `propagate_exports_all`/
   `propagate_unsatisfied_imports_all` (`StateRegistry_Propagation_smod.F90`)
   eagerly push every export up one level, unconditionally, on every
   advertise pass, so a single-level lookup at any ancestor already
   sees everything beneath it. `GraphBuilder` has no equivalent.
2. **History-style nested-substate export visibility.** Legacy exposes
   each component's export recursively, slash-qualified, inside every
   ancestor's own export namespace (`propagate_exports_virtual_pt`'s
   `comp_name/short_name` tagging), so a diagnostics component
   anywhere in the hierarchy can address any descendant's export by
   qualified path, with extensions (unit conversion, regrid) available
   to interpose on that connection exactly as for an ordinary one.

Both needs are instances of the same missing primitive: resolving a
qualified, multi-segment path (`comp1/comp2/.../short_name`) to the
`OuterMetaComponent`/`ComponentGraph`/`NodeId` that actually owns the
named item, without requiring every intermediate level to have already
pushed that item upward in advance. `GraphBuilder_CallbackWiring_smod.F90`
(Phase 4d, archived `callback-wiring`) already built exactly this
shape once, narrowly, for its own flattened qualified-export namespace
(`build_qualified_export_namespace`/`collect_qualified_exports`/
`resolve_descendant`) — a fresh, uncached, recursive descent from a
given root, composing one `/`-joined path segment per level. This
change generalizes that existing mechanism into a reusable,
framework-internal resolution primitive, rather than letting a second,
narrower reimplementation grow inside ordinary connection resolution
(which 3b2's own propagation design was about to do) or export
visibility.

**Architectural note, recorded for the project record (not verified
against this repo's own history, since the relevant era predates this
checkout): this lazy, downward-walk-from-a-named-root approach is
closer in spirit to how an older generation of MAPL handled this same
problem than to `StateRegistry`'s own eager, bottom-up push — worth
being explicit about, since it is a real design trade-off, not an
incidental implementation detail.** A lazy, on-demand qualified-path
walk achieves the same reachability as `StateRegistry`'s eager,
unconditional-for-every-export push, but does not give a subtree's own
`ComponentGraph` any structural representation of everything it
transitively exposes — unlike `StateRegistry`, where a subtree's own
export `ESMF_State` directly, inspectably contains (via flattened
naming) everything beneath it, satisfying the "a subtree acts as one
component" property structurally at that subtree's own boundary. This
change's resolution index does NOT provide that same structural
guarantee: "subtree X exposes item Y" becomes true only in the sense
of "reachable if resolution walks down and finds it," not "visible as
real port/resource-index structure if you ask subtree X directly."
This is accepted as a known, named trade-off for this change (see
design.md Risks) — revisit only if a concrete future need (e.g.
visualization/export tooling wanting a subtree's full exposed surface
as real, inspectable structure) makes the weaker property insufficient.

## What Changes

- Introduce a new, framework-internal resolution primitive —
  `resolve_qualified_component(root, path, rc)` — generalizing
  `GraphBuilder_CallbackWiring_smod.F90`'s existing `resolve_descendant`
  (currently private to that submodule) into `GraphBuilder.F90`'s own
  shared-helpers section (same visibility tier as `component_for`/
  `is_self`, REQ-GB-002 carve-out): given a root `OuterMetaComponent`
  and a `/`-separated path, walks `get_child_outer_meta` one segment at
  a time and returns the `OuterMetaComponent` the path's final segment
  names. Functionally identical to the existing `resolve_descendant` —
  this change relocates and renames it for shared use, not rewrites its
  algorithm.
- Introduce a second primitive —
  `enumerate_qualified_exports(root, rc)` — generalizing
  `build_qualified_export_namespace`/`collect_qualified_exports`
  (currently callback-wiring-specific, keyed to `QualifiedExportEntry`)
  into a reusable recursive descent that any caller needing "every
  export reachable beneath this root, by qualified path" can use,
  without being coupled to callback-wiring's own `QualifiedExportEntry`
  record shape or interface-conformance concerns.
- **Rewire ordinary connection resolution** (3b/3c,
  `GraphBuilder_ConnectionResolution_smod.F90`) to resolve a
  `MatchConnection`'s `src_pt%component_name`/`dst_pt%component_name`
  via `resolve_qualified_component` before enumerating candidates,
  instead of `component_spec_for`'s existing single-level-only
  dispatch (`"<self>"` or one direct child name). A connection naming a
  component several levels down the hierarchy (every real fixture's
  own shape) now resolves correctly for the first time; a connection
  naming a direct child or self continues to resolve exactly as
  before (`resolve_qualified_component` with a single-segment path is
  behaviorally identical to today's `component_for`).
- **Rewire `GraphBuilder_CallbackWiring_smod.F90`'s own
  `resolve_descendant`** to call the new shared
  `resolve_qualified_component` instead of keeping a private,
  duplicate implementation — no behavior change to callback-wiring's
  own existing scenarios, pure de-duplication.
- Document the new primitives as the intended foundation for (a)
  `graph-builder-stateitem-resolution`'s own cross-level needs once
  that change lands, (b) `3b2-cross-component-import-propagation`'s
  own propagation mechanism (to be revised, once this change lands, to
  use qualified-path resolution rather than per-level proxy-chaining),
  and (c) a future, separately-scoped nested-substate export-visibility
  capability for diagnostics/History-style access — this change
  provides the primitive; it does not itself wire up (c)'s own
  consumer-side behavior.
- **Explicitly out of scope**: any new graph structure (ports, proxy
  nodes, resource-index entries) created as a result of a qualified-
  path lookup — this change's two primitives are pure, read-only
  traversal/query functions over the existing `OuterMetaComponent`
  hierarchy; what a caller does with the result (create a proxy, wire
  a dependency edge, etc.) is each consuming change's own
  responsibility. A dedicated nested-substate export-visibility
  capability (need (c) above) — this change only supplies the
  traversal primitive it would be built on, not the capability itself.

## Capabilities

### Modified Capabilities
- `graph/graph-builder`: ordinary connection resolution's source/
  destination component lookup is restated to use qualified-path
  resolution (reaching any descendant, not only `"<self>"` or a direct
  child), rather than being silently limited to the single-level case
  3b's original scenarios never stated as a limitation in the first
  place (the limitation was an implementation gap, not a declared
  scope boundary — see `graph/graph-builder`'s existing "Ordinary
  resolution matches existing coupler behavior" requirement, whose own
  "Propagated (cross-level) items are not yet covered" scenario this
  change directly closes).

## Impact

- **Affected code**: `GraphBuilder.F90` (new shared primitives),
  `GraphBuilder_ConnectionResolution_smod.F90` (`component_spec_for`'s
  role in `check_match_connection_unsatisfied`/`resolve_match_connection`
  replaced by `resolve_qualified_component` for the
  `src_pt`/`dst_pt%component_name` lookup), `GraphBuilder_CallbackWiring_smod.F90`
  (`resolve_descendant` becomes a thin wrapper over the new shared
  primitive; `collect_qualified_exports` may be rewritten to call
  `enumerate_qualified_exports` and re-shape the result into
  `QualifiedExportEntry`, or left as-is if re-shaping costs more than
  it saves — left to design.md).
- **Existing code read, not modified**: `OuterMetaComponent%
  get_child_outer_meta`/`get_num_children`/`get_child_name` (already
  public, pre-existing) — both new primitives are built entirely from
  these, same as `resolve_descendant`/`collect_qualified_exports`
  already are today.
- **New Fortran types**: none — both primitives return/consume
  `OuterMetaComponent` pointers and plain character paths; no new
  record type is introduced (unlike `QualifiedExportEntry`, which
  stays callback-wiring's own, not generalized by this change).
- **Tests**: existing `Test_GraphBuilder.pf`/callback-wiring test
  coverage must continue to pass unmodified (pure refactor for the
  callback-wiring side). New coverage: ordinary connection resolution
  against a real multi-level (parent/child/grandchild) synthetic
  hierarchy, where the connection names the parent and child but the
  real items live on the grandchild — the shape every real fixture
  already has and 3b's own equivalence check could never exercise.
- **Dependencies**: none new. Does not depend on
  `graph-builder-stateitem-resolution` or
  `3b2-cross-component-import-propagation` — this change is a pure
  traversal-primitive addition, independently landable; those two
  changes become its consumers afterward, not its prerequisites.
- **Out of scope**: any new graph structure creation from a lookup
  result (see "What Changes"); a complete nested-substate export-
  visibility capability (traversal primitive only, not the consuming
  capability).
