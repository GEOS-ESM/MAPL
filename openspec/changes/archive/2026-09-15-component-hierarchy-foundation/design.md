## Context

`ComponentGraph` (`superstructure/generic/graph/ComponentGraph.F90`) is
already implemented, tested, and graph-neutral (REQ-CG-002): it owns node
registration, dependency networks, and three `PortIdNodeIdMap`-backed
tables — `import_ports`, `export_ports`, `child_bindings` — reachable
through `add_import_port`/`get_import_port`,
`add_export_port`/`get_export_port`, and
`add_child_port_binding`/`get_child_port_binding`. Every `insert` into
those tables requires the bound `NodeId` to already be owned by that same
graph (`this%nodes%count(node_id) > 0`).

`OuterMetaComponent` (`superstructure/generic/OuterMetaComponent.F90`)
already has most of `OuterComponent`'s ownership shape: `user_gc_driver`
(own driver), `children` (`GriddedComponentDriverMap`, one driver per
child), `registry` (`StateRegistry`), and framework-managed states via
`memory_checkpoint`/`component_spec`. It has no `ComponentGraph` field.
See proposal.md - Why/What Changes for the resulting gap and why it must
close before sub-changes 3b-3d.

## Goals / Non-Goals

**Goals:**
- Give every `OuterMetaComponent` exactly one owned, private
  `ComponentGraph`, constructed alongside the rest of the type in
  `new_outer_meta`, reachable through one new read-only accessor.
- Preserve the encapsulation boundary at the Fortran API level: verify
  (via tests) that no existing or new accessor lets a child reach its
  parent's `OuterMetaComponent`/`ComponentGraph`, and none lets a parent
  reach a child's `NodeId`s or `DependencyNetwork` directly.
- Leave `ComponentGraph`, `GraphBuilder` (not yet started), and
  `StateRegistry` untouched.

**Non-Goals:**
- Populating `child_bindings`, `import_ports`, or `export_ports` with
  real proxy/state-item nodes. That is `GraphBuilder`'s advertising and
  connection-resolution logic (roadmap sub-change 3b), gated on
  side-by-side validation against the existing imperative coupler
  (`17-open-questions.md` Q10).
- Any new gFTL container type. `ComponentGraph`'s existing
  `PortIdNodeIdMap`-backed tables already satisfy REQ-HIER-002's "public
  ports" and REQ-HIER-006's proxy-node storage; see Decisions below.
- Changing `GriddedComponentDriverMap`/`add_child`/`get_child` semantics.

## Decisions

**Reuse `ComponentGraph`'s existing port tables instead of adding new
storage on `OuterMetaComponent`.** `import_ports`/`export_ports`/
`child_bindings` already exist, are already `PortIdNodeIdMap`-backed
(`PortId -> NodeId`), and already enforce "bound `NodeId` must be owned by
this graph." Duplicating that as a second table directly on
`OuterMetaComponent` would create two sources of truth for the same
concept and contradict REQ-CG-002 (component-hierarchy types building
graph structure outside `ComponentGraph`). Alternative considered: add a
`child_name -> NodeId` map directly on `OuterMetaComponent` for proxy
nodes, keyed by name instead of `PortId`, on the theory that a name is
more "component-hierarchy-native" than a graph-internal `PortId`. Rejected
because it forces `GraphBuilder` (3b) to keep two lookup structures in
sync (the name-keyed one and the `PortId`-keyed one) for the same
proxy-node concept.

**One `ComponentGraph` field, constructed unconditionally in
`new_outer_meta`.** Matches REQ-HIER-004 ("exactly one local
`ComponentGraph`", "no single global graph instance"). Alternative
considered: lazy/allocatable `ComponentGraph`, created on first use (same
pattern as `memory_checkpoint`'s `ensure_memory_checkpoint_`). Rejected:
`ComponentGraph`'s own default constructor (`new_ComponentGraph()`,
`ComponentGraph.F90:183`) is cheap (no I/O, no ESMF calls — pure
container initialization), so lazy allocation buys nothing but adds a
null-check burden to every future accessor. Every `OuterMetaComponent`
will need a graph eventually (3b populates it unconditionally during
advertising), so eager construction is simpler and matches the type's
existing pattern for `children`/`registry` (both eagerly-valid,
non-allocatable fields).

**Single read-only accessor (`get_component_graph`), no setter.** Mirrors
existing accessors (`get_registry`, `get_component_spec`): return a
pointer to the private field, mutated only through `ComponentGraph`'s own
type-bound procedures (`register_node`, `add_import_port`, etc.), never
replaced wholesale. Follows the existing `target`/`pointer`-result
convention used by `get_registry`/`get_component_spec`/`get_user_gc_driver`
(`OuterMetaComponent.F90:520-543`) rather than introducing a new access
pattern.

**No accessor added for "parent's `ComponentGraph`" from a child, and no
accessor added for "child's `NodeId`/`DependencyNetwork`" from a parent.**
This is purely a negative decision (an omission), verified by a dedicated
API-surface test (per proposal.md - Impact/Tests) rather than by a
compile-time mechanism, since Fortran has no first-class way to express
"this module's public API must never grow a particular signature" outside
of code review and tests.

## Risks / Trade-offs

- **[Risk]** A future contributor adds a convenience accessor exposing
  `ComponentGraph` node identities to a parent (or grandparent) without
  realizing it breaks REQ-HIER-005. **Mitigation:** the encapsulation-
  boundary test suite added by this change (proposal.md - Impact/Tests)
  should be treated as a standing regression gate, not a one-time check;
  call this out in the test file's header comment.
- **[Risk]** Eager `ComponentGraph` construction for every
  `OuterMetaComponent`, including components with no children and few
  state items, adds a small fixed per-component memory/init cost.
  **Mitigation:** `new_ComponentGraph()` only initializes empty gFTL
  containers and a few scalar generators/lifecycle flags — negligible
  relative to the `ESMF_GridComp`/`StateRegistry` machinery already
  allocated per component.
- **[Trade-off]** Because 3a intentionally does not populate the graph,
  a fully-wired `OuterMetaComponent`'s `ComponentGraph` will sit empty
  (frozen or not) until 3b lands. This is expected per the roadmap's
  explicit 3a/3b split and is not a regression relative to today (there
  is no graph at all today).

## Migration Plan

No migration: this is additive. Existing `OuterMetaComponent` construction
paths (`new_outer_meta`, `attach_outer_meta`) gain one more initialized
field; no existing field, accessor, or behavior changes. No feature flag
needed since nothing consumes the new `ComponentGraph` field yet (3b is a
separate, later change). Rollback is a plain revert of this change's
commits.
