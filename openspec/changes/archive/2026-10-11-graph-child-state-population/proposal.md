## Why

Phase 10's own validation work (10b) confirmed, by direct experiment,
that `GRAPH_MODE_GRAPH` cannot yet act as the sole producer of a real
`ESMF_State` for the one connection shape every current graph-scenarios
fixture uses: a parent (`cap`) declaring an ordinary connection between
two of its own children (`comp_src`/`comp_dst`). Attempting the switch
fails immediately (`ESMF_RC_ARG_INCOMP` at `ESMF_StateAdd`,
`GraphBuilder.F90` `populate_one`) because
`graphbuilder_populate_state()`'s cross-component walk only ever inserts
a resolved field into the *parent's own* scratch `MultiState` — never
into either child's own real import/export `ESMF_State`, which is the
only state either child's own GridComp actually observes.

This is a narrower, already-isolated gap than Phase 10d's abandoned
run-phase invocation attempt: it is pure init-time state population
(`GENERIC_INIT_ACCEPT_TRANSFER`), and the double-invocation hazard that
blocked 10d (a second `GriddedComponentDriver%run()` call for the same
phase) does not apply to anything proposed here.

**Scope narrowed twice during implementation (final; see design.md D8
for the full account).** The original goal was a full end-to-end
demonstration: a real run-phase value changing and provably
propagating through a real extension chain into a sibling's own real
state. Attempting that surfaced three separate, real, previously-
undiscovered gaps, each confirmed by direct code reading:

1. A multi-hop extension chain's own intermediate node(s) are never
   materialized (only the chain's final node is) - exposed only once
   something actually executes the chain for real, which nothing had
   before this change.
2. `resolve_one`'s own mismatch branch never materialized the chain's
   own INPUT node either - **fixed in this change**.
3. Even with (2) fixed, the export-side proxy's own materialized field
   is an independent `FieldCreate`, never aliased to the real export
   field a run phase actually writes - **not fixed**, confirmed by a
   resulting `NaN` rather than a crash.

(1) was worked around by switching the demonstration fixture to a
single-characteristic chain (no distinct intermediate node to expose
it on). (3) remains open and is tracked as a roadmap work item
(`docs/graph/spec/20-implementation-roadmap.md` §20.4.9, "Phase 11"),
not attempted here, per explicit user direction to narrow scope now
and continue in a later session. What ships in this change is
therefore the static, init-time half of the original goal: `GraphBuilder.F90`'s
cross-component population fix (gap-free, idempotent, verified against
both the equivalence harness and this change's own new tests), plus a
D7 pull-hook addition whose own architectural safety (never invokes a
`MethodGraphNode`, never re-runs a driver) is exercised and confirmed,
even though its effect on a changing value cannot yet be asserted
until gap (3) closes.

The fix also establishes, as an explicit architectural rule going
forward (not merely an implementation detail of this one fix): a
parent component MAY reach into a child's own state to populate it;
a child component SHALL NOT be given, or need, any awareness of its
parent to receive that population. This mirrors the existing
`get_child_component_graph`/`get_child_component_spec` one-way
visibility shape already established for other framework-internal
parent-to-child reaches.

## What Changes

- `GraphBuilder.F90`'s `graphbuilder_populate_state`, in its ordinary
  cross-component walk (the loop over a parent's own declared
  children), inserts each child's own resolved import/export field
  into **that child's own** real `ESMF_State` — reached via the
  already-existing `get_child_outer_meta(child_name)%get_user_gc_driver()%get_states()`
  one-way parent-to-child accessor chain — instead of into the
  parent's own scratch `MultiState`. The parent's own same-component
  walk (an item the parent itself advertises and connects to itself)
  is unchanged. `populate_one` now inserts a `MAPL_NamedAlias` of the
  resolved field (under the item's own known short name), via
  `ESMF_StateAddReplace`, rather than the raw field via a bare
  `ESMF_StateAdd` — needed once the destination became a persistent
  real state (see design.md D1-D6's own correction).
- `resolve_one`'s own mismatch branch now also materializes the
  chain's own INPUT node (`export_node_id`), using the export's own
  typekind/units — previously never materialized at all by that
  branch (only the exact-match branch and the chain's final node
  were). See design.md D8 item 2.
- A new, distinctly-named, single-characteristic (`units` only)
  fixture, `units_mismatch_run_phase`, under
  `superstructure/generic/tests/graph-scenarios/` — a one-hop
  extension chain, deliberately avoiding the multi-hop
  intermediate-node gap design.md D8 names as a separate, unfixed
  follow-up. The demonstration test running against it asserts
  `comp_dst`'s own real import `ESMF_State` has exactly one entry
  (sole producer, `StateRegistry` never ran) and that the export
  proxy has a real, allocated field with the correct typekind/units —
  it does NOT assert a propagated run-phase numeric value (design.md
  D8's own gap 3, not fixed in this change).
- A new pFUnit test exercising this fix directly against
  `graphbuilder_populate_state` (synthetic parent/child components,
  mirroring `Test_GraphBuilder.pf`'s own existing construction-test
  style) asserting the child's own real `ESMF_State`, not the
  parent's, receives the field.
- `OuterMetaComponent/run_user.F90` gains one new call, immediately
  after the existing `call this%user_gc_driver%run(phase_idx=phase,
  _RC)` line: for each of `this` component's own declared children,
  pull every child's own import proxy `NodeId` via
  `graph%update(graph%get_default_network_id(), proxy_id)` - re-
  triggering any extension-chain transform whose input changed during
  the just-completed run phase. Gated by `graph_mode() ==
  GRAPH_MODE_GRAPH` (a no-op otherwise). Calls `ComponentGraph%update()`
  directly - never a `MethodGraphNode`, never a second `driver%run()`
  - see design.md D7. Exercised (runs without error) but its own
  effect on a changing value is not yet assertable (design.md D8's
  own gap 3).

## Capabilities

### New Capabilities
None.

### Modified Capabilities
- `graph/graph-builder`: the existing "Graph-native state population
  populates a real ESMF state from resolved graph structure"
  requirement is clarified and extended to specify *whose* real
  `ESMF_State` receives a resolved item when the connection being
  populated is between two sibling children of the component running
  population — today's requirement text is silent on this, and the
  current implementation gets it wrong (inserts into the parent's own
  state instead of the child's).
- `graph/graph-mode`: the existing "`graph` mode makes graph-native
  population the real producer" requirement's "legacy population is
  skipped... graph-native population is the only producer" scenario is
  extended to state explicitly that this holds for the cross-component
  (sibling-to-sibling) case too, not only the same-component case -
  today's requirement text does not distinguish the two, and the
  cross-component case does not yet satisfy it.

## Impact

- **Affected code:** `superstructure/generic/GraphBuilder.F90`
  (`graphbuilder_populate_state`'s cross-component loop,
  `populate_one`'s destination-state argument and
  `MAPL_NamedAlias`/`ESMF_StateAddReplace` fix, `resolve_one`'s
  mismatch-branch export-node materialization fix, a new
  `graphbuilder_run_pull_hook` procedure, design.md D7),
  `superstructure/generic/tests/graph-scenarios/units_mismatch_run_phase/`
  (new fixture, design.md D8), `superstructure/generic/tests/Test_GraphBuilder.pf`
  (new unit tests), `superstructure/generic/tests/Test_GraphModeDemonstration.pf`
  (new file), `superstructure/generic/OuterMetaComponent/run_user.F90`
  (one new call, design.md D7).
- **Not affected:** `graphbuilder_build_run_phase_node`/
  `run_phase_method_node_id` (the already-landed, construction-only
  Phase 10d work - still never invoked, still untouched);
  `Test_GraphEquivalenceHarness.pf` and its existing `BOTH`-mode
  fixtures, including `units_and_typekind_mismatch` (confirmed
  unaffected by regression run - this change targets `GRAPH_MODE_GRAPH`
  specifically, a different mode with a different, already-isolated
  gap); `mapl_ExtensionResolution_mod`'s own chain-building interface
  (`find_or_build_extension_chain`/`build_chain` - design.md D8's own
  gap 1 names a real need to change this, deliberately not attempted
  here); `UnitsConverterTransform`/`TypeKindConverterTransform`
  (reused unmodified); `ComponentGraph%update()` itself (called, never
  modified).
- **Downstream:** this is the first production code path to populate a
  real `ESMF_State` for a sibling-to-sibling connection entirely
  through the graph, with `StateRegistry` genuinely out of the
  picture for that component (static, init-time case) - a real
  advance toward, but not yet a complete demonstration of, the
  "demonstrate before replace" proof point several earlier Phase 10
  sub-changes (10a's own framing) deferred. The remaining gap (design.md
  D8's own gap 3 - a run-phase value provably propagating through a
  real extension chain into a sibling's own real state) is tracked as
  a new roadmap phase (`docs/graph/spec/20-implementation-roadmap.md`
  §20.4.9, "Phase 11"), to be picked up in a later session. Does not
  itself attempt Phase 6's legacy retirement, 3b2's cross-component
  unresolved-import propagation, or Phase 10d's run-phase invocation -
  all remain separately scoped, unblocked gaps this change does not
  reopen.
