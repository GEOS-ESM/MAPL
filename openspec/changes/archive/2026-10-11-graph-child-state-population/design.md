## Context

See proposal.md - Why. This fix closes the "`graph`-mode-alone
cross-component population" gap roadmap `docs/graph/spec/20-implementation-roadmap.md`
§20.4.8 names explicitly ("Explicitly out of scope... blocks any future
sub-change (10c/10d or Phase 6 itself)"), confirmed by 10b's own direct
experiment (`ESMF_RC_ARG_INCOMP` at `ESMF_StateAdd`). Everything the fix
needs already exists:

- `graphbuilder_populate_state`'s existing cross-component loop
  (`GraphBuilder.F90`) already iterates the parent's own declared
  children and already resolves each one's own proxy `NodeId` via
  `proxy_key(child_name, ...)` - it only ever inserts into the wrong
  destination `MultiState` (the parent's own `tmp_states`/
  `outer_states`, not the child's).
- `OuterMetaComponent%get_child_outer_meta(child_name)` already exists
  and is already used for the identical one-way parent-to-child reach
  by `graphbuilder_resolve_geometry`/`get_child_component_graph`/
  `get_child_component_spec` - confirmed by direct reading, no new
  accessor needed.
- `GriddedComponentDriver%get_states()` already returns the real
  `MultiState` (`importState`/`exportState`) a component's own
  `user_gc_driver` holds - exactly the destination this fix needs,
  already reachable via `child_meta%get_user_gc_driver()%get_states()`.
- `populate_one`'s existing field-resolution logic (export: direct;
  import: predecessor-walk through any extension chain) is already
  correct and is reused unmodified - only the destination `MultiState`
  argument it is called with changes for the cross-component case.

## Goals / Non-Goals

**Goals:**
- `graphbuilder_populate_state`'s cross-component loop inserts each
  child's own resolved field into that child's own real `ESMF_State`,
  not the parent's.
- A child component's own code/graph never needs, and is never given,
  any reference back to its parent to receive this population - the
  parent alone initiates the reach.
- A YAML-configured demonstration scenario, structurally close to a
  legacy `Test_Scenarios.pf` fixture, proves end-to-end: real transform
  chain fires, real child `ESMF_State` is populated, `StateRegistry`
  is not in the production path for that component.
- Zero behavior change under `GRAPH_MODE_REGISTRY` or `GRAPH_MODE_BOTH`
  - this fix is reached only from the `GRAPH_MODE_GRAPH` branch
    `graphbuilder_populate_state` already gates on
    (`_RETURN_IF(graph_mode() /= GRAPH_MODE_GRAPH)`, unchanged).

**Non-Goals:**
- Deeper-than-two-level hierarchies (grandchild connections, 3b2's own
  cross-component unresolved-import propagation). This fix is scoped
  to the flat, two-sibling-children shape every current
  `graph-scenarios` fixture already uses - the same scope 10a-10d's
  own fixture set has kept throughout.
- Phase 10d's run-phase invocation (double-invocation hazard,
  `GRAPH_MODE_GRAPH` legacy-run-call skip): constructing or invoking a
  `MethodGraphNode`, or calling a `GriddedComponentDriver%run()` a
  second time for the same phase. **Narrowed during implementation
  (D7)**: this change does add one new call site to `run_user.F90` -
  a plain `ComponentGraph%update()` pull, never a `MethodGraphNode`
  invocation - see D7 for why that is a structurally different,
  already-safe mechanism, not a reopening of this Non-Goal.
- Making `GRAPH_MODE_GRAPH` the production default anywhere, or
  retiring any `StateRegistry` code path (Phase 6's own job, gated on
  a much broader entry criterion than this one fix satisfies).
- Any change to `ExtensionResolution.F90`'s chain-building logic,
  `UnitsConverterTransform`/`TypeKindConverterTransform`, or
  `graph_mode()`'s own 3-way selector - all reused exactly as they
  exist today.

## Decisions

**D1: Where the fix lives.**
Inside `graphbuilder_populate_state`'s existing cross-component loop
only - not a new procedure, not a new call site, not a change to the
same-component (degenerate) branch above it. The loop already resolves
`child_name`/`var_spec`/`id_ptr` correctly; it only needs to look up
that child's own `OuterMetaComponent` (via `get_child_outer_meta`) and
pass that child's own `get_user_gc_driver()%get_states()` as the
destination `MultiState` to `populate_one`, instead of the single
`multi_state` argument the whole procedure receives today. Alternative
considered: have the *child's own* same-component populate-state call
(already correct for items it advertises to itself) somehow also
reach the sibling's resolved value. Rejected outright per the
project's own stated rule (user direction, this change's proposal.md):
a child SHALL NOT be given awareness of its parent; only the parent
reaching down is architecturally acceptable, matching
`get_child_component_graph`/`get_child_component_spec`'s own existing
one-way precedent.

**D2: `populate_one`'s own signature.**
`populate_one(graph, net, multi_state, id_ptr, state_intent, rc)`
already takes `multi_state` as a plain argument, not implicitly the
caller's own field - no signature change needed. The cross-component
loop simply computes a different `multi_state` value (the child's own)
per iteration instead of reusing the one value passed into
`graphbuilder_populate_state` for every call. The same-component
(degenerate) branch keeps using the original `multi_state` unchanged.

**D3: Reaching the child's own states.**
`get_child_outer_meta(child_name, rc)` returns
`type(OuterMetaComponent), pointer`; `%get_user_gc_driver()` returns
`type(GriddedComponentDriver), pointer`; `%get_states()` returns
`type(MultiState)` by value (confirmed,
`GriddedComponentDriver/get_states.F90`). Build the destination
`MultiState` fresh per child, once per iteration of the outer
`do i = 1, n` loop (not once per `var_spec`, to avoid redundant
lookups) - a child with multiple connected items reuses the same
looked-up `MultiState` value for all of them.

**D4: Export vs. import side - does the export child need touching
too?**
Yes, symmetrically. `populate_one`'s existing export branch
(`get_field_if_materialized`, direct) and import branch
(`find_materialized_predecessor`, walks the chain) are both already
called once per `var_spec` in the existing loop, each already keyed to
`var_spec%state_intent`. Both branches target the *same* child the
`var_spec` was read from (comp_src's own export VariableSpec resolves
against comp_src's own states; comp_dst's own import VariableSpec
resolves against comp_dst's own states) - D1's fix (look up the
current `child_name`'s own `MultiState` once per iteration) already
covers both sides correctly with no separate branching needed, since
the loop already iterates per-child, not per-connection-endpoint.

**D5: Fixture choice (reuse vs. new).**
Attempt `units_and_typekind_mismatch` (10c's own real two-characteristic
extension chain fixture, `superstructure/generic/tests/graph-scenarios/`)
unmodified first. It already has every shape this demonstration needs:
`cap`/`comp_src`/`comp_dst`, a real mismatch resolved through a real
transform chain (`UnitsConverterTransform` then
`TypeKindConverterTransform`), and hand-checked expected values already
written in its own `description.md`. If running it under
`GRAPH_MODE_GRAPH` surfaces anything that fixture's own existing
`has_run_phase=.false.` static-only shape cannot exercise (unlikely,
since this fix is about static, init-time population, not a run
phase), fall back to a new, distinctly-named fixture (e.g.
`units_and_typekind_mismatch_graph_mode/`) under the same directory -
never silently reusing the name for a fixture with different
semantics.

**D6: Test placement.**
A new pFUnit test added to `Test_GraphBuilder.pf`, mirroring that
file's own existing synthetic-component construction-test style
(`make_outer_meta`/`add_child`/`ChildSpec`, no real `ESMF_GridComp`
run) - the same style `test_exact_match_materializes_field_in_graph_mode`
already uses for the single-child-pair case, extended here to also
assert the *destination* `ESMF_State` is each child's own, not a
shared parent scratch state. This is the fast, synthetic-graph unit
test; the YAML-configured scenario (D5) is the separate, end-to-end
demonstration proposal.md's own Why section asks for - the two are
complementary, not redundant (unit test proves the mechanism in
isolation; the scenario proves the whole real lifecycle, including a
real transform, produces the right answer).

**D7: Re-propagating a run-phase value through the chain (found during
implementation - new scope, user-approved).**

**Why this came up:** D5's own demonstration scenario, once actually
run end-to-end, surfaced a real limitation beyond this change's
originally-scoped "static population" claim: `graphbuilder_populate_state`
only ever runs once, at `GENERIC_INIT_ACCEPT_TRANSFER` (init time) -
`comp_src.yaml`'s own `run: T: [10.]` section only writes `10.` into
`comp_src`'s export field when `GENERIC_RUN_USER` actually executes,
which happens strictly after init. Nothing re-populates `comp_dst`'s
own import state afterward, so a demonstration that calls a real run
phase and then expects to read `comp_dst`'s own correctly-converted
value would see stale/uninitialized data, not the fixture's own
hand-checked `1000.d0 Pa` - not a defect in D1-D6's own fix, but a
real gap in what "demonstration" needs to show to be convincing.

**Why this is safe, not a reopening of the Phase 10d Non-Goal.**
`ComponentGraph%update(network_id, node_id)` (Phase 1-2, already
landed, `graph/method-graph-node` capability's own REQ-NODE-007: "A
method node's invocation timing SHALL be decided entirely by whatever
code invokes it. Demand-driven graph update SHALL NOT invoke a method
node as a side effect...") is architecturally guaranteed, by the
already-landed `graph_update`/`finish_update_frame` dispatch
(`ComponentGraph_DemandDrivenUpdate.F90`'s own `class default` branch,
confirmed by direct reading), to skip any node that is not a
`StateItemNode` or `TransformGraphNode` - a `MethodGraphNode` is
never touched, and `update()` never calls `GriddedComponentDriver%
run()` under any circumstance. Phase 10d's own double-invocation
hazard was specifically about invoking a `MethodGraphNode`'s own
`GridCompDriverPhaseInvoker` adapter, which ends in a second
`driver%run()` call - this call never reaches that adapter, that
node type, or that driver method at all. It only re-executes
`TransformGraphNode%compute()` (`UnitsConverterTransform`/
`TypeKindConverterTransform`'s own `compute()`), walking plain data
dependencies the same way the equivalence harness's own `check()`
already reads payloads out-of-band.

**Where the call goes.** `OuterMetaComponent/run_user.F90`, immediately
after the existing `call this%user_gc_driver%run(phase_idx=phase, _RC)`
line - for the *same* reason the roadmap's own 10d entry picked this
exact call site ("the first point `GENERIC_RUN_USER` phases start
flowing through the graph"), but doing something categorically
smaller: for `this` component's own declared children (the same
enumeration `graphbuilder_populate_state`'s own cross-component loop
already walks), pull every child's own IMPORT proxy `NodeId`
(`proxy_key(child_name, IMPORT, short_name)`) via
`graph%update(graph%get_default_network_id(), proxy_id)`. No new
accessor, no new node type, no new adapter - `ComponentGraph%update()`
is called directly, the same type-bound call
`pull_callback_get_arguments`/`invoke_on_default_network` already make
internally. Gated by `graph_mode() == GRAPH_MODE_GRAPH` (a no-op
otherwise, matching every other new code path this change adds).

**Why `cap`'s own `run_user.F90` call, not `comp_dst`'s.**
`comp_dst` has no `run:` section of its own in this fixture - it is a
pure importer, and its own `run_user.F90` invocation (if it has a run
phase at all) has nothing to pull on from inside itself (D1's own
rule: a child has no awareness of, or reach into, its parent or
siblings). `cap`'s own `run_user.F90` invocation is what actually
calls `cap%user_gc_driver%run()`, which (via
`ConfigurableGridComp`'s `run_children_phases_`/`MAPL_GridCompRunChild`)
is what runs `comp_src` then `comp_dst` in sequence - by the time that
call returns, `comp_src`'s own write has already landed (ESMF handles
are shallow; `comp_src`'s own write lands directly in the memory the
chain's own input field aliases), so pulling immediately afterward, in
`cap`'s own `run_user.F90`, is the first point where re-pulling is
both correct and necessary. No change to `comp_dst`'s or `comp_src`'s
own `run_user.F90` invocation is needed - only `cap`'s.

**Why no second `populate_one`/`ESMF_StateAdd`-family call is needed.**
D1-D6's own fix already inserts a `MAPL_NamedAlias` (not a copy) of the
chain's final field into `comp_dst`'s own real import state, at init
time. Because that alias shares the chain's final field's own
underlying array (ESMF handles are shallow), re-executing the chain via
`graph%update()` updates that same array in place - `comp_dst`'s own
already-populated state sees the new value with no second insertion
call. This is the same shallow-aliasing property D1-D6 already relies
on; D7 only adds the one missing step (re-trigger the chain), not a
second population mechanism.

**D1-D6 correction, found during implementation: `populate_one` must
itself be idempotent and must alias by name, not insert the raw
field.** Actually running `GENERIC_INIT_PHASE_SEQUENCE`'s own fixed
two-pass `GENERIC_INIT_ACCEPT_TRANSFER` schedule against a child's own
now-PERSISTENT real state (D1's own fix, replacing the parent's
previously fresh-per-call scratch `MultiState`) surfaced two further,
real bugs in `populate_one` itself:
- A bare `ESMF_StateAdd` on the second pass fails outright (duplicate
  name) once the destination is persistent - fixed by switching to
  `ESMF_StateAddReplace`.
- `ESMF_StateAddReplace` matches/replaces purely by the inserted
  object's own NAME (confirmed: ESMF objects are shallow; matching is
  name-based, not object-identity-based) - but the chain's own final
  item is created with NO explicit name at all
  (`try_materialize_field_extension`'s mismatch-branch call site never
  passes `name=`), so two separate insertions of the same underlying
  field could never reliably re-match each other. Fixed per legacy's
  own `FieldClassAspect%add_to_state` precedent
  (`superstructure/generic/specs/FieldClassAspect.F90`): never insert
  the raw field - always insert a `MAPL_NamedAlias` of it, under the
  item's own known `short_name` (threaded through from each call
  site's own already-available `var_spec%short_name`), so every
  insertion of the same underlying item produces an alias with the
  identical, stable name `ESMF_StateAddReplace` can always match
  against.

**D8: Scope narrowed again, twice more, during implementation
(user-approved: "narrow the scope, add a roadmap work item, make it
work in a later session").** Attempting to actually demonstrate D7's
own pull step working end-to-end - a real run-phase value changing and
provably propagating through a real extension chain into a sibling's
own real state - surfaced two further, separate, real gaps, neither
specific to D1-D7's own fix, both confirmed by direct code reading
(not external-library behavior):

1. **Multi-hop chains have no materialized intermediate nodes.**
   `units_and_typekind_mismatch`'s own two-characteristic chain
   (`UnitsConverterTransform` -> `TypeKindConverterTransform`) has an
   INTERMEDIATE node between the two transforms that
   `try_materialize_field_extension` never touches - only the chain's
   own FINAL node is ever materialized. `UnitsConverterTransform%
   compute()`'s own `output_item%get_field()` call then fails outright
   (`GraphStateItem: esmf_field is not allocated`) once anything
   actually executes the chain for real (nothing ever had before this
   phase). Fixing this needs `ExtensionResolution.F90`'s own
   `find_or_build_extension_chain`/`build_chain` to expose every
   intermediate `NodeId`, not only the final one - a real change to
   that module's own public interface, which this change's own
   Non-Goals explicitly excludes ("Any change to
   `ExtensionResolution.F90`'s chain-building logic... reused exactly
   as they exist today," unchanged by D7's own narrower addition).
   **Worked around, not fixed**, by switching the demonstration
   fixture to a new, single-characteristic one
   (`units_mismatch_run_phase`, `units` only) whose one-hop chain has
   no distinct intermediate node to expose this on - its only "link"
   IS the chain's own final node, already correctly materialized by
   the existing, unmodified `try_materialize_field_extension` call.
2. **`resolve_one`'s own mismatch branch never materialized the
   chain's own INPUT node either** (`export_node_id`) - only the
   exact-match branch (a separate code path) and the chain's final
   node were ever materialized. **Fixed in this change**
   (`GraphBuilder.F90`, `resolve_one`'s mismatch branch): now also
   calls `try_materialize_field_extension` on `export_node_id`, passing
   `export_var_spec` for BOTH the export and import `VariableSpec`
   arguments - that procedure's own contract always materializes using
   the SECOND argument's typekind/units, so passing `export_var_spec`
   twice is what correctly resolves to the export's own values here
   (as opposed to the chain's final node, which correctly keeps using
   the import's own values via the existing, unchanged call).
3. **Even with (2) fixed, the export proxy's own materialized field
   is an INDEPENDENT `FieldCreate` - never aliased to `comp_src`'s own
   real export field.** `get_or_make_local_node_id` creates
   `export_node_id` as a brand-new proxy in `cap`'s own graph; nothing
   makes its own field a `MAPL_NamedAlias` of `comp_src`'s own real
   export field (the one `ConfigurableGridComp%run()` actually writes
   the YAML-configured `run:` value into). A real run phase's changed
   value therefore never reaches the proxy at all - confirmed by a
   resulting `NaN` (not a crash) when the demonstration test asserted
   the real converted value after a real run phase. **Not fixed** -
   this is the real remaining blocker to the actual goal (a changing
   run-phase value provably propagating end-to-end); fixing it needs
   the same "always alias, never duplicate" treatment D1-D6 already
   applied to the *import* (sibling-receiving) side, mirrored onto the
   *export* (sibling-producing) side.

**Final scope, this change.** The demonstration test
(`Test_GraphModeDemonstration.pf`) asserts only what is now actually
proven: `comp_dst`'s own real import state has exactly one entry
(sole producer, `StateRegistry` never ran for it), the export proxy
has a real, allocated field (gap 2's own fix, confirmed structurally),
and the chain's materialized attributes (typekind/units) match the
import's own declared requirements. It does NOT assert a propagated
numeric value - gap 3 blocks that, and is tracked as a roadmap work
item (`docs/graph/spec/20-implementation-roadmap.md` §20.4.9,
"Phase 11"), not fixed here. D7's own pull hook
(`graphbuilder_run_pull_hook`) is still exercised by the demonstration
test (it runs with no crash, confirming its own architectural safety
claim), but its own observable effect on a changing value cannot yet
be asserted until gap 3 closes - a future session's job, per user
direction, not this change's.

## Risks / Trade-offs

- **[Risk]** `graphbuilder_populate_state`'s existing same-component
  (degenerate) branch and this fix's cross-component branch now
  resolve destination states differently (one argument-supplied, one
  looked-up per child) - a future reader could assume they share one
  destination-resolution helper when they do not. -> **Mitigation:**
  keep the two branches' own existing structure (already two separate
  loops in the current code) and add a comment at the cross-component
  loop's own call site explaining why its destination differs from the
  degenerate branch's.
- **[Risk]** D7 is a new, if narrow, production call site added to
  `run_user.F90` - a file every real GridComp phase invocation already
  passes through, including configurations that have never opted into
  `GRAPH_MODE_GRAPH` at all. -> **Mitigation:** gated identically to
  every other new code path this change adds
  (`_RETURN_IF(graph_mode() /= GRAPH_MODE_GRAPH)`, checked first, before
  any child enumeration) - a strict no-op under the default
  (`GRAPH_MODE_REGISTRY`) mode and under `GRAPH_MODE_BOTH`, matching
  this change's own existing "zero behavior change" Goal.
- **[Risk]** Pulling on every declared child's own import proxy, every
  time `run_user.F90` runs for the parent, could become real
  per-timestep overhead for a configuration with many children/items
  once `GRAPH_MODE_GRAPH` is used for something larger than this
  change's own two-sibling demonstration. -> **Mitigation:** out of
  scope for this change (its own Non-Goal: not attempting to make
  `GRAPH_MODE_GRAPH` production-ready at scale) - `ComponentGraph%
  update()`'s own revision-check-before-execute discipline
  (`TransformGraphNode%needs_execution()`, already landed) already
  makes a pull against an unchanged input cheap (no re-computation),
  so the realistic cost is one revision check per declared child-item,
  not one transform re-execution - acceptable for this change's own
  demonstration-scale scope; a future change scaling `GRAPH_MODE_GRAPH`
  up would need its own cost analysis, not reopened here.
- **[Risk]** `get_child_outer_meta` is `intent(inout)` on `this`
  (confirmed, `OuterMetaComponent.F90`) even though this fix only
  reads from the result - calling it once per child per
  `graphbuilder_populate_state` invocation is a cheap, already-paid
  cost (`graphbuilder_resolve_geometry` already does the same per-call
  pattern), not a new performance concern. -> **Mitigation:** none
  needed; noted only so a future reader does not mistake the
  `inout` for an unintended mutation - the existing accessor's own
  implementation performs no mutation despite the dummy argument's
  declared intent.
- **[Risk]** Reusing `units_and_typekind_mismatch` (D5) under a new
  graph mode could regress its own existing `BOTH`-mode equivalence-
  harness coverage if anything about this fix's own code path is
  accidentally shared with that mode's gated branch
  (`graphbuilder_populate_state`'s own existing
  `_RETURN_IF(graph_mode() /= GRAPH_MODE_GRAPH)` guard). ->
  **Mitigation:** that guard is unchanged by this fix - `BOTH` mode
  still returns immediately, before reaching any of the new per-child
  lookup logic; re-run `Test_GraphEquivalenceHarness.pf`'s own existing
  `units_and_typekind_mismatch` case unmodified as part of this
  change's own regression check (tasks.md) to confirm.

## Migration Plan

No migration needed for existing production code:
`graphbuilder_populate_state` is already gated behind
`graph_mode() == GRAPH_MODE_GRAPH`, which no real production run
selects today (default `GRAPH_MODE_REGISTRY`). This fix changes
behavior only for test/demonstration code that explicitly opts into
`GRAPH_MODE_GRAPH` for a two-sibling-children fixture - exactly the
shape this change's own new test and scenario exercise. No rollback
mechanism beyond the existing graph-mode selector is needed.
