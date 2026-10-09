## Context

`GraphBuilder.F90`'s `resolve_one()` already has two branches for a
matched (export, import) pair: an exact-match branch that only calls
`this_graph%add_dependency(net_id, export_node_id, import_node_id)`, and
a mismatch branch that delegates to `mapl_ExtensionResolution_mod`'s
`find_or_build_extension_chain`, then — only when
`materialize_extensions_enabled()` is true — calls
`try_materialize_field_extension()`, which resolves the export's own
geom/vgrid (`resolve_export_geom`/`resolve_export_vgrid`, already
written, Field-item-only) and calls
`mapl_ExtensionMaterialization_mod::materialize_field_extension()` to
`FieldCreate()` a real field onto the extension chain's own node. The
exact-match branch has no equivalent: the export's own `StateItemNode`
is never given a real field, so nothing exists for a future state-
population step to find. Separately, `mapl_GraphMode_mod` already exists
as a single binary flag (`graph_native_enabled()`/
`set_graph_native_enabled()`), consulted today only by
`graphbuilder_run_geometry_hook()` — a purely additive, graph-side-only
hook with no real-`ESMF_State` interaction, so the binary flag has never
had to answer "who writes the real state" until now.

See `proposal.md` for full motivation; this document covers how.

## Goals / Non-Goals

**Goals:**
- Materialize a real field on the export side of an ordinary (exact-
  match) resolved connection, reusing `try_materialize_field_extension`'s
  existing geom/vgrid resolution rather than duplicating it.
- Add a graph-side equivalent of `StateRegistry%add_to_states` that can
  insert a resolved item's real field into a real `ESMF_State` under its
  own short name, including resolving an import's real field by walking
  back through whatever produced it (direct export, or an extension
  chain's own final node).
- Replace `mapl_GraphMode_mod`'s binary flag with the 3-way
  `registry`/`graph`/`both` selector proposal.md describes, updating the
  one existing call site (`graphbuilder_run_geometry_hook`) to consult
  it, with no behavior change to that hook's own decision (it remains
  active for both `graph` and `both`, exactly as it was for the old
  flag's `.true.`).

**Non-Goals:**
- `TypeKindCharacteristic` wiring into `build_characteristics()`, a real
  precision-cast transform, `MethodGraphNode` construction for an
  ordinary run phase, or any generalized equivalence-harness pFUnit
  suite — all 10b/10c/10d's own scope (§20.4.8), sequenced after this
  sub-change.
- Turning `graph` mode on end-to-end against any real fixture — per
  proposal.md and §20.4.8, `graph` mode is built and directly
  unit-testable here, but "demonstrate before replace" means no fixture
  in 10a-10d actually runs with it selected.
- Any change to composite-item materialization (`materialize_composite`)
  or non-Field item classes — scope stays identical to
  `try_materialize_field_extension`'s own existing Field-only
  restriction.
- Any change to `ComponentGraph`/`DependencyNetwork`/`StateItemNode`
  public APIs — this sub-change is a new call site and one new
  procedure, not a core-graph-type change.

## Decisions

**Reuse, don't duplicate, the mismatch branch's own geom/vgrid
resolution.** `try_materialize_field_extension(this, export_comp_name,
export_var_spec, import_var_spec, graph, node_id, failure_reason, rc)`
already does exactly the resolution this sub-change needs (Field-only
check, `resolve_export_geom`/`resolve_export_vgrid`, optional vgrid/
staggerloc handling, `materialize_field_extension()` call) — it is
generic over which `node_id` it materializes onto. The exact-match
branch in `resolve_one()` calls the same procedure, passing
`export_node_id` (not an extension chain's final node) as `node_id`,
gated by the new 3-way selector (`graph`/`both`, not `registry`) instead
of `materialize_extensions_enabled()` — a different, orthogonal gate
with a different owner (this sub-change's own mode, not the extension-
reuse capability's flag). `try_materialize_field_extension` itself does
not change.

**A materialized item must be findable by name once it is really
inserted into an `ESMF_State` — today's extension items never are.**
`materialize_field_extension()`'s own `FieldCreate()` call passes no
`name=` argument, because neither existing caller ever inserts the
result into a real `ESMF_State` (the mismatch-branch extension item is
compared via `node%get_payload()%get_field()` directly, never via
`ESMF_StateGet(state, name, field)`). Graph-native state population
(below) does need name-based lookup to match legacy's own
`ESMF_StateGet`-by-name contract. Decision: extend
`materialize_field_extension()`'s own signature with an optional
`name` argument, passed through to `FieldCreate(name=name, ...)`;
omitted (today's behavior, unnamed field) by the existing mismatch-
branch call site, and supplied (the var_spec's own `short_name`) by this
sub-change's new exact-match-branch call site. Backward compatible — no
existing call site's behavior changes.

**State-population import resolution walks the dependency network's own
predecessor edge, not a parallel bookkeeping structure.** Given an
import's `NodeId`, its real field is whatever real-field-bearing node is
upstream of it in the default network — found by following
`network%get_predecessors()` from the import node until a node whose
`GraphStateItem%itemType()` reports `ESMF_STATEITEM_FIELD` is reached (or
no predecessor exists, meaning unresolved — skip, per spec "Unresolved
import is not populated"). This mirrors `ExtensionResolution.F90`'s own
`build_chain`/`SetSharedCharacteristic.F90`'s existing
`get_successors`-walk precedent in shape (a network-adjacency walk, not a
new side-table), just in the opposite (predecessor) direction, and
collapses correctly for the exact-match case (single hop: import's
predecessor *is* the export) without any special-casing versus the
extension-chain case (predecessor chases through one or more transform
nodes to the chain's own real-field-bearing final node).

**Graph-native state population is a new procedure
(`graphbuilder_populate_state` or equivalent), not a mode branch bolted
onto `StateRegistry%add_to_states`.** `StateRegistry%add_to_states`
reads `StateRegistry`'s own `family_map`/`ExtensionFamily` structures
directly - it has no graph-side equivalent to branch into without
pulling `ComponentGraph` types into `StateRegistry`, which would invert
REQ-GB-001/002's one-directional dependency (`GraphBuilder` depends on
`StateRegistry`/`OuterMetaComponent`, never the reverse). Instead, a new
procedure in `GraphBuilder.F90`, called from `initialize_accept_transfer.F90`
immediately after `gb%run_connect_hook(this)` (mirroring where legacy's
own `add_to_states(mode='user', ...)` already runs in that same file),
gated so it only ever inserts into a real `ESMF_State` when the mode
selector reports `graph` (spec "Graph mode governs whether population
touches the real state").

**Correction, found while writing this sub-change's own pFUnit coverage
(before any task was marked complete), not merely a test-shape
question.** The first draft of this decision walked `this`'s own
`ComponentSpec%var_specs` via `item_key` only, mirroring
`advertise_one`'s own enumeration. That only ever finds a materialized
node for a connection declared and resolved entirely within one
component - a shape no real fixture uses.
`get_or_make_local_node_id()` (used by both `add_dependency`'s own
endpoints and this sub-change's new materialization call in
`resolve_one`) ALWAYS returns a PARENT-local proxy for any non-`<self>`
`comp_name` - never either child's own native `StateItemNode`. For the
ordinary cross-component case every real fixture actually uses (a
parent declares a `MatchConnection` between two of its own children,
exactly `Test_GraphBuilderEquivalence.pf`'s own fixture family), the
dependency edge AND the materialized payload both live only in the
PARENT's own graph, on the PARENT's own two proxies - neither child's
own native export/import node is ever touched. Since
`initialize_accept_transfer.F90` calls this procedure once per
component in the recursive tree (including each child, on itself),
walking only `this`'s own `var_specs`/`item_key` would find nothing
whenever `this` is one of the two connected children - the only time it
is ever actually called for a real fixture.

Fixed: the procedure also walks each of `this`'s own declared
children's `VariableSpec`s (`get_num_children`/`get_child_name`/
`get_child_component_spec` - the same enumeration
`graphbuilder_resolve_geometry`'s own push-direction loop already uses),
looking each one up via `proxy_key(child_name, state_intent,
short_name)` in `this`'s OWN graph instead of `item_key` - exactly the
resource-index convention `get_or_make_local_node_id` itself uses to
create that same proxy. A proxy only exists if some connection `this`
declared actually referenced that child item, so an item with no proxy
(never connected) is silently skipped, same as an unresolved import. The
original same-component (`item_key`-based) walk is kept as the
degenerate case for an item `this` advertises and connects entirely to
itself, if that ever occurs. Either way, a resolved item's real field is
found per the predecessor-walk decision above, and inserted into the
passed-in `MultiState`'s import or export `ESMF_State` (whichever the
item's own `state_intent` says). `ESMF_StateAdd` itself takes no `name=`
override - a field is findable by its own short name in an `ESMF_State`
only because it was already created with that name
(`materialize_field_extension`'s own `name` argument, set by
`resolve_one`'s exact-match branch to the export's own `short_name`;
correct for every fixture this phase exercises, since an exact match's
import and export share one short name by definition). INTERNAL items
and non-Field items are skipped (same Field-only scope as
materialization).

**`mapl_GraphMode_mod`'s 3-way selector replaces, rather than wraps, the
old binary API.** `set_graph_native_enabled(logical)`/
`graph_native_enabled()` are removed; replaced by `set_graph_mode(mode)`/
`graph_mode()` taking/returning one of three named values (`registry`/
`graph`/`both` — represented as a small enumerated type or constrained
character value, implementation's choice, following whichever of
`mapl_VerticalStaggerLoc_mod`'s enum-type precedent or a plain
`character(:)` the implementer finds cleaner; this design does not
mandate one). The one existing call site
(`graphbuilder_run_geometry_hook`'s `if (.not.
graph_native_enabled()) return`) is updated to `if (graph_mode() ==
MODE_REGISTRY) return` (or equivalent) — same observable decision
(active for the old flag's `.true.`, which now maps to both `graph` and
`both`), no behavior change for that hook. `Test_GraphGeometryHook.pf`'s
existing calls to the old setter are updated to call the new setter with
`graph` (the nearest equivalent of "the flag that used to mean `.true.`
for that hook's own purposes).

**`GRAPH_MODE_GRAPH` skips legacy's own `add_to_states` entirely, per the
graph-mode capability's own spec contract.** The graph-mode spec's
"`graph` mode makes graph-native population the real producer"
requirement is explicit that legacy's own population step SHALL NOT run
in `GRAPH_MODE_GRAPH` - not merely that graph-native population also
runs. `initialize_accept_transfer.F90`'s two existing
`this%registry%add_to_states(...)` calls (`mode='user'` and
`mode='outer'`) are therefore each wrapped in `if (graph_mode() /=
GRAPH_MODE_GRAPH) then ... end if`, alongside the new
`gb%run_populate_state_hook(this, tmp_states)` call. This is wired even
though `GRAPH_MODE_GRAPH` is never selected end-to-end by any fixture in
this phase (the Non-Goal above) - the spec requirement is about what the
code does when that mode is selected, not about this phase exercising
it; leaving the skip unwired would make the graph-mode spec's own
requirement false for code nobody has tested, which is a worse state
than code that is correct-by-inspection but untested end-to-end.
Verified only via `mapl_GraphMode_mod`'s own direct unit coverage
(selector round-trip) and code-level review - no new end-to-end fixture
is added solely to exercise this skip, consistent with the "demonstrate
before replace" Non-Goal above.

**`both` mode's no-collision guarantee is structural, not a runtime
check.** The exact-match-branch materialization call and the new
population procedure are both gated by the *same* selector read
(`graph_mode() /= MODE_REGISTRY` for materialization;
`graph_mode() == MODE_GRAPH` for population's real-`ESMF_State` insert).
Because population's real insert is gated strictly to `MODE_GRAPH` (never
`MODE_BOTH`), `both` mode can never reach the `ESMF_StateAdd`-equivalent
call — there is no code path in `both` mode that touches the shared
`ESMF_State` on graph-native's behalf, so the "no collision by
construction" claim (proposal.md, graph-mode spec) does not depend on
any new runtime guard; it falls out of which branch is reachable.

## Correction: pre-existing geometry-hook phase-timing bug, found and fixed

**Found while writing task 4.4's own pFUnit coverage** (the real,
DSO-backed, multi-rank `graphbuilder_equivalence` fixture run in
`GRAPH_MODE_BOTH`), not by any change this sub-change's own new code
makes - `mapl_GraphMode_mod`'s selector is also consulted by the
already-landed (`horizontal-geometry-graph-state-item`, Phase 4e)
geometry hook (`graphbuilder_run_geometry_hook`), since both share one
selector by design (this document's own Decision above). Turning the
selector on for the first time against a real (non-synthetic) fixture
exposed that the hook was wired to the wrong lifecycle phase:
`initialize_advertise.F90` called it at `GENERIC_INIT_ADVERTISE`, but
`GenericPhases.F90`'s own `GENERIC_INIT_PHASE_SEQUENCE` runs
`GENERIC_INIT_GEOM_A`/`GENERIC_INIT_GEOM_B` **after** `ADVERTISE`, not
before it - the opposite of what that hook's own module-header comment
claimed ("entirely before `GENERIC_INIT_ADVERTISE`"). Concretely: a
`GEOMETRY_PROVIDER` component's own `this%has_geom()` is still `.false.`
at `ADVERTISE` time, so `graphbuilder_advertise_geometry()` silently
no-ops, and `push_geometry_to_child()` (called moments later for any
`GEOMETRY_FROM_PARENT` child) then hits its own `_ASSERT` - "called
before this component advertised its own geometry" - every time.
Reproduced deterministically (moved with the test, not order-dependent,
confirmed by relocating the failing test within its own file and
watching the failure follow it) on a real DSO-backed fixture;
`Test_GraphGeometryHook.pf`'s existing synthetic tests never caught this
because they call `set_geom()` directly and invoke the hook manually,
bypassing the real lifecycle's own `GEOM_A`/`GEOM_B` timing entirely.

**Fix**: moved the `gb%run_geometry_hook(this)` call from
`initialize_advertise.F90` to `initialize_geom_b.F90`, positioned after
that file's own `recurse(this, phase_idx=MAPL_GENERIC_INIT_GEOM_B, ...)`
call. By that point, every component's own geom is real - a
`GEOMETRY_PROVIDER`'s own geom is set at the very start of its own
`GEOM_A`, well before `GEOM_B` starts; a `GEOMETRY_FROM_PARENT` child's
own geom is set by `initialize_geom_b`'s own
`propagate_geom_to_children()` call, which runs before recursion - and
the bottom-up ordering the hook's own contract requires (every child's
own geometry node already registered before a parent's own hook runs)
is preserved, since `recurse()` still runs before the hook call, just in
`GEOM_B` instead of `ADVERTISE`. No change to the hook's own internal
logic, the 3-way selector, or any `Test_GraphGeometryHook.pf` test (that
suite calls the hook directly, independent of either lifecycle call
site, and continues to pass unmodified). Corrected the module header's
own wrong phase-ordering claim in the same pass.

## Risks / Trade-offs

- **[Risk] Extending `materialize_field_extension()`'s signature touches
  a procedure the already-landed `extension-registry-visibility` change
  depends on.** → **Mitigation:** the new `name` argument is optional
  and defaults to today's unnamed-`FieldCreate` behavior; the existing
  mismatch-branch call site is not changed to pass it, so
  `Test_ExtensionMaterialization.pf` and
  `Test_GraphBuilderEquivalence.pf`'s existing mismatch-branch test keep
  passing unmodified.
- **[Risk] Predecessor-walk for state population silently picks the
  wrong node if more than one predecessor ever exists for an import
  node.** `add_dependency`'s own validation (already landed, "two
  producers for the same state item" check in `graphbuilder_freeze`)
  already rejects a configuration with more than one real producer for
  one import before this sub-change's code ever runs, so a frozen
  graph's import node is guaranteed single-predecessor by construction
  on the exact-match/extension-chain paths this sub-change's fixtures
  exercise → **Mitigation:** rely on that existing invariant rather than
  re-validating it; an explicit internal assertion (not a new user-
  facing error path) is reasonable defensive coding but not a new spec
  requirement.
- **[Risk] `graph` mode's population code path has no end-to-end fixture
  in this phase, per proposal.md's explicit deferral — a latent bug
  could sit unexercised until 10b/later work turns the mode on for
  real.** → **Mitigation:** direct (non-end-to-end) pFUnit coverage of
  the population procedure itself, called directly against a
  hand-built `ComponentGraph`/`MultiState` pair (same style
  `Test_GraphBuilder.pf`'s existing synthetic-graph tests already use),
  is required by this sub-change's own task list even though no real
  YAML fixture runs in `graph` mode.

## Open Questions

- Exact representation for the 3-way mode value (small derived
  enumerated type vs. constrained `character(:)`) — left to the
  implementation step; does not affect this design's decisions, specs,
  or task breakdown either way.
