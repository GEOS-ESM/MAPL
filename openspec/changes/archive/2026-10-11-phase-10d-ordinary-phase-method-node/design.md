## Context

See proposal.md - Why, including the "Re-scoped during implementation"
section there, which this design.md assumes as read. This document
originally covered construction + invocation + a fixture/harness
extension; invocation and the fixture/harness work were abandoned, not
deferred, after direct code tracing found two real blockers
(double-invocation of the user's own GridComp run phase; no safe mode
to validate re-materialization under). What remains is construction
only.

Everything construction needs already exists and is confirmed
unmodified-reusable by direct code reading:

- `GraphBuilder.F90`'s `build_callback_method_binding` - established,
  directly-mirrorable 3-step construction shape
  (`declare_argument`/`bind_argument`/`register_node`), differing only
  in where the argument list comes from.
- `GridCompDriverPhaseInvoker` (Phase 4b) and
  `OuterMetaComponentDriverResolver` (Phase 4b) - already generic/real;
  reused here only to build the node's attached adapter (never invoked
  by this change).
- `mapl_GraphMode_mod` (Phase 10a) - already exposes the 3-way
  selector; `graph_mode()` defaults to `GRAPH_MODE_REGISTRY`.

## Goals / Non-Goals

**Goals:**
- Construct exactly one run-phase `MethodGraphNode` per component,
  sourced from that component's own advertised ports, gated by the
  existing graph-mode selector.
- Zero behavior change under the default (`registry`) graph mode.
- Zero new invocation/adapter code - the node's attached adapter is
  built from existing Phase 4b types, never a new one.

**Non-Goals (explicit, not merely deferred - see proposal.md Why):**
- Any invocation call site. `run_user.F90` is not touched; nothing
  anywhere calls `invoke()`/`invoke_on_default_network` on the
  constructed node.
- Any fixture or `Test_GraphEquivalenceHarness.pf` change.
- Resolving the double-invocation hazard (a legacy-run-call skip
  mechanism) or the `GRAPH_MODE_GRAPH`-alone population gap - both
  named in proposal.md as real blockers for any future invocation-
  wiring attempt, neither attempted here.
- Supporting more than one run phase per component, or any phase other
  than `GENERIC_RUN_USER`. Moot for this change (no invocation call
  site exists to scope), but still true of the construction shape
  itself: one node per component, modeling that component's own
  `GENERIC_RUN_USER` phase only.
- Any change to callback construction/invocation
  (`build_callback_method_binding`, `CallbackMethodBinding`,
  `invoke_callback_method`) - a structurally similar but functionally
  separate path, untouched.

## Decisions

**D1: Where the constructed `NodeId` is stored, and how it is
accessed.**
Store it directly on `OuterMetaComponent` as a new, private
`type(NodeId)` field (`run_phase_method_node_id`, default-initialized
to `INVALID_NodeId`), populated once by the new construction call
(D2). Alternative considered for storage: look it up by a well-known
name each time through the `ComponentGraph`'s resource index, avoiding
a new stored field. Rejected: there is no existing "the component's
own run-phase node" resource-index key convention (the index is keyed
by `(component_name, StateIntent, var_name)` for state items, per
`proxy_key`'s existing contract) - inventing one just for this single
node is more machinery than one scalar field on the type that already
owns the graph.

**Accessor shape, found during review:** the accessor pair initially
drafted for this field returned `type(NodeId), pointer` (mirroring
`get_vertical_grid`'s own "allocatable field, pointer-or-null"
accessor shape), with the field itself declared `allocatable`. This
repo's own established precedent for a type exposing one of its own
*scalar* `NodeId` fields - as opposed to a map/container lookup -
is different: `BaseGraphNode%get_node_id()` (`BaseGraphNode.F90`) and
`CallbackMethodBinding%get_method_node_id()`/`get_network_id()`
(`CallbackMethodBinding.F90`) all return a plain `type(NodeId)` **by
value**, with "unset" signaled by the returned id's own
`is_valid()`/`INVALID_NodeId` sentinel - never a pointer, never an
`allocatable` field. The `get_vertical_grid`-style pointer accessor
solves a different problem (`class(VerticalGrid), allocatable` is a
potentially-expensive polymorphic value worth avoiding a copy of) that
does not apply to a four-byte `NodeId` scalar. The container-lookup
pointer idiom this codebase also uses heavily
(`ComponentGraph%get_resource_index`/`get_import_port`,
`StateItemMemberMap%at()`, `MethodGraphNode%get_argument_binding()`)
is a third, legitimately different case where `associated()` doubles
as a map's own found/not-found test - also not applicable to a bare
scalar field. Settled: `run_phase_method_node_id` is declared
`type(NodeId) :: run_phase_method_node_id = INVALID_NodeId` (no
`allocatable`); `get_run_phase_method_node_id(this)` returns
`type(NodeId)` by value; the setter asserts
`.not. this%run_phase_method_node_id%is_valid()` before storing. Every
caller checks "already set"/"not yet constructed" via the returned
id's own `%is_valid()`, never `associated()`/`allocated()`.

**D2: Where construction happens.**
Add `graphbuilder_build_run_phase_node(this, rc)` as a new private
procedure in `GraphBuilder.F90`'s existing submodule family, called
from `graphbuilder_run_connect_hook`'s own body (the same hook
`run_connect_hook` already runs from `initialize_accept_transfer.F90`),
guarded by `graph_mode() /= GRAPH_MODE_REGISTRY` so no graph-side
construction happens at all under the default mode.

**Placement within the hook, found during implementation:** this must
run *before* `graphbuilder_freeze`, not after.
`ComponentGraph%register_node`/`next_node_id` both unconditionally
`_ASSERT(.not. this%is_frozen())` (`ComponentGraph.F90:225-226,244-245`)
- freeze is a one-way lifecycle transition with no unfreeze path, so a
call placed after a successful freeze would hard-fail every time under
any non-registry graph mode. This node's own argument bindings are
sourced entirely from the component's own advertised import/export
items (D3), whose `NodeId`s are resolved by `advertise_one` at
`GENERIC_INIT_ADVERTISE` time - long before freeze, and independent of
connection resolution - so there is no reason to wait for freeze in the
first place. Corrected placement: call
`graphbuilder_build_run_phase_node` from inside
`graphbuilder_run_connect_hook`'s own body immediately after
`graphbuilder_resolve_connections` returns (status checked the same
`report_if_failed` way every other step in that hook already is), and
before the existing `graphbuilder_freeze` call.

**D3: How the node's arguments are sourced from "the component's own
advertised ports."**
`ComponentGraph%get_import_port`/`get_export_port` are point lookups
(`PortId -> NodeId`, confirmed by reading `ComponentGraph.F90:417-435`),
not an enumeration - there is no "give me every import/export `NodeId`
this component owns" method to iterate. The established idiom for
exactly this walk is what `graphbuilder_advertise`/
`graphbuilder_populate_state`'s own same-component branch already do:
iterate `this%get_component_spec()%var_specs` (a `VariableSpecVector`),
filter on `state_intent`, and resolve each one's already-registered
`NodeId` via `graph%get_resource_index(item_key(state_intent,
short_name))` - the identical lookup `advertise_one` itself populated
at advertise time. For each matching `VariableSpec`:
`declare_argument(name, access)` where `access` is IN for an import
item and OUT for an export item (no INOUT case - a component's own
advertised items are one-directional by construction, unlike a
callback argument which may be declared INOUT), then
`bind_argument(name, member_id)` where `member_id` is that item's own
resolved `NodeId`. INTERNAL items are skipped (neither IN nor OUT at
the component boundary, mirroring `advertise_one`'s own "INTERNAL
items get neither [port]" treatment). Declared unconstrained (no
`expected_kind`): `VariableSpec` carries no `MAPL_StateItem_Flag`-typed
field to source an equivalent constraint from (`itemType` is the
distinct, native `ESMF_StateItem_Flag`) - inventing a mapping solely to
satisfy an optional constraint this capability's own spec never asks
for would be new scope, not reuse. `register_node` is called once at
the end, mirroring `build_callback_method_binding`'s own last step.

**D4 (the node's invocation adapter is built but never invoked).**
The node is constructed with a real attached adapter - a
`GridCompMethodInvocation` wrapping a `GridCompDriverPhaseInvoker`
built from an `OuterMetaComponentDriverResolver(this)` and
`ESMF_METHOD_RUN` - rather than left adapter-less, even though nothing
in this change ever calls `invoke()` on it. Rationale: leaving the
adapter unattached would make the node permanently uninvocable (`spec
scenario "No adapter attached is not invocable"`, `method-graph-node`
capability) regardless of future wiring; attaching the real adapter
now means a future invocation-wiring change only needs to add a call
site, not revisit this change's own construction logic. The adapter's
own `driver_key` is `SELF_COMPONENT_NAME` - this node always targets
`this` component's own driver, never a child's, matching `run_user.F90`'s
own existing legacy call target.

## Risks / Trade-offs

- **[Risk]** `run_connect_hook`'s existing failure-tolerant posture
  (`report_if_failed`, never breaks real init) means a construction
  failure in the new run-phase-node step could be silently swallowed
  the same way other steps in that hook already are. -> **Mitigation:**
  acceptable here because nothing yet depends on the constructed node
  existing (no invocation call site, this change's own Non-Goal) - a
  swallowed construction failure under a non-default graph mode has no
  observable production consequence today. A future invocation-wiring
  change will need its own explicit check (e.g. `_ASSERT` on
  `get_run_phase_method_node_id()%is_valid()`) before relying on this
  node existing - not addressed here, since nothing here reads it back.
- **[Risk]** Building a real, driver-targeting adapter (D4) without
  ever invoking it could look like partially-wired invocation
  machinery to a future reader, inviting an unsafe shortcut (e.g.
  wiring a call site without first resolving the double-invocation
  hazard proposal.md names). -> **Mitigation:** this design.md and the
  `graph/ordinary-phase-wiring` spec delta both state explicitly, in
  more than one place, that invocation is a Non-Goal and names the real
  blockers; `graphbuilder_build_run_phase_node`'s own header comment
  will restate this inline, next to the adapter construction itself,
  as the first thing a future implementer reads before adding a call
  site.

## Migration Plan

No migration needed for existing production code: `graph_mode()`
defaults to `GRAPH_MODE_REGISTRY`, under which the new construction
step (D2) is an unconditional no-op, matching every prior Phase 10
sub-change's own zero-default-path-behavior-change pattern. No
rollback mechanism beyond the existing graph-mode selector is needed.
