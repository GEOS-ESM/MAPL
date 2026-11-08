## Context

See proposal.md for why. Today (`superstructure/generic/graph/`):

- `HierarchyGraph` holds the shared node map, network map and the two id
  generators. Each `ComponentGraph` creates its own default network at
  construction (`ComponentGraph.F90:208-210`) and owns any network it
  creates (`own_network_ids`).
- Ordinary edges go into the declaring component's default network
  (`GraphBuilder_ConnectionResolution_smod:245`), geometry pull into the
  parent's and geometry push into the child's (`GraphBuilder_Geometry_smod`
  :336, :432), callback get and put edges into two networks per method
  (`GraphBuilder_CallbackWiring_smod:381-382`), the inout return edge into
  a network per pairing (`:1421`).
- `update(network_id, node_id)` walks one network and reads transform
  port bindings from the calling graph (`ComponentGraph_DemandDrivenUpdate`
  :126-148, :215-272). Bindings are stored per `ComponentGraph`.
- Reuse keys (`EXTCHAIN:`, `EXPREVAL:`, `CALLBACK_COLLECTION:`,
  `WILDCARD_COLLECTION:`) live in the per-component resource index,
  next to the advertised-item keys. `add_resource_index` asserts that the
  node is owned by that graph, which forced `find_owning_component_graph`.
- The run-phase pull (`GraphBuilder_RunPhasePull_smod`) runs after a
  component's own run and pulls its direct children's imports in its own
  network. No production code advances an export's revision after a run
  (`advance_revision` is called only by `update()` outputs, callback put,
  `set_shared_characteristic`, and the test-only
  `invoke_on_default_network`), so a transform re-executes only on its
  first pull.
- A standalone `ComponentGraph()` (no hierarchy id) creates a fresh
  hierarchy, so graph-neutral unit tests get a hierarchy of their own.

Owner decisions (2026-11-08, meta session): one forward network; reuse
hierarchy-wide keyed by the export; write-back networks (callback put,
inout return) stay separate and are never pulled; callback get edges move
into the forward network; a second forward producer is an error at
add-time; inout borrower kept as is (deferred); population, geometry
merge and the `get_child_component_graph` carve-out are a follow-up
change.

## Goals / Non-Goals

**Goals:**
- One forward network per hierarchy; all forward edges go there.
- Pull by item: `update(node_id)`; same result from any component.
- Hierarchy-wide reuse; no duplicate chains across declaring components.
- Each component pulls its own imports before its run and advances its
  own exports after it, in graph mode.

**Non-Goals:**
- State population structure (`populate_descendants` recursion, the
  "left to the ancestor" rule): kept; only its network argument changes.
- Merging geometry push/pull; removing `get_child_component_graph`.
- Any change to the inout borrower beyond which network its forward
  edge lands in (it already used the default network).
- Legacy (`specs/`, `registry/`, `connection/`, `transforms/`).
- Narrower per-phase networks (spec 07 REQ-CG-001a future direction).

## Decisions

### D1. The forward network lives in `HierarchyGraph`
`HierarchyGraph` creates the forward network once (when the hierarchy is
created) and exposes its id. `ComponentGraph%get_default_network_id` is
renamed `get_forward_network_id` and returns that id; every caller is
renamed (mechanical). `create_network` still creates a network owned by
the calling graph; it is used only for write-back networks.
`add_dependency` accepts the forward network from any graph in the
hierarchy, and an owned write-back network as today.
*Alternative:* keep per-component networks plus a producer index
(field to network). Rejected: it needs a hierarchy-wide cycle check and
hierarchy-wide bindings anyway, and keeps the duplicate-chain problem.

### D2. One forward producer per state item, checked at add-time
When an edge into a `StateItemNode` is added to the forward network and
that node already has a different predecessor there, `add_dependency`
fails with a message naming the item. Re-adding the identical edge is a
no-op (the two-pass init may resolve a connection twice). Edges into a
`TransformGraphNode` are not limited. The check lives in `ComponentGraph`
(it knows node kinds); `DependencyNetwork` stays graph-neutral.
`DependencyNetwork%validate` still checks producers at freeze.

### D3. Port bindings and the reuse index move to `HierarchyGraph`
- The `PortBindingTable` moves to `HierarchyGraph`. `bind_port` no
  longer requires the transform to be owned by the calling graph, only
  registered in the hierarchy. `get_port_binding(s)` read the shared
  table, so `update()` does not depend on the calling graph.
- A hierarchy-wide reuse index (string key to `NodeId`) holds the four
  reuse keys above. New `ComponentGraph` pass-throughs
  `add_reuse_entry`/`get_reuse_entry` (no ownership assert).
  `add_resource_index` keeps only advertised-item keys (`item_key`,
  geometry), still per component and still owner-checked.
- `find_owning_component_graph` and its two callers' workaround are
  removed.
*Why a separate index rather than making the resource index shared:*
item keys are deliberately per component (two siblings may advertise
the same short name).

### D4. `update(node_id)`
`ComponentGraph%update(node_id, rc)` walks the forward network. The
network argument is removed: write-back networks are never pulled
(callback put revisions are advanced directly, as today). `finish_transform`
reads bindings for the forward network.

### D5. Run-phase pull and export advance per component
In `run_user.F90`, in graph mode only: before the user run call, the
component pulls each of its own advertised imports (`update(import)`);
after a successful run, it advances the revision of each of its own
advertised exports. The existing after-run loop over children's imports
is removed (each child now pulls its own). This is REQ-REV-011 (spec 11)
as written, and it matches legacy order (import couplers update before
the user run). Advancing exports is what lets a transform re-execute on
later steps; without it the pull is a no-op after the first step.
*Alternative:* keep the parent-driven loop but make it recursive.
Rejected by the owner (pull on the field).

### D6. Callback get edges in the forward network
`build_callback_method_binding` wires provider-to-member edges into the
forward network and creates only the put network.
`CallbackMethodBinding` keeps the put network id; the get network id is
removed. `pull_callback_get_arguments` pulls each bound member
(callback-side item), which reaches the provider and its producer.

### D7. Validation and freeze
- `allowed_foreign_node`'s per-call allowed-id set and the per-network
  allowed sets in `validate` reduce to "registered in this hierarchy".
- `validate_cross_network_writes` is removed. Under the decided model a
  write-back target may also have a forward producer (callback provider,
  inout owner); REQ-DEP-008a's same-pull guarantee comes from the
  invocation order (pull before, write-back after), as it does today.
- Each `ComponentGraph%freeze` validates and freezes its own write-back
  networks. `run_graph_freeze_hook` validates and freezes the forward
  network once, after `freeze_subtree_`.

### D8. Other callers switch networks
`StatePopulation` (`:90`), `SetSharedCharacteristic` (`:102`),
`MethodInvocation` (`:136`), `Test_Scenarios%check_graph` and
`GraphExport` use the forward network. `set_shared_characteristic` then
sees dependents wired by any component's connection (a latent bug today).

## Risks / Trade-offs

- [A hidden second producer surfaces as a new error] -> D2's message names
  the item; run all scenarios in graph mode; a real case goes to the owner.
- [Two-pass init re-adds edges] -> identical re-add is a no-op (D2).
- [Advancing exports changes run-time behavior in graph mode] -> only in
  `GRAPH_MODE_GRAPH`; `invalidate` and `statistics_real` (multi-step)
  scenarios pin values.
- [Visualization of one graph now shows forward edges from the whole
  hierarchy] -> acceptable; note in ARCHITECTURE batch.
- [`Test_ComponentGraph` asserts distinct default ids] -> replaced by the
  shared-id scenario.

## Migration Plan

Internal only: no YAML or user API change, registry mode unaffected.
Rollback is a revert of the implementing commits.
