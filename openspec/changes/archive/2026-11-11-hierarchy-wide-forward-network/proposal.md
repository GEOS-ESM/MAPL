## Why

Every `ComponentGraph` still has its own default `DependencyNetwork`, and
an ordinary connection's edges go into the network of the component that
declared the connection. Data flow, though, is about the field, not about
who declared the edge. Because of the split, an import whose connection
was declared two or more levels up is never pulled at run time (the
`history_1` scenario's `A/E_A1` is never converted). Two connections
declared by different components that need the same conversion of one
export each build their own extension chain. And the callback get-pull
cannot see a provider's real upstream producer. The hierarchy-wide change
(archived `2026-10-17-hierarchy-wide-component-graph`) already shares node
and network storage, but it kept network ownership per component as a
non-goal, with no reason given. This change finishes that move for
ordinary (forward) data flow.

## What Changes

- **BREAKING (internal API)**: One forward `DependencyNetwork` per
  hierarchy, held by the hierarchy's shared store, replaces every
  `ComponentGraph`'s own default network. All ordinary connection,
  extension-chain, expression, collection, geometry and callback-get
  edges go into it, whichever component declares them.
- A field has at most one forward producer across the whole hierarchy.
  A second one is rejected when the edge is added.
- Write-back networks stay separate and are never followed by a pull:
  the callback put network, and the inout borrower's return network
  (still deferred, unchanged).
- Reuse is hierarchy-wide. Extension chains, expression evaluations, and
  callback and wildcard collections are found by a key built from the
  export (or import) itself, not from the declaring component. This
  removes duplicate chains.
- Transform port bindings are stored hierarchy-wide, so a pull gives the
  same result whichever component asks for it.
- A pull names only the field. Each component pulls its own imports
  before its run method and advances its own exports after it
  (REQ-REV-011 as written). The parent's loop over its children's
  imports after its own run goes away.
- Validation and freeze: the forward network is checked as edges are
  added (one producer, no cycle) and frozen once, by the existing root
  freeze walk. Per-component ownership checks for foreign nodes and the
  per-component cross-network write check are removed.
- Not in this change (a follow-up change): state population no longer
  walking descendants, merging geometry push and pull, removing the
  `get_child_component_graph` carve-out.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `graph/component-hierarchy`: the shared store also holds the one
  forward network, the reuse index and the transform port bindings; one
  forward producer per field across the hierarchy; write-back networks.
- `graph/component-graph`: no per-instance default network; bindings and
  reuse lookups no longer per instance; validation no longer checks
  foreign-node ownership or cross-network writes.
- `graph/graph-builder`: ordinary edges go into the forward network;
  "Resolution creates or reuses the default dependency network" is
  removed; the run-phase pull becomes each component pulling its own
  imports.
- `graph/extension-reuse`: reuse applies across declaring components.
- `graph/callback-wiring`: get-side edges in the forward network; only
  the put side has a network of its own.
- `graph/node-revision-and-update`: an update request names a field and
  follows its forward producer, wherever the edge was declared.
- `graph/method-graph-node`: "default-network invocation" becomes
  forward-network invocation; bound inputs are pulled across declaring
  components.

## Impact

- `superstructure/generic/graph/`: `HierarchyGraph` (forward network,
  reuse index, port bindings), `ComponentGraph` (default network, owns
  checks, validate, freeze), `ComponentGraph_DemandDrivenUpdate`,
  `ExtensionResolution`, `MethodInvocation`, `SetSharedCharacteristic`,
  `callback/CallbackMethodBinding`.
- `superstructure/generic/`: `GraphBuilder_ConnectionResolution_smod`,
  `GraphBuilder_CallbackWiring_smod`, `GraphBuilder_Geometry_smod`,
  `GraphBuilder_RunPhasePull_smod`, `GraphBuilder_StatePopulation_smod`,
  `GraphBuilder_Freeze_smod`, `OuterMetaComponent/run_user.F90`.
- Tests: about 18 pFUnit files call `get_default_network_id`;
  `Test_ComponentGraph` (distinct default ids), `Test_CallbackMethodBinding`
  (hand-built get-net producer), `Test_Scenarios` `check_graph` (reads the
  root's network). New tests for cross-component reuse, a deep pull
  (`history_1`), a second forward producer, and the callback get-pull.
- Docs: `docs/graph/spec/06`, `07`, `11`, `15` text; ARCHITECTURE.md (meta
  doc batch). Legacy layer untouched.
