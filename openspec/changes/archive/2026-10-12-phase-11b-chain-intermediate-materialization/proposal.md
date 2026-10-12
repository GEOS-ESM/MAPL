## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.10 names **11b**
("Multi-hop extension-chain intermediate materialization") as the
second of Phase 11's two sub-changes: it closes §20.4.9's own first gap
(no materialized intermediate nodes), confirmed still open by direct
code reading today
(`superstructure/generic/graph/ExtensionResolution.F90`'s own
`build_chain`): every link in a mismatch-resolution chain is created
with its `StateItemNode` payload left unallocated except the chain's
own FINAL node, which `GraphBuilder.F90`'s four call sites materialize
via `try_materialize_field_extension`. A single-characteristic mismatch
(the only shape any landed fixture exercises today, including 11a's own
`units_mismatch_run_phase`) never notices, because its chain has exactly
one link - final node and only node are the same node. A real two-or-
more-characteristic chain (e.g. the still-undemonstrated
`units_and_typekind_mismatch` fixture) fails outright the first time
anything actually pulls on it: the second transform's own `compute()`
call reads its predecessor link's `GraphStateItem%get_field()`, which is
never allocated for any link but the last, producing
`GraphStateItem: esmf_field is not allocated`.

## What Changes

- Change `find_or_build_extension_chain`'s own public interface
  (`superstructure/generic/graph/ExtensionResolution.F90`) to also
  return every intermediate `NodeId` `build_chain` computes internally
  along the way to `final_node_id` - not only the final one. The chain's
  own first link (the one whose predecessor is `export_node_id` itself)
  and the chain's own final link are both included in this returned
  set; `export_node_id` itself is not (it is already the caller's own
  input, materialized separately by the caller's existing
  `try_materialize_field_extension` call on `export_node_id`, unchanged
  by this proposal).
- At each of `GraphBuilder.F90`'s own four `find_or_build_extension_chain`
  call sites (`GraphBuilder_ConnectionResolution_smod.F90`'s
  `resolve_one`, `GraphBuilder_Geometry_smod.F90`'s
  `resolve_geometry_from_child` and `push_geometry_to_child`, and
  `GraphBuilder_CallbackWiring_smod.F90`'s `wire_callback_argument`),
  materialize each newly-returned intermediate node with its own
  correctly-evolved typekind/units/geom - the state after the
  immediately-preceding link's own conversion has notionally applied,
  before the next link's own conversion runs - mirroring
  `try_materialize_field_extension`'s existing per-node materialization
  call, called once per intermediate node in chain order rather than
  once for the chain as a whole.
- Preserve `find_or_build_extension_chain`'s existing reuse-search
  behavior (`chain_key`/`get_resource_index`/`add_resource_index`)
  exactly as it is today: a reused chain's own intermediate nodes are
  already materialized from the call that first built the chain, so a
  caller hitting the reuse path never re-materializes anything -
  returning the already-known intermediate `NodeId`s from the resource
  index is sufficient, with no new materialization work on that path.
- No change to `11a`'s own export-side aliasing
  (`try_alias_export_field`, the chain's own `export_node_id` input) or
  to the chain's own final-node materialization call sites, both of
  which are unaffected disjoint code per §20.4.10's own confirmation
  that the two sub-changes touch disjoint code.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `graph/extension-reuse`: the existing "Framework-created extension
  items have a real, materialized payload" requirement gains an
  explicit guarantee that every intermediate link of a multi-step
  mismatch chain - not only the chain's own final item - is materialized
  with a real, allocated payload when payload materialization is
  enabled, so a chain of two or more transform steps can actually
  execute `compute()` end to end rather than failing on an intermediate
  link's unallocated input.
- `graph/graph-builder`: the existing "Graph-native state population
  populates a real ESMF state from resolved graph structure" and "A
  sibling connection's extension chain is re-evaluated after a run
  phase" requirements' own underlying machinery (demand-driven
  `update()` walking a resolved chain) gains the precondition that every
  link in that chain already has a real payload to read from and write
  to, for any chain `find_or_build_extension_chain` builds with more
  than one mismatched characteristic.

## Impact

- **Affected code:**
  `superstructure/generic/graph/ExtensionResolution.F90`
  (`find_or_build_extension_chain`'s public interface and `build_chain`'s
  internal bookkeeping); `superstructure/generic/GraphBuilder.F90` and
  its four call-site submodules
  (`GraphBuilder_ConnectionResolution_smod.F90`,
  `GraphBuilder_Geometry_smod.F90`,
  `GraphBuilder_CallbackWiring_smod.F90`) - each call site gains a loop
  over the newly-returned intermediate `NodeId`s, materializing each one
  the same way the existing final-node call already does.
- **Affected tests:**
  `superstructure/generic/graph/tests/Test_ExtensionResolution.pf`
  (chain construction, now asserting intermediate `NodeId`s are
  returned and are distinct from the final node for a multi-
  characteristic mismatch); a new or extended
  `Test_GraphModeDemonstration.pf` fixture exercising a real two-
  characteristic mismatch (units + typekind) under
  `materialize_extensions_enabled()`, asserting every link's payload is
  allocated before `compute()` is invoked on it.
- **No change** to `11a`'s own scope
  (`try_alias_export_field`, export-side proxy aliasing) - per
  §20.4.10, the two sub-changes touch disjoint code and neither's
  correctness depends on the other.
- **No change** to Phase 10d's double-invocation hazard, 3b2's
  cross-component import propagation, or Phase 7's pattern-based fan-in -
  unrelated code paths, confirmed by §20.4.10's own cross-reference.
- **Does not** attempt a real end-to-end `units_and_typekind_mismatch`
  run-phase demonstration test by itself unless convenient - per
  §20.4.10, that goal needs both 11a (landed) and this change together;
  landing this change's own chain-construction/materialization fix is
  the primary scope, with the end-to-end demonstration as a stretch
  goal for tasks.md to size, not a hard requirement of this proposal.
