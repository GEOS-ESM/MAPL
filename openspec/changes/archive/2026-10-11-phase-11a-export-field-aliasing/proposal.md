## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.10 names **11a**
("Export-side field aliasing") as the first of Phase 11's two
sub-changes, and the recommended first pick: it closes §20.4.9's second
gap, needs no public-interface change, and completes Phase 11's own
stated goal (a real run-phase value provably propagating through a real
extension chain into a sibling's own real `ESMF_State`) for the
single-characteristic case.

Confirmed still open by direct code reading today
(`superstructure/generic/GraphBuilder.F90`'s `resolve_one`): every call
site that materializes a sibling connection's export-side proxy
(`export_node_id`) does so via `try_materialize_field_extension` ->
`materialize_field_extension`, which calls `FieldCreate` to build a
brand-new, independent `ESMF_Field` - never a `MAPL_NamedAlias` of the
real field `comp_src`'s own `ConfigurableGridComp%run()` actually writes
the YAML-configured `run:` value into. `populate_one` (the import side,
landed by `graph-child-state-population`) already applies the correct
"always alias, never duplicate" treatment; the export side never
received the mirrored fix. `Test_GraphModeDemonstration.pf`'s own
`units_mismatch_run_phase` fixture confirms the resulting symptom
directly: the export proxy's field is real and allocated, but a run
phase's changed value never reaches it (a silent `NaN`, not a crash,
when a numeric assertion was attempted - not currently asserted in the
landed test, per that file's own header comment).

## What Changes

- Add a new procedure in `superstructure/generic/GraphBuilder.F90` that
  materializes a sibling connection's export-side proxy node
  (`export_node_id`) as a real `MAPL_NamedAlias` of the export
  component's own real, already-realized `ESMF_Field` - read directly
  off that component's own real export `ESMF_State` via `ESMF_StateGet`
  (plain ESMF-level read, no `StateRegistry` call, mirroring
  `populate_one`'s own `ESMF_StateAddReplace`/`MAPL_NamedAlias` idiom on
  the opposite side) - instead of calling `FieldCreate` to build an
  independent field.
- Apply this at both of `resolve_one`'s existing `export_node_id`
  materialization call sites (the exact-match branch, and the
  mismatch/extension-chain branch's own chain-input materialization) -
  the two places `try_materialize_field_extension` is called with
  `export_node_id` as its target today. The chain's own FINAL node
  (the transform output, materialized separately with the import's own
  typekind/units) is unaffected - that node's own payload is a real
  transform output, not a producer-side alias, and stays exactly as
  `graph-child-state-population` left it.
- Resolve, as an explicit implementation decision (see design.md), the
  two-pass `GENERIC_INIT_ACCEPT_TRANSFER` timing question this change
  surfaces: the export component's own real field is only guaranteed to
  exist in its own real `ESMF_State` once `GENERIC_INIT_REALIZE_ACCEPTED`
  has run for it at least once, which (per
  `enums/GenericPhases.F90`'s `GENERIC_INIT_PHASE_SEQUENCE`) happens
  *between* the first and second `GENERIC_INIT_ACCEPT_TRANSFER` passes -
  so the new procedure must tolerate "not yet realized" on the first
  pass and succeed by the second, rather than assuming the real field is
  always available the first time `resolve_one` runs.
- Update `Test_GraphModeDemonstration.pf`'s own
  `test_graph_mode_populates_sibling_states` to add a real
  `GENERIC_RUN_USER` phase invocation and assert the real, numeric,
  converted value now reaches `comp_dst`'s own real import state -
  closing that file's own header comment's "gap (3)... not yet
  assertable" note. The existing static assertions (item count,
  typekind, units, export-proxy-field-allocated) are kept, not replaced.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `graph/graph-builder`: the existing "Graph-native state population
  populates a real ESMF state from resolved graph structure" requirement
  gains an explicit guarantee that a sibling connection's export-side
  payload is the real, run-phase-writable field the export component's
  own `ESMF_GridComp` run phase actually writes into - not an
  independent copy - so that a run-phase value changing after init
  reaches the import side through the same resolved structure.

## Impact

- **Affected code:** `superstructure/generic/GraphBuilder.F90`
  (`resolve_one`'s two `export_node_id` materialization call sites, plus
  one new procedure); no change to
  `mapl_ExtensionMaterialization_mod`/`FieldCreate`'s own signature, and
  no change to the chain's own final-node materialization call site.
- **Affected test:** `superstructure/generic/tests/Test_GraphModeDemonstration.pf`,
  exercising the already-landed `units_mismatch_run_phase` fixture
  (`superstructure/generic/tests/graph-scenarios/units_mismatch_run_phase/`)
  with no fixture changes needed.
- **No change** to `11b`'s own scope
  (`ExtensionResolution.F90`'s multi-hop chain intermediate
  materialization) - per §20.4.10, the two sub-changes touch disjoint
  code and neither's correctness depends on the other.
- **No change** to Phase 10d's double-invocation hazard, 3b2's
  cross-component import propagation, or Phase 7's pattern-based fan-in -
  unrelated code paths, confirmed by §20.4.10's own cross-reference.
