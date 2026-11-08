# Legacy scenario parity under graph mode

`superstructure/generic/tests/Test_Scenarios.pf` runs every legacy scenario
under `GRAPH_MODE_REGISTRY`. A scenario whose `graph_ready` flag is `.true.`
in `scenario_table()` is additionally run under `GRAPH_MODE_GRAPH` and
`GRAPH_MODE_BOTH` (with `set_materialize_extensions(.true.)`), using the same
`expectations.yaml` and checkers. Flipping a flag to `.true.` is how later
tasks show progress. The ESSENTIAL `MAPL.generic.scenarios.shard<k>` tests
stay green because only ready scenarios get graph/both parameters.

## Running the pending scenarios

`MAPL.generic.scenarios.pending` (label `PENDING`, not ESSENTIAL) runs only the
scenarios with `graph_ready = .false.` under graph and both modes. It is
expected to fail until they are fixed:

```bash
with-nag ctest --test-dir nag -R scenarios.pending
```

Environment switches read by `get_parameters()` (usable with the test binary
directly): `MAPL_SCENARIO_PENDING` (any value: pending-only),
`MAPL_SCENARIO_ONLY=<name>` (restrict to one scenario) and
`MAPL_SCENARIO_SHARD=k/N` (keep every N-th parameter, starting at k, of what
the other switches leave; the ESSENTIAL ctest shards use this).

## Table

Measured 2026-10-22 on NAG 7.2.41 and gfortran (identical results). "Passes"
means every checker passes in both graph and both modes; it does not prove
the graph path produced the item (see "What ready means" below). Root causes
were confirmed by the A1 diagnosis (2026-10-22); cause ids (G1, R1, ...) refer
to "Verified root causes" below.

| Scenario | Ready? | Graph-mode result / root cause (verified) |
|---|---|---|
| scenario_0 | yes | passes |
| scenario_1 | yes | passes. With `MAPL_SCENARIO_GRAPH_OWNS_USER_STATES=1` it also passes in `GRAPH` mode, with the re-exported internal `Z_A1` (expected as `child_A/Z_A1`) supplied by the export-port walk |
| scenario_2 | yes | passes. With `MAPL_SCENARIO_GRAPH_OWNS_USER_STATES=1` it also passes in `GRAPH` mode, with the re-exported internal `Z_A1` renamed `ZZ_A1` (expected as `child_A/ZZ_A1`) supplied by the export-port walk |
| 3d_specs | yes | passes |
| ungridded_dims | yes | passes |
| dims_z_field (graph-scenarios) | yes | passes: two `dims: z` items on different horizontal grids connect with no chain |
| precision_extension | yes | passes |
| precision_extension_3d | yes | passes |
| export_dependency | yes | passes |
| expression | yes | passes |
| expression_match | yes | passes |
| invalidate | yes | passes |
| history_wildcard | yes | passes |
| extdata_1 | yes | passes |
| names_1 | yes | passes |
| vertical_alignment_same_grid | yes | passes |
| regrid | yes | passes (measured 2026-11-08): one geometry step (`HorizontalRegridTransform`), chain checked. The table entry sets `graph_owns_user_states`, so in `GRAPH` mode the value check reads the graph-regridded import (a corrupted regrid output fails it) |
| regrid_r8 | yes | passes (measured 2026-11-08): geometry then typekind step, chain checked; graph-owned user states as for `regrid` |
| propagate_geom | yes | passes (measured 2026-11-08): same grid on both sides, direct edge (`chain: []`). With `MAPL_SCENARIO_GRAPH_OWNS_USER_STATES=1` it also passes in `GRAPH` mode: the re-exported internal `Z_A1` (expected as `child_A/Z_A1`) comes from the export-port walk |
| vertical_regridding | no | passes, masked (M2): `B:I_B` is an exact match in the graph; vertical-grid difference invisible |
| vertical_regridding_2 | no | same as `vertical_regridding` (`B:I_B`, `D:I_D` exact). Passes with `MAPL_SCENARIO_GRAPH_OWNS_USER_STATES=1` too: a single-level grid's orientation entry holds UNSPECIFIED and `write_info` writes nothing for it |
| vertical_regridding_3 | no | same as `vertical_regridding` (`C:I_C`, `PHYS:T_PHYS` exact) |
| vertical_alignment_regrid | no | same as `vertical_regridding` |
| vertical_alignment_with_grid | no | same as `vertical_regridding` |
| scenario_reexport_twice | yes | passes; graph builds the re-export ports. With `MAPL_SCENARIO_GRAPH_OWNS_USER_STATES=1` it also passes in `GRAPH` mode: the user export state gets both re-exports from the port walk: `Eparent_B1`, a re-export of a child's export, and `Egrandparent_B1`, a re-export of that re-export |
| expression_defer_geom | yes | passes (measured 2026-11-08): each input on the file grid gets a geometry step (`HorizontalRegridTransform`) before the `ExpressionTransform`; the chain links carry the input names, which `StateEval` looks up |
| history_1 | no | passes since the G1 guard (measured 2026-11-06); not yet flipped. Recheck R1 before flipping |
| statistics | no | FAILS: R1 only |
| statistics_real | no | FAILS: R1, R2 (vector `A/UV`), P1; measured 2026-11-06: `MethodGraphNode: argument name already declared` (P1) |
| service_service | no | FAILS: R2 only. After R2, passes, with masked SERVICE items (M3) |
| service_with_options | no | same as `service_service` |
| vector_1 | no | FAILS: R2 only. After R2, passes, with masked VECTOR items (M3) |

## Verified root causes (A1 diagnosis)

Method: each pending scenario was run alone, under graph and both modes
(`MAPL_SCENARIO_PENDING=1 MAPL_SCENARIO_ONLY=<name>`), on NAG and gfortran.
Unique failure locations matched on both compilers. Each cause was then
confirmed by a minimal source experiment (reverted afterwards). With the G1,
R1, R2 and P1 experiments applied together, `MAPL.generic.scenarios.pending`
passed on NAG and the then-single ESSENTIAL `MAPL.generic.scenarios` entry
stayed green.

Why a step that `report_if_failed` only logs still fails a test: under pFUnit,
the MAPL throw method (`pfunit/MAPL_Initialize.F90`) records every failed
`_ASSERT`/`_VERIFY` as a test failure, even if the caller then continues.
In production, the same faults only log a warning and drop part of the graph.

### Hard failures (C2 candidates)

- **G1. Geometry pushed from a parent that has none.**
  `graphbuilder_resolve_geometry` (`GraphBuilder_Geometry_smod.F90`) calls
  `push_geometry_to_child` for every `GEOMETRY_FROM_PARENT` child. When the
  parent has no geometry of its own (no `geometry:` block, itself
  FROM_PARENT of a parent without geometry), `graphbuilder_advertise_geometry`
  registered no geometry node, so the push asserts
  `push_geometry_to_child called before this component advertised its own geometry`.
  Legacy `propagate_geom_to_children` copies the geom only
  `if (allocated(this%geom))`. Fixed: the push condition now has
  `.and. this%has_geom()` (`GraphBuilder_Geometry_smod.F90:302`). Experiment
  before the fix: it made `scenario_reexport_twice` and
  `expression_defer_geom` pass, and removed the assertion from `history_1`
  and `statistics_real`. After the fix `expression_defer_geom` and
  `history_1` pass; `statistics_real` still fails for R1, R2 and P1.
  First-pass reading confirmed.
- **R1. Export aliasing looks in the wrong component for propagated exports.**
  This corrects the first-pass "no import units" cause. That assertion
  (`try_materialize_field_extension`) is a secondary symptom. Real cause:
  `try_alias_export_field` looks for `export_node%get_short_name()`
  (unqualified, e.g. `T`) in the user export state of the connection's
  `src_comp` (e.g. `root`). For an export propagated from a grandchild, the
  port name is qualified (`A/T`), and the field is in `root/A`'s state, not
  `root`'s. The alias is never found, so resolution falls back to
  materializing a new field. That needs the import's units, and these
  imports declare none (MIRROR-style History/collection imports: `A/T`,
  `B/E_B2`, `TS_avg`). Experiment: resolving the owner from the qualified
  port name (`resolve_qualified_component(this, src_comp // '/' // prefix)`,
  leaf name for the state lookup) made `history_1` and `statistics` pass
  with no units change. A contrasting experiment (fall back to the export's
  units) was the wrong fix: `statistics` then failed `long_name` (the newly
  materialized field has no metadata), and `history_1` hit
  `get_item_typekind - no TYPE_KIND_CHARACTERISTIC_KIND established` (the
  `typekind: mirror` import). Affects: history_1, statistics,
  statistics_real. Latent (B2): imports with mirrored units/typekind
  have no characteristic, so any path that has to materialize for them fails.
- **R2. Export aliasing assumes every export is a Field.**
  `try_alias_export_field` calls `ESMF_StateGet(..., field=)` once the item
  is found, without checking `item_kind`. VECTOR and SERVICE exports are
  FieldBundles, so the call returns rc=13. This corrects the first-pass
  claim that "SERVICE items are excluded from graph resolution": they are
  not excluded. They reach `resolve_one`'s exact-match branch like any
  other item. Experiment: return "not found" for a non-Field item. The
  existing `try_materialize_field_extension` then reports
  `unsupported_item_class:VECTOR/SERVICE` as a warning, and the scenarios
  pass (see M3). Affects: service_service, service_with_options, vector_1,
  statistics_real (`A/UV` and the other vector stats).
- **P1. Run-phase method-node arguments are keyed by short name only.**
  `graphbuilder_build_run_phase_node` declares one argument per import/export
  under `var_spec%short_name`. `StatisticsGridComp` advertises an import and
  an export with the same name (`A/TS` in and `A/TS` out), so
  `MethodGraphNode%declare_argument` rejects the second one. An import and
  an export sharing a short name is legitimate; the graph must not infer
  anything from item names. Experiment (a hack, not a fix): mangling the
  key as `'in:'//short_name` made `statistics_real` pass, but broke
  `Test_GraphBuilder_RunPhaseNode` (it looks arguments up by bare name).
  Fix: key arguments structurally by (state intent, short name), or keep
  separate import/export argument sets, with intent passed to
  `get_argument`/`get_argument_binding`. Affects: statistics_real.

### Masked passes (not failures, but not graph-produced)

- **M1. Item geometry is rarely SPECIFIED (resolved for regrid).**
  `make_characteristics`
  (`specs/VariableSpec/make_characteristics.F90:74,245-284`) inserts
  `GEOMETRY_CHARACTERISTIC_KIND` for every field-carrying shape and SERVICE.
  It is SPECIFIED only when the item declares a geom or geom id; otherwise
  it is DEFERRED (MIRRORED for an EXPRESSION or `mirror_geom`, UNCHECKED for
  horizontal dims NONE) and takes the component geom at GEOM_B. Re-measured
  2026-11-08: the geometry difference was not masked. Before C1-geom-b,
  `regrid` and `regrid_r8` already logged unsupported GEOMETRY for
  `B:I_B1`. Since C1-geom-b a pair holding two geoms gets a
  `HorizontalRegridTransform`, and both scenarios are ready.
- **M2. Component vertical grid is not an item characteristic.**
  `VERTICAL_GRID_CHARACTERISTIC_KIND` is set only when
  `var_spec%vertical_grid` is allocated. `parse_var_specs` fills that only
  from an item-level `geometry:` block, never from the component's
  `geometry: vertical_grid:`. In every `vertical_*` scenario the grid is
  declared per component, so the import and export compare equal and
  legacy does the regrid. Even when it is set, the vertical regrid
  `Transform` is still missing. Not covered by C1: needs its own C2 task,
  or should be folded into B3-VerticalCoordinate.
- **M3. Non-Field shapes are not materialized by the graph.** After G1 and R2,
  EXPRESSION, VECTOR (`vector_1`,
  `statistics_real`) and SERVICE (`service_*`) connections log
  `unsupported_item_class:<shape>` and leave those items to legacy
  `add_to_states`. Flipping these scenarios to ready after G1/R2 gives a
  masked pass, like the ready rows in the next section.
  `expression_defer_geom` no longer logs this (re-measured 2026-11-08): its
  expression is evaluated on the graph path.

Suggested C2 split: `C2-geom-push` (G1), `C2-alias-owner` (R1),
`C2-alias-nonfield` (R2), `C2-runphase-args` (P1, key arguments by intent
and name), `C2-vertical-characteristic` (M2). R1 and R2 touch the same
procedure, so do them in sequence, not in parallel.

## What "ready" means today, and how much is graph-produced

`initialize_advertise` and `initialize_realize_accepted` call
`StateRegistry%add_to_states` in every graph mode (only
`initialize_accept_transfer` is gated on `GRAPH_MODE_GRAPH`), and
`GRAPH_MODE_BOTH` by design never writes graph output into the checked
states. So a green graph/both run means "the graph path runs without error
and the legacy-observed state matches", not "the graph path produced it".

One-off experiment (NAG, not committed): with the two legacy
`add_to_states` calls skipped under `GRAPH_MODE_GRAPH`, every scenario that
is `graph_ready` fails `item type` for these items only; connected
import/export pairs are still populated graph-natively:

| Scenario | Items the graph path did not populate |
|---|---|
| scenario_0 | internal `Z` |
| scenario_1, scenario_2, names_1, propagate_geom | unconnected import `child_A/I_A1` |
| 3d_specs, ungridded_dims, precision_extension, precision_extension_3d | unconnected import `A/I_A2` |
| export_dependency | export `child_A/E2` |
| vertical_alignment_same_grid | export `A/E_A` (unconsumed) |
| regrid | exports `A/E_A1`, `A/E_A1(1)` |
| expression, expression_match | exports `A/A`, `A/B` |
| invalidate | exports `A/T`, `T(1)`, `T(2)` |
| history_wildcard | exports `E_A1`, `E_A2`, `E1_A0` |
| extdata_1 | run phase fails (`status=41`), not an item-population gap |

Gaps: INTERNAL items, imports and exports with no connection, and
expression/wildcard/bracket-indexed exports (`T(1)`). Retiring the legacy
calls is plan work (D2 and the B tasks); do not remove them in a test task.

`regrid*` and `propagate_geom` are ready since C1-geom-b. The `vertical_*`
family stays `graph_ready = .false.` until a vertical `Transform` exists
(M2, C1-vertical).

## Legacy and graph behavior differences

Cases where legacy behaves differently from the graph design and legacy is
deliberately left alone (no legacy-behavior fixes; see the plan's "Working
with the owner"). Tasks add a row when they find one. This table is the
checklist for switching legacy off.

| Item | Legacy | Graph | Found by |
|------|--------|-------|----------|
| R4 expression inputs with an R8 consumer (`expression_per_consumer_r8`) | orders TYPEKIND before CLASS (`specs/ExpressionClassAspect.F90:119-124`); precision coupler fails on the expression item (`transforms/CouplerMetaComponent.F90:588`). Import couplers run in every mode, blocking this scenario even in graph mode; no legacy fix | converts each input to consumer precision before evaluation, then converts result units. Scenario stays pending and graph-only until legacy couplers stop running in graph mode | 3.23b B3-ExpressionPerConsumer |
| `geometry_mirror` / `vertical_grid_mirror` on an item, at the GEOM_A push | `make_GeomAspect` / `make_VerticalGridAspect` ignore the request; the component geom / vertical grid is pushed and the aspect becomes SPECIFIED, so a connection to an export on another grid regrids | item stays MIRRORED and adopts the exporter's geometry at connection (no regrid) | 3.39 (dropped) |
| EXPRESSION export with no geometry and no mirror flag, at the GEOM_A push | gets the component geom / vertical grid (SPECIFIED) | stays MIRRORED (design §5) | 3.39 preflight; revisit with 3.12/3.23 |
| `long_name` undeclared on an export or internal | none; an import inherits the export's in `connect_to_export` | falls back to the field dictionary, then the StandardName (deliberate, A8) | 3.13 |
| QuantityType value `QUANTITY_UNKNOWN` as a quantity-axis wildcard | legacy `QuantityTypeAspect%matches` keeps the wildcard: either side `QUANTITY_UNKNOWN` matches anything | dropped from `pair_outcome`: a declared `QUANTITY_UNKNOWN` now counts as undeclared (DEFERRED on EXPORT/INTERNAL, UNCHECKED on IMPORT), never reaching `pair_outcome` as a SPECIFIED value (`BASIS_NONE` stays a basis wildcard) | 3.36 |
| Unconnected export that a re-export names | allocated: the re-export chain activates the source, so `allocate_items` allocates it (a legacy bug; owner decision 2026-11-07) | stays created (GRIDSET once its geometry is written), not allocated; only a connection that consumes the export allocates it | 3.41a B3-FieldPerNode |
| Propagated child export in the user export state | `add_to_states(mode='user')` skips every item whose `comp_name` is non-empty (`registry/StateRegistry_Actions_smod.F90:73`), so a child's propagated export is not in the parent's user export state | with the user-state switch on, the export-port walk aliases each propagated port (`child/E`) into a substate of the user export state (`populate_export_port`) | 3.41b B3-ExportPortPopulation |
| Both export and import declare a regridder param, on different geoms | `GeomAspect%make_transform` `_FAIL`s (`specs/GeomAspect.F90:191-213`); connection setup aborts | reported per pair as an incompatible GEOMETRY conflict naming both signatures (with params); the component's other connections still resolve. Owner decision 2026-11-08 | C1-geom-b |
| Geometry mismatch on a FIELDBUNDLE, BRACKET, VECTOR or VECTORBRACKET item | `RegridTransform%update` regrids Fields and FieldBundles (`transforms/RegridTransform.F90:218-235`) | reports unsupported GEOMETRY until a bundle-level regrid Transform exists (owner decision 2026-11-08; C1-geom-c). Hybrid mode is unaffected: legacy does the regrid | C1-geom-b |
| Vertical regrid between different fixed-level grids where a field's alignment differs from its grid's direction | `VerticalRegridTransform` reverses each side's levels by the field's alignment (`transforms/VerticalRegridTransform.F90:430-436`, `:519-623`), but fixed-level coordinates are in grid order, so values are wrong: UP grid [1000,500,100] hPa to [800,300] hPa, T = 2p+1: a source stored DOWN gives [521,1501] instead of [1601,601]; a destination stored DOWN gets its output reversed. Model-grid (PLE/ZLE) coordinates arrive in the field's own order, where the flip should be right (read, not run) | builds each side's levels in that field's storage order, then regrids; correct values. Owner decision 2026-11-08 | C1-vertical-a |
| Import levels outside the export's level range | `compute_linear_map` copies the end value silently in Release; Debug builds abort on an assertion (`infrastructure/vertical/vertical/VerticalLinearMap.F90`) | copies the end value in every build and logs one warning per weight build. Owner decision 2026-11-08 | C1-vertical-a |
