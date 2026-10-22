# Legacy scenario parity under graph mode

`superstructure/generic/tests/Test_Scenarios.pf` runs every legacy scenario
under `GRAPH_MODE_REGISTRY`. A scenario whose `graph_ready` flag is `.true.`
in `scenario_table()` is additionally run under `GRAPH_MODE_GRAPH` and
`GRAPH_MODE_BOTH` (with `set_materialize_extensions(.true.)`), using the same
`expectations.yaml` and checkers. Flipping a flag to `.true.` is how later
tasks show progress. `MAPL.generic.scenarios` (ESSENTIAL) stays green because
only ready scenarios get graph/both parameters.

## Running the pending scenarios

`MAPL.generic.scenarios.pending` (label `PENDING`, not ESSENTIAL) runs only the
scenarios with `graph_ready = .false.` under graph and both modes. It is
expected to fail until they are fixed:

```bash
with-nag ctest --test-dir nag -R scenarios.pending
```

Environment switches read by `get_parameters()` (usable with the test binary
directly): `MAPL_SCENARIO_PENDING` (any value: pending-only) and
`MAPL_SCENARIO_ONLY=<name>` (restrict to one scenario).

## Table

Measured 2026-10-22 on NAG 7.2.41 and gfortran (identical results). "Passes"
means every checker passes in both graph and both modes; it does not prove
the graph path produced the item (see "What ready means" below). Root causes
were confirmed by the A1 diagnosis (2026-10-22); cause ids (G1, R1, ...) refer
to "Verified root causes" below.

| Scenario | Ready? | Graph-mode result / root cause (verified) |
|---|---|---|
| scenario_0 | yes | passes |
| scenario_1 | yes | passes |
| scenario_2 | yes | passes |
| 3d_specs | yes | passes |
| ungridded_dims | yes | passes |
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
| regrid | no | passes, masked (M1): graph sees `B:I_B1` as an exact match; the geometry difference is invisible. C1 target |
| regrid_r8 | no | passes, masked (M1): graph sees only the `TYPE_KIND` mismatch for `B:I_B1`, never the geometry one. C1 target |
| propagate_geom | no | passes, masked (M1): exact match; geometry handled only by legacy. C1 target |
| vertical_regridding | no | passes, masked (M2): `B:I_B` is an exact match in the graph; vertical-grid difference invisible |
| vertical_regridding_2 | no | same as `vertical_regridding` (`B:I_B`, `D:I_D` exact) |
| vertical_regridding_3 | no | same as `vertical_regridding` (`C:I_C`, `PHYS:T_PHYS` exact) |
| vertical_alignment_regrid | no | same as `vertical_regridding` |
| vertical_alignment_with_grid | no | same as `vertical_regridding` |
| scenario_reexport_twice | no | FAILS: G1 only |
| expression_defer_geom | no | FAILS: G1 only. After G1, passes, with masked EXPRESSION items (M3) |
| history_1 | no | FAILS: G1 and R1 |
| statistics | no | FAILS: R1 only |
| statistics_real | no | FAILS: G1, R1, R2 (vector `A/UV`), P1 |
| service_service | no | FAILS: R2 only. After R2, passes, with masked SERVICE items (M3) |
| service_with_options | no | same as `service_service` |
| vector_1 | no | FAILS: R2 only. After R2, passes, with masked VECTOR items (M3) |

## Verified root causes (A1 diagnosis)

Method: each pending scenario was run alone, under graph and both modes
(`MAPL_SCENARIO_PENDING=1 MAPL_SCENARIO_ONLY=<name>`), on NAG and gfortran.
Unique failure locations matched on both compilers. Each cause was then
confirmed by a minimal source experiment (reverted afterwards). With the G1,
R1, R2 and P1 experiments applied together, `MAPL.generic.scenarios.pending`
passed on NAG and the ESSENTIAL `MAPL.generic.scenarios` stayed green.

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
  `if (allocated(this%geom))`. Experiment: adding `.and. this%has_geom()` to
  the push condition made `scenario_reexport_twice` and
  `expression_defer_geom` pass, and removed the assertion from `history_1`
  and `statistics_real`. Affects: scenario_reexport_twice,
  expression_defer_geom, history_1, statistics_real. First-pass reading
  confirmed.
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

- **M1. No item-level geometry characteristic.** `populate_characteristics`
  (`GraphBuilder_Advertising_smod.F90`) never inserts
  `GEOMETRY_CHARACTERISTIC_KIND`. Only the component-level edges in
  `GraphBuilder_Geometry_smod.F90` use it. Variable items with different
  grids therefore compare equal. Also, `GeometryCharacteristic%build_transform`
  always fails. C1 needs B3-Geometry (an item-level characteristic) before
  a regrid mismatch can be detected at all. Flipping `regrid*` needs both.
- **M2. Component vertical grid is not an item characteristic.**
  `VERTICAL_COORDINATE_CHARACTERISTIC_KIND` is set only when
  `var_spec%vertical_grid` is allocated. `parse_var_specs` fills that only
  from an item-level `geometry:` block, never from the component's
  `geometry: vertical_grid:`. In every `vertical_*` scenario the grid is
  declared per component, so the import and export compare equal and
  legacy does the regrid. Even when it is set, the vertical regrid
  `Transform` is still missing. Not covered by C1: needs its own C2 task,
  or should be folded into B3-VerticalCoordinate.
- **M3. Non-Field shapes are not materialized by the graph.** After G1 and R2,
  EXPRESSION (`expression_defer_geom`), VECTOR (`vector_1`,
  `statistics_real`) and SERVICE (`service_*`) connections log
  `unsupported_item_class:<shape>` and leave those items to legacy
  `add_to_states`. Flipping these scenarios to ready after G1/R2 gives a
  masked pass, like the ready rows in the next section.

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

For the geometry/vertical family (`regrid*`, `propagate_geom`,
`vertical_*`) a geometry mismatch is also never detected, because variable
items carry no item-level `GeometryCharacteristic`. They stay
`graph_ready = .false.` so C1 (geometry/regrid `Transform`) has a flag to
flip.
