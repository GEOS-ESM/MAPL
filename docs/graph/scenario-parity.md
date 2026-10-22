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
the graph path produced the item (see Caveat).

| Scenario | Ready? | Graph-mode result / one-line root cause |
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
| regrid | no | passes, held back: plan expects a missing geometry transform; pass is probably masked (below). C1 target |
| regrid_r8 | no | same as `regrid`. C1 target |
| propagate_geom | no | same as `regrid`. C1 target |
| vertical_regridding | no | passes, held back: no vertical-regrid `Transform`; probably masked |
| vertical_regridding_2 | no | same as `vertical_regridding` |
| vertical_regridding_3 | no | same as `vertical_regridding` |
| vertical_alignment_regrid | no | same as `vertical_regridding` |
| vertical_alignment_with_grid | no | same as `vertical_regridding` |
| scenario_reexport_twice | no | FAILS: `push_geometry_to_child called before this component advertised its own geometry` (`GraphBuilder_Geometry_smod.F90:230`); parents with no own `geometry:` block |
| expression_defer_geom | no | FAILS: same geometry-push assertion; plus `no registered extension provider ... unsupported_item_class:EXPRESSION` |
| history_1 | no | FAILS: geometry-push assertion; plus `materialization reached with no import units` (`GraphBuilder_ConnectionResolution_smod.F90:1720`; import declares no units, legacy takes them from the export) |
| statistics | no | FAILS: `materialization reached with no import units` (same site) |
| statistics_real | no | FAILS: geometry-push assertion; no-import-units; `build_run_phase_node` fails (`MethodGraphNode%declare_argument`: argument name already declared); resolve_connections rc=13 |
| service_service | no | FAILS: rc=13 from `try_alias_export_field` (`ESMF_StateGet(..., field=)` on a non-Field export, `GraphBuilder_ConnectionResolution_smod.F90:1613`); SERVICE items are excluded from graph resolution |
| service_with_options | no | FAILS: same as `service_service` |
| vector_1 | no | FAILS: same `try_alias_export_field` rc=13 on a `class: vector` export (VECTORBRACKET/vector not handled in graph mode) |

Root causes above are first-pass readings of the failing logs and source
lines, not diagnosed fixes; the A1 diagnosis task owns confirming them.

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
