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
| grandparent_units_run_phase (graph-scenarios) | yes | passes (inferred from the commit and its 69/69 ESSENTIAL result; not re-measured): the root declares a connection between two grandchildren (`source_branch/producer/T` to `middle/consumer/producer/T`), expected chain `[units]`. The producer changes its export on step two and the consumer must see the converted value (5000.) before its run; this covers the run-phase pull and advance through the forward network. The table entry sets `graph_owns_user_states`. Per the message of commit `39c16e40f`, a mutation check (NAG) with `run_pull_hook` and `run_advance_hook` made no-ops failed only this scenario in graph mode |
| precision_extension | yes | passes |
| precision_extension_3d | yes | passes |
| export_dependency | yes | passes |
| expression | yes | passes |
| expression_match | yes | passes |
| invalidate | yes | passes. The table entry sets `graph_owns_user_states` (measured 2026-11-09 with the switch on, NAG: passes; the `T(1)`, `T(2)` expectations are `legacy_only`) |
| history_wildcard | yes | passes |
| extdata_1 | yes | passes |
| names_1 | yes | passes |
| vertical_alignment_same_grid | yes | passes (measured 2026-11-10, NAG): no longer masked. The two components declare separate fixed-levels grids with identical levels, which do not match (legacy rule: a fixed grid matches only a basic one), so the graph builds one `VerticalColumnRegridTransform` step (`chain: [vertical]`) that also covers the orientation (export downward, import upward). Constant value: guards that the step runs, not a flip; `vertical_orientation_flip` covers the flip |
| regrid | yes | passes (measured 2026-11-08): one geometry step (`HorizontalRegridTransform`), chain checked. The table entry sets `graph_owns_user_states`, so in `GRAPH` mode the value check reads the graph-regridded import (a corrupted regrid output fails it) |
| regrid_r8 | yes | passes (measured 2026-11-08): geometry then typekind step, chain checked; graph-owned user states as for `regrid` |
| propagate_geom | yes | passes (measured 2026-11-08): same grid on both sides, direct edge (`chain: []`). With `MAPL_SCENARIO_GRAPH_OWNS_USER_STATES=1` it also passes in `GRAPH` mode: the re-exported internal `Z_A1` (expected as `child_A/Z_A1`) comes from the export-port walk |
| vertical_regridding | yes | passes (measured 2026-11-10, NAG; red before C1-vertical-b1: ready only via legacy, the pending run logged `no registered extension provider ... B:I_B:VERTICAL_GRID`): A has a 3-level and B a 2-level fixed-levels grid, both `pressure`; the graph builds a `VerticalColumnRegridTransform` step (`chain: [vertical]`). The table entry sets `graph_owns_user_states`, so in `GRAPH` mode the value check reads the graph-regridded import: with the regrid output zeroed (mutation, NAG) only `vertical_regridding [value] mode=graph` fails. Constant value 15: guards that the step runs, not the numerics (`Test_VerticalColumnRegridTransform.pf` covers those) |
| vertical_regridding_2 | no | stays pending: `B:I_B` (A: model grid with `pressure` and `height`; B: fixed levels, `pressure`) and `D:I_D` (C: model grid, `height`; D: fixed levels, `height`) share one dimension, but a model grid has no level values the graph can read at connect (its PLE/ZLE coordinate is a field; no coordinate input port yet), so `build_transform` reports unsupported VERTICAL_GRID at connect, not a failure at compute (re-measured 2026-11-10, NAG, pending run passes through legacy). Passes with `MAPL_SCENARIO_GRAPH_OWNS_USER_STATES=1` too: a single-level grid's orientation entry holds UNSPECIFIED and `write_info` writes nothing for it |
| vertical_regridding_3 | no | stays pending, as `vertical_regridding_2`: `C:I_C` and `PHYS:T_PHYS` (DYN: model grid, `pressure`; C and PHYS: fixed-levels `pressure` grids) are reported unsupported VERTICAL_GRID at connect (model grid; also `I_C` is fed by the edge-staggered `PLE`) |
| vertical_alignment_regrid | yes | passes (measured 2026-11-10, NAG; red before: as `vertical_regridding`): A has 3 levels downward, B 4 levels upward, both `pressure`; one vertical step (`chain: [vertical]`) also covers the orientation. Constant value: cannot detect a flip (masked for values; the chain is graph-checked) |
| vertical_alignment_with_grid | yes | passes (measured 2026-11-10, NAG; red before: as `vertical_regridding`): A has 3 levels and B 2, both `pressure` and `with_grid`; one vertical step (`chain: [vertical]`). Constant value, chain graph-checked |
| vertical_orientation_flip | yes | new (C1-vertical-b1). Both children take the parent's fixed-levels grid (`[30, 20, 10]` hPa, from_parent), export `downward` with `default_vertical_profile: [30, 20, 10]`, import `upward`: one `VerticalFlipTransform` step (`chain: [orientation]`), and the import profile is checked as `[10, 20, 30]`. The table entry sets `graph_owns_user_states`; with the flip disabled (mutation, NAG) only `vertical_orientation_flip [vertical_profile] mode=graph` fails |
| scenario_reexport_twice | yes | passes; graph builds the re-export ports. The table entry sets `graph_owns_user_states`, so it is also the regression for propagated names staying out of the user export state. With `MAPL_SCENARIO_GRAPH_OWNS_USER_STATES=1` it also passes in `GRAPH` mode: the user export state gets both re-exports from the port walk: `Eparent_B1`, a re-export of a child's export, and `Egrandparent_B1`, a re-export of that re-export |
| expression_defer_geom | yes | passes (measured 2026-11-08): each input on the file grid gets a geometry step (`HorizontalRegridTransform`) before the `ExpressionTransform`; the chain links carry the input names, which `StateEval` looks up |
| history_1 | yes | passes in graph and both modes (the table entry is `.true.`), the deep pull going through the hierarchy's forward network. With `MAPL_SCENARIO_GRAPH_OWNS_USER_STATES=1` it was last measured failing two checks: `B/E_B3` (vertical-grid mismatch, no provider) and the `A/E_A1` value (tasks.md 3.44 Status); not re-measured since the forward network landed |
| statistics | yes | ready (masked pass per owner decision: legacy still fills the user states with the switch off). The earlier R1 failure no longer reproduces (measured 2026-11-09, with and without the R1 fix); its cause was not traced. With `MAPL_SCENARIO_GRAPH_OWNS_USER_STATES=1` it also passes: user states are populated at advertise |
| statistics_real | yes | ready (masked pass: legacy fills the user states, and the graph does not write a child's real export state while it does). Measured 2026-11-11, NAG and gfortran: all checks pass in registry, `both` and `GRAPH` modes. Red before `C2-statistics-real`: one check failed, `history/stat/<user>::export::A/QV_MIN` value in `GRAPH` mode (`range=[****,****]`). Cause (traced): `populate_one` replaced (AddReplace) the child's real export entry `A/QV_MIN` with an alias of the graph's own export Field; the trace showed the replacement Field has a different data address than the one it replaced. That the stat gridcomp (`make_min_stat`) holds the Field legacy put there, and `TimeMin` writes it, is inferred, not traced: its `farrayPtr` was not allocated at realize time (rc=51), so its address could not be printed. Fixed by `populate_one` skipping EXPORT items when the graph does not own the user states. With the fix reverted (mutation, NAG) only `statistics_real [value] mode=graph` fails. With `graph_owns_user_states` it still fails (re-measured 2026-11-11, NAG, through ctest, shards 0 and 3): 11 checks in `GRAPH` mode only (`exists`, `typekind`, `rank`, `standard_name`, `value`, `item type`, `status`, `units`, `geom_name`, `long_name`, `vertical_profile`; none in registry or `both` mode), and `graph_freeze:characteristics_resolved` fails with `StateItemDescriptor: item [stat]:<name> has characteristic GEOMETRY still DEFERRED at freeze` for 33 items, all `stat` internals and the exports of the VECTOR stats (`UV*`): the stat gridcomp declares them `mirror_geom` and the graph has no geometry to mirror for them. The failing-check list and the freeze failure are identical with and without the `C2-slash-export-alias` fix (own-export branch disabled by mutation), so the cause is not the slash alias and the flag stays `.false.`. That is expected: with the switch effective (`GRAPH` mode) `try_alias_export_field` returns at once, so it is not on the failing path. The new branch does fire in this scenario in `both` mode, where the switch has no effect: 16 times per run, once per stat export (`A/TS`, `A/PS`, `A/QV_MIN`, `A/UV`, ..., sub-state `A` each time; temporary print, NAG). Unit tests for the branch: `test_alias_finds_own_slash_export_in_substate`, `test_alias_finds_own_slash_export_in_nested_substates` and `test_alias_own_slash_export_without_substate_creates_field` in `Test_GraphBuilderMaterializeGate.pf` |
| service_service | yes | ready (masked pass: the SERVICE items are aliased from the legacy user export state and the graph does not materialize them, M3). Red before `C2-alias-nonfield` (measured 2026-11-10: `Tests run: 43, Failures: 24`, first log line `resolve_connections step failed with rc=13`), green after |
| service_with_options | yes | same as `service_service` |
| vector_1 | yes | passes in graph and both modes (measured 2026-11-11, NAG; red before C1-vertical-b2: the pending run logged `no registered extension provider ... child_B:uv:VERTICAL_GRID` and failed `[item type]` in graph and both modes). A has 3 fixed levels `[30, 20, 10]` hPa on 12x13 and B has 1 level `[23]` hPa on 8x5, so the chain is `[geometry, vertical]` (horizontal bundle regrid, then `VerticalColumnRegridTransform` on the bundle; the single-level import has an `UNSPECIFIED` direction, so the orientation difference rides on the vertical step). The table entry sets `graph_owns_user_states`, so the item-type, status and standard-name checks read the graph's bundles. No value is checked: the horizontal bundle regrid rotates the constant `fill_value` vector between the two grids, so the import members are not constant; with the vertical regrid output zeroed (mutation, NAG) this scenario still passes, and `vector_vertical_regrid` is the one that detects it. In the test, `step_kind` reads a bundle link's alignment from its first member when it has one (legacy records it on members only), and compares alignments as resolved directions |
| vector_vertical_regrid | yes | new (C1-vertical-b2). A VECTOR `uv` on 3 fixed pressure levels into 2 others, same horizontal grid: `chain: [vertical]`, bundle `VerticalColumnRegridTransform`. The table entry sets `graph_owns_user_states`; the import members hold the constant fill value 1, which a linear regrid keeps, so the value check guards that the step ran and filled both members (with the regrid output zeroed, mutation on NAG, only `vector_vertical_regrid [value] mode=graph` fails). Member order and level numerics are checked by `Test_VerticalColumnRegridTransform.pf` |
| vector_orientation_flip | yes | new (C1-vertical-b2). A VECTOR `uv` on the parent's shared fixed grid, export `downward`, import `upward`: `chain: [orientation]`, bundle `VerticalFlipTransform`. The table entry sets `graph_owns_user_states`. The values are constant (the test components cannot give vector members a profile; `default_vertical_profile` writes one Field), so the scenario cannot see the level order: it shows the step is built, runs and fills the members. Measured 2026-11-11 (NAG, through ctest): with the VECTOR flip output zeroed, `vector_orientation_flip [value] mode=graph` fails; with only the reversal removed (flip disabled) the scenario still passes. The level order is covered by `Test_VerticalColumnRegridTransform.pf` `test_flip_bundle`. Making the scenario order-sensitive needs the configurable test component to fill bundle members with a profile |
| vector_regrid | yes | ready (masked pass: the values of the imports come from legacy). Two VECTOR pairs on 12x13 and 6x7 lat-lon grids, both with `vertical_dim_spec: NONE` (with `center` the same two basic vertical grids still mismatch, see the vertical-grid difference row below); `chain:` expectations check `uv` as `[geometry]` and `wind` (`m s-1` to `km s-1`) as `[geometry, units]`. In the default run the bundle regrid executes twice and the units conversion once per scenario run (measured 2026-11-10 with a temporary trace), so the Transforms do run at compute, on bundles the graph created for the links and on the legacy export bundle as input. Passes with `graph_owns_user_states` too (measured 2026-11-10, NAG, all four shards): the graph creates the export bundles `uv` and `wind` at advertise and gives them their two members at connect (`C2-bundle-exports`); the same bundles are created with the switch off, and the legacy alias then replaces them, so the export, import and root checks of the per-member standard names pass on graph-owned states. The table row sets it |
| bracket_regrid | yes | ready (masked pass). A BRACKET export (declared in code by `ProtoBracketGridComp`, since YAML cannot declare a bracket) on a 12x13 grid into a Field import on a 6x7 grid, `vertical_dim_spec: NONE`; the `chain:` expectation checks `[geometry, item_shape]` (bundle regrid, then time interpolation to a Field). The values of the import come from legacy; the gridcomp fills no members. Passes with `graph_owns_user_states` too (measured 2026-11-10, NAG): the graph creates the export bundle with `bracket_size` members. The table row sets it |

Re-measured 2026-11-09 on NAG, in a tree with `C2-alias-owner` applied, with
`MAPL_SCENARIO_PENDING=1` through ctest (`MAPL.generic.scenarios.pending`;
running the binary directly under `with-nag` loses `DYLD_LIBRARY_PATH` and
every scenario then fails with "shared object not found"). Full pending run:
`Tests run: 232, Failures: 81`; the failing scenarios are
`service_service` and `service_with_options` (24 each; first log line
`resolve_connections step failed with rc=13`, R2), `statistics_real` (22;
`build_run_phase_node step failed with rc=1` (P1) and
`resolve_connections step failed with rc=13`) and
`expression_per_consumer_r8` (11, graph-only; the first failed assertion is
`Expected: is <0> but: was <1>`; its cause is the legacy import coupler, which
fails on the expression item, `CouplerMetaComponent.F90:588`). That scenario is
ready now (see R4 below). Each of `vertical_regridding`,
`vertical_regridding_2`, `vertical_regridding_3`, `vertical_alignment_regrid`,
`vertical_alignment_with_grid` and `vector_1`, run alone with
`MAPL_SCENARIO_ONLY`, passes (22 checks, no failures); in the full run none of
them fails. `statistics` is a ready scenario now (ready shards: 33 checks =
11 checks x graph, both and registry modes, no failures). It also passed with
`C2-alias-owner` reverted: run alone as a pending scenario on that tree
(41 tests, `OK`: 22 `statistics` checks, 11 x graph and both modes, plus 19
other tests in the binary), and alone through the ready shards with the fix
reverted (the same 33 checks, no failures), so this measurement does not show
R1 failing it. The per-row text above predates this measurement.

A pending run on 2026-11-10 on NAG, after `expression_per_consumer_r8` became
ready, gave `Tests run: 173, Failures: 22`, all of them `statistics_real`.

Measured 2026-11-10 on NAG with `MAPL_SCENARIO_ONLY=statistics_real` through
the pending ctest, before the run-phase node was dropped from the connect
hook: `Tests run: 41, Failures: 22`, first error `MethodGraphNode.F90:92`
`argument name already declared`. After: `Tests run: 41, Failures: 1`
(`value` check in `GRAPH` mode, see its table row). The remaining failure was
fixed by `C2-statistics-real`; the scenario is ready.

Measured 2026-11-09 on NAG with the switch on in `GRAPH` mode (3.42): all pass
(`plain_field`, `units_mismatch_run_phase`, `units_and_typekind_mismatch`,
`invalidate`, `graphbuilder_propagation`, `scenario_1`, `scenario_2`,
`scenario_reexport_twice`, `propagate_geom`, `names_1`, `statistics`). The
first five and `scenario_reexport_twice` now have `graph_owns_user_states` in
the table. The outer states (the ones the parent sees) are written by the
graph at `REALIZE_ACCEPTED` and share their Fields with the user states.

## Verified root causes (A1 diagnosis)

Method: each pending scenario was run alone, under graph and both modes
(`MAPL_SCENARIO_PENDING=1 MAPL_SCENARIO_ONLY=<name>`), on NAG and gfortran.
Unique failure locations matched on both compilers. Each cause was then
confirmed by a minimal source experiment (reverted afterwards). With the G1,
R1, R2 and P1 experiments applied together, `MAPL.generic.scenarios.pending`
(the P1 experiment is superseded: see the 2026-11-10 measurement paragraph)
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
  `history_1` pass; `statistics_real` still fails for R1, R2 and P1 (as
  diagnosed; with R1 in the tree it fails for R2 and P1 only). Outdated:
  see the 2026-11-10 measurement paragraph above (P1 resolved, one check left).
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
  Note 2026-11-09: `statistics` passes with and without the R1 fix in the tree
  (see the measurement paragraph); the failure described here was not traced.
- **R2. Export aliasing assumes every export is a Field.**
  `try_alias_export_field` calls `ESMF_StateGet(..., field=)` once the item
  is found, without checking `item_kind`. VECTOR and SERVICE exports are
  FieldBundles, so the call returns rc=13. This corrects the first-pass
  claim that "SERVICE items are excluded from graph resolution": they are
  not excluded. They reach `resolve_one`'s exact-match branch like any
  other item. Fixed by `C2-alias-nonfield`: the routine branches on
  `item_kind`; a FieldBundle (VECTOR, SERVICE, BRACKET) is aliased with
  `MAPL_NamedAlias(bundle)` and set as the node payload; any other kind is
  "not found". `service_service` and `service_with_options` were red
  (`Failures: 24` each, `rc=13`) and are green and ready (masked, M3).
  `vector_1` already passed before the fix (no `rc=13` in its log) and stays
  pending (its vertical-grid mismatch needed a bundle vertical Transform; ready since `C1-vertical-b2`). `statistics_real` no longer logs `rc=13`; its P1 failures are gone (see P1) and it is ready (see its table row).
- **P1. Run-phase method-node arguments are keyed by short name only (resolved: the node is no longer built at connect time).**
  `graphbuilder_build_run_phase_node` declares one argument per import/export
  under `var_spec%short_name`. `StatisticsGridComp` advertises an import and
  an export with the same name (`A/TS` in and `A/TS` out), so
  `MethodGraphNode%declare_argument` rejects the second one. An import and
  an export sharing a short name is legitimate; the graph must not infer
  anything from item names. Experiment (a hack, not a fix): mangling the
  key as `'in:'//short_name` made `statistics_real` pass, but broke
  `Test_GraphBuilder_RunPhaseNode` (it looks arguments up by bare name).
  Resolution (owner decision, 2026-11-10): nothing reads or invokes the
  node, so `graphbuilder_run_connect_hook` no longer builds it;
  `graphbuilder_build_run_phase_node` stays as dormant code (still called by
  `Test_GraphBuilder_RunPhaseNode`). The keying problem remains if the node
  is revived: key arguments structurally by (state intent, short name), or
  keep separate import/export argument sets, with intent passed to
  `get_argument`/`get_argument_binding`. Effect: `statistics_real` loses its
  22 P1 failures; one other check remained until `C2-statistics-real` (see its table row).

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
- **M2. Vertical grid mismatches are detected but not repaired.** In every
  `vertical_*` scenario the grid is declared per component, not per item.
  `make_vertical_grid_characteristic` gives each field-carrying item a
  DEFERRED VERTICAL_GRID entry (`make_characteristics.F90:332-338`), and
  `resolve_deferred` fills it from the component grid at GEOM_B. Each
  component builds its grid through the grid manager, which assigns a fresh
  id, and `needs_extension_for` compares the two held grids by the legacy
  rule (same id, or `VerticalGrid%matches`), so two components with
  identical fixed levels still mismatch (as in legacy). Since C1-vertical-b1
  a Field pair whose two grids both have level values in their one shared
  dimension gets a `VerticalColumnRegridTransform` (`vertical_regridding`,
  `vertical_alignment_*` are ready), and a VECTOR pair gets the bundle form
  of it since C1-vertical-b2 (`vector_1`). Still unsupported at connect: a
  model grid on either side (`vertical_regridding_2`, `_3`), a non-center
  stagger, a plain FIELDBUNDLE and a VECTORBRACKET. A BRACKET needs no
  vertical bundle Transform: the time interpolation makes it a Field first.
- **M3. SERVICE is not materialized by the graph; VECTOR and BRACKET
  bundles are.** In graph mode, as for a Field, the graph creates an empty
  CREATED bundle at advertise for every declared VECTOR item (import, export
  and internal) and for every BRACKET export, with either switch setting. A
  BRACKET is an export only, by design: it exists for ExtData (regrid before
  time interpolation), and a bracket import or internal is not supported (the
  spec refuses an import; an internal gets no bundle). A bundle gets its
  members (a vector's two, a bracket's `bracket_size`, as legacy `allocate`)
  when a connection consumes it, or, for a VECTOR internal with
  `graph_owns_user_states`, at `allocate_internals`. An unconnected export or import keeps an empty
  bundle. With `graph_owns_user_states` the bundle is put in the user states.
  With the switch off, legacy fills the user states and the graph adds only
  Fields; an exact-match SERVICE or VECTOR export is aliased from the legacy
  bundle (`C2-alias-nonfield`), which replaces the created bundle, destroying
  it with its members (also for a chain input, not only an exact match);
  SERVICE has no created bundle. A BRACKET export without `bracket_size` fails with a
  message (legacy leaves the size undefined); more than 2 still fails.
  `unsupported_item_class:<shape>`
  remains for SERVICE (`service_service`, `service_with_options`,
  `statistics_real` otherwise leave those items to legacy `add_to_states`;
  the graph creates no SERVICE bundle: `C2-service-exports`), for a
  non-Field chain link other than VECTOR or BRACKET, and for a bundle used
  as an expression input. Flipping the SERVICE scenarios to ready gives a
  masked pass, like the ready rows in the next section. `service_service`
  and `service_with_options` are flipped, and `vector_1` since
  `C1-vertical-b2`.
  `expression_defer_geom` no longer logs this (re-measured 2026-11-08): its
  expression is evaluated on the graph path. Since C1-geom-c-a the graph
  builds a regrid chain for a VECTOR or BRACKET geometry mismatch, and since
  C1-geom-c-b it materializes a VECTOR or BRACKET chain link as an allocated
  bundle without members (the Transform creates them at compute), including
  for a units or typekind step on a VECTOR. A link of another non-Field
  class, or a bundle link whose step has no bundle Transform (vertical
  grid), is still refused (`unsupported_item_class:<class>`, or the
  characteristic name).

Suggested C2 split: `C2-geom-push` (G1), `C2-alias-owner` (R1),
`C2-alias-nonfield` (R2), `C2-runphase-args` (P1, key arguments by intent
and name), `C2-vertical-characteristic` (M2: the graph does not repair a
vertical grid mismatch). R1 and R2 touch the same
procedure, so do them in sequence, not in parallel.

## What "ready" means today, and how much is graph-produced

With the user-state switch off (the default), `initialize_advertise` and
`initialize_realize_accepted` call `StateRegistry%add_to_states` in every
graph mode (only `initialize_accept_transfer` is gated on
`GRAPH_MODE_GRAPH`); with the switch on in `GRAPH` mode the graph fills the
user states and the outer states instead (`ARCHITECTURE.md` §2.4).
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

`regrid*` and `propagate_geom` are ready since C1-geom-b. `vertical_regridding`,
the `vertical_alignment_*` scenarios and `vertical_orientation_flip` are ready
since C1-vertical-b1; `vector_1`, `vector_vertical_regrid` and
`vector_orientation_flip` since C1-vertical-b2; `vertical_regridding_2`, `_3`
(model grids) are not (M2).

## Legacy and graph behavior differences

Cases where legacy behaves differently from the graph design and legacy is
deliberately left alone (no legacy-behavior fixes; see the plan's "Working
with the owner"). Tasks add a row when they find one. This table is the
checklist for switching legacy off.

| Item | Legacy | Graph | Found by |
|------|--------|-------|----------|
| R4 expression inputs with an R8 consumer (`expression_per_consumer_r8`) | orders TYPEKIND before CLASS (`specs/ExpressionClassAspect.F90:119-124`); precision coupler fails on the expression item (`transforms/CouplerMetaComponent.F90:588`). Import couplers run in every mode except graph mode with the user-state switch on, blocking this scenario otherwise; no legacy fix | converts each input to consumer precision before evaluation, then converts result units. Since B3-SkipLegacyCouplers the scenario passes with the switch on (NAG 2026-11-09, run alone). It is ready now, graph mode only, and its table entry sets `graph_owns_user_states` (measured 2026-11-10: 11 checks pass on NAG and gfortran; with the switch dropped the legacy coupler fails it again) | 3.23b B3-ExpressionPerConsumer |
| `geometry_mirror` / `vertical_grid_mirror` on an item, at the GEOM_A push | `make_GeomAspect` / `make_VerticalGridAspect` ignore the request; the component geom / vertical grid is pushed and the aspect becomes SPECIFIED, so a connection to an export on another grid regrids | item stays MIRRORED and adopts the exporter's geometry at connection (no regrid) | 3.39 (dropped) |
| EXPRESSION export with no geometry and no mirror flag, at the GEOM_A push | gets the component geom / vertical grid (SPECIFIED) | stays MIRRORED (design §5) | 3.39 preflight; revisit with 3.12/3.23 |
| `long_name` undeclared on an export or internal | none; an import inherits the export's in `connect_to_export` | falls back to the field dictionary, then the StandardName (deliberate, A8) | 3.13 |
| QuantityType value `QUANTITY_UNKNOWN` as a quantity-axis wildcard | legacy `QuantityTypeAspect%matches` keeps the wildcard: either side `QUANTITY_UNKNOWN` matches anything | dropped from `pair_outcome`: a declared `QUANTITY_UNKNOWN` now counts as undeclared (DEFERRED on EXPORT/INTERNAL, UNCHECKED on IMPORT), never reaching `pair_outcome` as a SPECIFIED value (`BASIS_NONE` stays a basis wildcard) | 3.36 |
| Unconnected export that a re-export names | allocated: the re-export chain activates the source, so `allocate_items` allocates it (a legacy bug; owner decision 2026-11-07) | stays created (GRIDSET once its geometry is written), not allocated; only a connection that consumes the export allocates it | 3.41a B3-FieldPerNode |
| Propagated child export in the user export state | `add_to_states(mode='user')` skips every item whose `comp_name` is non-empty (`registry/StateRegistry_Actions_smod.F90:73`), so a child's propagated export is not in the parent's user export state | with the user-state switch on, a propagated port (`child/E`, a slash in the name) is skipped by the user passes (advertise and accept_transfer) and written only to the OUTER export state, in a substate, at REALIZE_ACCEPTED (`populate_export_ports` with `include_propagated`); same as legacy | 3.41b B3-ExportPortPopulation; corrected by 3.42 B3-OuterStatePopulation |
| Position-labelled names (`T(1)`, `T(2)`, `I_A1(1)`, `A/E_A1(1)`) in the user and outer states | `add_to_states` writes some extension Fields under a name with a position suffix | the graph writes each item once under its declared or port name; no position-labelled copies. Expectation items for these names carry `legacy_only: true`, which `Test_Scenarios` skips when the graph owns the states (registry and both modes still check them). Owner decision 2026-11-07 | 3.42 B3-OuterStatePopulation |
| Equal basic vertical grids (same layer count, different manager ids) | `VerticalGridAspect%typesafe_matches` (`specs/VerticalGridAspect.F90` ~262-264) uses `vertical_grid%matches()`, which for `BasicVerticalGrid` compares layer counts, so two basic grids with the same `num_levels` match | fixed: when both sides hold a grid, `needs_extension_for` uses the same rule (same id, or the export grid `matches` the import grid), so no difference remains. Two basic grids with different counts are INCOMPATIBLE at connect (owner decision 2026-11-08); legacy has no basic-to-basic regrid either (its coordinate field fails) | fixed in C1-vertical-b1 |
| Stagger-only mismatch on the same (or matching) vertical grid, e.g. a CENTER export into an EDGE import | `typesafe_matches` (`specs/VerticalGridAspect.F90:193-277`) never compares the staggers: equal grids match, so no extension is built; the field shapes then disagree | VERTICAL_STAGGER mismatch, reported unsupported at connect (no Transform moves data between centers and edges; plan C1-vertical-a) | C1-vertical-b1 |
| Orientation difference where a vertical grid is Basic (e.g. export `downward`, import `upward`, both on the parent's Basic grid, or a Basic grid against a fixed grid) | errors at connect, in every mode: `make_transform` (`specs/VerticalGridAspect/make_transform.F90`) calls `find_common_physical_dimension` and then `get_coordinate_field` before it reads the alignments. A Basic grid reports the dimension `<unknown>`; Basic against Basic reaches `BasicVerticalGrid%get_coordinate_field` (`BasicVerticalGrid.F90:84`), which fails with "BasicVerticalGrid should have been connected to a different subclass"; Basic against a fixed grid has no common dimension and fails earlier | no step: the pair is wired directly and the data are copied in the export's level order. A warning is logged only when the Basic side has an explicit alignment opposite to the partner's real direction (two Basic grids with opposite explicit alignments also warn; a Basic grid with no alignment never warns). An explicit `vertical_alignment:` on a Basic item is still stored and written to the Field, but never reorders data. A single-level UNSPECIFIED orientation still compares unequal to up and down. Unit-tested only (`test_basic_grid_opposite_alignments_wire_directly_with_warning` and others): a scenario cannot run, because legacy fails the connection in every mode, including a `graph_only` row | C1-vertical-basic-orientation |
| 2D source (`vertical_dim_spec: NONE`) into a 3D import | `typesafe_matches` returns match when the source stagger is NONE (`:232-235`): connects with no step | an explicit NONE still mismatches a 3D import (design §5.1): a stagger step, reported unsupported | C1-vertical-b1 |
| Edge-staggered field between different vertical grids | `VerticalRegridTransform` adjusts the coordinates for the field's stagger (`adjust_coords`, `transforms/VerticalRegridTransform.F90`) | reported unsupported at connect (`VerticalGridCharacteristic%build_transform_for_item`: the column regrid handles level centers only between different grids) | C1-vertical-b1 |
| Model grid (PLE/ZLE coordinate) as source or destination of a vertical regrid | builds the coordinate fields through `ModelVerticalGrid%get_coordinate_field_with_coupler` (a `StateRegistry` extension with a coupler; `specs/VerticalGridAspect/make_transform.F90:96-110`) | reported unsupported VERTICAL_GRID at connect (`VerticalGrid%has_column_coordinate` is false for a model grid; no coordinate input port yet, a later task). Hybrid mode is unaffected: legacy does the regrid | C1-vertical-b1 |
| R8 field across a vertical regrid | `VerticalRegridTransform` reads R4 pointers only; legacy connects and fails inside ESMF at the first compute | regrids R8 fields (`VerticalColumnRegridTransform`). The vertical step runs before the typekind step and its output link keeps the export typekind, so an R8 export is regridded with R8 weights and then narrowed if the import is R4, and an R4 export into an R8 import is regridded with R4 weights and then converted. The Transform itself fails with a clear message on a mixed-kind pair, which the graph never builds | C1-vertical-r8 |
| Both export and import declare a regridder param, on different geoms | `GeomAspect%make_transform` `_FAIL`s (`specs/GeomAspect.F90:191-213`); connection setup aborts | reported per pair as an incompatible GEOMETRY conflict naming both signatures (with params); the component's other connections still resolve. Owner decision 2026-11-08 | C1-geom-b |
| Geometry mismatch on a FIELDBUNDLE or VECTORBRACKET item | `RegridTransform%update` regrids Fields and FieldBundles (`transforms/RegridTransform.F90:218-235`) | reports unsupported GEOMETRY (owner decisions 2026-11-08 and 2026-11-09; VECTORBRACKET is a follow-up, FIELDBUNDLE only if a client needs it). Hybrid mode is unaffected: legacy does the regrid | C1-geom-b; C1-geom-c-a |
| Geometry mismatch on a VECTOR or BRACKET item | `RegridTransform%update` regrids the bundle (`transforms/RegridTransform.F90:218-235`) | chain built with `HorizontalBundleRegridTransform` (one rotated regrid call for a VECTOR, exporter basis kept); the chain link is an allocated bundle without members, whose members the Transform creates at compute (a clone of each input member on the output geom, created again when the input member count changes) | C1-geom-c-a; C1-geom-c-b |
| Units or typekind mismatch on a VECTOR item | `ConvertUnitsTransform%update_field_bundle` and `CopyTransform` (`FieldBundleCopy`) convert the bundle member by member | `UnitsConverterTransform` and `TypeKindConverterTransform` convert member by member into members they create at compute on the output bundle (cloned from the input members, in the output typekind and units); the vector basis is carried to the output. A bracket is time-interpolated to a Field before these steps, so they never see a bracket | C1-geom-c-b |
| Standard names of the two components of a VECTOR | `VectorClassAspect%matches` compares the two split component names (`specs/VectorClassAspect.F90`, `standard_name_aspects(i)%matches`), so a mismatch in either component is a mismatch: legacy warns (permissive, the default) or rejects (strict) | the two names are split from `(n1,n2) suffix` at advertise and kept on the descriptor only to name the members the graph creates; they are not compared at connect: the graph neither warns nor rejects (`make_standard_name_characteristic` returns early for VECTOR), so a vector pair with different component names connects without a message | C2-bundle-exports (owner decision 2026-11-10) |
| First compute of a bracket regrid with `bracket_updated` false | skips the regrid, so the output is never filled | regrids on the first compute whatever the flag says | C1-geom-c-a |
| Bracket regrid after a regridder, geom, typekind or member-set change | regrids only when `bracket_updated` is true | also regrids after such a change, so the output is never stale | C1-geom-c-a |
| Interpolation weights of a regridded bracket | copied to the extension bundle by the coupler (`CouplerMetaComponent` `update_time_varying`) | the Transform copies the input weights to its output | C1-geom-c-a |
| Members of a regridded bracket or vector | come from the cloned class aspect at allocate | the Transform clones the input members onto the output geom at compute and recreates them when the count changes | C1-geom-c-a |
| Vertical regrid between different fixed-level grids where a field's alignment differs from its grid's direction | `VerticalRegridTransform` reverses each side's levels by the field's alignment (`transforms/VerticalRegridTransform.F90:430-436`, `:519-623`), but fixed-level coordinates are in grid order, so values are wrong: UP grid [1000,500,100] hPa to [800,300] hPa, T = 2p+1: a source stored DOWN gives [521,1501] instead of [1601,601]; a destination stored DOWN gets its output reversed. Model-grid (PLE/ZLE) coordinates arrive in the field's own order, where the flip should be right (read, not run) | builds each side's levels in that field's storage order, then regrids; correct values. Owner decision 2026-11-08 | C1-vertical-a |
| Import levels outside the export's level range | `compute_linear_map` copies the end value silently in Release; Debug builds abort on an assertion (`infrastructure/vertical/vertical/VerticalLinearMap.F90`) | copies the end value in every build and logs one warning per weight build. Owner decision 2026-11-08 | C1-vertical-a |
| Two connections into one import | last connection silently wins, by reading, not run: `connect_to_export` replaces the import's payload with each export in turn (`specs/FieldClassAspect.F90:292-304`); the one-connection assert is commented out (`connection/SimpleConnection.F90:105`) | the first connection wins. A second producer resolved in the same connect pass, for an ordinary or inout-borrower pairing, is reported per pair as a connection conflict naming the item (`second_producer`, `GraphBuilder_ConnectionResolution_smod.F90:410,519,1445`); the first connection's edge stays and the component's other connections still resolve. A connection arriving in a later pass, after the import settled, is silently skipped (`resolve_one`, `:254-273`); other edge kinds have no pre-check. Owner decision 2026-11-08 | C4-ForwardNetwork |
