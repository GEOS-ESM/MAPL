## 1. Implement the aliasing procedure

- [x] 1.1 Add `try_alias_export_field(this, export_comp_name, export_var_spec, graph, node_id, found, rc)` to `superstructure/generic/GraphBuilder.F90`: resolve `export_meta` via the existing `component_for` helper, reach `export_meta%get_user_gc_driver()%get_states()%exportState`, and call `ESMF_StateGet(export_state, itemName=export_var_spec%short_name, field=field, rc=status)`.
- [x] 1.2 On success, wrap `field` via `MAPL_NamedAlias(field, name=export_var_spec%short_name, _RC)` (reuse `mapl_FieldNamedAlias_mod`, already imported for `populate_one`) and set it as `node_id`'s `StateItemNode` payload (same `GraphStateItem%set`/`node%set_payload` idiom `materialize_field_extension` already uses); set `found = .true.`.
- [x] 1.3 On failure (non-zero `rc`, or item not found), set `found = .false.` and return success (not an error) - the caller falls back to `try_materialize_field_extension` unchanged.

## 2. Wire into `resolve_one`'s two existing call sites

- [x] 2.1 Exact-match branch (~line 849): call `try_alias_export_field` before the existing `try_materialize_field_extension` call; skip the latter when `found` is true.
- [x] 2.2 Mismatch-branch chain-input materialization (~line 904): same guard, same skip-when-found behavior - identical treatment, no divergence between the two call sites.
- [x] 2.3 Confirm (by reading the resulting diff, not just by intent) that neither call site's existing error-reporting/`unsupported` push-back behavior changes when `try_materialize_field_extension` still runs (the `found = .false.` fallback path) - only the `found = .true.` path is new behavior.

## 3. Confirm phase-timing assumption against real test output

- [x] 3.1 Build and run `Test_GraphModeDemonstration.pf` with task 1-2's changes in place (before touching the test file itself) - confirm via a temporary print/log, or by inspecting whether `try_materialize_field_extension` fires at all for the exact-match branch, whether `try_alias_export_field` succeeds on the FIRST `GENERIC_INIT_ACCEPT_TRANSFER` pass as design.md D2 predicts, or only the second - record whichever is actually observed in a code comment at the call site (same project convention `graph-child-state-population`'s own design.md corrections already follow for a static-trace-vs-observed-behavior mismatch).
- [x] 3.2 If the fallback path (D2) is taken unexpectedly for this fixture, investigate why (e.g. an NUOPC-level detail the static trace missed) before proceeding to task 4 - do not silently rely on the fallback for the demonstration this change exists to provide.

## 4. Update the demonstration test

- [x] 4.1 In `superstructure/generic/tests/Test_GraphModeDemonstration.pf`, add a real `ESMF_GridCompRun(cap_gc, importState=importState, exportState=exportState, clock=clock, phase=GENERIC_RUN_USER, userRC=user_status, _RC)` call after the existing init-phase-sequence loop (mirroring `Test_GraphEquivalenceHarness.pf`'s own `equivalence_fixture_setup` call shape).
- [x] 4.2 Read `comp_dst`'s own real import field's numeric value via `ESMF_FieldGet(result_field, farrayPtr=...)` (matching the real typekind already asserted, R4) and assert it equals `1000.` (the fixture's own hand-checked `10.` hPa -> `1000.` Pa conversion, `units_mismatch_run_phase/description.md`).
- [x] 4.3 Keep all existing assertions (item count, typekind, units, export-proxy-field-allocated) unchanged; update the file's own header comment to record gap (3) as closed, following the same style its existing header already uses for gaps (1)/(2).
- [x] 4.4 Confirm no change is needed to `units_mismatch_run_phase`'s own fixture YAML files (`cap.yaml`/`comp_src.yaml`/`comp_dst.yaml`) - per design.md D6, the existing `run: {T: [10]}` already provides what this test needs.

## 5. Validate

- [x] 5.1 Build `MAPL.generic` and run `Test_GraphModeDemonstration.pf`; confirm the new numeric assertion passes.
- [x] 5.2 Run the full `MAPL.generic`/`MAPL.generic.scenarios` test suites to confirm no regression in `Test_GraphBuilder.pf`, `Test_GraphEquivalenceHarness.pf`, or any other graph-mode test touching `resolve_one`'s two modified call sites.
- [x] 5.3 Update `docs/graph/spec/20-implementation-roadmap.md` §20.4.10's own 11a entry to record this change as landed, mirroring 4e/4f/4g/5b's own existing "**landed** (`openspec/changes/...`)" annotation convention.
