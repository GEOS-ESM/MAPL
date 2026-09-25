## 1. `FieldBundleClone` infrastructure

- [x] 1.1 Create `infrastructure/field_bundle/FieldBundleClone.F90` with module `mapl_FieldBundleClone_mod`, public interface `FieldBundleClone(bundle_in, bundle_out, rc)` that creates `bundle_out` via `FieldBundleCreate` and, for each field in `bundle_in` (via `ESMF_FieldBundleGet(..., fieldList=...)`), calls `FieldClone` and adds the clone to `bundle_out`.
- [x] 1.2 Export `MAPL_FieldBundleClone => FieldBundleClone` through `infrastructure/field_bundle/API.F90` (`mapl_field_bundle_api`), following the existing `MAPL_FieldBundleCopy` pattern.
- [x] 1.3 Register the new source file in the appropriate `CMakeLists.txt` under `infrastructure/field_bundle/`.
- [x] 1.4 Add a pFUnit test (e.g. `Test_FieldBundleClone.pf`) verifying: cloned bundle has same field count/names/typekinds/shapes as source; cloned fields are independently allocated (modifying source field data does not change clone data); clone works for bundles with zero fields.

## 2. Shared restart field-selection helper

- [x] 2.1 Identify the common logic in `superstructure/generic/RestartHandler.F90` (`write`/`read` methods: `MAPL_StateGet(state, bundle, rc)` + `MAPL_FieldBundleFilter` with `predicate_incomplete_`/`predicate_skip_restart_`) and extract a single helper (e.g. `RestartHandler%get_restart_bundle` or a private module function) that returns the restart-eligible flattened bundle for a given `ESMF_State` and file mode (write vs. read).
- [x] 2.2 Update `RestartHandler%write` and `RestartHandler%read` to call the new shared helper instead of duplicating `MAPL_StateGet` + filter calls.
- [x] 2.3 Confirm `write_restart.F90` and `initialize_read_restart.F90` continue to work unchanged through `RestartHandler%write`/`read` (no call-site changes needed there); run existing restart pFUnit tests to confirm no behavior change.

## 3. `OuterMetaComponent` in-memory checkpoint storage

- [x] 3.1 Add private type component `memory_checkpoint : type(ESMF_State)` to `OuterMetaComponent` in `superstructure/generic/OuterMetaComponent.F90`.
- [x] 3.2 Add private logical type component `has_memory_checkpoint` (default `.false.`) to `OuterMetaComponent`, tracking whether any in-memory write has occurred; initialize it in `new_outer_meta` (`OuterMetaComponent/new_outer_meta.F90`).
- [x] 3.3 Add a private helper procedure (e.g. `ensure_memory_checkpoint_`) that lazily creates `memory_checkpoint` (via `ESMF_StateCreate`) with three nested states named `"import"`, `"export"`, `"internal"` (via `ESMF_StateCreate` + `ESMF_StateAdd`) on first use, mirroring `MultiState`'s lazy-state-creation pattern (`superstructure/component/MultiState.F90`).
- [x] 3.4 Add a private accessor to retrieve the nested `ESMF_State` for a given `ESMF_StateIntent_Flag` (import/export/internal) from `memory_checkpoint`, mirroring `MultiState%get_state_by_esmf_intent`.

## 4. In-memory checkpoint write

- [x] 4.1 In `superstructure/generic/OuterMetaComponent/write_restart.F90`, replace the empty `GENERIC_INTERNAL_WRITE_RESTART` branch: for each of import/export/internal whose `this%component_spec%misc%checkpoint_controls%get_<state>()` is `.true.`, build the state's restart-eligible bundle using the shared helper from Task 2, clone it via `MAPL_FieldBundleClone` (Task 1), and store the resulting fields into the corresponding nested state of `memory_checkpoint` (destroying/replacing any previously stored fields for that state first).
- [x] 4.2 Set `has_memory_checkpoint = .true.` after a successful write of at least one state.
- [x] 4.3 Ensure the write branch calls `ensure_memory_checkpoint_` (Task 3.3) before storing, and returns via `_RETURN(ESMF_SUCCESS)` consistent with existing error-handling conventions in the file.

## 5. In-memory checkpoint read

- [x] 5.1 In `superstructure/generic/OuterMetaComponent/read_restart.F90`, replace the empty `GENERIC_INTERNAL_READ_RESTART` branch: for each of import/export/internal whose `this%component_spec%misc%restart_controls%get_<state>()` is `.true.`, assert (`_ASSERT`/`_FAIL`) that `has_memory_checkpoint` is `.true.` and that the corresponding nested state in `memory_checkpoint` holds a stored snapshot for that state; fail clearly if not.
- [x] 5.2 For each enabled, available state, fetch the live state's current restart-eligible bundle (shared helper from Task 2) and the stored checkpoint bundle, and call `MAPL_FieldBundleCopy` to copy field data values from the stored bundle into the live bundle's fields, leaving the live field objects themselves unchanged.
- [x] 5.3 Confirm states whose `restart_controls` flag is disabled are left untouched by the read branch.

## 6. Tests

- [x] 6.1 Add a pFUnit test exercising a write-then-read round trip on a `GenericGridComp`-based test harness: write in-memory checkpoint, mutate live import/export/internal field data, read in-memory checkpoint back, and assert original data values are restored.
- [x] 6.2 Add a pFUnit test asserting an in-memory read with no prior write for a given state fails with an error (per spec "Read with no prior write" scenario).
- [x] 6.3 Add a pFUnit test asserting a second in-memory write overwrites the first snapshot (read after two writes returns the second write's data, not the first's).
- [x] 6.4 Add a pFUnit test asserting states excluded by `checkpoint_controls`/`restart_controls` are neither stored nor restored.
- [x] 6.5 Confirm existing netCDF restart/checkpoint pFUnit tests still pass unchanged (regression check for the Task 2 refactor).

## 7. Validation

- [x] 7.1 Build MAPL with `module load nag-stack` per existing NAG build workflow.
- [x] 7.2 Run the full generic/restart/field_bundle pFUnit suites with `module load nag-stack` and confirm all pass, including the new tests from Task 6.
- [x] 7.3 Update `openspec/specs/internal-checkpoint/spec.md` expectations are satisfied by running `openspec validate add-memory-checkpoint --strict` before archiving.
