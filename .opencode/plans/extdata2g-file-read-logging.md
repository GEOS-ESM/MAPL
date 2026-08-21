# ExtData2G File Read Logging

## Feature summary

Add an opt-in capability to the ExtData2G component to record every unique file
read during a run and write a YAML report at finalization. Enabled by a new
key in the ExtData YAML config:

```yaml
log_files_read: extdata_files_read.yaml
```

The output file contains the run time range and the list of unique filenames:

```yaml
run_start: 2004-01-01T21:00:00
run_end:   2004-04-15T21:00:00
files_read:
  - case39.2004.nc4
```

## Status: IMPLEMENTATION COMPLETE (pending golden file population)

All source changes have been made, including two post-implementation bug fixes
(see bug fixes section below). The one remaining manual step is to populate
the golden reference file for case39 after the first successful build+run (see
below).

## Files changed

### Source

| File | Change |
|------|--------|
| `gridcomps/ExtData2G/ExtDataConfig.F90` | Added `log_files_read` (logical) and `files_read_log_path` (allocatable string) to `ExtDataConfig` type; parse `log_files_read` key in `new_ExtDataConfig_from_yaml` |
| `gridcomps/ExtData2G/ExtDataGridCompNG.F90` | Added `log_files_read`, `files_read_log_path`, `files_read` (StringVector), `run_start_time`, `run_end_time` to `MAPL_ExtData_State`; propagate config in `Initialize_`; harvest filenames from item brackets in `Run_`; write YAML log in `Finalize_` via `ESMF_HConfigFileSave`; added private `string_in_vector` helper for manual deduplication |

### Tests

| File | Change |
|------|--------|
| `Tests/ExtData_Testing_Framework/run_extdata.cmake` | After `ExtDataDriver.x` exits successfully, check for optional `compare.rc`; each non-comment line is `<generated> <reference>` and `cmake -E compare_files` is used to diff them |
| `Tests/ExtData_Testing_Framework/test_cases/extdata_2g_cases.txt` | Added `case39` |
| `Tests/ExtData_Testing_Framework/test_cases/case39/` | New test case directory (see below) |

## case39 structure

```
README                            — one-line description
CAP.rc                            — 2 phases: CAP1.rc, CAP2.rc
CAP1.rc                           — phase 1: GenerateExports + write files via History
CAP2.rc                           — phase 2: ExtData reads with log_files_read enabled
AGCM1.rc                          — RUN_MODE: GenerateExports (VAR2D R4 2D, VAR3D R8 3D)
AGCM2.rc                          — RUN_MODE: FillImport (no value assertions)
HISTORY1.rc                       — writes case39.%y4.nc4
HISTORY2.rc                       — inactive
extdata.yaml                      — active exports VAR2D, VAR3D + log_files_read: extdata_files_read.yaml
ExtData.rc                        — 1G fallback config (used when IS_EXTDATA1G=YES)
compare.rc                        — extdata_files_read.yaml extdata_files_read_expected.yaml
extdata_files_read_expected.yaml  — PLACEHOLDER: replace with actual output after first run
```

## Key design decisions

- **Key format**: string value `log_files_read: <filename>` — enables logging
  and names the output file simultaneously. An absent key means no logging.
- **Unique filenames only**: `StringVector` (gFTL v1) is used with a manual
  deduplication check via the private `string_in_vector` helper. `gFTL_StringSet`
  does not exist in this codebase (ExtData2G links `GFTL::gftl` v1 only).
- **Finalize hook**: the existing `Finalize_` subroutine (registered for
  `ESMF_METHOD_FINALIZE`) is extended in-place. State is retrieved via the
  existing `extract_` helper. The YAML is written before `MAPL_GenericFinalize`.
- **ESMF_HConfigFileSave pattern**: follows Cap.F90 and HistoryGridComp patterns.
- **Test verification**: `cmake -E compare_files` exact match against a committed
  golden file. `run_extdata.cmake` extended with optional `compare.rc` support
  (all existing tests unaffected — they have no `compare.rc`).

## Difference from mapl3g implementation

| Aspect | mapl3g (3G) | ExtData2G |
|---|---|---|
| State type | `ExtDataGridComp` (private, `_GET_NAMED_PRIVATE_STATE`) | `MAPL_ExtData_State` (legacy wrap, `ESMF_UserCompGetInternalState` via `extract_`) |
| Finalize hook | Separate `finalize_extdata` subroutine, registered in `setServices` | Extend existing `Finalize_` in-place |
| Filename source | `ExtDataReader%filename_map` → `get_unique_filenames` | `item%modelGridFields%comp1%get_parameters('L'/'R')` — queried for all non-const items in `Run_` |
| Config propagation | `modify_advertise` | `Initialize_` (after `new_ExtDataOldTypesCreator`) |
| Deduplication | `gFTL2_StringSet` (automatic) | `gFTL_StringVector` + `string_in_vector` helper (manual) |

## Remaining manual step after first build

1. Build and run case39 (`ctest -R ExtData2G_case39` or run manually).
2. The run will fail the compare step because `extdata_files_read_expected.yaml`
   is still a placeholder comment.
3. Temporarily comment out the `rm -rf` in `run_extdata.cmake`, run the test,
   copy the generated `extdata_files_read.yaml` from the temp run directory, and
   commit it as the golden file:
   `Tests/ExtData_Testing_Framework/test_cases/case39/extdata_files_read_expected.yaml`
4. Restore `run_extdata.cmake` and re-run to confirm the test passes.

## Notes on `_RC` macro usage

`ESMF_HConfigIsDefined` is a pure inquiry function — do **not** pass `_RC` to
it inside an `if(...)` condition. This matches the existing usage pattern
throughout `ExtDataConfig.F90` and the fix documented in the mapl3g plan.

## Bug fixes

### Bug 1: `%of()` → `%get()` (build error)

`StringVectorIterator` uses `%get()` (value return), not `%of()` (pointer
return). Two occurrences fixed in `ExtDataGridCompNG.F90`:
- Line 842: `filename => fiter%of()` → `filename = fiter%get()`
- Line 1953: `val => iter%of()` → `val = iter%get()`
- Corresponding declarations changed from `character(len=:), pointer` to
  `character(len=ESMF_MAXSTR)`.

### Bug 2: `files_read: ~` — empty output despite files being read

**Root cause (first attempt)**: The file-logging loop ran *after*
`MAPL_ExtDataDestroyCFIO`, which calls `IOBundles%clear()`. Fixed by moving
the loop before the destroy call.

**Root cause (second attempt / real fix)**: `IOBundles` only contains entries
for brackets that were *newly updated* on a given timestep (guarded by
`update=.true.` inside `IOBundle_Add_Entry`). On timesteps where no bracket
changes, `IOBundles` is empty so nothing was logged.

**Final fix**: Replaced the `IOBundles`-based loop with a direct loop over
`self%primary%import_names`, calling
`item%modelGridFields%comp1%get_parameters('L', file=...)` and
`get_parameters('R', file=...)` without any update guard. This always returns
the files currently held in the L/R brackets, regardless of whether they were
refreshed this timestep. Uses the existing `file_processed` local variable;
skips const items and `file_not_found` sentinels.
