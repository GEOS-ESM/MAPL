# ExtData File Read Logging

## Feature summary

Add an opt-in capability to the ExtData component to record every unique file
read during a run and write a YAML report at finalization. Enabled by a new
key in the ExtData input YAML:

```yaml
log_files_read: extdata_files_read.yaml
```

The output file contains the run time range and the list of unique filenames:

```yaml
run_start: 2004-01-20T00:00:00
run_end:   2004-04-19T21:00:00
files_read:
  - test.nc4
```

## Status: IMPLEMENTATION COMPLETE (pending build verification)

All source changes have been made. The one remaining manual step is to populate
the golden reference file for case44 after the first successful build+run (see
below).

## Files changed

### Source

| File | Change |
|------|--------|
| `gridcomps/extdata/ExtDataConfig.F90` | Added `log_files_read` (logical) and `files_read_log_path` (allocatable string) to `ExtDataConfig` type; parse `log_files_read` key in `new_ExtDataConfig_from_yaml` |
| `gridcomps/extdata/ExtDataFileReader.F90` | Added `use gFTL2_StringSet`; added `get_unique_filenames` procedure to `ExtDataReader` type — iterates internal `filename_map` and inserts each value into a caller-supplied `StringSet` |
| `gridcomps/extdata/ExtDataGridComp.F90` | Added `use gFTL2_StringSet`; extended `ExtDataGridComp` private type with `log_files_read`, `files_read_log_path`, `files_read` (StringSet), `run_start_time`, `run_end_time`; registered `finalize_extdata` in `setServices`; propagate config in `modify_advertise`; accumulate filenames in `run`; new `finalize_extdata` subroutine writes the YAML log via `ESMF_HConfigFileSave` |

### Tests

| File | Change |
|------|--------|
| `tests/MAPL3G_Component_Testing_Framework/run_comp_tester.cmake` | After all GEOS.x steps, check for optional `compare.rc`; each line is `<generated> <reference>` and `cmake -E compare_files` is used to diff them |
| `tests/MAPL3G_Component_Testing_Framework/test_cases/cases.txt` | Added `case44` |
| `tests/MAPL3G_Component_Testing_Framework/test_case_descriptions.md` | Added case44 description |
| `tests/MAPL3G_Component_Testing_Framework/test_cases/case44/` | New test case directory (see below) |

## case44 structure

```
steps.rc                          — 2 steps: cap1.yaml, cap2.yaml
nproc.rc                          — 1
cap_restart1.yaml                 — currTime: 2004-01-01T22:00:00
cap_restart2.yaml                 — currTime: 2004-01-20T00:00:00
cap1.yaml                         — step 1: GenerateExports + write files via History
cap2.yaml                         — step 2: ExtData reads with log_files_read enabled
GCM1.yaml                         — RUN_MODE: GenerateExports (E_1 R4 2D, E_2 R8 3D)
GCM2.yaml                         — RUN_MODE: FillImports (no value assertions)
extdata1.yaml                     — no active exports
extdata2.yaml                     — active exports E_1, E_2 + log_files_read: extdata_files_read.yaml
history1.yaml                     — writes test.nc4 at PT1H frequency
history2.yaml                     — inactive
logging.yaml                      — copied from case01
compare.rc                        — extdata_files_read.yaml extdata_files_read_expected.yaml
extdata_files_read_expected.yaml  — PLACEHOLDER: replace with actual output after first run
```

## Key design decisions

- **Key format**: string value `log_files_read: <filename>` — enables logging
  and names the output file simultaneously. An absent key means no logging.
- **Unique filenames only**: `StringSet` (gFTL2) is used for deduplication;
  the same file read at multiple timesteps appears only once.
- **Finalize hook**: registered via `MAPL_GridCompSetEntryPoint` with
  `ESMF_METHOD_FINALIZE` (no `phase_name` — defaults to
  `'GENERIC::FINALIZE_USER'` which is called by the MAPL generic finalize).
- **ESMF_HConfigFileSave pattern**: follows Cap.F90 `update_restart` and
  HistoryGridComp_private.F90 sequence-building patterns.
- **Test verification**: `cmake -E compare_files` exact match against a
  committed golden file. Can be improved to fuzzy/structural comparison later.

## Remaining manual step after first build

1. Build and run case44 (`ctest -R MAPL3G_Comp_Test_case44` or run manually).
2. The run will fail the compare step because `extdata_files_read_expected.yaml`
   is still a placeholder comment.
3. Copy the generated `extdata_files_read.yaml` from the temp run directory —
   or re-run with a persistent temp dir — and commit it as the golden file:
   `tests/MAPL3G_Component_Testing_Framework/test_cases/case44/extdata_files_read_expected.yaml`
4. To capture the output without the test cleaning up, temporarily comment out
   the `rm -rf` in `run_comp_tester.cmake`, run the test, copy the file, then
   restore the cmake script.

## Notes on `_RC` macro usage

`ESMF_HConfigIsDefined` is a pure inquiry function — do **not** pass `_RC` to
it inside an `if(...)` condition, as the macro expands to a statement
(`;if(MAPL_Verify(...)) return`) which is a syntax error inside a condition.
This bug was found and fixed during implementation (line 167 of ExtDataConfig.F90).
Use the bare call without `_RC`, matching the existing pattern on lines 79, 91,
105, etc. of that file.
