---
name: fix-vrange-handling
description: Context for the feature/bmauer/fixes-#4656 branch. Use when resuming work on the valid_range handling fix for ExtData2G, issue #4656, or the refine_valid_range / check_data_availability / VALIDATE_FILE_RANGES changes.
---

# Feature Branch: fixes-#4656 — valid_range handling in ExtData2G

## Branch

`feature/bmauer/fixes-#4656`, based on `release/v2`

## Repo

`/home/bmauer/models/mapl2g_fix_vrange_handling/MAPL`

## GitHub Issue

**#4656 — "Criteria for valid_range missing data error"**
https://github.com/GEOS-ESM/MAPL/issues/4656

The ask: only crash if data the model actually needs is missing. A user-configured
`valid_range` may span a wide data repository range, but users may only have
downloaded data for their specific run period. ExtData2G should give a clear,
actionable error message rather than crashing with a confusing missing-file assert.

## Design Philosophy

`valid_range` semantics are enforced precisely. `valid_range` itself is **never
mutated**. `refine_valid_range` is used as a diagnostic — it finds what is actually
on disk and stores those timestamps as `on_disk_range`. The `on_disk_range` is then
used at runtime for clamping in `persist_closest` (so handlers clamp to the actual
last file, not to an endpoint the user specified that may not have a file), and for
probing in `find_any_file`.

> **If the run period overlaps `valid_range`, every file needed within that overlap
> must exist — no exceptions.**
> **If the run period is outside `valid_range`, the extrapolation mode must have
> enough data to do its job. Either persist the last value, or have enough data to
> do a climatology.**
>
> Rather than silently proceeding with a narrowed range, we validate and fail fast
> with a clear error message stating what is on disk and what the user needs.

## What Was Implemented

### Changes to `gridcomps/ExtData2G/ExtDataFileStream.F90`

#### New field on `ExtDataFileStream` type
```fortran
type(ESMF_Time), allocatable :: on_disk_range(:)  ! actual first/last file timestamps
```

#### `refine_valid_range` (private)
- Returns `on_disk_first`, `on_disk_last`, `found_any` instead of mutating `valid_range`.
- `intent(in)` on `this`. `valid_range` is never written.
- `found_any = .false.` when no files exist in `valid_range`.

#### `check_data_availability` (new public method)
Signature: `(this, run_range, extrap_outside, unusable, rc)`

1. Calls `refine_valid_range` to discover on-disk range.
2. If `found_any=.false.` → `_FAIL` with message showing `valid_range` and template.
3. Stores `on_disk_first`/`on_disk_last` into `this%on_disk_range(1:2)`.
4. **Overlap check:** computes `max(run_range(1), valid_range(1))` to
   `min(run_range(2), valid_range(2))`. If overlap exists:
   - `on_disk_first > overlap_start + frequency` → `_FAIL` (missing data at start)
   - `on_disk_last  < overlap_end   - frequency` → `_FAIL` (missing data at end)
   - The `± frequency` tolerance accounts for mid-period file timestamps (e.g. Jan 15
     file legitimately covers a Jan 1 overlap boundary).
5. **Extrapolation sufficiency check** (run outside `valid_range`) for `clim`:
   - Extract `vr_yr1`/`vr_yr2` from `valid_range` and determine direction first.
   - Run before `valid_range` → `scan_year = vr_yr1`, `do_gap_scan = .true.`
   - Run after `valid_range`  → `scan_year = vr_yr2`, `do_gap_scan = .true.`
   - **Full-cycle check:** `(yr_last > yr_first) .or. (yr_last == yr_first .and. mm_first == 1 .and. mm_last == 12)`
     - On failure: scan the target year (or full `valid_range` if overlapping) and
       list every missing file explicitly in the error message.
   - **Direction-aware endpoint check:**
     - Before: `on_disk_first > valid_range(1) + frequency` → `_FAIL`
     - After:  `on_disk_last  < valid_range(2) - frequency` → `_FAIL`
   - **Gap scan** (`do_gap_scan=.true.`): probe every expected file in `[Jan 1, Dec 31]`
     of `scan_year`; `_FAIL` on first missing file with timestamp + template.
   - `persist_closest`: `found_any` is sufficient.

### Changes to `gridcomps/ExtData2G/ExtDataGridCompNG.F90`
- `get_global_options` gains a `validate_file_ranges` output argument (default `.true.`),
  reads `VALIDATE_FILE_RANGES` top-level key from `extdata.yaml`.
- Call site stores result into `config_yaml%validate_file_ranges`.
- `ESMF_ClockGet(clock, currtime=run_range(1), stoptime=run_range(2))` is before
  the `fillin_primary` loop; `run_range` is passed to both `fillin_primary` call sites.

### Changes to `gridcomps/ExtData2G/ExtDataOldTypesCreator.F90`
- `ExtDataOldTypesCreator` type has field:
  ```fortran
  logical, public :: validate_file_ranges = .true.
  ```
  (declared after `private` statement with explicit `public` attribute)
- `check_data_availability` call is gated on `this%validate_file_ranges`:
  ```fortran
  if (this%validate_file_ranges .and. user_set_range .and. &
       index(dataset%file_template,'%') /= 0 .and. &
       trim(time_sample%extrap_outside) /= "none") then
     call dataset%check_data_availability(run_range, time_sample%extrap_outside, _RC)
  end if
  ```

### Changes to `gridcomps/ExtData2G/ExtDataAbstractFileHandler.F90`

#### New field on abstract type
```fortran
type(ESMF_Time), allocatable :: on_disk_range(:)
```

#### `initialize`
Copies `on_disk_range` from `file_series` alongside `valid_range`.

#### `find_any_file` probe loop
Prefers `on_disk_range(1)` over `valid_range(1)` as the starting point.
Also fixed a pre-existing bug where the loop advanced by one frequency before
probing, meaning `valid_range(1)` itself was never tried.

### Changes to `gridcomps/ExtData2G/ExtDataSimpleFileHandler.F90`
`persist_closest` clamping uses `on_disk_range` when allocated, falling back to
`valid_range` otherwise.

### `gridcomps/ExtData2G/ExtDataClimFileHandler.F90`
Unchanged from original (uses `valid_range` for year logic; `on_disk_range` NOT used).

## Key Files

| File | Role |
|------|------|
| `gridcomps/ExtData2G/ExtDataFileStream.F90` | `refine_valid_range` (private, diagnostic) + `check_data_availability` (public, validates + sets `on_disk_range`) |
| `gridcomps/ExtData2G/ExtDataOldTypesCreator.F90` | `validate_file_ranges` field + gated `check_data_availability` call site |
| `gridcomps/ExtData2G/ExtDataGridCompNG.F90` | `VALIDATE_FILE_RANGES` yaml key, `run_range` fetch, `fillin_primary` call sites |
| `gridcomps/ExtData2G/ExtDataAbstractFileHandler.F90` | `on_disk_range` field, copy in `initialize`, use in `find_any_file` |
| `gridcomps/ExtData2G/ExtDataSimpleFileHandler.F90` | `persist_closest` clamping uses `on_disk_range` |
| `gridcomps/ExtData2G/ExtDataClimFileHandler.F90` | Unchanged |

## Opt-out Flag

Add `VALIDATE_FILE_RANGES: false` at the top level of `extdata.yaml` to skip all
file existence checks entirely, restoring pre-feature behavior. Default is `true`.

## New Test Cases

### case39 — `persist_closest`, one file, run outside `valid_range`
`Tests/ExtData_Testing_Framework/test_cases/case39/`

- Pass 1: generates one file — `case1.200501.nc4` (Jan 15 2005).
- `valid_range`: set to match the single file (exact timestamps).
- Pass 2: run requests `20080615` — far outside `valid_range`.
  `check_data_availability` passes (`found_any=.true.` sufficient for `persist_closest`).
  Clamps to `on_disk_range(2)`, persists Jan 15 value.
  Expected value: `14.0`.

### case40 — `clim`, `valid_range` matches 2007 on-disk data, run outside in 2008
`Tests/ExtData_Testing_Framework/test_cases/case40/`

- Pass 1: generates 2007 monthly files. `REF_TIME: 20070101 000000`.
- `valid_range: "2007-01-01/2007-12-31"`.
- Pass 2: run requests `20080229 120000`. Year-wraps Feb 29 2008 → Feb 2007.
  Expected value: `45.0+(73.0-45.0)*0.5` = 59.0.

### case40_variation_1 — `clim`, run before `valid_range`, first file late in vr_yr1
`Tests/ExtData_Testing_Framework/test_cases/case40_variation_1/`

- `valid_range: "2004-01-01T00:00:00/2006-12-31T00:00:00"`
- Pass 1: generates daily files starting 2004-12-30 (only one day of 2004).
- Pass 2: run before `valid_range` (2002).
- Expected: **FAIL** — direction check fires because `on_disk_first (2004-12-30) >
  valid_range(1) (2004-01-01) + frequency (1 day)`.

### case40_variation_2 — `clim`, run before `valid_range`, first file in 2005
`Tests/ExtData_Testing_Framework/test_cases/case40_variation_2/`

- `valid_range: "2004-01-01T00:00:00/2006-12-31T00:00:00"`
- Pre-generated files start at `case1.20050101.nc4`.
- Pass 2: run before `valid_range` (2002).
- Expected: **FAIL** — direction check: `on_disk_first (2005-01-01) > valid_range(1)
  (2004-01-01) + 1 day`.

## Key Design Decisions

1. `valid_range` is never mutated — it expresses user intent.
2. `on_disk_range` is the discovered actual range, stored on both `ExtDataFileStream`
   and `ExtDataAbstractFileHandler`.
3. The direction-aware endpoint check uses timestamp comparison with one-frequency
   tolerance — NOT year number comparison (the old approach allowed a single late-year
   file to pass incorrectly).
4. The gap scan for clim walks all expected file indices in `[Jan 1, Dec 31]` of the
   target year using the same abs/rel arithmetic as `refine_valid_range`.
5. Full-cycle check failure reports all missing files (not just first/last) for
   actionable diagnostics; gap scan failure reports the first missing file.
6. `VALIDATE_FILE_RANGES: false` in `extdata.yaml` disables all checks for users who
   want to skip the startup cost.

## Remaining Work

- Build and run `ctest` to verify all tests pass.
- Commit and open PR against `release/v2`.
