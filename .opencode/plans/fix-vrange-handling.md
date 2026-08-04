---
name: fix-vrange-handling
description: Context for the feature/bmauer/fixes-#4656 branch. Use when resuming work on the valid_range handling fix for ExtData2G, issue #4656, or the refine_valid_range / check_data_availability changes.
---

# Feature Branch: fixes-#4656 — valid_range handling in ExtData2G

## Branch

`feature/bmauer/fixes-#4656`, based on `release/v2`

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
5. **Extrapolation sufficiency check** (run outside `valid_range`):
   - `persist_closest`: `found_any` is sufficient.
   - `clim`: on-disk data must span a full annual cycle:
     `(yr_last > yr_first) .or. (yr_last == yr_first .and. mm_first == 1 .and. mm_last == 12)`

### Changes to `gridcomps/ExtData2G/ExtDataGridCompNG.F90`
- Moved `ESMF_ClockGet(clock, currtime=run_range(1), stoptime=run_range(2))` to
  **before** the `fillin_primary` loop.
- Passes `run_range` to both `fillin_primary` call sites.

### Changes to `gridcomps/ExtData2G/ExtDataOldTypesCreator.F90`
- Added `type(ESMF_Time), intent(in) :: run_range(2)` to `fillin_primary`.
- After `detect_metadata`, single clean validation call:

```fortran
if (user_set_range .and. index(dataset%file_template,'%') /= 0 .and. &
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
Copies `on_disk_range` from `file_series` alongside `valid_range`:
```fortran
if (allocated(file_series%on_disk_range)) then
    allocate(this%on_disk_range, source=file_series%on_disk_range)
end if
```

#### `find_any_file` probe loop
Prefers `on_disk_range(1)` over `valid_range(1)` as the starting point —
guaranteed to hit the actual first file on the first probe:
```fortran
if (allocated(this%on_disk_range)) then
   useable_time = this%on_disk_range(1)
else if (allocated(this%valid_range)) then
   useable_time = this%valid_range(1)
end if
```

Note: the user also fixed a pre-existing bug in `find_any_file` where the loop
advanced by one frequency before probing, meaning `valid_range(1)` itself was
never tried.

### Changes to `gridcomps/ExtData2G/ExtDataSimpleFileHandler.F90`

`persist_closest` clamping uses `on_disk_range` when allocated, falling back to
`valid_range` otherwise. This ensures clamping targets an actual file timestamp
rather than a user-specified endpoint that may not have a file:

```fortran
if (allocated(this%on_disk_range)) then
   ! clamp using on_disk_range(1) / on_disk_range(2)
else
   ! clamp using valid_range(1) / valid_range(2)
end if
```

### `gridcomps/ExtData2G/ExtDataClimFileHandler.F90`
- Lines 63–66: **original full-time `source_time` assertions** — restored and unchanged.
- Lines 57–58 and 85–93: continue using `valid_range` for year extraction and
  year-clamping. `on_disk_range` is NOT used by the clim handler — the user's
  intent for which years to clamp to is expressed in `valid_range`.

## Key Files

| File | Role |
|------|------|
| `gridcomps/ExtData2G/ExtDataFileStream.F90` | `refine_valid_range` (private, diagnostic) + `check_data_availability` (public, validates + sets `on_disk_range`) |
| `gridcomps/ExtData2G/ExtDataOldTypesCreator.F90` | Single `check_data_availability` call site |
| `gridcomps/ExtData2G/ExtDataGridCompNG.F90` | `run_range` fetch and `fillin_primary` call sites |
| `gridcomps/ExtData2G/ExtDataAbstractFileHandler.F90` | `on_disk_range` field, copy in `initialize`, use in `find_any_file` |
| `gridcomps/ExtData2G/ExtDataSimpleFileHandler.F90` | `persist_closest` clamping uses `on_disk_range` |
| `gridcomps/ExtData2G/ExtDataClimFileHandler.F90` | Unchanged from original (uses `valid_range` for year logic) |

## New Test Cases

### case39 — `persist_closest`, one file, run outside `valid_range`
`Tests/ExtData_Testing_Framework/test_cases/case39/`

- Pass 1: generates **one file only** — `case1.200501.nc4` (Jan 15 2005).
  `FILL_DEF: VAR2D time`, `REF_TIME: 20050101 000000`.
- `valid_range`: set to match the single file (exact timestamps).
- Pass 2: run requests `20080615` — far outside `valid_range`.
  `check_data_availability` passes (`found_any=.true.` is sufficient for
  `persist_closest` outside `valid_range`).
  At runtime: `on_disk_range = [2005-01-15, 2005-01-15]`, clamps to `on_disk_range(2)`,
  finds Jan 15 file, persists its value.
  Expected value: `14.0` (Jan 15 = day 14 since `20050101`).
- **Note:** user also modified `find_any_file` to probe `valid_range(1)` before
  advancing (pre-existing bug where the loop skipped the first candidate).

### case40 — `clim`, `valid_range` matches 2007 on-disk data, run outside in 2008
`Tests/ExtData_Testing_Framework/test_cases/case40/`

- Pass 1: generates 2007 monthly files. `FILL_DEF: VAR2D time`,
  `REF_TIME: 20070101 000000`.
- `valid_range: "2007-01-01/2007-12-31"`. No `source_time`.
- Pass 2: run requests `20080229 120000` (Feb 29 2008, outside `valid_range`).
  `check_data_availability` clim check: Jan–Dec 2007 = full annual cycle → passes.
  Year-wraps Feb 29 2008 → Feb 2007; interpolates between Feb 15 and Mar 15.
  Expected value: `45.0+(73.0-45.0)*0.5` (= 59.0).
- `HISTORY1.rc` uses `%y4%m2.nc4` (not `case1.2007%m2.nc4`).

## Remaining Work

- Build and run `ctest` to verify all tests pass with latest changes
  (`on_disk_range` propagation + `SimpleFileHandler` clamping fix).
- Commit and open PR against `release/v2`.
