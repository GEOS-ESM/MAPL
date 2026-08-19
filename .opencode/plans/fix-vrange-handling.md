---
name: fix-vrange-handling
description: Context for the feature/bmauer/fixes-#4656 branch. Use when resuming work on the valid_range handling fix for ExtData2G, issue #4656, or the refine_valid_range / check_data_availability / VALIDATE_FILE_RANGES / PRINT_REQUIRED_FILES changes.
---

# Feature Branch: fixes-#4656 — valid_range handling in ExtData2G

## Branch

`feature/bmauer/fixes-#4656`, based on `release/v2`

## Repo

`/home/bmauer/models/mapl2g_fix_vrange_handling/MAPL`

## GitHub Issue

**#4656 — "Criteria for valid_range missing data error"**
https://github.com/GEOS-ESM/MAPL/issues/4656

## Design Philosophy

`valid_range` expresses user intent about when extrapolation should kick in and what
data should be available. It is **never mutated**. `on_disk_range` is the discovered
actual range on disk, set only when `VALIDATE_FILE_RANGES: true`.

### Three scenarios

1. **`extrap_outside = "none"`** — no extrapolation. Scan window = `run_range`.
   If `valid_range` is set and `run_range` extends outside it → config error (clear
   fail). On-disk data must cover the full `run_range` including interpolation brackets.
   Bracket files are determined by opening candidate files and reading internal
   timestamps via `get_bracket_indices`. `valid_range` not required.

2. **`extrap_outside = "persist_closest"`** — scan window = `valid_range` (required).
   If run overlaps `valid_range`: boundary check + bracket-aware gap scan over
   `[max(overlap_start - freq, valid_range(1)), min(overlap_end + freq, valid_range(2))]`.
   If run is outside `valid_range`: `found_any` is sufficient (clamp to endpoint).

3. **`extrap_outside = "clim"`** — scan window = `valid_range` (required).
   If run overlaps `valid_range`: same bracket-aware gap scan as `persist_closest`.
   If run is outside `valid_range`: full-cycle + direction-aware endpoint + gap-scan
   checks on the target year (`vr_yr1` if before, `vr_yr2` if after).

## Key Files

| File | Role |
|------|------|
| `gridcomps/ExtData2G/ExtDataFileStream.F90` | `refine_valid_range` (private, takes explicit `scan_range`); `check_data_availability` (3-scenario logic); `get_required_files_hconfig` (manifest entry builder); `compute_index_bounds` (private standalone subroutine); `get_bracket_indices` (private method, opens files to read internal timestamps) |
| `gridcomps/ExtData2G/ExtDataOldTypesCreator.F90` | `validate_file_ranges` + `print_required_files` fields; gated call site; manifest accumulation keyed by `base_name` |
| `gridcomps/ExtData2G/ExtDataGridCompNG.F90` | `VALIDATE_FILE_RANGES` + `PRINT_REQUIRED_FILES` yaml keys; `run_range` fetch; manifest write after `fillin_primary` loop |
| `gridcomps/ExtData2G/ExtDataAbstractFileHandler.F90` | `on_disk_range` field; `find_any_file` forward+backward scan |
| `gridcomps/ExtData2G/ExtDataSimpleFileHandler.F90` | `persist_closest` clamping uses `on_disk_range` when allocated |
| `gridcomps/ExtData2G/ExtDataClimFileHandler.F90` | Unchanged |

## YAML Flags (in `extdata.yaml`)

```yaml
VALIDATE_FILE_RANGES: true        # default false — enables all 3-scenario checks
PRINT_REQUIRED_FILES: needed.yaml # default '' (disabled) — writes manifest yaml
```

Both are independent. `PRINT_REQUIRED_FILES` works without `VALIDATE_FILE_RANGES`.

## VALIDATE_FILE_RANGES Details

Default: **false**. Set `true` to enable startup file existence checks.

- **`"none"`**: scans `run_range`; fails if `valid_range` set and run is outside it;
  checks `on_disk_first <= run_range(1) + freq` and `on_disk_last >= run_range(2) - freq`;
  then calls `get_bracket_indices` to find the exact bracket files needed by opening
  candidate files and reading their internal timestamps; fails if any required file is missing.
- **`"persist_closest"`**: scans `valid_range`; overlap coverage check; bracket-aware
  gap scan over `[max(overlap_start - freq, valid_range(1)), min(overlap_end + freq, valid_range(2))]`;
  outside = `found_any` sufficient.
- **`"clim"`**: scans `valid_range`; overlap coverage check; same bracket-aware gap scan
  as `persist_closest` over the overlap window; outside = full-cycle +
  direction endpoint + gap scan on target year.

`on_disk_range` is set on `ExtDataFileStream` (and copied to `ExtDataAbstractFileHandler`)
**only** when `VALIDATE_FILE_RANGES: true`.

## PRINT_REQUIRED_FILES Details

Writes a YAML manifest of all files the run needs. For `extrap_outside="none"`,
files are determined by opening candidate files and reading internal timestamps
(via `get_bracket_indices`). For other scenarios, derived from template + frequency +
ranges without filesystem probing. Uses `ESMF_HConfigCreate` / `ESMF_HConfigAdd`
/ `ESMF_HConfigFileSave`.

### Output format

```yaml
run_range:
  - 2004-01-01T00:00:00
  - 2004-12-31T00:00:00
required_files:
  VAR2D:
    template: case1.%y4.nc4
    extrap_outside: none
    files:
      - case1.2004.nc4
  AEROSOL:                       # multi-rule: value is a sequence
    - template: aero.%y4%m2.nc4
      extrap_outside: persist_closest
      files:
        - aero.200401.nc4
    - template: aero2.%y4%m2.nc4
      extrap_outside: clim
      files:
        - aero2.200401.nc4
```

### Enumeration scope per scenario

| extrap_outside | run inside valid_range | run outside valid_range |
|---|---|---|
| `none` | bracket files from `get_bracket_indices` (opens files) | bracket files from `get_bracket_indices` (opens files) |
| `persist_closest` | indices in overlap | single endpoint file |
| `clim` | indices in overlap | all indices in target year (`vr_yr1` or `vr_yr2`) |

### `get_bracket_indices` (private method on `ExtDataFileStream`)

Determines the tightest `[n_lo, n_hi]` of file-series indices required to bracket
`run_range` based on **internal timestamps read from file metadata** (not filename
arithmetic). This is necessary because file naming (e.g. `test.%y4%m2.nc4`) does not
encode the internal timestamp (e.g. the 15th of each month).

Algorithm:
1. Expand `compute_index_bounds(run_range)` by ±1 to form a conservative candidate set.
2. For each candidate that exists on disk, open it via `DataCollections` +
   `FileMetadataUtils%get_time_info` to read the internal time vector.
   Probe times computed using the same walking logic as `compute_index_bounds`
   (handles both fixed-interval and relative monthly/yearly frequencies).
3. `n_lo` = highest index `n` where `last_ts(n) <= run_range(1)` (lower bracket)
4. `n_hi` = lowest  index `n` where `first_ts(n) >= run_range(2)` (upper bracket)
5. Fallback to conservative `[n_cand_lo, n_cand_hi]` if no files found.

Used by both `check_data_availability` (gap scan for `"none"`) and
`get_required_files_hconfig` (`"none"` case). The latter required changing
`this` from `intent(in)` to `intent(inout)` to allow file I/O through the
collection cache.

`get_global_options` is called **before** `new_ExtDataOldTypesCreator` (to get
`self%active` for early-return), but `new_ExtDataOldTypesCreator` has `intent(out)`
which resets all `config_yaml` fields. The options are therefore stored in temporaries
(`validate_file_ranges_tmp`, `print_required_files_path`) and re-applied to `config_yaml`
immediately after `new_ExtDataOldTypesCreator` returns.

## find_any_file behaviour

When `on_disk_range` is allocated (validation was on): tries `on_disk_range(2)` first
(covers sparse datasets where only the endpoint file exists), then walks forward from
`on_disk_range(1)` for `MAX_TRIALS` steps.

When `on_disk_range` is not allocated (validation off): walks forward from
`valid_range(1)` for `MAX_TRIALS` steps, then if nothing found walks **backward** from
`valid_range(2)` for `MAX_TRIALS` steps. This handles the case where data is sparse
and only exists near the end of the valid range (e.g. case39: one file at Dec 2005,
`valid_range(1)` = Jan 2005).

## New Test Cases

### case39 — `persist_closest`, one file (Dec 2005), run outside `valid_range` in 2008
`Tests/ExtData_Testing_Framework/test_cases/case39/`

- Pass 1: generates one file — `case1.200512.nc4`.
- `valid_range: "2005-01-15T00:00:00/2005-12-15T00:00:00"`.
- Pass 2: run requests `20080615`. Persists the Dec 2005 value.
- Expected value: `14.0`.

### case40 — `clim`, `valid_range` = 2007 monthly data, run outside in 2008
`Tests/ExtData_Testing_Framework/test_cases/case40/`

- Pass 1: generates 2007 monthly files.
- `valid_range: "2007-01-01/2007-12-31"`.
- Pass 2: run `20080229 120000`. Year-wraps Feb 29 → Feb 2007.
- Expected value: `45.0 + (73.0-45.0)*0.5 = 59.0`.

### case40_variation_1 — `clim`, run before `valid_range`, first file late in vr_yr1
Expected: **FAIL** — direction check: `on_disk_first (2004-12-30) > valid_range(1) (2004-01-01) + 1 day`.

### case40_variation_2 — `clim`, run before `valid_range`, first file in 2005
Expected: **FAIL** — direction check: `on_disk_first (2005-01-01) > valid_range(1) (2004-01-01) + 1 day`.

## Remaining Work

- Commit and open PR against `release/v2`.

## Change Log

### 2026-08-17 — Fix infinite loop in `refine_valid_range` Phase 2 binary search

**Problem:** When `reff_time` is derived from `valid_range(1)` (lines 119–134 of the
`ExtDataFileStream` constructor) and `run_range(1) < reff_time`, the Phase 2 binary
search in `refine_valid_range` could hang forever.  The midpoint formula `(lo + hi) / 2`
uses Fortran's truncation-toward-zero integer division.  For negative odd sums (e.g.
`lo=-4, hi=-3` → `-7/2 = -3`), this rounds toward the more positive value, yielding
`n_mid = hi`.  A file exists at that index, so `hi = n_mid = hi` — no change — and the
loop never converges.

**Trigger (case23 step 3):** `fstream2` has `valid_range: "2019-12-31/2020-01-10"` so
`reff_time = 2019-12-31`.  With `extrap_outside="none"` the scan window is `run_range =
[2019-12-27, 2020-01-06]`, giving `n_lo = -4`.  The search converges normally to
`lo=-4, hi=-3` and then stalls.

**Fix (`ExtDataFileStream.F90:330`):** Changed
`n_mid = (lo + hi) / 2` → `n_mid = lo + (hi - lo) / 2`.
Since `hi > lo` guarantees `hi - lo > 0`, the division always floors, producing
`n_mid ∈ [lo, hi-1]` and ensuring `hi` strictly decreases on every "found" branch.

Also removed the stray `_HERE` debug print that was left at the old line 334.

**File:** `gridcomps/ExtData2G/ExtDataFileStream.F90`

### 2026-08-17 — Overlap gap scan for `persist_closest` and `clim`; bracket gap scan for all three scenarios

**Problem 1:** When `extrap_outside` is `"persist_closest"` or `"clim"` and the run
overlaps `valid_range`, `check_data_availability` only checked that the on-disk boundary
files were within one `frequency` of the overlap endpoints. Interior gaps (e.g. a missing
monthly file) were not detected; MAPL would silently interpolate across the gap at runtime.

**Problem 2:** All three scenarios failed to account for the interpolation bracket files
immediately outside the run/overlap window. MAPL interpolates between the two nearest
bracketing files, so a file just outside the window can still be required. Example:
monthly files timestamped on the 15th, run from June 25–29 with `extrap_outside="none"` —
the July file is the right interpolation bracket but was never probed.

**Fix:**

- **`"none"`**: added file-by-file gap scan over `[run_range(1) - freq, run_range(2) + freq]`,
  clamped to `valid_range` if set.
- **`"persist_closest"` overlap**: added bracket-aware gap scan inside `if (has_overlap)`,
  scan window `[max(overlap_start - freq, valid_range(1)), min(overlap_end + freq, valid_range(2))]`.
- **`"clim"` overlap**: identical bracket-aware gap scan, inserted before the `full_cycle`/
  `do_gap_scan` logic (which is unchanged).

All gap scans accumulate all missing filenames into `missing_list` and fail with the full
list. Uses the same inline `scan_interval_seconds` + `reff_time + n * frequency` index
arithmetic and `fill_grads_template` pattern as the existing `do_gap_scan` block.

**File:** `gridcomps/ExtData2G/ExtDataFileStream.F90`

### 2026-08-18 — Clip validation range to each rule's active window for multi-rule exports

**Problem:** For multi-rule exports (e.g. `E_1` with `starting: 1970-01-01` using a clim
collection, and `starting: 2020-01-01` using a non-clim collection), `check_data_availability`
was called with the full simulation `run_range` for every rule. This caused spurious failures
when `VALIDATE_FILE_RANGES: true` and the run started before the second rule's `starting:`
date — Rule 2's files don't exist before 2020-01-01, but MAPL was trying to validate them
against the full run.

**Root cause:** In `ExtDataGridCompNG`, `time_ranges` (the `num_rules+1` fence-post array
from `get_time_range`) is used to stamp `start_end_time` onto each `temp_item` **after**
`fillin_primary` returns. So `check_data_availability` inside `fillin_primary` always saw
the full `run_range`, not the narrowed per-rule window.

**Fix:** Added an optional `time_range(2)` argument to `fillin_primary`
(`ExtDataOldTypesCreator.F90`). When present, `effective_run` is computed as the
intersection of `run_range` with `[time_range(1), time_range(2))` using explicit
`if`-logic (ESMF overloads `>` / `<` on `ESMF_Time` but not `max`/`min`). Both the
`check_data_availability` and manifest blocks are wrapped in
`if (effective_run(1) < effective_run(2))` so a rule entirely outside the run is silently
skipped. When `time_range` is absent (single-rule call), `effective_run = run_range`.

In `ExtDataGridCompNG.F90`, the multi-rule loop now passes
`time_range=[time_ranges(j), time_ranges(j+1)]` to `fillin_primary`. The single-rule
call site is unchanged.

Note: `valid_range` propagation to the file handler did **not** require a separate fix in
mapl2g — `valid_range` already flows from the `ExtDataFileStream` (dataset) object into
the handler via `handler%initialize(dataset, ...)`, so the gap-scan clamping in
`check_data_availability` already worked correctly once the right `effective_run` was
passed in.

**Files:** `gridcomps/ExtData2G/ExtDataOldTypesCreator.F90`,
`gridcomps/ExtData2G/ExtDataGridCompNG.F90`

### 2026-08-19 — Robust bracket-file determination via internal timestamps (`get_bracket_indices`)

**Problem:** The `"none"` gap scan in `check_data_availability` and the `"none"` case
in `get_required_files_hconfig` both used filename-arithmetic (`± frequency`) to
determine which files bracket `run_range`. This is fundamentally wrong when files have
internal timestamps that don't align with the filename grid (e.g. monthly files named
`test.%y4%m2.nc4` but internally timestamped on the 15th). The arithmetic would either:
- Miss needed files (e.g. only list `test.200401.nc4` when `test.200402.nc4` is also
  needed to bracket Feb 2), or
- Include files that aren't needed (e.g. `test.200403.nc4` for a run ending Feb 6), or
- Spuriously require non-existent files (e.g. `test.200312.nc4` when the run starts
  Jan 25 and `reff_time` is Jan 1).

**Note:** The single-rule out-of-bounds crash (`time_range(0)` subscript) from the
develop/mapl3g branch does not apply to mapl2g — `fillin_primary` uses a different
call pattern that does not index `time_range` unconditionally.

**Fix:** Replaced filename-arithmetic bracket finding with `get_bracket_indices`, a new
private method on `ExtDataFileStream` that opens each candidate file and reads its
internal time vector via `FileMetadataUtils%get_time_info`. Probe times are computed
using the same walking logic as `compute_index_bounds` (handles both fixed-interval and
relative monthly/yearly frequencies). The bracket is determined as:
- `n_lo` = highest index whose `last_ts <= run_range(1)` (lower bracket file)
- `n_hi` = lowest  index whose `first_ts >= run_range(2)` (upper bracket file)

`get_required_files_hconfig`'s `this` was changed from `intent(in)` to `intent(inout)`
to allow file I/O through the collection cache.

**File:** `gridcomps/ExtData2G/ExtDataFileStream.F90`
