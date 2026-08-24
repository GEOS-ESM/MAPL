---
name: fix-vrange-handling
description: Context for the feature/bmauer/fixes-#4656-develop branch. Use when resuming work on the valid_range handling fix for ExtData (develop/mapl3g), issue #4656, or the refine_valid_range / check_data_availability / VALIDATE_FILE_RANGES / PRINT_REQUIRED_FILES changes.
---

# Feature Branch: fixes-#4656-develop — valid_range handling in ExtData (develop)

## Branch

`feature/bmauer/fixes-#4656-develop`, based on `develop`

## Repo

`/home/bmauer/models/mapl3g_fix_vrange_handling/MAPL`

## Related Branch (mapl2g / release/v2)

The same feature was implemented in parallel on `release/v2` at:
`/home/bmauer/models/mapl2g_fix_vrange_handling/MAPL`
Plan file: `.opencode/plans/fix-vrange-handling.md` in that repo.

## GitHub Issue

**#4656 — "Criteria for valid_range missing data error"**
https://github.com/GEOS-ESM/MAPL/issues/4656

## Architecture (develop vs mapl2g)

The develop branch has a cleaner, more object-oriented ExtData implementation.
The mapl2g equivalents map as follows:

| mapl2g (release/v2) | develop |
|---|---|
| `gridcomps/ExtData2G/ExtDataFileStream.F90` | `gridcomps/extdata/AbstractDataSetFileSelector.F90` |
| `gridcomps/ExtData2G/ExtDataAbstractFileHandler.F90` | `gridcomps/extdata/AbstractDataSetFileSelector.F90` |
| `gridcomps/ExtData2G/ExtDataSimpleFileHandler.F90` | `gridcomps/extdata/NonClimDataSetFileSelector.F90` |
| `gridcomps/ExtData2G/ExtDataClimFileHandler.F90` | `gridcomps/extdata/ClimDataSetFileSelector.F90` |
| `gridcomps/ExtData2G/ExtDataOldTypesCreator.F90` | `gridcomps/extdata/PrimaryExport.F90` + `ExtDataConfig.F90` |
| `gridcomps/ExtData2G/ExtDataGridCompNG.F90` | `gridcomps/extdata/ExtDataGridComp.F90` |

## Design Philosophy

`valid_range` expresses user intent about when extrapolation should kick in and
what data should be available. It is **never mutated**. `on_disk_range` is the
discovered actual range on disk, set only when `VALIDATE_FILE_RANGES: true`.

### Three scenarios

1. **`extrap_outside = "none"`** — no extrapolation. Scan window = `run_range`.
   If `valid_range` is set and `run_range` extends outside it → config error.
   On-disk data must cover the full `run_range` including interpolation brackets.
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

## Key Files Changed

| File | Change |
|------|--------|
| `gridcomps/extdata/AbstractDataSetFileSelector.F90` | `refine_valid_range` takes explicit `scan_range`; `check_data_availability` 3-scenario restructure; new `compute_index_bounds` private helper; new `get_bracket_indices` private helper (opens files to read internal timestamps); new `get_required_files_hconfig` public method; `find_any_file` backward scan; bracket-aware gap scans using `get_bracket_indices` for `"none"` case |
| `gridcomps/extdata/PrimaryExport.F90` | Updated `check_data_availability` gate (enables `"none"` scenario); out-of-bounds guard for single-rule exports (`size(time_range)==0`); manifest accumulation keyed by base export name; new optional args `validate_file_ranges` + `required_files_hconfig` |
| `gridcomps/extdata/ExtDataConfig.F90` | `make_PrimaryExport` threads optional `validate_file_ranges` + `required_files_hconfig` to `PrimaryExport` constructor |
| `gridcomps/extdata/ExtDataGridComp.F90` | `modify_advertise` reads `validate_file_ranges` + `estimate_required_files` from hconfig; threads flags through `make_PrimaryExport`; writes manifest YAML after export loop |

## YAML Flags (in `extdata.yaml`)

```yaml
validate_file_ranges: true        # default false — enables all 3-scenario checks
estimate_required_files: needed.yaml # default '' (disabled) — writes manifest yaml
```

Both are independent. `estimate_required_files` works without `validate_file_ranges`.

## VALIDATE_FILE_RANGES Details

Default: **false**. Set `validate_file_ranges: true` to enable startup file existence checks.

- **`"none"`**: scans `run_range`; fails if `valid_range` set and run is outside it;
  checks `on_disk_first <= run_range(1) + freq` and `on_disk_last >= run_range(2) - freq`;
  then calls `get_bracket_indices` to find the exact bracket files needed by opening
  candidate files and reading their internal timestamps; fails if any required file is missing.
- **`"persist_closest"`**: scans `valid_range`; overlap coverage check; file-by-file gap
  scan over `[max(overlap_start - freq, valid_range(1)), min(overlap_end + freq, valid_range(2))]`
  to catch missing interpolation bracket files; outside = `found_any` sufficient.
- **`"clim"`**: scans `valid_range`; overlap coverage check; same bracket-aware gap scan
  as `persist_closest` over the overlap window; outside = full-cycle +
  direction endpoint + gap scan on target year.

`on_disk_range` is set on `AbstractDataSetFileSelector` **only** when
`validate_file_ranges: true`.

## estimate_required_files Details

Writes a YAML manifest of all files the run **might** need, as a conservative
over-approximation suitable for pre-run file staging. No filesystem access is
performed — all enumeration is pure filename-arithmetic. Because internal file
timestamps are not known without opening files (e.g. daily files timestamped at
12Z), the manifest may list files that turn out not to be needed at runtime.

All `extrap_outside` scenarios are included. Uses `ESMF_HConfigCreate` /
`ESMF_HConfigAdd` / `ESMF_HConfigFileSave`.

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
      - case1.2003.nc4
      - case1.2004.nc4
      - case1.2005.nc4
  AEROSOL:                       # multi-rule: value is a sequence
    - template: aero.%y4%m2.nc4
      extrap_outside: persist_closest
      files:
        - aero.200312.nc4
        - aero.200401.nc4
        - aero.200402.nc4
    - template: aero2.%y4%m2.nc4
      extrap_outside: clim
      files:
        - aero2.200312.nc4
        - aero2.200401.nc4
        - aero2.200402.nc4
```

### Enumeration scope per scenario

| extrap_outside | run inside valid_range | run outside valid_range |
|---|---|---|
| `none` | `compute_index_bounds(run_range) ±1` (arithmetic only) | same |
| `persist_closest` | `compute_index_bounds(overlap) ±1`, clamped to valid_range | single endpoint file, no expansion |
| `clim` | `compute_index_bounds(overlap) ±1`, clamped to valid_range | all indices in target year (`vr_yr1` or `vr_yr2`), no expansion |

The ±1 expansion covers the case where internal timestamps are offset from the
filename grid (e.g. a run starting at 9Z with 12Z-timestamped daily files needs
the previous day's file as the lower bracket). No expansion is applied for
outside-valid_range cases because MAPL clamps/wraps to a fixed endpoint and no
file outside valid_range is ever needed.

## Key Implementation Notes

### `find_any_file` behaviour

When `on_disk_range` is allocated (validation was on):
- Probes `on_disk_range(2)` immediately (covers sparse datasets where only the endpoint file exists)
- Then walks forward from `on_disk_range(1)` for `MAX_TRIALS` steps

When `on_disk_range` is not allocated (validation off):
- Walks forward from `ref_time` for `MAX_TRIALS` steps
- If that fails and `valid_range` is set, walks **backward** from `valid_range(2)` for `MAX_TRIALS` steps

### `compute_index_bounds` (private, on `AbstractDataSetFileSelector`)

Replaces the duplicated abs/rel index arithmetic that appeared separately in
`refine_valid_range`, `check_data_availability` (gap scan), and `get_required_files_hconfig`.
Uses `compute_time_at_index` for the relative-interval case.

### `get_bracket_indices` (private, on `AbstractDataSetFileSelector`)

Determines the tightest `[n_lo, n_hi]` of file-series indices required to bracket
`run_range` based on **internal timestamps read from file metadata** (not filename
arithmetic). This is necessary because file naming (e.g. `test.%y4%m2.nc4`) does not
encode the internal timestamp (e.g. the 15th of each month).

Algorithm:
1. Expand `compute_index_bounds(run_range)` by ±1 to form a conservative candidate set.
2. For each candidate that exists on disk, open it via `mapl_DataCollections` +
   `FileMetadataUtils%get_time_info` to read the internal time vector.
3. `n_lo` = highest index `n` where `last_ts(n) <= run_range(1)` (lower bracket)
4. `n_hi` = lowest  index `n` where `first_ts(n) >= run_range(2)` (upper bracket)
5. Fallback to conservative `[n_cand_lo, n_cand_hi]` if no files found.

Used by `check_data_availability` (gap scan for `"none"`) only.
`get_required_files_hconfig` no longer calls `get_bracket_indices` — it uses
pure `compute_index_bounds` arithmetic with ±1 expansion (no file I/O).
`this` is `intent(in)` on `get_required_files_hconfig`.

### `rule_sep` in `PrimaryExport`

A local `character(len=1), parameter :: rule_sep = "+"` is defined in `PrimaryExport.F90`
to avoid a circular dependency with `ExtDataConfig_mod` (which defines the same constant
as `public`). The manifest accumulation uses `rule_sep` to strip the rule suffix from
`export_var` to recover the base export name as the map key.

### No `intent(out)` reset issue

Unlike the mapl2g branch (where `new_ExtDataOldTypesCreator` had `intent(out)` and
reset the object, requiring options to be stored in temporaries and re-applied), the
develop branch passes `validate_file_ranges` and `required_files_hconfig` as arguments
directly into `make_PrimaryExport` → `PrimaryExport` constructor. No workaround needed.

## Remaining Work

- Commit and open PR against `develop`.
- Write test cases for `estimate_required_files` (cases removed; starting fresh).

## Change Log

### 2026-08-17 — Fix infinite loop in `refine_valid_range` Phase 2 binary search

**Problem:** When `reff_time` is derived from `valid_range(1)` and `run_range(1) < reff_time`,
the Phase 2 binary search in `refine_valid_range` could hang forever. The midpoint formula
`(lo + hi) / 2` uses Fortran's truncation-toward-zero integer division. For negative odd sums
(e.g. `lo=-4, hi=-3` → `-7/2 = -3`), this rounds toward the more positive value, yielding
`n_mid = hi`. A file exists at that index, so `hi = n_mid = hi` — no change — and the loop
never converges.

**Fix (`AbstractDataSetFileSelector.F90:381`):** Changed
`n_mid = (lo + hi) / 2` → `n_mid = lo + (hi - lo) / 2`.
Since `hi > lo` guarantees `hi - lo > 0`, the division always floors, producing
`n_mid ∈ [lo, hi-1]` and ensuring `hi` strictly decreases on every "found" branch.

**File:** `gridcomps/extdata/AbstractDataSetFileSelector.F90`

### 2026-08-17 — Overlap gap scan for `persist_closest` and `clim`

**Problem:** When `extrap_outside` is `"persist_closest"` or `"clim"` and the run
overlaps `valid_range`, `check_data_availability` only checked that the on-disk
boundary files were within one `file_frequency` of the overlap endpoints. Interior
gaps (e.g. a missing monthly file) were not detected; MAPL would silently interpolate
across the gap at runtime.

**Fix:** After the boundary checks inside `if (has_overlap)` in both the
`persist_closest` and `clim` cases of `check_data_availability`, added a
file-by-file gap scan over the full overlap window using `compute_index_bounds` +
`compute_time_at_index` + `mapl_fill_grads_template` + `inquire`. All missing files
are accumulated into `missing_list`; if any are found the run fails with the full list.

**File:** `gridcomps/extdata/AbstractDataSetFileSelector.F90`

### 2026-08-17 — Bracket-aware gap scan for all three scenarios

**Problem:** The gap scans added above scanned only `[overlap_start, overlap_end]` for
`persist_closest`/`clim`, and `"none"` had no gap scan at all. MAPL interpolates between
the two nearest bracketing files, so a file just outside the run/overlap window can still
be required. Example: monthly files timestamped on the 15th, run from June 25–29 with
`extrap_outside="none"` — the July file is the right interpolation bracket but was never
probed.

**Fix:** Extended all three gap scans to include one `file_frequency` on each side:

- **`"none"`**: new gap scan over `[run_range(1) - freq, run_range(2) + freq]`, clamped
  to `valid_range` if set.
- **`"persist_closest"` overlap**: scan window widened from `[overlap_start, overlap_end]`
  to `[max(overlap_start - freq, valid_range(1)), min(overlap_end + freq, valid_range(2))]`.
- **`"clim"` overlap**: same widening as `persist_closest`.

**File:** `gridcomps/extdata/AbstractDataSetFileSelector.F90`

### 2026-08-18 — Clip validation range to each rule's active window for multi-rule exports

**Problem:** For multi-rule exports (e.g. `E_1` with `starting: 1970-01-01` using a clim
collection, and `starting: 2020-01-01` using a non-clim collection), `check_data_availability`
was called with the full simulation `run_range` for every rule. This caused spurious failures
when `VALIDATE_FILE_RANGES: true` and the run started before the second rule's `starting:`
date — Rule 2's files don't exist before 2020-01-01, but MAPL was trying to validate them
against the full run from 2019-12-27.

**Fix (`PrimaryExport.F90`):** Before calling `check_data_availability` (and
`get_required_files_hconfig`), compute `effective_run` as the intersection of `run_range`
with this rule's active window `[time_range(1), time_range(2))`:

```fortran
if (run_range(1) > time_range(1)) then
   effective_run(1) = run_range(1)
else
   effective_run(1) = time_range(1)
end if
if (run_range(2) < time_range(2)) then
   effective_run(2) = run_range(2)
else
   effective_run(2) = time_range(2)
end if
```

If `effective_run(1) >= effective_run(2)` the rule is entirely outside the run and both
blocks are skipped (no error, no manifest entry). Single-rule exports are unaffected:
when `time_range` is size 0 (no multi-rule), `effective_run` falls back to `run_range`.

Note: ESMF overloads `>` / `<` on `ESMF_Time` but not `max`/`min`, so explicit
`if`-logic is required instead of intrinsic `max`/`min`.

**File:** `gridcomps/extdata/PrimaryExport.F90`

### 2026-08-18 — Pass `valid_range` to `NonClimDataSetFileSelector` for `extrap_outside="none"`

**Problem:** The bracket-aware gap scan in `check_data_availability` (scenario `"none"`)
clamps `t_scan_lo = run_range(1) - freq` to `valid_range(1)` when `valid_range` is
allocated on the selector. However, `valid_range` was only wired into
`NonClimDataSetFileSelector` for the `"persist_closest"` case — not for `"none"`. So for
a `"none"` collection whose `valid_range` starts exactly at `run_range(1)` (e.g.
`valid_range: 2020-01-01/2020-01-10`, rule active from `2020-01-01`), the scan would
probe `2019-12-31` (one freq below the start) and report it missing even though data
starts on `2020-01-01` as intended.

**Fix (`PrimaryExport.F90`):** When constructing `NonClimDataSetFileSelector`, pass
`valid_range=collection%valid_range` whenever the collection has one:

```fortran
if (collection%is_valid_range_allocated()) then
   non_clim_file_selector = NonClimDataSetFileSelector(..., valid_range=collection%valid_range, ...)
else
   non_clim_file_selector = NonClimDataSetFileSelector(...)
end if
```

This ensures the gap-scan clamping logic in `AbstractDataSetFileSelector` receives the
`valid_range` it was already designed to use.

**File:** `gridcomps/extdata/PrimaryExport.F90`

### 2026-08-19 — Out-of-bounds fix for single-rule exports with `VALIDATE_FILE_RANGES`

**Problem:** `get_time_range` in `ExtDataConfig.F90` returns a zero-length `time_range`
for single-rule exports (no `rule_sep` in the key). The `effective_run` clipping block
in `PrimaryExport.F90` then accessed `time_range(1)` unconditionally, causing:
`forrtl: severe (408): Subscript #1 of the array TIME_RANGE has value 1 which is greater than the upper bound of 0`

**Fix (`PrimaryExport.F90`):** Wrapped the `effective_run` clipping in
`if (size(time_range) == 2) then ... else; effective_run = run_range; end if`.

**File:** `gridcomps/extdata/PrimaryExport.F90`

### 2026-08-19 — Robust bracket-file determination via internal timestamps (`get_bracket_indices`)

**Problem:** The `"none"` gap scan in `check_data_availability` and the `"none"` case
in `get_required_files_hconfig` both used filename-arithmetic (`± file_frequency`) to
determine which files bracket `run_range`. This is fundamentally wrong when files have
internal timestamps that don't align with the filename grid (e.g. monthly files named
`test.%y4%m2.nc4` but internally timestamped on the 15th). The arithmetic would either:
- Miss needed files (e.g. only list `test.200401.nc4` when `test.200402.nc4` is also
  needed to bracket Feb 2), or
- Include files that aren't needed (e.g. `test.200403.nc4` for a run ending Feb 6), or
- Spuriously require non-existent files (e.g. `test.200312.nc4` when the run starts
  Jan 25 and `reff_time` is Jan 1).

**Fix:** Replaced filename-arithmetic bracket finding with `get_bracket_indices`, a new
private subroutine that opens each candidate file and reads its internal time vector via
`FileMetadataUtils%get_time_info`. The bracket is determined as:
- `n_lo` = highest index whose `last_ts <= run_range(1)` (lower bracket file)
- `n_hi` = lowest  index whose `first_ts >= run_range(2)` (upper bracket file)

`get_required_files_hconfig`'s `this` was changed from `intent(in)` to `intent(inout)`
to allow the file I/O.

**Files:** `gridcomps/extdata/AbstractDataSetFileSelector.F90`

### 2026-08-19 — Single-file straddle shortcut in `get_bracket_indices`

**Problem:** For files with multiple internal timestamps per file (e.g. one file per year
with 12 monthly timestamps), `get_bracket_indices` would include the files immediately
before and after the run even when a single file contains timestamps bracketing the entire
run window. Example: `%y4` template, 12-timestamp yearly files, run from
`2000-04-14T21:00:00` to `2000-04-15T21:00:00` — the 2000 file alone brackets the run,
but the algorithm returned indices for 1999, 2000, and 2001.

**Root cause:** The existing lower/upper bracket logic checked whether `last_ts(n) <=
run_range(1)` (file entirely before run) or `first_ts(n) >= run_range(2)` (file entirely
after run). A file that *straddles* the run (has timestamps on both sides) satisfies
neither condition and was never recognized as self-sufficient.

**Fix (`AbstractDataSetFileSelector.F90:505`):** Added a straddle check at the top of
the per-candidate loop body, before the existing lower/upper bracket tests. For each
candidate file whose `time_series` is read, an explicit loop checks whether the file
contains at least one timestamp `<= run_range(1)` (`has_lo_ts`) and at least one
timestamp `>= run_range(2)` (`has_hi_ts`). If both are true, the file alone provides
both interpolation brackets: `n_lo = n_hi = n` and the subroutine returns immediately.
The existing lower/upper bracket logic is unchanged and handles all non-straddling cases.

**Test case:** `tests/MAPL3G_Component_Testing_Framework/test_cases/case45` (removed; starting fresh)

**File:** `gridcomps/extdata/AbstractDataSetFileSelector.F90`

### 2026-08-21 — Rename YAML flags to lowercase; decouple `estimate_required_files` from `validate_file_ranges`

**Problem:** The two YAML flags were named `VALIDATE_FILE_RANGES` and `PRINT_REQUIRED_FILES`
(all-caps). The `PRINT_REQUIRED_FILES` feature was fundamentally mis-designed: it called
`get_bracket_indices` which opens files on disk to read internal timestamps — but the entire
purpose of this flag is pre-run planning when files may not yet exist. Additionally, the
manifest gate excluded `"persist_closest"` and `"clim"` scenarios entirely.

**Fix:**
- Renamed YAML keys to `validate_file_ranges` and `estimate_required_files` (lowercase).
- `get_required_files_hconfig` for `"none"` now uses `compute_index_bounds(run_range) ±1`
  (pure arithmetic, no file I/O). `this` reverted to `intent(in)`.
- Manifest gate widened to all `extrap_outside` scenarios (not just `"none"`).
- `"persist_closest"` and `"clim"` overlap cases: `compute_index_bounds(overlap) ±1`,
  clamped to `valid_range` bounds — same bracket logic as `"none"`.
- `"persist_closest"` outside: single endpoint file, no expansion.
- `"clim"` outside: full target year, no expansion.
- Removed test cases case44 and case45 (to be rewritten).

**Files:** `gridcomps/extdata/AbstractDataSetFileSelector.F90`,
`gridcomps/extdata/PrimaryExport.F90`, `gridcomps/extdata/ExtDataGridComp.F90`
