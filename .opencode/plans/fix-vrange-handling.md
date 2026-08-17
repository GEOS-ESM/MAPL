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
   On-disk data must cover the full `run_range` including interpolation brackets:
   file-by-file gap scan over `[run_range(1) - freq, run_range(2) + freq]` (clamped
   to `valid_range` if set). `valid_range` not required.

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
| `gridcomps/extdata/AbstractDataSetFileSelector.F90` | `refine_valid_range` takes explicit `scan_range`; `check_data_availability` 3-scenario restructure; new `compute_index_bounds` private helper; new `get_required_files_hconfig` public method; `find_any_file` backward scan; bracket-aware gap scans added to all three cases |
| `gridcomps/extdata/PrimaryExport.F90` | Updated `check_data_availability` gate (enables `"none"` scenario); manifest accumulation keyed by base export name; new optional args `validate_file_ranges` + `required_files_hconfig` |
| `gridcomps/extdata/ExtDataConfig.F90` | `make_PrimaryExport` threads optional `validate_file_ranges` + `required_files_hconfig` to `PrimaryExport` constructor |
| `gridcomps/extdata/ExtDataGridComp.F90` | `modify_advertise` reads `VALIDATE_FILE_RANGES` + `PRINT_REQUIRED_FILES` from hconfig; threads flags through `make_PrimaryExport`; writes manifest YAML after export loop |

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
  then file-by-file gap scan over `[run_range(1) - freq, run_range(2) + freq]` (clamped
  to `valid_range` if set) to catch missing interpolation bracket files.
- **`"persist_closest"`**: scans `valid_range`; overlap coverage check; file-by-file gap
  scan over `[max(overlap_start - freq, valid_range(1)), min(overlap_end + freq, valid_range(2))]`
  to catch missing interpolation bracket files; outside = `found_any` sufficient.
- **`"clim"`**: scans `valid_range`; overlap coverage check; same bracket-aware gap scan
  as `persist_closest` over the overlap window; outside = full-cycle +
  direction endpoint + gap scan on target year.

`on_disk_range` is set on `AbstractDataSetFileSelector` **only** when
`VALIDATE_FILE_RANGES: true`.

## PRINT_REQUIRED_FILES Details

Writes a YAML manifest of all files the run needs, derived purely from template +
frequency + ranges — no filesystem probing. Uses `ESMF_HConfigCreate` /
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
| `none` | all indices in `run_range` | all indices in `run_range` |
| `persist_closest` | indices in overlap | single endpoint file |
| `clim` | indices in overlap | all indices in target year (`vr_yr1` or `vr_yr2`) |

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

- Build and run tests to verify all changes compile and pass.
- Commit and open PR against `develop`.

## Change Log

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
