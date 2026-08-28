# ExtData Dry Run Feature — Session Plan

## What was done

### 1. Python dry run script (simple, Tier 1 only)
**File:** `gridcomps/extdata/extdata_dryrun.py`

A standalone Python script that estimates files needed by an ExtData component
without opening any files. Given an extdata YAML config and a run range, it
expands file templates over the maximalist time range `[run_start - freq,
run_end + freq]`, adjusted for `valid_range` clamping, climatological wrapping
(`extrap_outside: clim`), and `persist_closest` behaviour.

**Usage:**
```
extdata_dryrun.py --config extdata.yaml \
                  --run_start 2020-01-01T00:00:00 \
                  --run_end   2020-12-31T18:00:00 \
                  [--output   dry_run_files.yaml]
```

**Output YAML format:**
```yaml
run_start: '2020-01-01T00:00:00'
run_end:   '2020-12-31T18:00:00'
files:
  - /path/to/file.nc4
  ...
```

### 2. Python dry run + check script (Tiers 1/2/3)
**File:** `gridcomps/extdata/extdata_dryrun_check.py`

Fully self-contained (does not import from the simple script). Adds:

- **Tier 2 (`--check`):** filesystem existence check — splits estimated files
  into present/missing, writes `--missing_output` YAML.
- **Tier 3 (`--narrow`):** opens bracketing files with `netCDF4` to read time
  axes and confirm whether buffer files are actually needed. Implies `--check`.
- **`--verify_files_read PATH`:** reads a `files_read.yaml` produced at runtime
  by ExtData (via `log_files_read`) and verifies two things:
  1. Every file in `files_read` appears in the estimated set (coverage check —
     no false negatives).
  2. No predicted files are missing from disk (`missing_files` list is empty).
  Exits non-zero with diagnostics if either check fails. Requires `--check` (or
  `--narrow`) and `--missing_output`.

**Usage:**
```
# Tier 1 only
extdata_dryrun_check.py --config extdata.yaml \
    --run_start 2020-01-01T00:00:00 --run_end 2020-12-31T18:00:00 \
    --output estimated.yaml

# Tier 2
extdata_dryrun_check.py --config extdata.yaml \
    --run_start 2020-01-01T00:00:00 --run_end 2020-12-31T18:00:00 \
    --output estimated.yaml --check --missing_output missing.yaml

# Tier 3
extdata_dryrun_check.py --config extdata.yaml \
    --run_start 2020-01-01T00:00:00 --run_end 2020-12-31T18:00:00 \
    --output narrowed.yaml --narrow --missing_output missing.yaml

# Tier 3 + runtime verification (used by CTest)
extdata_dryrun_check.py --config extdata.yaml \
    --run_start 2020-01-01T00:00:00 --run_end 2020-12-31T18:00:00 \
    --output narrowed.yaml --narrow --missing_output missing.yaml \
    --verify_files_read files_read.yaml
```

**`--missing_output` YAML format:**
```yaml
run_start: '2020-01-01T00:00:00'
run_end:   '2020-12-31T18:00:00'
missing_files:
  - /path/to/missing.nc4
```

### 3. CMakeLists.txt
**File:** `gridcomps/extdata/CMakeLists.txt`

`extdata_dryrun_check.py` is installed to `bin/` and also copied into the build
tree so CTest can find it alongside `GEOS.x`:
```cmake
install(
  PROGRAMS extdata_dryrun_check.py
  DESTINATION bin)

# Also copy into the build tree so CTest can find the script alongside GEOS.x
# (MY_BINARY_DIR in run_comp_tester.cmake points to ${CMAKE_BINARY_DIR}/bin).
file(COPY extdata_dryrun_check.py DESTINATION ${CMAKE_BINARY_DIR}/bin)
```

Note: `file(COPY ...)` runs at cmake configure time. Changes to the script
require a `cmake ..` re-run to propagate into the build tree.

### 4. CTest integration
**Files:**
- `tests/MAPL3G_Component_Testing_Framework/run_comp_tester.cmake` — modified
- `tests/MAPL3G_Component_Testing_Framework/test_cases/case02/dryrun.rc` — new
- `tests/MAPL3G_Component_Testing_Framework/test_cases/case11/dryrun.rc` — new
- `tests/MAPL3G_Component_Testing_Framework/test_cases/case23/dryrun.rc` — new

After all GEOS.x steps complete (and after the existing `compare.rc` check),
`run_comp_tester.cmake` looks for a `dryrun.rc` file in the case directory. If
present it parses three keys and runs `extdata_dryrun_check.py --check --narrow
--verify_files_read files_read.yaml`. The `files_read.yaml` is the runtime log
written by ExtData's `log_files_read` feature during the last step.

**`dryrun.rc` format:**
```
extdata_config=<yaml file for the step with log_files_read>
run_start=<clock.start from the corresponding cap yaml>
run_end=<last time step of the segment>
```

**`run_end` derivation:**
- Cases with explicit `run_times`: use the last entry.
- Cases without: `clock.start + segment_duration - dt`.

**Verified results (CTest):**

| Case | Files covered | Missing |
|------|--------------|---------|
| case02 | 1 | 0 |
| case11 | 2 | 0 |
| case23 | 23 | 0 |

### 5. Fortran dry run code (removed)
The session initially added a Fortran `ExtDataDryRun.F90` module, hook in
`ExtDataGridComp.F90`, and `dry_run_output_path` field in `ExtDataConfig.F90`,
but these were removed at the user's request in favour of the Python-only
approach.

---

## Key design decisions / algorithms

### File enumeration algorithm (per collection)

For each `ExtDataCollection` + `ExtDataSample` pair (via `ExtDataRule`):

| `extrap_outside` | Effective range | `valid_range` required? |
|---|---|---|
| `none` (normal) | `[run_start-freq, run_end+freq]`, **not** clamped to `valid_range` | No |
| `persist_closest` | Same, hard-clamped to `valid_range` | Yes — error if absent |
| `clim` | Overlap direct + non-overlap tails remapped via `swap_year`, clamped | Yes — error if absent |

**Important:** `valid_range` clamping is applied **only** for `clim` and
`persist_closest`. Plain `none` collections are never clamped.

**Clim enumeration — `_enumerate_clim_files(expand=True/False)`:**

A non-recursive helper (no `CollectionFileResult`, just a flat set).
`expand=True` (default) adds ±1 freq step: `base = [run_start-freq, run_end+freq]`.
`expand=False` uses `[run_start, run_end]` directly (used for core-file
computation in Case B).

```
base = [run_start-freq, run_end+freq]   (expand=True)
overlap = [max(base[0], vr[0]), min(base[1], vr[1])]

Case A (overlap valid):
    enumerate overlap directly
    if base[0] < vr[0]:
        remap base[0] via swap_year(vr[0].year)
        step rs back one freq so bracketing file at vr[0] is included
        clamp to [freq.sub(vr[0]), vr[1]] and enumerate
    if base[1] > vr[1]:
        remap base[1] via swap_year(vr[1].year)
        clamp to [vr[0], vr[1]] and enumerate

Case B (no overlap):
    remap both ends into vr[0].year
    if remapped range does not wrap year boundary:
        step rs back one freq; clamp to [freq.sub(vr[0]), vr[1]]; enumerate
    else (wraps, e.g. Dec→Jan):
        Dec segment: [freq.sub(rs), vr[1]] clamped; enumerate
        Jan segment: [vr[0], re] clamped; enumerate
```

**`_enumerate_clim` — builds `CollectionFileResult` with buffer classification:**

- **Case A** (run overlaps `valid_range`):
  - `core_files` = direct overlap `[max(run_start,vr[0]), min(run_end,vr[1])]`
  - `left_buffer` = `_enumerate_clim_files` on `[freq.sub(run_start), run_start]` − core
  - `right_buffer` = `_enumerate_clim_files` on `[run_end, freq.add(run_end)]` − core
  - `narrow_left_ref = run_start`, `narrow_right_ref = run_end`

- **Case B** (run entirely outside `valid_range`):
  - `core_files` = `_enumerate_clim_files(..., expand=False)` on `[run_start, run_end]`
  - `left_buffer` = `_enumerate_clim_files(..., expand=False)` on `[freq.sub(run_start), run_start]` − core
  - `right_buffer` = `_enumerate_clim_files(..., expand=False)` on `[run_end, freq.add(run_end)]` − core
  - `narrow_left_ref = swap_year(run_start, vr[0].year)`
  - `narrow_right_ref = swap_year(run_end, vr[0].year)`
  - (Both refs use `vr[0].year` — the same year the file enumeration maps into)

**Static files (no `%` token, zero frequency):** expand template once.

### Static file coverage warning

`_check_static_coverage` opens a static file and warns if its time axis does
not fully cover `[run_start, run_end]`. The warning is **suppressed** for:
- `extrap == "clim"` — the file is used cyclically; year mismatch is expected.
- `extrap == "persist_closest"` — a single static file can always be persisted
  to cover any time by definition.

### Multi-rule exports (e.g. `starting:` key)

Rules in a list-valued export are sorted by `starting` time. Each rule's
active window is `[starting_i, starting_{i+1})`. The last rule has no end date
(`[starting_N, ∞)`).

- Rules whose window does not overlap `[run_start, run_end]` are **skipped entirely**.
- For active rules the run range passed to `enumerate_collection_files` is
  **clipped** to the rule's active window:
  ```
  effective_run_start = max(run_start, rule_start)
  effective_run_end   = min(run_end, rule_end)   # last rule: rule_end = run_end
  ```

### Union across rules sharing a collection

The script iterates all rules. Multiple rules may reference the same collection
(possibly with different `extrap_outside`). The global file set is the union
of all per-rule results. In `--narrow`/`return_per_collection` mode the results
are keyed by `(collection_name, extrap)` and unioned per key.
`narrow_left_ref` takes the min across rules; `narrow_right_ref` takes the max.

### Tier 3 narrowing logic (`--narrow`)

`narrow_left_ref` and `narrow_right_ref` are stored inside `CollectionFileResult`
(not in the outer entry dict). For clim collections they are remapped into the
`valid_range` year so file-time comparisons work correctly.

For each collection:
- **Core files**: always kept (unconditionally added to final set).
- **Core coverage check**: read times from the earliest and latest core files
  to determine if the core already brackets the run edges. If so, buffer files
  on that side are suppressed entirely (both existing and missing).
- **Left buffer** (when core does not cover left edge):
  - Missing files: kept (may be needed bracket → reported as missing).
  - Existing files: open with `netCDF4`; among all candidates keep only the one
    whose latest time ≤ `narrow_left_ref` is closest to `narrow_left_ref`
    (i.e. the actual left bracket). Others are dropped.
  - If `time` variable absent: keep conservatively, warn to stderr.
- **Right buffer** (when core does not cover right edge):
  - Missing files: kept.
  - Existing files: keep only the one whose earliest time > `narrow_right_ref`
    is closest to `narrow_right_ref`. Others are dropped.
- **Static files** (no token): kept; if file exists, open and warn to stderr if
  time axis does not cover `[run_start, run_end]` (advisory only, suppressed
  for `clim` and `persist_closest` — see above).

Time variable name is always `"time"` (MAPL convention).

---

## Bugs fixed

### Session 2

1. **Infinite recursion in `_enumerate_clim`** — the old code recursively called
   itself to classify left/right buffer files, causing `RecursionError` for any
   clim collection. Fixed by extracting a non-recursive `_enumerate_clim_files`
   helper and calling it for buffer classification instead.

2. **`_enumerate_normal` incorrectly clamped to `valid_range` for all collections**
   (both scripts) — fixed to only clamp when `extrap_outside == "persist_closest"`.
   This restored missing right-buffer files for normal collections that extend
   beyond `valid_range` (e.g. `test2_20200115.nc4` in case23).

3. **Clim buffer classification wrong when run is entirely outside `valid_range`**
   — the old Case B code used `swap_year(run_start, vr_start.year)` and
   `swap_year(run_end, vr_end.year)` and compared them directly, which broke
   when `valid_range` spans multiple years (e.g. case11: 2006–2007). Fixed by
   computing core via `_enumerate_clim_files(..., expand=False)` and deriving
   buffers from the same helper on the ±1-step windows.

4. **`narrow_files` used raw `run_start`/`run_end` for clim time comparisons**
   — file times are in the `valid_range` year, not the run year. Fixed by
   storing `narrow_left_ref`/`narrow_right_ref` inside `CollectionFileResult`
   and using remapped times (`swap_year(run_start, vr_start.year)`) for clim.

5. **Narrow kept all files with any time ≤ run_start** — changed to keep only
   the single file whose latest time closest to (and ≤) `narrow_left_ref`
   (the actual left bracket), mirroring Fortran bracket-search behaviour.

6. **Missing buffer files kept even when core already covers the bracket** —
   now suppressed when a boundary core file confirms the edge is already
   bracketed.

7. **`vr_start` clamping dropped the file that brackets `vr_start`** — when
   `vr_start` is mid-period (e.g. `2006-01-15T21` for a monthly collection),
   clamping `rs = max(rs, vr_start)` excluded the grid point at `2006-01-01`
   (which is the file covering `vr_start`). Fixed by stepping `rs` back one
   freq before clamping: `rs = freq.sub(rs); rs = max(rs, freq.sub(vr_start))`.
   Applied in both `_enumerate_clim_files` and `extdata_dryrun.py`.

### Session 3

8. **Spurious static-file coverage warning for clim collections** — case02 has
   a static `test.nc4` with times in 2004; the run is 2007–2008. The warning
   was emitted even though the file is used cyclically and the year mismatch is
   by design. Fixed by suppressing the warning when `extrap == "clim"`.

9. **Same spurious warning for `persist_closest` static files** — a single
   static file used with `persist_closest` can always be persisted to any time
   by definition. Fixed by also suppressing the warning when
   `extrap == "persist_closest"`.

---

## Test cases validated

| Case | Config | Run range | Method | Result |
|---|---|---|---|---|
| case23 | `extdata3.yaml` | `2019-12-25 – 2020-01-14` | Tier 1 + `--narrow` | Exact match (23 files) |
| case11 | `extdata2.yaml` | `2005-02-01 – 2005-02-25T21` | Tier 1 + `--narrow` | Exact match (2 files) |
| case11 | `extdata2.yaml` | `2005-02-02 – 2005-02-25T21` | Tier 1 | Correctly includes `test.200601.nc4` as conservative left buffer |
| case02 | `extdata2.yaml` | `2007-10-01 – 2008-02-26T21` | Tier 1 + `--narrow` | 1 file, 0 missing, no spurious warnings |
| case02, case11, case23 | — | — | CTest `--verify_files_read` | All pass (0 missing, full coverage) |

---

## File inventory

| File | Status | Notes |
|---|---|---|
| `gridcomps/extdata/extdata_dryrun.py` | New | Simple Tier 1 script |
| `gridcomps/extdata/extdata_dryrun_check.py` | New | Full Tier 1/2/3 script + `--verify_files_read` |
| `gridcomps/extdata/CMakeLists.txt` | Modified | `install(PROGRAMS ...)` + `file(COPY ...)` to build tree |
| `tests/MAPL3G_Component_Testing_Framework/run_comp_tester.cmake` | Modified | Added `dryrun.rc` post-run verification block |
| `tests/MAPL3G_Component_Testing_Framework/test_cases/case02/dryrun.rc` | New | Dry run params for case02 |
| `tests/MAPL3G_Component_Testing_Framework/test_cases/case11/dryrun.rc` | New | Dry run params for case11 |
| `tests/MAPL3G_Component_Testing_Framework/test_cases/case23/dryrun.rc` | New | Dry run params for case23 |
| `gridcomps/extdata/ExtDataConfig.F90` | Unchanged | Fortran additions were reverted |
| `gridcomps/extdata/ExtDataGridComp.F90` | Unchanged | Fortran additions were reverted |
| `gridcomps/extdata/ExtDataDryRun.F90` | Deleted | Fortran dry run removed |

The script is deployed two ways:
- `install(PROGRAMS ...)` → `install-debug/bin/` (for manual use)
- `file(COPY ...)` → `build-debug/bin/` (for CTest; requires `cmake ..` re-run
  after script changes)

---

## Known issues / future work

- The `valid_range` separator accepts both `/` and `,`.
- Static file narrowing (`--narrow`) only warns when a static file's time axis
  doesn't cover the run range; it does not add the static file to the missing
  list (and the warning is suppressed for `clim`/`persist_closest`).
- The simple script (`extdata_dryrun.py`) does **not** handle multi-rule
  exports with `starting:` keys. Only `extdata_dryrun_check.py` has the
  multi-rule fix. This could be backported if desired.
- `source_time` (a sub-range within `valid_range` for clim collections) is
  parsed by the Fortran runtime but not currently used there either — both
  scripts ignore it for now, consistent with the Fortran behaviour.
- Only case02, case11, case23 have CTest dry run verification. Other cases with
  `log_files_read` could be wired up similarly by adding a `dryrun.rc`.
- More test cases (other case directories) have not yet been validated manually.

---

## How to restore this session

Open this file in the session context and tell the assistant:
> "I want to continue working on the ExtData dry run Python scripts. Read the
> plan at `.opencode/plans/extdata-dryrun-python.md` for context."

The key files to re-read before continuing:
- `gridcomps/extdata/extdata_dryrun.py`
- `gridcomps/extdata/extdata_dryrun_check.py`
- `gridcomps/extdata/CMakeLists.txt`
- `tests/MAPL3G_Component_Testing_Framework/run_comp_tester.cmake`
