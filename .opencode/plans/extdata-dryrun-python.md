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

Both scripts are installed to `bin/` via:
```cmake
install(
  PROGRAMS extdata_dryrun.py extdata_dryrun_check.py
  DESTINATION bin)
```

### 4. Fortran dry run code (removed)
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
| `none` (normal) | `[run_start-freq, run_end+freq]`, clamped to `valid_range` if present | No |
| `persist_closest` | Same, hard-clamped to `valid_range` | Yes — error if absent |
| `clim` | Overlap direct + non-overlap tails remapped via `swap_year`, all clamped | Yes — error if absent |

**Clim overlap logic:**
```
base = [run_start-freq, run_end+freq]
overlap = [max(base[0], vr[0]), min(base[1], vr[1])]

if overlap valid:
    enumerate overlap directly
    if base[0] < vr[0]: remap base[0] via swap_year(vr[0].year), enumerate clamped left tail
    if base[1] > vr[1]: remap base[1] via swap_year(vr[1].year), enumerate clamped right tail
else (no overlap):
    remap both ends to vr[0].year
    if remapped range wraps year boundary: split into [rs, vr[1]] + [vr[0], re]
    clamp and enumerate
```

**Static files (no `%` token, zero frequency):** expand template once.

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

### Tier 3 narrowing logic (`--narrow`)

For each collection:
- **Core files** `[run_start, run_end]`: always kept.
- **Left buffer** files (before `run_start`):
  - If file does not exist: keep (may be needed left bracket → report as missing).
  - If file exists: open with `netCDF4`, keep only if it contains a time `<= run_start`.
- **Right buffer** files (after `run_end`):
  - If file does not exist: keep.
  - If file exists: open, keep only if it contains a time `> run_end`.
- **Static files** (no token): if file exists, open and warn to stderr if time
  axis does not cover `[run_start, run_end]` (advisory, file still kept).
- If `read_file_times` returns `None` (no `time` variable): keep conservatively
  and warn to stderr.

Time variable name is always `"time"` (MAPL convention).

---

## File inventory

| File | Status | Notes |
|---|---|---|
| `gridcomps/extdata/extdata_dryrun.py` | New | Simple Tier 1 script |
| `gridcomps/extdata/extdata_dryrun_check.py` | New | Full Tier 1/2/3 script |
| `gridcomps/extdata/CMakeLists.txt` | Modified | Added `install(PROGRAMS ...)` |
| `gridcomps/extdata/ExtDataConfig.F90` | Unchanged | Fortran additions were reverted |
| `gridcomps/extdata/ExtDataGridComp.F90` | Unchanged | Fortran additions were reverted |
| `gridcomps/extdata/ExtDataDryRun.F90` | Deleted | Fortran dry run removed |

---

## Known issues / future work

- The `valid_range` separator now accepts both `/` and `,` (was `,` only; bug
  found in case03 config which used `/`).
- Static file narrowing (`--narrow`) only warns; it does not add the static
  file to the missing list. If coverage is truly insufficient the user must
  act on the warning manually.
- The simple script (`extdata_dryrun.py`) does **not** handle multi-rule
  exports with `starting:` keys (no clipping/skipping). Only
  `extdata_dryrun_check.py` has the multi-rule fix. This could be backported
  to the simple script if desired.
- Both scripts have been manually synced to
  `install-debug/bin/` but that copy is not managed by the build system —
  a real rebuild + install is needed to pick up changes permanently.

---

## How to restore this session

Open this file in the session context and tell the assistant:
> "I want to continue working on the ExtData dry run Python scripts. Read the
> plan at `.opencode/plans/extdata-dryrun-python.md` for context."

The key files to re-read before continuing:
- `gridcomps/extdata/extdata_dryrun.py`
- `gridcomps/extdata/extdata_dryrun_check.py`
- `gridcomps/extdata/CMakeLists.txt`
