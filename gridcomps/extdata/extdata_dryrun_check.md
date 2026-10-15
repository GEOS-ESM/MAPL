# extdata_dryrun_check.py

Estimate, verify, and narrow the set of files that an ExtData component will
need for a given run, without actually executing the model.

---

## Overview

`extdata_dryrun_check.py` reads an ExtData YAML configuration file and a run
time window (`--run_start` / `--run_end`) and predicts which input files the
ExtData component will open during the run.  It operates in up to three tiers
of increasing cost and precision:

| Tier | Flag | Filesystem access | Purpose |
|------|------|-------------------|---------|
| 1 | *(default)* | None | Enumerate all files that *could* be needed (conservative) |
| 2 | `--check` | `stat` only | Split the list into present / missing |
| 3 | `--narrow` | Open + read time axis | Drop buffer files confirmed unnecessary |

A fourth mode, `--verify_files_read`, is used by the CTest test infrastructure
to validate predictions against the files ExtData actually opened at runtime.

---

## Requirements

| Package | Tier |
|---------|------|
| `pyyaml` | 1, 2, 3 |
| `python-dateutil` | 1, 2, 3 |
| `netCDF4` | 3 only |

---

## Three-tier model

### Tier 1 — enumeration (default)

Expands every collection's file template over a maximalist window
`[run_start − freq, run_end + freq]`, adjusted for `valid_range` clamping,
climatological wrapping (`extrapolation: clim`), and `persist_closest`
behaviour.  No files are opened.  Output: `--output` YAML with a `files` list.

### Tier 2 — existence check (`--check`)

Takes the Tier 1 list and calls `os.path.exists` on each entry.  Writes two
outputs: the full estimated list (`--output`) and a separate `--missing_output`
YAML listing only the absent files.

### Tier 3 — content narrowing (`--narrow`)

Before the existence check, opens the ±1-freq *buffer* files at the run edges
with `netCDF4` and reads their `time` variable to confirm whether each buffer
file is actually needed as a temporal bracket.  Files confirmed unnecessary are
dropped.  Implies `--check`.

For static collections (no `%` token in the template), opens the file and
emits an advisory warning to `stderr` if its time axis does not fully cover
`[run_start, run_end]`.  The warning is suppressed for `clim` and
`persist_closest` collections, where a time-axis mismatch is expected and
harmless.

---

## Usage

```bash
# Tier 1 — enumerate only
extdata_dryrun_check.py --config extdata.yaml \
    --run_start 2020-01-01T00:00:00 \
    --run_end   2020-12-31T18:00:00 \
    --output estimated.yaml

# Tier 2 — enumerate + existence check
extdata_dryrun_check.py --config extdata.yaml \
    --run_start 2020-01-01T00:00:00 \
    --run_end   2020-12-31T18:00:00 \
    --output estimated.yaml \
    --check \
    --missing_output missing.yaml

# Tier 3 — enumerate + narrow + existence check
extdata_dryrun_check.py --config extdata.yaml \
    --run_start 2020-01-01T00:00:00 \
    --run_end   2020-12-31T18:00:00 \
    --output narrowed.yaml \
    --narrow \
    --missing_output missing.yaml

# Tier 3 + runtime verification (used by CTest — see CTest Integration below)
extdata_dryrun_check.py --config extdata.yaml \
    --run_start 2020-01-01T00:00:00 \
    --run_end   2020-12-31T18:00:00 \
    --output narrowed.yaml \
    --narrow \
    --missing_output missing.yaml \
    --verify_files_read files_read.yaml
```

---

## Arguments reference

| Flag | Required | Description |
|------|----------|-------------|
| `--config PATH` | Yes | Path to the ExtData YAML configuration file |
| `--run_start ISO` | Yes | Run start time, e.g. `2020-01-01T00:00:00` |
| `--run_end ISO` | Yes | Run end time, e.g. `2020-12-31T18:00:00` |
| `--output PATH` | No | Output YAML for estimated/narrowed file list (default: stdout) |
| `--check` | No | Enable Tier 2 existence check; requires `--missing_output` |
| `--narrow` | No | Enable Tier 3 narrowing; implies `--check`; requires `--missing_output` and `netCDF4` |
| `--missing_output PATH` | No* | Output YAML for missing files; required with `--check`/`--narrow` |
| `--verify_files_read PATH` | No | Path to a `files_read.yaml` produced by ExtData at runtime; verifies coverage and no missing files; requires `--check`/`--narrow` and `--missing_output` |

---

## Output file formats

### `--output` YAML

```yaml
run_start: '2020-01-01T00:00:00'
run_end:   '2020-12-31T18:00:00'
files:
  - /path/to/file_20200101.nc4
  - /path/to/file_20200102.nc4
  ...
```

### `--missing_output` YAML

```yaml
run_start: '2020-01-01T00:00:00'
run_end:   '2020-12-31T18:00:00'
missing_files:
  - /path/to/absent_file.nc4
  ...
```

---

## ExtData config keys read

The script reads a subset of the ExtData YAML format.  The relevant keys are:

| Key | Location | Description |
|-----|----------|-------------|
| `Collections` | top-level | Map of collection names to `{template: ..., valid_range: ...}` |
| `template` | per collection | File path with `%`-tokens (e.g. `%y4`, `%m2`, `%d2`, `%h2`) |
| `valid_range` | per collection | ISO range `start/end` or `start,end`; required for `clim`/`persist_closest` |
| `Samplings` | top-level | Map of sampling names to `{extrapolation: ..., frequency: ...}` |
| `extrapolation` | per sampling | `none` (default), `clim`, or `persist_closest` |
| `frequency` | per sampling | ISO 8601 duration (e.g. `PT3H`, `P1M`); inferred from template tokens if absent |
| `Exports` | top-level | Map of export names to rules (single or list with `starting:`) |
| `collection` | per export rule | Name of the collection to use |
| `sample` | per export rule | Name of the sampling to use |
| `starting` | per export rule | ISO date at which this rule becomes active (multi-rule exports) |

### Supported `%`-tokens

| Token | Meaning |
|-------|---------|
| `%y4` | 4-digit year |
| `%y2` | 2-digit year |
| `%m2` | 2-digit month |
| `%m3` | 3-letter month abbreviation (e.g. `Jan`) |
| `%mc` | Capitalised month name (e.g. `January`) |
| `%d2` | 2-digit day of month |
| `%h2` | 2-digit hour |
| `%n2` | 2-digit minute |
| `%s2` | 2-digit second |
| `%j3` | 3-digit day of year |

A collection with no `%` token in its template is treated as a **static file**
(zero frequency; the template is expanded once).

---

## File enumeration algorithm

### Per-collection dispatch

For each `(collection, sampling)` pair referenced by at least one active export
rule the script calls one of three enumeration paths:

| `extrapolation` | Path | `valid_range` required? | Clamping |
|-----------------|------|------------------------|----------|
| `none` (default) | `_enumerate_normal` | No | Never clamped |
| `persist_closest` | `_enumerate_normal` | Yes | Hard-clamped to `valid_range` |
| `clim` | `_enumerate_clim` | Yes | Cyclic remapping into `valid_range` year |

### Normal enumeration (`none` / `persist_closest`)

Steps over `[run_start − freq, run_end + freq]` (one extra step on each side
to ensure bracketing files are included).  For `persist_closest` the window is
additionally hard-clamped to `[valid_range_start, valid_range_end]`.

### Clim enumeration

Clim collections reuse a fixed set of files (one year's worth) cyclically.
The enumeration is split into a non-recursive helper
`_enumerate_clim_files(run_start, run_end, expand=True/False)` that returns a
flat set of filenames, and a wrapper `_enumerate_clim` that classifies them
into *core*, *left buffer*, and *right buffer* (needed for Tier 3 narrowing).

**`_enumerate_clim_files` pseudocode:**

```
base = [run_start − freq, run_end + freq]   if expand=True
base = [run_start, run_end]                 if expand=False

overlap = [max(base[0], vr_start), min(base[1], vr_end)]

Case A — overlap is non-empty (run range intersects valid_range):
    enumerate overlap directly
    if base[0] < vr_start:
        rs = swap_year(base[0], vr_start.year)
        rs = max(rs − freq, freq.sub(vr_start))   # step back to include bracket
        enumerate [rs, vr_end] clamped
    if base[1] > vr_end:
        re = swap_year(base[1], vr_end.year)
        enumerate [vr_start, re] clamped

Case B — no overlap (run range entirely outside valid_range):
    rs = swap_year(run_start, vr_start.year)
    re = swap_year(run_end,   vr_start.year)
    if rs ≤ re (does not wrap year boundary):
        rs = max(rs − freq, freq.sub(vr_start))
        enumerate [rs, vr_end] clamped
    else (wraps, e.g. Nov → Feb):
        enumerate [rs − freq clamped to vr_start, vr_end]   (Dec side)
        enumerate [vr_start, re]                             (Jan side)
```

**Buffer classification in `_enumerate_clim`:**

| Situation | `core_files` | `left_buffer` | `right_buffer` | `narrow_*_ref` |
|-----------|-------------|---------------|----------------|----------------|
| Case A (run overlaps `valid_range`) | `_enumerate_clim_files([max(run_start, vr_start), min(run_end, vr_end)])` | `_enumerate_clim_files([run_start−freq, run_start])` − core | `_enumerate_clim_files([run_end, run_end+freq])` − core | `run_start` / `run_end` |
| Case B (run outside `valid_range`) | `_enumerate_clim_files([run_start, run_end], expand=False)` | `_enumerate_clim_files([run_start−freq, run_start], expand=False)` − core | `_enumerate_clim_files([run_end, run_end+freq], expand=False)` − core | `swap_year(run_start, vr_start.year)` / `swap_year(run_end, vr_start.year)` |

### Multi-rule exports (`starting:` key)

When an export is a list, each entry has a `starting:` date that defines when
that rule becomes active.  Rules are sorted by `starting` and each rule's
active window is `[starting_i, starting_{i+1})`.  The last rule is active until
`run_end`.

- Rules whose active window does not overlap `[run_start, run_end]` are skipped.
- For active rules the run range is clipped to the rule's window:

```
effective_run_start = max(run_start, rule_start)
effective_run_end   = min(run_end,   rule_end)    # last rule: rule_end = run_end
```

### Union across rules

Multiple rules may reference the same collection.  The final file set is the
union of all per-rule results.  In `--narrow` mode results are keyed by
`(collection_name, extrapolation)` and merged: `narrow_left_ref` takes the
minimum across rules; `narrow_right_ref` takes the maximum.

---

## Tier 3 narrowing logic

After collecting `core_files`, `left_buffer`, and `right_buffer` per
collection, `narrow_files` refines the list:

1. **Core files** — always kept unconditionally.

2. **Core coverage check** — read the `time` variable from the earliest and
   latest core files.  If the earliest core file's minimum time is already ≤
   `narrow_left_ref`, the left edge is already bracketed and the entire left
   buffer is suppressed (both existing and missing files).  Same logic on the
   right side.

3. **Left buffer** (when core does not cover the left edge):
   - Missing files: kept (they may be the needed bracket and will appear in
     `--missing_output`).
   - Existing files: open each with `netCDF4`; keep only the one whose latest
     time ≤ `narrow_left_ref` is closest to `narrow_left_ref` (the actual left
     bracket).  All others are dropped.
   - If the `time` variable is absent: keep the file conservatively and warn to
     `stderr`.

4. **Right buffer** (when core does not cover the right edge):
   - Missing files: kept.
   - Existing files: keep only the one whose earliest time > `narrow_right_ref`
     is closest to `narrow_right_ref`.  Others are dropped.

5. **Static files** — kept unconditionally.  If the file exists, open it and
   warn to `stderr` if its time axis does not fully cover `[run_start,
   run_end]`.  Warning suppressed for `clim` and `persist_closest`.

**Clim time remapping for narrowing:** file times are in the `valid_range`
year, not the run year.  For clim collections `narrow_left_ref` and
`narrow_right_ref` are remapped into `vr_start.year` (Case B) so that
comparisons against file times are correct.

---

## CTest integration

The script integrates with the MAPL component test framework
(`tests/MAPL3G_Component_Testing_Framework`) to validate dry run predictions
against files actually opened by a real model run.

### How it works

After each GEOS.x run, `run_comp_tester.cmake` checks for a `dryrun.rc` file
in the test case directory.  If present it runs the script with
`--check --narrow --verify_files_read files_read.yaml`, where
`files_read.yaml` is the runtime log written by ExtData's `log_files_read`
feature.

### `dryrun.rc` format

```
extdata_config=extdata2.yaml   # YAML file that has log_files_read
run_start=2020-01-01T00:00:00  # clock.start from the cap yaml
run_end=2020-12-31T18:00:00    # last time step of the segment
```

`run_end` is derived as:
- Cases with explicit `run_times` in the cap YAML: last entry in the list.
- Cases without: `clock.start + segment_duration − dt`.

### `--verify_files_read` checks

When `--verify_files_read PATH` is given the script performs two checks after
producing the estimated and missing file lists:

1. **Coverage** — every file in the `files_read` list must appear in the
   estimated set.  A file that ExtData actually opened but the dry run did not
   predict is a false negative; the script reports it and exits 1.

2. **No missing files** — the `--missing_output` YAML must have an empty
   `missing_files` list.  Any predicted file absent from disk is reported and
   the script exits 1.

On success the script prints `OK: dry run verified — N files covered, 0 missing.`

### Cases with `dryrun.rc`

| Case | Config | Run range | Files covered |
|------|--------|-----------|---------------|
| case02 | `extdata2.yaml` | `2007-10-01 – 2008-02-26T21` | 1 |
| case11 | `extdata2.yaml` | `2005-02-01 – 2005-02-25T21` | 2 |
| case23 | `extdata3.yaml` | `2019-12-25 – 2020-01-14` | 23 |

---

## Limitations

- **`source_time`** — a sub-range within `valid_range` used by some clim
  collections — is parsed by the Fortran runtime but ignored by both scripts,
  consistent with current Fortran behaviour.

- **Static file narrowing** is advisory only: if a static file's time axis does
  not cover the run range the script warns but does not add the file to the
  missing list, because it cannot determine at this level whether the gap is
  actually a problem.

- The script assumes the `time` variable in NetCDF files follows the MAPL
  convention (`units` attribute parseable by `netCDF4`/`cftime`).

- Only one level of multi-rule nesting is supported (a flat list of
  `{starting: ..., collection: ...}` entries).  Nested rules are not handled.
