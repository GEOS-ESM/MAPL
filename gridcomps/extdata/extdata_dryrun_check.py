#!/usr/bin/env python3
"""
extdata_dryrun_check.py  --  Estimate, verify, and narrow the files needed by
                             an ExtData component.

Three tiers of analysis are available:

  Tier 1 (default)  --  Cheap enumeration.  Expands file templates over the
                        maximalist time range [run_start - freq,
                        run_end + freq], adjusted for valid_range clamping,
                        climatological wrapping, and persist_closest.  No
                        filesystem access.  Output: --output YAML.

  Tier 2 (--check)  --  Filesystem existence check.  Takes the Tier 1 list
                        and tests whether each file exists on disk.  Writes a
                        separate --missing_output YAML listing absent files.
                        Does not open any files.

  Tier 3 (--narrow) --  File-content narrowing.  Opens the ±1-freq buffer
                        files at the run edges using netCDF4 and reads their
                        time axes to confirm whether they are actually needed
                        as bracketing files.  Files confirmed unnecessary are
                        dropped before the existence check.  Implies --check.
                        For static (no-token) collections, opens the file and
                        checks whether its time axis covers [run_start,
                        run_end]; emits a warning to stderr if not.

Usage
-----
  # Tier 1 only
  extdata_dryrun_check.py --config extdata.yaml \\
      --run_start 2020-01-01T00:00:00 --run_end 2020-12-31T18:00:00 \\
      --output estimated.yaml

  # Tier 2
  extdata_dryrun_check.py --config extdata.yaml \\
      --run_start 2020-01-01T00:00:00 --run_end 2020-12-31T18:00:00 \\
      --output estimated.yaml --check --missing_output missing.yaml

  # Tier 3
  extdata_dryrun_check.py --config extdata.yaml \\
      --run_start 2020-01-01T00:00:00 --run_end 2020-12-31T18:00:00 \\
      --output narrowed.yaml --narrow --missing_output missing.yaml
"""

import argparse
import calendar
import os
import re
import sys
from collections import namedtuple
from datetime import datetime, timedelta
from pathlib import Path

import yaml
from dateutil.relativedelta import relativedelta


# ---------------------------------------------------------------------------
# Month name look-up tables (matching StringTemplate.F90 evaluate_token)
# ---------------------------------------------------------------------------
_MONTHS_LOWER = [
    "jan", "feb", "mar", "apr", "may", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec",
]
_MONTHS_MIXED = [m.capitalize() for m in _MONTHS_LOWER]
_MONTHS_UPPER = [m.upper() for m in _MONTHS_LOWER]


# ---------------------------------------------------------------------------
# Token expansion  (mirrors fill_grads_template / evaluate_token in
# mp_utils/StringTemplate.F90)
# ---------------------------------------------------------------------------

def fill_grads_template(template: str, dt: datetime) -> str:
    """Expand GrADS-style time tokens in *template* using the datetime *dt*.

    Supported tokens:
        %y4  4-digit year
        %y2  2-digit year (last two digits)
        %m1  month, no leading zero
        %m2  month, zero-padded to 2 digits
        %mc  lowercase 3-letter month abbreviation
        %Mc  mixed-case 3-letter month abbreviation
        %MC  uppercase 3-letter month abbreviation
        %d1  day of month, no leading zero
        %d2  day of month, zero-padded to 2 digits
        %h1  hour, no leading zero
        %h2  hour, zero-padded to 2 digits
        %h3  hour, zero-padded to 3 digits
        %n2  minute, zero-padded to 2 digits
        %S2  second, zero-padded to 2 digits
        %D3  day-of-year, zero-padded to 3 digits
        %C2  2-digit century
    """
    doy = dt.timetuple().tm_yday

    # Order matters: longer/more-specific tokens before shorter prefix-sharing ones.
    _REPLACEMENTS = [
        ("%y4", f"{dt.year:04d}"),
        ("%y2", f"{dt.year % 100:02d}"),
        ("%MC", _MONTHS_UPPER[dt.month - 1]),
        ("%Mc", _MONTHS_MIXED[dt.month - 1]),
        ("%mc", _MONTHS_LOWER[dt.month - 1]),
        ("%m2", f"{dt.month:02d}"),
        ("%m1", str(dt.month)),
        ("%d2", f"{dt.day:02d}"),
        ("%d1", str(dt.day)),
        ("%h3", f"{dt.hour:03d}"),
        ("%h2", f"{dt.hour:02d}"),
        ("%h1", str(dt.hour)),
        ("%n2", f"{dt.minute:02d}"),
        ("%S2", f"{dt.second:02d}"),
        ("%D3", f"{doy:03d}"),
        ("%C2", f"{dt.year // 100:02d}"),
    ]

    result = template
    for token, value in _REPLACEMENTS:
        result = result.replace(token, value)
    return result


# ---------------------------------------------------------------------------
# Frequency representation
# ---------------------------------------------------------------------------

class Freq:
    """A file frequency that can be added to / subtracted from a datetime.

    Wraps either a timedelta (fixed-duration: minutes, hours, days) or a
    relativedelta (calendar-relative: months, years).

    is_calendar  True for monthly/yearly frequencies (relativedelta-based).
    is_zero      True for static/single-file collections (zero interval).
    """

    def __init__(self, delta, *, is_calendar: bool = False):
        self._delta = delta
        self.is_calendar = is_calendar

    @property
    def is_zero(self) -> bool:
        if self.is_calendar:
            return False
        return self._delta == timedelta(0)

    def add(self, dt: datetime) -> datetime:
        return dt + self._delta

    def sub(self, dt: datetime) -> datetime:
        return dt - self._delta

    def total_seconds(self) -> float:
        """Only valid for non-calendar frequencies."""
        if self.is_calendar:
            raise ValueError(
                "total_seconds() not meaningful for calendar-relative frequencies"
            )
        return self._delta.total_seconds()

    def __repr__(self):
        return f"Freq({self._delta!r}, is_calendar={self.is_calendar})"


# ---------------------------------------------------------------------------
# MAPL / ISO-8601 duration string parsing  (e.g. PT6H, P1D, P1M, P1Y, PT0S)
# ---------------------------------------------------------------------------

_DURATION_RE = re.compile(
    r"^P"
    r"(?:(\d+)Y)?"    # years
    r"(?:(\d+)M)?"    # months  (before T)
    r"(?:(\d+)D)?"    # days
    r"(?:T"
    r"(?:(\d+)H)?"    # hours
    r"(?:(\d+)M)?"    # minutes (after T)
    r"(?:(\d+)S)?"    # seconds
    r")?$",
    re.IGNORECASE,
)


def parse_freq_string(s: str) -> Freq:
    """Parse a MAPL ISO-8601 duration string into a Freq object."""
    m = _DURATION_RE.match(s.strip())
    if not m:
        raise ValueError(f"Cannot parse frequency string: {s!r}")
    yy, mo, dd, hh, mn, ss = (int(v) if v else 0 for v in m.groups())
    if yy or mo:
        return Freq(
            relativedelta(years=yy, months=mo, days=dd, hours=hh, minutes=mn, seconds=ss),
            is_calendar=True,
        )
    return Freq(timedelta(days=dd, hours=hh, minutes=mn, seconds=ss))


def infer_freq_from_template(template: str) -> Freq:
    """Infer the file frequency from the last %-token in the template.

    Mirrors ExtDataCollection.F90 lines 56-77.
    """
    last = template.rfind("%")
    if last < 0:
        return Freq(timedelta(0))
    token = template[last + 1: last + 3]
    mapping = {
        "y4": Freq(relativedelta(years=1), is_calendar=True),
        "m2": Freq(relativedelta(months=1), is_calendar=True),
        "d2": Freq(timedelta(days=1)),
        "h2": Freq(timedelta(hours=1)),
        "n2": Freq(timedelta(minutes=1)),
    }
    if token not in mapping:
        raise ValueError(
            f"Unsupported template token %{token!r} in template {template!r}"
        )
    return mapping[token]


# ---------------------------------------------------------------------------
# Reference time inference
# Mirrors ExtDataCollection.F90 lines 82-130.
# ---------------------------------------------------------------------------

def infer_reff_time(template: str, seed_time: datetime) -> datetime:
    """Truncate *seed_time* to the precision implied by the last token in *template*."""
    last = template.rfind("%")
    if last < 0:
        return seed_time
    token = template[last + 1: last + 3]
    y, mo, d, h = seed_time.year, seed_time.month, seed_time.day, seed_time.hour
    if token == "y4":
        return datetime(y, 1, 1, 0, 0, 0)
    elif token == "m2":
        return datetime(y, mo, 1, 0, 0, 0)
    elif token == "d2":
        return datetime(y, mo, d, 0, 0, 0)
    elif token == "h2":
        return datetime(y, mo, d, h, 0, 0)
    elif token == "n2":
        return datetime(y, mo, d, h, seed_time.minute, 0)
    else:
        return seed_time


# ---------------------------------------------------------------------------
# Leap-year-aware year substitution  (mirrors ExtDataUtilities.F90:swap_year)
# ---------------------------------------------------------------------------

def swap_year(dt: datetime, target_year: int) -> datetime:
    """Return *dt* with the year replaced by *target_year*.

    If *dt* is Feb 29 and *target_year* is not a leap year, uses Feb 28.
    """
    day = dt.day
    if dt.month == 2 and dt.day == 29 and not calendar.isleap(target_year):
        day = 28
    return dt.replace(year=target_year, day=day)


# ---------------------------------------------------------------------------
# Time / range string parsing
# ---------------------------------------------------------------------------

def parse_iso8601(s: str) -> datetime:
    """Parse an ISO-8601 datetime string (YYYY-MM-DDTHH:MM:SS or variants)."""
    s = s.strip().replace(" ", "T")
    if "T" not in s:
        s += "T00:00:00"
    date_part, time_part = s.split("T", 1)
    time_fields = time_part.split(":")
    while len(time_fields) < 3:
        time_fields.append("00")
    full = f"{date_part}T{':'.join(time_fields[:3])}"
    return datetime.fromisoformat(full)


def parse_time_range(s: str):
    """Parse a two-element time range string into (start_datetime, end_datetime).

    Accepts either '/' or ',' as the separator, optionally enclosed in square
    brackets, e.g.:
        "2000-01-01T00:00:00/2000-12-01T00:00:00"
        "[2000-01-01T00:00:00, 2000-12-01T00:00:00]"
    """
    s = s.strip().strip("[]")
    sep = "/" if "/" in s else ","
    parts = [p.strip() for p in s.split(sep)]
    if len(parts) != 2:
        raise ValueError(f"Expected two times in valid_range, got: {s!r}")
    return parse_iso8601(parts[0]), parse_iso8601(parts[1])


# ---------------------------------------------------------------------------
# YAML config loading with recursive subconfig support
# ---------------------------------------------------------------------------

_MAP_KEYS = {"Samplings", "Collections", "Exports", "Derived"}


def _merge_config(base: dict, extra: dict) -> dict:
    """Merge *extra* into *base*.

    For the top-level map keys the child dicts are merged additively.
    All other keys are overwritten.
    """
    for k, v in extra.items():
        if (
            k in _MAP_KEYS
            and k in base
            and isinstance(base[k], dict)
            and isinstance(v, dict)
        ):
            base[k].update(v)
        else:
            base[k] = v
    return base


def load_config(config_path: str) -> dict:
    """Load an ExtData YAML config, recursively following subconfigs keys."""
    config_path = Path(config_path).resolve()
    with open(config_path) as f:
        data = yaml.safe_load(f) or {}

    if "subconfigs" in data:
        sub_list = data.pop("subconfigs")
        if isinstance(sub_list, str):
            sub_list = [sub_list]
        for sub_path in sub_list:
            sub_full = config_path.parent / sub_path
            sub_data = load_config(str(sub_full))
            data = _merge_config(data, sub_data)

    return data


# ---------------------------------------------------------------------------
# Sample / collection helpers
# ---------------------------------------------------------------------------

_DEFAULT_SAMPLE = {
    "extrapolation": "none",
    "time_interpolation": True,
    "exact": False,
}


def resolve_sample(rule: dict, samplings: dict) -> dict:
    """Return the effective sample dict for a rule."""
    if "sample" not in rule:
        return _DEFAULT_SAMPLE.copy()
    sample_val = rule["sample"]
    if isinstance(sample_val, dict):
        merged = _DEFAULT_SAMPLE.copy()
        merged.update(sample_val)
        return merged
    name = str(sample_val)
    if name not in samplings:
        raise KeyError(f"Sample key {name!r} not found in Samplings")
    merged = _DEFAULT_SAMPLE.copy()
    merged.update(samplings[name])
    return merged


def parse_collection(col_dict: dict, run_start: datetime) -> dict:
    """Normalise a raw collection dict into a canonical form.

    Returns a dict with keys:
        template    str
        freq        Freq
        reff_time   datetime
        valid_range (datetime, datetime) | None
    """
    template = col_dict.get("template", "")
    if not template:
        raise ValueError("Collection has no 'template' key")

    if "freq" in col_dict:
        freq = parse_freq_string(str(col_dict["freq"]))
    else:
        freq = infer_freq_from_template(template)

    valid_range = None
    if "valid_range" in col_dict:
        valid_range = parse_time_range(str(col_dict["valid_range"]))

    seed = valid_range[0] if valid_range else run_start
    if "ref_time" in col_dict:
        reff_time = parse_iso8601(str(col_dict["ref_time"]))
    else:
        reff_time = infer_reff_time(template, seed)

    return {
        "template": template,
        "freq": freq,
        "reff_time": reff_time,
        "valid_range": valid_range,
    }


# ---------------------------------------------------------------------------
# Core time-range stepper
# ---------------------------------------------------------------------------

def _enumerate_time_range(
    template: str,
    reff_time: datetime,
    freq: Freq,
    range_start: datetime,
    range_end: datetime,
) -> set:
    """Expand *template* for every grid point of *freq* within
    [range_start, range_end].  Returns a set of path strings."""
    files = set()
    if range_start > range_end:
        return files

    if freq.is_zero:
        files.add(fill_grads_template(template, range_start))
        return files

    if freq.is_calendar:
        t = reff_time
        if t > range_start:
            while t > range_start:
                t = freq.sub(t)
        else:
            t_prev = t
            while t <= range_start:
                t_prev = t
                t = freq.add(t)
            t = t_prev
        t = freq.sub(t)
    else:
        step_s = freq.total_seconds()
        diff_s = (range_start - reff_time).total_seconds()
        n = int(diff_s // step_s)
        t = reff_time + timedelta(seconds=(n - 1) * step_s)

    while t <= range_end:
        if t >= range_start:
            files.add(fill_grads_template(template, t))
        t = freq.add(t)

    return files


# ---------------------------------------------------------------------------
# CollectionFileResult: carries the full file set plus the buffer sub-sets
# needed for --narrow mode.
# ---------------------------------------------------------------------------

CollectionFileResult = namedtuple(
    "CollectionFileResult",
    ["all_files", "core_files", "left_buffer", "right_buffer", "is_static"],
)


# ---------------------------------------------------------------------------
# Per-collection enumeration
# ---------------------------------------------------------------------------

def enumerate_collection_files(
    collection: dict,
    extrap_outside: str,
    run_start: datetime,
    run_end: datetime,
) -> CollectionFileResult:
    """Return a CollectionFileResult for *collection*."""
    template    = collection["template"]
    freq: Freq  = collection["freq"]
    reff_time   = collection["reff_time"]
    valid_range = collection["valid_range"]

    # Static single file
    if freq.is_zero or "%" not in template:
        path = fill_grads_template(template, reff_time)
        return CollectionFileResult(
            all_files={path}, core_files={path},
            left_buffer=set(), right_buffer=set(), is_static=True,
        )

    if extrap_outside in ("persist_closest", "clim") and valid_range is None:
        raise ValueError(
            f"extrap_outside={extrap_outside!r} requires valid_range "
            f"but none is set for template {template!r}"
        )

    if extrap_outside == "clim":
        return _enumerate_clim(template, freq, reff_time, valid_range, run_start, run_end)
    else:
        return _enumerate_normal(template, freq, reff_time, valid_range, run_start, run_end)


def _enumerate_normal(template, freq, reff_time, valid_range, run_start, run_end):
    """Normal / persist_closest enumeration with buffer tracking."""
    # Left buffer: [run_start - freq, run_start)
    buf_left_start = freq.sub(run_start)
    buf_left_end   = freq.sub(freq.add(run_start))  # = run_start - epsilon; use run_start exclusive

    # Right buffer: (run_end, run_end + freq]
    buf_right_start = freq.add(freq.sub(run_end))   # = run_end + epsilon; use run_end exclusive
    buf_right_end   = freq.add(run_end)

    # Apply valid_range clamping
    full_start = freq.sub(run_start)
    full_end   = freq.add(run_end)
    if valid_range is not None:
        full_start = max(full_start, valid_range[0])
        full_end   = min(full_end,   valid_range[1])

    all_files = _enumerate_time_range(template, reff_time, freq, full_start, full_end)

    # Core files: strictly within [run_start, run_end]
    core_files = _enumerate_time_range(template, reff_time, freq, run_start, run_end)

    # Buffer files: everything outside the core
    non_core = all_files - core_files
    # Classify by which side of the run range they fall on
    left_buffer  = set()
    right_buffer = set()
    for f in non_core:
        # Re-enumerate just the left buffer range to classify
        left_buffer.add(f)   # conservative: put all non-core in left initially

    # More precise classification: enumerate each buffer window separately
    left_start_clamped  = full_start
    left_end_clamped    = run_start
    right_start_clamped = run_end
    right_end_clamped   = full_end

    left_buffer  = _enumerate_time_range(template, reff_time, freq,
                                         left_start_clamped, left_end_clamped) - core_files
    right_buffer = _enumerate_time_range(template, reff_time, freq,
                                         right_start_clamped, right_end_clamped) - core_files

    return CollectionFileResult(
        all_files=all_files,
        core_files=core_files,
        left_buffer=left_buffer,
        right_buffer=right_buffer,
        is_static=False,
    )


def _enumerate_clim(template, freq, reff_time, valid_range, run_start, run_end):
    """Climatological wrapping enumeration with buffer tracking."""
    vr_start, vr_end = valid_range
    base_start = freq.sub(run_start)
    base_end   = freq.add(run_end)

    overlap_start = max(base_start, vr_start)
    overlap_end   = min(base_end,   vr_end)

    all_files = set()

    if overlap_start <= overlap_end:
        all_files |= _enumerate_time_range(template, reff_time, freq,
                                           overlap_start, overlap_end)
        if base_start < vr_start:
            rs = swap_year(base_start, vr_start.year)
            re = vr_start
            rs = max(rs, vr_start)
            re = min(re, vr_end)
            if rs <= re:
                all_files |= _enumerate_time_range(template, reff_time, freq, rs, re)
        if base_end > vr_end:
            rs = vr_end
            re = swap_year(base_end, vr_end.year)
            rs = max(rs, vr_start)
            re = min(re, vr_end)
            if rs <= re:
                all_files |= _enumerate_time_range(template, reff_time, freq, rs, re)
    else:
        rs = swap_year(base_start, vr_start.year)
        re = swap_year(base_end,   vr_start.year)
        if rs <= re:
            rs = max(rs, vr_start)
            re = min(re, vr_end)
            if rs <= re:
                all_files |= _enumerate_time_range(template, reff_time, freq, rs, re)
        else:
            rs1 = max(rs, vr_start)
            re1 = vr_end
            if rs1 <= re1:
                all_files |= _enumerate_time_range(template, reff_time, freq, rs1, re1)
            rs2 = vr_start
            re2 = min(re, vr_end)
            if rs2 <= re2:
                all_files |= _enumerate_time_range(template, reff_time, freq, rs2, re2)

    # For clim, compute core files by running the same logic over [run_start, run_end]
    # clamped to valid_range.  Everything outside that is a buffer file.
    core_start = max(run_start, vr_start)
    core_end   = min(run_end,   vr_end)
    if core_start <= core_end:
        core_files = _enumerate_time_range(template, reff_time, freq, core_start, core_end)
    else:
        # run entirely outside valid_range — all files are "buffer" (remapped)
        core_files = set()

    non_core = all_files - core_files

    # Classify non-core as left/right buffer based on which run edge they serve.
    # For clim, use the remapped run_start and run_end as reference points.
    left_buffer  = set()
    right_buffer = set()
    for f in non_core:
        # Files that were generated from the left tail (before run_start /
        # before vr_start) go to left_buffer; right tail to right_buffer.
        # Since we don't easily recover which side generated which file in the
        # clim case, we use a heuristic: enumerate the left tail and right tail
        # windows separately.
        pass  # filled below

    # Left tail window: base_start to run_start (remapped if needed)
    left_files  = set()
    right_files = set()

    # Re-enumerate just the left-buffer portion (one step before run_start)
    ls = freq.sub(run_start)
    le = run_start
    # Apply the same clim remapping logic for this sub-window
    left_result = _enumerate_clim(template, freq, reff_time, valid_range, ls, le)
    left_files  = left_result.all_files - core_files

    # Re-enumerate just the right-buffer portion
    rs_r = run_end
    re_r = freq.add(run_end)
    right_result = _enumerate_clim(template, freq, reff_time, valid_range, rs_r, re_r)
    right_files  = right_result.all_files - core_files

    left_buffer  = left_files
    right_buffer = right_files

    return CollectionFileResult(
        all_files=all_files,
        core_files=core_files,
        left_buffer=left_buffer,
        right_buffer=right_buffer,
        is_static=False,
    )


# ---------------------------------------------------------------------------
# Main orchestration
# ---------------------------------------------------------------------------

def collect_all_files(
    config: dict,
    run_start: datetime,
    run_end: datetime,
    return_per_collection: bool = False,
):
    """Traverse all Exports and return file information.

    When return_per_collection is False (default): returns a flat set of paths.
    When return_per_collection is True: returns a dict keyed by a unique
    collection+extrap key, each value being a dict with 'collection' metadata
    and a CollectionFileResult.
    """
    samplings   = config.get("Samplings",   {}) or {}
    collections = config.get("Collections", {}) or {}
    exports     = config.get("Exports",     {}) or {}

    if return_per_collection:
        per_collection = {}
    else:
        all_files = set()

    for export_name, export_val in exports.items():
        is_multi_rule = isinstance(export_val, list)
        rules = export_val if is_multi_rule else [export_val]

        # For multi-rule exports, sort by 'starting' and compute each rule's
        # active time window.  Rules whose window does not overlap the run
        # range are skipped entirely.
        if is_multi_rule:
            rules = sorted(
                [r for r in rules if isinstance(r, dict)],
                key=lambda r: parse_iso8601(str(r["starting"])),
            )
            rule_starts = [parse_iso8601(str(r["starting"])) for r in rules]
        else:
            rule_starts = [None]

        for i, rule in enumerate(rules):
            if not isinstance(rule, dict):
                continue

            # Determine the clipped run range for this rule
            if is_multi_rule:
                rule_start = rule_starts[i]
                # Last rule has no end; all others end when the next rule starts
                rule_end = rule_starts[i + 1] if i + 1 < len(rules) else None

                # Skip rules whose active window does not overlap [run_start, run_end]
                if rule_end is not None and rule_end <= run_start:
                    continue
                if rule_start > run_end:
                    continue

                # Clip the run range to this rule's active window
                effective_run_start = max(run_start, rule_start)
                effective_run_end   = min(run_end, rule_end) if rule_end is not None else run_end
            else:
                effective_run_start = run_start
                effective_run_end   = run_end

            col_name = rule.get("collection", "")
            if not col_name or col_name == "/dev/null":
                continue
            if col_name not in collections:
                raise KeyError(
                    f"Rule {export_name!r} references unknown collection {col_name!r}"
                )
            col_dict   = collections[col_name]
            sample     = resolve_sample(rule, samplings)
            extrap     = sample.get("extrapolation", "none") or "none"
            collection = parse_collection(col_dict, effective_run_start)
            result     = enumerate_collection_files(
                collection, extrap, effective_run_start, effective_run_end
            )

            if return_per_collection:
                # Key by (collection_name, extrap) — union results for same key
                key = (col_name, extrap)
                if key not in per_collection:
                    per_collection[key] = {
                        "collection": collection,
                        "result": result,
                    }
                else:
                    # Union the results
                    existing = per_collection[key]["result"]
                    per_collection[key]["result"] = CollectionFileResult(
                        all_files    = existing.all_files    | result.all_files,
                        core_files   = existing.core_files   | result.core_files,
                        left_buffer  = existing.left_buffer  | result.left_buffer,
                        right_buffer = existing.right_buffer | result.right_buffer,
                        is_static    = existing.is_static and result.is_static,
                    )
            else:
                all_files |= result.all_files

    if return_per_collection:
        return per_collection
    return all_files


# ---------------------------------------------------------------------------
# Tier 2: filesystem existence check
# ---------------------------------------------------------------------------

def check_files_exist(files: set) -> tuple:
    """Split *files* into (present, missing) based on os.path.exists()."""
    present = set()
    missing = set()
    for f in files:
        (present if os.path.exists(f) else missing).add(f)
    return present, missing


# ---------------------------------------------------------------------------
# Tier 3: file-content narrowing
# ---------------------------------------------------------------------------

def read_file_times(path: str):
    """Read the 'time' variable from a NetCDF file and return a list of
    datetime objects.

    Returns None if the 'time' variable is absent (caller should treat the
    file as usable and emit a warning).

    Raises on any other error (file unreadable, corrupt, etc.).
    """
    import netCDF4  # noqa: PLC0415 — intentional lazy import

    ds = netCDF4.Dataset(path)
    try:
        if "time" not in ds.variables:
            return None
        tvar = ds.variables["time"]
        times = netCDF4.num2date(
            tvar[:],
            tvar.units,
            getattr(tvar, "calendar", "standard"),
            only_use_cftime_datetimes=False,
            only_use_python_datetimes=True,
        )
        return list(times)
    finally:
        ds.close()


def narrow_files(per_collection: dict, run_start: datetime, run_end: datetime) -> set:
    """Narrow the estimated file list by opening existing buffer files and
    checking whether they actually contain bracketing times.

    For each collection:
    - Core files are always kept.
    - Left buffer files that exist on disk are opened; kept only if they
      contain at least one time <= run_start (could be a left bracket).
    - Right buffer files that exist on disk are opened; kept only if they
      contain at least one time > run_end (could be a right bracket).
    - Buffer files that do not exist are kept (they may be the needed bracket
      and are already candidates for the missing-files report).
    - Static collections: if the file exists, open it and warn to stderr if
      its time axis does not fully cover [run_start, run_end].

    Returns the narrowed set of all needed files.
    """
    final = set()

    for (col_name, extrap), entry in per_collection.items():
        result     = entry["result"]
        collection = entry["collection"]

        # --- Static collections ---
        if result.is_static:
            final |= result.all_files
            for f in result.all_files:
                if os.path.exists(f):
                    _check_static_coverage(f, run_start, run_end, col_name)
            continue

        # --- Core files: always keep ---
        final |= result.core_files

        # --- Left buffer ---
        for f in result.left_buffer:
            if not os.path.exists(f):
                # File is missing — keep it (it belongs in the missing report)
                final.add(f)
                continue
            times = _safe_read_times(f, col_name)
            if times is None:
                # No time variable — keep conservatively
                final.add(f)
            elif any(t <= run_start for t in times):
                final.add(f)
            # else: file exists but contains no times <= run_start; drop it

        # --- Right buffer ---
        for f in result.right_buffer:
            if not os.path.exists(f):
                final.add(f)
                continue
            times = _safe_read_times(f, col_name)
            if times is None:
                final.add(f)
            elif any(t > run_end for t in times):
                final.add(f)
            # else: file exists but contains no times > run_end; drop it

    return final


def _safe_read_times(path: str, col_name: str):
    """Wrapper around read_file_times that emits a warning and returns None
    on any error rather than raising."""
    try:
        times = read_file_times(path)
        if times is None:
            print(
                f"WARNING: collection {col_name!r}: file {path!r} has no 'time' "
                f"variable — keeping conservatively.",
                file=sys.stderr,
            )
        return times
    except Exception as exc:
        print(
            f"WARNING: collection {col_name!r}: could not read times from "
            f"{path!r} ({exc}) — keeping conservatively.",
            file=sys.stderr,
        )
        return None


def _check_static_coverage(path: str, run_start: datetime, run_end: datetime,
                            col_name: str):
    """Open a static (no-token) file and warn if its time axis does not fully
    cover [run_start, run_end]."""
    times = _safe_read_times(path, col_name)
    if times is None:
        return
    if not times:
        print(
            f"WARNING: collection {col_name!r}: static file {path!r} has an "
            f"empty time axis — cannot confirm run range coverage.",
            file=sys.stderr,
        )
        return
    t_min, t_max = min(times), max(times)
    if t_min > run_start or t_max < run_end:
        print(
            f"WARNING: collection {col_name!r}: static file {path!r} time axis "
            f"[{t_min}, {t_max}] does not fully cover run range "
            f"[{run_start}, {run_end}].",
            file=sys.stderr,
        )


# ---------------------------------------------------------------------------
# Output helpers
# ---------------------------------------------------------------------------

def write_output(output_path, run_start: datetime, run_end: datetime, files: set):
    """Write the estimated (or narrowed) file list as YAML."""
    doc = {
        "run_start": run_start.strftime("%Y-%m-%dT%H:%M:%S"),
        "run_end":   run_end.strftime("%Y-%m-%dT%H:%M:%S"),
        "files":     sorted(files),
    }
    text = yaml.dump(doc, default_flow_style=False, sort_keys=False)
    if output_path:
        with open(output_path, "w") as f:
            f.write(text)
    else:
        sys.stdout.write(text)


def write_missing_output(
    output_path: str,
    run_start: datetime,
    run_end: datetime,
    missing: set,
):
    """Write the missing-files report as YAML."""
    doc = {
        "run_start":     run_start.strftime("%Y-%m-%dT%H:%M:%S"),
        "run_end":       run_end.strftime("%Y-%m-%dT%H:%M:%S"),
        "missing_files": sorted(missing),
    }
    with open(output_path, "w") as f:
        yaml.dump(doc, f, default_flow_style=False, sort_keys=False)


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def parse_args():
    p = argparse.ArgumentParser(
        description=(
            "Estimate, verify, and narrow the files needed by an ExtData component."
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    p.add_argument(
        "--config", required=True,
        help="Path to the ExtData YAML configuration file.",
    )
    p.add_argument(
        "--run_start", required=True,
        help="Run start time in ISO 8601 format (e.g. 2020-01-01T00:00:00).",
    )
    p.add_argument(
        "--run_end", required=True,
        help="Run end time in ISO 8601 format (e.g. 2020-12-31T18:00:00).",
    )
    p.add_argument(
        "--output", default=None,
        help="Output YAML file path for the estimated/narrowed file list. "
             "Defaults to stdout.",
    )
    p.add_argument(
        "--check", action="store_true", default=False,
        help="Check which estimated files exist on disk. "
             "Requires --missing_output.",
    )
    p.add_argument(
        "--narrow", action="store_true", default=False,
        help="Open bracketing files to tighten the file list. "
             "Implies --check. Requires --missing_output. "
             "Requires netCDF4.",
    )
    p.add_argument(
        "--missing_output", default=None,
        help="Path to write the missing-files YAML report. "
             "Required when --check or --narrow is given.",
    )
    return p.parse_args()


def main():
    args = parse_args()

    # --narrow implies --check
    if args.narrow:
        args.check = True

    # Validate
    if args.check and not args.missing_output:
        sys.exit("Error: --missing_output is required when using --check or --narrow")

    # Fail early if netCDF4 is needed but unavailable
    if args.narrow:
        try:
            import netCDF4  # noqa: F401
        except ImportError:
            sys.exit(
                "Error: --narrow requires the netCDF4 Python package, "
                "which could not be imported."
            )

    run_start = parse_iso8601(args.run_start)
    run_end   = parse_iso8601(args.run_end)
    if run_end < run_start:
        sys.exit("Error: --run_end is before --run_start")

    config = load_config(args.config)

    if args.narrow:
        per_collection = collect_all_files(
            config, run_start, run_end, return_per_collection=True
        )
        final_files = narrow_files(per_collection, run_start, run_end)
    else:
        final_files = collect_all_files(config, run_start, run_end)

    write_output(args.output, run_start, run_end, final_files)

    if args.check:
        _, missing = check_files_exist(final_files)
        write_missing_output(args.missing_output, run_start, run_end, missing)


if __name__ == "__main__":
    main()
