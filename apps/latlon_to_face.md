# latlon_to_face.py

Convert a cubed-sphere NetCDF file from tiled lat/lon format to face format.

---

## Overview

`latlon_to_face.py` converts a cubed-sphere data file whose horizontal axes
are laid out as a single flattened `(lat, lon)` grid — where `lat = 6 * lon`
— into the face-decomposed layout `(nf, Ydim, Xdim)` used by MAPL and GEOS
face-format readers.

A *template* face file is required to supply all grid coordinate variables and
grid-related global attributes.  The script copies data variables from the
input, grid metadata from the template, and merges global attributes.

---

## Background

Cubed-sphere data in GEOS/MAPL appears in two distinct layouts:

| Layout | Dimensions | When used |
|--------|-----------|-----------|
| Tiled lat/lon | `(lat = 6C, lon = C)` | History output, some restart files, regridder output |
| Face | `(nf = 6, Ydim = C, Xdim = C)` | ExtData input files, face-format restarts |

In the lat/lon layout the six cube faces are stacked along the `lat` axis, so a
C180 grid has `lat = 1080`, `lon = 180`.  In the face layout each face is an
explicit `nf` index with `Ydim × Xdim = C × C` spatial extent.

This script performs the reshape `lat(6C) × lon(C) → nf(6) × Ydim(C) × Xdim(C)`
for all data variables, replacing lat/lon coordinate variables with the cubed-
sphere grid coordinates taken from the template.

---

## Requirements

| Package | Purpose |
|---------|---------|
| `numpy` | Array reshape and squeeze/expand operations |
| `netCDF4` | Reading and writing NetCDF files |

---

## Usage

```bash
latlon_to_face.py \
    --input   <latlon_file.nc4> \
    --template <face_template.nc4> \
    --output  <output_face_file.nc4> \
    [--add_time | --remove_time]
```

`--add_time` and `--remove_time` are mutually exclusive.

---

## Arguments reference

| Flag | Required | Description |
|------|----------|-------------|
| `--input PATH` | Yes | Input NetCDF file in tiled lat/lon format (`lat = 6 * lon`) |
| `--template PATH` | Yes | Template NetCDF file in face format; provides grid metadata |
| `--output PATH` | Yes | Output NetCDF file in face format (NETCDF4) |
| `--add_time` | No | Add a `time` dimension to data variables that do not have one |
| `--remove_time` | No | Remove the `time` dimension from data variables (time var is still written) |

---

## What comes from where

| Element | Source | Notes |
|---------|--------|-------|
| Data variables (values) | Input | Reshaped `lat(6C)×lon(C) → nf(6)×Ydim(C)×Xdim(C)` |
| `lev` variable | Input | Copied unchanged |
| `time` variable | Input | Always copied; size must be 1 |
| `Xdim`, `Ydim`, `nf` | Template | Dimension coordinate variables |
| `ncontact`, `contacts`, `anchor` | Template | Cubed-sphere contact topology |
| `lons`, `lats`, `corner_lons`, `corner_lats` | Template | Cell-centre and corner coordinates |
| Dimension sizes (`nf`, `Xdim`, `Ydim`, `ncontact`, `XCdim`, `YCdim`) | Template | |
| Grid global attributes | Template | `Gridname`, `grid_mapping_name`, `file_format_version`, `additional_vars`, `gridspec_file` |
| Non-grid global attributes | Input | All other global attributes from the input file |

Global attributes are merged: the input's attributes are written first, then
any template attribute in `FACE_GRID_GLOBAL_ATTRS` is overlaid.  Template
attributes not in that set are added only if the input does not already have an
attribute with the same name.

Data variables are given two additional attributes in the output:
- `coordinates = "lons lats"` — points to the 2-D coordinate variables
- `grid_mapping = "cubed_sphere"` — declares the grid mapping

---

## Template file requirements

The template must be a valid MAPL face-format NetCDF file.  It must contain:

**Dimensions:**

| Dimension | Description |
|-----------|-------------|
| `nf` | Number of faces (must be 6) |
| `Xdim` | Face X size (must equal `lon` size of input) |
| `Ydim` | Face Y size (must equal `lon` size of input) |
| `ncontact` | Number of face-to-face contacts |
| `XCdim` | Corner X size (defaults to `Xdim + 1` if absent) |
| `YCdim` | Corner Y size (defaults to `Ydim + 1` if absent) |

**Variables:**

| Variable | Required | Description |
|----------|----------|-------------|
| `Xdim` | Yes | X-dimension coordinate |
| `Ydim` | Yes | Y-dimension coordinate |
| `nf` | Yes | Face-number coordinate |
| `lons` | Yes | 2-D cell-centre longitudes `(nf, Ydim, Xdim)` |
| `lats` | Yes | 2-D cell-centre latitudes `(nf, Ydim, Xdim)` |
| `corner_lons` | Recommended | Cell-corner longitudes |
| `corner_lats` | Recommended | Cell-corner latitudes |
| `contacts` | Recommended | Face contact descriptor strings |
| `anchor` | Recommended | Contact anchor points |

**Global attributes:**

| Attribute | Description |
|-----------|-------------|
| `Gridname` | Grid identifier (e.g. `PE180x1080-CF`) |
| `grid_mapping_name` | Must be `cubed_sphere` |
| `file_format_version` | MAPL grid-spec version string |

A suitable template can be obtained from any existing face-format file on the
target grid (e.g. a face-format restart or boundary condition file).

---

## Time dimension handling

| Mode | Behaviour |
|------|-----------|
| *(default)* | Time dimension is passed through unchanged for variables that have it; absent for variables that do not |
| `--add_time` | Inserts a size-1 `time` dimension as the leading axis of all data variables that do not already have one.  Errors if a variable already has `time`. |
| `--remove_time` | Squeezes the `time` axis out of all data variables.  The `time` variable is still written to the output.  Errors if a variable does not have a `time` dimension. |

The `time` variable is always written to the output (copied from the input)
regardless of `--add_time` / `--remove_time`.

---

## Limitations and assumptions

- **Single time step** — the input `time` dimension must have size 1.  Files
  with multiple time steps are not supported; the reshape is applied to the
  entire array as read, so the result would be incorrect for `len(time) > 1`.

- **Strict `lat = 6 * lon` check** — the script validates that `lat == 6 * lon`
  and exits with an error if not.  Files that use a different stacking order or
  a non-cubed-sphere grid are not supported.

- **Grid size must match** — the `lon` size of the input must equal the `Xdim`
  size of the template.  The script exits with an error if they differ.

- **`cubed_sphere` grid mapping** — the `grid_mapping` attribute written on
  data variables is hardcoded to `"cubed_sphere"`.  No `cubed_sphere` variable
  is written to the output; it is expected to be present in consumers that
  already understand MAPL face-format files.

- **Level coordinate** — `lev` is copied verbatim from the input.  Hybrid
  pressure coordinates (`lev`, `ilev`, `ak`, `bk`) beyond a plain `lev` are
  not handled automatically.
