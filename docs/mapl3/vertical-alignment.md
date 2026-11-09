# Vertical Alignment in MAPL3

## Overview

MAPL3 lets a field with a vertical dimension declare how its vertical index is
ordered in memory, independent of the order of its vertical grid's levels. A
component states this with the `vertical_alignment` key of a state item. When
two connected items differ in storage order, MAPL3 reverses the vertical index
or folds the reversal into the vertical regridding weights.

The graph-native layer calls the same property *vertical orientation*
(`VerticalOrientationCharacteristic`); the YAML key and the legacy layer use
*alignment*.

## Background

### Grid Direction vs. Data Alignment

Two properties are easy to confuse:

1. **Grid direction**: the order of a vertical grid's level values. It is not
   a YAML key. A grid derives it, or has a default (see
   [Grid Direction](#grid-direction)).
   - `DOWN`: level values increase with index (e.g. `[1, 2, 3]`; pressure
     increasing toward the surface, or depth increasing with index). The index
     increases going down.
   - `UP`: level values decrease with index (e.g. `[3, 2, 1]`; pressure
     decreasing away from the surface). The index increases going up.

   A `fixed_levels` grid derives its direction from the numbers and from
   `physical_dimension`. A `height` or `altitude` grid increases going up, so
   `[0, 100, 500]` gives `UP` (see [Grid Direction](#grid-direction)); the
   "going down" and "going up" glosses above hold for pressure-like and
   depth-like coordinates.

2. **Vertical alignment** (`vertical_alignment`): the order of the field's
   vertical index in memory, relative to physical up and down.
   - `upward`: the first element is at the bottom; the index increases going up
     (the same order as an `UP` grid).
   - `downward`: the first element is at the top; the index increases going
     down (the same order as a `DOWN` grid).
   - `with_grid`: the field is stored in its grid's direction (the default).

A field is stored in grid order when its alignment resolves to its grid's
direction, and reversed relative to grid order when it resolves to the opposite
direction.

### Motivation

Different atmospheric and oceanic models use different conventions for storing
vertical data:

- **Atmospheric models** often use pressure coordinates (high pressure at the
  surface, low at the top)
  - Some store data top-to-bottom `[1 hPa, 10 hPa, 100 hPa, 1000 hPa]`
    (`downward`)
  - Others store bottom-to-top `[1000 hPa, 100 hPa, 10 hPa, 1 hPa]`
    (`upward`)

- **Ocean models** typically use depth coordinates (0 at the surface,
  increasing downward)
  - Data usually stored surface-to-bottom `[0 m, 10 m, 100 m, 1000 m]`
    (`downward`)

## YAML Configuration

### Component Specification

Vertical alignment is specified on state items that have a vertical dimension:

```yaml
mapl:
  geometry:
    vertical_grid:
      grid_type: fixed_levels                  # optional
      levels: [1000., 850., 500., 250., 100.]  # hPa; decreasing, so the grid direction is UP
      units: hPa
      physical_dimension: pressure

  states:
    export:
      temperature:
        standard_name: air_temperature
        units: K
        vertical_dim_spec: center
        vertical_alignment: upward              # Memory storage convention
```

### Grid Direction

The grid direction is not configured for a `fixed_levels` grid. A
`fixed_levels` `vertical_grid` block requires `levels` and
`physical_dimension`; `units` is optional and defaults per physical dimension
(`pressure` gives `Pa`, `height`, `altitude` and `depth` give `m`, `layer`
gives `1`, anything else `<unknown>`). The examples below that are complete
`vertical_grid` blocks include both required keys; the shorter snippets show
only the lines that matter and are abbreviated.

- A `fixed_levels` grid derives it from `levels` and `physical_dimension`. The
  levels must be strictly monotonic, or the grid is an error; a single level
  gives no direction. Otherwise:

  | `physical_dimension`                                   | increasing levels | decreasing levels |
  |--------------------------------------------------------|-------------------|-------------------|
  | `height`, `altitude`                                   | `UP`              | `DOWN`            |
  | `pressure`, `depth`, `layer`, any other name, or none  | `DOWN`            | `UP`              |

  The names are matched exactly and are case-sensitive, so `Height` is an
  "other name". For example, `physical_dimension: height` with
  `levels: [0., 100., 500.]` gives `UP`, and `levels: [500., 100., 0.]` gives
  `DOWN`. A `direction:` key in a `fixed_levels` grid is rejected, and a
  `coordinate_direction:` key is not read.

  **Changed meaning** (no warning is given). A multi-level `height` or
  `altitude` grid used to give `DOWN` for increasing levels. Two things change:

  - A YAML with such a grid and an explicit `vertical_alignment`: `upward` now
    means stored in grid order (it used to be reversed from the grid), and
    `downward` is reversed from it.
  - An item with `with_grid` alignment on such a grid now resolves to `UP`. A
    basic grid (direction `DOWN`) with the same number of levels counts as the
    same grid as a `fixed_levels` grid, in either order. Between an item on
    the height grid and an item on that basic grid, both the graph path
    (`VerticalColumnRegridTransform`) and the legacy path (`VerticalGridAspect`
    resolved alignments) now reverse the vertical index where they used to
    copy it.

  Review those alignments and connections. A `model` grid keeps its own
  direction (`DOWN` unless its `direction:` key says otherwise), so a
  height-valued `model` grid stays `DOWN` while a `fixed_levels` height grid
  is now `UP`.
- A `model` grid accepts an optional `direction:` key (`upward` or `downward`,
  or the short forms `up`, `u`, `down`, `d`, or `unspecified`); the default is
  `DOWN`.
- A basic grid (one defined only by `num_levels`) has direction `DOWN`.

### Alignment Options

The `vertical_alignment` field accepts `upward`, `downward` and `with_grid`
on both paths. The short forms `up`, `u`, `down`, `d` and `with-grid` are also
accepted, together with all-upper-case spellings (`UPWARD`, `UP`, `U`,
`DOWNWARD`, `DOWN`, `D`, `WITH_GRID`, `WITH-GRID`).

The graph path also accepts `mirror` (imports and expression exports only) and
lower-cases the value first, so mixed case such as `Upward` is accepted
there. An unknown value is an error on the graph path. The legacy path calls
the `VerticalAlignment` constructor directly on the written string, which does
not lower-case, so a mixed-case spelling or `mirror` gives an invalid
alignment there; what the legacy path does with an invalid alignment is not
documented here.

The examples below use one grid, `levels: [1000., 850., 500., 250., 100.]`,
whose direction is `UP`. Memory layout lists the level each stored element
holds, in index order.

#### 1. `with_grid` (Default)

The field is stored in its grid's direction. This is the default if
`vertical_alignment` is not specified.

**Example:**
```yaml
vertical_grid:
  levels: [1000., 850., 500., 250., 100.]  # direction UP

export:
  temperature:
    vertical_alignment: with_grid  # or omit for default
```

Memory layout: `[T(1000), T(850), T(500), T(250), T(100)]`

#### 2. `upward`

The first element is at the bottom. On an `UP` grid this is the grid's own
order.

**Example:**
```yaml
vertical_grid:
  levels: [1000., 850., 500., 250., 100.]  # direction UP

export:
  temperature:
    vertical_alignment: upward
```

Memory layout: `[T(1000), T(850), T(500), T(250), T(100)]`

On a `DOWN` grid, `upward` is the reverse of the grid order.

#### 3. `downward`

The first element is at the top. On an `UP` grid this is the reverse of the
grid order.

**Example:**
```yaml
vertical_grid:
  levels: [1000., 850., 500., 250., 100.]  # direction UP

export:
  temperature:
    vertical_alignment: downward
```

Memory layout: `[T(100), T(250), T(500), T(850), T(1000)]`

On a `DOWN` grid, `downward` is the grid order.

#### 4. `mirror` (imports and expression exports only)

The item takes the orientation of the export it is connected to, resolved to
`UP` or `DOWN`.

#### Defaults and omissions

- An item with no `vertical_alignment`, or `with_grid`, follows the grid it
  declares itself, or else the component's vertical grid.
- On the graph path, if the component has no vertical grid, the item has no
  vertical index to order and carries no orientation (the entry is dropped).
  The legacy path's handling is not documented here.
- An item with `vertical_dim_spec: none` has no vertical dimension and no
  orientation.
- A grid with a single level has no direction, so its fields are never reversed.

## Vertical Regridding with Alignment

When MAPL3 connects two components with different vertical grids or
alignments, the vertical-grid and orientation differences are handled
together.

### Same Grid, Different Alignments

Two grids count as the same when they have the same grid identity, or when one
of them is a basic grid with the same number of levels. Two `vertical_grid:`
blocks in different components create two different grids, even with identical
`levels`. A child component with the default (`from_parent`) geometry takes its
parent's vertical grid, so a parent and its children share one grid.

When the grids are the same and the two items' alignments resolve to different
directions, MAPL3 copies the data with the vertical index reversed and does not
interpolate. When they resolve to the same direction, nothing is done. The two
staggers must agree.

**Parent grid shared by children A and B (neither declares its own):**
```yaml
# parent
mapl:
  geometry:
    vertical_grid:
      levels: [1000., 850., 500.]   # direction UP
      physical_dimension: pressure
```

**Source Component A:**
```yaml
export:
  temperature:
    vertical_alignment: downward    # resolves to DOWN
```

**Destination Component B:**
```yaml
import:
  temperature:
    vertical_alignment: upward      # resolves to UP
```

**Transformation:** array reversal; no interpolation.

### Different Grids with Alignments

When the grids differ, MAPL3 performs vertical regridding and takes each
field's alignment into account.

**Source Component A:**
```yaml
vertical_grid:
  levels: [1000., 850., 500., 250., 100.]  # direction UP

export:
  temperature:
    vertical_alignment: downward    # stored [T(100), T(250), T(500), T(850), T(1000)]
```

**Destination Component B:**
```yaml
vertical_grid:
  levels: [900., 700., 400., 200.]         # direction UP

import:
  temperature:
    vertical_alignment: upward      # stored [T(900), T(700), T(400), T(200)]
```

**Transformation** (`VerticalColumnRegridTransform`):
1. Put each side's level values in that field's storage order. A's levels
   become `[100, 250, 500, 850, 1000]` (its alignment is opposite to its
   grid's direction); B's stay as they are.
2. Build linear interpolation weights between the two ordered level lists.
3. Apply the weights. The weights map stored data to stored data, so no data
   is reversed separately.

The graph-native `VerticalColumnRegridTransform` handles a Field only, in
single precision, with linear interpolation. When the grids differ, both
fields must be at level centers, the two grids must share a physical dimension
(for example `pressure`), and level values are converted to the destination's
units when the units differ. The same-grid copy only needs the two staggers to
agree. The legacy `VerticalRegridTransform` also accepts conservative
regridding.

## ExtData Integration

ExtData reads `vertical_alignment` from an export rule and stores it in the
rule (`ExtDataRule`). Nothing reads the stored value, so the key has no effect
on the data ExtData delivers.

## Implementation Details

### Grid Sameness

Both the legacy path and `VerticalColumnRegridTransform` decide whether two
grids are the same by comparing grid identities, and then by asking the source
grid whether it matches the destination grid. A `fixed_levels` or `model` grid
matches only a basic grid with the same number of layers; a basic grid matches
any grid with the same number of layers. Level values are not compared
element by element, and no tolerance is applied.

### Alignment Resolution

An alignment resolves to a direction against the item's grid:

```
resolve(with_grid, grid_direction) = grid_direction
resolve(upward,    grid_direction) = UP
resolve(downward,  grid_direction) = DOWN
```

Two directions are opposite only when one is `UP` and the other is `DOWN`;
a missing direction (single-level grid) is never opposite to anything.

```
dir_src = resolve(src_alignment, src_grid_direction)
dir_dst = resolve(dst_alignment, dst_grid_direction)

if same_grid:
    copy, reversed if opposite(dir_src, dir_dst)
else:
    order each side's level values as its field is stored:
        reverse the levels when opposite(grid_direction, resolved direction)
    interpolate between the ordered level lists
```

Destination levels outside the source range take the nearest end value, and
one warning per weight build is logged. The legacy transform asserts on
out-of-range levels in debug builds.

### Where the Work Is Done

- On the legacy path, `VerticalGridAspect` compares the two items and chooses
  `NullTransform` (same grid, same resolved direction) or
  `VerticalRegridTransform`.
- On the graph path, a `VerticalOrientation` mismatch and a `VerticalGrid`
  mismatch are detected, but their `build_transform` reports unsupported;
  `VerticalColumnRegridTransform` and `VerticalFlipTransform` exist with unit
  tests and are not yet built by any characteristic. See
  `docs/graph/ARCHITECTURE.md`.

## Examples

### Example 1: Atmospheric Model Coupling

Two atmospheric components sharing one vertical grid (both take the parent's)
and using different vertical storage conventions:

```yaml
# parent
vertical_grid:
  levels: [1000., 850., 700., 500., 300., 100.]   # direction UP
  physical_dimension: pressure
```

**Component A (GCM):** Stores data bottom-to-top
```yaml
export:
  U:
    vertical_alignment: upward  # Bottom-to-top storage
```

**Component B (Chemistry):** Stores data top-to-bottom
```yaml
import:
  U:
    vertical_alignment: downward  # Top-to-bottom storage
```

**Result:** The data is reversed during the exchange.

### Example 2: Atmosphere-Ocean Coupling

Atmospheric and ocean components with different coordinate systems:

**Atmosphere Component:**
```yaml
vertical_grid:
  levels: [1000., 925., 850., 700., 500., 300., 100.]  # direction UP
  units: hPa
  physical_dimension: pressure
export:
  air_temp:
    vertical_alignment: downward  # stored top-to-bottom, reversed from the grid
```

**Ocean Component:**
```yaml
vertical_grid:
  levels: [0., 10., 50., 150., 500., 1500.]            # direction DOWN
  units: m
  physical_dimension: depth
import:
  sea_surface_temp:
    vertical_alignment: downward  # stored surface-to-bottom, the grid order
```

**Note:** Different physical dimensions (pressure vs. depth) prevent direct
coupling; the two grids share no physical dimension. A mediator component would
be needed.

### Example 3: Height Grid

```yaml
vertical_grid:
  levels: [0., 100., 500.]      # direction UP: height increases going up
  units: m
  physical_dimension: height
export:
  temp_k:
    vertical_alignment: upward    # the grid order; no reversal
```

With `levels: [500., 100., 0.]` the direction is `DOWN`, and `upward` would
reverse the field.

## Best Practices

1. **Specify alignment explicitly** for clarity, even when using default `with_grid`
2. **Document vertical conventions** in component README files
3. **Use consistent alignment** within a component for all vertical fields
4. **Test alignment scenarios** with integration tests (see
   `superstructure/generic/tests/scenarios/vertical_alignment_*`)

## Constraints

### Vertical Regridding Requirements

Vertical regridding between different grids requires:

- both fields at level centers (an edge-staggered field does not regrid
  between different grids on the graph path);
- a physical dimension both grids support, with convertible units;
- level values from both grids, unless the grids are the same;
- linear interpolation on the graph path.

A destination level outside the source range is not an error on the graph path:
it takes the nearest end value and a warning is logged.

**Inside the range:**
- Source: `[1000, 850, 700, 500, 300, 100]` (range: 100-1000)
- Destination: `[900, 750, 600, 400, 200]` (range: 200-900): all interpolated

**Outside the range:**
- Source: `[1000, 850, 700, 500, 300, 100]` (range: 100-1000)
- Destination: `[1050, 800, 500, 200, 50]` (range: 50-1050): 1050 takes the
  value at 1000 and 50 takes the value at 100

## Testing

Integration tests for vertical alignment are provided in:
- `superstructure/generic/tests/scenarios/vertical_alignment_same_grid/`
- `superstructure/generic/tests/scenarios/vertical_alignment_regrid/`
- `superstructure/generic/tests/scenarios/vertical_alignment_with_grid/`

Unit tests for the graph transforms are in
`superstructure/generic/graph/tests/Test_VerticalColumnRegridTransform.pf`.

Run the scenarios from a build directory with:
```bash
ctest -R MAPL.generic.scenarios.shard
```
(`vertical_alignment_regrid` and `vertical_alignment_with_grid` are not yet
`graph_ready`; see `docs/graph/scenario-parity.md`.)

## Related Files

### Implementation
- `enums/VerticalAlignment.F90` - `VerticalAlignment` type, `VALIGN_UP/DOWN/WITH_GRID`, `resolve`
- `enums/VerticalCoordinateDirection.F90` - `VerticalCoordinateDirection` type
- `infrastructure/vertical/vertical_grid/VerticalGrid.F90` - grid direction accessors (default `DOWN`)
- `superstructure/generic/vertical/FixedLevelsVerticalGrid.F90` - direction derived from `levels` and `physical_dimension`
- `superstructure/generic/vertical/ModelVerticalGrid.F90` - optional `direction:` key
- `superstructure/generic/ComponentSpecParser/parse_var_specs.F90` - YAML parsing of `vertical_alignment`
- `superstructure/generic/specs/VariableSpec/make_characteristics.F90` - orientation characteristic from `vertical_alignment`
- `superstructure/generic/graph/characteristics/VerticalOrientationCharacteristic.F90` - the graph's orientation characteristic
- `superstructure/generic/graph/transforms/VerticalColumnRegridTransform.F90` - graph-native regrid, copy and reversal
- `superstructure/generic/graph/transforms/VerticalFlipTransform.F90` - graph-native index reversal
- `superstructure/generic/specs/VerticalGridAspect.F90` and `VerticalGridAspect/make_transform.F90` - legacy matching and transform choice
- `superstructure/generic/transforms/VerticalRegridTransform.F90` - legacy vertical regrid and flip
- `gridcomps/extdata/ExtDataRule.F90` - parses and stores `vertical_alignment`
