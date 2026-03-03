# Vertical Alignment in MAPL3

## Overview

MAPL3 introduces explicit control over vertical coordinate alignment for fields with vertical dimensions. This feature allows components to specify how vertical data should be stored in memory, independent of the grid's coordinate direction.

## Background

### Coordinate Direction vs. Data Alignment

It's important to distinguish between two concepts:

1. **Coordinate Direction** (`coordinate_direction`): Describes the physical interpretation of vertical coordinates
   - `downward`: Coordinates increase moving down (e.g., pressure increasing with depth)
   - `upward`: Coordinates increase moving up (e.g., height increasing with altitude)

2. **Vertical Alignment** (`vertical_alignment`): Describes how data is stored in memory
   - `downward`: Data stored with first element at top/start of coordinate system
   - `upward`: Data stored with first element at bottom/end of coordinate system
   - `with_grid`: Data alignment follows the grid's coordinate direction (default)

### Motivation

Different atmospheric and oceanic models use different conventions for storing vertical data:

- **Atmospheric models** often use pressure coordinates (high pressure at surface, low at top)
  - Some store data top-to-bottom `[1 hPa, 10 hPa, 100 hPa, 1000 hPa]`
  - Others store bottom-to-top `[1000 hPa, 100 hPa, 10 hPa, 1 hPa]`

- **Ocean models** typically use depth coordinates (0 at surface, increasing downward)
  - Data usually stored surface-to-bottom `[0 m, 10 m, 100 m, 1000 m]`

The vertical alignment feature allows MAPL3 to handle data exchange between components with different storage conventions transparently.

## YAML Configuration

### Component Specification

Vertical alignment is specified in component YAML files for fields with vertical dimensions:

```yaml
mapl:
  geometry:
    vertical_grid:
      class: fixed_levels
      levels: [1000., 850., 500., 250., 100.]  # hPa
      units: hPa
      physical_dimension: pressure
      coordinate_direction: downward           # Physical interpretation

  states:
    export:
      temperature:
        standard_name: air_temperature
        units: K
        vertical_dim_spec: center
        vertical_alignment: upward              # Memory storage convention
```

### Alignment Options

The `vertical_alignment` field accepts three values:

#### 1. `with_grid` (Default)

Data alignment follows the grid's `coordinate_direction`. This is the default behavior if `vertical_alignment` is not specified.

**Example:**
```yaml
vertical_grid:
  levels: [1000., 850., 500., 250., 100.]
  coordinate_direction: downward

export:
  temperature:
    vertical_alignment: with_grid  # or omit for default
```

Memory layout: `[T(1000), T(850), T(500), T(250), T(100)]`

#### 2. `downward`

Data is always stored with the first coordinate value first, regardless of coordinate direction.

**Example:**
```yaml
vertical_grid:
  levels: [1000., 850., 500., 250., 100.]
  coordinate_direction: downward

export:
  temperature:
    vertical_alignment: downward
```

Memory layout: `[T(1000), T(850), T(500), T(250), T(100)]`

#### 3. `upward`

Data is always stored with the last coordinate value first (reversed order).

**Example:**
```yaml
vertical_grid:
  levels: [1000., 850., 500., 250., 100.]
  coordinate_direction: downward

export:
  temperature:
    vertical_alignment: upward
```

Memory layout: `[T(100), T(250), T(500), T(850), T(1000)]`

## Vertical Regridding with Alignment

When MAPL3 connects two components with different vertical grids or alignments, it automatically handles the necessary transformations.

### Same Grid, Different Alignments (Degenerate Case)

When source and destination use the same vertical grid but different alignments, MAPL3 detects this and performs only an array reversal without interpolation:

**Source Component A:**
```yaml
vertical_grid:
  levels: [1000., 850., 500.]
  coordinate_direction: downward

export:
  temperature:
    vertical_alignment: downward
```

**Destination Component B:**
```yaml
vertical_grid:
  levels: [1000., 850., 500.]  # Same grid!
  coordinate_direction: downward

import:
  temperature:
    vertical_alignment: upward   # Different alignment
```

**Transformation:** Simple array reversal, no interpolation needed.

### Different Grids with Alignments

When grids differ, MAPL3 performs vertical regridding while respecting alignments:

**Source Component A:**
```yaml
vertical_grid:
  levels: [1000., 850., 500., 250., 100.]
  coordinate_direction: downward

export:
  temperature:
    vertical_alignment: downward
```

**Destination Component B:**
```yaml
vertical_grid:
  levels: [900., 700., 400., 200.]
  coordinate_direction: downward

import:
  temperature:
    vertical_alignment: upward
```

**Transformation:**
1. Align source data to monotonic decreasing order for regridding
2. Perform vertical linear interpolation
3. Apply destination alignment transformation

## ExtData Integration

ExtData can read files with different vertical conventions using `vertical_alignment`:

```yaml
# extdata.yaml
Exports:
  atmospheric_pressure:
    collection: ERA5_pressure_levels
    variable: P
    vertical_alignment: upward  # ERA5 stores top-to-bottom

  ocean_temperature:
    collection: HYCOM_depth_levels
    variable: TEMP
    vertical_alignment: downward  # HYCOM stores surface-to-bottom
```

## Implementation Details

### Flip Detection

MAPL3 automatically detects when source and destination grids are identical but require alignment transformation:

1. Compare vertical coordinate arrays element-by-element
2. If coordinates match: no regridding needed, only alignment transform
3. If coordinates differ: perform full vertical regridding

The tolerance for coordinate comparison is set to ensure robust detection even with floating-point precision differences.

### Alignment Resolution

The alignment resolution algorithm determines the transformation needed:

```
source_flip = (source_alignment != grid_direction)
dest_flip = (dest_alignment != grid_direction)

if same_grid:
    if source_flip != dest_flip:
        perform array reversal
else:
    apply alignment transforms for regridding
```

## Examples

### Example 1: Atmospheric Model Coupling

Two atmospheric components using different vertical storage conventions:

**Component A (GCM):** Stores data bottom-to-top
```yaml
vertical_grid:
  levels: [1000., 850., 700., 500., 300., 100.]
  coordinate_direction: downward
export:
  U:
    vertical_alignment: upward  # Bottom-to-top storage
```

**Component B (Chemistry):** Stores data top-to-bottom
```yaml
vertical_grid:
  levels: [1000., 850., 700., 500., 300., 100.]  # Same grid
  coordinate_direction: downward
import:
  U:
    vertical_alignment: downward  # Top-to-bottom storage
```

**Result:** Automatic array reversal during data exchange.

### Example 2: Atmosphere-Ocean Coupling

Coupling atmospheric and ocean models with different coordinate systems:

**Atmosphere Component:**
```yaml
vertical_grid:
  levels: [1000., 925., 850., 700., 500., 300., 100.]
  units: hPa
  coordinate_direction: downward
  physical_dimension: pressure
export:
  air_temp:
    vertical_alignment: downward
```

**Ocean Component:**
```yaml
vertical_grid:
  levels: [0., 10., 50., 150., 500., 1500.]
  units: m
  coordinate_direction: downward
  physical_dimension: depth
import:
  sea_surface_temp:
    vertical_alignment: downward
```

**Note:** Different physical dimensions (pressure vs. depth) prevent direct coupling. A mediator component would be needed.

### Example 3: ExtData with Vertical Regridding

Reading external data and regridding to component's vertical grid:

**Component Configuration:**
```yaml
vertical_grid:
  levels: [1000., 850., 700., 500., 300., 100.]
  coordinate_direction: downward
import:
  ozone:
    vertical_alignment: downward
```

**ExtData Configuration:**
```yaml
Collections:
  MERRA2_3d:
    template: /path/to/MERRA2.%y4%m2%d2.nc4

Exports:
  ozone:
    collection: MERRA2_3d
    variable: O3
    vertical_alignment: upward  # MERRA2 stored top-to-bottom
```

**Result:** ExtData reads O3 with upward alignment, MAPL3 transforms to downward alignment with regridding if grid differs.

## Best Practices

1. **Specify alignment explicitly** for clarity, even when using default `with_grid`
2. **Document vertical conventions** in component README files
3. **Use consistent alignment** within a component for all vertical fields
4. **Test alignment scenarios** with integration tests (see `generic3g/tests/scenarios/vertical_alignment_*`)

## Constraints

### VerticalLinearMap Requirements

When vertical regridding is performed, the destination vertical grid must be fully contained within the source vertical grid range:

```
minval(destination_levels) >= minval(source_levels)
maxval(destination_levels) <= maxval(source_levels)
```

**Valid Example:**
- Source: `[1000, 850, 700, 500, 300, 100]` (range: 100-1000)
- Destination: `[900, 750, 600, 400, 200]` (range: 200-900) ✓

**Invalid Example:**
- Source: `[1000, 850, 700, 500, 300, 100]` (range: 100-1000)
- Destination: `[1050, 800, 500, 200, 50]` (range: 50-1050) ✗

## Testing

Integration tests for vertical alignment are provided in:
- `generic3g/tests/scenarios/vertical_alignment_same_grid/`
- `generic3g/tests/scenarios/vertical_alignment_regrid/`
- `generic3g/tests/scenarios/vertical_alignment_with_grid/`

Run tests with:
```bash
cd build/generic3g/tests
mpirun -np 1 ./MAPL.generic3g.tests -f vertical_alignment
```

## Related Files

### Implementation
- `generic3g/vertical/VerticalRegridTransform.F90` - Flip detection and regridding
- `generic3g/field_utils/FieldWithVerticalAlignment.F90` - Alignment data structure
- `generic3g/states/VerticalAlignmentEnumType.F90` - Enumeration type
- `hconfig_utils/HConfigUtils.F90` - YAML parsing for `vertical_alignment`

### Documentation
- `.opencode/plans/vertical-alignment-implementation.md` - Implementation plan
- This document

## References

- Issue #4405: Initial feature request
- PR #4410: Core implementation (Tasks 1-6)
- PR #4412: Integration tests (Task 7)
- Issue #4413: Documentation (Task 8)
