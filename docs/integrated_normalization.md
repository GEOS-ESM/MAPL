# MAPL3 Integrated Normalization Architecture

**Version:** 1.0  
**Date:** March 2026  
**Status:** Implemented

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Developer Guide](#developer-guide)
3. [Migration Guide](#migration-guide)
4. [Performance Characteristics](#performance-characteristics)
5. [Testing Approach](#testing-approach)

---

## Architecture Overview

### Motivation: Why Integrated Normalization?

#### The Problem with Separate Normalization Transforms

The original design treated normalization as separate aspect transforms that would create extension fields for each normalization operation:

```
Field [kg/kg]
  ↓ ExportNormalizationAspect → Extension 1
Field [kg/m²]
  ↓ GeomAspect → RegridTransform
Field [kg/m²] (new grid)
  ↓ ImportNormalizationAspect → Extension 2
Field [kg/kg] (new grid)
```

**Critical Inefficiency:** Even when import and export grids exactly match but normalization is required, the system would create two extension fields:

1. Extension 1: Transform to canonical normalization for regridding
2. Extension 2: Transform back from canonical normalization

**Result:** 3 fields in memory (original + 2 extensions) when conceptually only 1 is needed.

**Impact:** In common production scenarios where grids often match, this could nearly triple memory consumption for normalized fields and adds unnecessary transformation overhead.

#### The Solution: Normalization-Transparent Transforms

**Key Insight:** Regridding transforms should maintain the field's current normalization internally, using canonical normalization only temporarily during the actual regridding operation:

```
Field [kg/kg, with specific normalization]
  ↓
RegridTransform internally:
  1. Denormalize: [kg/kg] → [kg/m²] using DELP from vertical coord
  2. Perform horizontal conservative regrid on [kg/m²]
  3. Renormalize: [kg/m²] → [kg/kg] using DELP from new vertical coord
  ↓
Field [kg/kg, SAME normalization] on new grid (no extension created)
```

**Benefits:**
- **Memory efficiency:** No extra extensions when grids differ but normalization matches
- **Transparency:** Transforms maintain export normalization (input norm = output norm)
- **Consistency:** Both horizontal and vertical transforms use the same pattern
- **Simplicity:** Only one `NormalizationAspect` needed (replaces ExportNormalization + ImportNormalization)

### Core Design Principles

1. **Transforms are normalization-transparent**
   - `RegridTransform` and `VerticalRegridTransform` maintain whatever normalization the field has upon entry
   - Input normalization = Output normalization (transparency)

2. **Auxiliary fields from vertical coordinates**
   - DELP (pressure thickness) and DZ (height thickness) are computed from vertical coordinate fields
   - PLE (pressure level edges) and ZLE (height level edges) obtained via coupler pattern
   - No need for pre-computed auxiliary fields in State

3. **Conditional internal normalization**
   - Transforms only perform internal normalization when:
     - Regridding method is conservative, AND
     - Field has a normalization requirement (from NormalizationAspect)
   - Non-conservative methods (bilinear, patch) bypass normalization

4. **NormalizationAspect for true mismatches**
   - Separate `NormalizationAspect` transform only created when import and export normalizations genuinely differ
   - Example: Export wants `NORMALIZE_DELP`, Import wants `NORMALIZE_NONE`
   - This is rare in practice

### Architecture Components

#### NormalizationAspect

Single unified aspect that replaces the previous ExportNormalization and ImportNormalization aspects.

**Key responsibilities:**
- Store normalization metadata (type, scale factor, canonical units)
- Determine when normalizations truly mismatch
- Create `NormalizationTransform` only for true mismatches
- Return `NullTransform` when normalizations match

**Structure:**
```fortran
type, extends(StateItemAspect) :: NormalizationAspect
   private
   type(NormalizationMetadata) :: metadata
   character(:), allocatable :: source_units
   character(:), allocatable :: target_units
contains
   procedure :: matches
   procedure :: make_transform    ! Only creates transform for true mismatch
   procedure :: get_normalization_type
   procedure :: get_metadata
end type
```

#### RegridTransform with Integrated Normalization

Horizontal regridding transform that maintains normalization internally.

**Key features:**
- Optional coordinate field (PLE or ZLE) for normalization
- Optional coordinate coupler for extension fields
- Performs internal denorm → regrid → renorm sequence
- Transparent to caller (same normalization in/out)

**Constructor signature:**
```fortran
type(RegridTransform) :: transform

! Without normalization (non-conservative or NORMALIZE_NONE)
transform = RegridTransform(src_geom, dst_geom, regridder_param)

! With normalization (conservative with DELP/DZ normalization)
transform = RegridTransform(src_geom, dst_geom, regridder_param, &
                            coord_field, coord_coupler, norm_metadata)
```

**Internal operation flow:**
```fortran
subroutine update(this, importState, exportState, clock, rc)
   if (this%has_normalization) then
      ! 1. Denormalize using export coord field
      call denormalize(export_field, export_coord_field, norm_metadata)
      
      ! 2. Regrid in canonical units [kg/m²]
      call ESMF_FieldRegrid(denorm_field, regridded_field, routehandle)
      
      ! 3. Renormalize using import coord field  
      call renormalize(regridded_field, import_coord_field, norm_metadata)
   else
      ! Direct regrid, no normalization
      call ESMF_FieldRegrid(export_field, import_field, routehandle)
   end if
end subroutine
```

#### VerticalRegridTransform with Integrated Normalization

Vertical regridding transform that maintains normalization internally.

**Same pattern as RegridTransform:**
- Optional coordinate field for normalization
- Internal denorm → vertical regrid → renorm
- Transparent normalization handling

#### GeomAspect Integration

GeomAspect queries for normalization requirements and creates RegridTransform with appropriate configuration.

**Key logic:**
```fortran
function make_transform(src_geom, dst_geom, other_aspects, rc)
   ! Query if normalization needed
   norm_aspect = to_NormalizationAspect(other_aspects, status)
   needs_norm = (status == SUCCESS) .and. (norm_type /= NORMALIZE_NONE)
   
   ! Conservative method?
   is_conservative = (regrid_method == REGRID_METHOD_CONSERVE)
   
   if (needs_norm .and. is_conservative) then
      ! Get vertical grid and coordinate field
      vert_aspect = to_VerticalGridAspect(other_aspects, _RC)
      vert_grid => vert_aspect%get_vertical_grid(_RC)
      
      ! Get PLE or ZLE coordinate field with coupler
      physical_dim = get_physical_dimension(norm_type)
      coord_field = vert_grid%get_coordinate_field(physical_dim, &
                                                    other_aspects, &
                                                    coord_coupler, _RC)
      
      ! Create transform with normalization support
      transform = RegridTransform(src_geom, dst_geom, regridder_param, &
                                  coord_field, coord_coupler, &
                                  norm_aspect%metadata)
   else
      ! No normalization needed
      transform = RegridTransform(src_geom, dst_geom, regridder_param)
   end if
end function
```

### Information Flow

#### Scenario 1: Matching Grids, No Normalization

```
Export: Field [kg/kg] on 4x4 grid, NORMALIZE_NONE
Import: Field [kg/kg] on 4x4 grid, NORMALIZE_NONE

GeomAspect::matches() → TRUE (grids match)
No transform created, no extensions
Result: 1 field in memory
```

#### Scenario 2: Different Grids, No Normalization

```
Export: Field [kg/kg] on 4x4 grid, NORMALIZE_NONE  
Import: Field [kg/kg] on 8x8 grid, NORMALIZE_NONE

GeomAspect::matches() → FALSE (grids differ)
GeomAspect::make_transform() → RegridTransform (no normalization)
Extension created: regridded field [kg/kg] on 8x8 grid
Result: 2 fields in memory (1 primary + 1 extension)
```

#### Scenario 3: Different Grids, With Normalization (Integrated)

```
Export: Field [kg/kg] on 4x4 grid, NORMALIZE_DELP
Import: Field [kg/kg] on 8x8 grid, NORMALIZE_DELP

GeomAspect::matches() → FALSE (grids differ)
NormalizationAspect::matches() → TRUE (same normalization)
GeomAspect::make_transform() → RegridTransform with integrated normalization

Transform internally:
  1. Get PLE_export from vertical grid
  2. Denormalize: [kg/kg] → [kg/m²] using PLE_export
  3. Regrid: [kg/m²]_4x4 → [kg/m²]_8x8
  4. Get PLE_import from vertical grid
  5. Renormalize: [kg/m²] → [kg/kg] using PLE_import

Extension created: field [kg/kg] on 8x8 grid (SAME normalization)
Result: 2 fields in memory (1 primary + 1 extension)
```

#### Scenario 4: Matching Grids, Different Normalization (Rare)

```
Export: Field [kg/kg] on 4x4 grid, NORMALIZE_DELP
Import: Field [kg/kg] on 4x4 grid, NORMALIZE_NONE

GeomAspect::matches() → TRUE (grids match)
NormalizationAspect::matches() → FALSE (normalizations differ)
NormalizationAspect::make_transform() → NormalizationTransform

Extension created: field denormalized [kg/m²] on 4x4 grid
Result: 2 fields in memory (1 primary + 1 extension for normalization)
```

### Memory Efficiency Comparison

#### Old Approach (Separate Normalization Transforms)

**Scenario:** Different grids + Conservative regridding with DELP normalization

```
1. Original field: [kg/kg]_4x4 with NORMALIZE_DELP
2. Extension 1 (Export normalization): [kg/m²]_4x4 (canonical)
3. Extension 2 (Regrid): [kg/m²]_8x8 (canonical on new grid)
4. Extension 3 (Import denormalization): [kg/kg]_8x8 with NORMALIZE_DELP

Total: 4 fields in memory (1 primary + 3 extensions)
```

#### New Approach (Integrated Normalization)

**Same scenario:**

```
1. Original field: [kg/kg]_4x4 with NORMALIZE_DELP
2. Extension 1 (Regrid with integrated norm): [kg/kg]_8x8 with NORMALIZE_DELP
   (Internal temporary [kg/m²] fields not kept in memory)

Total: 2 fields in memory (1 primary + 1 extension)
```

**Memory savings: 50% reduction** (4 fields → 2 fields)

---

## Developer Guide

### Using NormalizationAspect

#### Creating a Field with Normalization

```fortran
use mapl3g_NormalizationAspect
use mapl3g_NormalizationType

type(StateItemSpec) :: spec
type(NormalizationAspect) :: norm_aspect

! Create normalization aspect for DELP-normalized mixing ratio
norm_aspect = NormalizationAspect( &
   normalization_type=NORMALIZE_DELP, &
   scale_factor=1.0_REAL64, &
   canonical_units='kg m-2')

! Add to spec
call spec%set_aspect(norm_aspect, _RC)
```

#### Querying Normalization Requirements

```fortran
type(NormalizationAspect) :: norm_aspect
integer :: status

! Query if field has normalization
norm_aspect = to_NormalizationAspect(spec%get_aspects(), status)

if (status == SUCCESS) then
   norm_type = norm_aspect%get_normalization_type()
   
   select case (norm_type)
   case (NORMALIZE_DELP)
      ! Need pressure thickness (DELP)
      physical_dim = 'pressure'
   case (NORMALIZE_DZ)
      ! Need height thickness (DZ)
      physical_dim = 'height'
   case (NORMALIZE_NONE)
      ! No normalization
   end select
end if
```

#### Implementing a Transform with Integrated Normalization

**Pattern to follow:**

```fortran
type :: MyTransform
   private
   logical :: has_normalization = .false.
   type(ESMF_Field) :: coord_field              ! PLE or ZLE
   type(FieldCoupler) :: coord_coupler          ! For extension coord fields
   type(NormalizationMetadata) :: norm_metadata
contains
   procedure :: update
end type

function constructor(args, coord_field, coord_coupler, norm_metadata)
   if (present(coord_field)) then
      this%has_normalization = .true.
      this%coord_field = coord_field
      this%coord_coupler = coord_coupler
      this%norm_metadata = norm_metadata
   end if
end function

subroutine update(this, importState, exportState, clock, rc)
   if (this%has_normalization) then
      ! 1. Get export coordinate field
      export_coord = this%coord_field
      
      ! 2. Denormalize export field
      call denormalize(export_field, export_coord, this%norm_metadata, _RC)
      
      ! 3. Perform main operation on canonical field
      call my_operation(denormalized_field, result_field, _RC)
      
      ! 4. Get import coordinate field (possibly different grid)
      import_coord = this%coord_coupler%couple(export_coord, _RC)
      
      ! 5. Renormalize to maintain export normalization
      call renormalize(result_field, import_coord, this%norm_metadata, _RC)
   else
      ! Direct operation, no normalization
      call my_operation(export_field, import_field, _RC)
   end if
end subroutine
```

### Adding New Normalization Types

#### Step 1: Define the normalization type constant

```fortran
! In NormalizationType.F90
integer, parameter :: NORMALIZE_MY_TYPE = 4
```

#### Step 2: Add metadata support

```fortran
! In NormalizationMetadata.F90
type(NormalizationMetadata) :: metadata

select case (norm_type)
case (NORMALIZE_MY_TYPE)
   metadata = NormalizationMetadata( &
      normalization_type=NORMALIZE_MY_TYPE, &
      scale_factor=1.0_REAL64, &
      canonical_units='kg m-2', &
      physical_dimension='my_dimension')
end select
```

#### Step 3: Implement normalization/denormalization

```fortran
! Add cases to normalization helper functions
subroutine normalize(field, coord_field, metadata, rc)
   select case (metadata%normalization_type)
   case (NORMALIZE_MY_TYPE)
      ! Implement: field = field * coord_field / scale_factor
      call my_normalize_operation(field, coord_field, _RC)
   end select
end subroutine
```

#### Step 4: Update aspect to recognize new type

```fortran
! In NormalizationAspect.F90
function supports_conversion_specific(this, other, rc)
   ! Define when MY_TYPE can convert to/from other types
   select case (this%metadata%normalization_type)
   case (NORMALIZE_MY_TYPE)
      ! Can convert to NORMALIZE_NONE
      supports = (other_type == NORMALIZE_NONE)
   end select
end function
```

### Debugging Tips

#### Verify Normalization is Active

Add debug output to confirm normalization is being applied:

```fortran
if (this%has_normalization) then
   print *, 'DEBUG: Applying normalization type:', this%norm_metadata%normalization_type
   print *, 'DEBUG: Coord field range:', minval(coord_data), maxval(coord_data)
end if
```

#### Check Mass Conservation

For conservative regridding with normalization:

```fortran
! Before regrid
mass_before = sum(field_data * delp_data * area_data)

! After regrid
mass_after = sum(regridded_data * delp_new_data * area_new_data)

rel_error = abs(mass_after - mass_before) / mass_before
print *, 'Mass conservation error:', rel_error
```

#### Verify Extension Field Count

Check that integrated normalization reduces extensions:

```fortran
type(ExtensionFamily), pointer :: family

family => registry%get_extension_family(export_pt, _RC)
num_extensions = family%num_variants()

! Should be: 1 (primary) + 1 (regrid extension)
! NOT: 1 (primary) + 3 (norm + regrid + denorm)
print *, 'Number of variants:', num_extensions
```

#### Common Issues

**Issue: Normalization not applied when expected**
- Check that regrid method is conservative
- Verify NormalizationAspect is set on spec
- Confirm normalization_type /= NORMALIZE_NONE
- Check that vertical grid provides coordinate field

**Issue: Mass not conserved**
- Verify coordinate field (PLE/ZLE) is correct
- Check that same vertical coordinate system used for export and import
- Confirm scale factor is correct (usually 1.0)
- Verify regridding method is truly conservative

**Issue: Too many extensions created**
- Verify transforms have `has_normalization = .true.`
- Check that coordinate field and coupler are passed to transform constructor
- Confirm NormalizationAspect::matches() returns TRUE when normalizations match

---

## Migration Guide

### Changes from Original Plan

The [Conservative Regridding Implementation Plan](../.opencode/plans/CONSERVATIVE_REGRIDDING_IMPLEMENTATION_PLAN.md) originally proposed separate ExportNormalization and ImportNormalization aspects that would create explicit extension fields for normalization operations. This approach has been replaced with **integrated normalization**.

#### Key Differences

| Original Design | Integrated Normalization |
|----------------|-------------------------|
| ExportNormalization + ImportNormalization aspects | Single NormalizationAspect |
| Separate normalization transforms | Normalization integrated into RegridTransform |
| 3 extensions for norm+regrid+denorm | 1 extension for regrid (norm internal) |
| Always creates normalization extensions | Only creates extension for true mismatch |
| Vertical coord fields pre-computed | Vertical coord fields obtained dynamically |

### Code Migration

#### Aspect Naming

**Old:**
```fortran
use mapl3g_ExportNormalization
use mapl3g_ImportNormalization

type(ExportNormalization) :: export_norm
type(ImportNormalization) :: import_norm
```

**New:**
```fortran
use mapl3g_NormalizationAspect

type(NormalizationAspect) :: norm_aspect
```

#### Aspect Creation

**Old:**
```fortran
! Export side
export_norm = ExportNormalization( &
   normalization_type=NORMALIZE_DELP, &
   scale_factor=1.0_REAL64)
call export_spec%set_aspect(export_norm, _RC)

! Import side  
import_norm = ImportNormalization( &
   normalization_type=NORMALIZE_DELP, &
   scale_factor=1.0_REAL64, &
   is_inverse=.true.)
call import_spec%set_aspect(import_norm, _RC)
```

**New:**
```fortran
! Both export and import use same NormalizationAspect
norm_aspect = NormalizationAspect( &
   normalization_type=NORMALIZE_DELP, &
   scale_factor=1.0_REAL64, &
   canonical_units='kg m-2')

call export_spec%set_aspect(norm_aspect, _RC)
call import_spec%set_aspect(norm_aspect, _RC)
```

#### Aspect IDs

**Old:**
```fortran
EXPORT_NORMALIZATION_ASPECT_ID
IMPORT_NORMALIZATION_ASPECT_ID
```

**New:**
```fortran
NORMALIZATION_ASPECT_ID
```

#### Creating Transforms

**Old approach** (not actually implemented):
```fortran
! Would have created separate normalization transform
norm_transform = ExportNormalization%make_transform(...)
regrid_transform = GeomAspect%make_transform(...)
denorm_transform = ImportNormalization%make_transform(...)
```

**New approach:**
```fortran
! GeomAspect creates RegridTransform with integrated normalization
transform = GeomAspect%make_transform(src_spec, dst_spec, other_aspects, _RC)

! Transform internally handles:
! - Querying NormalizationAspect
! - Getting coordinate field from VerticalGrid
! - Denorm → Regrid → Renorm sequence
```

### Behavioral Changes

#### Extension Field Creation

**Old behavior** (theoretical):
- Grid mismatch + conservative + DELP normalization → 3 extensions
- Extension 1: normalized field [kg/m²]
- Extension 2: regridded field [kg/m²]  
- Extension 3: denormalized field [kg/kg]

**New behavior** (implemented):
- Grid mismatch + conservative + DELP normalization → 1 extension
- Extension 1: regridded field [kg/kg] with same normalization
- Internal temporary fields not kept in memory

#### Normalization Transparency

**New behavior guarantee:**
```
Input normalization = Output normalization
```

Transforms maintain the export field's normalization throughout the operation. There is no change to units or normalization state visible to the caller.

#### When NormalizationTransform is Created

**Old:** Would have been created for every normalized field

**New:** Only created when normalizations genuinely mismatch:
- Export has NORMALIZE_DELP, Import wants NORMALIZE_NONE → NormalizationTransform created
- Export has NORMALIZE_DELP, Import has NORMALIZE_DELP → No NormalizationTransform (handled internally by RegridTransform)

### Testing Migration

#### Test Structure

**Old test pattern** (not implemented):
```fortran
! Would have tested separate normalization transform
@Test
subroutine test_export_normalization()
   ! Create ExportNormalization transform
   ! Verify field [kg/kg] → [kg/m²]
end subroutine

@Test  
subroutine test_import_denormalization()
   ! Create ImportNormalization transform  
   ! Verify field [kg/m²] → [kg/kg]
end subroutine
```

**New test pattern:**
```fortran
! Test integrated normalization within transforms
@Test
subroutine test_conservative_regrid_with_normalization()
   ! Create RegridTransform with normalization
   ! Verify:
   ! - Input field [kg/kg]
   ! - Output field [kg/kg] (same normalization)
   ! - Mass conserved
end subroutine

@Test
subroutine test_memory_efficiency()
   ! Verify only 1 extension created, not 3
   num_extensions = family%num_variants()
   @assertEqual(2, num_extensions)  ! 1 primary + 1 extension
end subroutine
```

#### New Test Utilities

**Mass calculation helper:**
```fortran
function calculate_total_mass(data, delp, area) result(total_mass)
   real(ESMF_KIND_R8), intent(in) :: data(:,:,:)
   real(ESMF_KIND_R4), intent(in) :: delp(:,:,:)
   real(ESMF_KIND_R4), intent(in) :: area(:,:)
   real(ESMF_KIND_R8) :: total_mass
   
   ! mass = sum(q * Δp * area) / g
   total_mass = sum(real(data, ESMF_KIND_R8) * &
                    real(delp, ESMF_KIND_R8) * &
                    spread(real(area, ESMF_KIND_R8), 3, size(data,3))) / GRAVITY
end function
```

**Extension counting:**
```fortran
! Get extension family and count variants
family => src_registry%get_extension_family(export_pt, _RC)
num_extensions = family%num_variants()

! num_variants() returns: 1 (primary) + number of extensions
! Expected for integrated norm: 2 (1 primary + 1 regrid extension)
```

---

## Performance Characteristics

### Memory Savings

#### Common Case: Different Grids, Same Normalization

**Scenario:** 4x4 grid → 8x8 grid, both NORMALIZE_DELP, conservative regrid

**Old approach (theoretical):**
- Primary field: 4×4×10 = 160 grid points
- Extension 1 (norm): 4×4×10 = 160 grid points
- Extension 2 (regrid): 8×8×10 = 640 grid points
- Extension 3 (denorm): 8×8×10 = 640 grid points
- **Total: 1600 grid points** (1 primary + 3 extensions)

**New approach (implemented):**
- Primary field: 4×4×10 = 160 grid points
- Extension 1 (regrid with integrated norm): 8×8×10 = 640 grid points
- **Total: 800 grid points** (1 primary + 1 extension)

**Memory savings: 50%** (1600 → 800 grid points)

#### Best Case: Matching Grids, With Normalization

**Scenario:** 4x4 grid → 4x4 grid (same), NORMALIZE_DELP

**Old approach:** Would still create normalization extensions (though no regrid)
**New approach:** No extensions at all (grids match)

**Memory savings: Maximum** (no extensions created)

#### Worst Case: True Normalization Mismatch

**Scenario:** Export NORMALIZE_DELP, Import NORMALIZE_NONE

**Both approaches:** Create 1 extension for normalization conversion

**Memory savings: 0%** (same memory usage)

### Computational Overhead

#### Integrated Normalization Cost

**Operations per grid point:**
1. Denormalize: 1 multiply + 1 divide ≈ 2 FLOPs
2. Regrid: Cost depends on method (conservative is expensive anyway)
3. Renormalize: 1 multiply + 1 divide ≈ 2 FLOPs

**Total overhead: ~4 FLOPs per grid point**

**Context:** Conservative regridding itself involves hundreds of FLOPs per grid point (sparse matrix operations, area computations, etc.). The normalization overhead is negligible (~1-2% of total cost).

#### When Normalization is Skipped

Normalization operations are completely bypassed when:

1. **Non-conservative regrid methods** (bilinear, patch, nearest neighbor)
   - No normalization needed since these don't preserve integrals
   - Zero overhead

2. **NORMALIZE_NONE fields**
   - Fields with no normalization requirement
   - Zero overhead

3. **Matching grids**
   - No regridding needed at all
   - Zero overhead

### Scalability

#### Parallel Efficiency

Integrated normalization is embarrassingly parallel:
- Denormalization: Local operation on each grid point
- Regridding: Already parallel (ESMF routehandles)
- Renormalization: Local operation on each grid point

**No communication overhead** added by normalization.

#### Load Balancing

Normalization operations are uniformly distributed:
- Same cost per grid point
- No hotspots or irregular work patterns
- Excellent load balance

### Memory Access Patterns

#### Cache Efficiency

**Good:**
- Denormalization: Stream through field and coord arrays together
- Good spatial locality
- Predictable memory access pattern

**Could be better:**
- Separate denorm/renorm steps could cause extra memory traffic
- Future optimization: Fuse operations in regridding kernels

#### Memory Bandwidth

For a 3D field (nx × ny × nz):
- Denorm: Read field + coord, write denorm field = 3 arrays
- Regrid: Read denorm, write regrid = 2 arrays  
- Renorm: Read regrid + coord, write final = 3 arrays
- **Total: 8 array passes**

For comparison, old approach with separate extensions:
- Would need more memory allocation/deallocation
- More ESMF State operations
- More field creation overhead

**Net benefit:** Integrated approach reduces metadata operations and memory management overhead.

---

## Testing Approach

### Verification Strategy

#### Test Pyramid

1. **Unit Tests** (Fast, many)
   - NormalizationAspect creation
   - Metadata queries
   - Aspect matching logic
   - Helper function correctness

2. **Integration Tests** (Medium, several)
   - RegridTransform with integrated normalization
   - VerticalRegridTransform with integrated normalization
   - Full transform pipeline tests
   - Memory efficiency verification

3. **System Tests** (Slow, few)
   - Full gridded component with normalization
   - Multi-stage regridding (horizontal + vertical)
   - Real atmospheric fields

### Test Scenarios

#### Scenario 1: Horizontal Conservative Regrid with Mass Conservation

**Purpose:** Verify conservative horizontal regridding preserves total mass when using DELP normalization.

**Setup:**
- Source: 4×4 grid, 10 levels
- Target: 8×8 grid, 10 levels
- Field: Uniform CO₂ mixing ratio (400 ppm)
- Normalization: NORMALIZE_DELP
- Method: Conservative

**Verification:**
```fortran
! Calculate total mass before and after
mass_in = sum(q_in * delp_in * area_in) / g
mass_out = sum(q_out * delp_out * area_out) / g

rel_error = abs(mass_out - mass_in) / mass_in
@assertTrue(rel_error < 1.0e-5)
```

**Implementation:** `Test_IntegratedNormalization.pf::test_horizontal_conservative_mass_conservation`

#### Scenario 2: Non-Conservative Regrid (No Normalization)

**Purpose:** Verify non-conservative methods bypass normalization correctly.

**Setup:**
- Source: 4×4 grid
- Target: 8×8 grid  
- Method: Bilinear
- Normalization: NORMALIZE_NONE

**Verification:**
```fortran
! Should not invoke normalization code path
! Verify transform created without coord_field
@assertFalse(transform%has_normalization)
```

**Implementation:** `Test_IntegratedNormalization.pf::test_bilinear_no_normalization`

#### Scenario 3: Full 3D Pipeline

**Purpose:** Test horizontal + vertical regridding with normalization.

**Setup:**
- Horizontal: 4×4 → 6×6
- Vertical: 10 levels → 15 levels
- Both conservative with NORMALIZE_DELP

**Verification:**
```fortran
! Mass conserved through full pipeline
mass_final = sum(q_final * delp_final * area_final) / g
rel_error = abs(mass_final - mass_initial) / mass_initial
@assertTrue(rel_error < 1.0e-5)
```

**Implementation:** `Test_IntegratedNormalization.pf::test_3d_pipeline_mass_conservation`

#### Scenario 4: Memory Efficiency - Matching Grids

**Purpose:** Verify no extensions created when grids match.

**Setup:**
- Source: 4×4 grid, NORMALIZE_DELP
- Target: 4×4 grid (same), NORMALIZE_DELP

**Verification:**
```fortran
family => registry%get_extension_family(export_pt, _RC)
num_variants = family%num_variants()

! Should be 1 (just primary spec, no extensions)
@assertEqual(1, num_variants)
```

**Implementation:** `Test_IntegratedNormalization.pf::test_memory_efficiency_matching_grids`

#### Scenario 5: Memory Efficiency - Different Grids

**Purpose:** Verify only 1 extension created (not 3) when grids differ.

**Setup:**
- Source: 4×4 grid, NORMALIZE_DELP
- Target: 8×8 grid, NORMALIZE_DELP

**Verification:**
```fortran
num_variants = family%num_variants()

! Should be 2 (primary + 1 regrid extension)
! NOT 4 (primary + 3 for norm + regrid + denorm)
@assertEqual(2, num_variants)
```

**Implementation:** `Test_IntegratedNormalization.pf::test_memory_efficiency_different_grids`

### Mass Conservation Verification

#### Formula

For a mixing ratio field `q` [kg/kg] with pressure normalization:

```
Total Mass [kg] = sum(q * Δp * Area) / g
```

Where:
- `q`: Mixing ratio [kg/kg]
- `Δp`: Pressure thickness [Pa] (from DELP)
- `Area`: Grid cell area [m²]
- `g`: Gravitational acceleration [m/s²]

#### Helper Function

```fortran
function calculate_total_mass(data, delp, area) result(total_mass)
   real(ESMF_KIND_R8), intent(in) :: data(:,:,:)   ! Mixing ratio [kg/kg]
   real(ESMF_KIND_R4), intent(in) :: delp(:,:,:)   ! Pressure thickness [Pa]
   real(ESMF_KIND_R4), intent(in) :: area(:,:)     ! Grid cell area [m²]
   real(ESMF_KIND_R8) :: total_mass
   
   total_mass = sum(real(data, ESMF_KIND_R8) * &
                    real(delp, ESMF_KIND_R8) * &
                    spread(real(area, ESMF_KIND_R8), 3, size(data,3))) / GRAVITY
end function
```

#### Tolerance Selection

**Strict tolerance** (`1.0e-10`): For operations that should be bit-identical
- Copying fields
- Identity transforms
- Same-grid operations

**Loose tolerance** (`1.0e-5`): For regridding operations
- Conservative regridding introduces discretization errors
- Typical relative error: 1e-6 to 1e-8
- Use 1e-5 to allow margin for rounding

### Normalization Transparency Testing

#### Verify Input = Output Normalization

```fortran
@Test
subroutine test_normalization_transparency()
   ! Get normalization before transform
   norm_before = get_normalization_aspect(export_spec, _RC)
   
   ! Apply transform
   call transform%update(importState, exportState, clock, _RC)
   
   ! Get normalization after transform
   norm_after = get_normalization_aspect(import_spec, _RC)
   
   ! Should match
   @assertEqual(norm_before%get_normalization_type(), &
                norm_after%get_normalization_type())
end subroutine
```

### Memory Profiling Techniques

#### Extension Field Counting

```fortran
! Query extension family
family => registry%get_extension_family(export_pt, _RC)

! Count total variants (primary + extensions)
num_variants = family%num_variants()

! Expected values:
! - Matching grids: 1 (just primary)
! - Different grids: 2 (primary + 1 regrid extension)
! - Old approach would have been: 4 (primary + 3 extensions)
```

#### Memory Instrumentation

For detailed memory tracking:

```fortran
! Before connection
call get_memory_usage(mem_before, _RC)

! Create connection and transform
call connection%connect(registry, _RC)

! After connection  
call get_memory_usage(mem_after, _RC)

! Calculate memory increase per field
mem_per_field = (mem_after - mem_before) / num_fields

! Compare against expected
expected_mem = primary_field_size + extension_field_size
@assertTrue(abs(mem_per_field - expected_mem) < tolerance)
```

### Debugging Failing Tests

#### Mass Conservation Failures

**Check:**
1. Is coordinate field (PLE/ZLE) correct?
   ```fortran
   call ESMF_FieldGet(coord_field, localDe=0, farrayPtr=coord_data, _RC)
   print *, 'Coord range:', minval(coord_data), maxval(coord_data)
   ```

2. Are areas computed correctly?
   ```fortran
   call ESMF_FieldGet(area_field, localDe=0, farrayPtr=area_data, _RC)
   print *, 'Area range:', minval(area_data), maxval(area_data)
   print *, 'Total area:', sum(area_data)
   ```

3. Is DELP calculation correct?
   ```fortran
   ! DELP should be PLE(k+1) - PLE(k)
   delp(:,:,k) = ple(:,:,k+1) - ple(:,:,k)
   @assertTrue(all(delp > 0), 'DELP must be positive')
   ```

#### Extension Count Failures

**Check:**
1. Are specs complete (have all necessary aspects)?
   ```fortran
   ! Need both geom and vertical_grid to avoid mirror transforms
   @assertTrue(spec%has_aspect(GEOM_ASPECT_ID))
   @assertTrue(spec%has_aspect(VERTICAL_GRID_ASPECT_ID))
   ```

2. Did transform receive normalization parameters?
   ```fortran
   @assertTrue(transform%has_normalization)
   @assertTrue(associated(transform%coord_field))
   ```

3. Are normalizations actually matching?
   ```fortran
   export_norm = get_normalization(export_spec, _RC)
   import_norm = get_normalization(import_spec, _RC)
   @assertTrue(export_norm%matches(import_norm))
   ```

---

## Summary

### Key Takeaways

1. **Integrated normalization** makes transforms normalization-transparent
2. **Memory efficiency**: Reduces extensions from 4 → 2 in common cases
3. **Single NormalizationAspect** replaces Export/Import split
4. **Auxiliary fields from vertical coordinates** obtained dynamically
5. **Conservative regridding** preserves mass with proper normalization

### Implementation Status

- ✅ Task 1: NormalizationAspect consolidation (PR #4535)
- ✅ Task 2: RegridTransform integrated normalization (PR #4536)
- ✅ Task 3: GeomAspect enhancements (PR #4538)
- ✅ Task 4: VerticalRegridTransform enhancements (PR #4539)
- ✅ Task 5: Integration testing (PR #4546)
- ✅ Task 6: Aspect registry update (completed)
- ✅ Task 7: Documentation (this document)

### Future Work

Potential enhancements (not currently planned):

1. **Fused normalization kernels**: Combine denorm/regrid/renorm into single kernel for better cache utilization
2. **Additional normalization types**: Support for column-integrated quantities, surface-normalized fields, etc.
3. **Adaptive normalization**: Automatically select best normalization based on field characteristics
4. **Performance profiling**: Detailed timing breakdown of normalization overhead

---

## References

- [Conservative Regridding Implementation Plan](../.opencode/plans/CONSERVATIVE_REGRIDDING_IMPLEMENTATION_PLAN.md)
- [Integrated Normalization Implementation Plan](../.opencode/plans/INTEGRATED_NORMALIZATION_PLAN.md)
- [MAPL3 Issue #4531](https://github.com/GEOS-ESM/MAPL/issues/4531)
- PR #4535: NormalizationAspect consolidation
- PR #4536: RegridTransform integrated normalization
- PR #4538: GeomAspect enhancements
- PR #4539: VerticalRegridTransform enhancements
- PR #4546: Integration testing

---

**Document Maintained By:** MAPL3 Development Team  
**Last Updated:** March 2026  
**Version:** 1.0
