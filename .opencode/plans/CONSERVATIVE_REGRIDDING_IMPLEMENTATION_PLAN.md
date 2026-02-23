# MAPL3 Conservative Regridding - Detailed Implementation Plan

**Document Version:** 2.0  
**Date:** 2026-02-22  
**Status:** Phase 1 - In Progress

## Implementation Progress

- **Completed (2026-02-22):**
  - ✅ Task 1.1: QuantityType enum types (`QuantityType.F90`)
  - ✅ Task 1.2: QuantityTypeAspect (`QuantityTypeAspect.F90`)
  - ✅ Unit tests for both components (12 tests for enums, 20 tests for aspect)
  - ✅ Successfully builds with NAG compiler

## GitHub Issues

- **Epic:** #4436 - Conservative Regridding Support for MAPL3
- **Phase 1:** #4437 - Infrastructure + 2D Conservative Regridding
  - ✅ Task 1.1 & 1.2: #4435 - QuantityType & QuantityTypeAspect (COMPLETED)
  - ⚠️ **TODO:** Create issues for remaining Phase 1 tasks (1.3-1.8)

**Note:** When starting Phase 2, remember to create:
- Phase 2 GitHub issue (link to Epic #4436)
- Individual task issues for Phase 2 tasks

---

## Executive Summary

This document provides a comprehensive implementation plan for adding conservative regridding support to MAPL3. The design supports:

1. **3D mixing ratio fields** (wet/dry, mass/volume basis)
2. **3D concentration fields** (mass per volume)
3. **2D conservative regridding** (baseline capability)
4. **Extensible architecture** for future field types

**Key architectural decisions:**
- Fused vertical regridding (VerticalRegridTransform handles internal normalization)
- Vertical coordinates from grid metadata (recomputed, not regridded)
- Horizontal auxiliary fields conservatively regridded (delp, surface pressure)
- New aspect: `QuantityTypeAspect` describes field's physical nature
- NormalizationAspect writes units to Info (maintains units correctness)

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Phase Breakdown](#phase-breakdown)
3. [Detailed Task Specifications](#detailed-task-specifications)
4. [File Structure](#file-structure)
5. [Testing Strategy](#testing-strategy)
6. [Dependencies and Risks](#dependencies-and-risks)

---

## Architecture Overview

### Aspect-Based Transform Pipeline

Conservative regridding uses a sequence of aspects to transform fields:

```
Field [kg/kg] (wet mass mixing ratio)
  ↓
[QuantityTypeAspect] - metadata, no transform
  ↓
[NormalizationAspect] - × (delp/g) → [kg/m²]
  ↓
[GeomAspect] - horizontal conservative regrid
  ↓
[VerticalGridAspect] - vertical conservative regrid (fused dp handling)
  ↓
[InverseNormalizationAspect] - ÷ (delp/g) → [kg/kg]
  ↓
[UnitsAspect] - validates/converts units
  ↓
Field [kg/kg] on new grids
```

### New Components

**1. QuantityTypeAspect** (new aspect, metadata only)
- Describes what kind of physical quantity the field represents
- User provides semantic information (quantity type, dimensions, basis)
- Framework derives operational parameters (normalization method, conservability)
- Does not generate transforms (metadata aspect)

**2. NormalizationAspect** (new aspect, generates transform)
- Multiplies field by auxiliary field (delp/g or dz) before horizontal regrid
- Updates units in ESMF Info object
- Creates NormalizationTransform

**3. InverseNormalizationAspect** (new aspect, generates transform)
- Divides field by auxiliary field after vertical regrid
- Updates units back to original in ESMF Info object
- Creates InverseNormalizationTransform

**4. Enhanced VerticalGridAspect** (modified existing)
- Supports fused normalization (internally handles ÷dp, regrid, ×dp_new)
- Queries QuantityTypeAspect to determine if normalization needed
- Accesses primary vertical coordinate from grid metadata

**5. MixingRatioBasisAspect** (Phase 3, new aspect)
- Converts between different mixing ratio bases (wet↔dry, mass↔volume)
- Generates MixingRatioBasisTransform

### Auxiliary Field Strategy

**Horizontal regridding:**
- Auxiliary fields (delp, surface pressure) regridded conservatively
- Separate from tracer regridding (existing MAPL pipeline)
- Framework looks up regridded auxiliary fields from state

**Vertical regridding:**
- Vertical coordinates from grid metadata
- dp/dz values recomputed from coordinate definitions
- Not regridded as fields

---

## Phase Breakdown

### Phase 1: Infrastructure + 2D Conservative (2-3 weeks)

**Goal:** Establish metadata infrastructure and baseline 2D conservative regridding

**Deliverables:**
- QuantityTypeAspect implementation
- Enum types for quantity classification
- Validation framework for conservative regridability
- 2D conservative regridding tests
- Update delp/ps to use conservative regridding
- Documentation

**Effort:** ~80 hours

---

### Phase 2: 3D Wet Mass Mixing Ratio (3-4 weeks)

**Goal:** Enable conservative regridding of 3D wet air mass mixing ratios

**Deliverables:**
- NormalizationAspect (delp-based)
- InverseNormalizationAspect (delp-based)
- Enhanced VerticalRegridTransform (fused dp handling)
- Primary vertical coordinate metadata
- Auxiliary field reference system
- Comprehensive 3D conservative regridding tests
- User guide for wet mass mixing ratios

**Effort:** ~120 hours

---

### Phase 3: Dry Mass & Volume Mixing Ratios (2-3 weeks)

**Goal:** Support dry air basis and volume mixing ratios

**Deliverables:**
- MixingRatioBasisAspect
- InverseMixingRatioBasisAspect
- Molecular weight metadata handling
- Moisture field summation
- Dry air and volume mixing ratio tests
- Extended user guide

**Effort:** ~80 hours

---

### Phase 4: Concentration Support (2 weeks)

**Goal:** Support mass concentration fields [kg/m³]

**Deliverables:**
- NormalizationAspect (dz-based)
- InverseNormalizationAspect (dz-based)
- dz auxiliary field handling
- Concentration regridding tests
- Documentation

**Effort:** ~60 hours

---

### Phase 5: ExtData Integration & Advanced (3-4 weeks, DEFERRED)

**Goal:** ExtData integration and advanced features

**Deliverables:**
- ExtData heuristics for auxiliary fields
- Cross-type conversions (concentration → mixing ratio)
- Auxiliary field derivation when possible
- Performance optimization
- Complete user documentation

**Effort:** ~120 hours (deferred, requires ExtData design discussion)

---

## Detailed Task Specifications

### Phase 1: Infrastructure + 2D Conservative

#### Task 1.1: Create Enum Types for Quantity Classification
**File:** `generic3g/specs/QuantityType.F90`  
**Effort:** 4 hours

**Description:**
Create pseudo-enum types for quantity classification with proper validation and string conversion.

**Implementation:**
```fortran
module mapl3g_QuantityType
   implicit none
   private
   
   ! Quantity type classification
   type, public :: QuantityType
      integer :: value = QUANTITY_UNKNOWN
   contains
      procedure :: to_string => quantity_type_to_string
      procedure :: from_string => quantity_type_from_string
      procedure :: is_valid => quantity_type_is_valid
   end type
   
   integer, parameter, public :: QUANTITY_UNKNOWN = 0
   integer, parameter, public :: QUANTITY_MIXING_RATIO = 1
   integer, parameter, public :: QUANTITY_CONCENTRATION = 2
   integer, parameter, public :: QUANTITY_TEMPERATURE = 3
   integer, parameter, public :: QUANTITY_PRESSURE = 4
   integer, parameter, public :: QUANTITY_EXTENSIVE = 5
   
   ! Mixing ratio basis
   type, public :: MixingRatioBasis
      integer :: value = BASIS_NONE
   contains
      procedure :: to_string => basis_to_string
      procedure :: from_string => basis_from_string
   end type
   
   integer, parameter, public :: BASIS_NONE = 0
   integer, parameter, public :: BASIS_WET_MASS = 1
   integer, parameter, public :: BASIS_DRY_MASS = 2
   integer, parameter, public :: BASIS_VOLUME = 3
   
   ! Normalization type (internal use)
   type, public :: NormalizationType
      integer :: value = NORMALIZE_NONE
   end type
   
   integer, parameter, public :: NORMALIZE_NONE = 0
   integer, parameter, public :: NORMALIZE_DELP = 1
   integer, parameter, public :: NORMALIZE_DZ = 2
   
contains
   ! Implementation of to_string, from_string, validation methods
end module mapl3g_QuantityType
```

**Tests:**
- `Test_QuantityType.pf`: Enum creation, validation, string conversion

**Acceptance Criteria:**
- All enum types have validation methods
- String conversion (to/from) works correctly
- Invalid values rejected with clear error messages

---

#### Task 1.2: Create QuantityTypeAspect
**File:** `generic3g/specs/QuantityTypeAspect.F90`  
**Effort:** 12 hours

**Description:**
Create metadata aspect that describes the physical nature of a field and derives operational parameters.

**Implementation:**
```fortran
type, extends(StateItemAspect) :: QuantityTypeAspect
   private
   
   ! User-specified (semantic)
   type(QuantityType) :: quantity_type
   character(:), allocatable :: dimensions          ! e.g., "kg/kg", "kg/m3"
   type(MixingRatioBasis) :: basis
   real, allocatable :: molecular_weight            ! For volume mixing ratios
   
   ! Framework-derived (operational)
   logical :: conservative_regridable = .false.     ! Computed during initialize
   type(NormalizationType) :: normalization_type
   real :: normalization_scale = 1.0                ! e.g., 1/g for delp
   character(:), allocatable :: aux_field_name      ! e.g., "DELP", "DZ"
   
contains
   procedure :: initialize_derived_properties       ! Computes derived from user-specified
   procedure :: validate_conservative_regrid        ! Checks if conservative possible
   procedure :: get_normalization_info              ! Returns normalization parameters
   
   ! StateItemAspect interface
   procedure :: matches
   procedure :: make_transform                      ! Returns NullTransform (metadata only)
   procedure :: connect_to_export
   procedure :: supports_conversion_general
   procedure :: supports_conversion_specific
   procedure, nopass :: get_aspect_id
   procedure :: update_from_payload
   procedure :: update_payload
end type
```

**Derivation Logic:**
```fortran
subroutine initialize_derived_properties(this, rc)
   ! Determine if conservative regridding is supported
   if (this%quantity_type%value == QUANTITY_MIXING_RATIO) then
      this%conservative_regridable = .true.
      this%normalization_type%value = NORMALIZE_DELP
      this%normalization_scale = 1.0 / MAPL_GRAV
      this%aux_field_name = "DELP"
      
   else if (this%quantity_type%value == QUANTITY_CONCENTRATION) then
      this%conservative_regridable = .true.
      this%normalization_type%value = NORMALIZE_DZ
      this%normalization_scale = 1.0
      this%aux_field_name = "DZ"
      
   else if (this%quantity_type%value == QUANTITY_TEMPERATURE) then
      this%conservative_regridable = .false.  ! Temperatures don't conserve
      this%normalization_type%value = NORMALIZE_NONE
      
   ! ... other quantity types
   end if
end subroutine
```

**Tests:**
- `Test_QuantityTypeAspect.pf`: 
  - User creates aspect with semantic info
  - Framework correctly derives operational parameters
  - Validation catches unsupported combinations
  - Aspect serialization to/from Info object

**Acceptance Criteria:**
- Derivation logic correctly maps semantic → operational
- Clear error messages for unsupported quantity types
- Aspect integrates with existing aspect system (matches, make_transform, etc.)

---

#### Task 1.3: Update ESMF Info Keys
**File:** `shared/MAPL_ESMF_InfoKeys.F90`  
**Effort:** 2 hours

**Description:**
Add new Info keys for quantity type metadata.

**Changes:**
```fortran
! Add keys for QuantityTypeAspect
character(*), parameter :: KEY_QUANTITY_TYPE = "quantity_type"
character(*), parameter :: KEY_QUANTITY_DIMENSIONS = "quantity_dimensions"
character(*), parameter :: KEY_QUANTITY_BASIS = "quantity_basis"
character(*), parameter :: KEY_MOLECULAR_WEIGHT = "molecular_weight"
character(*), parameter :: KEY_CONSERVATIVE_REGRIDABLE = "conservative_regridable"
character(*), parameter :: KEY_NORMALIZATION_TYPE = "normalization_type"
character(*), parameter :: KEY_NORMALIZATION_SCALE = "normalization_scale"
character(*), parameter :: KEY_AUX_FIELD_NAME = "aux_field_name"
```

**Tests:**
- Verify keys don't conflict with existing keys
- Test Info object read/write with new keys

---

#### Task 1.4: Add QuantityTypeAspect to Aspect Registry
**File:** `generic3g/specs/StateItemAspect_registry.F90`  
**Effort:** 2 hours

**Description:**
Register new aspect ID and update aspect ordering.

**Changes:**
```fortran
! Add new aspect ID
type(AspectId), parameter :: QUANTITY_TYPE_ASPECT_ID = AspectId(7)

! Update aspect ordering (QuantityType before normalization)
type(AspectId), parameter :: DEFAULT_ASPECT_ORDER(*) = [ &
   CLASS_ASPECT_ID, &
   ATTRIBUTES_ASPECT_ID, &
   UNGRIDDED_DIMS_ASPECT_ID, &
   QUANTITY_TYPE_ASPECT_ID, &      ! NEW
   GEOM_ASPECT_ID, &
   VERTICAL_GRID_ASPECT_ID, &
   UNITS_ASPECT_ID, &
   TYPEKIND_ASPECT_ID &
]
```

---

#### Task 1.5: Add Validation for Conservative Regridding
**File:** `generic3g/specs/GeomAspect.F90` (modify)  
**File:** `generic3g/specs/VerticalGridAspect.F90` (modify)  
**Effort:** 6 hours

**Description:**
Add validation that conservative regridding is only used with appropriate field types.

**Changes to GeomAspect:**
```fortran
function make_transform(src, dst, other_aspects, rc) result(transform)
   ! ... existing code ...
   
   ! If conservative regridding requested, validate
   if (regridder_param%is_conservative()) then
      call validate_conservative_regrid(other_aspects, "horizontal", _RC)
   end if
   
   ! ... create transform ...
end function

subroutine validate_conservative_regrid(aspects, regrid_type, rc)
   type(AspectMap), intent(in) :: aspects
   character(*), intent(in) :: regrid_type
   
   type(QuantityTypeAspect) :: qty_aspect
   
   ! Get QuantityTypeAspect
   qty_aspect = to_QuantityTypeAspect(aspects, _RC)
   
   ! Check if conservative regridding is supported
   if (.not. qty_aspect%conservative_regridable) then
      _FAIL("Conservative " // regrid_type // " regridding not supported for quantity type: " // &
            qty_aspect%quantity_type%to_string())
   end if
end subroutine
```

**Tests:**
- Attempt conservative regridding of temperature field → fails with clear error
- Attempt conservative regridding of mixing ratio → succeeds
- Error messages are helpful and include quantity type

---

#### Task 1.6: Update delp and Surface Pressure to Use Conservative Regridding
**File:** Multiple (ExtData configuration, field specs)  
**Effort:** 8 hours

**Description:**
Change existing delp and surface pressure fields to use conservative rather than bilinear regridding.

**Changes:**
1. Update field specifications for DELP and PS
2. Set regridder_param to conservative
3. Add QuantityTypeAspect metadata (QUANTITY_PRESSURE, etc.)
4. Update ExtData internal field specifications

**Impact Analysis:**
- Need to verify this doesn't break existing tests
- May need to update baseline results
- Document the change and rationale

**Tests:**
- Verify delp is conservatively regridded
- Compare results vs bilinear (should be more accurate)
- Ensure integration tests still pass

---

#### Task 1.7: Implement 2D Conservative Regridding Tests
**File:** `generic3g/tests/Test_2DConservative.pf`  
**Effort:** 8 hours

**Description:**
Create comprehensive tests for 2D conservative regridding (baseline before 3D).

**Test Cases:**
1. **Column-integrated mass** [kg/m²]
   - Create field with known total mass
   - Regrid to different horizontal grid
   - Verify total mass conserved (within tolerance)

2. **Surface pressure** [Pa]
   - Conservative regridding of surface pressure
   - Compare to bilinear (should be different)
   - Verify physical consistency

3. **2D extensive quantity**
   - Field already in per-area units
   - Verify ESMF conservative works directly

**Validation:**
- Global integral conserved
- Local mass balance (check that sum of target cells equals source cell for exact overlaps)
- Tolerance: ~machine epsilon for idealized grids

---

#### Task 1.8: Documentation - Phase 1
**File:** `docs/conservative_regridding.md`  
**Effort:** 6 hours

**Description:**
Document the infrastructure and 2D conservative regridding capability.

**Sections:**
1. Overview of conservative regridding in MAPL3
2. QuantityTypeAspect usage guide
3. How to specify field metadata for conservative regridding
4. 2D conservative regridding examples
5. Validation and testing approach
6. Troubleshooting guide

---

### Phase 2: 3D Wet Mass Mixing Ratio

#### Task 2.1: Create NormalizationAspect
**File:** `generic3g/specs/NormalizationAspect.F90`  
**Effort:** 16 hours

**Description:**
Create aspect that normalizes fields before horizontal conservative regridding.

**Implementation:**
```fortran
type, extends(StateItemAspect) :: NormalizationAspect
   private
   
   character(:), allocatable :: aux_field_name     ! "DELP" or "DZ"
   real :: scale_factor                            ! e.g., 1/g
   character(:), allocatable :: source_units       ! e.g., "kg/kg"
   character(:), allocatable :: target_units       ! e.g., "kg/m2"
   
contains
   procedure :: matches
   procedure :: make_transform                     ! Creates NormalizationTransform
   procedure :: connect_to_export
   procedure :: supports_conversion_general
   procedure :: supports_conversion_specific
   procedure, nopass :: get_aspect_id
   procedure :: update_from_payload                ! Reads from QuantityTypeAspect
   procedure :: update_payload                     ! Writes units to Info!
end type
```

**Key Design Point - update_payload writes units:**
```fortran
subroutine update_payload(this, field, bundle, state, rc)
   ! Write normalization metadata
   call ESMF_AttributeSet(field, "normalization_field", this%aux_field_name, _RC)
   call ESMF_AttributeSet(field, "normalization_scale", this%scale_factor, _RC)
   
   ! CRITICAL: Update units in Info
   call ESMF_AttributeSet(field, "units", this%target_units, _RC)
   
   ! This allows UnitsAspect to see correct units when it calls update_from_payload()
end subroutine
```

**Initialization from QuantityTypeAspect:**
```fortran
subroutine update_from_payload(this, field, bundle, state, rc)
   type(QuantityTypeAspect) :: qty_aspect
   
   ! Get normalization info from QuantityTypeAspect
   qty_aspect = get_quantity_type_aspect(field, _RC)
   
   this%aux_field_name = qty_aspect%aux_field_name
   this%scale_factor = qty_aspect%normalization_scale
   
   ! Get source units
   call ESMF_AttributeGet(field, "units", this%source_units, _RC)
   
   ! Compute target units using UDUNITS
   this%target_units = compute_target_units(this%source_units, &
                                             this%aux_field_name, &
                                             this%scale_factor, _RC)
end subroutine

function compute_target_units(source_units, aux_field, scale, rc) result(target_units)
   ! Use UDUNITS to compute:
   ! source_units × (aux_field_units × scale) = target_units
   ! e.g., "kg/kg" × ("Pa" × 1/g) = "kg/m2"
   
   ! UDUNITS logic here...
end function
```

**Tests:**
- `Test_NormalizationAspect.pf`:
  - Creates aspect from QuantityTypeAspect metadata
  - Validates unit transformations
  - Tests update_payload writes correct units
  - Tests matches() logic

---

#### Task 2.2: Create NormalizationTransform
**File:** `generic3g/transforms/NormalizationTransform.F90`  
**Effort:** 12 hours

**Description:**
Transform that multiplies field by auxiliary field (delp/g or dz).

**Implementation:**
```fortran
type, extends(ExtensionTransform) :: NormalizationTransform
   private
   character(:), allocatable :: aux_field_name
   real :: scale_factor
contains
   procedure :: initialize
   procedure :: update
   procedure :: get_transformId
end type

subroutine update(this, importState, exportState, clock, rc)
   ! Get the field to normalize
   call ESMF_StateGet(importState, COUPLER_IMPORT_NAME, field=f_in, _RC)
   call ESMF_StateGet(exportState, COUPLER_EXPORT_NAME, field=f_out, _RC)
   
   ! Get the auxiliary field (e.g., DELP)
   call ESMF_StateGet(importState, this%aux_field_name, field=f_aux, _RC)
   
   ! Get data pointers
   call ESMF_FieldGet(f_in, farrayPtr=data_in, _RC)
   call ESMF_FieldGet(f_out, farrayPtr=data_out, _RC)
   call ESMF_FieldGet(f_aux, farrayPtr=aux_data, _RC)
   
   ! Perform normalization: out = in × (aux × scale)
   data_out = data_in * aux_data * this%scale_factor
end subroutine
```

**Tests:**
- `Test_NormalizationTransform.pf`:
  - Create mixing ratio field and delp field
  - Apply normalization
  - Verify output = input × (delp/g)
  - Verify units updated correctly
  - Test with 3D and 2D fields
  - Test error handling (aux field not found, etc.)

---

#### Task 2.3: Create InverseNormalizationAspect
**File:** `generic3g/specs/InverseNormalizationAspect.F90`  
**Effort:** 12 hours

**Description:**
Aspect that denormalizes fields after regridding.

**Implementation:**
Similar to NormalizationAspect but:
- Divides instead of multiplies
- Converts units back: `kg/m2 → kg/kg`
- Looks up regridded auxiliary field (delp on new horizontal grid)

**Key consideration:**
```fortran
! After horizontal regrid, need delp on new horizontal grid
! delp must have been regridded separately
call ESMF_StateGet(importState, this%aux_field_name, field=f_aux, _RC)
! This is the REGRIDDED delp
```

**Tests:**
- `Test_InverseNormalizationAspect.pf`:
  - Test inverse normalization
  - Verify units converted back
  - Test round-trip: normalize → denormalize (should recover original units)

---

#### Task 2.4: Create InverseNormalizationTransform
**File:** `generic3g/transforms/InverseNormalizationTransform.F90`  
**Effort:** 10 hours

**Description:**
Transform that divides field by auxiliary field.

**Implementation:**
```fortran
subroutine update(this, importState, exportState, clock, rc)
   ! Similar to NormalizationTransform but:
   data_out = data_in / (aux_data * this%scale_factor)
   
   ! Handle division by zero
   where (abs(aux_data) < epsilon(aux_data))
      data_out = 0.0  ! or MAPL_UNDEF?
   elsewhere
      data_out = data_in / (aux_data * this%scale_factor)
   end where
end subroutine
```

**Tests:**
- `Test_InverseNormalizationTransform.pf`:
  - Inverse transform
  - Division by zero handling
  - Round-trip verification

---

#### Task 2.5: Add Primary Vertical Coordinate to Vertical Grid
**File:** `gridcomps/VerticalGrid/VerticalGridSpec.F90` (or equivalent)  
**Effort:** 8 hours

**Description:**
Add metadata to identify primary vertical coordinate for conservative regridding.

**Changes:**
```fortran
type :: VerticalGridSpec
   ! ... existing members ...
   character(:), allocatable :: primary_coordinate  ! "pressure" or "height"
contains
   procedure :: set_primary_coordinate
   procedure :: get_primary_coordinate
end type

! During vertical grid creation:
call vert_grid%set_primary_coordinate("pressure", _RC)
```

**Convention:**
- First coordinate added is primary by default
- User can override with explicit call

**Tests:**
- Create vertical grid with pressure coordinate → primary = "pressure"
- Create vertical grid with multiple coordinates → first is primary
- Test get/set methods

---

#### Task 2.6: Enhance VerticalRegridTransform for Fused Normalization
**File:** `generic3g/transforms/VerticalRegridTransform.F90` (modify existing)  
**Effort:** 24 hours

**Description:**
Modify vertical regridding to internally handle normalization/denormalization for conservative regridding.

**Current behavior:**
- Takes field, regrids vertically
- No normalization

**New behavior:**
- Query QuantityTypeAspect to determine if normalization needed
- If conservative + needs normalization:
  1. Convert `[kg/m²]` → `[kg/(m²·Pa)]` using source vertical grid dp
  2. Regrid conservatively using overlap regions
  3. Convert `[kg/(m²·Pa)]` → `[kg/m²]` using target vertical grid dp
- Else: regrid as before

**Implementation:**
```fortran
subroutine update(this, importState, exportState, clock, rc)
   ! Get fields
   call ESMF_StateGet(importState, COUPLER_IMPORT_NAME, field=f_in, _RC)
   call ESMF_StateGet(exportState, COUPLER_EXPORT_NAME, field=f_out, _RC)
   
   ! Get QuantityTypeAspect from field metadata
   qty_aspect = get_quantity_type_aspect(f_in, _RC)
   
   if (qty_aspect%conservative_regridable .and. &
       qty_aspect%normalization_type%value == NORMALIZE_DELP) then
      
      ! Get vertical grids
      src_vert = get_vertical_grid(f_in, _RC)
      dst_vert = get_vertical_grid(f_out, _RC)
      
      ! Get dp arrays from grids
      dp_src = src_vert%get_layer_thickness(_RC)
      dp_dst = dst_vert%get_layer_thickness(_RC)
      
      ! Step 1: Convert to [kg/(m²·Pa)]
      data_temp = data_in / dp_src
      
      ! Step 2: Conservative vertical regrid
      call this%regridder%regrid_vertical(data_temp, data_out_temp, &
                                           src_vert, dst_vert, _RC)
      
      ! Step 3: Convert back to [kg/m²]
      data_out = data_out_temp * dp_dst
      
   else
      ! Regular vertical regridding (no normalization)
      call this%regridder%regrid_vertical(data_in, data_out, &
                                           src_vert, dst_vert, _RC)
   end if
end subroutine
```

**Tests:**
- `Test_VerticalRegridFused.pf`:
  - Test fused normalization for mixing ratio
  - Verify conservation (mass conserved across vertical regrid)
  - Test with different vertical grids
  - Test without normalization (temperature field, etc.)

---

#### Task 2.7: Update Aspect Ordering for Normalization
**File:** `generic3g/specs/FieldClassAspect.F90`, `FieldBundleClassAspect.F90`  
**Effort:** 4 hours

**Description:**
Add new normalization aspects to aspect ordering.

**New ordering:**
```fortran
type(AspectId), parameter :: FIELD_ASPECT_ORDER(*) = [ &
   CLASS_ASPECT_ID, &
   ATTRIBUTES_ASPECT_ID, &
   UNGRIDDED_DIMS_ASPECT_ID, &
   QUANTITY_TYPE_ASPECT_ID, &
   NORMALIZATION_ASPECT_ID, &           ! NEW - before horizontal
   GEOM_ASPECT_ID, &
   VERTICAL_GRID_ASPECT_ID, &           ! Fused normalization internal
   INVERSE_NORMALIZATION_ASPECT_ID, &   ! NEW - after vertical
   UNITS_ASPECT_ID, &
   TYPEKIND_ASPECT_ID &
]
```

---

#### Task 2.8: Integration Test - 3D Conservative Regridding
**File:** `generic3g/tests/Test_3DConservativeMixingRatio.pf`  
**Effort:** 16 hours

**Description:**
Comprehensive integration test for 3D conservative regridding of wet mass mixing ratios.

**Test Scenarios:**

1. **Idealized uniform field**
   - Create mixing ratio field with uniform value
   - Regrid to different horizontal and vertical grids
   - Verify total mass conserved
   - Verify field values reasonable

2. **Realistic atmospheric profile**
   - Create CO2 mixing ratio with realistic vertical profile
   - Include horizontal variations
   - Regrid conservatively
   - Verify conservation and physical realism

3. **Edge cases**
   - Very thin layers
   - Large grid ratio changes (coarse to fine, fine to coarse)
   - Pole regions (if applicable)

4. **Round-trip test**
   - Grid A → Grid B → Grid A
   - Should recover original (within tolerance)

5. **Comparison with bilinear**
   - Same field regridded conservatively vs bilinearly
   - Conservative should conserve mass, bilinear may not
   - Document differences

**Validation Metrics:**
- Global mass conservation: `∑(q × delp/g) ≈ constant` (< 1e-10 relative error)
- Local mass balance for coincident cells
- Monotonicity where expected
- No negative values (if physically impossible)

---

#### Task 2.9: User Guide - Phase 2
**File:** `docs/conservative_regridding_user_guide.md`  
**Effort:** 8 hours

**Description:**
Comprehensive user guide for 3D conservative regridding.

**Sections:**
1. Quick start - regridding a tracer field
2. Field metadata specification (QuantityTypeAspect)
3. Auxiliary fields (DELP) - how to ensure they're available
4. Validation and diagnostics
5. Performance considerations
6. Troubleshooting
7. Examples from GEOS use cases

---

### Phase 3: Dry Mass & Volume Mixing Ratios

#### Task 3.1: Create MixingRatioBasisAspect
**File:** `generic3g/specs/MixingRatioBasisAspect.F90`  
**Effort:** 16 hours

**Description:**
Aspect that converts between different mixing ratio bases (wet↔dry, mass↔volume).

**Implementation:**
```fortran
type, extends(StateItemAspect) :: MixingRatioBasisAspect
   private
   
   type(MixingRatioBasis) :: source_basis
   type(MixingRatioBasis) :: target_basis
   
   ! For volume conversions
   real, allocatable :: molecular_weight
   real :: molecular_weight_air  ! Could be constant or computed
   
   ! For dry/wet conversions
   character(:), allocatable, dimension(:) :: moisture_constituents
   
contains
   procedure :: make_transform  ! Creates appropriate conversion transform
   ! ... other StateItemAspect methods ...
end type
```

**Conversion Types:**
1. **Wet mass → Dry mass:** needs total moisture (q_total)
2. **Dry mass → Wet mass:** needs total moisture
3. **Volume → Mass:** needs molecular weights
4. **Mass → Volume:** needs molecular weights

**Tests:**
- Test each conversion type
- Test round-trip conversions
- Test with realistic moisture values

---

#### Task 3.2: Create Basis Conversion Transforms
**Files:** 
- `generic3g/transforms/WetToDryTransform.F90`
- `generic3g/transforms/DryToWetTransform.F90`
- `generic3g/transforms/VolumeToMassTransform.F90`
- `generic3g/transforms/MassToVolumeTransform.F90`

**Effort:** 20 hours total

**Description:**
Transforms that perform basis conversions.

**WetToDryTransform:**
```fortran
subroutine update(this, importState, exportState, clock, rc)
   ! Get tracer field (wet basis)
   ! Get moisture fields and compute q_total
   ! Convert: q_dry = q_wet × (1 + q_total)
end subroutine
```

**VolumeToMassTransform:**
```fortran
subroutine update(this, importState, exportState, clock, rc)
   ! Get tracer field (volume basis)
   ! Convert: q_mass = q_volume × (MW_species / MW_air)
end subroutine
```

**Tests:**
- Test each transform with known input/output values
- Test moisture field lookup and summation
- Test molecular weight application
- Error handling (missing moisture fields, invalid MW, etc.)

---

#### Task 3.3: Moisture Field Summation Logic
**File:** `generic3g/transforms/MoistureSum.F90` (utility)  
**Effort:** 8 hours

**Description:**
Utility for finding and summing moisture constituent fields.

**Implementation:**
```fortran
function compute_total_moisture(state, constituent_names, rc) result(q_total)
   type(ESMF_State), intent(in) :: state
   character(:), allocatable, dimension(:), intent(in) :: constituent_names
   real, allocatable :: q_total(:,:,:)
   
   ! Loop through constituent names
   ! Fetch each field from state
   ! Sum to get total moisture
end function
```

**Fallback strategies:**
- If constituent_names provided, use those
- Else look for standard names ("Q", "QLLS", "QILS")
- Else look for fields with quantity_type = moisture

---

#### Task 3.4: Update Aspect Ordering for Basis Conversion
**File:** `generic3g/specs/FieldClassAspect.F90`  
**Effort:** 2 hours

**Description:**
Add MixingRatioBasisAspect to ordering (before normalization).

**Updated ordering:**
```fortran
QUANTITY_TYPE_ASPECT_ID,
MIXING_RATIO_BASIS_ASPECT_ID,     ! NEW - convert to wet mass first
NORMALIZATION_ASPECT_ID,
GEOM_ASPECT_ID,
VERTICAL_GRID_ASPECT_ID,
INVERSE_NORMALIZATION_ASPECT_ID,
INVERSE_MIXING_RATIO_BASIS_ASPECT_ID,  ! NEW - convert back to original basis
UNITS_ASPECT_ID,
TYPEKIND_ASPECT_ID
```

---

#### Task 3.5: Integration Tests - Dry and Volume Mixing Ratios
**File:** `generic3g/tests/Test_DryMixingRatio.pf`, `Test_VolumeMixingRatio.pf`  
**Effort:** 12 hours

**Test Cases:**
1. Dry air mixing ratio conservative regridding
2. Volume mixing ratio (ppm) conservative regridding
3. Combined: dry volume → wet mass → regrid → wet mass → dry volume
4. Conservation validation with basis conversions

---

#### Task 3.6: Documentation - Phase 3
**File:** `docs/conservative_regridding_user_guide.md` (update)  
**Effort:** 6 hours

**New sections:**
- Dry air vs wet air mixing ratios
- Volume mixing ratios (ppm, ppb)
- Molecular weight specification
- Moisture field requirements
- Examples with basis conversions

---

### Phase 4: Concentration Support

#### Task 4.1: Add dz-based Normalization
**Files:** `NormalizationAspect.F90`, `InverseNormalizationAspect.F90` (modify)  
**Effort:** 8 hours

**Description:**
Extend normalization aspects to support dz (geometric thickness) in addition to delp.

**Changes:**
- NormalizationAspect already designed to handle different aux fields
- Ensure dz case tested
- Verify unit conversions: `[kg/m³] × [m] = [kg/m²]`

---

#### Task 4.2: dz Auxiliary Field Handling
**File:** `generic3g/specs/AuxiliaryFieldReference.F90` (if not yet created)  
**Effort:** 8 hours

**Description:**
Ensure dz fields can be specified, exported, and regridded.

**Considerations:**
- dz may vary horizontally (non-hydrostatic, terrain-following)
- Should dz be conservatively regridded? (Probably not - it's a length)
- Likely bilinear or patch for dz

---

#### Task 4.3: Integration Tests - Concentration Fields
**File:** `generic3g/tests/Test_ConcentrationRegrid.pf`  
**Effort:** 10 hours

**Test Cases:**
1. Mass concentration `[kg/m³]` conservative regridding
2. With pressure vertical coordinate (needs both dz and delp)
3. Conservation validation
4. Comparison with mixing ratio approach

---

#### Task 4.4: Documentation - Phase 4
**Effort:** 4 hours

**Description:**
Document concentration field support, dz requirements, and use cases.

---

### Phase 5: ExtData Integration & Advanced (DEFERRED)

**Note:** This phase requires separate design discussions with ExtData team (3-5 days). Tasks are outlined but not detailed.

#### Task 5.1: ExtData Heuristics for Auxiliary Fields
- How does ExtData determine field needs DELP?
- How to auto-load auxiliary fields from NetCDF?
- CF conventions support

#### Task 5.2: Cross-Type Conversions
- Concentration → Mixing Ratio output
- Requires combining normalization conversions
- Additional aspect logic

#### Task 5.3: Auxiliary Field Derivation
- Derive dz from delp when possible (hydrostatic assumption)
- Derive delp from surface pressure + vertical coordinate
- Heuristics and validation

#### Task 5.4: Performance Optimization
- Avoid redundant regridding of auxiliary fields
- Caching strategies
- Parallel I/O for large fields

---

## File Structure

### New Files to Create

```
MAPL3/
├── generic3g/
│   ├── specs/
│   │   ├── QuantityType.F90                      # Task 1.1
│   │   ├── QuantityTypeAspect.F90                # Task 1.2
│   │   ├── NormalizationAspect.F90               # Task 2.1
│   │   ├── InverseNormalizationAspect.F90        # Task 2.3
│   │   ├── MixingRatioBasisAspect.F90            # Task 3.1
│   │   └── InverseMixingRatioBasisAspect.F90     # Task 3.1
│   │
│   ├── transforms/
│   │   ├── NormalizationTransform.F90            # Task 2.2
│   │   ├── InverseNormalizationTransform.F90     # Task 2.4
│   │   ├── WetToDryTransform.F90                 # Task 3.2
│   │   ├── DryToWetTransform.F90                 # Task 3.2
│   │   ├── VolumeToMassTransform.F90             # Task 3.2
│   │   ├── MassToVolumeTransform.F90             # Task 3.2
│   │   └── MoistureSum.F90                       # Task 3.3 (utility)
│   │
│   └── tests/
│       ├── Test_QuantityType.pf                  # Task 1.1
│       ├── Test_QuantityTypeAspect.pf            # Task 1.2
│       ├── Test_2DConservative.pf                # Task 1.7
│       ├── Test_NormalizationAspect.pf           # Task 2.1
│       ├── Test_NormalizationTransform.pf        # Task 2.2
│       ├── Test_InverseNormalizationAspect.pf    # Task 2.3
│       ├── Test_InverseNormalizationTransform.pf # Task 2.4
│       ├── Test_VerticalRegridFused.pf           # Task 2.6
│       ├── Test_3DConservativeMixingRatio.pf     # Task 2.8
│       ├── Test_DryMixingRatio.pf                # Task 3.5
│       ├── Test_VolumeMixingRatio.pf             # Task 3.5
│       └── Test_ConcentrationRegrid.pf           # Task 4.3
│
├── docs/
│   ├── conservative_regridding.md                # Task 1.8
│   └── conservative_regridding_user_guide.md     # Task 2.9
│
└── CONSERVATIVE_REGRIDDING_IMPLEMENTATION_PLAN.md  # This file
```

### Files to Modify

```
MAPL3/
├── shared/
│   └── MAPL_ESMF_InfoKeys.F90                    # Task 1.3
│
├── generic3g/
│   ├── specs/
│   │   ├── StateItemAspect_registry.F90          # Task 1.4
│   │   ├── FieldClassAspect.F90                  # Task 2.7, 3.4
│   │   ├── FieldBundleClassAspect.F90            # Task 2.7
│   │   ├── GeomAspect.F90                        # Task 1.5
│   │   └── VerticalGridAspect.F90                # Task 1.5
│   │
│   └── transforms/
│       └── VerticalRegridTransform.F90           # Task 2.6
│
└── gridcomps/VerticalGrid/
    └── VerticalGridSpec.F90                      # Task 2.5
```

---

## Testing Strategy

### Unit Tests
- Each new module has dedicated pFUnit test
- Test individual components in isolation
- Mock dependencies where appropriate
- Target: >90% code coverage

### Integration Tests
- Test complete regridding pipeline
- Realistic atmospheric data
- Multiple grid combinations
- Conservation validation

### Validation Tests
- Compare with known solutions
- Benchmark against existing implementations
- Physical consistency checks
- Mass balance verification

### Performance Tests
- Large field regridding (realistic GEOS sizes)
- Memory profiling
- Parallel scaling (if applicable)

### Regression Tests
- Ensure existing functionality not broken
- Baseline comparisons where appropriate

---

## Dependencies and Risks

### Dependencies

**Internal:**
1. ESMF conservative regridding (exists)
2. Vertical grid infrastructure (exists)
3. UDUNITS library (exists)
4. pFUnit testing framework (exists)
5. Info object metadata system (exists)

**External:**
1. ExtData (Phase 5 only) - requires design discussion

### Risks and Mitigation

**Risk 1: Units orthogonality complexity**
- *Risk:* NormalizationAspect writing units may conflict with UnitsAspect
- *Mitigation:* Careful aspect ordering, comprehensive testing
- *Status:* Design solution identified (Option B)

**Risk 2: Auxiliary field availability**
- *Risk:* User forgets to export DELP, leading to runtime errors
- *Mitigation:* Clear error messages, validation at spec creation time, documentation
- *Fallback:* Phase 5 could auto-add auxiliary fields

**Risk 3: Vertical coordinate metadata access**
- *Risk:* Vertical grid may not have pressure/height arrays accessible
- *Mitigation:* Early investigation of vertical grid API
- *Contingency:* Require auxiliary fields for vertical coordinates too

**Risk 4: Performance degradation**
- *Risk:* Additional transforms may slow down regridding
- *Mitigation:* Fused vertical regridding, performance testing
- *Optimization:* Phase 5 could add caching

**Risk 5: ExtData integration complexity**
- *Risk:* Phase 5 may be more complex than anticipated
- *Mitigation:* Deferred to separate design discussion, 3-5 days allocated
- *Alternative:* Phase 5 could be split into multiple sub-phases

**Risk 6: Scope creep**
- *Risk:* Users request additional field types not in plan
- *Mitigation:* Clear phase definitions, defer non-critical features to later phases
- *Process:* New field types require design review before implementation

---

## Success Criteria

### Phase 1
- [ ] QuantityTypeAspect correctly derives operational parameters from semantic info
- [ ] 2D conservative regridding conserves mass (< 1e-10 relative error)
- [ ] Validation catches unsupported field types with clear errors
- [ ] All unit tests pass
- [ ] Documentation complete

### Phase 2
- [ ] 3D wet mass mixing ratio conservatively regridded
- [ ] Global mass conservation < 1e-10 relative error
- [ ] Units correct at every stage of pipeline
- [ ] Vertical regridding fused normalization works
- [ ] Integration tests pass with realistic atmospheric data
- [ ] User guide enables users to regrid tracers successfully

### Phase 3
- [ ] Dry air mixing ratios conservatively regridded
- [ ] Volume mixing ratios conservatively regridded
- [ ] Round-trip conversions work (dry → wet → dry)
- [ ] Moisture field summation robust
- [ ] All tests pass

### Phase 4
- [ ] Concentration fields conservatively regridded
- [ ] dz auxiliary field handling works
- [ ] All tests pass

### Phase 5 (Deferred)
- [ ] ExtData automatically loads auxiliary fields
- [ ] Cross-type conversions work
- [ ] Performance acceptable for GEOS production
- [ ] Complete documentation

---

## Appendix A: Use Case Examples

### Example 1: Wet Mass Mixing Ratio (Phase 2)

**Input:**
- Field: CO2 mixing ratio, units: `kg/kg` (wet air basis)
- Source grid: C90, 72 levels
- Target grid: C180, 91 levels
- Vertical coordinate: Pressure

**Metadata specification:**
```fortran
co2_spec = FieldSpec( &
   name = "CO2", &
   dims = "lev", &
   units = "kg/kg", &
   physical_property = QuantityTypeAspect( &
      quantity_type = QuantityType(QUANTITY_MIXING_RATIO), &
      dimensions = "kg/kg", &
      basis = MixingRatioBasis(BASIS_WET_MASS) &
   ), &
   regrid_method = REGRID_METHOD_CONSERVATIVE &
)
```

**What happens:**
1. Framework derives: `conservative_regridable=.true., normalization_type=DELP, scale=1/g`
2. NormalizationAspect: multiply by `delp/g` → `[kg/m²]`
3. GeomAspect: horizontal conservative regrid
4. VerticalGridAspect: vertical conservative regrid (fused dp handling)
5. InverseNormalizationAspect: divide by `delp/g` → `[kg/kg]`
6. UnitsAspect: validates final units match

**Result:** CO2 field on C180/91-level grid, mass conserved

---

### Example 2: Dry Volume Mixing Ratio (Phase 3)

**Input:**
- Field: O3 volume mixing ratio, units: `ppm` (dry air basis)
- Molecular weight: 48.0 g/mol

**Metadata:**
```fortran
o3_spec = FieldSpec( &
   name = "O3", &
   dims = "lev", &
   units = "ppm", &
   physical_property = QuantityTypeAspect( &
      quantity_type = QuantityType(QUANTITY_MIXING_RATIO), &
      dimensions = "ppm", &
      basis = MixingRatioBasis(BASIS_DRY_VOLUME), &
      molecular_weight = 48.0 &
   ), &
   regrid_method = REGRID_METHOD_CONSERVATIVE &
)
```

**What happens:**
1. MixingRatioBasisAspect: volume → mass (× MW_O3/MW_air) → `[kg/kg]` dry
2. MixingRatioBasisAspect: dry → wet (÷ (1+q_total)) → `[kg/kg]` wet
3. NormalizationAspect: multiply by `delp/g` → `[kg/m²]`
4. GeomAspect: horizontal conservative regrid
5. VerticalGridAspect: vertical conservative regrid
6. InverseNormalizationAspect: divide by `delp/g` → `[kg/kg]` wet
7. InverseMixingRatioBasisAspect: wet → dry (× (1+q_total))
8. InverseMixingRatioBasisAspect: mass → volume (× MW_air/MW_O3) → `[ppm]`

**Result:** O3 field in ppm (dry air basis), mass conserved

---

### Example 3: Concentration Field (Phase 4)

**Input:**
- Field: Aerosol mass concentration, units: `kg/m³`
- Vertical coordinate: Pressure

**Metadata:**
```fortran
aerosol_spec = FieldSpec( &
   name = "AEROSOL", &
   dims = "lev", &
   units = "kg/m3", &
   physical_property = QuantityTypeAspect( &
      quantity_type = QuantityType(QUANTITY_CONCENTRATION), &
      dimensions = "kg/m3" &
   ), &
   regrid_method = REGRID_METHOD_CONSERVATIVE &
)
```

**What happens:**
1. Framework derives: `normalization_type=DZ, aux_field=DZ`
2. NormalizationAspect: multiply by `dz` → `[kg/m²]`
3. GeomAspect: horizontal conservative regrid
4. VerticalGridAspect: vertical conservative regrid (fused dp handling, converts via dz)
5. InverseNormalizationAspect: divide by `dz` → `[kg/m³]`

**Result:** Aerosol field on new grid, mass conserved

---

## Appendix B: Effort Summary

| Phase | Total Effort | Major Components |
|-------|-------------|------------------|
| Phase 1 | ~80 hours | Infrastructure, QuantityTypeAspect, 2D tests |
| Phase 2 | ~120 hours | Normalization aspects, fused vertical, 3D tests |
| Phase 3 | ~80 hours | Basis conversions, moisture handling |
| Phase 4 | ~60 hours | Concentration support, dz handling |
| Phase 5 | ~120 hours (deferred) | ExtData, advanced features |
| **Total** | **~340 hours** | **Phases 1-4** |

**Timeline estimate (1 FTE):**
- Phase 1: 2-3 weeks
- Phase 2: 3-4 weeks
- Phase 3: 2-3 weeks
- Phase 4: 2 weeks
- **Total: ~9-12 weeks for Phases 1-4**

---

## Appendix C: Open Questions for Design Review

Before beginning implementation, please confirm:

1. **Naming:** Approve "QuantityTypeAspect" or suggest alternative
2. **delp regridding:** Confirm always use conservative regridding for delp
3. **PhysicalProperty design:** Confirm user-facing vs framework-derived split
4. **Units handling:** Confirm Option B (NormalizationAspect writes units to Info)
5. **Phase priorities:** Confirm phase ordering and scope
6. **Deferral of Phase 5:** Confirm ExtData integration deferred pending design discussion

---

## Document History

- **v2.0 (2026-02-20):** Complete rewrite based on dimensional analysis session
  - Changed PhysicalProperty to QuantityTypeAspect (extends StateItemAspect)
  - Clarified fused vertical regridding approach
  - Resolved units orthogonality (Option B)
  - Added pseudo-enum types
  - Detailed task specifications with code examples
  
- **v1.0 (Previous):** Initial plan (superseded)

---

**END OF IMPLEMENTATION PLAN**
