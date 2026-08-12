# MAPL3 Integrated Normalization - Design and Implementation Plan

**Document Version:** 1.0  
**Date:** 2026-03-16  
**Status:** Planning

## Context and Motivation

### Background

This plan supersedes portions of the [Conservative Regridding Implementation Plan](CONSERVATIVE_REGRIDDING_IMPLEMENTATION_PLAN.md), specifically addressing how normalization transformations interact with regridding operations.

The original plan treated normalization as separate aspect transforms that would create extension fields:
```
Field [kg/kg]
  ↓ NormalizationAspect → Extension 1
Field [kg/m²]
  ↓ GeomAspect → RegridTransform
Field [kg/m²] (new grid)
  ↓ InverseNormalizationAspect → Extension 2
Field [kg/kg] (new grid)
```

### The Problem

This approach has a critical inefficiency: **Even when import and export grids exactly match**, the system creates two extension fields:

1. Extension 1: Transform to canonical normalization for regridding
2. Extension 2: Transform back to export normalization

**Result:** 3 fields in memory (original + 2 extensions) when conceptually only 1 is needed.

**Impact:** In the most common production scenarios (where grids often match), this could nearly triple memory consumption and adds unnecessary transformation overhead.

### The Solution: Integrated Normalization

**Key Insight:** Regridding transforms should maintain the field's current normalization internally, using canonical normalization only for the actual regridding operation:

```
Field [kg/kg, export normalization]
  ↓
RegridTransform internally:
  1. Denormalize: [kg/kg] → [kg/m²] using DELP from vertical coord
  2. Horizontal conservative regrid
  3. Renormalize: [kg/m²] → [kg/kg] using DELP from vertical coord
  ↓
Field [kg/kg, SAME normalization] on new grid (no extension created)
```

**Benefits:**
- **Memory efficiency:** No extra extensions when grids differ but normalization matches
- **Transparency:** Transforms maintain export normalization (input norm = output norm)
- **Consistency:** Both horizontal and vertical transforms use same pattern
- **Simplicity:** Only one `NormalizationAspect` needed (replaces ExportNormalization + ImportNormalization)

---

## Architecture Overview

### Core Design Principles

1. **Transforms are normalization-transparent:** RegridTransform and VerticalRegridTransform maintain whatever normalization the field has upon entry

2. **Auxiliary fields from vertical coordinates:** DELP and DZ are computed from vertical coordinate fields (PLE, ZLE) obtained via coupler pattern

3. **Conditional internal normalization:** Transforms only perform internal normalization when:
   - Regridding is conservative, AND
   - Field has a normalization requirement (from NormalizationAspect)

4. **NormalizationAspect for true mismatches:** Separate NormalizationAspect transform only created when import and export normalizations genuinely differ

### Information Flow

**GeomAspect::make_transform:**
```fortran
! Query if normalization needed
norm_aspect = to_NormalizationAspect(other_aspects, status)
needs_norm = (status == SUCCESS) .and. (norm_type /= NORMALIZE_NONE)

! Get vertical grid and coordinate field if needed
if (needs_norm .and. is_conservative) then
   vert_aspect = to_VerticalGridAspect(other_aspects, _RC)
   vert_grid => vert_aspect%get_vertical_grid(_RC)
   
   ! Get PLE coordinate field with coupler
   physical_dim = get_physical_dimension(norm_type)  ! "pressure" or "height"
   coord_field = vert_grid%get_coordinate_field(physical_dim, other_aspects, &
                                                 coord_coupler, _RC)
   
   ! Pass to transform
   transform = RegridTransform(src_geom, dst_geom, regridder_param, &
                              coord_field, coord_coupler, norm_aspect%metadata)
else
   transform = RegridTransform(src_geom, dst_geom, regridder_param)
end if
```

**RegridTransform::update:**
```fortran
if (has_normalization) then
   ! Run coupler to update coordinate values
   call coord_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
   
   ! Compute layer thickness: dp(k) = coord(k+1) - coord(k)
   dp = compute_layer_thickness(coord_field)
   
   ! Denormalize: [kg/kg] → [kg/m²]
   data_temp = data_in * (dp * scale_factor)  ! scale_factor = 1/g for DELP
   
   ! Conservative horizontal regrid
   call regridder%apply(data_temp, data_regridded, _RC)
   
   ! Renormalize: [kg/m²] → [kg/kg]
   data_out = data_regridded / (dp * scale_factor)
else
   ! No normalization needed, direct regrid
   call regridder%apply(data_in, data_out, _RC)
end if
```

**VerticalRegridTransform::update** (similar pattern):
- Already has `v_in_field` and `v_out_field` (PLE at src and dst vertical grids)
- Computes `dp_src` and `dp_dst` from these coordinate fields
- Same fused normalization pattern as horizontal

### Aspect Architecture Changes

**Before:**
- `ExportNormalizationAspect` - describes field's current normalization
- `ImportNormalizationAspect` - describes inverse normalization after regridding
- Both create transforms in most cases

**After:**
- `NormalizationAspect` - single aspect describing field's current normalization
- Only creates transform when import and export normalizations truly differ (rare)
- Regridding transforms query this aspect but remain transparent to it

**Aspect Ordering:**
```fortran
type(AspectId), parameter :: FIELD_ASPECT_ORDER(*) = [ &
   CLASS_ASPECT_ID, &
   ATTRIBUTES_ASPECT_ID, &
   UNGRIDDED_DIMS_ASPECT_ID, &
   QUANTITY_TYPE_ASPECT_ID, &
   CONSERVATION_ASPECT_ID, &
   NORMALIZATION_ASPECT_ID, &      ! Single aspect, creates transform only for true mismatches
   GEOM_ASPECT_ID, &                ! Maintains normalization internally
   VERTICAL_GRID_ASPECT_ID, &       ! Maintains normalization internally
   UNITS_ASPECT_ID, &
   TYPEKIND_ASPECT_ID &
]
```

---

## Implementation Tasks

### Task 1: Update NormalizationAspect Structure

**Effort:** 8 hours  
**Branch:** `feature/4527-unified-normalization-aspect`  
**Files:**
- Rename `generic3g/specs/ExportNormalization.F90` → `generic3g/specs/NormalizationAspect.F90`
- Delete `generic3g/specs/ImportNormalization.F90`
- Update `generic3g/specs/StateItemAspect_registry.F90`

**Description:**
Consolidate ExportNormalization and ImportNormalization into a single NormalizationAspect. Remove the `is_inverse` flag since transforms now handle bidirectional normalization internally.

**Changes:**
```fortran
module mapl3g_NormalizationAspect
   type, extends(StateItemAspect) :: NormalizationAspect
      private
      
      ! Use composition with NormalizationMetadata
      type(NormalizationMetadata) :: metadata
      
      ! Aspect-specific fields
      character(:), allocatable :: source_units
      character(:), allocatable :: target_units
      
   contains
      ! StateItemAspect interface
      procedure :: matches
      procedure :: make_transform    ! Only creates transform for true mismatch
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure, nopass :: get_aspect_id
      
      ! Getters
      procedure :: get_normalization_type
      procedure :: get_normalization_scale
      procedure :: get_metadata
      
      procedure :: update_from_payload
      procedure :: update_payload
   end type NormalizationAspect
end module
```

**Key Logic Change in make_transform:**
```fortran
function make_transform(src, dst, other_aspects, rc) result(transform)
   ! Only create actual transform if normalizations differ
   ! (not just mirroring - actual parameter mismatch)
   
   dst_ = to_NormalizationAspect(dst, _RC)
   
   if (src%metadata == dst_%metadata) then
      ! Normalizations match - no transform needed
      allocate(transform, source=NullTransform())
   else
      ! True mismatch - need normalization transform
      ! This is rare (different units/conventions between import and export)
      allocate(transform, source=NormalizationTransform(...))
   end if
end function
```

**Tests:**
- Update `Test_ExportNormalization.pf` → `Test_NormalizationAspect.pf`
- Update `Test_ImportNormalization.pf` (merge into above)
- Test that matching normalizations produce NullTransform
- Test that mismatching normalizations produce NormalizationTransform

**Acceptance Criteria:**
- [ ] Single NormalizationAspect replaces Export/Import versions
- [ ] Aspect creates NullTransform when normalizations match
- [ ] Aspect creates NormalizationTransform only for true mismatches
- [ ] All existing tests pass with updated aspect
- [ ] Aspect ID correctly registered

---

### Task 2: Implement Integrated Normalization in RegridTransform

**Effort:** 20 hours  
**Branch:** `feature/4528-regrid-integrated-normalization`  
**Dependencies:** Task 1  
**Files:**
- `generic3g/transforms/RegridTransform.F90`

**Description:**
Enhance RegridTransform to perform internal normalization when needed, maintaining export normalization throughout the operation. This must be implemented first before GeomAspect can use it (Task 3).

**Constructor Changes:**
```fortran
type :: RegridTransform
   private
   ! Existing members
   type(ESMF_Geom) :: src_geom
   type(ESMF_Geom) :: dst_geom
   class(AbstractRegridder), allocatable :: regridder
   
   ! New members for integrated normalization
   logical :: has_normalization = .false.
   type(NormalizationMetadata) :: norm_metadata
   type(ESMF_Field) :: coord_field
   class(ComponentDriver), pointer :: coord_coupler => null()
   
contains
   procedure :: initialize
   procedure :: update
   procedure :: get_transformId
end type

function new_RegridTransform(src_geom, dst_geom, regridder_param, &
                             coord_field, coord_coupler, norm_metadata) &
                             result(transform)
   type(RegridTransform) :: transform
   type(ESMF_Geom), intent(in) :: src_geom, dst_geom
   type(EsmfRegridderParam), intent(in) :: regridder_param
   type(ESMF_Field), optional, intent(in) :: coord_field
   class(ComponentDriver), optional, pointer, intent(in) :: coord_coupler
   type(NormalizationMetadata), optional, intent(in) :: norm_metadata
   
   transform%src_geom = src_geom
   transform%dst_geom = dst_geom
   
   ! Standard regridder creation
   transform%regridder = make_regridder(src_geom, dst_geom, regridder_param, _RC)
   
   ! Store normalization info if provided
   if (present(coord_field) .and. present(coord_coupler) .and. &
       present(norm_metadata)) then
      transform%has_normalization = .true.
      transform%coord_field = coord_field
      transform%coord_coupler => coord_coupler
      transform%norm_metadata = norm_metadata
   end if
end function
```

**Update Method:**
```fortran
subroutine update(this, importState, exportState, clock, rc)
   class(RegridTransform), intent(inout) :: this
   type(ESMF_State), intent(inout) :: importState, exportState
   type(ESMF_Clock), intent(in) :: clock
   integer, optional, intent(out) :: rc
   
   integer :: status
   type(ESMF_Field) :: field_in, field_out
   real, pointer :: data_in(:,:,:), data_out(:,:,:), data_temp(:,:,:)
   real, pointer :: coord_data(:,:,:)
   real, allocatable :: dp(:,:,:)
   real :: scale
   integer :: i, j, k, km
   
   ! Get fields from coupler states
   call ESMF_StateGet(importState, COUPLER_IMPORT_NAME, field=field_in, _RC)
   call ESMF_StateGet(exportState, COUPLER_EXPORT_NAME, field=field_out, _RC)
   
   if (this%has_normalization) then
      ! Run coordinate coupler to update values
      if (associated(this%coord_coupler)) then
         call this%coord_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      end if
      
      ! Get coordinate data (e.g., PLE - pressure level edges)
      call ESMF_FieldGet(this%coord_field, farrayPtr=coord_data, _RC)
      
      ! Compute layer thickness: dp(k) = coord(k+1) - coord(k)
      call ESMF_FieldGet(field_in, farrayPtr=data_in, _RC)
      km = size(data_in, 3)  ! Number of vertical levels
      allocate(dp(size(data_in,1), size(data_in,2), km), _STAT)
      
      do k = 1, km
         dp(:,:,k) = coord_data(:,:,k+1) - coord_data(:,:,k)
      end do
      
      ! Get normalization scale factor (e.g., 1/g for DELP)
      scale = this%norm_metadata%get_normalization_scale()
      
      ! Allocate temporary array for normalized data
      allocate(data_temp(size(data_in,1), size(data_in,2), km), _STAT)
      
      ! Denormalize: [kg/kg] → [kg/m²]
      ! Formula: normalized = field_value * (dp * scale)
      do k = 1, km
         data_temp(:,:,k) = data_in(:,:,k) * (dp(:,:,k) * scale)
      end do
      
      ! Horizontal conservative regrid of normalized field
      call this%regridder%regrid(data_temp, field_in, field_out, _RC)
      
      ! Get regridded data
      call ESMF_FieldGet(field_out, farrayPtr=data_out, _RC)
      
      ! Renormalize: [kg/m²] → [kg/kg]
      ! Formula: field_value = normalized / (dp * scale)
      ! Note: Using same dp field - assumes dp doesn't vary horizontally
      ! (valid for pressure coordinates in typical atmospheric models)
      do k = 1, km
         where (abs(dp(:,:,k)) > epsilon(dp))
            data_out(:,:,k) = data_out(:,:,k) / (dp(:,:,k) * scale)
         elsewhere
            data_out(:,:,k) = 0.0  ! or MAPL_UNDEF
         end where
      end do
      
      deallocate(data_temp)
      deallocate(dp)
      
   else
      ! No normalization - direct regrid
      call this%regridder%regrid(field_in, field_out, _RC)
   end if
   
   _RETURN(_SUCCESS)
   _UNUSED_DUMMY(clock)
end subroutine update
```

**Tests:**
- Update `Test_RegridTransform.pf`:
  - Test normalized conservative horizontal regrid
  - Verify mass conservation: ∑(field_in * dp_in) ≈ ∑(field_out * dp_out)
  - Test without normalization (non-conservative case)
  - Test division by zero handling
  - Test with different scale factors
  - Test round-trip conservation

**Acceptance Criteria:**
- [ ] RegridTransform performs internal denormalization before regrid
- [ ] RegridTransform performs internal renormalization after regrid
- [ ] Mass conservation verified in tests
- [ ] No normalization overhead when not needed
- [ ] Coordinate coupler properly invoked
- [ ] Division by zero handled gracefully

---

### Task 3: Enhance GeomAspect to Support Integrated Normalization

**Effort:** 16 hours  
**Branch:** `feature/4529-geom-integrated-normalization`  
**Dependencies:** Task 1, Task 2  
**Files:**
- `generic3g/specs/GeomAspect.F90`

**Description:**
Modify GeomAspect::make_transform to detect normalization requirements, obtain vertical coordinate fields via coupler pattern, and pass to RegridTransform (which now supports integrated normalization from Task 2).

**GeomAspect Changes:**
```fortran
function make_transform(src, dst, other_aspects, rc) result(transform)
   class(ExtensionTransform), allocatable :: transform
   class(GeomAspect), intent(in) :: src
   class(StateItemAspect), intent(in) :: dst
   type(AspectMap), target, intent(in) :: other_aspects
   integer, optional, intent(out) :: rc
   
   integer :: status
   type(GeomAspect) :: dst_
   type(EsmfRegridderParam) :: regridder_param
   type(NormalizationAspect) :: norm_aspect
   type(VerticalGridAspect) :: vert_aspect
   class(VerticalGrid), pointer :: vert_grid
   type(NormalizationMetadata) :: norm_metadata
   type(NormalizationType) :: norm_type
   type(ESMF_Field) :: coord_field
   class(ComponentDriver), pointer :: coord_coupler
   character(:), allocatable :: physical_dimension
   logical :: needs_normalization
   
   dst_ = to_GeomAspect(dst, _RC)
   
   if (src%is_mirror()) then
      allocate(transform, source=ExtendTransform())
      _RETURN(_SUCCESS)
   end if
   
   regridder_param = get_regridder_param(src, dst_, _RC)
   
   ! Check if normalization needed for conservative regridding
   needs_normalization = .false.
   if (regridder_param%is_conservative()) then
      norm_aspect = to_NormalizationAspect(other_aspects, status)
      if (status == _SUCCESS) then
         norm_metadata = norm_aspect%get_metadata()
         norm_type = norm_metadata%get_normalization_type()
         needs_normalization = (norm_type /= NORMALIZE_NONE)
      end if
   end if
   
   if (needs_normalization) then
      ! Get vertical grid
      vert_aspect = to_VerticalGridAspect(other_aspects, _RC)
      vert_grid => vert_aspect%get_vertical_grid(_RC)
      
      ! Determine physical dimension from normalization type
      select case (norm_type%value)
      case (NORMALIZE_DELP)
         physical_dimension = "pressure"
      case (NORMALIZE_DZ)
         physical_dimension = "height"
      case default
         _FAIL("Unknown normalization type")
      end select
      
      ! Get coordinate field with coupler
      coord_field = vert_grid%get_coordinate_field(physical_dimension, &
                                                    other_aspects, &
                                                    coupler=coord_coupler, _RC)
      
      ! Create transform with normalization support
      allocate(transform, source=RegridTransform(src%geom, dst_%geom, &
                                                 regridder_param, &
                                                 coord_field, coord_coupler, &
                                                 norm_metadata))
   else
      ! Standard regridding without normalization
      allocate(transform, source=RegridTransform(src%geom, dst_%geom, &
                                                 regridder_param))
   end if
   
   _RETURN(_SUCCESS)
end function make_transform
```

**Tests:**
- Update `Test_GeomAspect.pf`:
  - Test regrid transform creation with normalization
  - Test regrid transform creation without normalization
  - Test conservative regrid queries NormalizationAspect
  - Test non-conservative regrid skips normalization

**Acceptance Criteria:**
- [ ] GeomAspect queries NormalizationAspect when regridding is conservative
- [ ] GeomAspect obtains coordinate field with coupler from VerticalGrid
- [ ] RegridTransform receives normalization metadata and coordinate coupler
- [ ] Non-conservative regridding skips normalization overhead
- [ ] Tests verify correct transform creation in all scenarios
---

### Task 4: Enhance VerticalRegridTransform for Consistency

**Effort:** 12 hours  
**Branch:** `feature/4530-vertical-integrated-normalization`  
**Dependencies:** Task 1, Task 2  
**Files:**
- `generic3g/transforms/VerticalRegridTransform.F90`
- `generic3g/specs/VerticalGridAspect.F90`

**Description:**
Update VerticalRegridTransform to use the same integrated normalization pattern as RegridTransform. Much of the infrastructure is already in place from the original plan's Task 2.6.

**Key Changes:**
- Query `NormalizationAspect` instead of `QuantityTypeAspect` (line 261)
- Compute `dp_src` and `dp_dst` from coordinate fields (PLE) consistently
- Same fused normalization pattern as horizontal transform

**Updated Logic:**
```fortran
subroutine update(this, importState, exportState, clock, rc)
   ! ... existing setup ...
   
   ! Check if normalization needed
   needs_normalization = .false.
   if (this%regrid_param%method == VERTICAL_REGRID_CONSERVATIVE) then
      ! Check if field has normalization metadata (stored during construction)
      needs_normalization = this%has_normalization
   end if
   
   if (needs_normalization) then
      ! Run coordinate couplers
      if (associated(this%v_in_coupler)) then
         call this%v_in_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      end if
      if (associated(this%v_out_coupler)) then
         call this%v_out_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      end if
      
      ! Compute layer thicknesses from coordinate fields
      dp_src = compute_layer_thickness(this%v_in_field)
      dp_dst = compute_layer_thickness(this%v_out_field)
      
      ! Denormalize: [kg/m²] → [kg/(m²·Pa)]
      data_temp = data_in / dp_src
      
      ! Conservative vertical regrid with overlap-based weights
      call this%regrid_conservative(data_temp, data_out_temp, dp_src, dp_dst, _RC)
      
      ! Renormalize: [kg/(m²·Pa)] → [kg/m²]
      data_out = data_out_temp * dp_dst
   else
      ! Standard vertical regrid (linear interpolation or other method)
      call this%regrid_standard(data_in, data_out, _RC)
   end if
end subroutine
```

**Tests:**
- Update `Test_VerticalRegridTransform.pf`:
  - Test fused normalization for conservative vertical regrid
  - Verify column mass conservation
  - Test consistency with horizontal transform pattern
  - Test without normalization

**Acceptance Criteria:**
- [ ] VerticalRegridTransform queries NormalizationAspect
- [ ] Fused normalization uses same pattern as RegridTransform
- [ ] dp computed consistently from coordinate fields
- [ ] Column mass conservation verified
- [ ] No code duplication between horizontal and vertical patterns

---

### Task 5: Integration Testing

**Effort:** 16 hours  
**Branch:** `feature/4531-integrated-normalization-tests`  
**Dependencies:** Task 2, Task 3, Task 4  
**Files:**
- `generic3g/tests/Test_IntegratedNormalization.pf` (new)
- Update `generic3g/tests/Test_3DConservativeMixingRatio.pf`

**Description:**
Comprehensive integration tests for the full pipeline with integrated normalization.

**Test Scenarios:**

1. **Matching Grids - No Extensions Created**
   ```fortran
   ! Setup: Import and export have same grid, same normalization
   ! Expected: Zero extension fields created
   ! Verify: Only 1 field in memory (not 3)
   ```

2. **Different Grids - Transparent Normalization**
   ```fortran
   ! Setup: Different grids, same normalization
   ! Expected: Only regrid transforms created (no normalization extensions)
   ! Verify: Field maintains [kg/kg] throughout, mass conserved
   ```

3. **Full 3D Conservative Pipeline**
   ```fortran
   ! Setup: Different horizontal and vertical grids
   ! Expected: Both transforms use integrated normalization
   ! Verify: Global mass conservation, correct units throughout
   ```

4. **Non-Conservative Fallback**
   ```fortran
   ! Setup: Bilinear regridding
   ! Expected: No normalization overhead
   ! Verify: Standard regridding path used
   ```

5. **True Normalization Mismatch**
   ```fortran
   ! Setup: Import wants [kg/kg], export provides [kg/m³]
   ! Expected: NormalizationAspect creates actual transform
   ! Verify: Correct conversion between unit systems
   ```

**Memory Profiling:**
```fortran
@test
subroutine test_memory_efficiency_matching_grids()
   ! Count extensions before and after connection
   initial_extensions = count_extensions()
   
   ! Connect with matching grids and normalization
   call connect_import_to_export(same_grid=.true., same_norm=.true.)
   
   final_extensions = count_extensions()
   
   ! Should create zero extensions (no regrid, no normalization)
   @assertEqual(0, final_extensions - initial_extensions)
end subroutine

@test
subroutine test_memory_efficiency_different_grids()
   ! Count extensions before and after connection
   initial_extensions = count_extensions()
   
   ! Connect with different grids but same normalization
   call connect_import_to_export(same_grid=.false., same_norm=.true.)
   
   final_extensions = count_extensions()
   
   ! Should create only 1 extension for regrid (not 3 for norm+regrid+denorm)
   @assertEqual(1, final_extensions - initial_extensions)
end subroutine
```

**Conservation Verification:**
```fortran
@test
subroutine test_global_mass_conservation_3d()
   ! Create realistic 3D mixing ratio field
   field_in = create_co2_field(grid_in)
   delp_in = get_delp(grid_in)
   
   ! Compute initial total mass
   mass_initial = sum(field_in * delp_in / GRAV)
   
   ! Regrid to different horizontal and vertical grid
   field_out = regrid_3d_conservative(field_in, grid_in, grid_out)
   delp_out = get_delp(grid_out)
   
   ! Compute final total mass
   mass_final = sum(field_out * delp_out / GRAV)
   
   ! Verify conservation (within numerical tolerance)
   rel_error = abs(mass_final - mass_initial) / mass_initial
   @assertLessThan(rel_error, 1.0e-10)
end subroutine
```

**Acceptance Criteria:**
- [ ] Memory efficiency verified (no extra extensions in common case)
- [ ] Mass conservation verified for full 3D pipeline
- [ ] Non-conservative path confirmed to skip normalization
- [ ] True normalization mismatches handled correctly
- [ ] Tests cover all critical scenarios
- [ ] Performance benchmarks show improvement

---

### Task 6: Update Aspect Registry and Ordering

**Effort:** 6 hours  
**Branch:** `feature/4532-update-aspect-ordering`  
**Dependencies:** Task 1  
**Files:**
- `generic3g/specs/StateItemAspect_registry.F90`
- `generic3g/specs/FieldClassAspect.F90`
- `generic3g/specs/FieldBundleClassAspect.F90`

**Description:**
Update aspect registry to use single NormalizationAspect, remove ExportNormalization and ImportNormalization IDs, and verify aspect ordering is correct.

**Changes:**
```fortran
! In StateItemAspect_registry.F90

! Remove these:
! type(AspectId), parameter :: EXPORT_NORMALIZATION_ASPECT_ID = AspectId(X)
! type(AspectId), parameter :: IMPORT_NORMALIZATION_ASPECT_ID = AspectId(Y)

! Add this:
type(AspectId), parameter :: NORMALIZATION_ASPECT_ID = AspectId(Z)

! Update aspect ordering
type(AspectId), parameter :: FIELD_ASPECT_ORDER(*) = [ &
   CLASS_ASPECT_ID, &
   ATTRIBUTES_ASPECT_ID, &
   UNGRIDDED_DIMS_ASPECT_ID, &
   QUANTITY_TYPE_ASPECT_ID, &
   CONSERVATION_ASPECT_ID, &
   NORMALIZATION_ASPECT_ID, &      ! Single aspect
   GEOM_ASPECT_ID, &                ! Maintains normalization internally
   VERTICAL_GRID_ASPECT_ID, &       ! Maintains normalization internally
   UNITS_ASPECT_ID, &
   TYPEKIND_ASPECT_ID &
]
```

**Tests:**
- Verify aspect order is deterministic
- Test that all aspects are registered
- Test that aspect IDs don't conflict

**Acceptance Criteria:**
- [ ] Single NORMALIZATION_ASPECT_ID registered
- [ ] EXPORT/IMPORT_NORMALIZATION_ASPECT_IDs removed
- [ ] Aspect ordering updated and tested
- [ ] No aspect ID conflicts
- [ ] All affected tests updated

---

### Task 7: Documentation and Migration Guide

**Effort:** 8 hours  
**Branch:** `feature/4533-integrated-normalization-docs`  
**Dependencies:** All above tasks  
**Files:**
- `docs/integrated_normalization.md` (new)
- Update `.opencode/plans/CONSERVATIVE_REGRIDDING_IMPLEMENTATION_PLAN.md`

**Description:**
Comprehensive documentation of the integrated normalization architecture and migration guide for existing code.

**Documentation Sections:**

1. **Architecture Overview**
   - Why integrated normalization
   - How transforms maintain export normalization
   - Auxiliary fields from vertical coordinates
   - Memory efficiency benefits

2. **Developer Guide**
   - How to use NormalizationAspect
   - How transforms query normalization requirements
   - How to add new normalization types
   - Debugging tips

3. **Migration Guide**
   - Changes from original plan
   - How existing code is affected
   - ExportNormalization → NormalizationAspect mapping
   - ImportNormalization removed (handled internally)

4. **Performance Characteristics**
   - Memory savings in common cases
   - Computational overhead analysis
   - When normalization is skipped

5. **Testing Approach**
   - How to verify conservation
   - How to test normalization transparency
   - Memory profiling techniques

**Update Original Plan:**
Add note to top of `CONSERVATIVE_REGRIDDING_IMPLEMENTATION_PLAN.md`:
```markdown
## SUPERSEDED - See Integrated Normalization Plan

**Note:** The normalization aspects of this plan (Phases 2-3) have been superseded 
by the [Integrated Normalization Plan](INTEGRATED_NORMALIZATION_PLAN.md) as of 
2026-03-16. The new design integrates normalization directly into regridding 
transforms, eliminating unnecessary extension fields and improving memory efficiency.

**What remains valid:**
- Phase 1: QuantityTypeAspect and infrastructure (completed)
- ConservationAspect architecture (completed)
- General testing strategy and principles
- Phase 4-5: Future work on mixing ratio conversions

**What changed:**
- ExportNormalization + ImportNormalization → single NormalizationAspect
- Normalization logic moved into RegridTransform and VerticalRegridTransform
- Auxiliary fields (DELP) obtained from vertical coordinates via coupler pattern
- No separate normalization extensions in common case (memory efficiency)

Please refer to the new plan for current implementation approach.
```

**Acceptance Criteria:**
- [ ] Comprehensive architecture documentation written
- [ ] Developer guide covers all common scenarios
- [ ] Migration guide explains changes clearly
- [ ] Original plan updated with superseding note
- [ ] Examples and code snippets included
- [ ] Performance characteristics documented

---

## GitHub Issue Structure

### Epic Issue

**Title:** Implement Integrated Normalization for Conservative Regridding

**Description:**
Implement integrated normalization within regridding transforms to eliminate unnecessary extension fields and improve memory efficiency. This replaces the separate normalization aspect transforms from the original conservative regridding plan.

**Problem:**
The original approach created 2 extension fields even when grids matched but needed normalization for conservative regridding. This could nearly triple memory usage in common scenarios.

**Solution:**
Integrate normalization directly into RegridTransform and VerticalRegridTransform. Transforms maintain export normalization throughout, using canonical normalization only internally during the actual regridding operation.

**Benefits:**
- Memory efficiency: No extra extensions in common case
- Transparency: Transforms maintain input normalization
- Consistency: Same pattern for horizontal and vertical
- Simplicity: Single NormalizationAspect instead of Export+Import

**Tasks:**
- [ ] #XXXX - Update NormalizationAspect structure
- [ ] #XXXX - Enhance GeomAspect for integrated normalization
- [ ] #XXXX - Implement integrated normalization in RegridTransform
- [ ] #XXXX - Enhance VerticalRegridTransform for consistency
- [ ] #XXXX - Integration testing
- [ ] #XXXX - Update aspect registry and ordering
- [ ] #XXXX - Documentation and migration guide

**Related:**
- Original plan: `.opencode/plans/CONSERVATIVE_REGRIDDING_IMPLEMENTATION_PLAN.md`
- New plan: `.opencode/plans/INTEGRATED_NORMALIZATION_PLAN.md`
- Epic #4436 (Conservative Regridding Support)

**Labels:** `enhancement`, `conservative-regridding`, `architecture`

---

### Sub-Issues (Templates)

**Task 1 Issue Template:**
```
Title: Consolidate to single NormalizationAspect

Description:
Consolidate ExportNormalization and ImportNormalization into a single NormalizationAspect. 
Remove the is_inverse flag since transforms now handle normalization internally.

Key Changes:
- Rename ExportNormalization.F90 → NormalizationAspect.F90
- Delete ImportNormalization.F90
- Update aspect registry
- Modify make_transform to only create transform for true mismatches

Acceptance Criteria:
- [ ] Single NormalizationAspect replaces Export/Import versions
- [ ] Aspect creates NullTransform when normalizations match
- [ ] All existing tests pass with updated aspect

Branch: feature/4527-unified-normalization-aspect
Effort: 8 hours
Epic: #XXXX
```

**Task 2 Issue Template:**
```
Title: Enhance GeomAspect to support integrated normalization

Description:
Modify GeomAspect::make_transform to detect normalization requirements, obtain 
vertical coordinate fields via coupler pattern, and pass to RegridTransform.

Key Changes:
- Query NormalizationAspect from other_aspects
- Get VerticalGrid and coordinate field (PLE) with coupler
- Pass normalization metadata and coordinate coupler to RegridTransform
- Skip normalization for non-conservative regridding

Acceptance Criteria:
- [ ] GeomAspect queries NormalizationAspect when regridding is conservative
- [ ] GeomAspect obtains coordinate field with coupler from VerticalGrid
- [ ] RegridTransform receives normalization metadata and coordinate coupler

Branch: feature/4528-geom-integrated-normalization
Effort: 16 hours
Epic: #XXXX
Dependencies: Task 1
```

*(Similar templates for Tasks 3-7)*

---

## Timeline and Dependencies

```
Task 1 (8h)  ──┐
               ├──> Task 2 (16h) ──┐
               │                    ├──> Task 5 (16h) ──> Task 7 (8h)
               ├──> Task 3 (20h) ──┤
               │                    │
               └──> Task 4 (12h) ──┘
                    Task 6 (6h) ────────────────────────┘

Total Sequential: ~86 hours (2-3 weeks)
With Parallelization: ~60 hours (1.5-2 weeks)
```

**Critical Path:** Task 1 → Task 3 → Task 5 → Task 7

**Parallel Opportunities:**
- Task 2, 3, 4, 6 can start after Task 1
- Task 6 is independent after Task 1

---

## Testing Strategy

### Unit Tests
- Each modified aspect has corresponding test file updated
- Transform tests verify internal normalization logic
- Aspect ordering tests ensure correct registration

### Integration Tests
- Full 3D conservative regridding pipeline
- Memory efficiency verification
- Mass conservation validation
- Performance benchmarks

### Regression Tests
- All existing conservative regridding tests must pass
- Phase 1 tests (QuantityTypeAspect) must pass unchanged
- Conservation tests must show same or better accuracy

### Performance Tests
- Memory usage profiling (common case should show ~3x reduction)
- Runtime overhead measurement (should be negligible or improved)
- Comparison with original approach

---

## Success Criteria

### Functional
- [ ] All tests pass with new integrated normalization
- [ ] Mass conservation verified in all test scenarios
- [ ] No extensions created when grids match and normalization matches
- [ ] True normalization mismatches still handled correctly

### Performance
- [ ] Memory usage reduced in common cases (matching grids)
- [ ] Runtime overhead negligible or improved
- [ ] Conservative regridding accuracy maintained or improved

### Code Quality
- [ ] No code duplication between horizontal and vertical patterns
- [ ] Clear separation of concerns (aspects vs transforms)
- [ ] Comprehensive documentation and examples
- [ ] Clean git history with logical feature branches

### Documentation
- [ ] Architecture clearly explained
- [ ] Migration guide helps developers understand changes
- [ ] Examples cover common use cases
- [ ] Original plan properly annotated as superseded

---

## Risks and Mitigations

### Risk: Breaking Changes to Existing Code
**Mitigation:** 
- Comprehensive regression testing
- Migration guide for affected code
- Backward compatibility where possible
- Staged rollout with feature branches

### Risk: Complexity in Transform Logic
**Mitigation:**
- Clear code comments and documentation
- Consistent pattern between horizontal and vertical
- Unit tests for each normalization path
- Code review focused on maintainability

### Risk: Performance Regression
**Mitigation:**
- Performance benchmarking before and after
- Profiling to identify bottlenecks
- Optimization of hot paths (layer thickness computation)
- Option to disable for debugging

### Risk: Conservation Accuracy Issues
**Mitigation:**
- Extensive conservation testing
- Comparison with original approach
- Validation against known analytical solutions
- Peer review of numerical methods

---

## Future Work (Out of Scope)

The following items from the original plan remain valid but are not addressed by this plan:

1. **Phase 4:** Dry mass and volume mixing ratios
   - MixingRatioBasisAspect
   - Molecular weight handling
   - Wet/dry conversions

2. **Phase 5:** ExtData integration
   - Heuristics for auxiliary fields
   - Cross-type conversions
   - Field derivation

3. **Additional Optimizations:**
   - Caching of computed dp fields
   - GPU acceleration for normalization
   - Lazy evaluation of auxiliary fields

These will be addressed in future plans after integrated normalization is proven successful.
