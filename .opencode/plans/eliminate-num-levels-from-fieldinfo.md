# Plan: Eliminate num_levels from FieldInfo Layer

**Created**: 2026-03-05  
**Status**: In Progress  
**Branch**: `feature/4487-eliminate-num-levels-from-fieldinfo`  
**Issue**: https://github.com/GEOS-ESM/MAPL/issues/4487

## Overview

**Goal**: Remove `num_levels` storage from FieldInfo and require all vertical fields to use `vgrid_id` + `vert_staggerloc`. The number of levels will be derived on-demand from the vertical grid.

**Background**: Currently, `num_levels` is stored in two ways in `FieldInfo.F90`:
1. `KEY_NUM_LEVELS` - Direct storage of number of levels
2. `KEY_NUM_VGRID_LEVELS` - Derived value based on `num_levels` + `vert_staggerloc`

This dual storage creates complications, especially when `vgrid_id` is passed without `num_levels`. The cleaner approach is to eliminate `num_levels` from the FieldInfo layer entirely and derive it on-demand from `vgrid_id` + `vert_staggerloc`.

**Scope**: 
- 6 production code files
- 8 test files  
- ~70-80 locations to update
- Core infrastructure changes in FieldInfo, FieldCreate, FieldDelta

## Strategy

- **Store in FieldInfo**: Only `vgrid_id` + `vert_staggerloc`
- **Derive when needed**: Use `VerticalGridManager` to look up the grid and compute `num_levels = vert_staggerloc%get_num_levels(vgrid%get_num_levels())`
- **Handle at higher layers**: FieldSet/Get and FieldBundleSet/Get do the derivation
- **No backward compatibility**: All vgrids must have IDs (per requirements)
- **FieldCreate change**: No longer accepts `num_levels` parameter

---

## Phase 1: Fix Production Code Bootstrap Patterns

### 1.1 Fix PrimaryExport.F90 ⚠️ CRITICAL BUG
**File**: `gridcomps/ExtData3G/PrimaryExport.F90`

**Current Code (Lines 217-220)**:
```fortran
vertical_grid => vgrid_manager%create_grid(BasicVerticalGridSpec(...), _RC)
call FieldBundleSet(bundle, geom=esmfgeom, units='<unknown>', &
        typekind=ESMF_TYPEKIND_R4, num_levels=this%vcoord%num_levels, &
        vert_staggerloc=VERTICAL_STAGGER_CENTER, _RC)
```

**Fix**: Pass the created `vertical_grid` to FieldBundleSet:
```fortran
vertical_grid => vgrid_manager%create_grid(BasicVerticalGridSpec(...), _RC)
call FieldBundleSet(bundle, geom=esmfgeom, units='<unknown>', &
        typekind=ESMF_TYPEKIND_R4, vgrid=vertical_grid, &
        vert_staggerloc=VERTICAL_STAGGER_CENTER, _RC)
```

Same fix needed around **line 171** in `make_bundle_for_spec`.

### 1.2 Update FieldBundleSet.F90
**File**: `field_bundle/FieldBundleSet.F90`

**Line 108**: Already passes `vgrid` when present - verify this path is always used and `num_levels` alone is never passed.

---

## Phase 2: Update Core Infrastructure

### 2.1 Modify FieldInfo.F90
**File**: `field/FieldInfo.F90`

**Changes**:

1. **Remove constants** (lines 63-64):
   ```fortran
   ! DELETE:
   character(*), parameter :: KEY_NUM_LEVELS = "/num_levels"
   character(*), parameter :: KEY_NUM_VGRID_LEVELS = "/num_vgrid_levels"
   ```

2. **Remove num_levels parameter from field_info_set_internal** (line 101):
   ```fortran
   ! DELETE from signature:
   integer, optional, intent(in) :: num_levels
   ```

3. **Remove num_levels storage** (lines 163-165):
   ```fortran
   ! DELETE:
   if (present(num_levels)) then
      call MAPL_InfoSet(info, namespace_ // KEY_NUM_LEVELS, num_levels, _RC)
   end if
   ```

4. **Remove backward compatibility code** (lines 176-190):
   ```fortran
   ! DELETE entire block marked "Delete later - needed for transition"
   ```

5. **Modify field_info_get_internal** to derive num_levels (lines 301-331):
   ```fortran
   ! REPLACE with derivation logic:
   if (present(num_levels) .or. present(num_vgrid_levels)) then
      ! Get vgrid_id (may already be retrieved above)
      integer :: vgrid_id_local
      call esmf_InfoGet(info, namespace_ // KEY_VGRID_ID, &
           vgrid_id_local, default=VERTICAL_GRID_NOT_FOUND, _RC)
      
      if (vgrid_id_local /= VERTICAL_GRID_NOT_FOUND) then
         type(VerticalGridManager), pointer :: vgrid_manager
         class(VerticalGrid), pointer :: vgrid_ptr
         type(VerticalStaggerLoc) :: vert_staggerloc_local
         integer :: num_vgrid_levels_local, num_levels_local
         character(:), allocatable :: vert_staggerloc_str
         
         ! Get vertical stagger location
         call MAPL_InfoGet(info, namespace_ // KEY_VERT_STAGGERLOC, &
              vert_staggerloc_str, _RC)
         vert_staggerloc_local = VerticalStaggerLoc(vert_staggerloc_str)
         
         ! Derive from vgrid
         vgrid_manager => get_vertical_grid_manager()
         vgrid_ptr => vgrid_manager%get_grid(id=vgrid_id_local, _RC)
         num_vgrid_levels_local = vgrid_ptr%get_num_levels()
         num_levels_local = vert_staggerloc_local%get_num_levels(num_vgrid_levels_local)
         
         if (present(num_levels)) num_levels = num_levels_local
         if (present(num_vgrid_levels)) num_vgrid_levels = num_vgrid_levels_local
      else
         ! No vertical grid
         if (present(num_levels)) num_levels = 0
         if (present(num_vgrid_levels)) num_vgrid_levels = 0
      end if
   end if
   ```

**Add import**: `use mapl3g_VerticalGridManager`

### 2.2 Update FieldCreate.F90
**File**: `field/FieldCreate.F90`

**Remove num_levels parameter entirely** from:
- Interface declarations
- Subroutine signatures  
- All internal uses

**Line 234**: Remove `num_levels` parameter from FieldInfoSetInternal call

**Add validation** (after line 73):
```fortran
if (present(num_levels)) then
   _FAIL('num_levels parameter is deprecated. Use vgrid parameter instead.')
end if
```

**Lines 76, 206, 154-170**: Update to work without num_levels - FieldGet will derive it

### 2.3 Update FieldSet.F90
**File**: `field/FieldSet.F90`

**Line 92**: Remove `num_levels` parameter:
```fortran
call FieldInfoSetInternal(field_info, &
     horizontal_dims_spec=horizontal_dims_spec, &
     vgrid_id=vgrid_id, &
     vert_staggerloc=vert_staggerloc, &
     vert_alignment=vert_alignment, &
     ! num_levels=num_levels, & ! DELETE
     ungridded_dims=ungridded_dims, &
     regridder_param_info=regridder_param_info, &
     _RC)
```

**Line 73**: Update FieldDelta creation (after FieldDelta changes)

**Remove num_levels parameter** from interface if present.

### 2.4 Update FieldDelta.F90
**File**: `field/FieldDelta.F90`

**Changes**:
1. **Remove num_levels component** (line 30)
2. **Remove num_levels storage** (lines 66, 187)
3. **Simplify or remove compute_num_levels_delta** (lines 132-149) - replace with vgrid_id comparison
4. **Remove num_levels from update call** (line 227)
5. **Update select_ungriddedUbound** (lines 350-389) - derive num_levels via FieldGet if needed

### 2.5 Update FieldBundleInfo.F90
**File**: `field_bundle/FieldBundleInfo.F90`

**Line 233**: Remove `num_levels` parameter from FieldInfoSetInternal call

---

## Phase 3: Fix Unit Tests

All these tests need to be updated to create and pass vertical grids instead of just `num_levels`:

### 3.1 Test_FieldDelta.pf
**Lines to fix**: 39, 71, 110, 149, 194, 234, 328-329

**Pattern**: 
```fortran
! OLD:
call MAPL_FieldSet(f, num_levels=ORIG_VGRID_LEVELS+1, _RC)

! NEW:
vgrid_manager => get_vertical_grid_manager()
vgrid => vgrid_manager%create_grid(BasicVerticalGridSpec(num_levels=ORIG_VGRID_LEVELS+1), _RC)
call MAPL_FieldSet(f, vgrid=vgrid, vert_staggerloc=VERTICAL_STAGGER_CENTER, _RC)
```

### 3.2 Test_RegridderManager.pf
**Line 54**: Add vgrid creation before field creation

### 3.3 Test_FieldBLAS.pf
**Line 406**: Add vgrid creation

### 3.4 Test_VerticalRegridTransform.pf ⚠️ MANY CHANGES
**48+ occurrences** - Systematic replacement needed throughout file

**Pattern**:
```fortran
! OLD:
f = MAPL_FieldCreate(geom, num_levels=km, vert_staggerloc=VERTICAL_STAGGER_EDGE, _RC)

! NEW:
vgrid_manager => get_vertical_grid_manager()
vgrid => vgrid_manager%create_grid(BasicVerticalGridSpec(num_levels=km), _RC)
f = MAPL_FieldCreate(geom, vgrid=vgrid, vert_staggerloc=VERTICAL_STAGGER_EDGE, _RC)
```

### 3.5 Other test files
- `Test_VectorBracketClassAspect.pf`: Line 64
- `Test_Couplers.pf`: Line 146  
- `Test_BracketClassAspect.pf`: Line 64
- `Test_ConfigurableGridComp.pf`: Line 56

All need same pattern: create vgrid, pass to field creation

---

## Phase 4: Update Field Bundle Infrastructure

### 4.1 FieldBundleInfo.F90
Already covered in Phase 2

### 4.2 FieldBundleSet.F90  
**Line 147**: Remove num_levels from FieldBundleInfoSetInternal if needed

### 4.3 FieldBundleDelta.F90
**Lines 216, 254-255**: Should work transparently - FieldGet will derive num_levels

---

## Phase 5: Validation & Testing

After all changes:

1. **Build the code** with all compilers (NAG, gfortran, Intel)
2. **Run full test suite**: `ctest` or equivalent
3. **Specific test cases**:
   - Fields with vertical grids (CENTER, EDGE, NONE stagger)
   - Fields without vertical grids (surface fields)
   - Field bundles with vertical grids
   - Delta operations
   - Regridding operations
4. **Integration tests**: Run higher-level MAPL applications

---

## Files Summary

**Production code files to modify**: 6
1. `field/FieldInfo.F90` ⭐ Core changes
2. `field/FieldCreate.F90`
3. `field/FieldSet.F90`
4. `field/FieldDelta.F90`
5. `field_bundle/FieldBundleInfo.F90`
6. `gridcomps/ExtData3G/PrimaryExport.F90` ⚠️ Bug fix

**Test files to modify**: ~8
1. `field/tests/Test_FieldDelta.pf` (8 occurrences)
2. `generic3g/tests/Test_VerticalRegridTransform.pf` (48+ occurrences)
3. `regridder_mgr/tests/Test_RegridderManager.pf` (1 occurrence)
4. `field/tests/Test_FieldBLAS.pf` (1 occurrence)
5. `generic3g/tests/Test_VectorBracketClassAspect.pf` (1 occurrence)
6. `generic3g/tests/Test_Couplers.pf` (1 occurrence)
7. `generic3g/tests/Test_BracketClassAspect.pf` (1 occurrence)
8. `generic3g/tests/Test_ConfigurableGridComp.pf` (1 occurrence)

**Total estimated changes**: ~70-80 locations

---

## Benefits

✅ Eliminates dual storage (KEY_NUM_LEVELS, KEY_NUM_VGRID_LEVELS)  
✅ Fixes bug where vgrid_id is set but num_levels is not  
✅ Single source of truth: vgrid_id + vert_staggerloc  
✅ Cleaner architecture: derived values computed on-demand  
✅ Removes "transition code" marked for deletion  
✅ **Fixes existing bug in PrimaryExport.F90**

---

## Risks & Mitigations

⚠️ **Large scope** → Systematic, phased approach  
⚠️ **Many test updates** → Use pattern matching to ensure consistency  
⚠️ **Breaking change** → No backward compatibility required (per user)  
⚠️ **Performance** → Minimal (VerticalGridManager caches grids)

---

## Progress Tracking

- [ ] Phase 1: Fix Production Code Bootstrap Patterns
  - [ ] 1.1 Fix PrimaryExport.F90
  - [ ] 1.2 Verify FieldBundleSet.F90
- [ ] Phase 2: Update Core Infrastructure
  - [ ] 2.1 Modify FieldInfo.F90
  - [ ] 2.2 Update FieldCreate.F90
  - [ ] 2.3 Update FieldSet.F90
  - [ ] 2.4 Update FieldDelta.F90
  - [ ] 2.5 Update FieldBundleInfo.F90
- [ ] Phase 3: Fix Unit Tests
  - [ ] 3.1 Test_FieldDelta.pf
  - [ ] 3.2 Test_RegridderManager.pf
  - [ ] 3.3 Test_FieldBLAS.pf
  - [ ] 3.4 Test_VerticalRegridTransform.pf
  - [ ] 3.5 Other test files
- [ ] Phase 4: Update Field Bundle Infrastructure
- [ ] Phase 5: Validation & Testing
  - [ ] Build with all compilers
  - [ ] Run full test suite
  - [ ] Integration tests
