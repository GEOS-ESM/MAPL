# Unused Unprefixed Entities Analysis - Issue #4999

**Date**: 2026-05-26  
**Scope**: Client repos (GMAO_Shared, FVdycoreCubed_GridComp, GEOS_Util, GEOSchem_GridComp, GOCART)

## Summary

Out of **82 unprefixed entities** exported through leaf modules in MAPL.F90, **74 are UNUSED** (90%) in ported client repositories.

## Unused Entities by Module (can be hidden)

### Utils Layer Modules (Already Addressed)

**mapl_KeywordEnforcer_mod** (0 uses):
- `KeywordEnforcer` ✓

**mapl_Shmem_mod** (0 uses):
- `GetSharedMemory` ✓
- `ReleaseSharedMemory` ✓

**mapl_MinMax_mod** (0 uses):
- `IntegerMinMax` ✓
- `RealMinMax` ✓
- `Real64MinMax` ✓

### MP Utils Layer (0 uses except DownBit)

**mapl_SplitCommunicator_mod** (0 uses):
- `SplitCommunicator` ✓
- `NULL_SUBCOMMUNICATOR_NAME` ✓

**mapl_SimpleCommSplitter_mod** (0 uses):
- `SimpleCommSplitter` ✓

**mapl_CommGroupDescription_mod** (0 uses):
- `CommGroupDescription` ✓

**mapl_AbstractCommSplitter_mod** (0 uses):
- `AbstractCommSplitter` ✓

**mapl_Downbit_mod** (2 uses):
- `DownBit` ❌ **USED** - must keep

### Base Layer (0 uses except StrTemplate and WRITE_PARALLEL)

**mapl_base_mod** - Unused (can hide):
- `set_reference_clock` ✓
- `fill_time_dict` ✓
- `fill_grads_template` ✓
- `fill_grads_template_esmf` ✓
- `LocalDisplacementEnsemble` ✓
- `ArrDescr` ✓
- `ArrDescrInit` ✓
- `ArrDescrSet` ✓

**mapl_base_mod** - Used (must keep):
- `StrTemplate` ❌ **USED** (32 uses)
- `WRITE_PARALLEL` ❌ **USED** (8 uses)

**mapl_FileMetadataUtils_mod** (0 uses):
- `FileMetadataUtil` ✓

### Superstructure / Generic Layer (all 0 uses)

**mapl_ComponentSpec_mod** (0 uses):
- `ComponentSpec` ✓
- `MiscellaneousComponentSpec` ✓
- `CheckpointControl` ✓

**mapl_RestartHandler_mod** (0 uses):
- `RestartHandler` ✓

**mapl_StateArithmeticParser_mod** (0 uses):
- `parser_variables_in_expression` ✓
- `CheckSyntax` ✓
- `RealNum` ✓
- `LowCase` ✓

### ESMF Utilities Layer (all 0 uses)

**mapl_ESMF_Time_Utilities_mod** (0 uses):
- `check_compatibility` ✓
- `interval_is_all_zero` ✓
- `sub_time_in_datetime` ✓

**mapl_SimpleAlarm_mod** (0 uses):
- `SimpleAlarm` ✓

**mapl_FieldPointerUtilities_mod** (0 uses):
- `FieldsHaveUndef` ✓
- `GetFieldsUndef` ✓
- `assign_fptr` ✓
- `FieldGetLocalElementCount` ✓
- `FieldGetLocalSize` ✓
- `FieldGetCptr` ✓
- `FieldClone` ✓
- `FieldsAreConformable` ✓
- `FieldsAreBroadcastConformable` ✓
- `FieldsAreSameTypeKind` ✓
- `FieldCopy` ✓
- `FieldCopyBroadcast` ✓
- `FieldSameData` ✓

**mapl_ISO8601_DateTime_mod** (0 uses):
- `convert_ISO8601_to_integer_time` ✓
- `convert_ISO8601_to_integer_date` ✓

### Regridder Layer

**mapl_EsmfRegridder_mod** (0 uses):
- `EsmfRegridder` ✓
- `EsmfRegridderParam` ✓
- `make_EsmfRegridderParam` ✓

**mapl_RegridderMethods_mod** - Unused (can hide):
- `REGRID_HINT_LOCAL` ✓
- `REGRID_HINT_FILE_WEIGHTS` ✓
- `REGRID_HINT_COMPUTE_TRANSPOSE` ✓
- `REGRID_METHOD_BILINEAR_MONOTONIC` ✓
- `REGRID_METHOD_BILINEAR_ROTATE` ✓
- `REGRID_METHOD_CONSERVE_MONOTONIC` ✓
- `REGRID_METHOD_VOTE` ✓
- `REGRID_METHOD_FRACTION` ✓
- `REGRID_METHOD_CONSERVE_2ND` ✓
- `REGRID_METHOD_PATCH` ✓
- `REGRID_METHOD_NEAREST_STOD` ✓
- `REGRID_METHOD_CONSERVE_HFLUX` ✓
- `UNSPECIFIED_REGRID_METHOD` ✓
- `regrid_method_string_to_int` ✓
- `regrid_method_int_to_string` ✓

**mapl_RegridderMethods_mod** - Used (must keep):
- `REGRID_METHOD_BILINEAR` ❌ **USED** (1 use)
- `REGRID_METHOD_CONSERVE` ❌ **USED** (1 use)

### HConfig Layer (all 0 uses)

**mapl_HConfigAs_mod** (0 uses):
- `HConfigAsItemType` ✓
- `HConfigAsStateIntent` ✓
- `HConfigAsTime` ✓
- `HConfigAsTimeInterval` ✓
- `HConfigAsTimeRange` ✓
- `HConfigAsStringVector` ✓

### Geom Layer (0 uses)

**mapl_GridGetGlobal_mod** (0 uses):
- `GridGetGlobalCellCountPerDim` ✓

## Used Entities (MUST KEEP - 8 total)

1. `DownBit` (2 uses) - mapl_Downbit_mod
2. `StrTemplate` (32 uses) - mapl_base_mod
3. `WRITE_PARALLEL` (8 uses) - mapl_base_mod
4. `String` (409 uses) - mapl_Utils_API_mod
5. `split` (26 uses) - mapl_Utils_API_mod
6. `lowercase` (32 uses) - mapl_Utils_API_mod
7. `uppercase` (66 uses) - mapl_Utils_API_mod
8. `REGRID_METHOD_BILINEAR` (1 use) - mapl_RegridderMethods_mod
9. `REGRID_METHOD_CONSERVE` (1 use) - mapl_RegridderMethods_mod

## Implementation Strategy

### Phase 1: Utils Layer ✅ (Already Completed)
- Created `utils/API.F90` with only: clauses
- Exports: String, split, lowercase, uppercase, mapl_os_mod entities
- Hides: All unused StringUtilities, FileSystemUtilities, DSO_Utilities, DirPath

### Phase 2: Create Additional Layer API Umbrellas

Need to create API umbrellas or add only: clauses for:

1. **MP Utils layer leaf modules** - Create mp_utils API additions
2. **Base layer** - Add to existing base/API.F90
3. **Superstructure layer** - Handle ComponentSpec, RestartHandler, StateArithmeticParser
4. **ESMF layer** - Handle ESMF_Time_Utilities, FieldPointerUtilities, ISO8601_DateTime, SimpleAlarm
5. **Regridder layer** - Add to RegridderMethods module or API
6. **HConfig layer** - Add to HConfig API
7. **Geom layer** - Add to Geom API

### Recommendation

Split implementation into separate PRs by layer:
- PR #1: Utils layer (already done, in current branch)
- PR #2: MP Utils + Base layers
- PR #3: Superstructure (ComponentSpec, RestartHandler, StateArithmeticParser)
- PR #4: ESMF utilities
- PR #5: Regridder + HConfig layers
- PR #6: Geom layer

This allows incremental progress and easier review/testing.
