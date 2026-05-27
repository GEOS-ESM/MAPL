# Comprehensive Audit of Unprefixed Entities in MAPL Public API

**Issue**: #4999 (part of #4975 / #4969)  
**Date**: 2026-05-26

## Executive Summary

Total unprefixed public entities across MAPL: **1126**

Of these, **82 unprefixed entities** are exported through modules directly used by `MAPL.F90`, plus additional entities exported through API umbrella modules.

## Methodology

1. Scanned all `.F90` files in MAPL for `public ::` declarations
2. Excluded already-prefixed entities (`mapl_`, `MAPL_`, `ESMF_`, `pfio`, `pFIO`)
3. Excluded operators and assignments
4. Cross-referenced with modules used in `MAPL.F90`

## Unprefixed Entities by Layer

### Distribution Across All Layers

```
366 infrastructure
176 superstructure  
156 utils
124 pfio (external, ignore)
 77 enums
 75 mp_utils
 61 gridcomps (internal, not exported)
 26 pflogger_stub (external, ignore)
 24 udunits2f (external, ignore)
 20 benchmarks (internal, not exported)
 12 base
  5 Apps (internal tools, not exported)
```

### Core MAPL Layers (exported through MAPL.F90)

Focus on: `base`, `enums`, `infrastructure`, `mp_utils`, `superstructure`, `utils`

## Detailed Breakdown: Modules Used Directly in MAPL.F90

### 1. Utils Layer Leaf Modules

**mapl_KeywordEnforcer_mod** (1 entity):
- `KeywordEnforcer` - dummy type for enforcing keyword-only arguments

**mapl_Shmem_mod** (2 entities):
- `GetSharedMemory` 
- `ReleaseSharedMemory`

**mapl_MinMax_mod** (3 entities):
- `IntegerMinMax`
- `RealMinMax`
- `Real64MinMax`

### 2. MP Utils Layer

**mapl_SplitCommunicator_mod** (2 entities):
- `SplitCommunicator`
- `NULL_SUBCOMMUNICATOR_NAME`

**mapl_SimpleCommSplitter_mod** (1 entity):
- `SimpleCommSplitter`

**mapl_CommGroupDescription_mod** (1 entity):
- `CommGroupDescription`

**mapl_AbstractCommSplitter_mod** (1 entity):
- `AbstractCommSplitter`

**mapl_Downbit_mod** (1 entity):
- `DownBit`

### 3. Base Layer

**mapl_base_mod** (10 entities):
- `set_reference_clock`
- `fill_time_dict`
- `fill_grads_template`
- `StrTemplate`
- `fill_grads_template_esmf`
- `LocalDisplacementEnsemble`
- `WRITE_PARALLEL`
- `ArrDescr`
- `ArrDescrInit`
- `ArrDescrSet`

**mapl_FileMetadataUtils_mod** (1 entity):
- `FileMetadataUtil`

### 4. Superstructure / Generic Layer

**mapl_ComponentSpec_mod** (3 entities):
- `ComponentSpec`
- `MiscellaneousComponentSpec`
- `CheckpointControl`

**mapl_RestartHandler_mod** (1 entity):
- `RestartHandler`

**mapl_StateArithmeticParser_mod** (4 entities):
- `parser_variables_in_expression`
- `CheckSyntax`
- `RealNum`
- `LowCase`

### 5. ESMF Utilities Layer

**mapl_ESMF_Time_Utilities_mod** (3 entities):
- `check_compatibility`
- `interval_is_all_zero`
- `sub_time_in_datetime`

**mapl_SimpleAlarm_mod** (1 entity):
- `SimpleAlarm`

**mapl_FieldPointerUtilities_mod** (13 entities):
- `FieldsHaveUndef`
- `GetFieldsUndef`
- `assign_fptr`
- `FieldGetLocalElementCount`
- `FieldGetLocalSize`
- `FieldGetCptr`
- `FieldClone`
- `FieldsAreConformable`
- `FieldsAreBroadcastConformable`
- `FieldsAreSameTypeKind`
- `FieldCopy`
- `FieldCopyBroadcast`
- `FieldSameData`

**mapl_ISO8601_DateTime_mod** (2 entities):
- `convert_ISO8601_to_integer_time`
- `convert_ISO8601_to_integer_date`

### 6. Regridder Layer

**mapl_EsmfRegridder_mod** (3 entities):
- `EsmfRegridder`
- `EsmfRegridderParam`
- `make_EsmfRegridderParam`

**mapl_RegridderMethods_mod** (17 entities):
- `REGRID_HINT_LOCAL`
- `REGRID_HINT_FILE_WEIGHTS`
- `REGRID_HINT_COMPUTE_TRANSPOSE`
- `REGRID_METHOD_BILINEAR`
- `REGRID_METHOD_BILINEAR_MONOTONIC`
- `REGRID_METHOD_BILINEAR_ROTATE`
- `REGRID_METHOD_CONSERVE`
- `REGRID_METHOD_CONSERVE_MONOTONIC`
- `REGRID_METHOD_VOTE`
- `REGRID_METHOD_FRACTION`
- `REGRID_METHOD_CONSERVE_2ND`
- `REGRID_METHOD_PATCH`
- `REGRID_METHOD_NEAREST_STOD`
- `REGRID_METHOD_CONSERVE_HFLUX`
- `UNSPECIFIED_REGRID_METHOD`
- `regrid_method_string_to_int`
- `regrid_method_int_to_string`

### 7. HConfig Layer

**mapl_HConfigAs_mod** (6 entities):
- `HConfigAsItemType`
- `HConfigAsStateIntent`
- `HConfigAsTime`
- `HConfigAsTimeInterval`
- `HConfigAsTimeRange`
- `HConfigAsStringVector`

### 8. Geom Layer

**mapl_GridGetGlobal_mod** (1 entity):
- `GridGetGlobalCellCountPerDim`

## Unprefixed Entities Exported Through API Umbrella Modules

### mapl_MaplFramework_mod (1):
- `MaplFramework`

### mapl_State_API_mod (4):
- `CallbackMap`
- `CallbackMapIterator`
- `CallbackMethodWrapper`
- `get_callback`

### mapl_Utils_API_mod (4):
- `String`
- `split`
- `lowercase`
- `uppercase`

### mapl_Geom_API_mod (11):
- `MaplGeom`
- `GeomSpec`
- `XYGeomSpec`
- `make_XYGeomSpec`
- `XY_COORD_STANDARD`
- `XY_COORD_ABI`
- `XYGeomFactory`
- `CubedSphereGeomSpec`
- `make_CubedSphereGeomSpec`
- `CubedSphereDecomposition`
- `make_CubedSphereDecomposition`

### mapl_VerticalGrid_API_mod (21):
- `VerticalGrid`
- `VerticalGridSpec`
- `VerticalGridFactory`
- `VerticalGridManager`
- `get_vertical_grid_manager`
- `IntegerPair`
- `VerticalStaggerLoc`
- `VERTICAL_STAGGER_NONE`
- `VERTICAL_STAGGER_EDGE`
- `VERTICAL_STAGGER_CENTER`
- `VERTICAL_STAGGER_MIRROR`
- `VERTICAL_STAGGER_INVALID`
- `VerticalAlignment`
- `VALIGN_WITH_GRID`
- `VALIGN_UP`
- `VALIGN_DOWN`
- `VALIGN_INVALID`
- `BasicVerticalGrid`
- `BasicVerticalGridSpec`
- `BasicVerticalGridFactory`
- `VerticalCoordinate`

### mapl_EsmfUtils_API_mod (3):
- `UngriddedDim`
- `make_UngriddedDim`
- `UngriddedDims` (appears twice in list)

### mapl_FieldBundle_API_mod (10):
- `FieldBundleType_Flag`
- `FIELDBUNDLETYPE_INVALID`
- `FIELDBUNDLETYPE_BASIC`
- `FIELDBUNDLETYPE_VECTOR`
- `FIELDBUNDLETYPE_BRACKET`
- `FIELDBUNDLETYPE_VECTORBRACKET`
- `VectorBasisKind`
- `VECTOR_BASIS_KIND_INVALID`
- `VECTOR_BASIS_KIND_GRID`
- `VECTOR_BASIS_KIND_NS`

### mapl_RegridderMgr_API_mod (22):
- `Regridder`
- `RegridderManager`
- `regridder_manager`
- `get_regridder_manager`
- `RegridderSpec`
- `REGRID_HINT_LOCAL`
- `REGRID_HINT_FILE_WEIGHTS`
- `REGRID_HINT_COMPUTE_TRANSPOSE`
- `REGRID_METHOD_BILINEAR`
- `REGRID_METHOD_BILINEAR_MONOTONIC`
- `REGRID_METHOD_BILINEAR_ROTATE`
- `REGRID_METHOD_CONSERVE`
- `REGRID_METHOD_CONSERVE_MONOTONIC`
- `REGRID_METHOD_VOTE`
- `REGRID_METHOD_FRACTION`
- `REGRID_METHOD_CONSERVE_2ND`
- `REGRID_METHOD_PATCH`
- `REGRID_METHOD_NEAREST_STOD`
- `REGRID_METHOD_CONSERVE_HFLUX`
- `UNSPECIFIED_REGRID_METHOD`
- `regrid_method_string_to_int`
- `regrid_method_int_to_string`

### mapl_Generic_API_mod (3):
- `user_setservices`
- `AbstractUserSetServices`
- `DSOSetServices`

## Strategy for Addressing Issue #4999

### Phase 1: Document Current State ✅
- Complete comprehensive audit (this document)

### Phase 2: Categorize by Usage
Need to determine which entities are:
1. **Actively used** in client repos (GEOSgcm, GOCART, etc.) - CANNOT remove
2. **Unused** - can hide behind API umbrellas with `only:` clauses
3. **Should be prefixed** - rename and provide backward compat

### Phase 3: Implementation Approach

For each layer that has unprefixed entities:

1. **If API umbrella exists**: Add `only:` clause to limit exports
2. **If no API umbrella**: Create one (like we did for `utils/API.F90`)
3. **Update MAPL.F90**: Use API umbrellas instead of leaf modules

### Phase 4: Breaking Changes
- Document all removed unprefixed entities in CHANGELOG
- Test against client repos to ensure zero-diff

## Questions for Human Review

1. **Scope**: Should we tackle ALL unprefixed entities, or focus on UNUSED ones first?
2. **Constants**: Many `REGRID_METHOD_*`, `VERTICAL_STAGGER_*` etc. look like they should use `MAPL_` prefix
3. **Types**: Should types like `String`, `UngriddedDims`, `VerticalGrid` be prefixed?
4. **Procedures**: Should procedures like `split`, `lowercase` be prefixed?
5. **Priority**: Which layers should we tackle first?

## Recommendation

Start with **unused entities only** to minimize breaking changes:
- Keep previously completed utils work (String, StringUtilities, FileSystemUtilities, DSO_Utilities, DirPath)
- Audit each remaining module for actual usage in client repos
- Create focused PRs per layer with usage data

## Notes

- This audit reveals the scope is MUCH larger than initially understood
- Many entities appear to be legitimate public API (used by clients)
- Need usage analysis across client repos before making changes
- May need to split issue #4999 into multiple sub-issues by layer
