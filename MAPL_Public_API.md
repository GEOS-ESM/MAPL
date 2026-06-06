# MAPL Public API Reference

This document lists all entities made public through the top-level `MAPL` umbrella module (`mapl/MAPL.F90`).
The umbrella itself has no `private` statement, so everything it `use`s is re-exported.

---

## Top-Level: `MAPL` (`mapl/MAPL.F90`)

The MAPL umbrella module directly `use`s the following modules. Two entities are renamed at this level:

| Renamed entity | Original name | Source module |
|---|---|---|
| `initialize_profiler` | `initialize` | `mapl_Profiler_mod` |
| `finalize_profiler` | `finalize` | `mapl_Profiler_mod` |

All other entities from the modules listed below are re-exported under their original names.

---

## Intermediate Umbrella Modules

These are API-aggregator or re-export modules used by the MAPL umbrella.

---

### `mapl_VM_API_mod` — `infrastructure/esmf/API.F90`

No `private` statement; re-exports all public entities from `mapl_vm_mod`.

| Entity | Kind |
|---|---|
| `mapl_AmIRoot` | interface/function |
| `mapl_AmIPet` | interface/function |
| `mapl_Barrier` | interface/subroutine |

---

### `mapl_MaplFramework_mod` — `mapl/MaplFramework.F90`

Has `private`; selective exports.

| Entity | Kind |
|---|---|
| `MaplFramework` | derived type (TBPs: `initialize`, `finalize`, `get`, `is_initialized`) |
| `MAPL_Initialize` | interface → subroutine |
| `MAPL_Finalize` | subroutine |
| `MAPL_Get` | interface → subroutine (two overloads) |

---

### `generic3g` — `superstructure/generic/Generic3g.F90`

No `private` statement; re-exports all public symbols from a chain of sub-modules.
Notable source modules and their key exported entities:

| Source module | Key public entities |
|---|---|
| `mapl_ESMF_Subset_mod` | See **ESMF Subset** section below |
| `mapl_Generic_mod` | `MAPL_GridCompGet`, `MAPL_GridCompSet`, `MAPL_GridCompAddChild`, `MAPL_GridCompAddVarSpec`, `MAPL_GridCompIsGeneric`, `MAPL_GridCompIsUser`, `MAPL_GridCompSetEntryPoint`, `MAPL_GridCompRunChild`, `MAPL_GridCompRunChildren`, `MAPL_GridCompGetInternalState`, `MAPL_GridCompSetGeometry`, `MAPL_GridCompSetGeom`, `MAPL_GridCompSetVerticalGrid`, `MAPL_GridCompAddConnection`, `MAPL_GridCompReexport`, `MAPL_GridCompConnectAll`, `MAPL_GridCompTimerStart`, `MAPL_GridCompTimerStop`, `MAPL_ClockGet`, `MAPL_UserCompGetInternalState`, `MAPL_UserCompSetInternalState` |
| `mapl_VariableSpec_mod` | `VariableSpec` (type) |
| `mapl_GenericPhases_mod` | phase constants/types |
| `mapl_OuterMetaComponent_mod` | `OuterMetaComponent` (type) |
| `mapl_GenericGridComp_mod` | `MAPL_GridCompCreate` |
| `mapl_BasicVerticalGrid_mod` | `BasicVerticalGrid`, `BasicVerticalGridSpec`, `BasicVerticalGridFactory` |
| `mapl_ModelVerticalGrid_mod` | `ModelVerticalGrid`, `ModelVerticalGridFactory` |
| `mapl_ComponentDriver_mod` | `ComponentDriver` (type) |
| `mapl_GriddedComponentDriver_mod` | `GriddedComponentDriver` (type) |
| `mapl_ChildSpec_mod` | `ChildSpec` (type) |
| `mapl_UserSetServices_mod` | `user_setservices` (abstract interface), `AbstractUserSetServices` (abstract type), `DSOSetServices` (type) |
| `mapl_VerticalStaggerLoc_mod` | `VerticalStaggerLoc` (type), stagger location constants |
| `mapl_geomio` | See `mapl_geomio` section below |
| `mapl_ESMF_Utilities_mod` | ESMF utility wrappers |
| `mapl_OpenMP_Support_mod` | OpenMP helpers |

---

### `mapl_State_API_mod` — `infrastructure/fields/state/API.F90`

Has `private`; selective exports with renames.

| Entity | Kind | Renamed from | Source module |
|---|---|---|---|
| `MAPL_StateGet` | subroutine | `StateGet` | `mapl_StateGetImpl_mod` |
| `MAPL_StateGetPointer` | subroutine | `StateGetPointer` | `mapl_StateGetPointerImpl_mod` |
| `MAPL_ParserVariablesInExpression` | function | `parser_variables_in_expression` | `mapl_StateArithmeticParser_mod` |
| `mapl_StateAddMethod` | subroutine | — | `mapl_StateAddMethodImpl_mod` |
| `CallbackMap` | type | — | `mapl_StateAddMethodImpl_mod` |
| `CallbackMapIterator` | type | — | `mapl_StateAddMethodImpl_mod` |
| `CallbackMethodWrapper` | type | — | `mapl_StateAddMethodImpl_mod` |
| `get_callbacks` | function | — | `mapl_StateAddMethodImpl_mod` |
| `MAPL_StateGetGeom` | subroutine | `StateGetGeom` | `mapl_StateGetGeomImpl_mod` |

---

### `mapl_Geom_API_mod` — `infrastructure/geom/geom/API.F90`

Has `private`; selective exports with renames.

| Entity | Kind | Renamed from | Source module |
|---|---|---|---|
| `mapl_GeomGet` | subroutine | `GeomGet` | `mapl_GeomAccessors_mod` |
| `mapl_GridGet` | subroutine | `GridGet` | `mapl_GridAccessors_mod` |
| `mapl_GridGetCoordinates` | subroutine | `GridGetCoordinates` | `mapl_GridAccessors_mod` |
| `mapl_GridGetHorzIJIndex` | subroutine | — | `mapl_GeomAccessors_mod` |
| `mapl_GeomGetHorzIJIndex` | subroutine | — | `mapl_GeomAccessors_mod` |
| `mapl_GridGetGlobalCellCountPerDim` | subroutine | `GridGetGlobalCellCountPerDim` | `mapl_GridGetGlobal_mod` |
| `mapl_GridHasDE` | function | `grid_has_DE` | `mapl_GridAccessors_mod` |
| `MaplGeom` | type | — | `mapl_MaplGeom_mod` |
| `mapl_SameGeom` | function | — | `mapl_GeomUtilities_mod` |
| `mapl_GeomGetId` | function | — | `mapl_GeomUtilities_mod` |
| `GeomManager` | type | — | `mapl_GeomManager_mod` |
| `geom_manager` | variable (singleton) | — | `mapl_GeomManager_mod` |
| `get_geom_manager` | function | — | `mapl_GeomManager_mod` |
| `get_mapl_geom` | function | — | `mapl_GeomManager_mod` |
| `GeomSpec` | type | — | `mapl_GeomSpec_mod` |
| `mapl_Interval` | type | `Interval` | `mapl_Subgrid_mod` |
| `mapl_make_subgrids` | subroutine | `make_subgrids` | `mapl_Subgrid_mod` |
| `XYGeomSpec` | type | — | `mapl_XYGeomSpec_mod` |
| `make_XYGeomSpec` | function | — | `mapl_XYGeomSpec_mod` |
| `XY_COORD_STANDARD` | parameter | — | `mapl_XYGeomSpec_mod` |
| `XY_COORD_ABI` | parameter | — | `mapl_XYGeomSpec_mod` |
| `XYGeomFactory` | type | — | `mapl_XYGeomFactory_mod` |
| `CubedSphereGeomSpec` | type | — | `mapl_CubedSphereGeomSpec_mod` |
| `make_CubedSphereGeomSpec` | function | — | `mapl_CubedSphereGeomSpec_mod` |
| `CubedSphereDecomposition` | type | — | `mapl_CubedSphereDecomposition_mod` |
| `make_CubedSphereDecomposition` | function | — | `mapl_CubedSphereDecomposition_mod` |

---

### `mapl_HConfig_API` — `infrastructure/esmf/hconfig/HConfig_API.F90`

No `private`; uses `only:` with renames — these six names are the only public exports.

| Entity | Kind | Renamed from | Source module |
|---|---|---|---|
| `mapl_HConfigAsItemType` | function | `HConfigAsItemType` | `mapl_HConfigAs_mod` |
| `mapl_HConfigAsStateIntent` | function | `HConfigAsStateIntent` | `mapl_HConfigAs_mod` |
| `mapl_HConfigAsTime` | function | `HConfigAstime` | `mapl_HConfigAs_mod` |
| `mapl_HConfigAsTimeInterval` | function | `HConfigAsTimeInterval` | `mapl_HConfigAs_mod` |
| `mapl_HConfigAsTimeRange` | function | `HConfigAsTimeRange` | `mapl_HConfigAs_mod` |
| `mapl_HConfigAsStringVector` | function | `HConfigAsStringVector` | `mapl_HConfigAs_mod` |

---

### `mapl_VerticalGrid_API_mod` — `infrastructure/vertical/vertical_grid/API.F90`

Has `private`; clean categorical export list.

| Entity | Kind | Source module |
|---|---|---|
| `VerticalGrid` | abstract type | `mapl_VerticalGrid_mod` |
| `VerticalGridSpec` | abstract type | `mapl_VerticalGridSpec_mod` |
| `VerticalGridFactory` | abstract type | `mapl_VerticalGridFactory_mod` |
| `VerticalGridManager` | type | `mapl_VerticalGridManager_mod` |
| `get_vertical_grid_manager` | function | `mapl_VerticalGridManager_mod` |
| `IntegerPair` | type | `mapl_IntegerPair_mod` |
| `VerticalStaggerLoc` | type | `mapl_VerticalStaggerLoc_mod` |
| `operator(==)` | operator | `mapl_VerticalStaggerLoc_mod` |
| `operator(/=)` | operator | `mapl_VerticalStaggerLoc_mod` |
| `VERTICAL_STAGGER_NONE` | parameter | `mapl_VerticalStaggerLoc_mod` |
| `VERTICAL_STAGGER_EDGE` | parameter | `mapl_VerticalStaggerLoc_mod` |
| `VERTICAL_STAGGER_CENTER` | parameter | `mapl_VerticalStaggerLoc_mod` |
| `VERTICAL_STAGGER_MIRROR` | parameter | `mapl_VerticalStaggerLoc_mod` |
| `VERTICAL_STAGGER_INVALID` | parameter | `mapl_VerticalStaggerLoc_mod` |
| `VerticalAlignment` | type | `mapl_VerticalAlignment_mod` |
| `VALIGN_WITH_GRID` | parameter | `mapl_VerticalAlignment_mod` |
| `VALIGN_UP` | parameter | `mapl_VerticalAlignment_mod` |
| `VALIGN_DOWN` | parameter | `mapl_VerticalAlignment_mod` |
| `VALIGN_INVALID` | parameter | `mapl_VerticalAlignment_mod` |
| `BasicVerticalGrid` | type (concrete) | `mapl_BasicVerticalGrid_mod` |
| `BasicVerticalGridSpec` | type | `mapl_BasicVerticalGrid_mod` |
| `BasicVerticalGridFactory` | type | `mapl_BasicVerticalGrid_mod` |
| `VERTICAL_GRID_NOT_FOUND` | parameter (integer) | `mapl_VerticalGrid_mod` |

---

### `mapl_EsmfUtils_API_mod` — `infrastructure/esmf/EsmfUtils_API.F90`

Has `private`; minimal exports.

| Entity | Kind | Renamed from | Source module |
|---|---|---|---|
| `UngriddedDim` | type | — | `mapl_UngriddedDim_mod` |
| `make_UngriddedDim` | function | `make_ungriddedDim` | `mapl_UngriddedDim_mod` |
| `UngriddedDims` | type | — | `mapl_UngriddedDims_mod` |

---

### `mapl_Field_API` — `infrastructure/fields/field/API.F90`

No `private`; uses `only:` with renames, so all local names are public.

| Entity | Kind | Renamed from | Source module |
|---|---|---|---|
| `MAPL_FieldClone` | function | `FieldClone` | `mapl_FieldPointerUtilities_mod` |
| `MAPL_FieldGet` | subroutine | `FieldGet` | `mapl_FieldGetImpl_mod` |
| `MAPL_FieldSet` | subroutine | `FieldSet` | `mapl_FieldSetImpl_mod` |
| `MAPL_FieldFill` | subroutine | `FieldFill` | `mapl_FieldFillImpl_mod` |
| `MAPL_AssignFptr` | subroutine | `assign_fptr` | `mapl_FieldPointerUtilities_mod` |
| `MAPL_FieldCreate` | interface | — | `mapl_FieldCreateImpl_mod` |
| `MAPL_FieldEmptyComplete` | interface | — | `mapl_FieldCreateImpl_mod` |
| `MAPL_FieldsAreAliased` | function | — | `mapl_FieldCreateImpl_mod` |
| `StateItemAllocation` | type | — | `mapl_StateItemAllocation_mod` |
| `operator(==)` | operator | — | `mapl_StateItemAllocation_mod` |
| `operator(/=)` | operator | — | `mapl_StateItemAllocation_mod` |
| `operator(<)` | operator | — | `mapl_StateItemAllocation_mod` |
| `operator(>=)` | operator | — | `mapl_StateItemAllocation_mod` |
| `STATEITEM_ALLOCATION_NONE` | parameter | — | `mapl_StateItemAllocation_mod` |
| `STATEITEM_ALLOCATION_REQUIRED` | parameter | — | `mapl_StateItemAllocation_mod` |
| `STATEITEM_ALLOCATION_OPTIONAL` | parameter | — | `mapl_StateItemAllocation_mod` |
| `STATEITEM_ALLOCATION_INTERNAL` | parameter | — | `mapl_StateItemAllocation_mod` |
| `STATEITEM_ALLOCATION_SHARED` | parameter | — | `mapl_StateItemAllocation_mod` |
| `STATEITEM_ALLOCATION_EXTERNAL` | parameter | — | `mapl_StateItemAllocation_mod` |
| `RestartMode` | type | — | `mapl_RestartModes_mod` |
| `MAPL_RESTART_REQUIRED` | parameter | — | `mapl_RestartModes_mod` |
| `MAPL_RESTART_SKIP` | parameter | — | `mapl_RestartModes_mod` |
| `FieldInfoGetShared` | subroutine | — | `mapl_FieldInfo_mod` |
| `FieldInfoSetShared` | subroutine | — | `mapl_FieldInfo_mod` |
| `FieldInfoSetInternal` | subroutine | — | `mapl_FieldInfo_mod` |
| `FieldInfoGetInternal` | subroutine | — | `mapl_FieldInfo_mod` |
| `FieldInfoCopyShared` | subroutine | — | `mapl_FieldInfo_mod` |

---

### `mapl_FieldBundle_API_mod` — `infrastructure/fields/field_bundle/API.F90`

Has `private`; comprehensive selective export.

| Entity | Kind | Renamed from | Source module |
|---|---|---|---|
| `MAPL_FieldBundleCreate` | function | `FieldBundleCreate` | `mapl_FieldBundleCreateImpl_mod` |
| `MAPL_FieldBundlesAreAliased` | function | `FieldBundlesAreAliased` | `mapl_FieldBundleCreateImpl_mod` |
| `MAPL_FieldBundleDestroy` | subroutine | — | `mapl_FieldBundleDestroyImpl_mod` |
| `MAPL_FieldBundleGet` | subroutine | `FieldBundleGet` | `mapl_FieldBundleGetImpl_mod` |
| `MAPL_FieldBundleGetByIndex` | subroutine | `FieldBundleGetByIndex` | `mapl_FieldBundleGetByIndexImpl_mod` |
| `MAPL_FieldBundleSet` | subroutine | `FieldBundleSet` | `mapl_FieldBundleSetImpl_mod` |
| `MAPL_FieldBundleAdd` | subroutine | `ESMF_FieldBundleAdd` | ESMF |
| `MAPL_FieldBundleGetPointer` | subroutine | `FieldBundleGetPointerToData` | `mapl_FieldBundleGetPointerImpl_mod` |
| `MAPL_FieldBundleCopy` | subroutine | `FieldBundleCopy` | `mapl_FieldBundleCopyImpl_mod` |
| `MAPL_FieldBundleSameData` | function | `FieldBundleSameData` | `mapl_FieldBundleMatch_mod` |
| `MAPL_FieldBundleInfoGetInternal` | subroutine | `FieldBundleInfoGetInternal` | `mapl_FieldBundleInfo_mod` |
| `MAPL_FieldBundleInfoSetInternal` | subroutine | `FieldBundleInfoSetInternal` | `mapl_FieldBundleInfo_mod` |
| `FieldBundleType_Flag` | type | — | `mapl_FieldBundleType_Flag_mod` |
| `FIELDBUNDLETYPE_INVALID` | parameter | — | `mapl_FieldBundleType_Flag_mod` |
| `FIELDBUNDLETYPE_BASIC` | parameter | — | `mapl_FieldBundleType_Flag_mod` |
| `FIELDBUNDLETYPE_VECTOR` | parameter | — | `mapl_FieldBundleType_Flag_mod` |
| `FIELDBUNDLETYPE_BRACKET` | parameter | — | `mapl_FieldBundleType_Flag_mod` |
| `FIELDBUNDLETYPE_VECTORBRACKET` | parameter | — | `mapl_FieldBundleType_Flag_mod` |
| `operator(==)` | operator | — | `mapl_FieldBundleType_Flag_mod` |
| `operator(/=)` | operator | — | `mapl_FieldBundleType_Flag_mod` |
| `VectorBasisKind` | type | — | `mapl_VectorBasisKind_mod` |
| `VECTOR_BASIS_KIND_INVALID` | parameter | — | `mapl_VectorBasisKind_mod` |
| `VECTOR_BASIS_KIND_GRID` | parameter | — | `mapl_VectorBasisKind_mod` |
| `VECTOR_BASIS_KIND_NS` | parameter | — | `mapl_VectorBasisKind_mod` |

---

### `mapl_mp_utils` — `mp_utils/API.F90`

No `private`; uses `only:` with renames — all resulting local names are public.

| Entity | Kind | Renamed from | Source module |
|---|---|---|---|
| `MAPL_MaxMin` | subroutine | — | `mapl_ArrayReductions_mod` |
| `MAPL_AreaMean` | subroutine | — | `mapl_ArrayReductions_mod` |
| `MAPL_MemInfoWrite` | subroutine | `MemInfoWrite` | `mapl_MemInfo_mod` |
| `MAPL_PackTime` | subroutine | `PackTime` | `mapl_TimeUtilities_mod` |
| `MAPL_UnpackTime` | subroutine | `UnpackTime` | `mapl_TimeUtilities_mod` |
| `PackedDateCreate` | function | — | `mapl_PackedTime_mod` |
| `PackedTimeCreate` | function | — | `mapl_PackedTime_mod` |
| `PackedDateTimeCreate` | function | — | `mapl_PackedTime_mod` |
| `StrTemplate` | subroutine | — | `mapl_StringTemplate_mod` |

---

### `mapl_RegridderMgr_API_mod` — `infrastructure/geom/regridder_mgr/API.F90`

Has `private`; clean full listing.

| Entity | Kind | Source module |
|---|---|---|
| `Regridder` | abstract type | `mapl_Regridder_mod` |
| `RegridderManager` | type | `mapl_RegridderManager_mod` |
| `regridder_manager` | variable (singleton) | `mapl_RegridderManager_mod` |
| `get_regridder_manager` | function | `mapl_RegridderManager_mod` |
| `RegridderSpec` | type | `mapl_RegridderSpec_mod` |
| `REGRID_HINT_LOCAL` | parameter | `mapl_RegridderMethods_mod` |
| `REGRID_HINT_FILE_WEIGHTS` | parameter | `mapl_RegridderMethods_mod` |
| `REGRID_HINT_COMPUTE_TRANSPOSE` | parameter | `mapl_RegridderMethods_mod` |
| `REGRID_METHOD_BILINEAR` | enumerator | `mapl_RegridderMethods_mod` |
| `REGRID_METHOD_BILINEAR_MONOTONIC` | enumerator | `mapl_RegridderMethods_mod` |
| `REGRID_METHOD_BILINEAR_ROTATE` | enumerator | `mapl_RegridderMethods_mod` |
| `REGRID_METHOD_CONSERVE` | enumerator | `mapl_RegridderMethods_mod` |
| `REGRID_METHOD_CONSERVE_MONOTONIC` | enumerator | `mapl_RegridderMethods_mod` |
| `REGRID_METHOD_VOTE` | enumerator | `mapl_RegridderMethods_mod` |
| `REGRID_METHOD_FRACTION` | enumerator | `mapl_RegridderMethods_mod` |
| `REGRID_METHOD_CONSERVE_2ND` | enumerator | `mapl_RegridderMethods_mod` |
| `REGRID_METHOD_PATCH` | enumerator | `mapl_RegridderMethods_mod` |
| `REGRID_METHOD_NEAREST_STOD` | enumerator | `mapl_RegridderMethods_mod` |
| `REGRID_METHOD_CONSERVE_HFLUX` | enumerator | `mapl_RegridderMethods_mod` |
| `UNSPECIFIED_REGRID_METHOD` | enumerator | `mapl_RegridderMethods_mod` |
| `regrid_method_string_to_int` | function | `mapl_RegridderMethods_mod` |
| `regrid_method_int_to_string` | function | `mapl_RegridderMethods_mod` |
| `generate_esmf_regrid_param` | function | `mapl_RegridderMethods_mod` |

---

### `mapl_Generic3g_API_mod` — `superstructure/generic/API.F90`

Has `private`; minimal exports.

| Entity | Kind | Source module |
|---|---|---|
| `user_setservices` | abstract interface | `mapl_UserSetServices_mod` |
| `AbstractUserSetServices` | abstract type | `mapl_UserSetServices_mod` |
| `DSOSetServices` | type | `mapl_UserSetServices_mod` |

---

### `mapl_base3g_mod` — `base3g/API.F90`

Has `private`; large selective export with many renames.

#### Packed time utilities (`mapl_PackedTime_mod`)

| Entity | Kind | Renamed from |
|---|---|---|
| `MAPL_PackedDateCreate` | function | `PackedDateCreate` |
| `MAPL_PackedTimeCreate` | function | `PackedTimeCreate` |
| `MAPL_PackedDateTimeCreate` | function | `PackedDateTimeCreate` |
| `MAPL_ESMFTimeFromPacked` | function | `ESMFTimeFromPacked` |
| `MAPL_UnpackDate` | subroutine | `UnpackDate` |
| `MAPL_UnpackTime` | subroutine | `UnpackTime` |
| `MAPL_UnpackDateTime` | subroutine | `UnpackDateTime` |

#### Simulation time (`mapl_SimulationTime_mod`)

| Entity | Kind |
|---|---|
| `set_reference_clock` | subroutine |
| `fill_time_dict` | subroutine |

#### MPI communications (`MAPL_CommsMod`)

| Entity | Kind | Notes |
|---|---|---|
| `mapl_CommsBcast` | subroutine | |
| `mapl_CommsScatterV` | subroutine | |
| `mapl_CommsGatherV` | subroutine | |
| `mapl_CommsAllGather` | subroutine | |
| `mapl_CommsAllGatherV` | subroutine | |
| `mapl_CommsAllReduceMin` | subroutine | |
| `mapl_CommsAllReduceMax` | subroutine | |
| `mapl_CommsAllReduceSum` | subroutine | |
| `mapl_CommsSend` | subroutine | |
| `mapl_CommsRecv` | subroutine | |
| `mapl_CommsSendRecv` | subroutine | |
| `mapl_AM_I_ROOT` | function | |
| `mapl_AM_I_RANK` | function | |
| `mapl_NPES` | function | |
| `ArrayGather` | subroutine | |
| `ArrayScatter` | subroutine | |
| `MAPL_ROOT` | parameter | |
| `MAPL_ArrayGather` | subroutine | alias for `ArrayGather` |
| `MAPL_ArrayScatter` | subroutine | alias for `ArrayScatter` |
| `mapl_CreateRequest` | function | |
| `mapl_CommRequest` | type | |
| `mapl_ArrayIGather` | subroutine | |
| `mapl_ArrayIScatter` | subroutine | |
| `mapl_CollectiveWait` | subroutine | |
| `mapl_CollectiveScatter3D` | subroutine | |
| `mapl_CollectiveGather3D` | subroutine | |
| `mapl_RoundRobinPEList` | function | |
| `mapl_BcastShared` | subroutine | |
| `ArrPtr` | type | |

#### Saturation vapor (`mapl_SatVapor_mod`)

| Entity | Kind |
|---|---|
| `MAPL_EQsatSET` | subroutine |
| `MAPL_EQsat` | function |

#### String templates (`mapl_StringTemplate_mod`)

| Entity | Kind |
|---|---|
| `fill_grads_template` | subroutine |
| `StrTemplate` | function |
| `fill_grads_template_esmf` | subroutine |

#### Solar orbit (`mapl_Sun_mod`)

| Entity | Kind |
|---|---|
| `MAPL_SunOrbit` | type |
| `MAPL_SunOrbitCreate` | function |
| `MAPL_SunOrbitCreateFromConfig` | function |
| `MAPL_SunOrbitCreated` | function |
| `MAPL_SunOrbitDestroy` | subroutine |
| `MAPL_SunOrbitQuery` | subroutine |
| `MAPL_SunGetInsolation` | subroutine |
| `MAPL_SunGetSolarConstant` | function |
| `MAPL_SunGetDaylightDuration` | subroutine |
| `MAPL_SunGetDaylightDurationMax` | subroutine |
| `MAPL_SunGetLocalSolarHourAngle` | subroutine |

#### Memory utilities (`mapl_MemUtils_mod`)

| Entity | Kind |
|---|---|
| `MAPL_MemUtilsInit` | subroutine |
| `MAPL_MemUtilsDisable` | subroutine |
| `MAPL_MemUtilsWrite` | subroutine |
| `MAPL_MemUtilsIsDisabled` | function |
| `MAPL_MemUtilsFree` | subroutine |
| `MAPL_MemCommited` | function |
| `MAPL_MemUsed` | function |
| `MAPL_MemReport` | subroutine |

#### Time interpolation (`mapl_TimeInterpolation_mod`)

| Entity | Kind |
|---|---|
| `MAPL_Interp_Fac` | function |
| `MAPL_ClimInterpFac` | function |

#### File I/O constants (`mapl_FileIO_mod`)

| Entity | Kind |
|---|---|
| `WRITE_PARALLEL` | parameter |

#### Simple bundle (`mapl_SimpleBundleMod_impl_mod`)

| Entity | Kind |
|---|---|
| `MAPL_SimpleBundle` | type |
| `MAPL_SimpleBundleCreate` | function |
| `MAPL_SimpleBundlePrint` | subroutine |
| `MAPL_SimpleBundleGetIndex` | function |
| `MAPL_SimpleBundleDestroy` | subroutine |

#### NetCDF I/O (`mapl_NCIO_mod`)

| Entity | Kind |
|---|---|
| `MAPL_VarRead` | subroutine |
| `MAPL_VarWrite` | subroutine |
| `MAPL_NCIOGetFileType` | function |
| `MAPL_IOGetNonDimVars` | subroutine |
| `MAPL_IOCountNonDimVars` | function |
| `MAPL_IOChangeRes` | subroutine |
| `MAPL_IOCountLevels` | function |

#### Array descriptors (`mapl_FileIOShared_mod`)

| Entity | Kind |
|---|---|
| `ArrDescr` | type |
| `ArrDescrInit` | subroutine |
| `ArrDescrSet` | subroutine |

#### File metadata (`mapl_FileMetadataUtils_mod`)

| Entity | Kind |
|---|---|
| `FileMetadataUtils` | type + constructor interface |

#### Ensemble (`mapl_LocalDisplacementEnsemble_mod`)

| Entity | Kind |
|---|---|
| `LocalDisplacementEnsemble` | type |

---

### `mapl_FieldUtils` — `infrastructure/fields/field/FieldUtils.F90`

No `private`; open re-export of multiple sub-modules. All public entities from the following are re-exported:

| Source module | Description |
|---|---|
| `mapl_FieldUnaryFunctions_mod` | Unary field math functions (e.g., `MAPL_FieldSqrt`, `MAPL_FieldLog`) |
| `mapl_FieldBinaryOperations_mod` | Binary field operators: `operator(+)`, `operator(-)`, `operator(*)`, `operator(/)` on `ESMF_Field` |
| `mapl_FieldUtilities_mod` | Field utility subroutines (copy, zero, fill) |
| `mapl_FieldPointerUtilities_mod` | `MAPL_AssignFptr`, `MAPL_FieldClone`, `FieldsHaveUndef`, `GetFieldsUndef`, `FieldGetLocalElementCount`, `FieldGetLocalSize`, `FieldGetCptr`, `FieldsAreConformable`, `FieldsAreBroadcastConformable`, `FieldsAreSameTypeKind`, `FieldCopy`, `MAPL_FieldDestroy`, `FieldCopyBroadcast`, `FieldSameData` |
| `mapl_FieldBLAS_mod` | BLAS-like field operations (`MAPL_FieldAxpy`, `MAPL_FieldDot`, etc.) |

---

### `mapl_geomio` — `infrastructure/geom/GeomIO/GeomIO.F90`

No `private`; open re-export of multiple sub-modules.

| Source module | Notable entities |
|---|---|
| `mapl_DataCollection_mod` | `DataCollection` type |
| `mapl_DataCollectionVector_mod` | `DataCollectionVector` type |
| `mapl_DataCollectionManager_mod` | `DataCollectionManager` type, singleton |
| `mapl_pFIOServerBounds_mod` | pFIO server bound types |
| `mapl_FieldBundleWrite_mod` | `MAPL_FieldBundleWrite` and related |
| `mapl_FieldBundleRead_mod` | `MAPL_FieldBundleRead` and related |
| `mapl_CompressionSettings_mod` | `CompressionSettings` type |
| `mapl_GeomCatagorizer_mod` | geometry categorization |
| `mapl_GeomPFIO_mod` | geometry + pFIO integration |
| `mapl_SharedIO_mod` | shared I/O infrastructure |

---

### `pfio` — `pfio/pFIO.F90`

No `private`; open re-export of the entire pFIO parallel file I/O library. All public entities from each sub-module are re-exported:

| Source module | Key public entities |
|---|---|
| `pFIO_ConstantsMod` | I/O constants, type kind flags |
| `pFIO_UnlimitedEntityMod` | `UnlimitedEntity` type |
| `pFIO_AttributeMod` | `Attribute` type |
| `pFIO_VariableMod` | `Variable` type |
| `pFIO_CoordinateVariableMod` | `CoordinateVariable` type |
| `pFIO_FileMetadataMod` | `FileMetadata` type |
| `pFIO_NetCDF4_FileFormatterMod` | `NetCDF4_FileFormatter` type |
| `pFIO_AbstractDirectoryServiceMod` | `AbstractDirectoryService`, `PortInfo` |
| `pFIO_DirectoryServiceMod` | `DirectoryService` type |
| `pFIO_AbstractServerMod` | `AbstractServer` abstract type |
| `pFIO_BaseServerMod` | `BaseServer` type |
| `pFIO_MpiServerMod` | `MpiServer` type |
| `pFIO_MultiLayerServerMod` | `MultiLayerServer` type |
| `pFIO_MultiCommServerMod` | `MultiCommServer` type |
| `pFIO_MultiGroupServerMod` | `MultiGroupServer` type |
| `pFIO_UtilitiesMod` | Utility subroutines |
| `pFIO_ServerThreadMod` | `ServerThread` type |
| `pFIO_ClientThreadMod` | `ClientThread` type |
| `pFIO_ClientThreadVectorMod` | `ClientThreadVector` type |
| `pFIO_ClientManagerMod` | `ClientManager`, `o_Clients`, `i_Clients`, `init_IO_ClientManager` |
| `pFIO_AbstractSocketMod` | `AbstractSocket` abstract type |
| `pFIO_AbstractSocketVectorMod` | `AbstractSocketVector` type |
| `pFIO_MpiSocketMod` | `MpiSocket` type |
| `pFIO_SimpleSocketMod` | `SimpleSocket` type |
| `pFIO_AbstractDataReferenceMod` | `AbstractDataReference` abstract type |
| `pFIO_ArrayReferenceMod` | `ArrayReference` type |
| `pFIO_StringAttributeMapMod` | `StringAttributeMap` type |
| `pFIO_StringVariableMapMod` | `StringVariableMap` type |
| `pFIO_LocalMemReferenceMod` | `LocalMemReference` type |
| `pFIO_FormatterPtrVectorMod` | `FormatterPtrVector` type |
| `pFIO_StringVectorUtilMod` | String vector utilities |

Also exports module-level variable: `debug_unit` (integer, save).

---

## Leaf Modules (directly used by MAPL umbrella)

---

### `mapl_String_mod` — `utils/String.F90`

| Entity | Kind |
|---|---|
| `String` | type + constructor interface |

---

### `mapl_StringUtilities_mod` — `utils/StringUtilities.F90`

| Entity | Kind |
|---|---|
| `split` | interface |
| `to_lower` | interface |
| `to_upper` | interface |
| `capitalize` | interface |
| `is_alpha` | function |
| `is_alpha_only` | function |
| `is_numeric` | function |
| `is_alphanumeric` | function |
| `to_string` | function |
| `to_character_array` | function |
| `lowercase` | function |
| `uppercase` | function |
| `is_digit` | function |
| `get_ascii_interval` | interface |
| `is_alphanum_character` | function |
| `is_lower_character` | function |
| `is_upper_character` | function |

---

### `mapl_FileSystemUtilities_mod` — `utils/FileSystemUtilities.F90`

| Entity | Kind |
|---|---|
| `get_file_extension` | function |
| `get_file_basename` | function |

---

### `mapl_DSO_Utilities_mod` — `utils/DSO_Utilities.F90`

| Entity | Kind |
|---|---|
| `is_valid_dso_name` | function |
| `is_valid_dso_extension` | function |
| `is_supported_dso_name` | function |
| `is_supported_dso_extension` | function |
| `adjust_dso_name` | function |
| `SYSTEM_DSO_EXTENSION` | parameter (character) |

---

### `mapl_SplitCommunicator_mod` — `mp_utils/SplitCommunicator.F90`

| Entity | Kind |
|---|---|
| `SplitCommunicator` | type + constructor interface |
| `NULL_SUBCOMMUNICATOR_NAME` | parameter (character) |

---

### `mapl_SimpleCommSplitter_mod` — `mp_utils/SimpleCommSplitter.F90`

| Entity | Kind |
|---|---|
| `SimpleCommSplitter` | type + constructor interface |

---

### `mapl_Sort_mod` — `utils/MAPL_Sort.F90`

| Entity | Kind |
|---|---|
| `MAPL_Sort` | interface (generic over multiple type/rank combinations) |

---

### `mapl_Shmem_mod` — `mp_utils/Shmem/Shmem.F90`

| Entity | Kind |
|---|---|
| `MAPL_GetNodeInfo` | subroutine |
| `MAPL_CoresPerNodeGet` | subroutine |
| `MAPL_InitializeShmem` | subroutine |
| `MAPL_FinalizeShmem` | subroutine |
| `MAPL_AllocNodeArray` | interface (generic) |
| `MAPL_DeAllocNodeArray` | interface (generic) |
| `MAPL_ShmemAmOnFirstNode` | function |
| `MAPL_SyncSharedMemory` | subroutine |
| `MAPL_BroadcastToNodes` | interface (generic) |
| `MAPL_AllocateShared` | interface (generic) |
| `GetSharedMemory` | subroutine |
| `ReleaseSharedMemory` | subroutine |
| `MAPL_GetNewRank` | function |
| `MAPL_NodeComm` | integer variable (save) |
| `MAPL_NodeRootsComm` | integer variable (save) |
| `MAPL_MyNodeNum` | integer variable (save) |
| `MAPL_AmNodeRoot` | logical variable (save) |
| `MAPL_ShmInitialized` | logical variable (save) |
| `MAPL_NodeRankList` | allocatable array variable |

---

### `mapl_Throw_mod` — `utils/MAPL_Throw.F90`

| Entity | Kind |
|---|---|
| `MAPL_throw_exception` | subroutine |
| `MAPL_set_throw_method` | subroutine |

---

### `mapl_Range_mod` — `utils/MAPL_Range.F90`

| Entity | Kind |
|---|---|
| `MAPL_Range` | interface (REAL32 and REAL64 variants) |

---

### `mapl_MinMax_mod` — `utils/MAPL_MinMax.F90`

| Entity | Kind |
|---|---|
| `IntegerMinMax` | type |
| `RealMinMax` | type |
| `Real64MinMax` | type |

---

### `mapl_LoadBalance_mod` — `mp_utils/MAPL_LoadBalance.F90`

| Entity | Kind |
|---|---|
| `MAPL_BalanceWork` | subroutine |
| `MAPL_BalanceCreate` | subroutine |
| `MAPL_BalanceDestroy` | subroutine |
| `MAPL_BalanceGet` | subroutine |
| `MAPL_Distribute` | parameter (integer = 1) |
| `MAPL_Retrieve` | parameter (integer = 2) |

---

### `mapl_KeywordEnforcer_mod` — `utils/KeywordEnforcer.F90`

| Entity | Kind |
|---|---|
| `KeywordEnforcer` | abstract type |

---

### `mapl_Interp_mod` — `utils/Interp/Interp.F90`

| Entity | Kind |
|---|---|
| `MAPL_Interp` | interface (generic interpolation) |

---

### `mapl_Hash_mod` — `utils/MAPL_Hash.F90`

| Entity | Kind |
|---|---|
| `MAPL_HashCreate` | integer function |
| `MAPL_HashIncrement` | integer function |
| `MAPL_HashDestroy` | subroutine |
| `MAPL_HashSize` | integer function |
| `MAPL_HashDump` | integer function |

---

### `mapl_ErrorHandling_mod` — `utils/ErrorHandling.F90`

| Entity | Kind |
|---|---|
| `MAPL_Assert` | interface |
| `MAPL_Verify` | function |
| `MAPL_Return` | subroutine |
| `MAPL_Deprecated` | subroutine |
| `MAPL_SetFailOnDeprecated` | subroutine |
| `MAPL_abort` | subroutine |
| `MAPL_set_abort_handler` | subroutine |
| `MAPL_SUCCESS` | enumerator (= 0) |
| `MAPL_UNKNOWN_ERROR` | enumerator |
| `MAPL_NO_SUCH_PROPERTY` | enumerator |
| `MAPL_NO_SUCH_VARIABLE` | enumerator |
| `MAPL_TYPE_MISMATCH` | enumerator |
| `MAPL_UNSUPPORTED_TYPE` | enumerator |
| `MAPL_VALUE_NOT_SUPPORTED` | enumerator |
| `MAPL_NO_DEFAULT_VALUE` | enumerator |
| `MAPL_DUPLICATE_KEY` | enumerator |
| `MAPL_STRING_TOO_SHORT` | enumerator |

---

### `mapl_DirPath_mod` — `utils/MAPL_DirPath.F90`

| Entity | Kind |
|---|---|
| `DirPath` | type (extends `StringVector`) |
| `dirpaths` | module variable (type `DirPath`) |

---

### `MAPL_Constants` — `utils/Constants/Constants.F90`

No `private`; aggregates five sub-modules without filtering. Also defines one subroutine.

#### Module-level

| Entity | Kind |
|---|---|
| `initialize_constants` | subroutine |

#### Internal constants (`mapl_InternalConstants_mod`)

Kind parameters: `MAPL_R8`, `MAPL_R4`, `MAPL_RN`, `MAPL_I8`, `MAPL_I4`, `MAPL_IN`

Undefined sentinels: `MAPL_UNDEFINED_REAL32`, `MAPL_UNDEFINED_REAL64`, `MAPL_UNDEFINED_REAL`, `MAPL_UNDEF`, `MAPL_UNDEFINED_INTEGER`, `MAPL_UNDEFINED_CHAR`

String defaults: `MAPL_GRID_NAME_DEFAULT`, `MAPL_GRID_FILE_NAME_DEFAULT`, `MAPL_CF_COMPONENT_SEPARATOR`

Grid/topology flags: `MAPL_DimTopoEdge`, `MAPL_DimTopoCyclic`, `MAPL_DimTopoCenter`, `MAPL_DimsUnknown`, `MAPL_DimsVertOnly`, `MAPL_DimsHorzOnly`, `MAPL_DimsHorzVert`, `MAPL_DimsTileOnly`, `MAPL_DimsTileTile`, `MAPL_DimsNone`, `MAPL_ScalarField`, `MAPL_VectorField`, `MAPL_AGrid`, `MAPL_CGrid`, `MAPL_DGrid`

Coupling flags: `MAPL_CplUNKNOWN`, `MAPL_CplSATISFIED`, `MAPL_CplNEEDED`, `MAPL_CplNOTNEEDED`, `MAPL_CplAverage`, `MAPL_CplMin`, `MAPL_CplMax`, `MAPL_CplAccumulate`

Misc flags: `MAPL_DESTINATIONMASK`, `MAPL_UseStarrQsat`, `MAPL_UseGoffGratchQsat`, `MAPL_UseMurphyKoopQsat`, `MAPL_UseCAMQsat`, `MAPL_Unknown`, `MAPL_IsGather`, `MAPL_IsScatter`, `MAPL_TileNameLength`, `MAPL_NoShm`, `MAPL_SUCCESS` (enum), `MAPL_FILE_NOT_FOUND`, `MAPL_FriendlyVariable`, `MAPL_FieldItem`, `MAPL_BundleItem`, `MAPL_StateItem`, `MAPL_NoRestart`, `MAPL_UnitsRadians`, `MAPL_Write2Disk`, `MAPL_Write2RAM`, `MAPL_VLocationNone`, `MAPL_VLocationEdge`, `MAPL_VLocationCenter`, `MAPL_MinMaxUnknown`, `MAPL_AttrGrid`, `MAPL_AttrTile`, `MAPL_Uninitialized`, `MAPL_InitialDefault`, `MAPL_InitialRestart`, `MAPL_DuplicateEntry`, `MAPL_ConnUnknown`, `MAPL_Self`, `MAPL_Import`, `MAPL_Export`, `MAPL_FirstPhase`, `MAPL_SecondPhase`, `MAPL_ThirdPhase`, `MAPL_FourthPhase`, `MAPL_FifthPhase`

Surface types: `MAPL_Ocean`, `MAPL_Lake`, `MAPL_LandIce`, `MAPL_Land`, `MAPL_Vegetated`, `MAPL_NumVegTypes`

Regrid methods: `MAPL_RotateLL`, `MAPL_RotateCube`, `MAPL_HorzTransOrderBinning`, `MAPL_HorzTransOrderBilinear`, `MAPL_HorzTransOrderFraction`, `MAPL_HorzTransOrderSample`

Restart flags: `MAPL_RestartOptional`, `MAPL_RestartSkip`, `MAPL_RestartRequired`, `MAPL_RestartBootstrap`, `MAPL_RestartSkipInitial`

Quantization: `MAPL_NBITS_NOT_SET`, `MAPL_NBITS_UPPER_LIMIT`, `MAPL_NOQUANTIZE`, `MAPL_QUANTIZE_BITGROOM`, `MAPL_QUANTIZE_GRANULAR_BITROUND`, `MAPL_QUANTIZE_BITROUND`, `MAPL_QUANTIZE_MAX_NSD`, `MAPL_QUANTIZE_MAX_NSB`

Masks: `MAPL_MASK_OUT`, `MAPL_MASK_IN`

File types: `MAPL_FILETYPE_NC4`, `MAPL_FILETYPE_TXT`, `MAPL_FILETYPE_BIN`, `MAPL_FILETYPE_UNK`

#### Math constants (`mapl_MathConstants_mod`)

`MAPL_PI_R8`, `MAPL_PI`, `MAPL_DEGREES_TO_RADIANS_R8`, `MAPL_DEGREES_TO_RADIANS`, `MAPL_RADIANS_TO_DEGREES`

#### Physical constants (`mapl_PhysicalConstants_mod`)

`MAPL_STEFAN_BOLTZMANN`, `MAPL_AVOGADRO`, `MAPL_UNIVERSAL_GAS_CONSTANT`

Deprecated aliases: `MAPL_STFBOL`, `MAPL_AVOGAD`, `MAPL_RUNIV`

#### Earth constants (`mapl_EarthConstants_mod`)

`MAPL_MEAN_DRY_SURFACE_PRESSURE`, `MAPL_SECONDS_PER_SIDEREAL_DAY`, `MAPL_GRAVITY`, `MAPL_RADIUS`, `MAPL_ROTATION_RATE_R8`, `MAPL_ROTATION_RATE`, `MAPL_ECCENTRICITY`, `MAPL_SEMIMAJOR_AXIS`, `MAPL_SEMIMINOR_AXIS`, `MAPL_GEO_ORBIT_RADIUS`, `MAPL_KM_PER_DEGREE`, `MAPL_DEGREE_PER_KM`

Deprecated aliases: `MAPL_PSDRY`, `MAPL_GRAV`, `MAPL_OMEGA_R8`, `MAPL_OMEGA`, `MAPL_EARTH_ECCENTRICITY`, `MAPL_EARTH_SEMIMAJOR_AXIS`, `MAPL_KM_PER_DEG`, `MAPL_DEG_PER_KM`

#### Earth atmospheric constants (`mapl_EarthAtmosphericConstants_mod`)

`MAPL_MOLECULAR_WEIGHT_DRY_AIR`, `MAPL_MOLECULAR_WEIGHT_WATER`, `MAPL_MOLECULAR_WEIGHT_OZONE`, `MAPL_GAS_CONSTANT_DRY_AIR`, `MAPL_GAS_CONSTANT_WATER_VAPOR`, `MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_PRESSURE`, `MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_VOLUME`, `MAPL_SPECIFIC_HEAT_WATER_VAPOR_CONST_PRESSURE`, `MAPL_SPECIFIC_HEAT_WATER_VAPOR_CONST_VOLUME`, `MAPL_KAPPA`, `MAPL_MOLECULAR_WEIGHT_RATIO`, `MAPL_CP_VAPOR_OVER_DRY_AIR`, `MAPL_CV_VAPOR_OVER_DRY_AIR`, `MAPL_ADIABATIC_INDEX`, `MAPL_LATENT_HEAT_VAPORIZATION`, `MAPL_LATENT_HEAT_FUSION`, `MAPL_LATENT_HEAT_SUBLIMATION`, `MAPL_REFERENCE_PRESSURE`, `MAPL_MEAN_SURFACE_PRESSURE`, `MAPL_FREEZING_POINT`, `MAPL_CELSIUS_TO_KELVIN`, `MAPL_SPECIFIC_HEAT_ICE`, `MAPL_SPECIFIC_HEAT_WATER`, `MAPL_DENSITY_LIQUID_WATER`, `MAPL_DENSITY_SEAWATER`, `MAPL_DENSITY_SEAICE`, `MAPL_DENSITY_SNOW`, `MAPL_KINEMATIC_VISCOSITY_AIR`, `MAPL_VON_KARMAN`, `MAPL_MINIMUM_WIND_SPEED`

Deprecated aliases: `MAPL_AIRMW`, `MAPL_H2OMW`, `MAPL_O3MW`, `MAPL_RDRY`, `MAPL_RVAP`, `MAPL_CPDRY`, `MAPL_CVDRY`, `MAPL_CPVAP`, `MAPL_CVVAP`, `MAPL_EPSILON`, `MAPL_DELTAP`, `MAPL_DELTAV`, `MAPL_GAMMAD`, `MAPL_RGAS`, `MAPL_CP`, `MAPL_VIREPS`, `MAPL_ALHL`, `MAPL_ALHF`, `MAPL_ALHS`, `MAPL_P00`, `MAPL_SRFPRS`, `MAPL_TICE`, `MAPL_CAPICE`, `MAPL_CAPWTR`, `MAPL_RHOWTR`, `MAPL_RHO_SEAWATER`, `MAPL_RHO_SEAICE`, `MAPL_RHO_SNOW`, `MAPL_NUAIR`, `MAPL_KARMAN`, `MAPL_USMIN`

---

### `mapl_CommGroupDescription_mod` — `mp_utils/CommGroupDescription.F90`

| Entity | Kind |
|---|---|
| `CommGroupDescription` | type + constructor interface |

---

### `mapl_AbstractCommSplitter_mod` — `mp_utils/AbstractCommSplitter.F90`

| Entity | Kind |
|---|---|
| `AbstractCommSplitter` | abstract type |

---

### `mapl_Downbit_mod` — `mp_utils/DownBit.F90`

| Entity | Kind |
|---|---|
| `DownBit` | interface (generic over 1D/2D/3D) |

---

### `mapl_Sleep_mod` — `utils/MAPL_Sleep.F90`

| Entity | Kind |
|---|---|
| `MAPL_Sleep` | subroutine |

---

### `mapl_Profiler_mod` — `mp_utils/profiler/MAPL_Profiler.F90`

No `private`; itself a package re-exporter of ~30 sub-modules. Two names are renamed at the MAPL umbrella level.

| Entity (as seen in MAPL) | Kind | Notes |
|---|---|---|
| `initialize_profiler` | subroutine | Renamed from `initialize` by MAPL umbrella |
| `finalize_profiler` | subroutine | Renamed from `finalize` by MAPL umbrella |
| `report_global_profiler` | subroutine | |
| *(all other profiler entities)* | various | AbstractMeter, MeterNode, BaseProfiler, AdvancedMeter, DistributedProfiler, TimeProfiler, MemoryProfiler, ProfileReporter, CsvProfileReporter, column types, GlobalProfilers, etc. |

---

### `mapl_StateMask_mod` — `infrastructure/fields/state/StateMasking.F90`

| Entity | Kind |
|---|---|
| `StateMask` | type |

---

### `mapl_StateArithmeticParser_mod` — `infrastructure/fields/state/StateArithmeticParser.F90`

| Entity | Kind |
|---|---|
| `parser_variables_in_expression` | function |
| `MAPL_StateEval` | subroutine |
| `CheckSyntax` | subroutine/function |
| `RealNum` | function |
| `LowCase` | function |

---

### `mapl_StateFilter_mod` — `infrastructure/fields/state/StateFilter.F90`

| Entity | Kind |
|---|---|
| `StateFilterItem` | interface (generic over R4 2D/3D) |

---

### `mapl_Generic_mod` — `superstructure/generic/MAPL_Generic.F90`

| Entity | Kind |
|---|---|
| `MAPL_GridCompGetOuterMeta` | interface/subroutine |
| `MAPL_GridCompGetRegistry` | subroutine |
| `MAPL_GridCompAddVarSpec` | subroutine |
| `MAPL_GridCompAddSpec` | subroutine |
| `MAPL_GridCompAdvertiseVariable` | subroutine |
| `MAPL_GridCompIsGeneric` | function |
| `MAPL_GridCompIsUser` | function |
| `MAPL_GridCompGet` | subroutine |
| `MAPL_GridCompSet` | subroutine |
| `MAPL_GridCompSetEntryPoint` | subroutine |
| `MAPL_GridCompAddChild` | subroutine |
| `MAPL_GridCompGetChildName` | function |
| `MAPL_GridCompRunChild` | subroutine |
| `MAPL_GridCompRunChildren` | subroutine |
| `MAPL_GridCompGetInternalState` | subroutine |
| `MAPL_GridCompSetGeometry` | subroutine |
| `MAPL_GridcompGetResource` | subroutine |
| `MAPL_ClockGet` | subroutine |
| `MAPL_GridCompSetGeom` | subroutine |
| `MAPL_GridCompSetVerticalGrid` | subroutine |
| `MAPL_GridCompAddConnection` | subroutine |
| `MAPL_GridCompAddConnectivity` | subroutine (legacy alias) |
| `MAPL_GridCompReexport` | subroutine |
| `MAPL_GridCompConnectAll` | subroutine |
| `MAPL_GridCompTimerStart` | subroutine |
| `MAPL_GridCompTimerStop` | subroutine |
| `MAPL_STATEITEM_STATE` | parameter |
| `MAPL_STATEITEM_FIELDBUNDLE` | parameter |
| `MAPL_STATEITEM_SERVICE` | parameter |
| `MAPL_STATEITEM_VECTOR` | parameter |
| `MAPL_UserCompGetInternalState` | subroutine |
| `MAPL_UserCompSetInternalState` | subroutine |

---

### `mapl_ComponentSpec_mod` — `superstructure/generic/specs/ComponentSpec.F90`

| Entity | Kind |
|---|---|
| `ComponentSpec` | type |
| `MiscellaneousComponentSpec` | type |
| `CheckpointControls` | type |

---

### `mapl_RestartHandler_mod` — `superstructure/generic/RestartHandler.F90`

| Entity | Kind |
|---|---|
| `RestartHandler` | type + constructor interface |

---

### `mapl_ESMF_Time_Utilities_mod` — `infrastructure/esmf/ESMF_Time_Utilities.F90`

| Entity | Kind |
|---|---|
| `check_compatibility` | subroutine |
| `interval_is_all_zero` | function |
| `sub_time_in_datetime` | function |

---

### `mapl_SimpleAlarm_mod` — `infrastructure/esmf/alarm/SimpleAlarm.F90`

| Entity | Kind |
|---|---|
| `SimpleAlarm` | type + constructor interface |

---

### `mapl_StateItemImpl_mod` — `infrastructure/esmf/StateItem.F90`

| Entity | Kind |
|---|---|
| `MAPL_STATEITEM_UNKNOWN` | parameter (`ESMF_StateItem_Flag`) |
| `MAPL_STATEITEM_FIELD` | parameter |
| `MAPL_STATEITEM_FIELDBUNDLE` | parameter |
| `MAPL_STATEITEM_STATE` | parameter |
| `MAPL_STATEITEM_SERVICE` | parameter |
| `MAPL_STATEITEM_SERVICE_PROVIDER` | parameter |
| `MAPL_STATEITEM_SERVICE_SUBSCRIBER` | parameter |
| `MAPL_STATEITEM_WILDCARD` | parameter |
| `MAPL_STATEITEM_BRACKET` | parameter |
| `MAPL_STATEITEM_VECTOR` | parameter |
| `MAPL_STATEITEM_VECTORBRACKET` | parameter |
| `MAPL_STATEITEM_EXPRESSION` | parameter |

---

### `mapl_FieldPointerUtilities_mod` — `infrastructure/esmf/FieldPointerUtilities.F90`

| Entity | Kind |
|---|---|
| `FieldsHaveUndef` | function |
| `GetFieldsUndef` | function |
| `assign_fptr` | subroutine/interface |
| `FieldGetLocalElementCount` | function |
| `FieldGetLocalSize` | function |
| `FieldGetCptr` | function |
| `FieldClone` | function |
| `FieldsAreConformable` | function |
| `FieldsAreBroadcastConformable` | function |
| `FieldsAreSameTypeKind` | function |
| `FieldCopy` | subroutine |
| `MAPL_FieldDestroy` | subroutine |
| `FieldCopyBroadcast` | subroutine |
| `FieldSameData` | function |

---

### `mapl_ErrorHandling_mod` — `utils/ErrorHandling.F90`

Same file as `mapl_ErrorHandling_mod`; exports the identical set of entities (see `mapl_ErrorHandling_mod` above).

---

### `mapl_ISO8601_DateTime_mod` — `utils/MAPL_ISO8601_DateTime.F90`

> Note: the `private` statement is commented out in this module, making all module-level entities implicitly public.

| Entity | Kind | Notes |
|---|---|---|
| `convert_ISO8601_to_integer_time` | function | explicitly public |
| `convert_ISO8601_to_integer_date` | function | explicitly public |
| `ISO8601Date` | type | implicitly public |
| `ISO8601Time` | type | implicitly public |
| `ISO8601DateTime` | type | implicitly public |
| `ISO8601Duration` | type | implicitly public |
| `ISO8601Interval` | type | implicitly public |
| `date_fields` | type | implicitly public (internal helper) |
| `time_fields` | type | implicitly public (internal helper) |
| `operator(.divides.)` | operator interface | |

---

### `mapl_EsmfRegridder_mod` — `infrastructure/geom/regridder_mgr/EsmfRegridder.F90`

| Entity | Kind |
|---|---|
| `EsmfRegridder` | type |
| `EsmfRegridderParam` | type |
| `make_EsmfRegridderParam` | function |

---

### `mapl_RegridderMethods_mod` — `infrastructure/geom/regridder_mgr/RegridderMethods.F90`

| Entity | Kind |
|---|---|
| `REGRID_HINT_LOCAL` | parameter (integer = 1) |
| `REGRID_HINT_FILE_WEIGHTS` | parameter (integer = 2) |
| `REGRID_HINT_COMPUTE_TRANSPOSE` | parameter (integer = 4) |
| `REGRID_METHOD_BILINEAR` | enumerator |
| `REGRID_METHOD_BILINEAR_MONOTONIC` | enumerator |
| `REGRID_METHOD_BILINEAR_ROTATE` | enumerator |
| `REGRID_METHOD_CONSERVE` | enumerator |
| `REGRID_METHOD_CONSERVE_MONOTONIC` | enumerator |
| `REGRID_METHOD_VOTE` | enumerator |
| `REGRID_METHOD_FRACTION` | enumerator |
| `REGRID_METHOD_CONSERVE_2ND` | enumerator |
| `REGRID_METHOD_PATCH` | enumerator |
| `REGRID_METHOD_NEAREST_STOD` | enumerator |
| `REGRID_METHOD_CONSERVE_HFLUX` | enumerator |
| `UNSPECIFIED_REGRID_METHOD` | enumerator (= -1) |
| `regrid_method_string_to_int` | function |
| `regrid_method_int_to_string` | function |
| `generate_esmf_regrid_param` | function |

---

### `mapl_HConfigAs_mod` — `infrastructure/esmf/hconfig/HConfigAs.F90`

| Entity | Kind |
|---|---|
| `HConfigAsItemType` | interface |
| `HConfigAsStateIntent` | interface |
| `HConfigAsTime` | interface |
| `HConfigAsTimeInterval` | interface |
| `HConfigAsTimeRange` | interface |
| `HConfigAsStringVector` | interface |

---

### `mapl_StringTemplate_mod` — `mp_utils/StringTemplate.F90`

| Entity | Kind |
|---|---|
| `fill_grads_template` | subroutine |
| `StrTemplate` | subroutine |
| `fill_grads_template_esmf` | subroutine |

---

### `mapl_FileMetadataUtils_mod` — `base3g/FileMetadataUtilities.F90`

| Entity | Kind |
|---|---|
| `FileMetadataUtils` | type + constructor interface |

---

### `mapl_os_mod` — `utils/OS.F90`

| Entity | Kind |
|---|---|
| `mapl_GetCurrentWorkingDirectory` | interface/function |
| `mapl_ChangeDirectory` | interface/subroutine |
| `mapl_MakeDirectory` | interface/subroutine |
| `mapl_DirectoryExists` | interface/function |
| `mapl_RemoveDirectory` | interface/subroutine |
| `mapl_RemoveFile` | interface/subroutine |
| `mapl_PushDirectory` | interface/subroutine |
| `mapl_PopDirectory` | interface/subroutine |
| `mapl_ClearDirectoryStack` | interface/subroutine |
| `mapl_PathJoin` | interface/function |
| `mapl_MakeSymbolicLink` | interface/subroutine |

---

### `mapl_GridGetGlobal_mod` — `infrastructure/geom/geom/GridGetGlobal.F90`

| Entity | Kind |
|---|---|
| `GridGetGlobalCellCountPerDim` | interface/subroutine |

---

### `mapl_CompressionSettings_mod` — `infrastructure/geom/GeomIO/CompressionSettings.F90`

| Entity | Kind |
|---|---|
| `CompressionSettings` | type + constructor interface |

---

### `mapl_PythonBridge_mod` — `python/PythonBridge.F90`

| Entity | Kind |
|---|---|
| `initialize_python_bridge` | subroutine |
| `MAPL_pybridge_gcrun` | subroutine |
| `MAPL_pybridge_gcrun_with_internal` | subroutine |
| `MAPL_pybridge_gcinit` | subroutine |
| `MAPL_pybridge_gcfinalize` | subroutine |

---

## ESMF Subset: `mapl_ESMF_Subset_mod` — `infrastructure/esmf/ESMF_Subset.F90`

This module provides a controlled, curated subset of ESMF entities so that MAPL components
use MAPL wrappers instead of `use ESMF` directly. It is used by `generic3g`, which is used
by the MAPL umbrella — all entities below are therefore visible through `use MAPL`.

No renames — all entities are re-exported under their original ESMF names.

### Types

| Name |
|---|
| `ESMF_VM` |
| `ESMF_Clock` |
| `ESMF_Alarm` |
| `ESMF_Time` |
| `ESMF_TimeInterval` |
| `ESMF_Config` |
| `ESMF_Geom` |
| `ESMF_Grid` |
| `ESMF_Mesh` |
| `ESMF_LocStream` |
| `ESMF_Xgrid` |
| `ESMF_Field` |
| `ESMF_FieldBundle` |
| `ESMF_State` |
| `ESMF_HConfig` |
| `ESMF_HConfigIter` |
| `ESMF_GridComp` |
| `ESMF_Info` |

### Parameters / Constants

| Name | Type |
|---|---|
| `ESMF_SUCCESS` | integer |
| `ESMF_FAILURE` | integer |
| `ESMF_METHOD_INITIALIZE` | integer |
| `ESMF_METHOD_RUN` | integer |
| `ESMF_METHOD_FINALIZE` | integer |
| `ESMF_STATEINTENT_IMPORT` | `ESMF_StateIntent_Flag` |
| `ESMF_STATEINTENT_EXPORT` | `ESMF_StateIntent_Flag` |
| `ESMF_CALKIND_GREGORIAN` | `ESMF_CalKind_Flag` |

### Subroutines / Functions

| Name |
|---|
| `ESMF_TimePrint` |
| `ESMF_TimeSet` |
| `ESMF_CalendarSetDefault` |
| `ESMF_HConfigAsStringMapKey` |
| `ESMF_HConfigAsString` |
| `ESMF_HConfigCreate` |
| `ESMF_HConfigCreateAt` |
| `ESMF_HConfigDestroy` |
| `ESMF_HConfigIsDefined` |
| `ESMF_HConfigIterBegin` |
| `ESMF_HConfigIterEnd` |
| `ESMF_HConfigIterLoop` |
| `ESMF_HConfigGetSize` |
| `ESMF_VMGet` |
| `ESMF_VMGetCurrent` |
| `ESMF_ClockCreate` |
| `ESMF_ClockGet` |
| `ESMF_InfoGetFromHost` |
| `ESMF_InfoGet` |
| `ESMF_InfoIsSet` |

### Operators

| Operator | Operand types |
|---|---|
| `operator(+)` | `ESMF_Time`, `ESMF_TimeInterval` arithmetic |
| `operator(-)` | `ESMF_Time`, `ESMF_TimeInterval` arithmetic |
| `operator(/)` | `ESMF_TimeInterval` arithmetic |
| `operator(*)` | `ESMF_TimeInterval` arithmetic |
| `operator(==)` | `ESMF_Time`, `ESMF_TimeInterval` comparison |
| `operator(/=)` | `ESMF_Time`, `ESMF_TimeInterval` comparison |
| `operator(<)` | `ESMF_Time`, `ESMF_TimeInterval` comparison |
| `operator(<=)` | `ESMF_Time`, `ESMF_TimeInterval` comparison |
| `operator(>)` | `ESMF_Time`, `ESMF_TimeInterval` comparison |
| `operator(>=)` | `ESMF_Time`, `ESMF_TimeInterval` comparison |

---

*Document generated from source analysis of MAPL3 (`mapl/MAPL.F90` and all transitively used modules).*
