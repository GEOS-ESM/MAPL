# MAPL infrastructure layer

**Audience:** coding agents and developers working on `infrastructure/` in MAPL.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [`docs/ARCHITECTURE.md`](../docs/ARCHITECTURE.md), [`AGENTS.md`](../AGENTS.md).
The parent of this repository in the GEOSgcm fixture is documented in
`../../../docs/ARCHITECTURE.md` (code path in the fixture, from the
repository root; not a link).
Deeper document: [`geom/ARCHITECTURE.md`](geom/ARCHITECTURE.md).

## 1. Purpose

`infrastructure/` holds the ESMF-facing building blocks that the generic
layer (`superstructure/generic`), the gridded components (`gridcomps/`)
and the top-level `mapl/` library use: ESMF utilities, field and field-bundle
operations, the field dictionary, geometries and their manager, geometry
file I/O, regridders, and vertical grids and vertical regridding.

## 2. Directory and module map

| Directory | Library target | Contents |
|---|---|---|
| `infrastructure/` itself (`API.F90`, plus the `field_dictionary/` sources) | `MAPL.infrastructure` | Umbrella `mapl_infrastructure_api`; field dictionary sources |
| `esmf/` | `MAPL.esmf` | ESMF utilities, communications, HConfig helpers, ungridded dimensions |
| `field/` | `MAPL.field` | Field create/get/set/fill, arithmetic, units, restart modes |
| `field_bundle/` | `MAPL.field_bundle` | Field-bundle create/get/set/copy/filter |
| `field_dictionary/` | (compiled into `MAPL.infrastructure`) | Standard-name dictionary |
| `geom/` | `MAPL.geom` | Geometry specs, factories, manager (see [`geom/ARCHITECTURE.md`](geom/ARCHITECTURE.md)) |
| `geom_io/` | `MAPL.GeomIO` | Geometry-aware file I/O of field bundles |
| `regridder_mgr/` | `MAPL.regridder_mgr` | Regridder and route-handle managers |
| `vertical/vertical_grid/` | `MAPL.vertical_grid` | Vertical-grid types, factories, manager |
| `vertical/vertical/` | `MAPL.vertical` | Vertical regridding maps and utilities |

Each subdirectory has an `API.F90` that defines one umbrella module
re-exporting a renamed subset of its modules, except `field_dictionary/` and
`vertical/vertical/`, which have none (`esmf/API.F90`, `field/API.F90`,
`field_bundle/API.F90`, `geom/API.F90`, `geom_io/API.F90`,
`regridder_mgr/API.F90`, `vertical/vertical_grid/API.F90`).
`infrastructure/API.F90` (`mapl_infrastructure_api`) uses the umbrellas of
`esmf`, `field`, `field_bundle`, `geom`, `vertical_grid`, `regridder_mgr` and
`geom_io`. The umbrellas use the prefixes `MAPL_` or `mapl_` on re-exported names
(for example `MAPL_FieldCreate => FieldCreate`).

## 3. Public interfaces

Verified from each `API.F90`; only the groups that other layers use are listed.

### esmf (`mapl_esmf_api`)
- `esmf/API.F90` is `private` with an explicit `public ::` list; only the
  names below are exported.
- Communication wrappers: `MAPL_ROOT`, `MAPL_Am_I_Root`, `MAPL_Am_I_Rank`,
  `MAPL_NPES`, `MAPL_Barrier`, `MAPL_CommsSend/Recv/SendRecv`,
  `MAPL_CommsGatherV`, `MAPL_CommsScatterV`, `MAPL_CommsAllGather`,
  `MAPL_CommsAllGatherV`, `MAPL_CommsAllReduceMin/Max/Sum`,
  `MAPL_ArrayGather`, `MAPL_ArrayScatter` (`esmf/comms/Comms.F90`);
  shared-memory variants `MAPL_RoundRobinPEList`, `MAPL_BcastShared`,
  `MAPL_CommsBcast`, `MAPL_CommRequest`, `MAPL_CreateRequest`,
  `MAPL_ArrayIGather`, `MAPL_ArrayIScatter`, `MAPL_CollectiveWait`
  (`esmf/comms/ShmemComms.F90`).
- Configuration: `MAPL_HConfigGet` (`esmf/hconfig_utils/hconfig_get.F90`),
  `MAPL_HConfigMatch` (`esmf/ESMF_HConfigUtilities.F90`), and the
  `mapl_HConfigAs*` converters (`esmf/hconfig/HConfigAs.F90`).
- Metadata on ESMF objects: `MAPL_InfoSet/Get`, shared and private
  variants, `MAPL_InfoSetNamespace` (`esmf/InfoUtilities.F90`).
- Ungridded dimensions: `mapl_UngriddedDim`, `mapl_make_UngriddedDim`,
  `mapl_UngriddedDims`.
- Field pointer utilities (`esmf/FieldPointerUtilities.F90`):
  `MAPL_FieldGetCPtr`, `MAPL_FieldCopy`, `MAPL_AssignFptr`,
  `MAPL_FieldGetLocalElementCount`, `MAPL_FieldClone`, `MAPL_FieldHasDE`.
- `MAPL_SimpleAlarm` (`esmf/alarm/SimpleAlarm.F90`),
  `MAPL_SubTimeInDateTime`.
- Constants: `MAPL_STATEITEM_*` (`UNKNOWN`, `FIELD`, `FIELDBUNDLE`, `STATE`,
  `SERVICE`, `SERVICE_PROVIDER`, `SERVICE_SUBSCRIBER`, `WILDCARD`, `BRACKET`,
  `VECTOR`, `VECTORBRACKET`, `EXPRESSION`) from `esmf/StateItem.F90`, and
  `MAPL_TYPEKIND_MIRROR` from `esmf/TypeKind.F90`.
- The umbrella `use`s `mapl_LU_Bound_mod`, `mapl_HorizontalDimsSpec_mod` and
  `mapl_DistGridGet_mod`, but none of their names is in the `public` list,
  so they are not re-exported. Likewise not exported: the types and routines
  of `mapl_StateItem_mod` and `mapl_typekind_mod`, and
  `mapl_ESMF_Interfaces_mod` (`esmf/ESMF_Interfaces.F90`). Other layers use
  `mapl_StateItem_mod` directly (for example
  `superstructure/generic/transforms/VerticalRegridTransform.F90`).
- `esmf/apps/noleap_calendar` is built by `add_subdirectory (apps)`.

### field (`mapl_field_api`)
`MAPL_FieldCreate`, `MAPL_FieldEmptyComplete`, `MAPL_FieldsAreAliased`,
`MAPL_FieldGet`, `MAPL_FieldSet`, `MAPL_NamedAlias`, `MAPL_FieldFill`,
`MAPL_FieldApplyUserRoutine`, `MAPL_I_FieldSliceRoutine`,
`MAPL_RESTART_REQUIRED`, `MAPL_RESTART_SKIP` (`field/API.F90`). Other field
modules (`FieldBLAS`, `FieldUnaryFunctions`, `FieldBinaryOperations`,
`FieldUnits`, `FieldCondensedArray`, `FieldDelta`, `FieldInfo`, `FieldUtilities`)
are compiled in `MAPL.field` and not re-exported by the umbrella. The
`MAPL.fields` target is an INTERFACE alias linking `MAPL.field`
(`field/CMakeLists.txt`).

### field_bundle (`mapl_field_bundle_api`)
`MAPL_FieldBundleCreate/Clone/Copy/Destroy/Get/GetByIndex/Set/Filter`,
`MAPL_FieldBundleGetPointer`, `MAPL_FieldBundleGetGeom`,
`MAPL_FieldBundleInfoGetInternal/SetInternal`, `MAPL_FieldBundleSameData`,
`MAPL_FieldBundlesAreAliased`, `MAPL_FieldBundleApplyUserRoutine`,
`MAPL_FieldPredicate`; `MAPL_FieldBundleAdd` renames `ESMF_FieldBundleAdd`
(`field_bundle/API.F90`).

### field_dictionary
- `FieldDictionary` (type with `get_item`, `has_item`, `get_units`,
  `get_long_name`, `get_standard_name`, `get_regrid_method`, `add_item`,
  `add_aliases`), `get_field_dictionary`, `load_field_dictionary`
  (`field_dictionary/FieldDictionary.F90`); the constructor reads YAML
  (`new_from_yaml`).
- `FieldDictionaryItem`, `CF_Provenance` (`FieldDictionaryItem.F90`);
  `FieldDictionaryConfig`, `set_field_dictionary_config`,
  `get_field_dictionary_config` (`FieldDictionaryConfig.F90`).
- It has no umbrella module and is not in `mapl_infrastructure_api`.
  `mapl/MaplFramework.F90` builds a `FieldDictionaryConfig` and calls
  `set_field_dictionary_config` and `load_field_dictionary`; the generic layer
  uses it in `superstructure/generic/specs/StandardNameAspect.F90` and
  `superstructure/generic/specs/VariableSpec.F90`.

### geom (`mapl_geom_api`)
`mapl_GeomManager`, `mapl_get_geom_manager`, `mapl_get_mapl_geom`,
`mapl_MaplGeom`, `mapl_GeomSpec`, `mapl_GeomId`, `mapl_SameGeom`,
`mapl_GeomGet`, `mapl_GridGet`, `mapl_GeomGetHorzIJIndex`,
`mapl_GridGetCoordinates`, `MAPL_CollectiveGather3D/Scatter3D`,
`mapl_make_subgrids`, `mapl_CubedSphereGeomSpec`,
`mapl_CubedSphereDecomposition`. Details in
[`geom/ARCHITECTURE.md`](geom/ARCHITECTURE.md).

### geom_io (`mapl_geomio_api`)
`mapl_GeomPFIO` and `mapl_make_geom_pfio` (`GeomPFIO.F90`,
`GeomCategorizer.F90`); `mapl_FieldBundleWriter` (`FieldBundleWrite.F90`);
`mapl_read_bundle` (`FieldBundleRead.F90`); `mapl_DataCollection`,
`mapl_DataCollections`, `MAPL_AddDataCollection` (`DataCollectionManager.F90`);
`mapl_pFIOServerBounds`, `MAPL_PFIO_BOUNDS_READ`; `mapl_CompressionSettings`;
`mapl_bundle_to_metadata`, `mapl_esmf_to_pfio_type`,
`mapl_ungridded_dims_from_metadata` (`SharedIO.F90`).

### regridder_mgr (`mapl_regridder_mgr_api`)
`mapl_Regridder`, `mapl_RegridderSpec`, `mapl_RegridderManager`,
`mapl_get_regridder_manager`, `mapl_EsmfRegridderParam`; the method and hint
constants `MAPL_REGRID_METHOD_*` (bilinear, bilinear-monotonic,
bilinear-rotate, conserve, conserve-monotonic, vote, fraction, conserve-2nd,
patch, nearest-stod, conserve-hflux) and `MAPL_REGRID_HINT_*`; the converters
`mapl_regrid_method_string_to_int`, `mapl_regrid_method_int_to_string`
(`RegridderMethods.F90`).
- `RegridderManager.F90`: singleton `regridder_manager` with `get_regridder`,
  `make_regridder`, `add_regridder`, `delete_regridder`, `add_factory`. Its
  construction registers one factory, `EsmfRegridderFactory`; a second
  factory line is commented out in the source.
- `RoutehandleManager.F90` (`get_routehandle`, `add_routehandle`,
  `delete_routehandle`) caches ESMF route handles; it is compiled in the
  library but not re-exported by `API.F90`.
- Type hierarchy: `Regridder` (abstract), `EsmfRegridder`, `NullRegridder`;
  `RegridderFactory`, `EsmfRegridderFactory`; `RegridderParam`,
  `EsmfRegridderParam`; `DynamicMask.F90`.

### vertical (`mapl_vertical_grid_api` for `vertical_grid`)
- `mapl_VerticalGrid`, `mapl_VerticalGridSpec`, `mapl_VerticalGridFactory`
  (abstract); `mapl_BasicVerticalGrid`, `mapl_BasicVerticalGridSpec`,
  `mapl_BasicVerticalGridFactory`; `mapl_VerticalGridManager`,
  `mapl_get_vertical_grid_manager`; `MAPL_VERTICAL_GRID_NOT_FOUND`
  (`vertical/vertical_grid/API.F90`). `MirrorVerticalGrid.F90` is compiled but
  not re-exported.
- `MAPL.vertical` (`vertical/vertical/`): `VerticalRegridMethod`
  (`VERTICAL_REGRID_LINEAR`, `VERTICAL_REGRID_CONSERVATIVE`,
  `VERTICAL_REGRID_UNKNOWN`), `compute_linear_map`
  (`VerticalLinearMap.F90`), `compute_conservative_map`
  (`VerticalConservativeMap.F90`), `CSR_SparseMatrix.F90` (`matmul`,
  `add_row`), `Eta2Eta.F90`, `VerticalRegridUtilities.F90`,
  `VerticalCoordinate.F90`, `VerticalRegridConserveInterface.F90`. No `API.F90`
  exists there. `superstructure/generic/transforms/VerticalRegridTransform.F90`
  uses `mapl_VerticalLinearMap_mod`, `mapl_VerticalConservativeMap_mod` and
  `mapl_VerticalRegridMethod_mod`; `gridcomps/extdata/PrimaryExport.F90` uses
  one or more of `mapl_Eta2Eta_mod`, `VerticalRegridUtilitiesMod`,
  `VerticalCoordinateMod`.

## 4. Dependencies

From the `CMakeLists.txt` files (`DEPENDENCIES` lists):

| Target | Depends on (MAPL targets) |
|---|---|
| `MAPL.esmf` | `MAPL.enums`, `MAPL.utils`, `MAPL.mp_utils` |
| `MAPL.vertical_grid` | `MAPL.enums`, `MAPL.pfio` |
| `MAPL.vertical` | `MAPL.enums`, `MAPL.utils`, `MAPL.pfio`, `udunits2f` |
| `MAPL.geom` | `MAPL.pfio`, `MAPL.enums`, `MAPL.utils`, `MAPL.esmf`, `MAPL.mp_utils` |
| `MAPL.field` | `MAPL.enums`, `MAPL.vertical_grid`, `MAPL.utils`, `MAPL.esmf`, `MAPL.geom`, `udunits2f` |
| `MAPL.field_bundle` | `MAPL.field`, `MAPL.utils`, `MAPL.esmf`, `MAPL.geom` |
| `MAPL.regridder_mgr` | `MAPL.geom`, `MAPL.field_bundle`, `MAPL.utils` |
| `MAPL.GeomIO` | `MAPL.field`, `MAPL.field_bundle`, `MAPL.geom`, `MAPL.pfio`, `MAPL.base`, `MAPL.utils`, `MAPL.esmf`, `MAPL.regridder_mgr` |
| `MAPL.infrastructure` | `MAPL.esmf`, `MAPL.field`, `MAPL.field_bundle`, `MAPL.geom`, `MAPL.regridder_mgr`, `MAPL.GeomIO`, `MAPL.vertical`, `PFLOGGER::pflogger`, `GFTL_SHARED::gftl-shared-v2`, `ESMF::ESMF` |

`MAPL.vertical_grid` is not listed for `MAPL.infrastructure`; it is reached
through `MAPL.field`, and `infrastructure/API.F90` uses
`mapl_vertical_grid_api`.

Third-party links: `ESMF::ESMF` is linked by every target (for `MAPL.geom`,
`MAPL.vertical` and `MAPL.vertical_grid` through
`target_link_libraries (... PUBLIC ESMF::ESMF)` rather than `DEPENDENCIES`).
Most list `GFTL::gftl-v2` in `DEPENDENCIES`; `MAPL.vertical` does not, and
`MAPL.infrastructure` lists `GFTL_SHARED::gftl-shared-v2` instead.
`PFLOGGER::pflogger` is listed only by `MAPL.esmf`, `MAPL.field`,
`MAPL.field_bundle`, `MAPL.vertical` and `MAPL.infrastructure`.

Direction of dependency:
- Infrastructure uses `enums`, `utils`, `mp_utils`, `pfio`, `base`,
  `udunits2f` (lower layers). Only `MAPL.GeomIO` lists `MAPL.base`.
- Used by: `superstructure/generic` lists `MAPL.regridder_mgr`, `MAPL.geom`,
  `MAPL.vertical_grid`, `MAPL.GeomIO`, `MAPL.vertical`, `MAPL.field_bundle`,
  `MAPL.esmf`, `MAPL.infrastructure`
  (`superstructure/generic/CMakeLists.txt`); `mapl/CMakeLists.txt` lists
  `MAPL.infrastructure`, `MAPL.field`, `MAPL.regridder_mgr`, `MAPL.esmf`,
  `MAPL.GeomIO`.
- Internal order: `esmf` and `vertical_grid` first; then `geom`; `field`;
  `field_bundle`; `regridder_mgr`; `GeomIO`; `infrastructure`.
- `infrastructure/CMakeLists.txt` states that `MAPL.infrastructure` is a
  consolidated library "being built up incrementally" with subdirectories
  absorbed as circular dependencies are resolved, and cites GitHub issue
  #5014. Today only `field_dictionary` is absorbed; the other subdirectories
  keep their own targets. `MAPL.field_dictionary` is an `ALIAS` of
  `MAPL.infrastructure`.

## 5. Build and tests

- All library targets are `TYPE SHARED` and built with `esma_add_library`.
- Subdirectories added by `infrastructure/CMakeLists.txt`: `esmf`, `field`,
  `field_bundle`, `geom`, `regridder_mgr`, `geom_io`, `vertical`.
  `field_dictionary/` is not added as a subdirectory (its own
  `CMakeLists.txt` exists with target `MAPL.field_dictionary` but is not
  read; the sources are listed in `infrastructure/CMakeLists.txt`).
- Tests are pFUnit, added only when `PFUNIT_FOUND`, with
  `EXCLUDE_FROM_ALL`. Test targets and labels (`LABELS "ESSENTIAL"` set in
  each `tests/CMakeLists.txt`):
  - `MAPL.esmf.tests` (`esmf/tests`), `MAPL.alarm.tests`
    (`esmf/alarm/tests`), `MAPL.hconfig.tests` (`esmf/hconfig/tests`),
    `MAPL.hconfig_utils.tests` (`esmf/hconfig_utils/tests`).
  - `MAPL.field.test_fieldcreate`, `MAPL.field.test_utils` (`field/tests`).
  - `MAPL.field_bundle.tests` (`field_bundle/tests`).
  - `MAPL.geom.tests` (`geom/tests`).
  - `MAPL.GeomIO.tests`, `MAPL.pFIOServerBounds.tests` (`geom_io/tests`).
  - `MAPL.regridder_mgr.tests` (`regridder_mgr/tests`).
  - `MAPL.vertical_grid.tests` (`vertical/vertical_grid/tests`).
  - No tests exist for `field_dictionary/` or `vertical/vertical/`.

## 6. Known traps

- The directory `field_dictionary/` has a `CMakeLists.txt` that is not used;
  editing its source list or dependencies changes nothing. The effective list
  is in `infrastructure/CMakeLists.txt`.
- `MAPL.vertical` and `MAPL.vertical_grid` are different targets in nested
  directories `vertical/vertical/` and `vertical/vertical_grid/`.
- `API.F90` umbrellas export only a subset; check the module name when a
  routine is "not found" through `mapl_*_api`.
- `mapl_GridGetHorzIJIndex` is a deprecated stub; see the geometry document.
- Module-file naming: most infrastructure modules are named
  `mapl_<Name>_mod`; some are not (`VerticalCoordinateMod`,
  `VerticalRegridConserveInterfaceMod`, `VerticalRegridUtilitiesMod` in
  `vertical/vertical/`).
- `GeomManager` and `RegridderManager` are process-wide singletons; state
  persists for the life of the program.

## 7. Observations (noted, not fixed)

- `infrastructure/CMakeLists.txt` lists `GFTL_SHARED::gftl-shared-v2` while the
  other targets use `GFTL::gftl-v2`.
- `field_dictionary/CMakeLists.txt` (unused) lists `MAPL.shared` among its
  dependencies; no other infrastructure `CMakeLists.txt` names that target.
- `RegridderManager.F90` carries a commented-out factory registration
  (`horzHorzFluxRegridderFactory`).
- Open question: none.
