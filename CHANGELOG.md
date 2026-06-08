# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- mlc-disable -->
## [Unreleased]
<!-- mlc-enable -->

### Changed

- Change all `MAPL_GridCompAddVarSpec` calls in Gridcomps with `MAPL_GridCompAddSpec` calls to avoid exposting VariableSpec in `use MAPL`. Required changes to interface for `MAPL_GridCompAddSpec` and acg
- Removed old interfaces MAPL\_RTRN, MAPL\_Vrfy, MAPL\_ASRT, mapl\_ExceptionHandling\_mod

### Changed

- Renamed modules with mapl_<name>Impl_mod to mapl_<name>_mod
- Eliminitade "shim" interface libraries.
- Aligned API umbrellas with actual content.  Earlier refactoring had
  done some weird indirect publishing.
>>>>>>> develop
- Eliminate some items from public interface of MAPL.
- Cleanup umbrella in ./base
- Change AddChild interface in history to not expose ChildSpec derived type
- Reverted to use API.F90 instead of Export.F90.  (Elimination of Internal.F90 is for later phase.)
- Major cleanup of Export.F90 in various layers.  But not done yet.
- Adding public items for generic layer.
- Lock down all Export umbrella modules to expose only their declared public API (#5026/#5029).
  Added `private` statements and explicit `public ::` declarations to all 12 `Export.F90` files
  (`enums/`, `utils/`, `mp_utils/`, `base/`, `infrastructure/esmf/`, `infrastructure/field/`,
  `infrastructure/field_bundle/`, `infrastructure/geom/`, `infrastructure/vertical/vertical_grid/`,
  `infrastructure/regridder_mgr/`, `superstructure/generic/`, `superstructure/state/`).
  Dissolved all legacy `API.F90` shim files: `infrastructure/esmf/HConfig_API.F90`,
  `infrastructure/esmf/API.F90`, `infrastructure/esmf/EsmfUtils_API.F90`,
  `infrastructure/esmf/comms/Comms_API.F90`, `infrastructure/geom/API.F90`,
  `infrastructure/regridder_mgr/API.F90`, `superstructure/generic/API.F90`,
  `superstructure/state/API.F90` (plus the three already deleted in a prior increment).
  Their symbols are now routed through the proper export umbrellas.
  Added `mapl_state_export` and `mapl_field_bundle_export` to the top-level `MAPL` umbrella,
  which were previously only reachable via the now-deleted API shims.
  Removed from export umbrellas symbols confirmed to have no external consumers: `XYGeomSpec`,
  `XYGeomFactory`, and `XY_COORD_*` from `geom/Export.F90`; `IntegerPair` from
  `vertical_grid/Export.F90`.
  Added to export umbrellas symbols confirmed to have external consumers: `CubedSphereGeomSpec`,
  `CubedSphereDecomposition`, `AbstractUserSetServices`, `DSOSetServices`, VM/comms utilities,
  and HConfig conversion functions.
  Added `BasicVerticalGrid`, `BasicVerticalGridSpec`, `BasicVerticalGridFactory`, and
  `VerticalGridManager`/`get_vertical_grid_manager` to `vertical_grid/Export.F90` as part
  of the public vertical-grid API.
  Renamed unprefixed enum constants and types to their `MAPL_`-prefixed equivalents across
  ~35 source and test files: `FIELDBUNDLETYPE_*` → `MAPL_FIELDBUNDLETYPE_*`,
  `STATEITEM_ALLOCATION_*` → `MAPL_STATEITEM_ALLOCATION_*`,
  `GENERIC_COUPLER_*` → `MAPL_GENERIC_COUPLER_*`,
  `VectorBasisKind`/`VECTOR_BASIS_KIND_*` → `MAPL_VectorBasisKind`/`MAPL_VECTOR_BASIS_KIND_*`,
  `FieldBundleType_Flag` → `MAPL_FieldBundleType_Flag`.
- Update CI to use Baselibs 8.32.0 and circleci-tools orb v5
- Update `components.yaml`
  - ESMA_env v5.22.0
    - Update to GEOSpyD 26.3.2 Python 3.14

### Fixed

- Different bug introduced when fixing bug just below this entry.
- get_checkpoint_subdir() as causing dangling pointer for NAG compiler.
- Remove `FileMetadataUtils` dependency from `MAPL.vertical` (#5017). `VerticalCoordinate`
  now takes `FileMetadata` (pfio) directly instead of the `FileMetadataUtils` wrapper.
  `udunits2f` is now an explicit dependency of `MAPL.vertical`. Prerequisite for #5014.

- Wrap `ieee_is_nan` calls in `Test_FieldFill.pf` with `ieee_set_halting_mode`/`ieee_set_flag`
  to suppress and clear the IEEE invalid-operation trap around sNaN queries. Without this,
  `-Ktrap=fp` (nvfortran) causes SIGFPE when a signaling NaN is loaded for inspection.
  Zero-diff for all non-test code.

- Guard `ieee_set/get_halting_mode` and `ieee_set_flag` calls in `Test_FieldFill.pf` with
  `#ifndef __DARWIN` to fix a runtime error on macOS with NAG (`IEEE_SET_HALTING_MODE is not
  supported`). IEEE halting mode is not supported on macOS/ARM regardless of compiler. The
  `__DARWIN` macro is already defined on Apple builds. Fixes #5023.

### Changed

- Rename `Export_<subdir>.F90` and `Internal_<subdir>.F90` umbrella files to
  `Export.F90` and `Internal.F90` in all in-scope subdirectories (#5021).
  Affected: `enums/`, `infrastructure/field/`, `infrastructure/field_bundle/`,
  `infrastructure/esmf/`, `infrastructure/geom/`, `infrastructure/regridder_mgr/`,
  `infrastructure/vertical/vertical_grid/`, `mp_utils/`, `superstructure/generic/`,
  `superstructure/state/`, `utils/`. Module names are unchanged. Legacy `API.F90`
  files with active external consumers are preserved; unused ones (`enums/API.F90`,
  `utils/API.F90`) are deleted. Superseded `MpUtilsExport/Internal.F90` and
  `UtilsExport/Internal.F90` stubs are also removed.


- Move `VerticalStaggerLoc` from `MAPL.vertical_grid` into `MAPL.enums` (#5014).
  `MAPL.enums` is now a dependency of `MAPL.vertical_grid`; backward-compatible
  re-exports of the unqualified names are preserved in `Export_vertical_grid.F90`.

- Restore `MAPL.field_bundle` and `MAPL.state` as proper separate CMake libraries
  (#5014). A previous agent had consolidated `field/`, `field_bundle/`, and `state/`
  sources into a single `MAPL.field` target using relative `../` paths and interface
  shims. Each subdirectory now has its own `CMakeLists.txt`, its own
  `Export_*.F90` umbrella module, and is built as an independent shared library.

- Standardize umbrella module filenames to `Internal_<subdir>.F90` / `Export_<subdir>.F90`
  convention (#5010). Eliminates filename collisions when multiple subdirectories contribute
  sources to a single CMake library target (fixes Intel CI failure). Affects all subdirectories
  that previously used generic `Internal.F90` / `Export.F90` names, and renames the
  `Field`-prefixed variants in `infrastructure/field/`, `field_bundle/`, and `state/` to match
  the new convention. Fortran module names are unchanged.

### Added

- Implement two-tier umbrella module pattern across all MAPL subdirectories (#5004).
  Created `Internal.F90` (internal MAPL API) and `Export.F90` (public external API) umbrella
  modules for 10 major subdirectories: `mp_utils/`, `enums/`, `utils/`, `base/`,
  `infrastructure/esmf/`, `infrastructure/field/` (aggregate of field/, field_bundle/, state/),
  `infrastructure/geom/`, `infrastructure/vertical/vertical_grid/`,
  `infrastructure/regridder_mgr/`, and `superstructure/generic/`. Each `Internal.F90` aggregates
  all leaf modules from its subdirectory with no `private` statement, allowing all symbols to
  flow through for use by MAPL sibling subdirectories. Each `Export.F90` selectively re-exports
  only MAPL-prefixed public API symbols, providing a clean interface for external consumers.
  Updated top-level `mapl/MAPL.F90` to import all new `*_export` modules alongside legacy API
  modules for backward compatibility. This establishes the foundation for future API cleanup
  and symbol namespace management. Sibling consumers of `enums/` leaf modules migrated to use
  `mapl_Enums_internal` with MAPL-prefixed names (~48 files across infrastructure/ and
  superstructure/). Cleaned up duplicate symbols: deleted unused `utils/TimeUtils.F90` module
  and its tests (had zero consumers), consolidated duplicate `is_digit` implementation, removed
  duplicate `UnpackDateTime` from `Regrid_Util.F90`, and resolved `KEY_UNITS`/`KEY_TYPEKIND`
  naming conflicts between esmf and history layers using selective `only:` imports. Created
  GitHub issue #5005 for deferred `utils/` sibling migration. Zero-diff for all passing tests.
- Added support for MAPL_STATEITEM_VECTOR ITEMTYPE in ACG

### Changed

- Fixed uninitialized error in RoutehandleParam.F90 (fixes #5001)
- Hide 30 unused entities from MAPL umbrella module (#4999, part of #4975/#4969).
  Removed unused public exports across 5 layers using 'only:' clauses:
  Utils layer (2): MAPL_ObjectWrite, MAPL_ObjectRead;
  ESMF layer (6): ESMF_ConfigCopy{Logical,Integer,String,Real}, ESMF_FieldIsDone, ESMF_VmGetLocal;
  HConfig layer (2): HConfigIterator, HConfig_2_ESMF_Config;
  Base layer (9): MAPL_Var{Write,Read}, MAPL_VarWriteNCFileClose, MAPL_VarReadNCFileClose,
  MAPL_NCIO{WriteToFile,ChangeRes,GetFileType}, SunOrbitCreated, MAPL_SunGet{LocalTime,DaylightDuration,Insolation};
  Superstructure layer (3): CheckSyntax, RealNum, LowCase;
  MP Utils layer (5+3 modules): Removed full module imports for mapl_SplitCommunicator_mod,
  mapl_SimpleCommSplitter_mod, mapl_CommGroupDescription_mod, mapl_AbstractCommSplitter_mod,
  mapl_Downbit_mod (entities: SplitCommunicator, NULL_SUBCOMMUNICATOR_NAME, SimpleCommSplitter,
  CommGroupDescription, AbstractCommSplitter, DownBit).
  All removed entities were unused in gridcomps. Breaking change for external code using removed
  entities - must import leaf modules directly. Zero-diff for ported client repos.
- Create utils API umbrella to limit exported unprefixed entities (#4999, part of #4975/#4969).
  Created `utils/API.F90` that exports only used entities from String, StringUtilities, and OS
  utilities. Removed from public API: unused StringUtilities functions (to_lower, to_upper,
  capitalize, is_alpha, is_alpha_only, is_numeric, is_alphanumeric, to_string, to_character_array,
  is_digit, get_ascii_interval, is_alphanum_character, is_lower_character, is_upper_character),
  all FileSystemUtilities functions (get_file_extension, get_file_basename), all DSO_Utilities
  functions (is_valid_dso_name, is_valid_dso_extension, is_supported_dso_name,
  is_supported_dso_extension, adjust_dso_name, SYSTEM_DSO_EXTENSION), and all DirPath entities
  (DirPath type, dirpaths variable). Updated MAPL.F90 to use utils API umbrella instead of
  individual leaf modules. Breaking change for any external code using removed entities.
  Still exported: String (type), split, lowercase, uppercase, and all mapl_os_mod functions.
  Zero-diff for ported client repos (removed entities were unused).
- Add `mapl_` prefix to `initialize_constants` subroutine (#4976, part of #4975/#4969).
  Renamed `initialize_constants()` → `mapl_initialize_constants()` in
  `utils/Constants/Constants.F90` to follow MAPL3 naming conventions (lowercase `mapl_`
  prefix for procedures). Breaking change if any external code was calling
  `initialize_constants` directly (unlikely as subroutine is currently empty/placeholder).
  All `MAPL_*` constants remain publicly accessible. Zero-diff.
- Consolidate enums into `enums/` layer and introduce `MAPL_` API constants (#4973, part of #4969).
  Moved `StateItemAllocation` and `FieldBundleType_Flag` from `infrastructure/field/` and
  `infrastructure/field_bundle/` respectively into the `enums/` layer. Created `enums/API.F90`
  that exports all enum constants with a `MAPL_` prefix (e.g., `MAPL_STATEITEM_ALLOCATION_ACTIVE`,
  `MAPL_FIELDBUNDLETYPE_BASIC`), establishing the public API pattern for MAPL enums. Updated
  internal MAPL clients to use enum modules directly. Breaking change for external clients using
  unprefixed type names (`StateItemAllocation`, `FieldBundleType_Flag`) - these are no longer
  exported; use MAPL_-prefixed constants instead. Zero-diff.
- Flatten `infrastructure/` directory tree (#4972, part of #4969).
  Removes one level of nesting from the fields and geom sublayers:
  `infrastructure/fields/field/` → `infrastructure/field/`,
  `infrastructure/fields/field_bundle/` → `infrastructure/field_bundle/`,
  `infrastructure/fields/state/` → `infrastructure/state/`,
  `infrastructure/geom/geom/` → `infrastructure/geom/`,
  `infrastructure/geom/GeomIO/` → `infrastructure/geom_io/`,
  `infrastructure/geom/regridder_mgr/` → `infrastructure/regridder_mgr/`.
  No source changes; CMake targets (`MAPL.fields`, `MAPL.geom`, `MAPL.GeomIO`,
  `MAPL.regridder_mgr`) are unchanged. Zero-diff.
- Remove `3g` suffix from all module and directory names (#4971, part of #4969).
  Renames `base3g/` → `base/`, `Generic3g.F90` → `Generic.F90`, module `generic3g`
  → `mapl_Generic`, `mapl_Generic3g_API_mod` → `mapl_Generic_API_mod`,
  `mapl_base3g_mod` → `mapl_base_mod`, and CMake targets `MAPL.generic3g` →
  `MAPL.generic` and `MAPL.base3g` → `MAPL.base`. Downstream clients require only
  recompilation; no source changes needed.
- Migrate `base3g/` comms code to the infrastructure layer (#4970, part of #4969).
  Deletes `base3g/Comms.F90` (`MAPL_CommsMod`, 1636 lines) and the entire
  `base3g/include/` directory (16 `.H` files). Introduces two new modules:
  `mapl_ShmemComms_mod` (`infrastructure/esmf/comms/ShmemComms.F90`) for
  shared-memory-aware broadcast operations, and `mapl_GridComms_mod`
  (`infrastructure/geom/geom/GridComms.F90`) for 3-D collective scatter/gather.
  `MAPL_CollectiveGather3D` and `MAPL_CollectiveScatter3D` remain accessible
  via `USE MAPL` through re-export from `mapl_Geom_API_mod`. Zero-diff.

### Added

- Re-export `PackedDateCreate`, `PackedTimeCreate`, `PackedDateTimeCreate` (from
  `MAPL_PackedTimeMod`) and `StrTemplate` (from `MAPL_StringTemplate`) via the
  `mapl_mp_utils` API so they are accessible through `USE MAPL` without client
  code needing to reference internal submodules directly (fixes #4963).

### Fixed

- Fix NVHPC compiler build failure in `superstructure/generic/OpenMP_Support.F90`:
  replace `ESMF_UserCompGetInternalState` and `ESMF_UserCompSetInternalState` with
  their MAPL wrapper equivalents (`MAPL_UserCompGetInternalState` /
  `MAPL_UserCompSetInternalState`).
- Relaxed the comparison standard for grid_is_ok in case a grid is r4
- Fix `mapl/MAPL.F90` to use `mapl_ErrorHandling` directly instead of the thin
  `mapl_ErrorHandlingMod` wrapper, resolving an `ifx` linker issue with bare
  `mapl_return_` thunks in client code.
- Fix vector component naming lifecycle in `superstructure/generic/specs/VariableSpec.F90`
  and `superstructure/generic/specs/VectorClassAspect.F90` by using resolved
  `vector_component_names` when constructing `VectorClassAspect` and setting
  component field names during create-time rather than deferring in add-to-state
  (PR #4946).

- Fix client code in `gridcomps/` and `base3g/` broken by the Phase 9
  `mapl3g_` → `mapl_` rename: replace all `use mapl3g_*` statements with
  `use MAPL` (umbrella) or `use mapl_*` (for symbols not yet re-exported by
  the umbrella). Affected files: `ExtDataGridComp_private.F90`,
  `PrimaryExport.F90`, `ConfigurableGridComp.F90`, `StatisticsGridComp.F90`,
  `MAPL_OrbGridCompMod.F90`, `HistoryCollectionGridComp_private.F90`,
  `FileIOShared.F90`, `MAPL_LocStreamMod.F90`, `NCIO.F90`,
  `SimpleBundleMod.F90`, `StateItem.F90`, `FieldDictionaryConfig.F90`,
  and associated test files in `gridcomps/` (#4944).
- Rename `mapl3g_TimeVariance`, `mapl3g_AbstractCovarianceKernel`,
  `mapl3g_ShiftedCovarianceKernel`, and `mapl3g_WelfordCovarianceKernel`
  to `mapl_` prefix (modules added after Phase-9 branched from develop).
- Fix lookup mappings in ACG to be bidirectional so users can use aliases or
  actual values in columns in ACG spec files.
### Added
- Extended StatisticsGridComp to support variance of a single field.
- Extended `FieldBundleGetPointerToData` interface with REAL64 pointer overloads
  for index/name and 2D/3D variants (PR #4948).

### Changed

- Rename all internal MAPL modules from `mapl_<Name>[Mod]` to `mapl_<Name>_mod`
  convention (#4958). Affects 446 module definitions across 695 source files.
  Three thin-wrapper duplicate modules removed (`mapl_ErrorHandlingMod`,
  `mapl_KeywordEnforcerMod`, `MAPL_ShmemMod`). Duplicate `VerticalAlignment.F90`
  removed from `superstructure/generic/specs/` (canonical copy remains in
  `infrastructure/vertical/vertical_grid/`). Legacy `base3g/Comms.F90` retains
  `MAPL_CommsMod` name pending resolution of #4961. `MAPL_Constants` retains its
  name as it functions as an umbrella module.

- Remove `MAPL_GridCompsMod` and enforce that `gridcomps/` modules `use MAPL`
  (the umbrella module) rather than internal modules directly (#4959).

- Resolve Intel Fortran error #6450 (case-insensitive module/alias name
  collision) for 13 modules in `infrastructure/fields/` whose module name
  matched their sole public symbol when renamed via USE aliases in the layer
  API modules. Fix: rename the internal module declarations by appending `Impl`
  (e.g. `mapl_FieldGet` → `mapl_FieldGetImpl`, `mapl_StateGet` →
  `mapl_StateGetImpl`, etc.). The public API symbols (`MAPL_FieldGet`,
  `MAPL_StateGet`, etc.) and the API module names (`mapl_Field_API`,
  `mapl_State_API`, etc.) are unchanged. All internal consumers updated
  (#4944). Affected modules: `mapl_FieldGet`, `mapl_FieldSet`, `mapl_FieldFill`,
  `mapl_FieldCreate`, `mapl_FieldBundleGet`, `mapl_FieldBundleSet`,
  `mapl_FieldBundleCopy`, `mapl_FieldBundleCreate`, `mapl_FieldBundleGetByIndex`,
  `mapl_FieldBundleGetPointer`, `mapl_StateGet`, `mapl_StateGetGeom`,
  `mapl_StateGetPointer`, `mapl_FieldBundleDestroy`, `mapl_StateDestroy`,
  `mapl_StateItem`, `mapl_StateAddMethod`.

- Rename `mapl_GeomGet` → `mapl_GeomAccessors` and `mapl_GridGet` →
  `mapl_GridAccessors` (previously named `mapl_GeomQueries`/`mapl_GridQueries`
  in an earlier step) to resolve Intel Fortran error #6450. Also renamed
  `mapl_GeomGetHorzIJIndex` and `mapl_GridGetHorzIJIndex` (deprecated stub)
  into `mapl_GeomAccessors`. The `*Accessors` naming is consistent with
  `mapl_FieldAccessors` and correctly describes modules that provide both
  read and write access to ESMF objects. All internal consumers updated.
  Public API symbols unchanged (#4944).

- Merge `mapl_GeomGetHorzIJIndex` and `mapl_GridGetHorzIJIndex` (deprecated
  stub) into `mapl_GeomGet` (`infrastructure/geom/geom/GeomGet.F90`). All three
  were single-public-symbol modules causing Intel Fortran error #6450
  (case-insensitive module/alias name collision in `geom/API.F90`). Public names
  are now `mapl_GeomGet`, `mapl_GeomGetHorzIJIndex`, and `mapl_GridGetHorzIJIndex`
  directly in `mapl_GeomGet`; unprefixed aliases retained for internal use.
  `GeomGetHorzIJIndex.F90` and `GridGetHorzIJIndex.F90` deleted (#4944).

- Consolidate `mapl_MaxMin` and `mapl_AreaMean` (formerly in `utils/arrays/`)
  into a single module `mapl_ArrayReductions` in `mp_utils/`. Both are
  MPI-dependent parallel collective reductions and belong in `MAPL.mp_utils`
  rather than `MAPL.utils`. The public names are now `MAPL_MaxMin` and
  `MAPL_AreaMean` (previously `MaxMin` and `AreaMean` exposed via renaming
  aliases in `mapl_Utilities`). The old single-symbol modules caused Intel
  Fortran error #6450 (case-insensitive module name collision with USE rename
  aliases). Fixes build failure introduced in Phase 9 (#4944).

- Introduce `mp_utils/API.F90` (`module mapl_mp_utils`) as the canonical
  layer-level re-export module for `MAPL.mp_utils`, following the `API.F90`
  pattern used by `base3g/`, `infrastructure/esmf/`, etc. Replaces
  `utils/utilities.F90` (`module mapl_Utilities`) which was MPI-free in name
  but MPI-dependent in practice. The top-level `MAPL` umbrella now uses
  `mapl_mp_utils` instead of `mapl_Utilities`. Internal consumers updated.

- Phase 9 of MAPL v3 directory restructuring (#4905, closes #4944): rename all
  `mapl3g_` module/submodule name prefixes to `mapl_` throughout the codebase
  (531 files, ~2400 substitutions). The `mapl3g_` prefix was a transitional
  namespace used during the MAPL2→MAPL3 migration; the two namespaces are now
  unified under `mapl_`. Client code should access all public symbols through
  the top-level `MAPL` module rather than `use`-ing internal `mapl_*` modules
  directly. User-facing documentation in `docs/` updated to match.

- Dissolve `esmf_utils/` stub: move `comms/` (MAPL_Comms.F90, API.F90, and
  associated header files) into `infrastructure/esmf/comms/`; add sources to
  `MAPL.esmf` target; remove `add_subdirectory(esmf_utils)` from top-level
  `CMakeLists.txt`. Part of the MAPL v3 directory restructuring (#4905).
- Dissolve `geom/` stub: move `CMakeLists.txt` build logic into
  `infrastructure/geom/geom/CMakeLists.txt`; remove `add_subdirectory(geom)`
  from top-level `CMakeLists.txt`. `MAPL.geom` target is now built entirely
  within `infrastructure/geom/`. Part of the MAPL v3 directory restructuring
  (#4905).
- Create `infrastructure/esmf/` (`MAPL.esmf`, Tier 3) consolidating
  `esmf_utils/`, `vm/`, `alarm/`, `hconfig/`, `hconfig_utils/`, and
  ESMF-related files from `generic3g/`. Backward-compatibility INTERFACE
  aliases provided for all five former library names.
- Phase 8 of MAPL v3 directory restructuring (#4905, closes #4942): lowercase
  remaining top-level directories: `mapl3g/`→`mapl/`, `Python/`→`python/`,
  `Tests/`→`tests/`. Install paths updated to match (`lib/Python`→`lib/python`,
  `${esma_include}/Tests`→`${esma_include}/tests`).
  Dissolve `shared/` directory entirely: `DownBit.F90` moved to `mp_utils/`,
  `ShaveMantissa.c/.h` moved to `utils/`, `hinterp.F90` deleted (dead code),
  `MaplShared.F90` deleted (umbrella module replaced by direct `use` of underlying
  modules in three consumers). All `shared/tests/` pFUnit sources moved to
  `utils/tests/` and merged into its `CMakeLists.txt`. A backward-compatibility
  INTERFACE alias `MAPL.shared → MAPL.mp_utils` is provided pending retirement
  of all internal consumers (tracked in #4942).
- Phase 7 of MAPL v3 directory restructuring (#4905, closes #4940): rename
  `Apps/`→`apps/`; move pfio demo executables into `pfio/programs/`; rename
  `mp_utils/profiler/demo/`→`mp_utils/profiler/examples/`.
- Phase 6 of MAPL v3 directory restructuring (#4905, closes #4938): rename
  `gridcomps/` subdirectories from legacy names to canonical lowercase names:
  `cap3g/`→`cap/`, `History3G/`→`history/`, `ExtData3G/`→`extdata/`,
  `StatisticsGridComp/`→`statistics/`, `Orbit/`→`orbit/`. CMake target names
  updated throughout: `MAPL.cap3g`→`MAPL.cap`, `MAPL.history3g`→`MAPL.history`,
  `MAPL.extdata3g`→`MAPL.extdata`, `MAPL_StatisticsGridComp`→`MAPL.statistics`.
- Phase 5 of MAPL v3 directory restructuring (#4905, closes #4930): consolidate
  `component/` and the remainder of `generic3g/` (after Phase 4 extractions)
  into `superstructure/component/` and `superstructure/generic/` respectively.
  Old top-level directories retained as stubs for backward compatibility.
- Phase 4 of MAPL v3 directory restructuring (#4905, closes #4925): consolidate
  `geom/`, `GeomIO/`, `regridder_mgr/`, `field/`, `field_bundle/`, `state/`,
  `vertical/`, `vertical_grid/`, and ESMF-related sources into `infrastructure/`
  subdirectories. Several files from `base3g/` moved to `mp_utils/`. Old
  top-level directories retained as stubs for backward compatibility.

- Create `utils/` directory (`MAPL.utils`, Tier 1) as an MPI-free library
  consolidating serial-only sources from `shared/`, `utilities/`,
  and `generic3g/`. `MAPL.utils` depends only on ESMF and OpenMP — no MPI.
  Part of the MAPL v3 directory restructuring (#4905, phase 2a, closes #4915).

- Remove direct MPI dependencies from `ErrorHandling.F90` and `MAPL_Throw.F90`
  using a lazy-init procedure pointer pattern. Both modules now default to serial
  `error stop` behavior and are fully MPI-free. MPI-aware error handling (rank in
  error messages, `MPI_Abort`) is provided by the new `MAPL_MpiErrorHandling.F90`
  module; call `MAPL_initialize_error_handling()` once after `MPI_Init` to
  register MPI-aware handlers. Serial and pFUnit programs require no changes.
  `MAPL_ExceptionHandling` is now an alias defined in `ErrorHandling.F90` rather
  than a separate file. Closes #4917, part of #4905.

- Create `enums/` directory (`MAPL.enums`, Tier 0) and relocate 11 enum-like
  type definitions into it from `esmf_utils/`, `generic3g/`, and `component/`.
  No module names or interfaces changed; this is a pure file relocation.
  Part of the MAPL v3 directory restructuring (#4905, phase 1a, closes #4907).

- Several files were renamed, MAPL_InfoSet was replaced with ESMF_InfoSet.
- Update `.mlc.toml` to ignore directories either brought in by mepo or created
by opencode
- Rename apps/MAPL_GridCompSpecs_ACGv3.py to MAPL_GridCompSpecs_ACG.py

### Removed
- Remove `UnpackDateTime` from utils/TimeUtilities.F90

### Deprecated

<!-- mlc-disable -->
## [v3.0.0-alpha-0] - 2026-05-15
<!-- mlc-enable -->

### Added

- Add [`docs/mapl3/diffs-from-mapl2.md`](docs/mapl3/diffs-from-mapl2.md) — a comprehensive
  overview of the architectural and user-facing differences between MAPL v3 and MAPL v2.
  This document covers component structure, connections, field specifications, resource
  files, Cap/time-loop changes, History3G, ExtData, the new Statistics component,
  clocks, and build system changes.  It is intended as the primary migration reference
  for developers and users moving from MAPL2 to MAPL3.
- Add [`docs/mapl3/api-changes.md`](docs/mapl3/api-changes.md) — a procedure-level
  reference of core framework API changes: stubbed-out V2 procedures, new MAPL3
  framework entry points (`MAPL_initialize`, `MAPL_finalize`, `MaplFramework`),
  and replacements for lifecycle, child management, field specs, connectivity,
  resource access, and timer APIs.

## Previous Versions

- **Note to Developers**: For MAPL v2 changes, please refer to the CHANGELOG.md for specific tags or for the [CHANGELOG.md in the `release/v2` branch}(https://github.com/GEOS-ESM/MAPL/blob/release/v2/CHANGELOG.md). From now on, all MAPL v3 changes will be documented in this CHANGELOG.md file. The `release/v2` branch will continue to maintain its own CHANGELOG.md for v2-specific changes until the end of support for MAPL v2.
