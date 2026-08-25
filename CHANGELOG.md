# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- mlc-disable -->
## [Unreleased]
<!-- mlc-enable -->

### Fixed

- Fixed documentation workflows so manual runs publish only from trusted branches and v2 and MAPL3 documentation deployments preserve each other's output
- Removed deployment and build-cache credentials from pull request jobs and restricted PR workflow tokens to read-only access

### Changed

- Moved DSO-backed child `setServices` ownership into child configurations and added support for raw `ESMF_GridCompCreate` followed by `ESMF_GridCompSetServices` startup.
- Refactored `UserSetServices.F90` to remove the `user_setservices` interface, rename `AbstractUserSetServices` to `UserSetServices`, and giving `ProcSetServices` and `DSOSetServices` their own constructors
- `Regrid_Util.x` now uses the fargparse library for command line argument parsing instead
  of raw Fortran intrinsics. Multi-character options that previously used a single-dash prefix
  (e.g. `-ogrid`, `-nx`, `-ny`, `-method`, `-tp_in`, `-tp_out`, `-lon_range`, `-lat_range`,
  `-stretch_factor`, `-deflate`, `-shave`, `-quantize_algorithm`, `-quantize_level`,
  `-zstandard_level`, `-file_weights`, `-vars`, `-t`) now require a double-dash prefix
  (e.g. `--ogrid`, `--nx`). The short forms `-i` and `-o` are preserved. The `--help` flag
  is now handled automatically by fargparse and prints a formatted usage summary.
### Added

- Added `log_files_read` option to ExtData2G to easily log all files read during a run
- Added `MAPL_FieldApplyUserRoutine`/`MAPL_FieldBundleApplyUserRoutine` to apply a user routine to each slice of a field (or every field in a bundle) with ungridded/vertical dimensions, plus `MAPL_FieldGetPointerToSlice` (overloaded for R4 and R8) for typed per-slice access. Slices are 2D by default, or 3D when the field has exactly three non-ungridded (grid + vertical) dimensions (for example a 4D field whose fourth dimension is the ungridded dimension). The slice-routine interface is unlimited-polymorphic and assumed-rank, so a single user routine handles R4/R8 and 2D/3D slices via `select rank`/`select type`
- `update_restart` in `Cap.F90` now supports a `skip_restart_write` boolean flag in the
  `ESMF_HConfig`. When present and `true`, the routine returns immediately without writing
  the restart file. Default behavior (key absent or `false`) is unchanged.

- `Regrid_Util.x` now has the option to be drive via a yaml file passed on the command line rather than
   a whole list of command line arguments.

- Modified ExtData tests to get path to test data from environment variable `LOCAL_REGRESSION_DATA_DIR`

- Added regression test for Regrid\_Util.x

- External pfio server GridComp and ctest: new `mapl_PfioServerGridComp_mod` provides
  an ESMF GridComp whose `run` phase creates and starts an `MpiServer` or
  `MultiGroupServer`; `MaplFramework` gains `mapl_connect_to_server`,
  `mapl_publish_server`, module-level external-client registry, and
  `finalize_servers` shutdown (sends `terminate` to each external client before
  freeing `DirectoryService` resources); `HistoryGridComp` connects to an external
  server in `GENERIC::INIT_REALIZE`; `MaplServerUtilities` fixed two
  `ESMF_HConfigCreateAt` → `ESMF_HConfigCreateAtMapVal` iterator bugs; added
  2-PET ctest `pfio_server_captest` under `gridcomps/cap/tests/`; fixed multiple
  missing `TARGET` attributes in `pfio` exposed by NAG Fortran debug mode

- Added tests to check the use of ESMF_CALKIND_NOLEAP as the default calendar
- Named default pfio server constants (#5242): new module `mapl_DefaultServerNames_mod`
  exports `MAPL_DEFAULT_INPUT_SERVER` and `MAPL_DEFAULT_OUTPUT_SERVER`; all hardcoded
  `'i_client'`/`'o_client'`/`'i_server'`/`'o_server'` string literals replaced with these
  constants throughout `MaplFramework`, `RestartHandler`, `GeomPFIO`, `GridPFIO`,
  `FieldBundleRead`, `FieldBundleWrite`, `HistoryGridComp`, `ExtDataFileReader`, and
  `PrimaryExport`; fixed `MAX_LEN_PORT_NAME` (16 → 64) to support longer port names
- Added `MAPL_StateMerge` to combine two `ESMF_State` objects into one without allocating new field memory
- MAPL3 initialization lifecycle (#5231): new 6-call application lifecycle
  (`MAPL_Initialize`, `MAPL_CreateServers`, `MAPL_CapCreate`, `MAPL_RunServers`,
  `MAPL_CapRun`, `MAPL_Finalize`) with explicit driver/server arguments and
  fast-fail resource validation; wildcard `'*'` support for `num_nodes` in the
  last server entry; `pfunit` bootstrap updated to call `MAPL_CreateServers`


- Refactor local IO server management (#5239): added `pFIO_StringServerMapMod`
  (`StringServerMap`) for polymorphic server storage; replaced raw `o_server`/
  `i_server` pointers in `MaplFramework` with `local_server_map`; renamed
  `initialize_simple_servers` → `initialize_local_servers` with an
  `add_local_server` helper to eliminate duplication; local servers are now
  always created for model PETs regardless of whether a remote `servers:`
  section is present; `finalize_servers` now clears the map instead of no-op.

- Refactored `pFIO_ClientManagerMod` (#5234): replaced module-level `i_client`/`o_client`
  variables with a `StringClientThreadMap` (public, PROTECTED, TARGET) and a
  `get_client_thread(name)` accessor; updated all call sites in MAPL to use the
  accessor; exposed `mapl_get_client_thread` through `mapl_pfio_api`.
- Added ability to specify per-variable units, precision, averaging type, and regridding method for fields in a history collection
- Changed default to false for run_extdata and run_history in CapGridComp, and modified the necessary yaml files for all tests to pass
- unit tests for server initialization logic (#5214)
- Refactored server initialization (#5214)
  - added tests
- Refactored `pFIO_ClientManagerMod`: replaced `ClientThreadVector` pool with a
  single `class(ClientThread), allocatable` member; removed multi-client cycling
  logic (`next`, `set_current`, `size`, `set_optimal_server`, `split_server_pools`,
  `set_server_size`) and server-pool fields; renamed module-level singletons
  `i_Clients`/`o_Clients` to `i_Client`/`o_Client` and the corresponding
  `mapl_pfio_api` aliases to `mapl_i_client`/`mapl_o_client`.
- Replaced MAPL_UserComp[Set , Get]InternalState with ESMF_InternalState[Set , Get]
- Changed "use esmf" to "import <specifi ESMF objects>" in GeomPFI abstract interfaces
- Added PythonBridge to MAPL interface
- Moved configurable test from superstructure/generic
- Consolidated MAPL ESMF_Info keys into mapl_esmf_info_keys_mod
- Update `components.yaml`
  - ESMA_env v5.24.0
    - Update to GEOSpyD 26.3.2 Python 3.14
    - Update GEOSgcm to use Baselibs 8.32.0
    - Move NAS runs to use Intel MPI by default
  - ESMA_cmake v4.40.0
    - Update ifx and NVHPC flags
    - Better detect FMS/yaml support (needed for spack)
    - Add new `color_message` function
    - Add helper script for regression test work
- For ACG, only declare pointer and get_pointer for MAPL_STATEITEM_FIELD
- For ACG, add spec_filters to generalize testing specs

### Fixed

- Fixed the unreliable feedback from Python bridge failures
- Improved `SimpleConnection` assertion messages for unknown virtual connection points
- Fixed bug in FieldBundleRead when file grid and output bundle grid are different grid classes
- Buggy logic in server initialization (#5214)
- Missing call to initialize error handling in MPI context
- Fixed bug that prevented R8 exports from being written in R8 in History

### Removed

- Removed `ESMF_HCONFIGSET_HAS_INTENT_INOUT` preprocessor conditionals now that
  ESMF 9.0.0 is required (≥ 8.9.0, where `ESMF_HConfigSet` gained `intent(inout)`).
  The `intent(inout)` declarations in `HConfigUtilities.F90`, `OuterMetaComponent.F90`,
  `add_child_by_spec.F90`, and `MAPL_Generic.F90` are now unconditional.
  Updated `INSTALL.md` to reflect the ESMF 9.0.0 minimum requirement.
  Closes [#3477](https://github.com/GEOS-ESM/MAPL/issues/3477).

<!-- mlc-disable -->
## [v3.0.0-alpha.2] - 2026-06-12
<!-- mlc-enable -->

### Changed

- Renamed MAPL public exports to all have "MAPL_" prefix.

<!-- mlc-disable -->
## [v3.0.0-alpha.1] - 2026-06-12
<!-- mlc-enable -->

### Added

- `FieldBundleFilter` for filtering field bundles by predicate.
- Generic checkpointing support: `MAPL_GridCompSetCheckpoint` added to public
  API; `StatisticsGridComp` and `GridComp` now use the generic checkpoint mechanism.
- `MAPL_GridCompAddChild`: new overloads accepting either a setservices procedure
  or a DSO name + procedure name.
- `MAPL_GriddedComponentDriver` and `MAPL_DriverInitializePhases` added to
  public API.
- `StatisticsGridComp`: extended to support variance of a single field.
- `FieldBundleGetPointerToData`: added REAL64 overloads for 2D/3D index/name variants.
- `MAPL_STATEITEM_VECTOR` item type support in ACG spec files.
- `PFIO` layer now has a public API umbrella.
- Re-export `PackedDateCreate`, `PackedTimeCreate`, `PackedDateTimeCreate`, and
  `StrTemplate` through the top-level `MAPL` umbrella module.
- `to_string` (`integer_to_string`) added to `mapl_StringUtilities`.

### Changed

- **MAPL v3 directory restructuring complete**: consolidated sources into
  `infrastructure/`, `superstructure/`, `enums/`, `utils/`, `mp_utils/`, and
  `base/`; renamed `gridcomps/` subdirectories to canonical lowercase names;
  removed all `3g` suffixes from module and directory names; unified the
  `mapl3g_` module namespace under `mapl_`.
- **Public API lockdown**: all layer umbrella modules now carry explicit
  `private` + `public ::` declarations. Internal shim files dissolved; symbols
  routed through proper export umbrellas.
- **Namespace standardization**: all internal module names follow the
  `mapl_<Name>_mod` convention. Unprefixed enum constants and types renamed to
  `MAPL_`-prefixed equivalents. Temporary backward-compatible aliases for unprefixed
  names are provided where needed (e.g. `VerticalStaggerLoc` enums) pending
  updates in downstream consumers.
- `MAPL_GridCompAddVarSpec` replaced by `MAPL_GridCompAddSpec` (avoids exposing
  `VariableSpec` through `use MAPL`); old interface removed.
- `Cap.F90` and `GEOS.F90` moved into `mapl/`; `CapGridComp` now invoked via DSO.
- CI updated to Baselibs 8.32.0 and circleci-tools orb v5; `components.yaml`
  updated to ESMA_env v5.22.0 / GEOSpyD 26.3.2.

### Fixed

- Various compiler fixes: NVHPC build failure in `OpenMP_Support.F90`; `ifx`
  linker issue with error-handling thunks; NAG dangling pointer in checkpoint
  directory helper; IEEE trap suppression for sNaN on `-Ktrap=fp` builds.
- `VariableSpec`/`VectorClassAspect`: fixed vector component naming lifecycle
  (names now resolved at create-time rather than deferred to add-to-state).
- ACG lookup mappings made bidirectional so aliases and actual values are
  interchangeable in spec files.

### Removed

- Legacy error handling interfaces `MAPL_RTRN`, `MAPL_Vrfy`, `MAPL_ASRT`, and
  `mapl_ExceptionHandling_mod`.
- Dead code: `utils/TimeUtilities.F90`, `ESMF_Subset.F90`, and other unused modules.

<!-- mlc-disable -->
## [v3.0.0-alpha.0] - 2026-05-15
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
