# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- mlc-disable -->
## [Unreleased]
<!-- mlc-enable -->

### Fixed

- Fixed `MAPL_LocStreamGet` accessing unassociated geometry when requesting
  `tilelons` or `tilelats` with GNU Fortran.

### Added

### Changed

### Removed

### Deprecated

<!-- mlc-disable -->
## [v3.0.0-alpha.3] - 2026-09-25
<!-- mlc-enable -->

### Added

- Enforced `standard_name` agreement across connected Imports and Exports
  (GEOS-ESM/MAPL#5413). Unset Import names accept any Export; an unnamed
  Export warns but connects. Mismatches warn and use the Export name by default,
  or fail in strict mode. Vector component names are checked individually.
  Configure strict mode with
  `field_dictionary: {path: ..., validation_mode: strict}`; the existing
  `field_dictionary: <path>` form remains valid and defaults to permissive.
- Added optional `coordinate_tolerance` for comparing file-based LatLon grids
  (GEOS-ESM/MAPL#5385). It is a fraction of the new grid's spacing; ExtData
  collections default to `0.1` (10%) and can specify `0` for exact comparison.
  Other geom clients default to exact comparison.
- Added in-memory checkpoint/restart support.
- Added vector statistics (average, min, max, accumulation, and variance) in
  History and StatisticsGridComp.
- Added `MAPL_FieldApplyUserRoutine`/`MAPL_FieldBundleApplyUserRoutine` and
  `MAPL_FieldGetPointerToSlice` for applying a routine to R4/R8 field slices.
- Added `MAPL_StateMerge` to combine states without allocating new field memory,
  and `StateGetPointer` overloads for paired vector-field pointers.
- Added per-variable units, precision, averaging type, and regridding method
  for History collections.
- Added `extdata_dryrun_check.py` to predict ExtData input files, optionally
  checking file existence and narrowing by NetCDF time axes; added
  `log_files_read` to record files actually read.
- Added `latlon_to_face.py` for converting cubed-sphere NetCDF files to the
  MAPL/GEOS face layout.
- Added `skip_restart_write` to the CapDriver HConfig to suppress restart-file
  writes when set to `true`.
- Added an external pfio server GridComp and server lifecycle support for
  History; added named default input/output server constants.
- Added PythonBridge to the MAPL interface.

### Changed

- Declared `standard_name`s now always supply FieldDictionary
  defaults for `long_name` and `units`. Remove `use_field_dictionary=` from
  `make_VariableSpec`/`MAPL_GridCompAddSpec` calls; strict mode also rejects
  names missing from the dictionary.
- Split `cap.yaml` into `mapl.yaml`, `cap_driver.yaml`, and
  `cap_gridcomp.yaml`; `mapl.yaml` points to the driver config, which points
  to the gridcomp config. Renamed `model_petcount`/`has_model_petcount` to
  `app_petcount`/`has_app_petcount`, and `mapl_Cap_mod` to
  `mapl_CapDriver_mod`.
- `Regrid_Util.x` now uses fargparse: multi-character options
  require `--` (e.g. `--ogrid` instead of `-ogrid`); `-i` and `-o` remain.
  It can also read options from a YAML file.
- ExtData vector-variable lists now use YAML sequences instead of
  semicolon-separated strings.
- The default RouteHandle line type is now `LINETYPE_GREAT_CIRCLE` for all
  methods.
- MAPL applications now follow an explicit six-call lifecycle
  (`MAPL_Initialize`, `MAPL_CreateServers`, `MAPL_CapCreate`,
  `MAPL_RunServers`, `MAPL_CapRun`, `MAPL_Finalize`). Server ownership and
  initialization were refactored; local servers are created for model PETs
  even when remote servers are configured. The last server's `num_nodes`
  accepts `'*'`.
- DSO-backed child `setServices` ownership moved into child configurations;
  startup via `ESMF_GridCompCreate`/`ESMF_GridCompSetServices` is supported.
- `UserSetServices` replaces `AbstractUserSetServices`; the `user_setservices`
  interface was removed in favor of `ProcSetServices`/`DSOSetServices`
  constructors. Use `ESMF_InternalStateSet`/`ESMF_InternalStateGet` in place of
  the MAPL user-component internal-state wrappers.
- ACG Writer accepts AddSpec arguments in any order.
- `run_extdata` and `run_history` now default to false in CapGridComp.
- `MAPL_Initialize` can return the parsed app config to Cap create/run, so
  `cap_driver.yaml` is read only once.
- ESMF 9.0.0 is now the minimum supported version; the build uses
  `ESMA_cmake`'s dependency targets and version checks.
- MAPL phases now align more closely with NUOPC phases.
- The Discover NAG CI workflow skips fork PRs unless a maintainer reruns it;
  PR jobs now use read-only tokens without deployment or build-cache credentials.

### Fixed

- Corrected `standard_name` matching in vertical-grid and expression
  transforms: coordinate fields and expression operands no longer inherit
  unrelated names from the field being transformed. Unnamed vector components
  and unnamed Exports no longer cause spurious mismatches or crashes.
- Fixed a Flang 23 crash in HConfig and file-metadata geometry creation on
  hardened macOS systems ([LLVM #223705](https://github.com/llvm/llvm-project/issues/223705)).
- Generic components created with direct SetServices now inherit their parent's
  VM, preventing communicator-context exhaustion.
- Fixed cubed-sphere coordinate endpoints with NAG, and NetCDF quantization
  and Zstandard detection with Spack.
- Fixed ExtDataFileReader's dangling pointer, History's R8 exports and
  cubed-sphere corner longitudes, and missing restart coordinate data.
- Fixed FieldBundleRead when input and output grid classes differ.
- Documentation deployments for v2 and MAPL3 no longer overwrite each other.
- Expression fields with referenced variables can infer their vertical stagger
  and grid without `vertical_dim_spec`; conflicting operand staggers now fail
  explicitly. Constant expressions still require `vertical_dim_spec`.
- LatLon grids with zero-extent decomposition bins no longer crash on PETs
  without a DE.
- Field-wide `standard_name` and per-alias `long_name` now survive connections,
  re-exports, expression fields, bundles, History copies, and state-to-bundle
  conversion. `restart_mode` is also preserved when fields are re-aliased.

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
