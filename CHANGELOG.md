# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- mlc-disable -->
## [Unreleased]
<!-- mlc-enable -->

### Changed

- Renamed the load-balance public API and exported the direction constants through the
  `mp_utils` umbrella, which previously omitted them and forced clients to `use
  mapl_LoadBalance_mod` directly: `MAPL_BalanceCreate`/`MAPL_BalanceGet`/`MAPL_BalanceWork`/
  `MAPL_BalanceDestroy` are now `MAPL_LoadBalancerCreate`/`MAPL_LoadBalancerGet`/
  `MAPL_LoadBalancerRun`/`MAPL_LoadBalancerDestroy`, and `MAPL_Distribute`/`MAPL_Retrieve` are
  now `MAPL_LOADBALANCER_DISTRIBUTE`/`MAPL_LOADBALANCER_RETRIEVE`. The old names are gone.
  The module and its file were renamed to match: `mapl_LoadBalance_mod` in
  `mp_utils/MAPL_LoadBalance.F90` is now `mapl_LoadBalancer_mod` in
  `mp_utils/MAPL_LoadBalancer.F90`

### Fixed
- Fixed bug prevents regridding methods that require dynamic masking from executing the dynamic mask in ExtData.
  `EsmfRegridderParam%make_info` serialized only the routehandle param, so the dynamic mask
  was dropped when ExtData passed the param to the field bundle as `ESMF_Info`; the mask
  (type, `handleAllElements`, kind, src/dst values) is now round-tripped. `RoutehandleParam`
  serialization also now supports `CONSERVE_2ND`, `PATCH` and `NEAREST_STOD`, which
  previously failed. `CONSERVE_2ND` now maps to `ESMF_REGRIDMETHOD_CONSERVE_2ND` instead of
  plain `ESMF_REGRIDMETHOD_CONSERVE`, so ExtData outputs for masked regrids and `CONSERVE_2ND`
  may change. Added `Test_EsmfRegridderParam.pf`

- Fixed a crash in `RestartHandler` when a state's restart-eligible bundle ends up empty
  after filtering (e.g. a component whose exports are all unallocated because nothing is
  connected downstream and `activate_all_exports` is off). The state's non-zero item count
  passed the existing guard, but `MAPL_FieldBundleGetGeom` on the empty bundle returned an
  uninitialized geom and `ESMF_InfoGetFromHost` then failed in `GeomGetId`. `write_bundle_`
  and `read_bundle_` now return early when the bundle holds no fields, resolving a
  pre-existing TODO
- Fixed an ifort internal compiler error in `GraphBuilder.F90` by replacing
  self-referencing array-constructor growth (`x = [x, y]`) of an allocatable
  `QualifiedExportEntry` array with an explicit allocate/copy/`move_alloc`
  temporary.
- Avoided passing the HConfig geometry-factory predicate as an internal
  procedure callback, fixing a Flang 23 crash on hardened macOS systems; see
  [LLVM #223705](https://github.com/llvm/llvm-project/issues/223705).
- Avoided passing the file-metadata geometry-factory predicate as an internal
  procedure callback, preventing the same Flang 23 crash in metadata-based
  geometry creation; see [LLVM #223705](https://github.com/llvm/llvm-project/issues/223705).
- Extended History and StatisticsGridComp so one can take average, min, max, accumulation, and variance for vectors
- Fixed Generic components created through direct SetServices to inherit their parent VM, preventing communicator-context exhaustion during repeated setup.
- Fixed corrupted cubed-sphere coordinate endpoints in NAG-generated output
- Fixed detection of NetCDF quantization and Zstandard support when using Spack
- Fixed documentation workflows so manual runs publish only from trusted branches and v2 and MAPL3 documentation deployments preserve each other's output
- Removed deployment and build-cache credentials from pull request jobs and restricted PR workflow tokens to read-only access
- Dangling pointer in ExtDataFileReader due to a missing target attribute on ExtDataReader
- Fixed omission of setting FieldBundle allocation status in create() for ServiceClassAspect
- Fixed `standard_name`/`long_name` Field metadata being collapsed to a single,
  field-wide value across a connection. Because a connected Import's `ESMF_Field`
  is an `ESMF_NamedAlias` of its Export's field, and aliases share one underlying
  `ESMF_Info` host, only the Export side's declared `standard_name`/`long_name`
  ever survived; an Import (or a re-export several hops away) that declared its
  own value had it silently discarded. Each connection endpoint - Export, Import,
  and any intermediate transform/coupler hop - now persists its own
  `standard_name`/`long_name` in a per-`NamedAlias`-id namespace of the shared
  `ESMF_Info` (the same pattern already used for `restart_mode`). An endpoint
  that declares neither now inherits its predecessor's value (one-directional,
  downstream only) instead of resolving to a hardcoded `'unknown'`.
  `MAPL_FieldGet(field, standard_name=, long_name=)` resolves the value for the
  specific alias represented by the `field` handle passed in; no signature
  change was needed since callers already hold the correct alias from a specific
  `ESMF_State`.
- Fixed two further gaps in the `standard_name`/`long_name` propagation above.
  (1) An `expression:`-derived export (e.g. `E_sum: {expression: A+B,
  standard_name: "foo", long_name: "bar"}`) always lost its declared name: its
  `ExpressionClassAspect` never carried `standard_name`/`long_name` at all, and
  because `ExpressionClassAspect` never "matches" a `FieldClassAspect`, any
  connection into it - including an implicit same-name match (the mechanism
  `MAPL_GridCompConnectAll`/History's `var_list: {source: ...}` use) - always
  went through `StateItemSpec%make_extension`'s aspect substitution, which
  unconditionally replaced it with the consumer's own nameless goal aspect.
  `ExpressionClassAspect` now carries its declared name, and a new
  `inherit_descriptive_metadata` hook (default no-op on `StateItemAspect`,
  overridden on `FieldClassAspect`) lets the superseded aspect hand its name to
  its replacement when the replacement doesn't already have its own. (2) Even
  for plain Fields, `MAPL_FieldGet`'s alias-scoped read had no symmetric
  counterpart in `MAPL_FieldSet`, which still wrote to a single unaliased slot;
  a consumer that duplicates a field via `ESMF_FieldCreate` (as History's
  `create_alias_field` does, rather than `ESMF_NamedAlias`) gets its own,
  different alias id, so the name written under the original field's id was
  never found. `MAPL_FieldSet` now writes `standard_name`/`long_name` through
  the same alias-scoped path `MAPL_FieldGet` reads from, and
  `create_alias_field` explicitly re-copies the names across its field
  duplication.
- Fixed `standard_name`/`long_name` always resolving to `'unknown'` for
  fields placed into a `FieldBundle` (`class: service`, `class: vector`,
  `class: bracket`, `class: vector_bracket`). `FieldClassAspect%add_to_bundle`
  adds a field via a plain `ESMF_FieldBundleAdd`, not `ESMF_NamedAlias`, so it
  never went through the alias-scoped write path added for #5399; any field
  later retrieved from such a bundle resolved `ESMF_NamedAliasGet` to id=0,
  an empty slot. `standard_name`/`long_name` are now attached via
  `FieldClassAspect%update_payload` at field-creation time - the same point
  every other characteristic aspect (units, typekind, geom, ...) attaches its
  metadata - so the unaliased field's own id=0 slot is populated before it is
  ever placed in a bundle. `VectorClassAspect`, `BracketClassAspect`, and
  `VectorBracketClassAspect` each drove their per-component field through a
  local `update_payload` helper that forwarded to sibling aspects but never
  invoked the component's own `update_payload`; fixed to call it.
- Closed a remaining gap in the `standard_name`/`long_name` alias-scoping
  above: the base (unnamespaced) `FieldInfoSetInternal`/`FieldInfoGetInternal`
  overload still accepted `standard_name`/`long_name` as plain, non-aliased
  keys - a leftover from before the per-`NamedAlias`-id scheme, and a footgun
  for any future caller. `MAPL_FieldCreate`/`FieldEmptyComplete`
  (`field_empty_complete` in `FieldCreate.F90`) used exactly that path, so a
  field built via `MAPL_FieldCreate(..., standard_name=, long_name=)` had its
  name written to a key `MAPL_FieldGet` - which always reads through the
  alias-scoped overload - could never find, resolving to `'unknown'`
  regardless of any `FieldClassAspect` involvement. `standard_name`/
  `long_name` are now handled entirely inside `field_info_set_internal`/
  `field_info_get_internal` themselves (like every other item there - units,
  typekind, ...), with a `named_alias_id` argument that scopes just those two
  keys to their own per-alias namespace; `MAPL_FieldSet`/`MAPL_FieldGet`/
  `FieldClassAspect%add_to_state` each resolve their own alias id once and
  pass it through in the same call as everything else. `FieldBundleInfo`'s
  unrelated bundle-wide "field prototype" template (describing the bundle as
  a whole, not any specific Field's own identity) passes a fixed `id=0`.
- Fixed `superstructure/state/StateGet.F90`'s `state_get_bundle` (used to
  serialize a `State` into a synthetic `FieldBundle`, e.g. for I/O) silently
  dropping `standard_name`/`long_name`/`restart_mode` when re-aliasing a
  `FieldBundle` member field: it called bare `ESMF_NamedAlias`, which has no
  knowledge of MAPL's per-`NamedAlias`-id metadata, so the field's brand new
  alias id resolved to defaults regardless of what the source field carried.
  Added `MAPL_NamedAlias` (`infrastructure/field/FieldNamedAlias.F90`) as the
  one place that wraps `ESMF_NamedAlias` for `ESMF_Field` and additionally
  copies `standard_name`/`long_name`/`restart_mode` from the source field's
  own resolved alias id onto the new alias's own (different) id; every
  `ESMF_NamedAlias(field, ...)` call site in MAPL - `state_get_bundle` and
  each `ClassAspect%add_to_state`/`connect_to_import` that creates a Field
  alias - now goes through it. `MAPL_NamedAlias` also accepts
  `ESMF_FieldBundle`/`ESMF_State` (used by the bundle/vector/bracket/service/
  state `ClassAspect`s' own `add_to_state`), as a plain pass-through: MAPL
  does not attach per-alias-id metadata to bundles (their `standard_name`/
  `long_name` "field prototype" template lives at a single fixed id, shared
  by every alias of the same bundle) or to nested states today, so there is
  nothing to propagate for those two - they are included so every
  `ESMF_NamedAlias` call in MAPL shares one consistent, safe name.

- Fixed `LatLonDecomposition`'s topology constructor to pack out zero-extent bins returned
  by `mapl_GetPartition()` when a LatLon grid is too coarse to be decomposed onto the
  requested `nx`/`ny` topology given ESMF's `min_extent=2` constraint, and updated
  `LatLonGeomFactory`'s `fill_coordinates` to use `grid_has_de`/`grid_get_interior` so PETs
  that legitimately own no DE in that case are skipped instead of crashing; added
  `Test_LatLonZeroDE.pf` and a `LatLonDecomposition` unit test covering this case
- Fixed several crashes affecting ranks with no local decomposition element (DE)
  for a field's grid (the "coarse grid, extra PET" scenario), continuing the
  `LatLonDecomposition`/`LatLonGeomFactory` fix above: `pFIOServerBounds` gained
  a `has_de` argument so a no-DE rank still reports the true, shared
  `global_start`/`global_count` (required since pfio's server sizes its shared
  read/write buffer from one arbitrary representative message per collective
  `request_id`), zeroing only its own local `file_shape`; `ExtDataFileReader`
  and `GridPFIO` (History/Restart read+write, plus coordinate writing) now call
  their collective pfio requests unconditionally on every rank instead of
  skipping no-DE ranks, since `ClientThread`'s `request_id` counter is local
  and unsynchronized and skipping it on some ranks desyncs which request a
  given rank's data belongs to; and `FieldGetCptr`/`assign_fptr`'s common
  `get_cptr` now returns a zero-size result instead of crashing for a no-DE
  field, transparently fixing `FieldCopy`, `FieldBLAS`, `FieldUtilities`,
- Added regression test coverage for the no-local-DE fixes above: new
  `Test_FieldPointerUtilities.pf` and `Test_pFIOServerBounds.pf` exercise
  `FieldGetCptr`/`assign_fptr`/`FieldCopy` and `pFIOServerBounds` directly on
  no-DE ranks/inputs, and existing `Test_FieldBLAS.pf`/`Test_FieldArithmetic.pf`/
  `Test_FieldCondensedArray_private.pf` gained no-DE variants (also adding the
  first coverage at all for `FieldSet`/`FieldIsConstant`).

- Ensured all libraries created by MAPL are built as shared libraries by
  adding `TYPE SHARED` to `MAPL.raster_to_mesh` in
  `infrastructure/geom/Mesh/raster_to_mesh/CMakeLists.txt` and `SHARED` to
  `MAPL.Apps.tests.acg3` in `apps/tests/acg3/CMakeLists.txt`, and replaced
  the stale `MAPL.shared` dependency with `MAPL.utils` and `MAPL.enums`.

### Added

- Added CI verification step to `.github/actions/ci-build-and-test-mapl/action.yml`
  that checks for and fails if any static libraries (`*.a`) are installed in
  standalone MAPL builds.
- Added ability to output on a set of fixed pressure or height levels in History3G

### Changed

- Update `components.yaml`
  - ESMA_cmake v4.51.0
    - Make ESMA_cmake reentrant
    - Fix issue with quad precision detection test
    - Fixes for Flang and NVIDIA compilers

### Removed

- Removed obsolete `BUILD_SHARED_MAPL` CMake option and unused
  `MAPL_LIBRARY_TYPE` variable from root `CMakeLists.txt` and `INSTALL.md`, as
  MAPL3 exclusively builds shared libraries.

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
