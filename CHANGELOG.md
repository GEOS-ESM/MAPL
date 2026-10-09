# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- mlc-disable -->
## [Unreleased]
<!-- mlc-enable -->

### Added

- Enforced the `standard_name` convention between a connected Import and
  Export (GEOS-ESM/MAPL#5413). A new `StandardNameAspect`
  (`superstructure/generic/specs/StandardNameAspect.F90`), modeled directly
  on `UnitsAspect`, checks agreement via the standard `AspectMap`
  connection-resolution `matches()` machinery: equal values connect
  silently; an Import may declare a wildcard/unset `standard_name` to
  accept any Export; an Export with no declared `standard_name` warns but
  still connects (never a fatal error); a genuine disagreement is a fatal
  error in the new `STRICT` `ValidationMode` and a warning (Export's value
  wins) in the default `PERMISSIVE` mode. `standard_name` is now a single,
  unaliased, field-wide value (like `units`) rather than the per-connection-
  endpoint value it briefly became; `long_name` is unaffected and keeps its
  existing per-alias/inheritance behavior. For a `Vector` item, whose
  `standard_name` is a compound `"(name1,name2)"` encoding of two
  independent component names, agreement is checked per component.
  `FieldDictionary`-driven `long_name`/`units` defaulting from a declared
  `standard_name` (`VariableSpec`) is now unconditional - the
  `use_field_dictionary` opt-in flag has been removed (**BREAKING**: any
  code passing `use_field_dictionary=` to `make_VariableSpec`/
  `MAPL_GridCompAddSpec` will need to drop the argument); a `standard_name`
  absent from the dictionary is likewise a fatal error in strict mode. The
  previously-unused `ValidationMode`/`FieldDictionaryConfig`
  (`infrastructure/field_dictionary/`) are now wired into
  `MaplFramework%initialize_field_dictionary`, which accepts either the
  existing bare-string `field_dictionary: <path>` cap.yaml key or a new
  mapping form, `field_dictionary: {path: ..., validation_mode: strict|permissive}`.
  Default mode is permissive, so existing configurations are unaffected
  until a run opts into strict mode.

### Changed

- Activated `superstructure/generic/tests/field_dictionary_test.yaml` (a
  checked-in fixture that had never actually been loaded by anything) as
  the default-path `FieldDictionary` for all six of
  `superstructure/generic/tests/`'s pFUnit binaries
  (`MAPL.generic.scenarios/.transforms/.vertical/.aspects/.components/.core`),
  so MAPL's own test suite - especially the YAML-driven scenario tests -
  finally exercises `FieldDictionary`-driven `long_name`/`units` defaulting
  and `standard_name` enforcement against real data. Converged every one of
  `MAPL.generic.scenarios`'s scenario directories onto the dictionary and
  switched that binary from the default permissive `ValidationMode` to
  `STRICT` (via a new `Initialize_strict()` entry point alongside the
  existing `Initialize()` in `mapl_pFUnit_Initialize_mod`,
  `pfunit/MAPL_Initialize.F90` - no other pFUnit binary is affected). This
  is the first MAPL test coverage that genuinely exercises `STRICT`
  enforcement end-to-end against realistic scenario fixtures (previously
  only isolated `Test_Aspects.pf`/`Test_FieldDictionary.pf` unit tests
  exercised `STRICT` in isolation).

  Reaching full coverage required resolving several classes of pre-existing
  issue that permissive mode's warn-and-continue behavior had always
  masked, none of them dictionary gaps as such:
  - Genuinely inconsistent naming: human-sentence `standard_name`s in
    `statistics`/`statistics_real` converged to proper CF `snake_case`
    identifiers (`surface_temperature`, `surface_air_pressure`,
    `specific_humidity`, `air_pressure_at_sea_level`; also fixed a
    stray-whitespace bug in a vector field's compound-encoded name), and
    several distinctly-suffixed synthetic names in `vertical_regridding_2`/
    `_3` (`air_pressure_ple_edge`/`air_pressure_c_center`/
    `air_pressure_dyn_center`, `temperature_dyn_center`/
    `temperature_phys_center`) consolidated onto plain `air_pressure`/
    `air_temperature`.
  - Incidental Import/Export `standard_name` placeholder mismatches with no
    dependent test behavior - each side had simply been given its own
    independent auto-generated name rather than the value it actually
    receives over the connection - fixed by making the Import side match
    its connected Export across: the `I_A1`/`E_A1`/`Z_A1` family
    (`scenario_1`, `scenario_2`, `propagate_geom`); the `A1`/`A3`/`B2`
    family shared by `3d_specs`, `precision_extension`,
    `precision_extension_3d`, and `ungridded_dims`; the `E_A`/`I_B` pair
    shared by `vertical_regridding` and all three `vertical_alignment_*`
    scenarios; `export_dependency`'s `E1`/`I1` pair; and
    `history_wildcard`'s undocumented `huh1` override (removed, now
    inherits from its connection instead).
  - Two scenarios (`names_1`, `expression`) had deliberately declared a
    disagreeing `standard_name` specifically to exercise permissive-mode's
    "warn but connect anyway, Export's value wins" behavior. Per
    maintainer direction that behavior was not worth preserving as a
    dedicated scenario, so both were changed to agree instead (the
    *observed* `standard_name` was already the Export's value either way,
    so no `expectations.yaml` changes were needed) and their explanatory
    comments updated accordingly.
  - `superstructure/generic/tests/gridcomps/ProtoStatGridComp.F90` hardcoded
    `standard_name='<unknown>'` (`StandardNameAspect`'s own "not set"
    sentinel) as if it were a real declared name - fixed by omitting the
    argument.
  - Two real framework bugs, both a "goal spec built by copying an
    unrelated field's `AspectMap` wholesale, overriding some aspects but
    not `StandardNameAspect`" - see `### Fixed`
    (`VerticalGridAspect%make_transform`, `ExpressionClassAspect%make_transform`).
  - `field_dictionary_test.yaml` gained dictionary entries for every
    remaining `standard_name` used anywhere in `superstructure/generic/tests/scenarios/`.

### Fixed

- Fixed a spurious `standard_name` convention violation raised while
  resolving a vertical regrid between two mismatched vertical grids
  (`VerticalGridAspect%make_transform`, GEOS-ESM/MAPL#5413). Building the
  criteria for looking up/extending the vertical coordinate field (e.g.
  `PLE`/`ZLE`) copied the *payload* field's entire `AspectMap` wholesale and
  then overrode only `UnitsAspect` with the coordinate field's own units;
  `StandardNameAspect` was not similarly overridden, so the coordinate
  field's lookup incorrectly inherited the payload's `standard_name` as a
  match requirement. Since the coordinate field is a different physical
  quantity than the payload (e.g. payload `air_temperature` vs. coordinate
  `air_pressure`), this failed in `STRICT` `ValidationMode` (and warned
  spuriously in the default permissive mode) for essentially every
  cross-vertical-grid connection with a real, distinct `standard_name` on
  each side. The coordinate-field aspect map now also overrides
  `StandardNameAspect` (to unchecked), matching the existing treatment of
  `UnitsAspect`.
- Fixed the same class of bug as above in a second location:
  `ExpressionClassAspect%make_transform` builds a `goal_spec` to look
  up/extend each of an `expression:` field's referenced variables (e.g. `A`,
  `B` in `E_sum: {expression: A + B}`) by copying the expression's own
  `AspectMap` wholesale and overriding only `CLASS_ASPECT_ID`; a referenced
  variable's own `standard_name` (e.g. `A`'s) was therefore spuriously
  compared against the expression's unrelated declared `standard_name`
  (e.g. `E_sum`'s), which is fatal under `STRICT` (and warns spuriously
  under permissive) essentially any time an expression declares its own
  `standard_name` and differs from any of its operands - the normal case,
  since an expression's result is a different quantity than any single
  operand. The `goal_spec`'s `StandardNameAspect` is now also overridden (to
  unchecked), matching the existing `CLASS_ASPECT_ID` treatment.
- Fixed two `standard_name`-enforcement bugs found while wiring MAPL's own
  test suite up to a real `FieldDictionary` (see `### Changed` above).
  (1) `VariableSpec`'s `MAPL_STATEITEM_VECTOR` case defaulted a Vector's two
  split component names to the literal string `'unknown'` when no
  `standard_name` was declared at all, instead of leaving them unset;
  `StandardNameAspect` only treats its own `'<unknown>'` sentinel as a
  wildcard, so the literal `'unknown'` was treated as a real, specified
  name and produced spurious "standard_name convention violation" warnings
  for every such Vector (e.g. the statistics gridcomp's internal vector
  accumulators) connecting to any vector with a real name. (2)
  `StandardNameAspect%connect_to_export` unconditionally copied the
  export's (possibly unallocated) `standard_name` onto the import side,
  which both crashed with an "ALLOCATABLE ... is not currently allocated"
  runtime error once (1) was fixed and stopped masking it, and would have
  incorrectly discarded the import's own declared name whenever the export
  side legitimately declared none.
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
- Fixed `expression` state items (`expression: (A + B)/C`) requiring an explicit
  `vertical_dim_spec` even when it references at least one variable. The
  omitted-key case previously left `VerticalGridAspect`'s vertical stagger at an
  invalid sentinel while `VariableSpec::make_VerticalGridAspect` could still mark
  the aspect resolved purely because a component-level vertical grid resource was
  available, failing with "BasicVerticalGrid should have been connected to a
  different subclass before this is called." An initial fix that forced a
  genuinely mirrored state in `create()` (relying on the same
  mirror-from-connection/`ExtendTransform` mechanism `GeomAspect` already uses)
  did not hold up: a component-level vertical grid resource
  (`MAPL_GridCompSetVerticalGrid`) unconditionally overwrites *every* item's
  `VerticalGridAspect` status to resolved once declared, regardless of what
  `create()` set it to, silently undoing the forced mirror before the item is
  ever connected. `ExpressionClassAspect` now instead directly resolves its own
  vertical stagger (and vertical grid, if applicable) from whichever of its
  referenced variables are already resolved at the time - first on a best-effort
  basis in `create()`, and again (more reliably, since by then referenced
  variables and any component-level override have had a chance to settle) at
  actual connection time in `make_transform`. The same logic also validates
  consistency: if the expression's own vertical stagger is (or resolves to) a
  concrete value and any of its resolved referenced variables disagree, that is
  a hard error naming the conflicting items, rather than allowing arithmetic on
  dimensionally incompatible operands. A referenced variable that is not yet
  resolved - or not yet even registered - is skipped rather than treated as an
  error. Expressions with no referenced variables (e.g. a literal/constant
  expression) are unaffected and still require an explicit `vertical_dim_spec`.
- Fixed omission of setting FieldBundle allocation status in create() for ServiceClassAspect
=======
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
