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
- Fixed `LatLonDecomposition`'s topology constructor to pack out zero-extent bins returned
  by `mapl_GetPartition()` when a LatLon grid is too coarse to be decomposed onto the
  requested `nx`/`ny` topology given ESMF's `min_extent=2` constraint, and updated
  `LatLonGeomFactory`'s `fill_coordinates` to use `grid_has_de`/`grid_get_interior` so PETs
  that legitimately own no DE in that case are skipped instead of crashing; added
  `Test_LatLonZeroDE.pf` and a `LatLonDecomposition` unit test covering this case
=======
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

### Changed

- Changed ESMF\_RouteHandle parameters so LINETYPE_GREAT_CIRCLE is default for all methods
- Reworked MAPL phases to better align with NUOPC phases.
- Simplified the `CMakeLists.txt` ESMF handling: `ESMA_cmake` now creates the NetCDF/HDF5/ESMF/MPI targets and enforces a minimum ESMF version for both Baselibs and Spack builds automatically, so the manual `if (NOT Baselibs_FOUND) ... else () ... endif ()` block is no longer needed. MAPL3's stricter ESMF >= 9.0.0 requirement is now expressed by setting `ESMA_ESMF_MIN_VERSION` before `include(esma)`.
  - Update `components.yaml`
    - ESMA_env v5.26.0
    - ESMA_cmake v4.48.0
    - ecbuild geos/v3.15.2
- Split cap.yaml into mapl.yaml, cap_driver.yaml, and cap_gridcomp.yaml (see issue #5355)
- Renamed `model_petcount`/`has_model_petcount` to `app_petcount`/`has_app_petcount`
  throughout the codebase, config files, and documentation
- Renamed `mapl/Cap.F90` to `mapl/CapDriver.F90` and its module `mapl_Cap_mod` to `mapl_CapDriver_mod`
- `MAPL_Initialize` gained a new optional `app_config` (intent(out)) argument that resolves
  and returns the `app.config`-derived hconfig (i.e. the `cap_driver.yaml` contents); `mapl/GEOS.F90`
  passes this through to `MAPL_CapCreate`/`MAPL_CapRun` via a new `config` argument on
  both, so `cap_driver.yaml` is parsed from disk only once instead of independently in each procedure
- Moved `mapl/cap_gridcomp.yaml` to `gridcomps/cap/cap_gridcomp.yaml`, replacing the stale
  `gridcomps/cap/CapGridComp.yaml` (which used outdated `root`/`extdata`/`history` keys no
  longer read by `CapGridComp.F90`, which reads `root_name`/`extdata_name`/`history_name`)
- Moved the `gridcomp_config` key out of `mapl.yaml`'s `app:` section into `cap_driver.yaml`
  as `cap_gridcomp_config`, so `mapl.yaml`'s `app:` section only points at `cap_driver.yaml`
  (via `config`), and `cap_driver.yaml` in turn points at `cap_gridcomp.yaml`
- For vector items in ExtData change variables separted by `;` to a sequence of variables like History
- Moved DSO-backed child `setServices` ownership into child configurations and added support for raw `ESMF_GridCompCreate` followed by `ESMF_GridCompSetServices` startup.
- Refactored `UserSetServices.F90` to remove the `user_setservices` interface, rename `AbstractUserSetServices` to `UserSetServices`, and giving `ProcSetServices` and `DSOSetServices` their own constructors
- `Regrid_Util.x` now uses the fargparse library for command line argument parsing instead
  of raw Fortran intrinsics. Multi-character options that previously used a single-dash prefix
  (e.g. `-ogrid`, `-nx`, `-ny`, `-method`, `-tp_in`, `-tp_out`, `-lon_range`, `-lat_range`,
  `-stretch_factor`, `-deflate`, `-shave`, `-quantize_algorithm`, `-quantize_level`,
  `-zstandard_level`, `-file_weights`, `-vars`, `-t`) now require a double-dash prefix
  (e.g. `--ogrid`, `--nx`). The short forms `-i` and `-o` are preserved. The `--help` flag
  is now handled automatically by fargparse and prints a formatted usage summary.
- Removed unused fields and methods from InnerMetaComponent

### Added

- Added optional coordinate-comparison tolerance for LatLon grid equality (`GEOS-ESM/MAPL#5385`), so that two file-based LatLon grids whose coordinates differ only by numerical noise can be treated as the same grid, letting `GeomManager` reuse geoms/RouteHandles instead of minting new ones on every file swap. `coordinate_tolerance` is a dimensionless fraction of a grid's own coordinate spacing (DX) - e.g. `0.01` means "within 1% of the minimum spacing between adjacent grid points" - not an absolute coordinate difference. Comparison is directional: when a new (not yet cached) grid is looked up against an already-registered one, only the new grid's own declared tolerance and its own spacing are consulted; the already-registered grid's tolerance never matters. The tolerance is sourced from a generic `coordinate_tolerance` attribute on `FileMetadata` (read via the existing generic attribute API; no new `pfio`/`FileMetadata` methods added); the geom layer itself defaults to `0` (strict/bitwise) when the attribute is absent, staying neutral for any client. `ExtData` is the first client to set this attribute: file collections may set an optional `coordinate_tolerance` in their YAML config, which `PrimaryExport` stamps onto each file's `FileMetadata` before requesting a geom for it. Unlike the geom layer's own neutral default, `ExtData` defaults `coordinate_tolerance` to a nonzero value (`0.1`, i.e. 10% of DX) when a collection's config omits it, restoring MAPL2's historical default-tolerant grid-reuse behavior for existing users; a collection can set `coordinate_tolerance: 0` explicitly to opt into strict comparison.
- Added `StateGetPointer` overloads for retrieving paired (u, v) field pointers from a vector-type ESMF FieldBundle stored in a state
- Added `extdata_dryrun_check.py`, a Python utility that predicts which input
  files an ExtData component will need for a given run without executing the
  model. Supports three tiers: template enumeration (Tier 1), filesystem
  existence check (Tier 2, `--check`), and time-axis narrowing via `netCDF4`
  (Tier 3, `--narrow`). A `--verify_files_read` flag compares predictions
  against the runtime `log_files_read` output for use in CTest. Wired into
  the MAPL3G component test framework for case02, case11, and case23.
- Added `latlon_to_face.py`, a Python utility that converts a cubed-sphere
  NetCDF file from tiled lat/lon layout (`lat = 6 * lon`) to the face layout
  (`nf / Ydim / Xdim`) required by MAPL/GEOS face-format readers.
- Added new GEOShs CI test
- Added capability for doing in-memory checkpoint/restart.  Testing remains fairly basic, so further work is likely needed when we port replay to MAPL3.
- Added `log_files_read` option to ExtData2G to easily log all files read during a run
- Added `MAPL_FieldApplyUserRoutine`/`MAPL_FieldBundleApplyUserRoutine` to apply a user routine to each slice of a field (or every field in a bundle) with ungridded/vertical dimensions, plus `MAPL_FieldGetPointerToSlice` (overloaded for R4 and R8) for typed per-slice access. Slices are 2D by default, or 3D when the field has exactly three non-ungridded (grid + vertical) dimensions (for example a 4D field whose fourth dimension is the ungridded dimension). The slice-routine interface is unlimited-polymorphic and assumed-rank, so a single user routine handles R4/R8 and 2D/3D slices via `select rank`/`select type`
- `update_restart` in `CapDriver.F90` now supports a `skip_restart_write` boolean flag in the
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
- Improved error handling for issues writing netcdf files


### Fixed

- Fixed restart handler so checkpoints have data in the coordinate variables.
- Fixed the unreliable feedback from Python bridge failures
- Improved `SimpleConnection` assertion messages for unknown virtual connection points
- Fixed bug in FieldBundleRead when file grid and output bundle grid are different grid classes
- Buggy logic in server initialization (#5214)
- Missing call to initialize error handling in MPI context
- Fixed bug that prevented R8 exports from being written in R8 in History
- check before assign tilelons and tilelats in MAPL_Locstreamget
- Fixed bug causing 'already allocated' error when setting corner longitudes in cubed-sphere History files

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
