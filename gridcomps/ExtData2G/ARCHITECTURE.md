# gridcomps/ExtData2G: architecture

**Audience:** coding agents and developers working on the second-generation (YAML) ExtData gridded component in MAPL.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `gridcomps/ExtData2G/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

ExtData2G is the YAML-driven ExtData: the MAPL gridded component that fills
model imports from files on disk. It is the component GEOSgcm runs (see
"Selection"). The legacy resource-file component is in
[`../ExtData/ARCHITECTURE.md`](../ExtData/ARCHITECTURE.md).

## Existing documents
- `README.md`: short overview and links to the MAPL wiki user guide and a
  sample YAML.
- `ExtDataGridComp.md`: user reference for the YAML file (sections
  Collections, Samplings, Exports, Derived; mask and arithmetic functions;
  special cases). This document does not restate it; it adds the code
  structure and run sequence.
- `../../base/ArthParser.md`: grammar of derived-field arithmetic expressions.

## Selection and build
- CMake option `USE_EXTDATA2G` (`<MAPL>/CMakeLists.txt`, default `ON`) sets
  `EXTDATA2G_TARGET` to `MAPL.ExtData2G`; `gridcomps/CMakeLists.txt` adds this
  directory only `if(USE_EXTDATA2G)`. `gridcomps/ExtData/` is added
  unconditionally.
- Run-time key `USE_EXTDATA2G:` read by `MAPL_CapGridComp` (default
  `.false.` in the code). GEOSgcm sets `USE_EXTDATA2G: .TRUE.` in
  `src/Applications/@GEOSgcm_App/CAP.rc.tmpl`. When true Cap adds child
  `EXTDATA` with `ExtData2G_SetServices` (module `MAPL_ExtDataGridComp2G`,
  file `ExtDataGridCompNG.F90`); when true but not built, Cap fails with
  "ExtData2G requested but not built". Otherwise it adds the legacy
  `ExtData1G_SetServices`.
- Library target `MAPL.ExtData2G`; depends on `MAPL.shared`, `MAPL.base`,
  `MAPL.generic`, `MAPL.griddedio`, `udunits2f`, `MAPL.vertical`,
  `MAPL.state_utils`. `tests/` (pFUnit) is added `EXCLUDE_FROM_ALL`.
  `ExtDataGridCompNG.F90` is compiled at the lower optimisation level
  (`FOPT1`) with Intel Release builds (comment in the CMake file: 401 s at
  O3 versus 8 s at O1).

## How ExtData's export list is built
ExtData2G has no static export spec. Cap constructs it
(`MAPL_CapGridComp`, `initialize_extdata`):
1. Cap reads the lists `CAP_IMPORTS`, `CAP_EXPORTS`, `EXTDATA_IMPORTS` from
   its config (`get_vec_from_config`).
2. Every Field in the root component's import state gets attribute
   `RESTART = MAPL_RestartSkip` (`MAPL_AddAttributeToFields` on the root;
   bundles do not). Then, for every item: if its name is in `CAP_IMPORTS` it
   goes to Cap's own import state; otherwise the field (or bundle) is added
   to ExtData's **export** state (`cap%child_exports(cap%extdata_id)`).
3. Fields named in `EXTDATA_IMPORTS` (`<field>,<component>` pairs) are taken
   from the root's export state and added to ExtData's import state (inputs
   to derived expressions or vertical regridding).
4. Cap then calls `ESMF_GridCompInitialize` on ExtData with those states.
Net effect: ExtData fills every root import that no component provides by
connection. *Unverified:* where `CAP_IMPORTS`/`EXTDATA_IMPORTS` are defined in
GEOSgcm (no match in `src/Applications`); the lists are read from Cap's
config.

## Configuration
Cap loads `EXTDATA_CF` (default `ExtData.rc`) into ExtData's config and adds
`NX`, `NY`, `CF_EXTDATA`, `EXPID`, `RUN_DT`. ExtData2G reads from it one key:
`EXTDATA_YAML_FILE:` (default `extdata.yaml`). The rest is YAML.

Top-level YAML keys (`ExtDataConfig.F90`, `ExtDataGridCompNG.F90`):

| Key | Meaning | Parsed by |
|---|---|---|
| `subconfigs` | list of further YAML files, read recursively and merged | `ExtDataConfig` |
| `Collections` | name -> `template`, optional `freq`, `ref_time`, `valid_range` | `ExtDataFileStream` |
| `Samplings` | name -> `extrapolation` (`none`, `persist_closest`, `clim`), `time_interpolation`, `exact`, `update_frequency`, `update_reference_time`, `update_offset`, `source_time` | `ExtDataSample` |
| `Exports` | import name -> rule(s): `collection`, `variable`, `sample`, `regrid`, `linear_transformation`, `starting`, `fail_on_missing_file`, `enable_vertical_regrid`; a list gives multiple time-ranged rules | `ExtDataRule` |
| `Derived` | import name -> `function`, `sample` | `ExtDataDerived` |
| `USE_EXTDATA` | logical; false makes ExtData inactive | `get_global_options` |
| `file_weights` | logical; passed to prefetch | `get_global_options` |
| `debug`, `log_files_read` | debug level; path of a log of files read | `ExtDataConfig` |

Duplicate keys across merged files are an error (`ExtDataGridComp.md`).
Special values: collection `/dev/null` makes the export a constant field
(`isConst`), filled once and skipped afterwards. A vector pair is one rule
with the two names separated by `;` (components EW and NS).

Parsing chain: `ExtDataConfig` (merge files into maps of collections,
samplings, rules, derived) -> `ExtDataOldTypesCreator` (look up by name and
fill the older `PrimaryExport` / `DerivedExport` types in `ExtDataTypeDef`).
Regrid strings map to integer methods (`BILINEAR` default, `CONSERVE`,
`VOTE`, `FRACTION;value`); `FRACTION;` stores a fraction value
(`ExtDataOldTypesCreator`, `regrid_method_string_to_int`).

## Modules
| Role | Files |
|---|---|
| Component | `ExtDataGridCompNG.F90` (SetServices, `Initialize_`, `Run_`, `Finalize_`, vertical interpolation, aux fields PS and Q) |
| Config | `ExtDataConfig`, `ExtDataFileStream`, `ExtDataRule`, `ExtDataSample`, `ExtDataDerived`, `ExtDataOldTypesCreator`, `ExtDataTypeDef`, `ExtDataConstants`, `ExtDataLgr` |
| Time | `ExtDataUpdatePointer` (update alarm), `ExtDataBracket`, `ExtDataNode` (left/right field, time, file, index) |
| File handlers | `ExtDataAbstractFileHandler` (abstract `get_file_bracket`), `ExtDataSimpleFileHandler`, `ExtDataClimFileHandler` |
| IO | `ExtData_IOBundleMod`, `ExtData_IOBundleVectorMod` (wrap `MAPL_GriddedIO`, regrid method, fraction) |
| Containers | `ExtDataPrimaryExportVector`, `ExtDataDerivedExportVector` |

## Initialize
`Initialize_` (`ExtDataGridCompNG.F90`):
1. Read `EXTDATA_YAML_FILE`; `get_global_options`. If `USE_EXTDATA` is false
   or ExtData's export state is empty, set inactive and return.
2. Build `ExtDataOldTypesCreator` from the YAML at the clock's current time.
3. For each export item, classify with `get_item_type`: derived, primary
   scalar or vector, or not found. Not-found items are logged and cause
   `_FAIL("Unsatisfied imports in ExtData")`.
4. Add extra primary items needed by derived expressions
   (`get_extra_derived_items`), held in `ExtDataState` (internal namespace
   state).
5. Per primary item and rule: `fillin_primary`; multi-rule items get one
   entry per rule with start/end times; register a pFIO collection
   (`MAPL_DataAddCollection`); if the rule overlaps the run range, `GetLevs`
   reads the file's vertical structure.
6. Per derived item: `fillin_derived`.
7. Create holding fields for primary items needed by derived items; add
   companion `PS_<name>` and `Q_<name>` items when rules carry auxiliary
   surface-pressure or humidity variables (vertical regrid);
   `confirm_imports_for_vregrid`.
8. Log the list of variables ExtData will provide.

## Run (each step)
`Run_` returns at once when inactive. Otherwise:
1. **READ_LOOP**, per primary item: pick the rule valid at the current time
   (`get_item_index`); constant items are set once. `update_freq%check_update`
   (`ExtDataPointerUpdate`) decides whether to update now and supplies the
   time to use; `filestream%get_file_bracket` (the handler) fills the item's
   `ExtDataBracket` with left and right nodes (file, time index). A bundle
   entry is queued for each node that needs data.
2. Populate bundles, create CFIO objects, `MAPL_ExtDataPrefetch`, wait on the
   pFIO i-clients, `MAPL_ExtDataReadPrefetch`; regridding happens in this
   read through `MAPL_GriddedIO` using the rule's method. Then
   `MAPL_ExtDataFlipBracketSide` and destroy the CFIO objects.
3. **INTERP_LOOP**: for items updated this step,
   `MAPL_ExtDataInterpField` interpolates the bracket to the time
   (`ExtDataBracket%interpolate_to_time`; linear unless `time_interpolation`
   is false; `linear_transformation` offset/scale applied).
4. **VINTERP_LOOP**: `MAPL_ExtDataVerticalInterpolate` for items with
   `enable_vertical_regrid` (conserving mass-mixing, emission or
   volume-mixing remap with PS/Q, from `VerticalRegridConserveInterfaceMod`).
5. **Derived**: per derived item, if its update alarm rings,
   `evaluate_derived_field` over `ExtDataState`.
Finalize logs files read when `log_files_read` is set (*Inferred* from the
config key; not read in detail).

## Time bracketing
- `ExtDataBracket` holds `left_node` and `right_node`; `swap_node_fields`
  slides the bracket forward when the time passes the right node, so only the
  new side is read.
- `ExtDataSimpleFileHandler`: time-ordered file series from the collection
  template; finds files via `freq`, `ref_time`, `valid_range`; honours
  `persist_closest` outside the valid range.
- `ExtDataClimFileHandler`: used when `extrapolation: clim`
  (`primary_item%cycling`); maps the target time into a climatology year
  (`swap_year`), including year-wrap bracketing.
- `ExtDataAbstractFileHandler` supplies shared pieces: file metadata,
  `get_time_on_file`, `find_any_file`, snapping a time to the file grid.
- `exact: true` and `time_interpolation: false` disable interpolation;
  `source_time` restricts which times of the source are used.

## Derived expressions
`ExtDataDerived` stores `function`. If the string contains `mask` it is
handled by `StateMask` (for example `regionmask(CO_BF,CO_regionMask;4,10)`);
otherwise by the MAPL arithmetic parser (`parser_variables_in_expression`,
evaluated with `MAPL_StateEval`; grammar in `../../base/ArthParser.md`).
Variables of the expression must themselves be exports (primary or derived)
in the YAML.

## Who uses it
- Job script: `gcm_run.j` copies `*.yaml` into the run directory and runs
  `construct_extdata_yaml_list.py GEOS_ChemGridComp.rc`, which writes
  `extdata.yaml` as a `subconfigs:` list of component YAMLs for each
  `ENABLE_<comp>: .TRUE.` plus "Extras"; `gcm_run.j` also `touch`es an empty
  `ExtData.rc` ("MAPL expectation ... not actually used"). Also
  `gcm_forecast.tmpl`.
- Component YAMLs (examples): `@GOCART/ESMF/GOCART2G_GridComp/DU2G_GridComp/DU2G_GridComp_ExtData.yaml`
  (Collections, Samplings with `persist_closest`, `clim`,
  `update_frequency: PT24H`, Exports with `regrid: CONSERVE`);
  `@TR/TR_ExtData.yaml` (a `Derived:` sum of exports);
  `@GOCART/ESMF/GOCART_GridComp/CO_GridComp/CO_GridComp_ExtData.yaml`
  (`regionmask` derived); `DataSea_ExtData.yaml` (`collection: /dev/null`).
  All under `src/Components/@GEOSgcm_GridComp/...`.
- Cap: `MAPL_CapGridComp` creates, initializes (`initialize_extdata`), runs
  and finalizes it each step.

## Known traps
- ExtData runs only on items nobody else provides; an unmatched import is a
  hard failure at Initialize, not a silent default.
- Out-of-range times need `valid_range` in the collection and an
  `extrapolation` choice; otherwise the run stops.
- Duplicate key names across subconfig files are errors.
- `ExtData.rc` must still exist (Cap loads it) even though 2G ignores its
  contents except `EXTDATA_YAML_FILE`.

## Observations
- `ExtDataGridComp.md` gives the YAML default `extdata.yaml` but does not
  name the key: the code reads `EXTDATA_YAML_FILE:` from `ExtData.rc`, which
  GEOSgcm leaves empty, so the default applies.
- Code default for `USE_EXTDATA2G` is `.false.`; the CMake option default is
  `ON`; GEOSgcm's `CAP.rc.tmpl` sets true. Three different defaults.
- Multi-rule items use `sidx` as `I1`, which suggests at most nine rules per
  name (*Inferred* from `write(sidx,'(I1)')`).
- `ExtData_IOBundleMod.F90` and `ExtData_IOBundleVectorMod.F90` exist in
  both ExtData directories; the modules are named differently
  (`MAPL_ExtData_IOBundleMod` in the legacy one, `MAPL_ExtDataNG_IOBundleMod`
  here).
