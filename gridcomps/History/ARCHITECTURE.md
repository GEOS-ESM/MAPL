# gridcomps/History: the History gridded component

**Audience:** coding agents and developers working on MAPL `gridcomps/History/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `gridcomps/History/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## 1. Purpose

`History` writes diagnostic output: it reads fields from the exports of the
component hierarchy, optionally regrids them (horizontal and vertical),
optionally time-averages them, and writes them as file "collections"
(`<dir>/MAPL_HistoryGridComp.F90`, module `MAPL_HistoryGridCompMod`).
Every GEOSgcm run uses it. It is a child of Cap (see
`../Cap/ARCHITECTURE.md`), not of the GEOS root.

User-level documents (not repeated here):
- `<dir>/HistoryGridComp.md`: key reference for `HISTORY.rc`.
- `<dir>/README.md`: short introduction.

## 2. Files

| File | Role |
|---|---|
| `MAPL_HistoryGridComp.F90` | `SetServices`, `Initialize`, `Run`, `Finalize`, `RecordRestart`; config parsing; regrid, expression and splitting helpers; legacy GrADS (`flat`) writer `MAPL_GradsCtlWrite` |
| `MAPL_HistoryCollection.F90` | types `HistoryCollection` (one per collection: alarms, grids, items, samplers, state), `FieldSet`, `HistoryCollectionGlobalAttributes`; gFTL vector/map instantiations |
| `Sampler/MAPL_TrajectoryMod*.F90` | `HistoryTrajectoryMod`: trajectory sampler |
| `Sampler/MAPL_MaskMod*.F90` | `MaskSamplerMod`: mask sampler |
| `Sampler/MAPL_StationSamplerMod.F90` | `StationSamplerMod`: station sampler |
| `Sampler/MAPL_EpochSwathMod.F90` | `MAPL_EpochSwathMod`: swath (epoch) sampler |

Build: one library `MAPL.history` (`<dir>/CMakeLists.txt`), depending on
`MAPL.shared`, `MAPL.constants`, `MAPL.base`, `MAPL.generic`, `MAPL.profiler`,
`MAPL.griddedio`, `MAPL.state_utils`. All files listed are compiled.

## 3. How Cap creates History

(`../Cap/MAPL_CapGridComp.F90`, Cap `SetServices`, `initialize_history`,
`run`)
- Cap reads `HIST_CF:` (default `HIST.rc`) with `MAPL_GetResource`, loads it as
  `cap%CF_HIST`, and adds the child `HIST` with `MAPL_AddChild(..., SS =
  Hist_SetServices)`; it stores `history_id`.
- `initialize_history`: the root's export state (`cap%child_exports(root_id)`)
  is added to History's import state (comment: "All the EXPORTS of the
  Hierarchy are made IMPORTS of History"). Cap also attaches the
  `MAPL_LocStreamList` internal state (type `HISTORY_ExchangeListWrap`), then
  calls `ESMF_GridCompInitialize` on History.
- Run: Cap keeps a separate clock `clock_hist`, a copy of the model clock. In
  the last run phase Cap runs the root, advances `clock` and `clock_hist`, then
  runs History with `clock_hist`. History therefore sees the exports of the
  step just computed, with the clock already advanced. `WriteRestart` is called
  on both root and History (entry point `RecordRestart`) in the first phase.
- Finalize: Cap calls `ESMF_GridCompFinalize` on History.

History's own `SetServices` registers `Initialize`, `Run`, `Finalize` and
`WRITERESTART`, allocates a `HISTORY_STATE` internal state, and calls
`MAPL_GenericSetServices`; it declares no specs.

## 4. HISTORY.rc structure

The file is an ESMF Config. Keys below are read in `Initialize`
(`MAPL_HistoryGridComp.F90`) unless another routine is named.

**Global keys** (read from History's own config): `EXPID:`, `EXPDSC:`,
`EXPSRC:`, `CoresPerNode:`, `DisableSubVmChecks:`, `AvoidRootNodeThreshold:`,
`FileOrder:` (default `ABC`), `Allow_Overwrite:`, `file_weights:`,
`IntegerTime:`, `CollectionWriteSplit:`, `ServerSizeSplit:`,
`SINGLE_COLUMN:`, `VERSION:` (default 1), and the global-attribute keys
(`Institution:`, `References:`, `Contact:`, `Comment:`, `Conventions:`,
`Source:`).

**Lists:** `COLLECTIONS:` (a `::`-terminated list of quoted names; only listed
collections are written), `GRID_LABELS:` (output grid names; each name has
`<name>.GRID_TYPE:` and grid-factory keys such as `.IM_WORLD:`, `.NX:`; made
with `grid_manager%make_grid`), and, for `VERSION: 2`, `FIELD_SETS:` (named,
reusable field lists, referenced with `<coll>.field_set:`).

**Per-collection keys** (`<coll>.<key>:`), read per collection from a
split-out file (below):

| Key | Meaning / default |
|---|---|
| `template` | file-name template (default empty) |
| `format` | `flat` (default; GrADS binary plus `.ctl`) or `CFIO` (netCDF through pFIO); `CFIOasync` is silently changed to `CFIO` with a message |
| `mode` | `instantaneous` (default) or `time-averaged` |
| `frequency` (default 060000), `duration` (default = frequency), `ref_date`, `ref_time`, `start_date`/`start_time`, `end_date`/`end_time`, `acc_interval`, `acc_ref_time` | alarm set-up |
| `monthly` | integer flag; sets frequency to one month and moves ref time to the start of the month |
| `grid_label` | key into `GRID_LABELS` for horizontal regridding; `resolution:` and `cubeFormat:` also read |
| `regrid_method` or `conservative` | the two are exclusive; `xlevels:` is also read |
| `levels`, `vvars`, `vscale`, `vunit`, `positive` | vertical interpolation (`vvars` is a coordinate expression plus its source component, for example `'log(PLE)' , 'DYN'`); without `vvars`, `levels` are level indices |
| `fields` | the field table (below) |
| `splitField`, `UseRegex` | integer flags |
| `deflate`, `nbits`, `zstandard_level`, `quantize_algorithm`, `quantize_level`, `chunksize` | netCDF compression and chunking |
| `sampler_type`, `station_id_file`, `station_skip_line`, `obs_files` | sampler selection (section 7) |
| `regrid_name`, `regrid_exch`, `tilefile`-type keys | tile regridding to or from a `LocStream` |
| `backwards`, `ForceOffsetZero`, `timeStampStart`, `verbose` | undocumented or rarely used |

**Split step.** The root rank scans the file named by `HIST_CF:` (default
`HIST.rc`) and writes one `<coll>.rcx` file per collection, containing every
line starting with `<coll>.` plus continuation lines of `fields` and
`obs_files` up to `::`. Each collection is then loaded from its own `.rcx`.
`regen_rcx_for_obs_platform` replaces this when `HISTORY.rc` has
`DEFINE_OBS_PLATFORM` (OSSE use); `regen_rcx_for_schema_version_1` handles old
trajectory schemas.

**Field table** (`parse_fields`): each row has the export name, the component
name, an optional output alias, and an optional time-averaging coupler
function (`MIN`/`MAX`). `U;V` in the name field marks a vector pair for
regridding. Names containing any of `()^/*+-` are treated as expressions
(`MAPL_SetExpression`, `MAPL_RunExpression`), unless `UseRegex` is set.
`wildCardExpand` expands regex names; `splitUngriddedFields` implements
`splitField`.

## 5. How History gets fields, and when it runs

(`MAPL_HistoryGridComp.F90`, `Initialize`)
- The list of component names is collected from the field tables
  (`statelist`). Each is looked up in History's import state, which holds the
  root's export state, with `MAPL_ExportStateGet`; the component name in the
  field table is therefore the name of a child of the root (for example
  `DYN`, `MOIST`, `AGCM`). A name not found on all ranks is reported with
  `Cannot Find <name>` and the run aborts; a name missing only on some ranks
  sets `subVm` for the collection.
- Each found export state is copied (`CopyStateItems`) so History can add or
  split fields without changing the component's export.
- Per collection, `MAPL_StateGet` finds the field (or bundle) by export name;
  failure writes `ERROR: cannot find output <name> in <component>`.
- Per collection, History builds working states: `CIM`/`GIM` (inputs, with the
  generic coupler `MAPL_GenericCplComp` for time averaging) and `Regrid(n)`
  (`state_out`). Time averaging runs in coupler components created in
  `Initialize` and run each step in `Run`.
- `Run` (one call per model step): for each collection, accumulate through the
  coupler; then decide from alarms (`his_alarm`, `seg_alarm`, `mon_alarm`,
  `start_alarm`, `end_alarm`, in `HistoryCollection`) whether to write; on
  writing, generate the file name from `template` (`MAPL_StringTemplate`),
  regrid, and post. After the loop it calls `o_Clients%done_collective_stage`
  and `post_wait`.
- `Finalize`: closes `CFIO` collections, finalizes mask and station samplers.

## 6. Output path

- `format: CFIO` goes through pFIO using `griddedio` (`use MAPL_GriddedIOMod`,
  `pFIO_ClientManagerMod`): per collection `Initialize` creates a
  `mGriddedIO` object, calls `CreateFileMetaData`, then
  `o_Clients%add_hist_collection`; `Run` calls `mGriddedIO%modifyTime` and
  `bundlepost` to send the data to the output server. See
  `../../griddedio/ARCHITECTURE.md` and `../../pfio/ARCHITECTURE.md`.
- Shipped templates: the code default is `format: flat`, but 19 of the 20
  `src/Applications/@GEOSgcm_App/HISTORY*.tmpl*` files set `format: 'CFIO'`
  on their collections (1 to 112 collections per file), and
  `HISTORY_MERRA2-DD.rc.tmpl` sets `'CFIOasync'` (changed to `CFIO` by
  `Initialize`). None sets `flat`. GEOSgcm runs therefore normally write
  through pFIO.
- `format: flat` (code default; no shipped template uses it) writes
  GrADS-style binary with `GETFILE` and `MAPL_GradsCtlWrite`. The source still
  has `use MAPL_CFIOMod` and calls to `MAPL_CFIOIsCreated` /
  `MAPL_CFIOdestroy` in `Finalize`. The top-level `../../README.md` (item 8)
  calls `MAPL_cfio` "a deprecated lower-level I/O layer that is generally
  replaced by GMAO_pFIO". See `../../MAPL_cfio/ARCHITECTURE.md`.
- `MAPL_TileGridIOMod` is imported for tile-based output; *Inferred:* it is
  used with the tile-file keys (`tilefile`, `regrid_name`, `regrid_exch`).

## 7. Samplers

A collection selects a sampler with `<coll>.sampler_type:`; empty (default)
means the ordinary gridded path. Selection is a chain of `if` tests on
`sampler_type` in `Initialize`, `Run` and `Finalize`.

| `sampler_type` | Module | What it does | Extra keys |
|---|---|---|---|
| `trajectory` | `HistoryTrajectoryMod` | samples along a track given by observation files; regrids to track points; output is a time series; own alarm and file handle | `obs_files`, `Epoch`, `schema`, `index`, `var_name_*` keys, `output_leading_dim`; GRID_LABELS and index variable names added in `regen_rcx_for_schema_version_1` |
| `mask` | `MaskSamplerMod` | samples the model at points from a mask (grid-file) definition; own `oClients` collection | `obs_files`, `index_name_x/y`, `var_name_x/y/proj`, `att_name_proj`, `thin_factor`, `output_leading_dim` |
| `station` | `StationSamplerMod` | samples at station locations from a station file; appends each step | `station_id_file`, `station_skip_line` |
| `swath` | `MAPL_EpochSwathMod` (`samplerHQ`) | accumulates regridded data on a swath grid over an epoch; `GRID_TYPE: Swath` entries in `GRID_LABELS` | `<grid>.Epoch:`, `<grid>.tunit:`; epoch must equal frequency (`verify_epoch_equals_freq`) |

## 8. Who uses it

Every GEOSgcm run. Templates are in
`src/Applications/@GEOSgcm_App`: `HISTORY.rc.tmpl`, `HISTORY.AGCM.rc.tmpl`,
`HISTORY.AOGCM.rc.tmpl`, `HISTORY_MERRA2.rc.tmpl`, `HISTORY_S2S.rc.tmpl`,
`HISTORY_FP.rc.*.tmpl` and others; `gcm_setup` selects one and copies it to
`HISTORY.rc`.
- `HISTORY.AGCM.rc.tmpl`: `COLLECTIONS: 'geosgcm_prog'`; `geosgcm_prog.format:
  'CFIO'`, `.grid_label: PC@CLIM_IMx@CLIM_JM-DC`, `.regrid_method:
  'BILINEAR'`, `.vvars: 'log(PLE)' , 'DYN'`, `.levels:` in hPa with `.vscale:
  100.0`, `.fields:` rows such as `'PHIS' , 'AGCM'`.
- `HISTORY.rc.tmpl`: `COLLECTIONS:` such as `'inst3_3d_asm_Np-'`.
- `gcm_run.j` greps `HISTORY.rc` for `<coll>.monthly: 1` to find monthly
  restart names.
No `sampler_type` appears in the fixture's `*.tmpl` files.

## 9. Known traps

- The split step is textual: a key line is picked up only if it begins with
  `<coll>.`.
- The code default `format` is `flat`, but every shipped GEOSgcm template
  sets `CFIO` (or `CFIOasync`); a new collection that omits `format:` writes
  GrADS binary, unlike the shipped ones.
- Field table component names must be children of the root; a typo aborts at
  `Initialize`.
- `_RC` on many `ESMF_ConfigGetAttribute` calls for optional keys are paired
  with `default=`; a missing optional key is silent.

## 10. Observations (noted, not fixed)

- `HistoryGridComp.md` lists `coll_name.nbit:`; the code reads `nbits:`
  (section 3 of the same document says `nbits`).
- `HistoryGridComp.md` section 2.4 and 4.1 mention `regrid_label`/`conservative`;
  the code reads `regrid_method:` and `conservative:`; `regrid_label` is not
  read. The document also lists `regrid_name`/`regrid_exch` without
  descriptions; they are tile-regrid keys read near the end of the per-collection
  block.
- `HistoryGridComp.md` says `end_date`/`end_time` turn the collection off and
  `ref_date` defaults to the application clock date; the code defaults match
  that, but the document omits `start_date`, `start_time`, `acc_interval`,
  `acc_ref_time`, `sampler_type`, `field_set`, `FIELD_SETS`, `VERSION`,
  `zstandard_level` and `quantize_*`, all read by the code.
- `HistoryGridComp.md` says "`History` uses MAPL PFIO"; this holds for normal
  runs. The only difference is that the code default (`format` = `flat`)
  differs from every shipped template (`CFIO`).
- `HistoryGridComp.md` sections 4.3, 4.4 and 4.6 (expressions, bundles,
  monthly) are headings with no text.
- `HistoryGridComp.md` says Cap calls the three methods; Cap also calls
  `WriteRestart` (`RecordRestart`). The source header lists `format` as "flat"
  or "CFIO"; the code also accepts `CFIOasync` as a deprecated alias.
