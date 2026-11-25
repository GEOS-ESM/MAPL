# generic/ — MAPL_Generic, specs, connections, generic IRF methods

**Audience:** coding agents and developers working on GEOS gridded components and on MAPL.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `generic/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## 1. Purpose

`generic/` builds the library target `MAPL.generic` (`generic/CMakeLists.txt`;
depends on `MAPL.oomph`, `MAPL.shared`, `MAPL.profiler`, `MAPL.base`, pflogger,
gFTL, ESMF, NetCDF). It holds `MAPL_GenericMod` (`MAPL_Generic.F90`, ~11k
lines), the module every GEOS gridded component uses: generic
SetServices/Initialize/Run/Finalize (IRF) methods, the import/export/internal
specs, children, connections, resources, timers, and the `MAPL_MetaComp`
object. Also: the variable-spec types, the automatic coupler component, and a
set of small component-class modules (section 12).

Other documents: user guide `docs/user_guide/docs/mapl_core.md` and
`mapl_connect.md` (hand-written; not restated here). Error macros:
`../include/ARCHITECTURE.md`. `MAPL_GetPointer`: section 11.

## 2. The SetServices pattern

A component's `SetServices(GC, RC)` (verified in `GEOS_GcmGridComp.F90`,
`GEOS_PhysicsGridComp.F90` under `src/Components/@GEOSgcm_GridComp`) does,
in this order by convention:

1. `call MAPL_GridCompSetEntryPoint(GC, ESMF_METHOD_INITIALIZE, Initialize, _RC)`
   and `..._RUN, Run` for any phase the component overrides (section 8).
2. `MAPL_AddImportSpec / AddExportSpec / AddInternalSpec` calls (section 3).
3. `MAPL_AddChild` and `MAPL_AddConnectivity / MAPL_TerminateImport`
   (section 4), if composite.
4. `MAPL_TimerAdd` calls (a no-op, section 7).
5. `call MAPL_GenericSetServices(GC, _RC)` last.

`MAPL_GenericSetServices` (`MAPL_Generic.F90`) then: retrieves the
`MAPL_MetaComp` (creating it if absent, `MAPL_InternalStateRetrieve`);
registers the generic Initialize, Run (as `MAPL_GenericRunChildren`),
Finalize and WriteRestart (`MAPL_GenericRecord`) only for methods the
component has not already registered (`register_generic_entry_points`);
finds the root GC (`MAPL_GetRootGC`); wires the children
(`setup_children`, calling `MAPL_WireComponent`, section 4); and processes
export `DEPENDS_ON` / `DEPENDS_ON_CHILDREN` (marks the named specs
`alwaysAllocate`; `process_spec_dependence`).

Registering your own method before `MAPL_GenericSetServices` replaces the
generic one; your method normally calls the generic one directly
(`MAPL_GenericInitialize`, `MAPL_GenericRunChildren`, `MAPL_GenericFinalize`
are public for that reason; header comment of `MAPL_Generic.F90`). The
assumption stated in the header comment: grid and config are already present
in the GC when `MAPL_GenericSetServices` runs.

## 3. The three specs

Calls: `MAPL_AddImportSpec`, `MAPL_AddExportSpec`, `MAPL_AddInternalSpec`
(generic interfaces in `MAPL_Generic.F90`; specific routines
`MAPL_StateAddImportSpec_`, `MAPL_StateAddExportSpec_`,
`MAPL_StateAddInternalSpec`). Each stores a `MAPL_VarSpec` in the component's
`COMPONENT_SPEC` (`VarSpec.F90`, `VarSpecType.F90`, `StateSpecification.F90`);
no fields exist until Initialize (section 5).

| Option | Meaning | Spec kinds |
|---|---|---|
| `SHORT_NAME`, `LONG_NAME`, `UNITS` | name and CF-style metadata | all |
| `DIMS` | `MAPL_DimsHorzOnly`, `MAPL_DimsHorzVert`, `MAPL_DimsVertOnly`, `MAPL_DimsTileOnly`, `MAPL_DimsTileTile`, `MAPL_DimsNone` (`../shared/Constants/InternalConstants.F90`) | all |
| `VLOCATION` | `MAPL_VLocationNone/Edge/Center` | all |
| `DATATYPE`, `PRECISION`, `FIELD_TYPE`, `STAGGERING`, `ROTATION`, `HALOWIDTH`, `NUM_SUBTILES`, `UNGRIDDED_DIMS` | field kind and shape | all (`UNGRIDDED_UNIT/NAME/COORDS` export only) |
| `DEFAULT` | real value the field is filled with at creation | all |
| `REFRESH_INTERVAL`, `AVERAGING_INTERVAL` | time-averaging for couplers | all |
| `RESTART` | `MAPL_RestartOptional` (default), `MAPL_RestartSkip`, `MAPL_RestartRequired`, `MAPL_RestartBootstrap`, `MAPL_RestartSkipInitial` | import, internal |
| `FRIENDLYTO` | colon-list of components the field is "friendly" to (`MAPL_GridCompGetFriendlies` gathers them into a bundle) | internal |
| `ADD2EXPORT` | logical; also add the internal item to the export spec | internal |
| `ATTR_INAMES/IVALUES/RNAMES/RVALUES` | extra integer/real attributes | internal |
| `DEPENDS_ON`, `DEPENDS_ON_CHILDREN`, `POSITIVE` | allocation dependency; sign convention | export |

Variants: `MAPL_AddExportSpec(GC, SHORT_NAME, CHILD_ID[, TO_NAME])` and
`MAPL_AddImportSpec(GC, SHORT_NAME, CHILD_ID)` re-export or re-import a
child's item; `MAPL_AddExportSpec(GC, CHILD_ID)` re-exports all
(`MAPL_StateAddExportSpecFrmChld[_all]`, `...FrmAll`). Many GEOS components
generate these calls from ACG `*_StateSpecs.rc` files (see `../Apps/ARCHITECTURE.md`).

`MAPL_DoNotAllocateImport/Internal` and `MAPL_DoNotDeferExport` change the
allocation policy for named items.

Fixture use: `MAPL_AddExportSpec` ~4500 calls, `MAPL_AddImportSpec` ~1400,
`MAPL_AddInternalSpec` ~900 (meta scan of `src/Components`). Examples:
`GEOSmkiau_GridComp/IAU_GridCompMod.F90` (`DUDT`, `DVDT` internal specs,
`DIMS=MAPL_DimsHorzVert`, `VLOCATION=MAPL_VLocationCenter`);
`GEOSturbulence_GridComp/GEOS_TurbulenceGridComp.F90` (uses `FRIENDLYTO`/`ADD2EXPORT`).

## 4. Children, connections, wiring

- `MAPL_AddChild(GC, NAME=, SS=SetServicesRoutine[, grid, configFile, petList])`
  returns the child index (`AddChildFromGC`, `AddChildFromMeta`; DSO forms
  `AddChildFromDSO*` load `userRoutine` from a shared object). It creates the
  child GC with the parent's config (`child_meta%cf = meta%cf`) or, with
  `configFile`, a config loaded from that file; creates the child's import and
  export states `<name>_Imports/_Exports`; and calls the child's SetServices
  at once (`ESMF_GridCompSetServices`) (`AddChild_preamble`, `AddChildFromMeta`).
  If `NAME` has no `:` the parent's prefix up to its `:` is prepended
  (`make_full_name`). Fixture: 89 calls; `GEOS_GcmGridComp.F90` adds
  `AGCM`, `OGCM`, `AIAU`, `ADFI`, `GIGATRAJ`, `WGCM`, `DATAATM`.
- `MAPL_AddConnectivity(GC, SHORT_NAME=(/.../), SRC_ID=, DST_ID=)` (same name
  on both sides) or `(GC, SRC_NAME=, SRC_ID=, DST_NAME=, DST_ID=)` (rename;
  `...Rename`, `...RenameMany`, `...Many`) records an export-of-child-SRC to
  import-of-child-DST entry in `meta%connectList%connect`. Fixture: 149
  calls, e.g. `GEOS_GcmGridComp.F90` connects `PHIS AK BK U V TV PS DELP ...`
  from `AGCM` to `AIAU`.
- `MAPL_TerminateImport(GC, SHORT_NAME=, CHILD=)` (also `...Many`,
  `...AnyImport`, `...AllBut`, `...All`) puts an entry in the
  `DONOTCONN` list: that child import is *not* promoted to the parent's
  import. The "any import" form (`SHORT_NAME="MAPL_AnyChildImport"`) stops
  promotion of every import of that child (`MAPL_WireComponent`,
  `PARENTIMPORT`). Fixture: 32 calls, e.g. `GEOS_GcmGridComp.F90`.
- `MAPL_ConnectService / AdvertiseService / RequestService`: service
  connections (`MAPL_ServiceServices.F90`); not scanned for fixture use.

`MAPL_WireComponent` (run from `setup_children`) then, for each child import
not listed in `DONOTCONN`: if a connection exists from a sibling export
(`VarConn::varIsConnected`) it is satisfied, otherwise the import spec is
added to the parent's own import spec (so it propagates upward and is filled
by whoever feeds the parent). *Verified:* the wiring consults only the
explicit `connectList`; this code contains no match-by-name search among
siblings. (The user guide `mapl_connect.md` describes "automatic" coupling:
the automatic part is the coupler generation below, not discovery.)

- Same spec on both ends (`EX_SPECS(N) == IM_SPECS(K)`, `MAPL_Reconnect`):
  the child import field *is* the sibling export field (shared, no coupler).
- Specs differ (precision, interval, ...): a coupler `ESMF_CplComp` named
  `<src>_2_<dst>` is created (`GenericCplSetServices`, `GenericCplComp.F90`);
  `MAPL_GenericRunChildren` runs it after the source child's last run phase.
- `MAPL_GenericConnCheck` (root only) reports unused connect and
  do-not-connect entries as "CONNECT ERRORS FOUND" and fails.

## 5. Generic Initialize (`MAPL_GenericInitialize`)

Recursive. Order (`MAPL_GenericInitialize` and its internal routines):
read `COMPONENT_TO_RECORD:` and `TEST_FRAMEWORK:` resources; take the grid
from the GC (a child without a valid grid is given the parent's) and record
sizes, layout, lat/lon in `MYGRID`; read `CHECKPOINT_POSITIVE:`; set the
component clock and main run alarm (`handle_clock_and_main_alarm`, resources
`RUN_DT:` heartbeat, `DT:`, `RUN_AT_INTERVAL_START:`; component `DT` must be
a multiple of the heartbeat); tiling for components with a location stream;
`handle_record` (`RECORD_FREQUENCY:`, `RECORD_REF_DATE/TIME:`);
`initialize_children_and_couplers`; create the import state from import
specs; create the internal state `<comp>_INTERNAL` from internal specs and
read its restart (`INTERNAL_RESTART_FILE:`, `INTERNAL_HEADER:`; if no file
label, the component's `ReadRestart` method, "coldstart", is called when
registered); create the export state; add children's export states to the
parent export; `MAPL_AdjustIsNeeded`; service wiring; memory-use report
(`MAPL_MemUtilsWrite`). Specs become fields via
`MAPL_StateCreateFromVarSpecNew`. `RESTART=MAPL_RestartSkip` and friends are
honoured when the file is read (`../base/BinIO.F90`, `NCIO.F90`).

## 6. Generic Run, Finalize, Record, Refresh

- **Run** (`MAPL_GenericRunChildren`): runs children phase by phase: for
  `PHASE = 1..max(child run phases)`, for each child in `MAPL_AddChild`
  order that has that phase, `ESMF_GridCompRun` with the child's registered
  phase number (timer = child name). Couplers whose source is child I run
  when `PHASE == NUMPHASES` of child I. A leaf with no children does nothing.
- **Wrapper** (`MAPL_GenericWrapper`): every method registered through
  `MAPL_GridCompSetEntryPoint` is invoked via this wrapper: starts the global
  and component profilers (`GenRunMine` timer on Run), optionally records
  before/after (`COMPONENT_TO_RECORD:`), calls the user routine (through
  `omp_driver` if the component uses OpenMP threads, Run only), checks
  `userRC`.
- **Finalize** (`MAPL_GenericFinalize`): finalizes children; then writes
  checkpoints: `INTERNAL_CHECKPOINT_FILE:` (type
  `INTERNAL_CHECKPOINT_TYPE:`, default `DEFAULT_CHECKPOINT_TYPE:` = `pnc4`),
  `IMPORT_CHECKPOINT_FILE:`, optional export checkpoint
  (`checkpoint_export_state`); `RECORD_FINAL:`; reports the generic profile;
  destroys the state. Checkpoint names are GrADS templates
  (`fill_grads_template`).
- **Record** (`MAPL_GenericRecord`, registered as WRITERESTART; only one
  record phase is supported: `size(meta%phase_record)==1`) writes intermediate
  checkpoints of children and self; `MAPL_AddRecord`/`MAPL_DisableRecord`
  manage record alarms. **Refresh** (`MAPL_GenericRefresh`,
  `MAPL_METHOD_REFRESH`) re-reads state; both are driven by an external
  caller (*Inferred:* mainly Cap/History-level code, not checked here).

## 7. Resources and timers

`MAPL_GetResource(MAPL_or_config, val, label, default=, value_is_set=, rc=)`
(`MAPL_GetResourceFromMAPL_scalar/array`, `...FromConfig_...` in
`MAPL_Generic.F90`) delegates to `MAPL_GetResource_config_scalar/array`
(`../base/MAPL_Resource.F90`). With a `MAPL_MetaComp` the config is
`state%cf` (the component's ESMF_Config: the parent's, unless the child was
added with `configFile`) and the component name is `state%compname`. Label
search order (`get_labels_with_prefix`): `<compname>_<label>`,
`<type>_<label>` (the name after any `:`), bare `<label>`,
`<compname>.<label>`; first found wins. With a bare `ESMF_Config` only the
bare label is searched. If not found and `default` is absent, returns
`ESMF_FAILURE`; `PRINTRC:` controls echoing. `value` and `default` must have
the same type. Fixture: ~1600 calls, e.g. `GEOS_GcmGridComp.F90`
(`USE_CICE_Thermo:`).

Timers: `MAPL_TimerOn/Off(MAPL, name)` start/stop a node in the component's
`t_profiler` (`../profiler/ARCHITECTURE.md`); leading `-` characters in the
name are stripped; `'TOTAL'` is ignored. `MAPL_TimerAdd` is an empty routine
kept for compatibility. Generic Finalize prints the profile
(`report_generic_profile`). Fixture: ~1200 On/Off calls.

## 8. Run phases and entry points

`MAPL_GridCompSetEntryPoint(GC, method, routine, rc)`: `method` is
`ESMF_METHOD_INITIALIZE/RUN/FINALIZE/WRITERESTART/READRESTART` or
`MAPL_METHOD_REFRESH`. Each call appends a phase number n (1, 2, ... up to
`MAPL_MAX_PHASES` = 10) to `meta%phase_run` (etc.), registers
`MAPL_GenericWrapper` as ESMF phase n and the user routine as ESMF phase
`MAPL_MAX_PHASES + n`. So Run phases are simply the order of
`MAPL_GridCompSetEntryPoint(..., ESMF_METHOD_RUN, ...)` calls. A parent's
generic Run (above) calls child phase k only if `k <= size(child%phase_run)`.
A parent that overrides Run calls `ESMF_GridCompRun(child, phase=k)` itself.
Fixture: ~250 calls; `GEOS_PhysicsGridComp.F90`, `GEOS_GcmGridComp.F90`.
Exceeding 10 phases only prints an error message.

## 9. MAPL_MetaComp and access routines

`MAPL_MetaComp` (type in `MAPL_Generic.F90`; extends the framework component
classes of section 12) is held as an ESMF user internal state named
`MAPL_GenericInternalState`. Fields of interest: `compname`, `full_name`,
`cf`, `HEARTBEAT`, `t_profiler`, `connectList`, `CCS/CCcreated` (couplers),
`phase_init/run/final/record/coldstart/refresh`, `ALARM(0:LAST_ALARM)`,
`CLOCK`, `ORBIT`, `LOCSTREAM`, `RECORD`, plus child accessors
(`get_child_gridcomp`, `get_child_import_state`, ...).

- `MAPL_GetObjectFromGC(GC, MAPL, rc)` (`MAPL_InternalStateGet`): get it;
  valid after `MAPL_GenericSetServices`. Failure if absent. Fixture
  example: `GEOS_GcmGridComp.F90`.
- `MAPL_InternalStateRetrieve` / `MAPL_InternalStateCreate`: get-or-create.
- `MAPL_Get(MAPL, IM=, JM=, LM=, grid=, LONS=, LATS=, CF=, RUNALARM=,
  ORBIT=, HEARTBEAT=, INTERNAL_ESMF_STATE=, IMPORTspec=, EXPORTspec=,
  INTERNALspec=, GCS=, GIM=, GEX=, childrens_*=, ...)`
  (`MAPL_GenericStateGet`): read-only accessor with optional outputs.
  `MAPL_Set` (`...StateSet`, `...SetFromGC`): set `ORBIT`, `LM`,
  `RUNALARM`, `CHILDINIT`, etc.
- Other public helpers: `MAPL_GetLogger`, `MAPL_StateAlarmAdd/Get`,
  `MAPL_ReadForcing`, `MAPL_GetChildLocstream`, `MAPL_FriendlyGet`,
  `MAPL_ChildAddAttribToImportSpec`, `MAPL_StatePrintSpecCSV`,
  `MAPL_AddAttributeToFields`, `MAPL_MethodAdd`, `MAPL_GridCreate`.

## 10. Fixture usage summary (meta scan of `src/Components`)

`MAPL_GetPointer` ~8800; `MAPL_AddExportSpec` ~4500; `MAPL_GetResource`
~1600; `MAPL_AddImportSpec` ~1400; `MAPL_TimerOn/Off` ~1200;
`MAPL_AddInternalSpec` ~900; `MAPL_GridCompSetEntryPoint` ~250;
`MAPL_AddConnectivity` 149; `MAPL_AddChild` 89; `MAPL_GenericSetServices`
41; `MAPL_TerminateImport` 32. Reference composite:
`src/Components/@GEOSgcm_GridComp/GEOS_GcmGridComp.F90`.

## 11. Where `MAPL_GetPointer` lives

Not in `generic/`. The component-facing name comes from the header
`../include/MAPL_Generic.h`, which does
`#define MAPL_GetPointer ESMFL_StateGetPointerToData`; the routines are
generated from `../base/GetPointer.H` (included repeatedly by
`../base/ESMFL_Mod.F90`, one specific per rank/kind). Signature:
`(STATE, PTR, NAME, alloc=, notFoundOK=, rc=)`. See `../base/ARCHITECTURE.md`.
*Inferred:* sources that do not include `MAPL_Generic.h` use the module's
own name for these routines.

## 12. Other modules in `generic/`

| Group | Files | Role |
|---|---|---|
| Spec types | `VarSpecType.F90`, `VarSpec.F90`, `VarSpecPtr.F90`, `VarSpecVector.F90`, `VarSpecMiscMod.F90`, `StateSpecification.F90`, `ComponentSpecification.F90` | `MAPL_VarSpec` (legacy, public through `MAPL_VarSpecMod`), vectors, state/component spec containers; header of `VarSpecType.F90` says it "should be private to generic layer" |
| Connections | `VarConn.F90`, `VarConnPoint.F90`, `VarConnType.F90`, `VarConnVector.F90` | connect / do-not-connect lists |
| Coupler | `GenericCplComp.F90` | `MAPL_GenericCplCompMod`: automatic coupler between children (time averaging, regridding-free copy) |
| Component classes | `AbstractComponent`, `AbstractFrameworkComponent`, `AbstractComposite`, `BaseComponent`, `BaseFrameworkComponent`, `CompositeComponent`, `ConcreteComposite`, `StringCompositeMap`, `DecoratorComponent`, `MaplComponent`, `StubComponent`, `SurrogateFrameworkComponent`, `MaplGenericComponent.F90`, `MaplGeneric.F90` | composite/decorator hierarchy that `MAPL_MetaComp` extends to hold children by name (`add_child`, `get_child_idx`); `MaplGenericComponent` is a newer class with its own `SubComponent` type and `UserMethod` interface |
| Threads | `OpenMP_Support.F90` | `make_subgrids/subfields/substates/subgridcomps`, `find_bounds`: split grid and states by thread for `omp_driver` |
| Services | `MAPL_ServiceServices*.F90`, `MAPL_*ServiceItemVector.F90` | provide/request service connections |
| Misc | `RunEntryPoint.F90`, `EntryPointVector.F90`, `CallbackMap.F90`, `MAPL_ESMF_Interfaces.F90`, `driver.F90`, `UserComponent.F90` | entry-point lists; `demo` executable (`driver.F90` + `UserComponent.F90`) |

Unit tests: `generic/tests/` (`Test_CompositeComponent.pf`,
`Test_ConcreteComposite.pf`, `Test_VarSpec.pf`; built only with pFUnit,
`EXCLUDE_FROM_ALL`).

## 13. Known traps

- `MAPL_GenericSetServices` must be last in SetServices if you register your
  own entry points; entry points registered after it add a *second* phase
  (the generic one stays phase 1).
  *Inferred:* from `register_generic_entry_points` testing `associated(meta%phase_*)`.
- Imports not connected and not terminated are promoted to the parent;
  a typo in `MAPL_AddConnectivity` names fails in `MAPL_GenericConnCheck`.
- Child `DT` must be a multiple of the heartbeat `RUN_DT:`.
- Resource label lookup depends on component *name* (including prefix before
  `:`); renaming a child changes which `<name>_LABEL:` entries apply.

## 14. Observations (noted, not fixed)

- `MAPL_GetResourceFromMAPL_scalar` carries a FIXME: when no value is set it
  returns failure even if `value_is_set` is present.
- `MAPL_TimerAdd` (`MAPL_GenericStateClockAdd`) does nothing.
- `MAPL_GenericRunChildren` has the comment "if user wants to run particular
  phase only, when should we run couplers" (unresolved).
- Exceeding `MAPL_MAX_PHASES` prints but does not return an error.
- Many `_ASSERT` messages read 'needs informative message'.
- `MAPL_Generic.F90` header describes `MAPL_GenericSetServices` with
  optional child arguments (SS1..SS5) that the current routine does not have.
