# MAPL (MAPL2 line) — repository architecture

**Audience:** coding agents and developers working on MAPL and on GEOS gridded components.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Parent: [GEOSgcm fixture architecture, section 2.3 MAPL](../../../../docs/ARCHITECTURE.md). Index and rules: [../AGENTS.md](../AGENTS.md). Decisions: [decisions.md](decisions.md).

## 1. Layer map
Library targets and their `DEPENDENCIES` (each directory's `CMakeLists.txt`):

```
shared/ (MAPL.constants, MAPL.shared)
   |-> profiler  (MAPL.profiler: shared)
   |-> field_utils (shared)
   |-> pfio      (MAPL.pfio: shared, profiler)
   |-> udunits2f (shared)
MAPL_cfio_r4/r8 (ESMF, NetCDF only)
   |
base (MAPL.base: shared, constants, profiler, pfio, MAPL_cfio_r4, field_utils, udunits2f)
   |-> oomph    (MAPL.oomph: base)
   |-> generic  (MAPL.generic: oomph, shared, profiler, base)
   |-> griddedio (shared, constants, base, pfio, MAPL_cfio_r4)
   |-> state    (MAPL.state_utils: base, shared)
   |-> vertical (shared, base, pfio)
   |-> gridcomps (MAPL.gridcomps: base, pfio, MAPL_cfio_r4, MAPL.cap; Cap, History, ExtData, ExtData2G, Orbit)
MAPL (umbrella library `MAPL`): base, pfio, MAPL_cfio_r4, griddedio,
   field_utils, state_utils, vertical, oomph, generic, gridcomps, orbit,
   python_bridge and the ExtData2G target (shared, profiler and udunits2f
   arrive through these)
```
Details per directory are in the documents of section 6. Dependencies other than those listed are in each directory document.

## 2. What `use MAPL` brings in
The module `MAPL` (`MAPL/MAPL.F90`) uses `MAPLBase_mod`, `MAPL_GenericMod`, `MAPL_VarSpecMiscMod`, `ESMF_CFIOMod`, `pFIO`, `MAPL_GridCompsMod`, `mapl_StubComponent`, `MAPL_ESMFFieldBundleRead`/`Write` (griddedio), `MAPL_OpenMP_Support` (renamed entities), `MAPL_Profiler`, `MAPL_FieldUtils`, `MAPL_StateUtils` and `MAPL_PythonBridge`, mostly without `only:` lists, so a component sees the union of their public names. Through `MAPLBase_mod`, which has no module-level `private`, it also gets `MAPL_ExceptionHandling`, the constants and most shared modules. The library `MAPL` links more than the module uses: `vertical`, `oomph` and `udunits2f` modules are not brought in by `use MAPL`. See [../MAPL/ARCHITECTURE.md](../MAPL/ARCHITECTURE.md).

## 3. Run-time picture of a GEOS run
- The main program builds a `MAPL_Cap` and calls its `run`: it starts MPI and ESMF, splits the communicator for pFIO servers and ensemble members, then creates and drives `CapGridComp` ([Cap](../gridcomps/Cap/ARCHITECTURE.md)).
- `CapGridComp` owns the clock, reads `CAP.rc`, creates the root component (the GCM) and the `HIST` and `EXTDATA` children.
- Each time step: ExtData fills imports that no component provides (run phase 1) ([ExtData2G](../gridcomps/ExtData2G/ARCHITECTURE.md) or [ExtData](../gridcomps/ExtData/ARCHITECTURE.md), chosen by `USE_EXTDATA2G` in `CAP.rc`); the root runs its children; the clock advances; History collects exports and writes ([History](../gridcomps/History/ARCHITECTURE.md)).
- File output goes through pFIO: clients on the model ranks send to server ranks ([pfio](../pfio/ARCHITECTURE.md), [griddedio](../griddedio/ARCHITECTURE.md)).
- User-level descriptions: [CapGridComp.md](../gridcomps/Cap/CapGridComp.md), [HistoryGridComp.md](../gridcomps/History/HistoryGridComp.md), [ExtDataGridComp.md](../gridcomps/ExtData2G/ExtDataGridComp.md).

## 4. How a component uses MAPL
- `SetServices`: the component registers its IRF methods and declares import/export/internal specs (`MAPL_AddImportSpec`, `MAPL_AddExportSpec`, `MAPL_AddInternalSpec`, or ACG-generated include files from `Apps/`), children (`MAPL_AddChild`) and connections (`MAPL_AddConnectivity`), then calls `MAPL_GenericSetServices`.
- Resources are read from the component's rc file with `MAPL_GetResource`; timers via `MAPL_TimerOn`/`MAPL_TimerOff`; pointers to data via the `MAPL_GetPointer` macro.
- Errors are handled with the macros `_RC`, `_VERIFY`, `_ASSERT`, `_RETURN`, `_FAIL`, `__RC__`.
- Details: [generic](../generic/ARCHITECTURE.md), [include](../include/ARCHITECTURE.md), tool [Apps](../Apps/ARCHITECTURE.md).

## 5. Cross-directory traps
- `USE_EXTDATA2G` has three defaults: CMake option ON, code default `.false.` in Cap, and GEOSgcm `CAP.rc.tmpl` true ([ExtData2G](../gridcomps/ExtData2G/ARCHITECTURE.md), [Cap](../gridcomps/Cap/ARCHITECTURE.md)).
- History collection `format` defaults to `flat` in code; shipped GEOSgcm templates set `CFIO` ([History](../gridcomps/History/ARCHITECTURE.md)).
- There is no automatic connection by name between components; the user guide says there is ([generic](../generic/ARCHITECTURE.md)).
- `MAPL_GetPointer` is a preprocessor macro, not a routine ([include](../include/ARCHITECTURE.md)).
- `base/MAPL_Profiler.F90` defines `MAPL_ProfMod`; the `profiler/` library defines `mapl_Profiler` ([profiler](../profiler/ARCHITECTURE.md), [MAPL](../MAPL/ARCHITECTURE.md)).
- ExtData and ExtData2G are both built; both have `ExtData_IOBundleMod.F90` and `ExtData_IOBundleVectorMod.F90` with different module names ([ExtData](../gridcomps/ExtData/ARCHITECTURE.md)).
- `oomph` is not the real spec layer (that is `generic/`) and is linked into `MAPL` though `MAPL.F90` does not use it ([oomph](../oomph/ARCHITECTURE.md)).
- `profiler/CMakeLists.txt` lists `MinCycleColumn.F90` twice ([profiler](../profiler/ARCHITECTURE.md)).
- `HEARTBEAT_DT` must equal `RUN_DT` in the root and ExtData rc files ([Cap](../gridcomps/Cap/ARCHITECTURE.md)).
- Existing documents disagree with the code in places (for example History key names); see each directory's Observations.

## 6. Children
[generic](../generic/ARCHITECTURE.md), [include](../include/ARCHITECTURE.md), [base](../base/ARCHITECTURE.md), [pfio](../pfio/ARCHITECTURE.md), [griddedio](../griddedio/ARCHITECTURE.md), [MAPL_cfio](../MAPL_cfio/ARCHITECTURE.md), [gridcomps](../gridcomps/ARCHITECTURE.md), [Cap](../gridcomps/Cap/ARCHITECTURE.md), [History](../gridcomps/History/ARCHITECTURE.md), [ExtData](../gridcomps/ExtData/ARCHITECTURE.md), [ExtData2G](../gridcomps/ExtData2G/ARCHITECTURE.md), [Orbit](../gridcomps/Orbit/ARCHITECTURE.md), [shared](../shared/ARCHITECTURE.md), [profiler](../profiler/ARCHITECTURE.md), [field_utils](../field_utils/ARCHITECTURE.md), [state](../state/ARCHITECTURE.md), [vertical](../vertical/ARCHITECTURE.md), [udunits2f](../udunits2f/ARCHITECTURE.md), [oomph](../oomph/ARCHITECTURE.md), [MAPL](../MAPL/ARCHITECTURE.md), [Apps](../Apps/ARCHITECTURE.md), [Python](../Python/ARCHITECTURE.md), [Tests](../Tests/ARCHITECTURE.md), [benchmarks](../benchmarks/ARCHITECTURE.md), [pflogger_stub](../pflogger_stub/ARCHITECTURE.md).

## 7. Build structure
- Top `CMakeLists.txt` options: `USE_EXTDATA2G`, `BUILD_WITH_FARGPARSE`, `BUILD_WITH_PFLOGGER`, `BUILD_WITH_FLAP`, `BUILD_SHARED_MAPL`, `BUILD_PYTHONBRIDGE` (not defined as an option in MAPL's CMake; only tested in `Python/CMakeLists.txt`).
- `MAPL_cfio` produces two libraries (`MAPL_cfio_r4`, `MAPL_cfio_r8`).
- `docs/` and `benchmarks/` are added only with `BUILD_WITH_FARGPARSE`; `pfunit/` only when pFUnit is found.
- In the GEOSgcm fixture MAPL is built from source only when no external MAPL is found (`src/Shared/CMakeLists.txt`, per the fixture document).
