# MAPL (umbrella library)

**Audience:** coding agents and developers working on MAPL `MAPL/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `MAPL/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
One source file, `MAPL.F90`, defining module `MAPL`, whose only job is to
re-export other packages' public entities, and the library `MAPL` that links
every MAPL library. Components write `use MAPL` and the linker flag `MAPL`
and get the component-facing API.

## Build (`MAPL/CMakeLists.txt`)
- Library `MAPL` (`esma_set_this()`), `TYPE ${MAPL_LIBRARY_TYPE}` (shared by default, option `BUILD_SHARED_MAPL`), source `MAPL.F90` only.
- Dependencies: `MAPL.base MAPL.pfio MAPL_cfio_r4 MAPL.griddedio MAPL.field_utils MAPL.state_utils MAPL.vertical MAPL.oomph`, `ESMF::ESMF`, `NetCDF::NetCDF_Fortran`, `MPI::MPI_Fortran`, `FLAP::FLAP` when `BUILD_WITH_FLAP`; always also `MAPL.generic MAPL.gridcomps MAPL.orbit MAPL.python_bridge` and `${EXTDATA_TARGET}`.
- Compile definition `BUILD_WITH_EXTDATA2G` when `USE_EXTDATA2G`. Note `MAPL.F90` itself has no `#ifdef`.

## What `use MAPL` brings in (`MAPL.F90`)
Module `MAPL` contains exactly these `use` statements and `implicit none`:

| Statement | Package (directory) |
|---|---|
| `use MAPLBase_mod` | base (`../base/ARCHITECTURE.md`); itself `use`s the base modules (see below) |
| `use MAPL_GenericMod` | `MAPL_Generic`, specs, timers (`../generic/ARCHITECTURE.md`) |
| `use MAPL_VarSpecMiscMod` | generic (VarSpec helpers) |
| `use ESMF_CFIOMod` | `MAPL_cfio` (`../MAPL_cfio/ARCHITECTURE.md`) |
| `use pFIO` | pfio (`../pfio/ARCHITECTURE.md`) |
| `use MAPL_GridCompsMod` | gridcomps (`../gridcomps/ARCHITECTURE.md`) |
| `use mapl_StubComponent` | stub-component support (generated via `mapl_create_stub_component`) |
| `use MAPL_ESMFFieldBundleRead`, `use MAPL_ESMFFieldBundleWrite` | griddedio (`../griddedio/ARCHITECTURE.md`) |
| `use MAPL_OpenMP_Support, only:` with renames `MAPL_get_current_thread => get_current_thread`, `MAPL_get_num_threads => get_num_threads`, `MAPL_find_bounds => find_bounds`, `MAPL_Interval => Interval` | OpenMP helpers |
| `use MAPL_Profiler, initialize_profiler => initialize, finalize_profiler => finalize` | profiler (`../profiler/ARCHITECTURE.md`) |
| `use MAPL_FieldUtils` | field_utils (`../field_utils/ARCHITECTURE.md`) |
| `use MAPL_StateUtils` | state (`../state/ARCHITECTURE.md`) |
| `use MAPL_PythonBridge` | Python (`../Python/ARCHITECTURE.md`) |

A second module in the same file, `MAPL_Mod`, only does `use MAPL`.

Arrives transitively via `MAPLBase_Mod` (`../base/Base.F90`; it has no module-level `private` and `use`s, among others): `MAPL_ExceptionHandling`, `MAPL_ConstantsMod` (so `MAPL_GRAV`, `MAPL_CP`, ... and the kinds), `MAPL_ShmemMod`, `MAPL_SortMod`, `MAPL_InterpMod`, `MAPL_HashMod`, `MAPL_DirPathMod`, `MAPL_KeywordEnforcerMod`, `MAPL_SimpleCommSplitterMod`, `MAPL_SplitCommunicatorMod`, `MAPL_LoadBalanceMod`, plus the base modules (`MAPL_BaseMod`, `MAPL_CommsMod`, regridder and grid-factory modules, `MAPL_ProfMod`, ...). So `use MAPL` brings in most `MaplShared` members although `MaplShared` itself is not used. The error-handling preprocessor macros (`_RC`, `_VERIFY`, ...) are a separate matter: they come from the `include/` headers (`../include/ARCHITECTURE.md`).

Not brought in: `udunits2f`, the vertical modules, `oomph`.

## Who uses it
- Fixture: `use MAPL` appears in 278 files in the fixture's meta scan; for example `src/Components/@GEOSgcm_GridComp/GEOS_GcmGridComp.F90` and `src/Components/@GEOSgcm_GridComp/GEOSmkiau_GridComp/DFI_GridComp.F90`.
- Inside MAPL: not searched.

## Known traps
- Module name `MAPL` equals the library and CMake project name.
- `MAPL_Profiler` is imported with `initialize`/`finalize` renamed to `initialize_profiler`/`finalize_profiler`; the old `base/MAPL_Profiler.F90` defines the different module `MAPL_ProfMod`.
- A name clash between a component's own entity and anything re-exported is silent until compile; use `only:` lists in new code.

## Observations
- The comment in `MAPL.F90` says it "re-exports the public entities of the underlying packages"; with no `only:` lists the exported set is whatever each package makes public.
- `MAPL.oomph` is a dependency but `MAPL.F90` does not `use oomph`.
