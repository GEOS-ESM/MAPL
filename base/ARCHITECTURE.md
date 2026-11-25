# base/ — MAPL legacy core library (`MAPL.base`)

**Audience:** coding agents and developers working on MAPL `base/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `base/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose

`base/` is the oldest part of MAPL (the top-level README calls it the legacy core). It holds the utilities that components and the framework layer (`../generic/`) are built on: grid factories, regridders, location streams (tiles), NetCDF/binary I/O, MPI helpers, time helpers, saturation-vapor-pressure and solar routines, and the resource-file (`MAPL_GetResource`) back end. It does not contain `MAPL_Generic` itself (see `../generic/ARCHITECTURE.md`).

Components normally do not `use` these modules one by one: they `use MAPL` (see `../MAPL/ARCHITECTURE.md`), which re-exports them through `MAPLBase_Mod`.

## Build

Library target `MAPL.base` (`esma_set_this (OVERRIDE MAPL.base)`), built from `CMakeLists.txt`.

- Dependencies: `MAPL.shared`, `MAPL.constants`, `MAPL.profiler`, `MAPL.pfio`, `MAPL_cfio_r4`, `MAPL.field_utils`, `udunits2f`, pFlogger, gFTL / gFTL-shared (v1 and v2), ESMF, NetCDF-Fortran, MPI; OpenMP privately.
- Private compile definitions: `TWO_SIDED_COMM`, `MAPL_MODE`. Public include directory: `../include`.
- `SimulationTime.F90` is added only if `BUILD_WITH_PFLOGGER`.
- Extra target: executable `cub2latlon.x` (from `cub2latlon_regridder.F90`), linked against `MAPL.base`, `MAPL.pfio`.
- `TeX/` is added only if LaTeX is found; `tests/` only if pFUnit is found (and is `EXCLUDE_FROM_ALL`).
- `red_ma.pl` is installed to `bin`.
- C sources compiled in: `regex_F.c`, `MAPL_ObsUtil.c`, `c_mapl_locstream_F.c`, `getrss.c`, `memuse.c`.
- Present but not compiled: `tstqsat.F90` (commented in the list as an "Orphaned program"); the `.H`/`.h` files are includes (below); data and doc files (`orbit.tex`, `orbit.pdf`, `eqsat_verification.dat`, `Sample_ExtData.rc`, `Grid_Use_Cases.txt`, `mapl_tree.py`, `mapl_vlist.py`) are not built.

## Contents

Module names are given where they differ from file names. Role grouping is by file name and header comments.

| Role | Files (module) | Notes |
|---|---|---|
| Umbrella | `Base.F90` (`MAPLBase_Mod`) | Only `use` lines (plus one private flag); no module-level `private`, so everything it uses reaches `use MAPL` users. Uses: `ESMFL_Mod` (comment: "Stopgap"), `MAPL_ExceptionHandling`, `MAPL_BaseMod`, `MAPL_IOMod`, `MAPL_CFIOMod`, `MAPL_CommsMod`, `MAPL_ConstantsMod` (explicitly incl. `MAPL_PI_R8`), `MAPL_ConfigMod`, `MAPL_SortMod`, `MAPL_ProfMod`, `MAPL_SunMod`, `MAPL_LocStreamMod`, `MAPL_InterpMod`, `MAPL_SatVaporMod`, `MAPL_MemUtilsMod`, `MAPL_HashMod`, `MAPL_LoadBalanceMod`, `MAPL_AbstractGridFactoryMod`, the RegridderManager/NewRegridderManager/AbstractRegridder/HorizontalFlux/Transpose/RegridderSpec/RegridderTypeSpec/RegridMethods modules, `MAPL_GridManagerMod`, LatLon/EASE/CubedSphere/External grid-factory modules, `MAPL_ShmemMod`, `MAPL_MaxMinMod`, `MAPL_SimpleBundleMod`, `MAPL_DirPathMod`, `MAPL_KeywordEnforcerMod`, `MAPL_SimpleCommSplitterMod`, `MAPL_SplitCommunicatorMod`, `MAPL_EtaHybridVerticalCoordinateMod` and `MAPL_VerticalDataMod` (base's own, not `../vertical`), `MAPL_ApplicationSupport`, `MAPL_ServerManager`, `MAPL_FileMetadataUtilsMod`, `MAPL_SphericalGeometry`, `MAPL_EASEConversion`, `mapl_LocalDisplacementEnsemble`. Several (Sort, Hash, LoadBalance, Shmem, DirPath, KeywordEnforcer, comm splitters, Constants, ExceptionHandling) live in `../shared/` or elsewhere. |
| Misc base utilities | `Base/Base_Base.F90`, `Base/Base_Base_implementation.F90` (`MAPL_Base`) | "assorted utilities and constants used throughout the MAPL Library" (coupling allocation, field alloc/commit, ...). Submodule-style split into interface and implementation. |
| ESMF helpers / pointer access | `ESMFL_Mod.F90` (`ESMFL_Mod`) | `ESMFL_StateGetPtrToData`, `ESMFL_StateGetPointerToData`, field-array getters, state/bundle regrid, diff, statistics. Generic bodies come from `GetPointer.H`, `GetFieldArray.H` via `overload.macro`. |
| Resources / config | `MAPL_Resource.F90`, `MAPL_Config.F90` (`MAPL_ConfigMod`) | `MAPL_GetResource_config_scalar/_array` are the back ends. The generic name `MAPL_GetResource` is declared in `../generic/MAPL_Generic.F90`, not here. Resource-file pieces: `MAPL_Resource_preamble.h`, `_MakeString.h`, `_SetValue.h`. |
| Communication | `MAPL_Comms.F90` (`MAPL_CommsMod`) | `MAPL_CommsBcast`, `AllReduce{Min,Max,Sum}`, gather/scatter, `MAPL_Am_I_Root`, `MAPL_NPES`, array gather/scatter on layouts. Bodies from `bcast.H`, `allreduce*.H`, `allgather*.H`, `gather.H`, `scatter.H`, `arraygather*.H`, `arrayscatter*.H`, `send.H`, `recv.H`, `sendrecv.H`. |
| Grids | `MAPL_AbstractGridFactory.F90`, `MAPL_GridManager.F90`, `MAPL_LatLon`/`CubedSphere`/`Tripolar`/`EASE`/`Swath`/`XY`/`External`/`Llc` `GridFactory.F90`, `MAPL_EASEConversion.F90`, `MAPL_DefGridName.F90`, `MAPL_GetLatLonCoord.F90`, `MaplGrid.F90`, `MAPL_GridSpec.F90`, `MAPL_GridType.F90`, `MAPL_SphericalGeometry.F90`, `MAPL_EtaHybridVerticalCoordinate.F90`, string/int64 → grid/factory map containers | `MAPL_GridManagerMod` is the registry; factories build ESMF grids from a config prefix. |
| Regridding | `MAPL_AbstractRegridder.F90`; `MAPL_RegridderManager.F90`, `NewRegridderManager.F90`; `MAPL_Conservative`, `Esmf`, `Voting`, `Tiling`, `Fractional`, `Transpose`, `Identity`, `LatLonToLatLon`, `Locstream` `Regridder.F90`; `HorizontalFluxRegridder.F90`; `RegridMethods.F90`; `RegridderSpec*.F90`, `RegridderTypeSpec.F90`, `MAPL_RegridderVector.F90`; `Regrid_Functions_Mod.F90`; `MAPL_VerticalInterpMod.F90`, `MAPL_VerticalMethods.F90` | Described in `regridding_methods.md` (existing). Both the older `MAPL_RegridderManager` and `NewRegridderManager` are compiled. `NewRegridderManager.F90` holds `mapl_NewRegridderManager_private`. |
| Location streams / tiles | `MAPL_LocStreamMod.F90`, `MAPL_LocStreamFactoryMod.F90`, `c_mapl_locstream_F.c` | `MAPL_LocStreamCreate`, `MAPL_LocStreamTransform` (grid↔tile, tile↔tile). Used by Surface-type components. |
| I/O | `NCIO.F90`, `BinIO.F90`, `FileIOShared.F90`, `MAPL_IO.F90`, `MAPL_CFIO.F90`, `CFIOCollection.F90`, `ESMF_CFIOPtrVectorMod.F90`, `FileMetadataUtilities*.F90`, `MAPL_NetCDF.F90`, `Plain_netCDF_Time.F90`, `MAPL_ObsUtil*.{F90,c}` | `MAPL_VarRead`/`MAPL_VarWrite` are generic interfaces in both `NCIO` and `BinIO`. `read_parallel.H`, `write_parallel.H`, `arrayscatter.H`, `arraygather.H` give the typed variants. |
| Time | `SimulationTime.F90`, `MAPL_TimeMethods.F90`, `TimeStringConversion.F90`, `MAPL_ISO8601_DateTime_ESMF.F90`, `MAPL_DateTime_Parsing_ESMF.F90`, `MAPL_SimpleAlarm.F90`, `MAPL_ESMFTimeVectorMod.F90`, `MAPL_NominalOrbitsMod.F90`, `MAPL_sun_uc.F90` (+ `sun.H`) | `MAPL_SunGetInsolation`, `MAPL_SunGetSolarConstant` are interfaces in `MAPL_sun_uc.F90`; `orbit.tex`/`orbit.pdf` document the orbit math. |
| Physics helpers | `MAPL_SatVapor.F90` (`MAPL_SatVaporMod`), `MAPL_MaxMinMod.F90` | Saturation routines (`QSAT0`, ...) get bodies from `qsatice.H`, `qsatlqu.H`, `qsatramp.H`; `eqsat.H`, `eqsatramp.H`, `esatice.H`, `esatlqu.H` are the matching e-sat sources. `MAPL_MaxMin` prints field min/max. |
| Misc | `MAPL_MemUtils.F90` (+ `getrss.c`, `memuse.c`), `StringTemplate.F90`, `regex_module.F90`, `ServerManager.F90`, `ApplicationSupport.F90`, `MAPL_SimpleBundleMod.F90`, `LDE.F90`, `MAPL_Profiler.F90` (`MAPL_ProfMod`), `MAPL_stubs.F90` | `ApplicationSupport` and `ServerManager` support the Cap driver and pFIO server setup. |

### Include files (`.H`, `.h`)

These are preprocessed bodies, not modules. Each is included once per type/rank, after `#define NAME_`/`NAMESTR_` and with `overload.macro` generating the specific names.

| Include | Included by |
|---|---|
| `GetPointer.H`, `GetFieldArray.H` | `ESMFL_Mod.F90` |
| `bcast.H`, `allreduce{min,max,sum}.H`, `allgather.H`, `allgatherv.H`, `gather.H`, `scatter.H`, `send.H`, `recv.H`, `sendrecv.H`, `arraygather.H`, `arraygatherRcvCnt.H`, `arrayscatter.H`, `arrayscatterRcvCnt.H` | `MAPL_Comms.F90` |
| `read_parallel.H`; `arrayscatter.H` (also by `MAPL_Comms.F90`) | `BinIO.F90` |
| `write_parallel.H`; `arraygather.H` (also by `MAPL_Comms.F90`) | `FileIOShared.F90` |
| `qsatice.H`, `qsatlqu.H`, `qsatramp.H` | `MAPL_SatVapor.F90` |
| `eqsat.H` | `qsatice.H`, `qsatlqu.H`; it in turn does `#include FILE_` with `FILE_` defined as `esatice.H` / `esatlqu.H` in those files |
| `eqsatramp.H` | `qsatramp.H` |
| `sun.H` | `MAPL_sun_uc.F90` |
| `MAPL_Resource_MakeString.h`, `MAPL_Resource_SetValue.h` | `MAPL_Resource.F90` |
| `unused_dummy.H` (from `../include`) | many modules |
| `templates/vector.inc` | `CFIOCollection`, `ESMF_CFIOPtrVectorMod`, `FileMetadataUtilitiesVector` |

The `esatice.H`/`esatlqu.H` inclusion goes through the `FILE_` macro, so a plain grep for `#include "esatice.H"` finds nothing.

### `MAPL_GetPointer`

There is no routine of that name in `base/`. `../include/MAPL_Generic.h` defines `MAPL_GetPointer` as a preprocessor macro for `ESMFL_StateGetPointerToData`, whose specifics are expanded in `ESMFL_Mod.F90` from `GetPointer.H`. A component must `#include "MAPL_Generic.h"` to use the name.

### Existing documents

`ArthParser.md` (arithmetic parser, in this directory), `regridding_methods.md`, `Grid_Use_Cases.txt`, `Sample_ExtData.rc`, LaTeX sources in `TeX/` (`MAPL_Intro.tex`, `MAPL_CFIODescr.tex`, `MAPL_ExceptionsDescr.tex`, `HistoryIntro.tex`, `ExtDataIntro.tex`, ...). Not restated here.

## Who uses it

Inside MAPL: `../generic/` (`MAPL_Generic.F90` uses `ESMFL_StateGetPointerToData`, grid and regridder modules), `../gridcomps/` (History, ExtData2G, Cap use regridders, grid factories, NCIO), `../griddedio/`, `../field_utils/`, `../MAPL/` (umbrella).

In the GEOSgcm fixture (`src/Components`):

- Location streams: `@GEOSgcm_GridComp/GEOS_GcmGridComp.F90`, `.../GEOSogcm_GridComp/GEOS_OgcmGridComp.F90` (`MAPL_LocStreamCreate`, `MAPL_LocStreamTransform`).
- NCIO: `.../GEOSmkiau_GridComp/DynVec_GridComp.F90`, `.../@FVdycoreCubed_GridComp/c2c_rst.F90` (`MAPL_VarRead`).
- Comms: `GEOS_GcmGridComp.F90`, `.../@FVdycoreCubed_GridComp/CubeToLatLon.F90` (`MAPL_CommsBcast`).
- Sun: `.../GEOS_OradBioGridComp/GEOS_OradBioGridComp.F90` (`MAPL_SunGetInsolation`).
- MaxMin: `.../@FVdycoreCubed_GridComp/DynCore_GridCompMod.F90`, `.../GEOSturbulence_GridComp/GEOS_TurbulenceGridComp.F90`.
- Pointer access: `GEOS_GcmGridComp.F90`, `.../GEOSmkiau_GridComp/DFI_GridComp.F90` (`MAPL_GetPointer`).

Case-insensitive match counts in the fixture's `src/Components`: `MAPL_LocStreamTransform` 445, `MAPL_VarRead` + `MAPL_VarWrite` 772, `MAPL_SunGetInsolation` 46, `MAPL_MaxMin` 198 (includes `MAPL_MaxMinMod` uses).

## Known traps

- `MAPL_GetPointer` is a macro (needs `MAPL_Generic.h`); `MAPL_GetResource` is declared in `generic/`, not here.
- Type-generic names (`MAPL_VarRead`, `MAPL_CommsBcast`, `QSAT0`, ...) are produced by include files and `overload.macro`; they do not appear in the source as written, so grepping for a specific variant fails.
- `MAPL_VarRead` is defined in both `NCIO` and `BinIO` (separate interfaces).
- Two regridder managers coexist (`MAPL_RegridderManager`, `NewRegridderManager`).
- `-DTWO_SIDED_COMM` is set for the library; the one-sided variant is not selected by this build.
- `MAPL_stubs.F90` is compiled in the library.
- `tstqsat.F90` is not compiled and must not be treated as a library test; real tests are in `tests/`.

## Observations

- `MAPLBase_Mod` carries the comment "Stopgap" on `use ESMFL_Mod`.
- The CMake list contains `ESMF_CFIOPtrVectorMod.F90` and `CFIOCollection.F90`/`MAPL_CFIO.F90` twice each; the duplicate entries are harmless to CMake de-duplication but unusual. Neither was tested.
- Both `MAPL_Profiler.F90` (module `MAPL_ProfMod`) and `../profiler/` exist; the relationship was not examined.
- `tests/CMakeLists.txt` builds `add_pfunit_ctest(MAPL.base.tests ...)` from 13 active `.pf` tests plus `MockGridFactory`/`MockRegridder` (`MAX_PES 8`, label `ESSENTIAL`), and `mapl_bundleio_test.x` with ctests `bundleio_tests_latlon` and `bundleio_tests_cube`. `testbin.F90`, `utCFIO_*.F90` and `ut_ExtData.F90` are not wired in.
