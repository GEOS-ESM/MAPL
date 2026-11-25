# shared

**Audience:** coding agents and developers working on MAPL `shared/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `shared/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
The bottom layer of MAPL: error handling, constants and small utilities that
every other MAPL library (and most GEOS components) uses. It depends on no
other MAPL library, except the constants library it contains.

Existing document: [`ErrorHandling.md`](ErrorHandling.md) (the error-handling macros; not restated here). The macro definitions live in `../include/` (see [`../include/ARCHITECTURE.md`](../include/ARCHITECTURE.md)).

## Build
- `shared/CMakeLists.txt`: library `MAPL.shared` (`esma_set_this (OVERRIDE MAPL.shared)`), `TYPE ${MAPL_LIBRARY_TYPE}`. Dependencies `MAPL.constants GFTL_SHARED::gftl-shared MPI::MPI_Fortran PFLOGGER::pflogger`; OpenMP private; PUBLIC include directory `../include`.
- `Constants/CMakeLists.txt` (added at the end): separate library `MAPL.constants`, no dependencies. Option `USE_CODATA_2018_CONSTANTS` (default `OFF`) defines `CODATA_2018_CONSTANTS`.
- `Interp/` and `Shmem/` are Fortran submodules (`Interp.F90` + `Interp_implementation.F90`, `Shmem.F90` + `Shmem_implementation.F90`) compiled into `MAPL.shared`.
- C sources: `hash.c`, `sort.c`, `ShaveMantissa.c` (header `ShaveMantissa.h`).
- `SYSTEM_DSO_SUFFIX` is defined from `CMAKE_SHARED_LIBRARY_SUFFIX` (for `DSO_Utilities`).
- Tests: `tests/` (pFUnit; `Test_Partition`, `test_String`, `test_TimeUtils`, `test_DSO_Utilities`, `test_FileSystemUtilities`, `test_MAPL_CF_Time`, `test_MAPL_DateTime_Parsing`, `test_MAPL_ISO8601_DateTime`) when pFUnit is found.

## Contents
Umbrella module `MaplShared` (`MaplShared.F90`) re-exports: `mapl_String`, `mapl_FileSystemUtilities`, `mapl_DSO_Utilities`, `mapl_SplitCommunicatorMod`, `mapl_SimpleCommSplitterMod`, `mapl_SortMod`, `mapl_ShmemMod`, `mapl_ThrowMod`, `mapl_RangeMod`, `mapl_MinMaxMod`, `mapl_LoadBalanceMod`, `mapl_KeywordEnforcerMod`, `mapl_InterpMod`, `mapl_HashMod`, `mapl_ErrorHandlingMod`, `mapl_DirPathMod`, `mapl_Constants`, `mapl_CommGroupDescriptionMod`, `mapl_AbstractCommSplitterMod`, `mapl_DownbitMod`, `mapl_sleepMod`. Not in that list: `MAPL_ExceptionHandling`, `mapl_Partition`, the date/time modules, `MAPL_TimeUtilsMod`, `hinterp`.

| Role | Modules / files |
|---|---|
| Error handling | `MAPL_ExceptionHandling` (umbrella of `MAPL_ThrowMod` and `MAPL_ErrorHandlingMod`); `MAPL_Throw` (`MAPL_throw_exception`, `MAPL_set_throw_method`; the macros in `../include/` expand to calls of these and of the next module); `MAPL_ErrorHandling` (`MAPL_Assert`, `MAPL_Verify`/`MAPL_Vrfy`, `MAPL_Return`/`MAPL_RTRN`, `MAPL_ASRT`, `MAPL_abort`, `MAPL_SUCCESS` and error codes) |
| Constants | `Constants/` (below) |
| Language helpers | `MAPL_KeywordEnforcer` (empty type that forces following arguments to be passed by keyword); `String` (`mapl_String`, a string wrapper type) |
| Algorithms | `MAPL_Sort` (+`sort.c`), `MAPL_Hash` (+`hash.c`: `MAPL_HashCreate/Increment/Destroy/Size/Dump`), `MAPL_Range`, `MAPL_MinMax`, `Partition` (`mapl_GetPartition`: split n items among k bins, for domain decomposition), `MAPL_LoadBalance` (`MAPL_BalanceWork`), `Interp/` (`MAPL_Interp`, linear interpolation in several ranks), `hinterp.F90` (horizontal interpolation between uniform lat-lon grids) |
| Parallel | `AbstractCommSplitter`, `SimpleCommSplitter`, `SplitCommunicator`, `CommGroupDescription` (MPI communicator splitting), `Shmem/` (`MAPL_Shmem`: node-shared memory arrays via C `shmget/shmat/shmdt/shmctl`; `MAPL_AllocNodeArray`) |
| Date and time | `MAPL_ISO8601_DateTime` (ISO 8601-1:2019 strings), `MAPL_DateTime_Parsing`, `MAPL_CF_Time` (CF time strings), `TimeUtils` (validity checks of dates `YYYYMMDD` and times `HHMMSS` "as passed to, say, History") |
| Files and system | `MAPL_DirPath` (search path of directories), `FileSystemUtilities` (basename/extension), `DSO_Utilities` (shared-object extension handling), `MAPL_Sleep` |
| Precision reduction | `DownBit` (`DownBit` keeps `nbits_to_keep` bits), `ShaveMantissa.c` |

### Constants (`Constants/`)
`MAPL_Constants` (file `Constants.F90`) re-exports three modules; `MAPL_ConstantsMod` ("for backwards compatibility") just re-exports `MAPL_Constants`. Main groups:
- `MAPL_InternalConstantsMod`: kinds (`MAPL_R8`, `MAPL_R4`, `MAPL_RN`, `MAPL_I8`, `MAPL_I4`, `MAPL_IN`), undefined markers (`MAPL_UNDEFINED_REAL`, `MAPL_UNDEFINED_INTEGER`, ...), state-item and coupling flags (`MAPL_FieldItem`, `MAPL_BundleItem`, `MAPL_StateItem`, `MAPL_CplSATISFIED`, ...), surface-type codes (`MAPL_Ocean`, `MAPL_Lake`, `MAPL_LandIce`, `MAPL_Land`, `MAPL_Vegetated`), quantization limits (`MAPL_NBITS_*`, `MAPL_QUANTIZE_MAX_*`), file types (`MAPL_FILETYPE_*`).
- `MAPL_MathConstantsMod`: `MAPL_PI`, `MAPL_PI_R8`, `MAPL_DEGREES_TO_RADIANS`, `MAPL_RADIANS_TO_DEGREES`.
- `MAPL_PhysicalConstantsMod`: Earth (`MAPL_GRAV`, `MAPL_RADIUS`, `MAPL_OMEGA`, `MAPL_EARTH_*`, `MAPL_KM_PER_DEG`); gas constants and heat capacities (`MAPL_RUNIV`, `MAPL_RDRY`/`MAPL_RGAS`, `MAPL_CPDRY`/`MAPL_CP`, `MAPL_CVDRY`, `MAPL_RVAP`, `MAPL_CPVAP`, `MAPL_KAPPA`, `MAPL_EPSILON`, `MAPL_VIREPS`); molecular weights (`MAPL_AIRMW`, `MAPL_H2OMW`, `MAPL_O3MW`); latent heats (`MAPL_ALHL`, `MAPL_ALHF`, `MAPL_ALHS`); reference values (`MAPL_P00`, `MAPL_TICE`, `MAPL_CELSIUS_TO_KELVIN`, `MAPL_STFBOL`, `MAPL_AVOGAD`, `MAPL_KARMAN`); densities (`MAPL_RHOWTR`, `MAPL_RHO_SEAWATER`, `MAPL_RHO_SEAICE`, `MAPL_RHO_SNOW`). `MAPL_STFBOL`, `MAPL_AVOGAD`, `MAPL_RUNIV` have CODATA-2018 and older values selected by `USE_CODATA_2018_CONSTANTS`.

## Who uses it
- Everything in MAPL (`MAPL.shared` is the base of the library graph); `../vertical/` uses `MAPL_ConstantsMod, only: MAPL_GRAV, MAPL_AIRMW, MAPL_H2OMW`; `../state/StateMasking.F90` uses `MAPL_Constants`.
- Fixture: `MAPL_Constants`/`MAPL_ConstantsMod` in 117 files, for example `src/Components/@GEOSgcm_GridComp/GEOSmkiau_GridComp/mkiau_specmod.F90` and `src/Components/@GEOSgcm_GridComp/GEOSogcm_GridComp/@GEOS_OceanGridComp/MOM6_GEOSPlug/MOM6_GEOSPlug.F90`. `MAPL_ExceptionHandling` in 11 files, for example `.../GEOSsurface_GridComp/Utils/Raster/makebcs/rasterize.F90` and `.../GEOScatchCN_GridComp/utils/dbg_cnlsm_offline.F90` (stand-alone utilities).
- `use MAPL` does not use `MaplShared`, but `MAPL_ExceptionHandling` and `MAPL_Constants*` (and `MAPL_Shmem`, `MAPL_Sort`, `MAPL_Interp`, `MAPL_Hash`, ...) reach `use MAPL` users through `MAPLBase_Mod`; see `../MAPL/ARCHITECTURE.md`.

## Known traps
- Constants are default-`real` parameters except those marked `_R8`; mixing them into `real(8)` code is silent single precision unless the `_R8` forms are used.
- `MAPL_ConstantsMod` and `MAPL_Constants` are the same entities.
- `MAPL.shared` and `MAPL.constants` are two libraries; the umbrella `MaplShared` re-exports `mapl_Constants`.

## Observations
- `ErrorHandling.md` is the only hand-written document here; the rest is header comments (the `TimeUtils`, `ISO8601` and `Parsing` files carry their format descriptions in comments).
- `CMakeLists.txt` has a commented `#add_subdirectory(tests EXCLUDE_FROM_ALL)` at the end besides the active `PFUNIT_FOUND` one.
- Fixture components mostly get the error macros by `#include "MAPL_Generic.h"`/`MAPL_ErrLog.h` (`../include/ARCHITECTURE.md`), which is why only 11 files `use MAPL_ExceptionHandling` directly (inferred; not traced).
