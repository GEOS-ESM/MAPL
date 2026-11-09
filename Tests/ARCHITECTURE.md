# MAPL Tests

**Audience:** coding agents and developers working on MAPL `Tests/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `Tests/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
Driver programs and ctest cases that exercise MAPL itself (ExtData, History, Cap, `MAPL_GetHorzIJIndex`, pFIO demos). Unit tests (pFUnit) live beside the code they test, not here.

## Build (`Tests/CMakeLists.txt`)
- Library `MAPL.test_utilities` (always): `ExtDataRoot_GridComp.F90`, `VarspecDescription.F90`.
- Only `if (BUILD_WITH_FARGPARSE)` (option, default ON in the top `CMakeLists.txt`): `ExtDataDriver.x` (`ExtDataDriver.F90`, `ExtDataDriverGridComp.F90`, `ExtDataDriverMod.F90`; defines `BUILD_WITH_EXTDATA2G` when `USE_EXTDATA2G`), `pfio_MAPL_demo.x`, `MAPL_demo_fargparse.x`, `CapDriver.x`, and subdirectories `ExtData_Testing_Framework` (EXCLUDE_FROM_ALL) and `GetHorzIJIndex`.
- `ExtDataDriver.x` and `GetHorzIJIndexDriver.x` are added as dependencies of `build-tests`.

## Contents
| Item | Role |
|---|---|
| `ExtDataDriver.F90` and `ExtDataDriver*.F90` | Cap-like driver that runs repeated ExtData/History/root-component instances from a CAP.rc listing sub-CAP.rc files; see [`ExtDataDriver.md`](ExtDataDriver.md). |
| `generate_extdatadriver_input.py` | Builds ExtDataDriver inputs so ExtData can mimic a GEOS run; [`generate_extdatadriver_input.md`](generate_extdatadriver_input.md). |
| `CapDriver.F90` | Driver using the MAPL Cap. |
| `ExtData_Testing_Framework/` | `test_cases/` directories (per-case `nproc.rc`, rc files) run through ExtDataDriver.x. |
| `GetHorzIJIndex/` | `create_stretched_cs_grid.x`, `GetHorzIJIndexDriver.x` and `run_gethorzijindex.cmake`. |
| `pfio_MAPL_demo.F90`, `MAPL_demo_fargparse.F90` | Demos (no ctest registration found). |

## ctest registration
- `ExtData_Testing_Framework/CMakeLists.txt`: `ExtData2G_<case>` and `ExtData1G_<case>` tests, labels `EXTDATA{1,2}G_{SMALL,SLOW,BIG}_TESTS`; small ones also `ESSENTIAL`. Cases come from `test_cases/extdata_1g_cases.txt` (1G); tests skipped when netCDF lacks Quantize+ZSTD for the listed cases. Process count from the case's `nproc.rc`, default 1.
- `GetHorzIJIndex/CMakeLists.txt`: `GetHorzIJIndex_{NO_OMP,OMP_1_thread,OMP_4_thread}`, labels `THREADING_TESTS;ESSENTIAL`.
- `CapDriver.x`, demos: no `add_test` in `Tests/`.

## Who uses it
- Within MAPL CI via ctest and `make tests` (`build-tests`). *Unverified:* use of `ExtDataDriver.x` by GEOS developers outside CI.

## Known traps
- Nothing here builds when `BUILD_WITH_FARGPARSE` is OFF.
- `ExtData_Testing_Framework` is EXCLUDE_FROM_ALL; its tests need `ExtDataDriver.x` built.

## Observations (noted, not fixed)
- `ExtDataDriver.md` describes ExtDataDriver as covering both ExtData and History; the `Tests/` directory name does not reflect that.
