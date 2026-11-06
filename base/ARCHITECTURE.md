# base/: file I/O, location streams, sun orbit, memory utilities (library `MAPL.base`)

**Audience:** coding agents and developers working on MAPL `base/`.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [../docs/ARCHITECTURE.md](../docs/ARCHITECTURE.md) (repository map; in the fixture, the parent
repository document is `../../../docs/ARCHITECTURE.md` from the repository root). Rules for agents: [../AGENTS.md](../AGENTS.md).

## Purpose
`base/` is what remains of the MAPL2 "legacy core" after the MAPL3 restructuring. It holds file I/O helpers
(`NCIO`, `FileIO`, `FileIOShared`, file-metadata utilities), the tile/location-stream module
(`MAPL_LocStreamMod`), the solar-orbit routines (`SunOrbit`), memory reporting (`MemUtils`), and
`SimpleBundleMod`. The CMake source list says these are the "Geom-dependent files (remaining after mp_utils
split)" (base/CMakeLists.txt). It no longer contains grid factories, regridders, `MAPL_Generic`, MPI
communication wrappers or the resource-file back end.

## Directory and module map
| File | Module | Role |
|---|---|---|
| `API.F90` | `mapl_base_api` | Layer umbrella; re-exports with a `mapl_` prefix (see below). |
| `NCIO.F90` | `mapl_NCIO_mod` | Parallel NetCDF read/write of arrays, states and bundles; generic `VarRead`, `VarWrite`, tiling-file read (`ReadTilingNC4`). Uses `pFIO`. |
| `FileIO.F90` | `mapl_FileIO_mod` | Generic `write_parallel` for strings, scalars and 1-D arrays. |
| `FileIOShared.F90` | `mapl_FileIOShared_mod` | `ArrDescr` and its helpers, `TileMaskGet`, array scatter/gather through shared memory, the other `WRITE_PARALLEL` variants. |
| `FileMetadataUtilities.F90`, `FileMetadataUtilitiesVector.F90` | `mapl_FileMetadataUtils_mod`, `mapl_FileMetadataUtilsVector_mod` | `FileMetaDataUtils`: queries on a pFIO `FileMetadata`. |
| `MAPL_LocStreamMod.F90`, `c_mapl_locstream_F.c` | `mapl_LocStreamMod` | Location streams (tiles): `LocStreamCreate`, `LocStreamTransform`, `LocStreamCreateXform`, `LocStreamGet`, `LocStreamFracArea`, `GridCoordAdjust`. |
| `SunOrbit.F90` | `mapl_Sun_mod` | `MAPL_SunOrbit` type and `MAPL_SunOrbitCreate*`, `MAPL_SunGetInsolation`, `MAPL_SunGetSolarConstant`, daylight-duration and hour-angle routines. |
| `MemUtils.F90` | `mapl_MemUtils_mod` | `MemUtilsInit/Write/Disable/IsDisabled/Free`, `MemCommited`, `MemUsed`, `MemReport`. |
| `SimpleBundleMod.F90` | `mapl_SimpleBundleMod_impl_mod` | `SimpleBundle` type and Create/Print/GetIndex/Destroy. |

## Public interfaces
Components get these names through `use MAPL` (see [../mapl/ARCHITECTURE.md](../mapl/ARCHITECTURE.md)); `mapl/MAPL.F90` uses
`mapl_base_api`. `API.F90` is `private` by default and has an explicit `public` list. It exports the
`MemUtils` names as `mapl_MemUtilsInit`, `mapl_MemUtilsWrite`, `mapl_MemUtilsDisable`, `mapl_MemUtilsIsDisabled`, `mapl_MemUtilsFree`, `mapl_MemCommited`, `mapl_MemUsed`, `mapl_MemReport`; the sun-orbit names under the `MAPL_Sun*` names; `WRITE_PARALLEL`,
`ArrDescr`, `ArrDescrInit`, `ArrDescrSet`; the `SimpleBundle` names; `mapl_VarRead`, `mapl_VarWrite`,
`mapl_NCIOGetFileType`, `mapl_IOGetNonDimVars`, `mapl_IOCountNonDimVars`, `mapl_IOChangeRes`,
`mapl_IOCountLevels`; and `FileMetaDataUtils` (base/API.F90).
Not in the `public` list although imported by `use`: the `mapl_LocStream*` names, `mapl_GridCoordAdjust`,
`mapl_TileMaskGet`, `mapl_MemUtilsModeNode/Full/Base`, `mapl_FileMetadataUtilsVector` (base/API.F90).

## Dependencies
- Library `MAPL.base`, type SHARED. `DEPENDENCIES`: `ESMF::ESMF`, pFlogger, gFTL-shared v2, `MAPL.utils`,
  `MAPL.geom`, `MAPL.esmf`, `MAPL.pfio`, `MAPL.mp_utils`, `MAPL.field_bundle`, `MAPL.constants`, MPI, NetCDF-Fortran
  (base/CMakeLists.txt).
- Used by: `MAPL` (`mapl/CMakeLists.txt`), `MAPL.GeomIO` (`infrastructure/geom_io/CMakeLists.txt`), and the
  restart-simulator benchmark (`benchmarks/io/restart_simulator/CMakeLists.txt`).
- Code in `base/` imports `mapl_Comms_mod`, `mapl_esmf_api`, `mapl_field_api`, `mapl_geom_api`, `mapl_GridAccessors_mod`
  and `mapl_Shmem_mod` (base/NCIO.F90, base/FileIOShared.F90, base/MAPL_LocStreamMod.F90). The first five are in `infrastructure/` (`esmf`, `field`, `geom`); `mapl_Shmem_mod` is in `mp_utils/`.

## Build and tests
- `MAPL_MODE` is a private compile definition. When `DUSTY` is set, `FileIOShared.F90` and `NCIO.F90` get extra compile flags (base/CMakeLists.txt).
- `tests/CMakeLists.txt` defines the pFUnit test `MAPL.base.tests` from `Test_PackedTime.pf` only (`MAX_PES 1`; no `LABELS` set). That test
  uses `mapl_PackedTime_mod`, which is in `mp_utils/`.
- Test directory: `base/tests`. The only test file is `Test_PackedTime.pf`.

## Known traps
- The CMake comments say `PackedTime`, `SimulationTime`, `TimeInterpolation`, `LocalDisplacementEnsemble`, `SatVapor` and
  `StringTemplate` "moved to mp_utils/". `API.F90` repeats this for the string template. Look in `mp_utils/` for them.
- `base/API.F90` does not re-export the `LocStream` routines even though it imports them; they are reachable only by `use mapl_LocStreamMod` directly.

## Comparison with MAPL2 (`origin/release/v2:base/ARCHITECTURE.md`)
- Carries over (names verified in the files above): location streams (`LocStreamCreate`, `LocStreamTransform`), the sun-orbit
  routines (`MAPL_SunGetInsolation`, `MAPL_SunGetSolarConstant`), `MemUtils`, `NCIO` with `VarRead`/`VarWrite`, and `SimpleBundle`.
  Module names changed (for example MAPL2 `MAPL_IOMod`, `MAPL_SunMod`; here `mapl_NCIO_mod`, `mapl_Sun_mod`).
- Does not carry over: on this tree there is no `Base.F90`/`MAPLBase_Mod`, `ESMFL_Mod`, `MAPL_Comms`, `MAPL_Resource`/`MAPL_Config`, `BinIO`, grid
  factories, `ServerManager`, `ApplicationSupport` or `MaplGrid` in `base/` (absent from the directory listing). Regridder code is now
  in `infrastructure/regridder_mgr/` (`RegridderManager.F90`). Where the rest went was not traced for this document.

## Observations (noted, not fixed)
- `base/tests` contains a test for a module that lives in `mp_utils/`.
- `API.F90` contains the comment "StrTemplate moved to mapl_mp_utils_export", but the umbrella in `mp_utils/` is named `mapl_mp_utils_api`.
