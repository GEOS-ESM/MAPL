# mp_utils/: MPI-dependent utilities, shared memory, profiler (library `MAPL.mp_utils`)

**Audience:** coding agents and developers working on MAPL `mp_utils/`.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [../docs/ARCHITECTURE.md](../docs/ARCHITECTURE.md) (repository map; in the fixture, the parent
repository document is `../../../docs/ARCHITECTURE.md` from the repository root). Rules for agents: [../AGENTS.md](../AGENTS.md).

## Purpose
`mp_utils/` is the second tier above `utils/`: MPI-dependent code, plus the ESMF-dependent time and saturation-vapor-pressure helpers
that used to be in the legacy core. The CMake comment: "MPI-dependent utilities (Tier 2 in MAPL v3 restructuring). Depends on MAPL.utils + MPI.
ESMF-dependent files absorbed from base3g/" (mp_utils/CMakeLists.txt). It also holds the profiler that was a separate library in earlier versions
("Profiler (absorbed from profiler/)").

## Directory and module map
| Group | Files | Role |
|---|---|---|
| Umbrella | `API.F90` (`mapl_mp_utils_api`) | Re-exports the `MAPL_`-prefixed names. |
| MPI errors | `MAPL_MpiErrorHandling.F90` (`mapl_MpiErrorHandling_mod`) | MPI error-code handling (the MPI-free handlers are in `utils/`). |
| Reductions | `ArrayReductions.F90` | `AreaMean`, `MaxMin` (exported as `MAPL_AreaMean`, `MAPL_MaxMin`). |
| Compression | `DownBit.F90` | Mantissa reduction with an MPI-dependent reduction (per CMake comment). |
| Communicator splitting | `AbstractCommSplitter.F90`, `CommGroupDescription.F90`, `SplitCommunicator.F90`, `SimpleCommSplitter.F90` | Splitting `MPI_COMM_WORLD`-style communicators into groups. |
| Load balancing | `MAPL_LoadBalancer.F90`, `Partition.F90` | `LoadBalancerCreate/Run/Get/Destroy`; the distribute/retrieve constants. Old `MAPL_LoadBalance*` names are kept as aliases ("Temporary pre-LoadBalancer aliases", mp_utils/API.F90). |
| Shared memory | `Shmem/Shmem.F90` (module `mapl_Shmem_mod`, interfaces), `Shmem/Shmem_implementation.F90` | Node-level shared arrays: `InitializeShmem`, `FinalizeShmem`, `AllocNodeArray`, `GetSharedMemory`, `ReleaseSharedMemory`, `BroadcastToNodes`, `GetNodeInfo`, node communicators. |
| Time | `PackedTime.F90`, `SimulationTime.F90`, `TimeInterpolation.F90`, `LocalDisplacementEnsemble.F90` | Packed date/time, simulation-time dictionary, time interpolation. |
| Physics helpers | `SatVapor.F90` with `qsatice.H`, `qsatlqu.H`, `qsatramp.H`, `eqsat.H`, `eqsatramp.H`, `esatice.H`, `esatlqu.H` | Saturation vapor pressure/humidity; the `.H` files are included bodies. |
| Strings | `StringTemplate.F90` | `StrTemplate`, `fill_grads_template`, `fill_grads_template_esmf`. |
| Profiler | `profiler/*.F90` (meters, gauges, profilers), `profiler/reporting/*.F90` (columns and reporters), `profiler/profiler_report_config.yaml`, `profiler/examples/`, `profiler/tests/` | See below. |

### Profiler
- Module `mapl_Profiler_mod` (`profiler/MAPL_Profiler.F90`) is what `mapl/MAPL.F90` uses, with the renames `profiler_initialize => initialize`,
  `profiler_finalize => finalize`.
- Class families (by file name): meters (`AbstractMeter`, `AdvancedMeter`, `DistributedMeter`, `MeterNode`, `MeterNodeStack`, ...), gauges
  (`FortranTimerGauge`, `MpiTimerGauge`, `MallocGauge`, `RssMemoryGauge`, `VmstatMemoryGauge`, `NullGauge`), profilers (`BaseProfiler`, `TimeProfiler`,
  `MemoryProfiler`, `DistributedProfiler`, `StubProfiler`), reporting (`ProfileReporter`, `CsvProfileReporter`, and a family of `*Column` types).
- `GlobalProfilers.F90` provides `get_global_time_profiler`, `get_global_memory_profiler`, `initialize_global_time_profiler`,
  `initialize_global_memory_profiler`. `MaplFramework` calls `profiler_initialize` with both global profilers enabled (`initialize_profilers`, mapl/MaplFramework.F90).
- `profiler_report_config.yaml` is described in its own header as an example configuration for the columns of a report.

## Public interfaces
`mapl_mp_utils_api` is `private` with an explicit `public` list: `MAPL_MaxMin`, `MAPL_AreaMean`, the packed-time routines
(`MAPL_PackedDateCreate`, `MAPL_PackedTimeCreate`, `MAPL_PackedDateTimeCreate`, `MAPL_ESMFTimeFromPacked`, `MAPL_Unpack*`), `mapl_StrTemplate` and
the GrADS template routines, the Shmem routines (`MAPL_InitializeShmem`, `MAPL_AllocNodeArray`, `MAPL_AllocateShared`, `MAPL_BroadcastToNodes`, `MAPL_NodeComm`, ...),
and `MAPL_LoadBalancer*` plus the old aliases. Other modules (`SatVapor`, comm splitters, `Partition`, `SimulationTime`) are used by direct module name.

## Dependencies
- `DEPENDENCIES`: `MAPL.utils`, `MAPL.constants`, MPI, ESMF, gFTL-shared, gFTL v1 and v2, pFlogger; OpenMP private (mp_utils/CMakeLists.txt).
- Used by (CMake): `MAPL.utils` tests, `MAPL.pfio`, `MAPL.base`, `MAPL` (`mapl/`), `infrastructure/esmf`, `infrastructure/geom`.

## Build and tests
- `MAPL_MODE` is a private compile definition. When `DUSTY` is set, `Shmem/Shmem.F90`, `Shmem/Shmem_implementation.F90` and `MAPL_LoadBalancer.F90` get extra compile flags (comment: relaxed flags for MPI interfaces).
- Profiler examples (`profiler/examples/CMakeLists.txt`): executables `profiler_demo.x`, `mpi_demo.x`, `hybrid_demo.x`, `csv_demo.x`; ctest tests
  `Profiler_Demo_Basic`, `Profiler_Demo_MPI`, `Profiler_Demo_Hybrid`, labelled `ESSENTIAL`.
- Profiler unit tests (`profiler/tests/`, only when `PFUNIT_FOUND`, `EXCLUDE_FROM_ALL`): pFUnit target `MAPL.profiler.tests` (`MAX_PES 8`, label `ESSENTIAL`).
- `mp_utils/` has no top-level `tests/` directory. The `ArrayReductions` and `Partition` tests are in `utils/tests` (see [../utils/ARCHITECTURE.md](../utils/ARCHITECTURE.md)); `Test_PackedTime.pf` is in `base/tests`.

## Known traps
- Several modules that other documents or MAPL2 place in `base/` are here: `PackedTime`, `SimulationTime`, `TimeInterpolation`, `LocalDisplacementEnsemble`, `SatVapor`, `StringTemplate`.
- `mapl_mp_utils_api` exports a subset; `SatVapor`, `SimulationTime`, `TimeInterpolation`, comm splitters and `Partition` are not in its `public` list.

## Observations (noted, not fixed)
- `utils/API.F90` exports `mapl_UnpackDate` (from `TimeUtilities`) and `mp_utils/API.F90` exports `MAPL_UnpackDate` (from `PackedTime`). Fortran names are case-insensitive, so both public names are the same identifier, and both umbrellas are used by `mapl/MAPL.F90`. *Inferred:* the two entities then conflict if a user references the name; not compiled here.
- The profiler tests set `MODULE_DIRECTORY` under `MAPL.profiler/tests` and include `${esma_include}/MAPL.profiler` and `MAPL.shared`, directory names that no longer match a target in this tree (mp_utils/profiler/tests/CMakeLists.txt).
