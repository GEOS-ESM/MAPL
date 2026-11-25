# profiler

**Audience:** coding agents and developers working on MAPL `profiler/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `profiler/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
Hierarchical timing and memory profiling: nested named meters, per-process
and across-process (distributed) statistics, and column-based text reports.
Every gridded component's timers go through it.

## Build (`profiler/CMakeLists.txt`)
- Library `MAPL.profiler`, `TYPE ${MAPL_LIBRARY_TYPE}`.
- Dependencies `GFTL_SHARED::gftl-shared GFTL::gftl-v1 GFTL::gftl-v2 MAPL.shared MPI::MPI_Fortran`; OpenMP private.
- Present but **not compiled** (not in `srcs`): `DistributedMeterNode.F90` (its module is named `MAP_DistributedMeterNode`).
- `demo/` is added `EXCLUDE_FROM_ALL`; `tests/` (pFUnit: `test_TimeProfiler.pf`, `test_MeterNode.pf`, `test_ProfileReporter.pf`, column and meter tests) is `EXCLUDE_FROM_ALL`.

## Contents
Umbrella module `mapl_Profiler` (`MAPL_Profiler.F90`) re-exports everything below, and also provides the global profilers (`get_global_time_profiler`, `get_global_memory_profiler`) and `initialize`/`finalize`.

| Role | Files |
|---|---|
| Meters (abstract and concrete) | `AbstractMeter`, `AdvancedMeter`, `DistributedMeter`, `AbstractMeterFactory` |
| Gauges (low-level measures) | `AbstractGauge`, `MpiTimerGauge`, `FortranTimerGauge`, `RssMemoryGauge`, `VmstatMemoryGauge` |
| Meter tree | `AbstractMeterNode`, `MeterNode`, `MeterNodeVector`, `MeterNodePtr`, `MeterNodeStack` |
| Profilers | `BaseProfiler`, `TimeProfiler`, `MemoryProfiler`, `DistributedProfiler` |
| Reporting (`reporting/`) | `ProfileReporter` and column classes: name, inclusive, exclusive, percentage, num-cycles, min/mean/max-cycle, std-dev, text/memory formatting, separators, `MultiColumn` |

## Relation to `MAPL_TimerOn` / `MAPL_TimerOff`
In `../generic/MAPL_Generic.F90`, `MAPL_TimerOn`/`MAPL_TimerOff` are generic names for `MAPL_GenericStateClockOn`/`...Off`; the per-component state holds a `type(DistributedProfiler), public :: t_profiler`, created with `DistributedProfiler(trim(name), MpiTimerGauge(), comm=comm)`. Component timers are therefore meters in that profiler. The Cap uses the global time and memory profilers (`../gridcomps/Cap/MAPL_CapGridComp.F90`). See `../generic/ARCHITECTURE.md`.

## Who uses it
- `../generic/MAPL_Generic.F90`, `../pfio/` (`AbstractServer.F90`, `MultiGroupServer.F90`, `ServerThread.F90`, `MpiServer.F90`), `../Tests/ExtDataDriverGridComp.F90`, `../gridcomps/Cap/` (`MAPL_CapGridComp.F90`, `MAPL_NUOPCWrapperMod.F90`).
- Re-exported by `use MAPL` with `initialize`/`finalize` renamed `initialize_profiler`/`finalize_profiler` (`../MAPL/ARCHITECTURE.md`).
- Fixture: components use timers through `MAPL_TimerOn/Off`, not this library directly (no direct use found in a sampled search).

## Known traps
- `../base/MAPL_Profiler.F90` defines a different, older module `MAPL_ProfMod`; do not confuse it with `mapl_Profiler`.
- The meters are timed by `MpiTimerGauge` in `MAPL_Generic`; `FortranTimerGauge` is the alternative (`system_clock`-style) gauge (inferred from names).

## Observations
- `MinCycleColumn.F90` is listed twice in `srcs`; `DistributedMeterNode.F90` declares module `MAP_DistributedMeterNode` (typo) and is not built.
- Depends on both gFTL v1 and v2 targets.
