# MAPL benchmarks

**Audience:** coding agents and developers working on MAPL `benchmarks/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `benchmarks/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
Stand-alone MPI/netCDF I/O benchmarks that mimic GEOS checkpoint and restart behavior. Not part of the model or of ctest.

## Build
- Top `CMakeLists.txt`: `add_subdirectory (benchmarks)` only inside `if (BUILD_WITH_FARGPARSE)` (default ON), together with `docs`.
- `benchmarks/CMakeLists.txt` adds `io/`; `io/CMakeLists.txt` adds `raw_bw`, `gatherv`, `combo`, `checkpoint_simulator`, `restart_simulator`.
- `raw_bw/CMakeLists.txt`: `raw_bw.x` from `BW_Benchmark.F90 BW_BenchmarkSpec.F90 driver.F90`, defines `USE_MPI`, links `MAPL.shared`, MPI, fArgParse, OpenMP, netCDF Fortran.

## Contents (`io/`)
| Directory | Role |
|---|---|
| `raw_bw/` | Raw write bandwidth (also a `run.x` file, not built). |
| `gatherv/` | Times the `MPI_Gatherv` step of current (2023) GEOS checkpointing; equal subdomain sizes (`README.md`). |
| `combo/` | `raw_bw` plus `gatherv`; nominally the default GEOS checkpoint write for one field (`README.md`). |
| `checkpoint_simulator/` | Writes 3D cubed-sphere variables using the strategy of MAPL's checkpoint code; options such as `--config_file`, `--nx` (`README.md`). |
| `restart_simulator/` | Restart-reading counterpart (`README.md`). |

## Who uses it
Developers tuning I/O. *Unverified:* any fixture script or CI that runs them (none found in this directory's CMake).

## Known traps
- Built whenever `BUILD_WITH_FARGPARSE` is ON, so they add to normal build time.
- `raw_bw/BW_Benchmark.F90` and `combo/BW_Benchmark.F90` are separate copies.

## Observations (noted, not fixed)
- No `add_test` in `benchmarks/`.
