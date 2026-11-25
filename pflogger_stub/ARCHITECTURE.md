# pflogger_stub

**Audience:** coding agents and developers working on MAPL `pflogger_stub/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `pflogger_stub/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
A minimal replacement for the pFlogger library, so MAPL compiles when
pFlogger is not available.

## Build
Top-level `CMakeLists.txt`:
```
option(BUILD_WITH_PFLOGGER "Build MAPL with pFlogger library support" ON)
...
if (BUILD_WITH_PFLOGGER)
  add_definitions(-DBUILD_WITH_PFLOGGER)
else ()
  add_subdirectory (pflogger_stub)
endif ()
```
- The default (`ON`) finds pFlogger (>= 1.9.5, required) and this directory is **not built**. With `-DBUILD_WITH_PFLOGGER=OFF` the stub is built instead.
- `pflogger_stub/CMakeLists.txt`: library named `pflogger` (`esma_set_this (OVERRIDE pflogger)`), sources `PFL_KeywordEnforcer.F90 WrapArray.F90 pflogger_stub.F90`, dependency `GFTL_SHARED::gftl-shared`, a PUBLIC include directory `${MAPL_SOURCE_DIR}/include`, plus `add_library(PFLOGGER::pflogger ALIAS pflogger)` so the other MAPL libraries' `PFLOGGER::pflogger` dependency resolves to the stub. `recordOptArgs.inc` is an include.
- No `tests/` directory.

## Contents
| File | Role |
|---|---|
| `pflogger_stub.F90` | the stub logger module(s) standing in for pFlogger |
| `PFL_KeywordEnforcer.F90` | keyword-enforcer type used by the stub's interfaces |
| `WrapArray.F90` | array wrapper helper |
| `recordOptArgs.inc` | include for optional-argument recording |

(File purposes inferred from names and the build list; the sources were not read beyond listing.)

## Who uses it
Every MAPL library that lists `PFLOGGER::pflogger` as a dependency (`shared`, `field_utils`, `state`, `vertical`, ...) when the option is `OFF`. GEOS fixture builds use pFlogger itself; nothing in the fixture was found that selects the stub.

## Known traps
- The library is named `pflogger`, the same as the real one; do not install both.
- With the stub, code under `#ifdef BUILD_WITH_PFLOGGER` is not compiled (the definition is absent).

## Observations
- Since the stub is only selected by the option, its coverage of the pFlogger API is whatever MAPL's non-`#ifdef` code needs; not verified.
