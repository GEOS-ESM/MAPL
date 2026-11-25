# MAPL (MAPL2 line) — agent guide

**Audience:** coding agents and developers working on MAPL and on GEOS gridded components.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## What this is
MAPL (Modeling Analysis and Prediction Layer) is "a foundation layer of the GEOS architecture, whose original purpose is to supplement the Earth System Modeling Framework (ESMF)"; it fills in missing ESMF capabilities, provides higher-level interfaces for boiler-plate logic and enforces componentization conventions across GEOS gridded components (`README.md`, first paragraph).

This is the MAPL2 line (`release/v2`). The `develop` branch is MAPL3, a different architecture with its own documents; nothing here describes it.

Parent: GEOSgcm fixture architecture, section 2.3 (`../../../docs/ARCHITECTURE.md` in the fixture). Repository-level map: [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md); decisions: [docs/decisions.md](docs/decisions.md).

## Reading order for a GEOS component developer
1. [generic/](generic/ARCHITECTURE.md): `MAPL_Generic`, specs, children, connections, resources.
2. [include/](include/ARCHITECTURE.md): error-handling macros and `MAPL_GetPointer`.
3. [gridcomps/](gridcomps/ARCHITECTURE.md), then [Cap](gridcomps/Cap/ARCHITECTURE.md): the main program's driver and time loop.
4. [History](gridcomps/History/ARCHITECTURE.md): output collections.
5. [ExtData2G](gridcomps/ExtData2G/ARCHITECTURE.md) (and [ExtData](gridcomps/ExtData/ARCHITECTURE.md)): input from files.

## Directory documents
Component-facing API:
- [generic/](generic/ARCHITECTURE.md): `MAPL_Generic`, specs, connections.
- [include/](include/ARCHITECTURE.md): preprocessor headers (macros).
- [MAPL/](MAPL/ARCHITECTURE.md): the `use MAPL` umbrella module.
- [base/](base/ARCHITECTURE.md): legacy core library.
- [state/](state/ARCHITECTURE.md), [vertical/](vertical/ARCHITECTURE.md), [field_utils/](field_utils/ARCHITECTURE.md): state, vertical-grid and field utilities.

Gridcomps:
- [gridcomps/](gridcomps/ARCHITECTURE.md), [Cap](gridcomps/Cap/ARCHITECTURE.md), [History](gridcomps/History/ARCHITECTURE.md), [ExtData](gridcomps/ExtData/ARCHITECTURE.md), [ExtData2G](gridcomps/ExtData2G/ARCHITECTURE.md), [Orbit](gridcomps/Orbit/ARCHITECTURE.md).

I/O:
- [pfio/](pfio/ARCHITECTURE.md): parallel client-server I/O.
- [griddedio/](griddedio/ARCHITECTURE.md): ESMF fields and bundles to and from pFIO.
- [MAPL_cfio/](MAPL_cfio/ARCHITECTURE.md): legacy CFIO.

Utilities:
- [shared/](shared/ARCHITECTURE.md), [profiler/](profiler/ARCHITECTURE.md), [udunits2f/](udunits2f/ARCHITECTURE.md), [oomph/](oomph/ARCHITECTURE.md), [pflogger_stub/](pflogger_stub/ARCHITECTURE.md).

Tools:
- [Apps/](Apps/ARCHITECTURE.md), [Python/](Python/ARCHITECTURE.md), [Tests/](Tests/ARCHITECTURE.md), [benchmarks/](benchmarks/ARCHITECTURE.md).

## Existing hand-written documents
- [README.md](README.md); [docs/user_guide/](docs/user_guide/README.md); [docs/tutorial/](docs/tutorial/README.md); `docs/Ford/` (documentation-generator inputs).
- [gridcomps/Cap/CapGridComp.md](gridcomps/Cap/CapGridComp.md); [gridcomps/History/HistoryGridComp.md](gridcomps/History/HistoryGridComp.md), [README](gridcomps/History/README.md); [gridcomps/ExtData2G/ExtDataGridComp.md](gridcomps/ExtData2G/ExtDataGridComp.md), [README](gridcomps/ExtData2G/README.md).
- [pfio/pfio.md](pfio/pfio.md); [shared/ErrorHandling.md](shared/ErrorHandling.md); [base/ArthParser.md](base/ArthParser.md); [Apps/README_ACG_WRITER.md](Apps/README_ACG_WRITER.md); [include/README.md](include/README.md).

## Rules for agents
- Note bugs in the relevant document's Observations section; do not fix them unless asked.
- Keep the documents current when you change the code they describe; update the "Verified against" line.
- Existing user guides may disagree with the code (for example the user guide describes automatic connection by name; the code does not connect automatically). The code wins; note the disagreement.
- Cite files and routines, never line numbers.

## Build and test facts
- Top `CMakeLists.txt` options: `USE_EXTDATA2G` (default ON; sets the target `MAPL.ExtData2G` into `gridcomps` and defines `BUILD_WITH_EXTDATA2G`), `BUILD_WITH_FARGPARSE` (default ON; `docs/`, `benchmarks/`, Cap command-line parser, `Tests/` drivers), `BUILD_WITH_PFLOGGER` (default ON), `BUILD_WITH_FLAP` (default OFF), `BUILD_SHARED_MAPL` (default ON), `BUILD_PYTHONBRIDGE` (not defined as an option in MAPL's CMake; only tested in `Python/CMakeLists.txt`, off unless set).
- `MAPL_cfio` is built twice, as `MAPL_cfio_r4` and `MAPL_cfio_r8` ([MAPL_cfio/](MAPL_cfio/ARCHITECTURE.md)).
- Both ExtData generations are compiled when `USE_EXTDATA2G` is ON; the run-time key in `CAP.rc` selects one.
- Unit tests are pFUnit tests beside the code (`<dir>/tests/`), registered with `add_pfunit_ctest` (included when pFUnit is found; `pfunit/` is `EXCLUDE_FROM_ALL`).
- Integration cases are in [Tests/](Tests/ARCHITECTURE.md), registered with `add_test`/`set_tests_properties`. Labels such as `ESSENTIAL` mark the subset the CI dashboard script (`CTestDashboard.cmake`) selects.
