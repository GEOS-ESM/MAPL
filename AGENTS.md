# MAPL (MAPL3 line) — agent guide

**Audience:** coding agents and developers working on MAPL and on GEOS gridded components.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## What this is
MAPL (Modeling Analysis and Prediction Layer) is "a foundation layer of the GEOS architecture, whose original purpose is to supplement the Earth System Modeling Framework (ESMF)" (`README.md`, first paragraph). This is the MAPL3 line: `develop` has the layered layout (`utils`, `mp_utils`, `pfio`, `infrastructure`, `superstructure`, `gridcomps`; top `CMakeLists.txt`). The directory list in `README.md` still describes the MAPL2 layout. The MAPL2-line documents are on `release/v2`.

Parent: the GEOSgcm architecture document (`../../../docs/ARCHITECTURE.md` in the fixture). Repository map: [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md); decisions: [docs/decisions.md](docs/decisions.md).

## Reading order for a GEOS component developer
1. [generic layer](docs/generic/ARCHITECTURE.md): the generic gridded component, specs, children, connections.
2. [superstructure/](superstructure/ARCHITECTURE.md), then [mapl/](mapl/ARCHITECTURE.md): the `use MAPL` umbrella.
3. [gridcomps/](gridcomps/ARCHITECTURE.md), then [cap](gridcomps/cap/ARCHITECTURE.md): driver and time loop.
4. [history](gridcomps/history/ARCHITECTURE.md), [extdata](gridcomps/extdata/ARCHITECTURE.md): output and input.
5. [infrastructure/](infrastructure/ARCHITECTURE.md), [geom](infrastructure/geom/ARCHITECTURE.md).

## Documents
- Repository: [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md), [docs/decisions.md](docs/decisions.md), [MAPL_Public_API.md](MAPL_Public_API.md).
- Superstructure: [superstructure/ARCHITECTURE.md](superstructure/ARCHITECTURE.md); generic layer: [docs/generic/README.md](docs/generic/README.md), [docs/generic/ARCHITECTURE.md](docs/generic/ARCHITECTURE.md), and the scoped agent guide `superstructure/generic/AGENTS.md`.
- Infrastructure: [infrastructure/ARCHITECTURE.md](infrastructure/ARCHITECTURE.md), [infrastructure/geom/ARCHITECTURE.md](infrastructure/geom/ARCHITECTURE.md).
- Gridcomps: [gridcomps/ARCHITECTURE.md](gridcomps/ARCHITECTURE.md), [cap](gridcomps/cap/ARCHITECTURE.md), [history](gridcomps/history/ARCHITECTURE.md), [extdata](gridcomps/extdata/ARCHITECTURE.md).
- Libraries: [base/](base/ARCHITECTURE.md), [pfio/](pfio/ARCHITECTURE.md), [utils/](utils/ARCHITECTURE.md), [mp_utils/](mp_utils/ARCHITECTURE.md), [mapl/](mapl/ARCHITECTURE.md).
- MAPL3 guides: [diffs-from-mapl2](docs/mapl3/diffs-from-mapl2.md), [api-changes](docs/mapl3/api-changes.md), [gridcomp-template](docs/mapl3/gridcomp-template.md), [vertical-alignment](docs/mapl3/vertical-alignment.md), [field_dictionary_developer](docs/mapl3/field_dictionary_developer.md).
- User guide: [docs/user_guide/README.md](docs/user_guide/README.md).
- Change proposals: `openspec/` (archived changes under `openspec/changes/archive/`, capability specs under `openspec/specs/`).

## Rules for agents
- Note bugs in the relevant document's Observations section; do not fix them unless asked.
- Keep the documents current when you change the code they describe; update the "Verified against" line.
- The code wins over guides; note a disagreement (for example `README.md`'s directory list).
- Cite files and routines, never line numbers.
- Work inside `superstructure/generic/` follows its own `superstructure/generic/AGENTS.md`.
- Markdown links stay inside this repository (CI runs a link checker, `.github/workflows/markup-link-checker.yml`).

## Build and test facts
- CMake project `MAPL` 3.0.0; needs ESMF 9.0.0 or newer (`ESMA_ESMF_MIN_VERSION` in the top `CMakeLists.txt`), gFTL, gFTL-shared, pFlogger, and fArgParse (option `BUILD_WITH_FARGPARSE`, default ON).
- Options: `BUILD_WITH_FARGPARSE` (adds `docs/`, `benchmarks/`, and test frameworks needing it), `BUILD_WITH_PFLOGGER` (default ON; OFF builds `pflogger_stub/`).
- Unit tests are pFUnit tests in `<dir>/tests/`, `EXCLUDE_FROM_ALL`, added when `PFUNIT_FOUND`. Ctest label `ESSENTIAL` marks the subset the dashboard script (`CTestDashboard.cmake`) runs; `COMP_TESTS_BIG` marks large component tests (`tests/MAPL3G_Component_Testing_Framework/CMakeLists.txt`); `PERFORMANCE` marks the `pFIO_tests_*` tests (`pfio/tests/CMakeLists.txt`).
