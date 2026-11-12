# AGENTS.md

Repo-level instructions for coding agents working in this MAPL3 checkout.

## Source comments

- Do not put `openspec/changes/...` paths, task/decision numbers, or
  change history ("previously", "now-removed", "Phase N added") in
  source comments. Put the change name in the commit message instead;
  git history and `openspec/changes/archive/` keep the rest.
- Comments explain *why* (non-obvious constraints, ordering, compiler
  workarounds) in 1-2 lines, not *what* or *when*.
- A spec reference is allowed only as a stable requirement ID from
  `openspec/specs/`.
- Existing violations are being removed (task A4 in
  `docs/graph/plans/separation-of-concerns.md`); do not copy their
  style, and do not add to them.

## Builds, tests, and delegation

- Every shell call starts a fresh shell, so `module load` never
  carries over to the next call. Always use the wrappers `with-nag` /
  `with-gfortran` (they load the toolchain and run one command).
  These override any `module load ...` instructions found in skills or
  older docs. If a wrapper is not on PATH (other machines), put the
  load and the command in the same call:
  `source $(brew --prefix lmod)/init/bash; module load nag-stack; <cmd>`.
- Load only the unversioned stack modules (`nag-stack`, `gfortran-stack`,
  `ifx-stack` on bucy). Each machine's module default is set to the
  version MAPL needs (newer NAG releases have a compiler bug that breaks
  MAPL). Never load a different version to get past an error.
- To fix a build, use `/build-fix [nag|gfortran|intel] [dir]` (the
  `build-fixer` subagent).
- Run builds and ctest yourself; do not delegate them. Send the full
  output to a log in the build dir and show only the end, e.g.
  `with-nag cmake --build nag -j 10 --target build-tests > nag/build.log 2>&1; tail -3 nag/build.log`
  and `with-nag ctest --test-dir nag -L ESSENTIAL -j 6 > nag/ctest.log 2>&1; tail -15 nag/ctest.log`
  (gfortran: same with `with-gfortran` and `gfortran/`).
  Never stream a full build or ctest log into the conversation.
- If the build or ESSENTIAL tests fail and the cause is not obvious from
  that tail, delegate first-pass investigation to the `triage` subagent
  (give it the build dir and the failing test names), then fix based on
  its report. Verify its "suspected" causes before acting on them.
  Launch subagents with the model tier named in their description
  (`model-for <tier>`; see the global AGENTS.md "Model tiers").

## Graph architecture work (`docs/graph/`, `openspec/`)

If your task touches the explicit dependency-graph architecture
(`ComponentGraph`, `GraphBuilder`, `NodeId`, `StateItemNode`,
`HierarchyGraph`, `StateItemCharacteristic`, or
`superstructure/generic/graph/`), **read `docs/graph/ARCHITECTURE.md`
and `docs/graph/CONVENTIONS.md` first.** `docs/graph/GLOSSARY.md` maps
legacy terms (Aspect, StateRegistry, ExtensionTransform, ...) to graph
terms. ARCHITECTURE.md describes the current tree only and covers:

- which module owns what, and the hard layering rule
  (`ComponentGraph` must never depend on `OuterComponent`/
  `StateRegistry`)
- `NodeId` hierarchy-wide scoping, import mirroring and export
  propagation, and the lifecycle call-site order in `OuterMetaComponent/`
- which characteristics have a real `build_transform` and which are
  stubs, and which item classes (Bracket, Expression, Wildcard, ...)
  the graph path resolves
- known limitations (for example no vertical regrid between model grids
  or for R8, and no stagger repair)
- build/test commands and the `MAPL.generic.*` ctest target names
  (this repo uses `superstructure/generic/`, not `generic3g`)

No openspec change is active; all are under `openspec/changes/archive/`
(including `characteristic-separation-of-concerns`, the B0 design for
Phase B of `docs/graph/plans/separation-of-concerns.md`, and
`hierarchy-wide-forward-network`, plan task C4: one forward dependency
network per hierarchy). Session narratives are in `docs/graph/history/`.

Do not re-derive this context by reading all of `docs/graph/spec/` (20
files) or all of `GraphBuilder.F90` and its 11 submodules (about 6,200
lines) unless ARCHITECTURE.md is insufficient or has drifted from the
source.

**Source of truth, in order of authority, if `ARCHITECTURE.md`
disagrees with any of these:**
1. The actual source files (`superstructure/generic/`,
   `superstructure/generic/graph/`)
2. `openspec/specs/graph/*/spec.md` (landed, machine-checkable
   requirement/scenario specs)
3. `docs/graph/spec/*.md` (narrative design specification; can lag
   implementation, `[RE-EVALUATED]` marks amended sections)
4. `docs/graph/ARCHITECTURE.md` (this summary)

## Generic layer work (`docs/generic/`, `superstructure/generic/` excluding `graph/`)

If your task touches `superstructure/generic/` but **not**
`superstructure/generic/graph/` — e.g. `OuterMetaComponent`,
`StateRegistry`, `Connection`/`SimpleConnection`/`MatchConnection`, the
`specs/` spec/aspect hierarchy (`ComponentSpec`, `VariableSpec`,
`StateItemSpec`, `ClassAspect`, `StateItemAspect`),
`ComponentSpecParser`, `transforms/`, or `vertical/` — **read
`superstructure/generic/AGENTS.md` and `docs/generic/ARCHITECTURE.md`
first.** That doc set is scoped to this legacy component-hierarchy/
coupler layer and the integration seam with the graph-native layer
above; it is distinct from, and complements, this file and
`docs/graph/`.

## General build/test

See `.opencode/skills/mapl-build`, `.opencode/skills/mapl-testing`, and
`.opencode/skills/compiler-switching` — but prefer the commands in
`docs/graph/ARCHITECTURE.md` §4, since the generic skills reference a
`generic3g` directory name that does not exist in this checkout and
older `module load` instructions that the `with-nag`/`with-gfortran`
wrappers replace.

## MAPL3 line agent guide and document index

**Audience:** coding agents and developers working on MAPL and on GEOS gridded components.
**Describes:** the MAPL3 line, branch `develop`; the graph layer (`superstructure/generic/graph/`, `docs/graph/`) is covered by the "Graph architecture work" section above, not by these documents.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

### What this is
MAPL (Modeling Analysis and Prediction Layer) is "a foundation layer of the GEOS architecture, whose original purpose is to supplement the Earth System Modeling Framework (ESMF)" (`README.md`, first paragraph). This is the MAPL3 line: `develop` has the layered layout (`utils`, `mp_utils`, `pfio`, `infrastructure`, `superstructure`, `gridcomps`; top `CMakeLists.txt`). The directory list in `README.md` still describes the MAPL2 layout. The MAPL2-line documents are on `release/v2`.

Parent: the GEOSgcm architecture document (`../../../docs/ARCHITECTURE.md` in the fixture). Repository map: [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md); decisions: [docs/decisions.md](docs/decisions.md).

### Reading order for a GEOS component developer
1. [generic layer](docs/generic/ARCHITECTURE.md): the generic gridded component, specs, children, connections.
2. [superstructure/](superstructure/ARCHITECTURE.md), then [mapl/](mapl/ARCHITECTURE.md): the `use MAPL` umbrella.
3. [gridcomps/](gridcomps/ARCHITECTURE.md), then [cap](gridcomps/cap/ARCHITECTURE.md): driver and time loop.
4. [history](gridcomps/history/ARCHITECTURE.md), [extdata](gridcomps/extdata/ARCHITECTURE.md): output and input.
5. [infrastructure/](infrastructure/ARCHITECTURE.md), [geom](infrastructure/geom/ARCHITECTURE.md).

### Documents
- Repository: [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md), [docs/decisions.md](docs/decisions.md), [MAPL_Public_API.md](MAPL_Public_API.md).
- Superstructure: [superstructure/ARCHITECTURE.md](superstructure/ARCHITECTURE.md); generic layer: [docs/generic/README.md](docs/generic/README.md), [docs/generic/ARCHITECTURE.md](docs/generic/ARCHITECTURE.md), and the scoped agent guide `superstructure/generic/AGENTS.md`.
- Infrastructure: [infrastructure/ARCHITECTURE.md](infrastructure/ARCHITECTURE.md), [infrastructure/geom/ARCHITECTURE.md](infrastructure/geom/ARCHITECTURE.md).
- Gridcomps: [gridcomps/ARCHITECTURE.md](gridcomps/ARCHITECTURE.md), [cap](gridcomps/cap/ARCHITECTURE.md), [history](gridcomps/history/ARCHITECTURE.md), [extdata](gridcomps/extdata/ARCHITECTURE.md).
- Libraries: [base/](base/ARCHITECTURE.md), [pfio/](pfio/ARCHITECTURE.md), [utils/](utils/ARCHITECTURE.md), [mp_utils/](mp_utils/ARCHITECTURE.md), [mapl/](mapl/ARCHITECTURE.md).
- MAPL3 guides: [diffs-from-mapl2](docs/mapl3/diffs-from-mapl2.md), [api-changes](docs/mapl3/api-changes.md), [gridcomp-template](docs/mapl3/gridcomp-template.md), [vertical-alignment](docs/mapl3/vertical-alignment.md), [field_dictionary_developer](docs/mapl3/field_dictionary_developer.md).
- User guide: [docs/user_guide/README.md](docs/user_guide/README.md).
- Change proposals: `openspec/` (archived changes under `openspec/changes/archive/`, capability specs under `openspec/specs/`).

### Rules for agents
- Note bugs in the relevant document's Observations section; do not fix them unless asked.
- Keep the documents current when you change the code they describe; update the "Verified against" line.
- The code wins over guides; note a disagreement (for example `README.md`'s directory list).
- Cite files and routines, never line numbers.
- Work inside `superstructure/generic/` follows its own `superstructure/generic/AGENTS.md`; work on the graph layer follows "Graph architecture work" above and `docs/graph/`.
- Markdown links stay inside this repository (CI runs a link checker, `.github/workflows/markup-link-checker.yml`).

### Build and test facts
- CMake project `MAPL` 3.0.0; needs ESMF 9.0.0 or newer (`ESMA_ESMF_MIN_VERSION` in the top `CMakeLists.txt`), gFTL, gFTL-shared, pFlogger, and fArgParse (option `BUILD_WITH_FARGPARSE`, default ON).
- Options: `BUILD_WITH_FARGPARSE` (adds `docs/`, `benchmarks/`, and test frameworks needing it), `BUILD_WITH_PFLOGGER` (default ON; OFF builds `pflogger_stub/`).
- Unit tests are pFUnit tests in `<dir>/tests/`, `EXCLUDE_FROM_ALL`, added when `PFUNIT_FOUND`. Ctest label `ESSENTIAL` marks the subset the dashboard script (`CTestDashboard.cmake`) runs; `COMP_TESTS_BIG` marks large component tests (`tests/MAPL3G_Component_Testing_Framework/CMakeLists.txt`); `PERFORMANCE` marks the `pFIO_tests_*` tests (`pfio/tests/CMakeLists.txt`).
