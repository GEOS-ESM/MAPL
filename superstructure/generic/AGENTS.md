# AGENTS.md: `superstructure/generic/`

**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Scoped instructions for coding agents working anywhere under
`superstructure/generic/`. This file is distinct from the repository-root
[`AGENTS.md`](../../AGENTS.md), which applies as well.

## Where to start

Read [`docs/generic/ARCHITECTURE.md`](../../docs/generic/ARCHITECTURE.md)
first when working on `OuterMetaComponent`, `StateRegistry`,
`Connection`/`SimpleConnection`/`MatchConnection`, the `specs/`
spec/aspect hierarchy (`ComponentSpec`, `VariableSpec`, `StateItemSpec`,
`ClassAspect`, `StateItemAspect`), `ComponentSpecParser`, `transforms/`
(`ExtensionTransform` family), or `vertical/`
(`ModelVerticalGrid`/`FixedLevelsVerticalGrid`). It is a navigational
summary covering:

- the module map for every file and subdirectory in this tree (§1)
- dependencies among the subdirectories, and the initialize call order
  (§2)
- `CMakeLists.txt` structure (§3)
- the six `MAPL.generic.*` ctest suites this tree's tests build into (§4)
- previous architectural decisions, each cited back to its
  `openspec/changes/archive/` source (§5)

The component-graph layer (`superstructure/generic/graph/`) is not on
this line. If a document or issue refers to `GraphBuilder`,
`ComponentGraph` or `GRAPH_MODE_*`, that code is not in this tree.

This context is in `docs/generic/ARCHITECTURE.md`. It is a summary, not a
replacement for the primary sources it cites. The index to the whole document set is
[`docs/generic/README.md`](../../docs/generic/README.md).

**Source of truth, in order of authority, if the architecture document
disagrees with any of these:**

1. The actual source files under `superstructure/generic/`.
2. `openspec/changes/archive/*/{design.md,proposal.md}` for the specific
   decision in question (§5 of `ARCHITECTURE.md` has the index).
3. `openspec/specs/generic/*/spec.md` and
   `openspec/specs/internal-checkpoint/spec.md` (landed requirement and
   scenario specs).
4. `docs/generic/ARCHITECTURE.md` and `docs/generic/README.md`.

## Rules

- Note bugs you find; do not fix them unless asked.
- Keep `docs/generic/ARCHITECTURE.md` current in the same change that
  alters the structure it describes.

## Build and test facts

- Library target: `MAPL.generic`. Test targets are `MAPL.generic.*`
  all labelled `ESSENTIAL`;
  `docs/generic/ARCHITECTURE.md` §4 lists the six suites and what each
  covers. Tests are built by the `build-tests` target.
