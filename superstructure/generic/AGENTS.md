# AGENTS.md — `superstructure/generic/` (non-graph)

Scoped instructions for coding agents working anywhere under
`superstructure/generic/` **except** `superstructure/generic/graph/`.
This file is distinct from the repo-root `AGENTS.md` and from
`docs/graph/`'s own guidance — see the routing rule immediately below.

## Routing: which doc set applies?

- If your task touches `superstructure/generic/graph/`,
  `ComponentGraph`, `GraphBuilder`, `NodeId`, `StateItemNode`,
  `HierarchyGraph`, `StateItemCharacteristic`, or anything under
  `openspec/changes/.../hierarchy-wide-component-graph/` — **stop, and
  read the repo-root `AGENTS.md`'s "Graph architecture work" section
  and `docs/graph/ARCHITECTURE.md` instead.** That is a separately
  maintained, actively-evolving doc set; do not use this file for that
  work.
- Otherwise — for `OuterMetaComponent`, `StateRegistry`, `Connection`/
  `SimpleConnection`/`MatchConnection`, the `specs/` spec/aspect
  hierarchy (`ComponentSpec`, `VariableSpec`, `StateItemSpec`,
  `ClassAspect`, `StateItemAspect`), `ComponentSpecParser`,
  `transforms/` (`ExtensionTransform` family), or `vertical/`
  (`ModelVerticalGrid`/`FixedLevelsVerticalGrid`) — **read
  `docs/generic/ARCHITECTURE.md` first.** It is a navigational summary
  covering:
  - the module map for every file/subdirectory in this tree (module
    map, §1)
  - how this layer and the graph-native layer coexist today
    (`mapl_GraphMode_mod`'s 3-way selector, the `initialize_advertise.F90`
    call-site ordering, §2.2-§2.4)
  - verified build/test commands and the seven `MAPL.generic.*` ctest
    targets this tree's tests build into (§4)
  - previous architectural decisions, each cited back to its
    `openspec/changes/archive/` source (§5)
  - what is and is not scheduled for change (this layer is stable —
    see §6's prerequisite-gated retirement note before assuming any
    part of `registry/`/`connection/`/`specs/` is deprecated)

Do not re-derive this context from scratch by reading all ~34,000
lines across this tree's ~140 files unless `docs/generic/
ARCHITECTURE.md` is insufficient or you suspect it has drifted from
source — it is a summary, not a replacement for the primary sources it
cites, and it explicitly flags its own `[ASSUMPTION]`s and `[NOTE]`s
versus directly-verified facts.

**Source of truth, in order of authority, if `docs/generic/
ARCHITECTURE.md` disagrees with any of these:**
1. The actual source files (`superstructure/generic/*`, excluding
   `graph/`)
2. `openspec/changes/archive/*/{design.md,proposal.md}` for the
   specific decision in question (§5 of `ARCHITECTURE.md` has the
   index)
3. `openspec/specs/generic/*/spec.md`, `openspec/specs/
   internal-checkpoint/spec.md` (landed, machine-checkable
   requirement/scenario specs)
4. `docs/generic/ARCHITECTURE.md` / `docs/generic/README.md` (this
   summary)

## General build/test

See `.opencode/skills/mapl-build`, `.opencode/skills/mapl-testing`, and
`.opencode/skills/compiler-switching` — but note the same correction
`docs/graph/ARCHITECTURE.md` §4.4 already records: this repo uses
`superstructure/generic/` and `MAPL.generic.*` ctest target names, not
`generic3g`/`MAPL.generic3g.*`. `docs/generic/ARCHITECTURE.md` §4.2
lists the seven real target names and what each one tests.
