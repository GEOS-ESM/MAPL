# AGENTS.md

Repo-level instructions for coding agents working in this MAPL3 checkout.

## Graph architecture work (`docs/graph/`, `openspec/`)

If your task touches the explicit dependency-graph architecture
(`ComponentGraph`, `GraphBuilder`, `NodeId`, `StateItemNode`,
`HierarchyGraph`, `superstructure/generic/graph/`, or anything under
`openspec/changes/hierarchy-wide-component-graph/`), **read
`docs/graph/ARCHITECTURE.md` first.** It is a verified, up-to-date
navigational summary covering:

- which module owns what, and the hard layering rule
  (`ComponentGraph` must never depend on `OuterComponent`/
  `StateRegistry`)
- the current, just-landed `NodeId` hierarchy-wide-scoping change and
  what it broke/fixed (proxy removal, foreign-node admission)
- the exact state of the active, uncommitted work (Task Group 6 of
  `openspec/changes/hierarchy-wide-component-graph/tasks.md`:
  import-mirroring propagation plus task 6.8, export propagation
  (discovered as a required completion, not optional) — tasks
  6.1-6.6/6.8 implemented and tested, full suite passing with zero
  regressions; 6.7 (explicit follow-up) and Groups 7-10 not yet
  started)
- verified build/test commands and results (NAG and gfortran: both
  confirmed, full suite passing with zero graph-related regressions)
- a correction to the generic `mapl-build`/`mapl-testing` skills' path
  names: this repo uses `superstructure/generic/` and
  `MAPL.generic.*` targets, not `generic3g`/`MAPL.generic3g.*`

Do not re-derive this context from scratch by reading the full
`docs/graph/spec/` tree (20 files) or the full `GraphBuilder.F90` +
11 submodules (6000+ lines) unless `ARCHITECTURE.md` is insufficient
or you suspect it has drifted from the source — it is a summary, not a
replacement for the primary sources it cites, and it explicitly flags
its own `[ASSUMPTION]`s versus directly-verified facts.

**Source of truth, in order of authority, if `ARCHITECTURE.md`
disagrees with any of these:**
1. The actual source files (`superstructure/generic/`,
   `superstructure/generic/graph/`)
2. `openspec/changes/hierarchy-wide-component-graph/{design.md,tasks.md,proposal.md}`
   (the active, in-flight change — current task-completion state)
3. `openspec/specs/graph/*/spec.md` (landed, machine-checkable
   requirement/scenario specs)
4. `docs/graph/spec/*.md` (the narrative design specification — can
   lag implementation; sections needing a `[RE-EVALUATED]` pass are
   named explicitly in `ARCHITECTURE.md` §2.2/§5)
5. `docs/graph/ARCHITECTURE.md` (this summary)

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
`.opencode/skills/compiler-switching` — but for graph-related work
specifically, prefer the verified commands in
`docs/graph/ARCHITECTURE.md` §4, since the generic skills reference a
`generic3g` directory name that does not exist in this checkout.
