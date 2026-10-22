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
- known limitations (no geometry or vertical regrid `Transform`)
- build/test commands and the `MAPL.generic.*` ctest target names
  (this repo uses `superstructure/generic/`, not `generic3g`)

`openspec/changes/` currently holds only `archive/`; there is no active
in-flight change. Planned refactoring is in
`docs/graph/plans/separation-of-concerns.md`. Session narratives are in
`docs/graph/history/`.

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
