# Generic layer documentation

High-level, implementation-tracking documentation for
`superstructure/generic/` **excluding** `superstructure/generic/graph/`,
which has its own doc set (`docs/graph/`). Written for AI agents making
further changes in the legacy component-hierarchy/coupler layer and the
integration seam that connects it to the graph-native layer — short,
points at real file locations, describes current behavior rather than
design history.

**This supplements, not replaces, the `openspec/changes/archive/`
proposals it cites.** Those remain the primary record of *why* a
decision was made; this doc states current fact and points at them for
rationale. If source and this doc disagree, trust source, then fix the
doc in the same change.

## How to use these docs

1. Start here to see which area you need.
2. Read `ARCHITECTURE.md` — it is one file (not split into per-topic
   files like `docs/graph/`, because this layer is stable/legacy, not
   under active phase-based development — there is less churn to track
   per-topic).
3. If your task touches `superstructure/generic/graph/` or
   `GraphBuilder*.F90` internals, go to `docs/graph/README.md` instead
   — this doc only covers those files' existence and call-site shape
   (`ARCHITECTURE.md` §1.1, §2.4), not their internals.
4. Follow the file/line references into real source for exact
   signatures.

## Topic map (all in `ARCHITECTURE.md`)

| Section | Covers |
|---|---|
| §0 | Scope: what "the generic layer" is, what's excluded (`graph/`, `superstructure/component/`, `infrastructure/`) |
| §1.1 | Top-level entry-point files (`API.F90`, `MAPL_Generic.F90`, `GenericGridComp.F90`, etc.) — includes a note on a stale, uncompiled duplicate `to_itemtype.F90` |
| §1.2 | `OuterMetaComponent`/`OuterMetaComponent/` — the component-hierarchy type and its ~65 lifecycle submodules |
| §1.3 | `registry/` — legacy `StateRegistry`/`ExtensionFamily` |
| §1.4 | `connection/` — `Connection`/`SimpleConnection`/`MatchConnection`/`ReexportConnection` |
| §1.5 | `ComponentSpecParser/` — YAML → `ComponentSpec` |
| §1.6 | `specs/` — `ComponentSpec`/`VariableSpec`/`StateItemSpec`/`StateItemAspect`/`ClassAspect` family (largest subdirectory) |
| §1.7 | `transforms/` — `ExtensionTransform` family (units/typekind/regrid/time-interpolate/normalize), and `couplers/`'s now-empty status |
| §1.8 | `vertical/` — `ModelVerticalGrid`/`FixedLevelsVerticalGrid` (layer-specific only; generic numerics live in `infrastructure/vertical/`) |
| §2.1 | Dependency direction among this file's own subdirectories |
| §2.2 | `StateItemSpec`/`AspectMap` — the legacy coupler's central value type |
| §2.3 | `mapl_GraphMode_mod`'s 3-way selector — who actually produces a component's `ESMF_State` |
| §2.4 | Lifecycle call sites where this layer calls into the graph layer (`initialize_advertise.F90` and others) |
| §2.5 | `superstructure/component/` dependency (`GriddedComponentDriver`) — a sibling, not a subdirectory |
| §3 | `CMakeLists.txt` structure, `esma_add_fortran_submodules` mechanism |
| §4 | Verified build/test commands, the seven `MAPL.generic.*` ctest targets |
| §5 | Previous architectural decisions (standard-name enforcement, field-dictionary strict mode, expression vertical-dim/field-name propagation, extension-registry visibility, internal/memory checkpoint, griddedcomponentdriver integration lifecycle, grid blurring, component-hierarchy foundation) |
| §6 | Completed phases / outstanding work — this layer is stable; the only live "work" is graph-side prerequisites (Phase 6a retirement gate, Phase 7a landed/7b-7c pending) |

## Source layout at a glance

- `superstructure/generic/` top level (~20 `.F90` files) — entry
  points, umbrella modules, and the `GraphBuilder*.F90`/
  `OuterMetaComponent.F90` integration seam (seam internals documented
  in `docs/graph/`, not here).
- `OuterMetaComponent/` (64 files, 3079 lines) — lifecycle submodules.
- `registry/` (19 files, 1638 lines) — legacy `StateRegistry`.
- `connection/` (10 files, 1256 lines) — `Connection` family.
- `ComponentSpecParser/` (9 files, 1034 lines) — YAML parsing.
- `specs/` (44 files, 10,658 lines) — spec/aspect type hierarchy.
- `transforms/` (14 files + `notes.md`, 3211 lines) — data converters
  (absorbed `couplers/`, now empty).
- `vertical/` (2 files, 905 lines) — layer-specific vertical grids.
- `graph/` — **out of scope here**, see `docs/graph/`.

## Conventions seen throughout the source

- Mutation only through named accessor methods, never raw component
  access — same convention as `graph/` (see `docs/graph/README.md`).
- `_ASSERT`/`_RETURN`/`_VERIFY` MAPL error-handling macros
  (`mapl_ErrorHandling_mod`) used uniformly; see
  `.opencode/skills/mapl-error-handling/SKILL.md`.
- Per-aspect connection compatibility is decided one axis at a time
  (`StateItemAspect%can_connect_to`/`matches`), dispatched through an
  `AspectMap`, not by one monolithic comparison — see
  `ARCHITECTURE.md` §2.2 and `specs/StateItemAspect.F90`'s own header
  comment table before changing any aspect's compatibility rule.
- `esma_add_fortran_submodules()` (not plain `target_sources`) is used
  for `OuterMetaComponent/` and `ComponentSpecParser/` specifically to
  avoid same-filename collisions across submodule subdirectories — see
  `ARCHITECTURE.md` §3.
