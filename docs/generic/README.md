# Generic layer documentation

**Audience:** coding agents and developers working on `superstructure/generic/`.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

High-level, implementation-tracking documentation for
`superstructure/generic/` (CMake target `MAPL.generic`). Written for AI
agents making changes in the component-hierarchy and coupler layer: short,
points at real file locations, describes current behavior rather than
design history. The component-graph layer is not on this line, so none of
these documents describe it.

**This supplements, not replaces, the `openspec/changes/archive/`
proposals it cites.** Those remain the primary record of *why* a
decision was made; this document states current fact and points at them
for rationale. If source and this document disagree, trust source, then
fix the document in the same change.

## How to use these docs

1. Start here to see which area you need.
2. Read [`ARCHITECTURE.md`](ARCHITECTURE.md). It is one file, not split
   per topic.
3. Scoped agent instructions for the directory are in
   [`superstructure/generic/AGENTS.md`](../../superstructure/generic/AGENTS.md);
   the repository-wide ones are in the root [`AGENTS.md`](../../AGENTS.md).
4. Follow the file references into real source for exact signatures.

## Topic map (all in `ARCHITECTURE.md`)

| Section | Covers |
|---|---|
| §0 | Scope: what "the generic layer" is, what is excluded (`superstructure/component/`, `infrastructure/`) |
| §1.1 | Top-level entry-point files (`API.F90`, `MAPL_Generic.F90`, `GenericGridComp.F90`, etc.), including a stale, uncompiled duplicate `to_itemtype.F90` and an uncompiled `Generic.F90` |
| §1.2 | `OuterMetaComponent`/`OuterMetaComponent/`: the component-hierarchy type and its lifecycle submodules |
| §1.3 | `registry/`: `StateRegistry`/`ExtensionFamily` |
| §1.4 | `connection/`: `Connection`/`SimpleConnection`/`MatchConnection`/`ReexportConnection` |
| §1.5 | `ComponentSpecParser/`: YAML to `ComponentSpec` |
| §1.6 | `specs/`: `ComponentSpec`/`VariableSpec`/`StateItemSpec`/`StateItemAspect`/`ClassAspect` family (largest subdirectory) |
| §1.7 | `transforms/`: `ExtensionTransform` family; `couplers/` is empty |
| §1.8 | `vertical/`: `ModelVerticalGrid`/`FixedLevelsVerticalGrid` (layer-specific only; generic numerics live in `infrastructure/vertical/`) |
| §2.1 | Dependency direction among this layer's own subdirectories |
| §2.2 | `StateItemSpec`/`AspectMap`: the coupler's central value type |
| §2.3 | Where the generic layer sits in the initialize sequence (`initialize_advertise.F90`) |
| §2.4 | `superstructure/component/` dependency (`GriddedComponentDriver`) |
| §3 | `CMakeLists.txt` structure, `esma_add_fortran_submodules` mechanism |
| §4 | Test targets: the six `MAPL.generic.*` ctest suites |
| §5 | Previous architectural decisions, each cited to its `openspec/` record |

## Source layout at a glance

Counts are of `.F90` files and their lines on `develop` @ `1a5a5acf`.

- `superstructure/generic/` top level (16 `.F90` files, 4016 lines):
  entry points, umbrella modules, and the `OuterMetaComponent.F90` type.
- `OuterMetaComponent/` (54 files, 2582 lines): lifecycle submodules.
- `registry/` (20 files, 1638 lines): `StateRegistry`.
- `connection/` (10 files, 1256 lines): `Connection` family.
- `ComponentSpecParser/` (9 files, 1034 lines): YAML parsing.
- `specs/` (40 `.F90` files incl. `GeomAspect/` and `VerticalGridAspect/`,
  10,740 lines): spec and aspect type hierarchy.
- `transforms/` (15 `.F90` files plus `notes.md`, 3211 lines): data
  converters (`couplers/` has no sources; its files are in `transforms/`).
- `vertical/` (2 files, 905 lines): layer-specific vertical grids.
- `tests/`: six pFUnit suites plus scenario fixtures (§4).

## Conventions seen throughout the source

- `_ASSERT`/`_RETURN`/`_VERIFY` MAPL error-handling macros
  (`mapl_ErrorHandling_mod`) are used uniformly.
- Per-aspect connection compatibility is decided one axis at a time
  (`StateItemAspect%can_connect_to`/`matches`), dispatched through an
  `AspectMap`, not by one monolithic comparison. See `ARCHITECTURE.md`
  §2.2 and the header comment table in `specs/StateItemAspect.F90`.
- `esma_add_fortran_submodules()` (not plain `target_sources`) is used
  for `OuterMetaComponent/` and `ComponentSpecParser/`; see
  `ARCHITECTURE.md` §3.
