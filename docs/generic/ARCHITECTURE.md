# MAPL3 Generic Layer Architecture — Implementation Reference

**Audience:** coding agents and developers working in
`superstructure/generic/` **outside** `superstructure/generic/graph/`.
That subdirectory (the explicit dependency-graph architecture) has its
own, separately maintained documentation set — see
`docs/graph/README.md` / `docs/graph/ARCHITECTURE.md` — and is treated
here only as a dependency this layer calls into (§2.4), never
re-described.

**Relationship to other docs:**
- `docs/graph/` — the graph-native layer's own docs. Read those for
  `ComponentGraph`/`GraphBuilder`/`NodeId`/`StateItemNode` work.
- `openspec/specs/generic/*/spec.md` — machine-checkable
  requirement/scenario specs for landed, generic-layer-specific
  capabilities (field-name-propagation, standard-name-enforcement,
  expression-vertical-dim-resolution). Treat as authoritative for the
  behavior they cover.
- `openspec/specs/internal-checkpoint/spec.md` — landed checkpoint
  (restart) requirements.
- `openspec/changes/archive/` — landed, folded-in proposals. Each
  directory's `proposal.md`/`design.md` is the primary record of *why*
  a piece of this layer looks the way it does; this file cites them
  rather than restating their reasoning.
- **This file** — a navigational layer on top of the above, written for
  quick orientation without re-reading ~34,000 lines of source across
  ~140 files. Where this file and the sources it cites disagree, the
  sources win — this file may lag.

All facts below were verified by reading the actual source tree,
`CMakeLists.txt` files, and `git log`/`git show` output on 2026-10-20,
unless marked **[ASSUMPTION]**. This pass did not re-run the build or
test suite; §4's commands are carried over, lightly corrected, from
`docs/graph/ARCHITECTURE.md` §4 (verified there on 2026-10-16/17/19) —
treat them as a known-working recipe, not a fresh verification.

## 0. What is "the generic layer," and what is out of scope here

`superstructure/generic/` (CMake target `MAPL.generic`) is the single
library that implements:
- the user-facing `GenericSetServices`/`GenericGridComp` entry points
  every MAPL gridded component goes through,
- the private per-component state type (`OuterMetaComponent`) and its
  ~65 lifecycle-phase submodules,
- the YAML-driven `ComponentSpec` description of a component's
  variables, children, and connections, and its parser,
- the legacy imperative `StateRegistry`/`Connection`/`ExtensionFamily`
  coupler machinery that resolves those connections and builds
  automatic unit/typekind/regrid couplers,
- the `ClassAspect`/`StateItemAspect` family of per-variable metadata
  descriptors the legacy machinery dispatches on,
- the `Transform` (legacy `ExtensionTransform`) family of concrete data
  converters (units, typekind, regrid, time-interpolate, normalize,
  conservative-regrid),
- vertical-grid support (`ModelVerticalGrid`, `FixedLevelsVerticalGrid`),
- and, as of the in-flight `hierarchy-wide-component-graph` work, the
  **integration points** (`GraphBuilder*.F90`) that let a parallel,
  explicit-dependency-graph implementation (`superstructure/generic/
  graph/`) run alongside or in place of the legacy path, selected by
  `mapl_GraphMode_mod`'s 3-way switch.

**Out of scope for this file:** everything under
`superstructure/generic/graph/` itself (graph-neutral core:
`GraphNode`/`ComponentGraph`/`DependencyNetwork`/`StateItemCharacteristic`/
`Transform` (graph-native) /callbacks/route handles — ~70 modules,
~12,000 lines). Fully documented in `docs/graph/`. This file documents
only the *rest* of `superstructure/generic/`, plus the handful of
integration files (`GraphBuilder*.F90`, `OuterMetaComponent*`) that sit
at the seam between the two and are needed to understand how legacy
code calls into the graph layer — those files are also covered by
`docs/graph/ARCHITECTURE.md` §1.2 in more depth; this file only notes
the call-site shape (§2.4), not their internals.

**Also out of scope / noted but not detailed:** `superstructure/
component/` (sibling directory: `GriddedComponentDriver`/
`ComponentDriver` — the generic layer depends on it, not the reverse;
see §2.5) and `infrastructure/` (e.g. `infrastructure/vertical/
vertical_grid/` which owns `VerticalGrid`/`VerticalGridSpec`/
`VerticalGridFactory`, consumed but not defined by `superstructure/
generic/vertical/`).

## 1. Module map

### 1.1 Top-level integration/entry-point files (`superstructure/generic/*.F90`)

| File | Module | Lines | Responsibility |
|---|---|---|---|
| `API.F90` | `mapl_generic_api` | 124 | Umbrella `use`-and-re-`public` module; the one file most external code should `use` instead of reaching into individual generic-layer modules directly. |
| `Generic.F90` | `mapl_Generic` | 14 | A second, much smaller umbrella (re-exports a handful of symbols: `OuterMetaComponent`, `MAPL_GridCompCreate`, `VerticalGrid`/`ModelVerticalGrid`, `ChildSpec`, `UserSetServices`, OpenMP support). **[ASSUMPTION]** Not itself referenced by any other `CMakeLists.txt`/`.F90` file found by a repo-wide grep in this pass; likely a legacy or convenience alias module. Verify before assuming it is dead — not confirmed unreachable, only confirmed *unused by anything this pass searched*. |
| `MAPL_Generic.F90` | `mapl_Generic_mod` | 1332 | The real, long-standing "generic" facade: thin wrapper procedures (`MAPL_AddImportSpec`, `MAPL_AddExportSpec`, etc.) intended to be backward-compatible with MAPL2-era call sites, delegating to `OuterMetaComponent`/`InnerMetaComponent` methods. |
| `GenericGrid.F90` | `mapl3_GenericGrid` | 14 | Tiny re-export shim. |
| `GenericGridComp.F90` | `mapl_GenericGridComp_mod` | 390 | `GenericSetServices`/`GridCompCreate` — the actual ESMF ally wiring. Declares the generic init phases (`MAPL_PROPAGATE_GRID`, `MAPL_ADVERTISE`, `MAPL_REALIZE`) that a user gridcomp's own phases can be appended to with `_PRE`/`_POST`. |
| `ComponentSpecParser.F90` + `ComponentSpecParser/` (9 files, 1034 lines) | `mapl_ComponentSpecParser_mod` | 146 (+ submodules) | Parses a component's YAML `ComponentSpec` (children, connections, variable specs, geometry spec, set-services, timespec) into the `specs/` type hierarchy. See §1.5. |
| `to_itemtype.F90` | *(submodule of `mapl_ComponentSpecParser_mod`)* | 56 | **[NOTE]** An apparently-stale **duplicate** of `ComponentSpecParser/to_itemtype.F90` lives at the top level. The top-level copy is one `case` arm *ahead* (`'vector_bracket'` → `MAPL_STATEITEM_VECTORBRACKET`) of the one actually compiled (`ComponentSpecParser/to_itemtype.F90`, wired via `esma_add_fortran_submodules` at `CMakeLists.txt:124-130`); the top-level file is **not referenced by any `CMakeLists.txt`** in this checkout (confirmed by grep) and is therefore not compiled. Treat the `ComponentSpecParser/` copy as the real one; the top-level file looks like an accidental leftover from a refactor, not a second valid implementation. Do not edit the top-level copy expecting it to take effect. |
| `UserSetServices.F90` | `mapl_UserSetServices_mod` | 238 | Wraps a user's own `SetServices` procedure pointer/interface so `OuterMetaComponent` can invoke it uniformly. |
| `MethodPhasesMap.F90` | `mapl_MethodPhasesMap_private_mod` | 191 | Maps ESMF phase names/indices for init/run methods. |
| `InnerMetaComponent.F90` | `mapl_InnerMetaComponent_mod` | 84 | The *inner* gridcomp's private state (distinct from `OuterMetaComponent`, the wrapping component MAPL itself creates). Holds the user's own `GridComp`, its own `Clock`, and a pointer back to the owning `OuterMetaComponent`. |
| `OuterMetaComponent.F90` + `OuterMetaComponent/` (64 files, 3079 lines) | `mapl_OuterMetaComponent_mod` | 857 (+ submodules) | The central component-hierarchy type. See §1.2. |
| `OuterMetaComponentDriverResolver.F90` | `mapl_OuterMetaComponentDriverResolver_mod` | 84 | Small helper resolving a child's `GriddedComponentDriver` from an `OuterMetaComponent`. |
| `GraphScenarioBootstrap.F90` | `mapl_GraphScenarioBootstrap_mod` | 135 | Test/scenario-harness bootstrap helper (YAML-driven fixture setup for scenario tests and the `graph_diagnostic` app). |
| `GraphResolutionEntry.F90`, `GraphResolutionEntryVector.F90` | `mapl_GraphResolutionEntry_mod`, `...Vector_mod` | 42, 14 | Structured resolution-report record type used by `GraphBuilder`'s lifecycle hooks (shared with the graph layer; documented in depth in `docs/graph/ARCHITECTURE.md`). |
| `GraphBuilder.F90` + ten `GraphBuilder_*_smod.F90` | `mapl_GraphBuilder_mod` | 1174 (+ ~10 submodules) | The graph integration layer. **Documented in `docs/graph/ARCHITECTURE.md` §1.2** — listed here only because the files live in this directory; this doc does not re-describe their internals. |
| `MAPL3_Deprecated.F90` | `mapl_Deprecated_mod` | 13 | Marker module for deprecated-symbol bookkeeping. |
| `RestartHandler.F90` | `mapl_RestartHandler_mod` | 220 | Read/write-restart orchestration helper used by `OuterMetaComponent/write_restart.F90` / `read_restart.F90`. |
| `RunEntryPoint.F90`, `EntryPointVector.F90` | `mapl_RunEntryPoint_mod`, `mapl_EntryPointVector_mod` | 11, 15 | Tiny supporting types for registering custom run-phase entry points (`set_entry_point.F90`). |
| `OpenMP_Support.F90` | `mapl_OpenMP_Support_mod` | 461 | OpenMP thread/region helpers used by generic-layer run-phase dispatch. |

### 1.2 `OuterMetaComponent/` — the component-hierarchy type and its lifecycle

`OuterMetaComponent.F90` (857 lines) declares the type's full private
state and every type-bound-procedure interface; the 64 files in
`OuterMetaComponent/` are its submodules (one procedure or tightly
related group per file), totaling 3079 lines. This is the largest
single type in the generic layer by procedure count.

Key state owned by `OuterMetaComponent` (from its own `use` list and
declarations, `OuterMetaComponent.F90:1-30`):
- a `HierarchyId` (`mapl_HierarchyId_mod`) + local `ComponentGraph`
  (`mapl_ComponentGraph_mod`) — the graph-layer attachment point.
- a `NodeId` for its own run-phase `MethodGraphNode`
  (`get_run_phase_method_node_id`/`set_run_phase_method_node_id`).
- a `StateRegistry` (`mapl_StateRegistry_mod`) — the legacy coupler
  machinery, still the only producer of a component's real `ESMF_State`
  under the default `GRAPH_MODE_REGISTRY`.
- a `GriddedComponentDriverMap` of children (`children`), each entry a
  `GriddedComponentDriver` (owned by sibling `superstructure/
  component/`, not this layer — see §2.5).
- a `ComponentSpec` (the parsed YAML description) and a `ChildSpec`.
- a `CheckpointControls` value and vertical-grid/geometry state
  (`GeomId`, `VerticalGrid`).

Representative submodule groupings (not exhaustive — see
`CMakeLists.txt:99-122` for the authoritative file list):

| Group | Files | Covers |
|---|---|---|
| Construction/teardown | `new_outer_meta.F90`, `init_meta.F90`, `free_outer_meta.F90`, `attach_outer_meta.F90`, `get_outer_meta_from_outer_gc.F90` | Allocating and attaching an `OuterMetaComponent` to an ESMF `GridComp`'s private state. |
| Child management | `add_child_by_spec.F90`, `get_num_children.F90`, `get_child_name.F90`, `get_child_by_name.F90`, `get_child_driver.F90`, `get_child_outer_meta.F90`, `get_child_component_spec.F90`, `get_child_component_graph.F90`, `apply_to_children_custom.F90` | Building and querying the child hierarchy. |
| Init-phase lifecycle | `initialize_advertise.F90`, `initialize_modify_advertised.F90`, `initialize_realize_provided.F90`, `initialize_accept_transfer.F90`, `initialize_realize_accepted.F90`, `initialize_graph_freeze.F90`, `initialize_geom_a.F90`, `initialize_geom_b.F90`, `initialize_set_clock.F90`, `initialize_user.F90`, `initialize_read_restart.F90` | The ordered sequence of generic init sub-phases; see §2.4 for the verified call order inside `initialize_advertise.F90`. |
| Run-phase dispatch | `run_children.F90`, `run_child_by_name.F90`, `run_custom.F90`, `run_user.F90`, `run_clock_advance.F90`, `recurse.F90` | Recursing a given ESMF phase index down the child hierarchy (`recurse_`, `OuterMetaComponent/recurse.F90:12`) and dispatching user vs. generic run code. |
| Connections | `connect_all.F90`, `advertise_variable.F90` | Drives the legacy `Connection`/`StateRegistry` resolution for a component's declared connections. |
| Geometry/vertical grid | `set_geom.F90`, `get_geom.F90`, `get_geom_id.F90`, `has_geom.F90`, `propagate_geom_to_children.F90`, `set_vertical_grid.F90`, `get_vertical_grid.F90` | `propagate_geom_to_children` (`OuterMetaComponent/propagate_geom_to_children.F90:10`) pushes a parent's geometry down to any child whose `component_spec%geometry_spec%kind == GEOMETRY_FROM_PARENT`. |
| Restart | `write_restart.F90`, `read_restart.F90`, `get_checkpoint_dir.F90`, `ensure_memory_checkpoint.F90`, `get_memory_checkpoint_state.F90` | Delegates to `RestartHandler.F90`; see §5 (internal/memory checkpoint decisions). |
| Graph-layer attachment | `get_component_graph.F90`, `get_child_component_graph.F90`, `get_run_phase_method_node_id.F90`, `set_run_phase_method_node_id.F90`, `connections_resolved.F90` | Accessors `GraphBuilder*.F90` uses; documented from the graph side in `docs/graph/ARCHITECTURE.md`. |
| Misc accessors | `get_name.F90`, `get_gridcomp.F90`, `get_hconfig.F90`, `set_hconfig.F90`, `get_phases.F90`, `get_internal_state.F90`, `get_logger.F90`, `get_registry.F90`, `get_component_spec.F90`, `get_user_gc_driver.F90`, `set_entry_point.F90`, `start_timer.F90`, `stop_timer.F90`, `finalize.F90` | Plain field accessors and small utility procedures. |

### 1.3 `registry/` — legacy `StateRegistry` and extension bookkeeping (11 files, 1638 lines)

| File | Responsibility |
|---|---|
| `StateRegistry.F90` | The type declaration + all module-procedure interfaces (public API surface). Holds per-component maps keyed by `VirtualConnectionPt`/`ActualConnectionPt`. |
| `StateRegistry_Lifecycle_smod.F90` | Construction, naming, basic existence queries (`has_virtual_pt`). |
| `StateRegistry_Hierarchy_smod.F90` | Subregistry (child-registry) management: `add_subregistry`, `has_subregistry`, `get_subregistry_by_name`. |
| `StateRegistry_Extensions_smod.F90` | `add_virtual_pt`, `add_family`, `add_primary_spec` — the `ExtensionFamily` bookkeeping for a given virtual connection point. |
| `StateRegistry_Propagation_smod.F90` | `propagate_unsatisfied_imports_{all,subregistry,virtual_pt}` and (by symmetry, confirmed in source) the export-propagation counterpart — the legacy, unconditional one-level-up bubbling that `GraphBuilder_ConnectionResolution_smod.F90`'s `run_propagate_hook`/`run_export_propagate_hook` now parallel on the graph side (see `docs/graph/ARCHITECTURE.md` §3.1 for the full graph-side account of why this was replicated, not reused, there). |
| `StateRegistry_Actions_smod.F90` | `allocate_items`, `add_to_states` (populates a real `ESMF_State`/`ESMF_FieldBundle` from registry contents), `filter` (pattern-matching query). |
| `StateRegistry_SetGeometry_smod.F90` | `set_geometry` — pushes an `ESMF_Geom`/`VerticalGrid` pair onto every registered item. |
| `AbstractRegistry.F90` | Bare abstract marker type (empty, no deferred methods) — `StateRegistry`'s nominal parent; exists for a possible future second concrete registry, not currently used for polymorphic dispatch anywhere found in this pass. |
| `ExtensionFamily.F90` | Tracks the set of `StateItemSpec`s that are mutually related by extension (one "primary" spec plus its couplers' derived specs), keyed by `VirtualConnectionPt`. |
| Container files: `ActualPtSpecPtrMap.F90`, `ActualPtComponentDriverMap.F90`, `VirtualPtStateItemPtrMap.F90`, `VirtualPtStateItemSpecMap.F90`, `ActualPtStateItemSpecMap.F90`, `StateItemVector.F90`, `RegistryPtr.F90`, `RegistryPtrMap.F90`, `ActualPtVector.F90`, `ActualPtVec_Map.F90`, `VirtualPtFamilyMap.F90` | gFTL map/vector instantiations backing the above. |

This is the legacy imperative coupler machinery. It is **still the
sole producer of a component's real `ESMF_State`** under the default
`GRAPH_MODE_REGISTRY` (see §2.3), and remains active and tested
regardless of graph-layer progress. Per `docs/graph/ARCHITECTURE.md`
§6, legacy `StateRegistry` retirement ("Phase 6a") is explicitly gated
on a five-item prerequisite list in `docs/graph/spec/
20-implementation-roadmap.md` §20.4.11 — **do not treat this directory
as dead code**; it has no scheduled removal date, only a list of
graph-side prerequisites that must land first.

### 1.4 `connection/` — the `Connection` family (10 files, 1256 lines)

| File | Responsibility |
|---|---|
| `Connection.F90` | Abstract base: `get_source`/`get_destination`/`activate`/`connect`, all deferred. |
| `ConnectionPt.F90`, `ConnectionPtVector.F90` | A resolved (component, item-name) address and its vector container. |
| `VirtualConnectionPt.F90`, `VirtualConnectionPtVector.F90` | An *unresolved* connection endpoint (declared name, before matching). |
| `ActualConnectionPt.F90` | A resolved endpoint bound to a real `StateItemSpec`. |
| `SimpleConnection.F90` | A connection with explicit, named source and destination (`ConnectionPt` pair) — the ordinary case. |
| `MatchConnection.F90` | A connection matching by pattern/wildcard rather than an explicit pair; shares `connect`/structural-resolution logic with `SimpleConnection` on the graph-integration side (see `docs/graph/ARCHITECTURE.md` §1.2's note on `connection_pt_pair_resolves`). |
| `ReexportConnection.F90` | Re-exports an already-connected item under a different name/level; stays on the `class default` (not-specially-handled) path in `GraphBuilder_ConnectionResolution_smod.F90`'s resolution dispatch. |
| `ConnectionVector.F90` | gFTL vector of `class(Connection)`. |

Every concrete `Connection` subtype, plus `StateRegistry`, is
referenced directly by `superstructure/generic/graph/` integration
code (`GraphBuilder_ConnectionResolution_smod.F90` dispatches on
`class is (SimpleConnection)` / `class is (MatchConnection)`) — this is
one of the two families (along with `specs/`) that both the legacy
path and the graph integration layer depend on directly.

### 1.5 `ComponentSpecParser/` — YAML → `ComponentSpec` (9 files, 1034 lines)

| File | Responsibility |
|---|---|
| `parse_component_spec.F90` | Top-level entry: parses one component's full YAML block into a `ComponentSpec`. |
| `parse_children.F90`, `parse_child.F90` | Recursively parses the `children:` list into `ChildSpec`s. |
| `parse_connections.F90` | Parses the `connections:` list into `Connection` objects (dispatches to `SimpleConnection`/`MatchConnection`/`ReexportConnection` construction based on YAML shape). |
| `parse_var_specs.F90` | Parses the `variables:`/import/export list into `VariableSpec`s; calls `to_itemtype` (below) per entry. |
| `parse_geometry_spec.F90` | Parses the `geometry:` block into a `GeometrySpec`. |
| `parse_setservices.F90` | Resolves the named user `SetServices` procedure. |
| `parse_timespec.F90` | Parses run-phase timestep/alarm configuration. |
| `to_itemtype.F90` | Classifies a variable's declared item type (`field`/`expression`/`vector`/`service`/`wildcard`/`bracket`/`vector_bracket`) from its YAML `class:`/`expression:` keys. **This is the file actually compiled** — see the top-level duplicate's note in §1.1. |

### 1.6 `specs/` — the `ComponentSpec`/`VariableSpec`/aspect type hierarchy (44 files, 10,658 lines — largest subdirectory)

This is the metadata model the legacy coupler (and, for a handful of
axes, the parser) dispatches on. Three loosely-layered families:

**Spec containers** (what a component declares):
- `ComponentSpec.F90` — a component's full declaration: children,
  variables, connections, geometry spec, set-services reference.
- `ChildSpec.F90` + `ChildSpecMap.F90` — one child's declaration
  (name, YAML source, geometry-inheritance mode).
- `VariableSpec.F90` (plus `VariableSpecTag.F90`, `VariableSpecMemberMap.F90`,
  `VariableSpecVector.F90`, `VariableSpec_private.F90`) — one
  import/export's full declaration: name, units, typekind, ungridded
  dims, standard name, restart mode, and (per `openspec/changes/
  archive/2026-09-18-composite-state-spec`) a polymorphic map of
  declared composite *members* of itself, keyed by name
  (`VariableSpecMemberMap`, templated over the bare marker type
  `VariableSpecTag` specifically to let `VariableSpec` contain a map of
  its own type without a circular module dependency — see that file's
  own header comment and `design.md` "Decisions").
- `GeometrySpec.F90`, `GridSpec.F90` — declared geometry (grid kind,
  resolution, or "inherit from parent").
- `CheckpointControls.F90` — four independent boolean flags
  (`import`/`export`/`internal`/`bootstrap`) controlling which
  categories of state participate in restart I/O.

**Resolved state-item model** (what a connection actually resolves
against):
- `StateItemSpec.F90` — the resolved, in-memory representation of one
  state item: an `AspectMap` of its `StateItemAspect` values, its
  `ESMF_StateIntent_Flag`, its dependency list
  (`VirtualConnectionPtVector`), and (per the "merged from
  StateItemExtension" comment at `StateItemSpec.F90:40-44`) direct
  producer/consumer tracking — a `ComponentDriverVector` of consumers
  and a single `producer` pointer, used by the legacy coupler to know
  which `ComponentDriver` computes/depends on this spec.
- `StateItemSpecVector.F90`, `StateItemSpecPtrVector.F90`,
  `StateItemSpecMap.F90` — containers.

**`StateItemAspect`/`ClassAspect` family** (the per-axis metadata
values a `StateItemSpec` carries in its `AspectMap`, and the
connection-compatibility rules between them):
- `StateItemAspect.F90` — abstract base. Its header comment
  (`StateItemAspect.F90:2-30`) documents the full allowed-connection
  table between `simple`/`mirror`/`timedep` aspect kinds and when a
  coupler is required — read this comment directly before changing any
  `can_connect_to`/`matches` override.
- `ClassAspect.F90` — abstract subclass distinguishing a state item's
  *structural class* (plain field vs. one of the special subclasses
  below) from its other (`GeomAspect`/`UnitsAspect`/etc.) metadata.
  Concrete subclasses, one per YAML `class:` value:
  - `FieldClassAspect.F90` (+ `FieldClassAspect_smod.F90`) — ordinary
    field; `matches_a` accepts another `FieldClassAspect` or a
    `WildcardClassAspect`.
  - `FieldBundleClassAspect.F90`, `StateClassAspect.F90` — bundle/state
    structural classes.
  - `VectorClassAspect.F90` (+ `ActualPtFieldAspectMap.F90`) — a
    multi-component vector quantity.
  - `WildcardClassAspect.F90` — matches any structural class (used for
    the catch-all export side of a wildcard import).
  - `ServiceClassAspect.F90` — a callable service, not a data value.
  - `BracketClassAspect.F90`, `VectorBracketClassAspect.F90` — a
    time-interpolation bracket (2-field/2-fieldbundle holding adjacent
    time levels + weights); legacy's own shape-changing conversion to
    an ordinary field/vector (`TimeInterpolateTransform.F90`). The
    graph-native layer's analogous but independently-implemented
    `ItemShapeCharacteristic`/`BracketTimeInterpolationTransform`
    (`BRACKET`→`FIELD` only, landed Phase 7a) is a **separate, parallel
    mechanism** — see §2.4/§6 for how the two relate.
  - `ExpressionClassAspect.F90` — a computed (formula-driven) item;
    see `openspec/specs/generic/expression-vertical-dim-resolution/
    spec.md` for the one landed cross-cutting rule governing it
    (§1.7 below).
- Value aspects (one per metadata axis, each a concrete
  `StateItemAspect` subclass with its own `can_connect_to`/`matches`
  compatibility rule): `UnitsAspect.F90`, `TypekindAspect.F90`,
  `UngriddedDimsAspect.F90`, `GeomAspect.F90` (+ submodule directory
  `GeomAspect/make_transform.F90`), `VerticalGridAspect.F90` (+
  submodule directory `VerticalGridAspect/make_transform.F90`),
  `AttributesAspect.F90`, `StandardNameAspect.F90`,
  `QuantityTypeAspect.F90`, `ConservationAspect.F90`,
  `NormalizationAspect.F90`.
- `AspectId.F90`, `AspectStatus.F90` — small supporting enums/ids used
  by `AspectMap` to key and status-tag aspect entries.

**[ASSUMPTION]** The `specs/` family's `StateItemAspect`/`ClassAspect`
hierarchy and the graph-neutral `graph/characteristics/
StateItemCharacteristic` hierarchy (`docs/graph/README.md`'s
`characteristics.md`) are **structurally parallel but code-independent
reimplementations** of largely the same axis list (units, typekind,
geometry, ungridded dims, standard name, quantity type, conservation,
normalization, item-shape/bracket). This parallel-reimplementation
relationship is stated directly in multiple places already read this
session (`docs/graph/ARCHITECTURE.md` §1.1's characteristics row,
Phase 12's narrative in §7, and the Phase 7a proposal's "No change to
legacy `ClassAspect`" impact note) — treated as verified, not an
assumption of this pass, but flagged because no single file states "these
two hierarchies are intentionally kept separate forever"; the roadmap's
own framing is that the graph-native side is being built up to
eventually make the `specs/` aspect side removable (Phase 6a), not that
the two will coexist indefinitely.

### 1.7 `transforms/` — concrete data converters, legacy `ExtensionTransform` family (14 files + 1 note, 3211 lines)

| File | Responsibility |
|---|---|
| `TransformId.F90` | Small id/key type identifying a transform instance. |
| `ExtensionTransform.F90` | Abstract base (legacy coupler's transform interface — distinct from, and not a parent of, the graph-native `mapl_Transform_mod` the graph layer defines independently). |
| `NullTransform.F90` | No-op placeholder, used so a function returning an `ExtensionTransform` can allocate a return value under error conditions. |
| `ExtendTransform.F90` | A near-no-op transform used when an export simply "mirrors" an aspect value onto a differently-typed import (no real data conversion). |
| `TransformVector.F90` | gFTL vector of `class(ExtensionTransform)`. |
| `RegridTransform.F90` | Horizontal regrid (via `MAPL_FieldClone`/`MAPL_FieldGet`, ESMF regrid handles). |
| `VerticalRegridTransform.F90` | Vertical regrid, delegating to `superstructure/generic/vertical/` and `infrastructure/vertical/`. |
| `CopyTransform.F90` | Plain field copy (same shape/units/typekind — still routed through the coupler machinery for uniformity). |
| `ConvertUnitsTransform.F90` | Units conversion via `udunits2`. |
| `NormalizationTransform.F90` | Normalization-aspect-driven value rescaling. |
| `TimeInterpolateTransform.F90` | Bracket (2-field + weights) → ordinary field time interpolation — legacy's own implementation of the same operation Phase 7a's graph-native `BracketTimeInterpolationTransform` (in `graph/transforms/`) independently reimplements. |
| `EvalTransform.F90` | Expression (`ExpressionClassAspect`) formula evaluation. |
| `ExtensionTransformUtils.F90` | Shared helper procedures for the above. |
| `CouplerMetaComponent.F90`, `GenericCoupler.F90` | **Historically lived in `couplers/`** (see below); absorbed into `transforms/` per that directory's own `CMakeLists.txt` comment. The actual ESMF gridcomp wrapper around one `ExtensionTransform` instance, used to run a coupler as a genuine child gridcomp when a transform needs its own `Initialize`/`Run`/`Finalize` lifecycle rather than being called as a plain procedure. |
| `notes.md` | Free-form developer notes file; not a doc this pass restructured — read directly if touching this directory. |

**`couplers/` subdirectory is now empty** — its `CMakeLists.txt`
contains only the comment `# Sources moved to transforms/:
CouplerMetaComponent.F90, GenericCoupler.F90`. It is still listed in
the parent `CMakeLists.txt`'s `add_subdirectory(couplers)` (so CMake
configuration does not break), but contributes zero sources. Treat it
as a historical marker, not a live module group — do not add new files
there.

### 1.8 `vertical/` — vertical-grid types specific to the generic layer (2 files, 905 lines)

| File | Responsibility |
|---|---|
| `ModelVerticalGrid.F90` | `mapl_ModelVerticalGrid_mod` — a `StateRegistry`-aware vertical grid built from a component's own declared levels; pulls in nearly the whole `specs/` aspect family (see its own `use` list) because it must participate in the same aspect-matching machinery as any other `StateItemAspect`. |
| `FixedLevelsVerticalGrid.F90` | `mapl_FixedLevelsVerticalGrid_mod` — a vertical grid defined by an explicit, fixed list of levels (parsed from YAML `ESMF_HConfig`), extending the generic (non-layer-specific) `mapl_VerticalGridSpec_mod` from `infrastructure/vertical/vertical_grid/`. |

The genuinely generic vertical-grid machinery (`VerticalGrid`,
`VerticalGridSpec`, `VerticalGridFactory`, `VerticalCoordinateDirection`,
`VerticalStaggerLoc`, the actual regrid-method numerics —
`CSR_SparseMatrix`/`VerticalConservativeMap`/`VerticalLinearMap`) lives
in `infrastructure/vertical/vertical_grid/` and `infrastructure/
vertical/vertical/`, **not** in this directory — confirmed by
`vertical/CMakeLists.txt`'s own comment ("moved to infrastructure/
vertical/vertical/"). This directory holds only the two
layer-specific concrete grid types above.

## 2. Important interfaces and dependencies

### 2.1 Dependency direction among the subdirectories in this file

```
specs/            <-- depended on by: connection/, registry/, transforms/,
                       vertical/, ComponentSpecParser/, OuterMetaComponent/
connection/       <-- depended on by: registry/ (StateRegistry uses Connection
                       indirectly via resolved specs), OuterMetaComponent/
                       (connect_all.F90), GraphBuilder_ConnectionResolution_smod.F90
registry/         <-- depended on by: OuterMetaComponent/ (owns one StateRegistry),
                       connection/ (SimpleConnection/MatchConnection/
                       ReexportConnection all `use mapl_StateRegistry_mod`),
                       vertical/ModelVerticalGrid.F90
transforms/       <-- depended on by: registry/ (StateRegistry_Actions_smod /
                       ExtensionFamily resolution builds transforms),
                       vertical/ (VerticalRegridTransform wraps
                       infrastructure/vertical/ numerics)
ComponentSpecParser/ <-- depended on only by OuterMetaComponent/ (add_child_by_spec.F90
                       and friends parse a child's YAML into a ComponentSpec)
```

No subdirectory here depends on `superstructure/generic/graph/` —
confirmed by grep (no `graph/` or `mapl_ComponentGraph_mod`/
`mapl_NodeId_mod`/`mapl_StateItemCharacteristic_mod` `use` statement
found in `specs/`, `connection/`, `registry/`, `transforms/`,
`vertical/`, or `ComponentSpecParser/`). The *only* files in this
directory tree that depend on the graph layer are the top-level
`GraphBuilder*.F90` files and `OuterMetaComponent.F90`/its submodules
(§2.4) — i.e. exactly the integration seam, nothing deeper.

### 2.2 `StateItemSpec`/`AspectMap` — the legacy coupler's central value type

A `StateItemSpec` (`specs/StateItemSpec.F90:30` onward) is the resolved
representation of one import or export. It carries:
- an `AspectMap` (keyed by `AspectId`) of `StateItemAspect` values —
  one entry per axis (units, typekind, geometry, class, ...), looked up
  via `get_aspect_order()`/`get_aspect_priorities()` to determine
  comparison/resolution order;
- its `ESMF_StateIntent_Flag` (import/export/internal);
- a `VirtualConnectionPtVector` of its own declared dependencies (for
  `expression`/composite items that reference other items);
- direct producer/consumer tracking: `producer` (the single
  `ComponentDriver` that computes this spec, if any) and `consumers`
  (every `ComponentDriver` that depends on it) — per the file's own
  comment, this was "merged from StateItemExtension," i.e. an earlier,
  separate type whose responsibility was folded in here.

Connection compatibility between two `StateItemSpec`s is decided
per-aspect, via each aspect's own `can_connect_to`/`matches` method —
**not** by a single top-level comparison. `StateItemAspect.F90`'s
header-comment table (simple/mirror/timedep × simple/mirror/timedep,
whether a coupler is required) is the one authoritative statement of
the cross-aspect-kind connection rules; read it directly, do not
re-derive it from individual aspect files.

### 2.3 `GRAPH_MODE_*` — who actually produces a component's `ESMF_State`

`superstructure/generic/graph/GraphMode.F90` (`mapl_GraphMode_mod`) is
the single global 3-way selector between the legacy path this file
documents and the graph-native path `docs/graph/` documents:

| Mode | Behavior |
|---|---|
| `GRAPH_MODE_REGISTRY` (default) | Only `StateRegistry`/`transforms/` populate the real `ESMF_State`. Zero behavior change from before the graph layer existed. This is what every real production run uses today — nothing sets `set_graph_mode()` outside test code. |
| `GRAPH_MODE_BOTH` | Graph-native materialization runs *alongside* the legacy coupler, but never inserts into the same `ESMF_State` — readable only out of band, directly from the graph node's own payload. Used to compare the two paths with no collision risk. |
| `GRAPH_MODE_GRAPH` | Graph-native state population is the *sole* producer; the legacy coupler's population step is skipped for that component. Not yet exercised end-to-end against any real fixture by design (`docs/graph/ARCHITECTURE.md` §6's "demonstrate before replace" note) — unit-testable regardless. |

This file's own subdirectories (`specs/`, `connection/`, `registry/`,
`transforms/`) run **unconditionally** regardless of `graph_mode()` —
legacy resolution (`StateRegistry%propagate_unsatisfied_imports`/
`propagate_exports`, `process_connections`) always executes; only
*whether the graph-native path also runs, and whether its result feeds
the real `ESMF_State`*, is gated by this selector. A mismatch detected
by a legacy aspect's own `can_connect_to`/`matches` (e.g.
`UngriddedDimsAspect`, which rejects disagreement unconditionally,
independent of `ValidationMode`) can hard-fail `process_connections()`
*before* `GraphBuilder`'s own resolution ever runs, regardless of
`graph_mode()` — a real, previously-encountered gotcha documented from
the graph side in `docs/graph/ARCHITECTURE.md` §7's "Surprise #2."

### 2.4 Lifecycle call sites where this layer calls into the graph layer

Verified in `OuterMetaComponent/initialize_advertise.F90` (also stated
from the graph side in `docs/graph/ARCHITECTURE.md` §2.4; repeated here
because it is the one place this layer's own lifecycle code explicitly
calls out to `GraphBuilder`):

```
recurse()                              ! bottom-up: every child's own advertise pass first
run_advertise_hook(this)               ! GraphBuilder — creates StateItemNodes from VariableSpec
run_activate_hook(this)                ! GraphBuilder — connection resolution
run_propagate_hook(this)               ! GraphBuilder — import mirroring (graph-native analog of
                                        !   StateRegistry%propagate_unsatisfied_imports, below)
registry%propagate_unsatisfied_imports ! legacy — unchanged, still runs in parallel
registry%propagate_exports             ! legacy — unchanged
```

Both the legacy registry calls and the `GraphBuilder` hook calls run
unconditionally in sequence — the graph-native hooks do not replace the
legacy calls; they run *alongside* them, with `GRAPH_MODE_*` only
gating whether the graph-native result ends up in the real `ESMF_State`
(§2.3). Other `OuterMetaComponent` lifecycle submodules with their own
`GraphBuilder` call sites, by phase:
- `initialize_accept_transfer.F90` — `run_connect_hook`
  (post-connection-resolution convergence check).
- `initialize_graph_freeze.F90` — the hierarchy-wide `freeze()` call
  (see `docs/graph/ARCHITECTURE.md` §2.2's "single hierarchy-wide
  operation" account).
- `connect_all.F90` — drives legacy `Connection`/`StateRegistry`
  resolution; does not itself call `GraphBuilder` (confirmed by
  reading the file in this pass; graph-native connection resolution
  is driven entirely from `initialize_advertise.F90`'s hooks above).

### 2.5 `superstructure/component/` — a dependency, not a sibling-owned-the-same-way concern

`GriddedComponentDriver`, `ComponentDriver`, `ComponentDriverVector`,
`GriddedComponentDriverMap`, `GriddedComponentDriverVector`,
`ComponentDriverPtrVector` are all defined in `superstructure/
component/`, a **sibling** directory to `superstructure/generic/`, not
a subdirectory of it. `OuterMetaComponent` owns a
`GriddedComponentDriverMap` of its children and `StateItemSpec` tracks
`ComponentDriver` producer/consumer pointers directly — i.e. the
generic layer depends on `superstructure/component/`, never the
reverse. This dependency edge is real and load-bearing (confirmed by
`use mapl_GriddedComponentDriver_mod` appearing throughout
`OuterMetaComponent/`, `registry/`, `connection/`, and `vertical/
ModelVerticalGrid.F90`), but `superstructure/component/` itself is
**not documented by this file** — it is a separate library with its
own CMake target, outside this pass's scope.

## 3. `CMakeLists.txt` structure (verified, all files read directly)

`superstructure/generic/CMakeLists.txt` defines the `MAPL.generic`
library (`esma_set_this (OVERRIDE MAPL.generic)`) with:
- a top-level `srcs` list (the files in §1.1, minus `GraphBuilder_*_smod.F90`'s
  own submodule machinery which is listed in the same `srcs` list
  directly — NAG/gfortran submodule files are ordinary sources here,
  unlike the `esma_add_fortran_submodules` mechanism used for
  `OuterMetaComponent/` and `ComponentSpecParser/`, see below);
- `add_subdirectory()` for `specs` (aliased to build dir
  `generic_specs` — see the file's own long comment explaining a
  Spack/Intel/`ifort`-under-GCC `specs`-file-name collision this
  avoids), `registry`, `connection`, `transforms`, `couplers` (now
  empty), `graph` (out of scope here), `vertical` — each of those
  subdirectories' own `CMakeLists.txt` calls `target_sources(MAPL.generic
  PRIVATE ...)` directly, listed in full in §1.3-§1.8 above;
- two `esma_add_fortran_submodules()` calls — one for
  `OuterMetaComomponent/` (all 64 files, listed verbatim at
  `CMakeLists.txt:99-122`), one for `ComponentSpecParser/` (9 files,
  `CMakeLists.txt:124-130`). This macro
  (`ESMA_cmake/esma_support/esma_add_fortran_submodules.cmake`) exists
  specifically to avoid a same-filename collision across separate
  submodule subdirectories — it copies each source into a
  `${SUBDIRECTORY}_SMOD_${file}`-named build artifact rather than
  compiling it in place, so two different subdirectories may each
  contain a file with the same base name (not currently the case here,
  but this is the mechanism that would make it safe) without clashing
  object-file names.
- library dependencies: `MAPL.enums`, `MAPL.regridder_mgr`, `MAPL.geom`,
  `MAPL.vertical_grid`, `MAPL.GeomIO`, `MAPL.esmf`, `MAPL.field`,
  `MAPL.state`, `MAPL.utils`, `MAPL.vertical`, `MAPL.component`
  (confirms §2.5's dependency direction), `MAPL.field_bundle`,
  `MAPL.infrastructure`, plus `ESMF::ESMF`, `NetCDF::NetCDF_Fortran`,
  `udunits2f`, `PFLOGGER::pflogger`, `GFTL_SHARED`/`GFTL`.
- `add_subdirectory(tests EXCLUDE_FROM_ALL)` guarded by
  `if (PFUNIT_FOUND)`.

## 4. Verified build and test commands

Carried over from `docs/graph/ARCHITECTURE.md` §4 (verified there on
2026-10-16/17/19 against this same checkout's `nag`/`gfortran` build
trees) — **not independently re-run for this pass**. The ctest target
names below are the authoritative, non-`graph`-specific suites this
file's subdirectories feed into; `docs/graph/ARCHITECTURE.md` §4
documents the same commands with `MAPL.generic.components`/
`MAPL.generic.scenarios` emphasis for graph-layer regressions
specifically.

### 4.1 Standard build/test invocation

```bash
module load nag-stack/7.2.41
cmake --build nag --target build-tests -j 8
ctest --test-dir nag -L ESSENTIAL --output-on-failure -j 4   # all 7 MAPL.generic.* suites carry this label

module load gfortran-stack
cmake --build gfortran --target build-tests -j 8
ctest --test-dir gfortran -L ESSENTIAL --output-on-failure -j 4
```

(`nag-stack/7.2.41` is the correct meta-module for this checkout's
configured toolchain — confirmed previously by reading
`nag/CMakeCache.txt`'s `BASEDIR`; a bare `module load nag mpi
baselibs` does not resolve on this host's Lmod setup.)

### 4.2 The seven `superstructure/generic/tests/` ctest targets (verified by reading `tests/CMakeLists.txt` directly)

All carry `LABELS "ESSENTIAL"` and (except `.components.quiet`)
`MAX_PES 4`:

| Target | Test sources (file) | Covers (relative to this doc's directories) |
|---|---|---|
| `MAPL.generic.scenarios` | `Test_Scenarios.pf`, `Test_MemoryCheckpoint.pf`, `Test_GraphBuilderEquivalence.pf`, `Test_GraphEquivalenceHarness.pf`, `Test_GraphModeDemonstration.pf` | End-to-end YAML-driven scenario fixtures exercising the full init-phase lifecycle (both legacy and graph-native paths, all three `GRAPH_MODE_*` values). Runs under **strict** field-dictionary `ValidationMode` (`EXTRA_INITIALIZE Initialize_strict`) — the only suite here that does. |
| `MAPL.generic.transforms` | `Test_TimeInterpolateTransform.pf`, `Test_ConvertUnitsTransform.pf`, `Test_CopyTransform.pf`, `Test_ExtensionTransform.pf`, `Test_ExtensionTransformUtils.pf`, `Test_2DConservativeRegrid.pf`, `Test_NormalizationTransform.pf`, `Test_3DConservativeMixingRatio.pf`, `Test_IntegratedNormalization.pf`, `Test_RegridTransform.pf` | §1.7 `transforms/`. |
| `MAPL.generic.vertical` | `Test_VerticalRegridTransform.pf`, `Test_ModelVerticalGrid.pf`, `Test_VerticalLinearMap.pf`, `Test_VerticalConservativeMap.pf` | §1.8 `vertical/` (plus `infrastructure/vertical/` numerics). |
| `MAPL.generic.aspects` | `Test_BaseAspect.pf`, `Test_AspectMap.pf`, `Test_Aspects.pf`, `Test_TypekindAspect.pf`, `Test_UngriddedDimsAspect.pf`, `Test_BracketClassAspect.pf`, `Test_VectorBracketClassAspect.pf`, `Test_UnitsAspect.pf`, `Test_QuantityTypeAspect.pf`, `Test_GeomAspect.pf`, `Test_ConservationAspect.pf`, `Test_NormalizationAspect.pf`, `Test_ExpressionClassAspect.pf`, `Test_VerticalGridAspect_NoneMatch.pf` | §1.6 `specs/` aspect family. |
| `MAPL.generic.components` | `Test_GenericGridComp.pf`, `Test_ComponentSpec.pf`, `Test_VariableSpecMembers.pf`, `Test_VariableSpecCallback.pf`, `Test_VariableSpecInout.pf`, `Test_ComponentSpecParser.pf`, `Test_WriteYaml.pf`, `Test_ExtensionFamily.pf`, `Test_ComponentHierarchyGraph.pf`, `Test_GraphBuilder*.pf` (5 files), `Test_GraphBuilderMaterializeGate.pf`, `Test_GraphMode.pf`, `Test_OuterMetaComponentDriverResolver.pf`, `Test_GraphBuilderPropagation.pf`, `Test_GridCompDriverPhaseInvoker.pf` | §1.2 `OuterMetaComponent/`, §1.5 `ComponentSpecParser/`, plus every `GraphBuilder*` unit suite (graph-integration-layer tests; detailed in `docs/graph/`). |
| `MAPL.generic.components.quiet` | `Test_GraphGeometryHook.pf`, `Test_GraphBuilder_ConvergenceCheck.pf` | Same scope as `.components`, split out because both files deliberately drive a `GraphBuilder` hook down a negative path that logs a `report_if_failed` warning; run with `MAX_PES 1` and `COMMAND_LINE_ARGS --level ERROR` to suppress that expected noise without hiding real warnings elsewhere (pFUnit upstream PR #585 mechanism, manually backported into this tree's installed pFUnit). |
| `MAPL.generic.core` | `Test_VirtualConnectionPt.pf`, `Test_ConnectionPt.pf`, `Test_FieldDictionary.pf`, `Test_FieldDictIntegration.pf`, `Test_StateRegistry.pf`, `Test_Couplers.pf`, `Test_CSR_SparseMatrix.pf`, `Test_timestep_propagation.pf`, `Test_propagate_time_varying.pf`, `Test_ClockGet.pf`, `Test_VariableSpec_private.pf`, `Test_VectorBasisKind.pf`, `Test_QuantityType.pf`, `Test_ConservationType.pf`, `Test_PolymorphicSetMirror.pf` | §1.3 `registry/` (`StateRegistry`/`Couplers`), §1.4 `connection/` (`ConnectionPt`/`VirtualConnectionPt`), field-dictionary integration. |

All seven depend on `proto_extdata_gc proto_stat_gc` (fixture gridcomps
built from `superstructure/generic/tests/gridcomps/`:
`ProtoExtDataGC.F90`, `ProtoStatGridComp.F90`) and link a shared
`MAPL.generic.test_common` library (`MockUserGridComp.F90`,
`MockAspect.F90`, `GraphBuilderTestSupport.F90`) built once to avoid
parallel-build race conditions across the seven executables.

### 4.3 Correction to the generic `mapl-build`/`mapl-testing` skills

Those skills reference a `generic3g/tests` build subdirectory and
`MAPL.generic3g.*` ctest target names. **This checkout uses
`superstructure/generic/` and `MAPL.generic.*` target names** —
confirmed by reading both `superstructure/generic/CMakeLists.txt` and
`superstructure/generic/tests/CMakeLists.txt` directly, and already
documented from the graph side in `docs/graph/ARCHITECTURE.md` §4.4.
Use the seven real target names in §4.2 above, not the skill's
`generic3g` examples, on this branch/repo state.

## 5. Previous architectural decisions (this layer, non-graph)

Each item below cites the `openspec/changes/archive/` directory that
is the primary record; read that directory's `proposal.md`/`design.md`
directly before making a related change — this is a pointer list, not
a restatement of the reasoning.

- **Standard-name enforcement**
  (`openspec/changes/archive/2026-09-24-enforce-standard-name-convention/`,
  landed spec: `openspec/specs/generic/standard-name-enforcement/spec.md`).
  A connected Import/Export pair's `standard_name` values must agree
  (when both are declared and non-wildcard); disagreement is fatal
  under strict `ValidationMode`, a warning otherwise. Matching
  `standard_name` also back-fills `long_name`/`units` from the
  `FieldDictionary`. Implemented on the legacy side via
  `StandardNameAspect.F90`'s `matches()`; the graph-native
  `StandardNameCharacteristic` (landed Phase 12, `graph/
  characteristics/`) implements the equivalent detection but — per
  `docs/graph/ARCHITECTURE.md` §7's "Surprise #1" — has no real
  `build_transform` repair step yet (any genuine mismatch on this axis
  fails loudly rather than converting).
- **Field-dictionary use in scenario tests**
  (`openspec/changes/archive/2026-09-24-use-field-dictionary-in-scenario-tests/`).
  `MAPL.generic.scenarios` runs under strict `ValidationMode`
  process-wide (`Initialize_strict`); every other pFUnit suite here
  still uses the permissive default (`Initialize`). `ValidationMode` is
  a process-wide singleton set once at `EXTRA_INITIALIZE` time — it
  cannot vary per-test within one executable, which is why
  `MAPL.generic.components.quiet` had to be split into its own
  executable (§4.2) rather than just filtering within `.components`.
- **Expression vertical-dim-spec resolution**
  (`openspec/changes/archive/2026-09-22-derive-expression-vertical-dim-spec/`,
  landed spec: `openspec/specs/generic/expression-vertical-dim-resolution/
  spec.md`). An `expression` item's unspecified vertical dim spec
  resolves from its referenced variables' agreeing stagger, treated as
  a resolution *candidate*, not a fixed authoritative value — declaring
  `vertical_dim_spec` explicitly on the expression item is required
  only when it references no variables.
- **Expression field-name propagation**
  (`openspec/changes/archive/2026-09-21-expression-field-name-propagation/`,
  landed spec: `openspec/specs/generic/field-name-propagation/spec.md`).
  Governs how a referenced field's real name flows into an expression's
  own formula evaluation (`EvalTransform.F90`).
- **Extension-registry visibility**
  (`openspec/changes/archive/2026-09-17-extension-registry-visibility/`).
  Governs which `ExtensionFamily` members (primary vs. derived specs)
  are visible under which short name in a user-facing `ESMF_State` —
  the `ExtensionFamily.F90` header comment ("primary/base item spec is
  tracked separately to enable control of which will appear in user
  states") is this decision's direct implementation.
- **Internal/memory checkpoint**
  (`openspec/changes/archive/2026-08-26-internal-checkpoint/`,
  `openspec/changes/archive/2026-08-26-add-memory-checkpoint/`, landed
  spec: `openspec/specs/internal-checkpoint/spec.md`). Defines restart
  I/O for `internal` state specifically (as opposed to import/export),
  and an in-memory (non-file) checkpoint mechanism
  (`ensure_memory_checkpoint.F90`/`get_memory_checkpoint_state.F90`,
  `RestartHandler.F90`). `CheckpointControls.F90`'s four independent
  flags (`import`/`export`/`internal`/`bootstrap`) are this decision's
  direct data representation.
- **GriddedComponentDriver integration lifecycle**
  (`openspec/changes/archive/2026-09-20-griddedcomponentdriver-integration-lifecycle/`).
  Governs how a child's `GriddedComponentDriver` (owned by
  `superstructure/component/`, §2.5) participates in this layer's own
  init/run phase sequencing — relevant whenever touching
  `OuterMetaComponent/run_children.F90`,
  `OuterMetaComponentDriverResolver.F90`, or `get_child_driver.F90`.
- **Grid blurring**
  (`openspec/changes/archive/2026-09-15-grid-blurring/`). A
  geometry-aspect-adjacent capability; read that proposal directly if
  touching `GeomAspect.F90`/`GeomAspect/make_transform.F90`.
- **Component-hierarchy foundation**
  (`openspec/changes/archive/2026-09-15-component-hierarchy-foundation/`).
  The original design establishing `OuterMetaComponent` owning both a
  `ComponentGraph` and a legacy `StateRegistry` side by side — the
  architectural decision that makes §2.1-§2.4 of this file (and the
  whole graph/legacy coexistence this layer implements) possible in
  the first place. Read this one first if the "why does
  `OuterMetaComponent` own both?" question comes up.

## 6. Completed phases and outstanding work (cross-reference)

This layer (everything in this file, i.e. *not* `graph/`) is **stable,
legacy, and still load-bearing** — it is not itself undergoing active
phase-based development the way `graph/` is. The relevant "work" from
this file's perspective is tracked entirely on the *graph* side, as
prerequisites for eventually retiring parts of this layer. See
`docs/graph/ARCHITECTURE.md` §6 for the authoritative, actively
maintained phase list. The two facts from that list most relevant to
this file:

- **Phase 6a (legacy `StateRegistry` retirement) has an explicit,
  five-item prerequisite list** (`docs/graph/spec/
  20-implementation-roadmap.md` §20.4.11) that has **not** been
  satisfied as of this file's writing — do not treat any part of
  `registry/`, `connection/`, or `specs/`'s aspect hierarchy as
  deprecated or scheduled for near-term removal. There is no removal
  date; only a prerequisite list, still in progress on the graph side.
- **Phase 7a (Bracket→Field shape dispatch) is landed** (confirmed
  this session by reading HEAD commit `8960f8ba`'s diff and
  `openspec/changes/archive/2026-10-20-phase-7a-shape-dispatch-wiring/`'s
  own `tasks.md`, 47/47 checked) — the graph-native path can now
  resolve a `BRACKET`-declared export into a `FIELD`-declared import
  without routing through this layer's own `BracketClassAspect`/
  `TimeInterpolateTransform`. **Phase 7b (Expression) and 7c
  (Wildcard) remain unimplemented** — `ExpressionClassAspect.F90` and
  `WildcardClassAspect.F90` (and their `transforms/EvalTransform.F90`/
  legacy wildcard-matching counterparts) are still the only working
  implementation of those two item classes. `VectorBracketClassAspect`
  (vector-shaped bracket→vector) is explicitly out of scope for Phase
  7a too (proposal.md's own "Why" section) and remains legacy-only.

No other outstanding work specific to this file's directories was
found in this pass's `openspec/changes/` survey — every other archived
change touching `specs/`/`registry/`/`connection/`/`transforms/`/
`vertical/`/`ComponentSpecParser/` (§5 above) is fully landed, with no
open follow-up filed against it.
