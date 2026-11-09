# MAPL3 Generic Layer Architecture: Implementation Reference

**Audience:** coding agents and developers working on `superstructure/generic/`.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Parent: [`docs/ARCHITECTURE.md`](../ARCHITECTURE.md); repository
agent rules: [`AGENTS.md`](../../AGENTS.md); scoped rules:
[`superstructure/generic/AGENTS.md`](../../superstructure/generic/AGENTS.md);
index: [`README.md`](README.md).

**Relationship to other documents:**
- `openspec/specs/generic/*/spec.md`: requirement/scenario specs for
  landed generic-layer capabilities (`field-name-propagation`,
  `standard-name-enforcement`, `expression-vertical-dim-resolution`).
  Authoritative for the behavior they cover.
- `openspec/specs/internal-checkpoint/spec.md`: landed checkpoint
  (restart) requirements.
- `openspec/changes/archive/`: landed, folded-in proposals. Each
  directory's `proposal.md`/`design.md` is the primary record of *why* a
  piece of this layer looks the way it does; this file cites them rather
  than restating their reasoning.
- The component-graph layer (an explicit dependency graph alongside this
  layer's `StateRegistry`) is not on this line; nothing here describes it.
- **This file**: a navigational layer on top of the above, for quick
  orientation. Where this file and the sources it cites disagree, the
  sources win.

All statements below were checked against the tree at the commit above
(`git ls-files`, reading source and `CMakeLists.txt` files) unless
labelled *Inferred* or *Unverified*. No build or test was run for this
document.

## 0. What is "the generic layer," and what is out of scope here

`superstructure/generic/` (CMake target `MAPL.generic`) is the single
library that implements:
- the user-facing `GenericSetServices`/`GenericGridComp` entry points
  every MAPL gridded component goes through (`GenericGridComp.F90`),
- the private per-component state type (`OuterMetaComponent`) and its 54
  lifecycle-phase submodules,
- the YAML-driven `ComponentSpec` description of a component's
  variables, children, and connections, and its parser,
- the imperative `StateRegistry`/`Connection`/`ExtensionFamily` coupler
  machinery that resolves those connections and builds automatic
  unit/typekind/regrid couplers,
- the `ClassAspect`/`StateItemAspect` family of per-variable metadata
  descriptors the coupler machinery dispatches on,
- the `ExtensionTransform` family of concrete data converters (units,
  typekind, regrid, time-interpolate, normalize, conservative-regrid),
- vertical-grid support (`ModelVerticalGrid`, `FixedLevelsVerticalGrid`).

**Noted but not detailed:** `superstructure/component/` (sibling
directory: `GriddedComponentDriver`/`ComponentDriver`; the generic layer
depends on it, not the reverse; see §2.4) and `infrastructure/` (for
example `infrastructure/vertical/vertical_grid/`, which owns
`VerticalGrid`/`VerticalGridSpec`/`VerticalGridFactory`, consumed but not
defined by `superstructure/generic/vertical/`).

## 1. Module map

### 1.1 Top-level files (`superstructure/generic/*.F90`)

| File | Module | Lines | Responsibility |
|---|---|---|---|
| `API.F90` | `mapl_generic_api` | 124 | Umbrella module that `use`s `mapl_Generic_mod` and re-`public`s selected symbols (`mapl_GridCompAddVarSpec`, `mapl_GridCompGet`, ...). |
| `Generic.F90` | `mapl_Generic` | 20 | A second umbrella (re-exports `OuterMetaComponent`, `MAPL_GridCompCreate`, vertical-grid types, `ChildSpec`, `UserSetServices`, OpenMP support). It is not in the `srcs` list of `CMakeLists.txt` and no `use mapl_Generic` was found in the repository by grep: not compiled, apparently unused. *Inferred:* a leftover. |
| `MAPL_Generic.F90` | `mapl_Generic_mod` | 1332 | The long-standing facade: public `GridComp*` procedures (`GridCompAddVarSpec`, `GridCompAddSpec`, `GridCompGet`, `GridCompAddChild`, `GridCompRunChild(ren)`, `GridCompAddConnection`, `GridCompConnectAll`, `GridCompSetGeometry`, `GridCompGetCheckpointDir`, ...) delegating to `OuterMetaComponent` methods. |
| `GenericGrid.F90` | `mapl3_GenericGrid` | 14 | Defines the type `GenericGrid` (an `ESMF_Grid` and an `ESMF_LocStream`); `use`d by `InnerMetaComponent.F90`. |
| `GenericGridComp.F90` | `mapl_GenericGridComp_mod` | 378 | `GenericSetServices`/`MAPL_GridCompCreate`: the ESMF wiring. Its header comment lists the generic initialize phases (`MAPL_PROPAGATE_GRID`, `MAPL_ADVERTISE`, `MAPL_REALIZE`) that a user gridcomp's own phases can supplement with a `_PRE`/`_POST` suffix; phase constants come from `mapl_GenericPhases_mod` (`enums/GenericPhases.F90`). |
| `ComponentSpecParser.F90` + `ComponentSpecParser/` (9 files, 1034 lines) | `mapl_ComponentSpecParser_mod` | 146 (+ submodules) | Interface block of the YAML `ComponentSpec` parser (children, connections, variable specs, geometry spec, set-services, timespec). See §1.5. |
| `to_itemtype.F90` | *(submodule of `mapl_ComponentSpecParser_mod`)* | 56 | **Not compiled.** A duplicate of `ComponentSpecParser/to_itemtype.F90` (54 lines; the one wired in `CMakeLists.txt` through `esma_add_fortran_submodules`). The top-level copy has one extra `case` arm (`'vector_bracket'` to `MAPL_STATEITEM_VECTORBRACKET`) and is referenced by no `CMakeLists.txt`. Editing it has no effect. |
| `UserSetServices.F90` | `mapl_UserSetServices_mod` | 238 | Wraps a user's `SetServices` procedure so `OuterMetaComponent` can invoke it uniformly. |
| `MethodPhasesMap.F90` | `mapl_MethodPhasesMap_private_mod`, `mapl_MethodPhasesMapUtils_mod`, `mapl_MethodPhasesMap_mod` | 191 | Maps ESMF method/phase names and indices for initialize/run. |
| `InnerMetaComponent.F90` | `mapl_InnerMetaComponent_mod` | 84 | The inner (user) gridcomp's private state, distinct from `OuterMetaComponent` (the wrapping component MAPL creates). |
| `OuterMetaComponent.F90` + `OuterMetaComponent/` (54 files, 2582 lines) | `mapl_OuterMetaComponent_mod` | 627 (+ submodules) | The central component-hierarchy type. See §1.2. |
| `MAPL3_Deprecated.F90` | `mapl_Deprecated_mod` | 13 | Marker module for deprecated-symbol bookkeeping. |
| `RestartHandler.F90` | `mapl_RestartHandler_mod` | 306 | Restart read/write orchestration, used by `OuterMetaComponent/write_restart.F90`, `read_restart.F90` and `initialize_read_restart.F90`. |
| `RunEntryPoint.F90`, `EntryPointVector.F90` | `mapl_RunEntryPoint_mod`, `mapl_EntryPointVector_mod` | 11, 15 | Small supporting types for registering custom run-phase entry points (`OuterMetaComponent/set_entry_point.F90`). |
| `OpenMP_Support.F90` | `mapl_OpenMP_Support_mod` | 461 | OpenMP thread/region helpers; `API.F90` re-exports only `mapl_find_bounds`, `mapl_get_num_threads` and `mapl_get_current_thread` (renamed from `find_bounds`, `get_num_threads`, `get_current_thread`). |

### 1.2 `OuterMetaComponent/`: the component-hierarchy type and its lifecycle

`OuterMetaComponent.F90` declares the type's private state and every
type-bound-procedure interface; the 54 files in `OuterMetaComponent/` are
its submodules (one procedure or tightly related group per file),
totaling 2582 lines. All 54 are listed in the first
`esma_add_fortran_submodules` call in `CMakeLists.txt`; that list is the
authoritative inventory.

Key state owned by `OuterMetaComponent` (its own `use` list and
declarations in `OuterMetaComponent.F90`):
- `self_gridcomp` and `user_gc_driver` (a `GriddedComponentDriver`), the
  `user_setservices`, timestep/offset and `user_phases_map`;
- `hconfig`;
- `geom_id`, `geom` and `vertical_grid` (geometry state);
- `memory_checkpoint`, a nested `ESMF_State` created lazily by
  `ensure_memory_checkpoint_`;
- `children`, a `GriddedComponentDriverMap` (drivers are owned by sibling
  `superstructure/component/`, §2.4);
- `registry`, a `StateRegistry` (`mapl_StateRegistry_mod`);
- the `ComponentSpec` (`component_spec`), `CheckpointControls`, and a
  logger and profiler.

Representative submodule groupings (not exhaustive):

| Group | Files | Covers |
|---|---|---|
| Construction/teardown | `new_outer_meta.F90`, `init_meta.F90`, `free_outer_meta.F90`, `attach_outer_meta.F90`, `get_outer_meta_from_outer_gc.F90`, `SetServices.F90` | Allocating and attaching an `OuterMetaComponent` to an ESMF `GridComp`'s private state; the generic `SetServices`. |
| Child management | `add_child_by_spec.F90`, `get_num_children.F90`, `get_child_name.F90`, `get_child_by_name.F90`, `apply_to_children_custom.F90` | Building and querying the child hierarchy (`add_child_by_spec.F90` parses the child's YAML with `mapl_ComponentSpecParser_mod`). |
| Initialize phases | `initialize_advertise.F90`, `initialize_modify_advertised.F90`, `initialize_realize_provided.F90`, `initialize_accept_transfer.F90`, `initialize_realize_accepted.F90`, `initialize_geom_a.F90`, `initialize_geom_b.F90`, `initialize_set_clock.F90`, `initialize_user.F90`, `initialize_read_restart.F90` | The ordered generic initialize sub-phases; see §2.3 for the call order inside `initialize_advertise.F90`. |
| Run dispatch | `run_children.F90`, `run_child_by_name.F90`, `run_custom.F90`, `run_user.F90`, `run_clock_advance.F90`, `recurse.F90` | `recurse_` (`recurse.F90`) calls `child%initialize(phase_idx=...)` for each child; `recurse_write_restart_` does the same for restart writing. |
| Connections | `connect_all.F90`, `advertise_variable.F90` | `connect_all` (reached from `MAPL_Generic.F90`, `gridcomp_connect_all`) and `advertise_variable` (used by `self_advertise` in `initialize_advertise.F90`). |
| Geometry/vertical grid | `set_geom.F90`, `get_geom.F90`, `has_geom.F90`, `propagate_geom_to_children.F90`, `set_vertical_grid.F90`, `get_vertical_grid.F90` | `propagate_geom_to_children` pushes a parent's geometry to children whose spec kind is `GEOMETRY_FROM_PARENT`. |
| Restart | `write_restart.F90`, `read_restart.F90`, `get_checkpoint_dir.F90`, `ensure_memory_checkpoint.F90`, `get_memory_checkpoint_state.F90` | Delegates to `RestartHandler.F90`; uses `GENERIC_INTERNAL_WRITE_RESTART`/`GENERIC_INTERNAL_READ_RESTART` from `mapl_GenericPhases_mod`; see §5. |
| Accessors | `get_name.F90`, `get_gridcomp.F90`, `get_hconfig.F90`, `set_hconfig.F90`, `get_phases.F90`, `get_internal_state.F90`, `get_logger.F90`, `get_registry.F90`, `get_component_spec.F90`, `get_user_gc_driver.F90`, `set_entry_point.F90`, `start_timer.F90`, `stop_timer.F90`, `finalize.F90` | Field accessors and small utilities. |

### 1.3 `registry/`: `StateRegistry` and extension bookkeeping (20 files, 1638 lines)

| File | Responsibility |
|---|---|
| `StateRegistry.F90` | The type declaration and all module-procedure interfaces. Holds per-component maps keyed by `VirtualConnectionPt`/`ActualConnectionPt`. |
| `StateRegistry_Lifecycle_smod.F90` | Construction, naming, existence queries (`has_virtual_pt`, `num_owned_items`). |
| `StateRegistry_Hierarchy_smod.F90` | Subregistry (child-registry) management: `add_subregistry`, `has_subregistry`, `get_subregistry_by_name`, `get_subregistry_by_conn_pt`. |
| `StateRegistry_Extensions_smod.F90` | `add_virtual_pt`, `add_family`, `add_primary_spec`: the `ExtensionFamily` bookkeeping for a virtual connection point. |
| `StateRegistry_Propagation_smod.F90` | `propagate_unsatisfied_imports_{all,subregistry,virtual_pt}` and `propagate_exports_{all,subregistry,virtual_pt}`. |
| `StateRegistry_Actions_smod.F90` | `allocate_items`, `add_to_states` (populates a real `ESMF_State`/`ESMF_FieldBundle` from registry contents), `filter` (pattern-matching query, comment "for MatchConnection"). |
| `StateRegistry_SetGeometry_smod.F90` | `set_geometry`: pushes a geom/vertical-grid pair onto registered items through the aspects' set-geometry methods. |
| `AbstractRegistry.F90` | Abstract marker type, `StateRegistry`'s nominal parent. |
| `ExtensionFamily.F90` | Tracks the `StateItemSpec`s related by extension (one primary spec plus derived specs) for a `VirtualConnectionPt`; its header comment says the primary spec is tracked separately to control which appears in user states. |
| Container files: `ActualPtSpecPtrMap.F90`, `ActualPtComponentDriverMap.F90`, `VirtualPtStateItemPtrMap.F90`, `VirtualPtStateItemSpecMap.F90`, `ActualPtStateItemSpecMap.F90`, `StateItemVector.F90`, `RegistryPtr.F90`, `RegistryPtrMap.F90`, `ActualPtVector.F90`, `ActualPtVec_Map.F90`, `VirtualPtFamilyMap.F90` | gFTL map/vector instantiations backing the above. |

`StateRegistry` populates the user import, export and internal states:
`add_to_states` (called from `initialize_advertise.F90` with
`mode='user'`) calls `spec%add_to_state`, implemented in
`specs/FieldClassAspect.F90` (which calls `ESMF_StateAddReplace`) and
`specs/ServiceClassAspect.F90`. The memory-checkpoint states and the
restart-write states are built outside the registry
(`OuterMetaComponent/ensure_memory_checkpoint.F90`,
`OuterMetaComponent/write_restart.F90`).

### 1.4 `connection/`: the `Connection` family (10 files, 1256 lines)

| File | Responsibility |
|---|---|
| `Connection.F90` | Abstract base (`get_source`/`get_destination`/`activate`/`connect`). |
| `ConnectionPt.F90`, `ConnectionPtVector.F90` | A (component, item-name) address and its vector container. |
| `VirtualConnectionPt.F90`, `VirtualConnectionPtVector.F90` | An unresolved connection endpoint (declared name). |
| `ActualConnectionPt.F90` | A resolved endpoint. |
| `SimpleConnection.F90` | Explicit, named source and destination. |
| `MatchConnection.F90` | Matches by pattern/wildcard rather than an explicit pair (uses `StateRegistry%filter`). |
| `ReexportConnection.F90` | Re-exports an already-connected item. |
| `ConnectionVector.F90` | gFTL vector of `class(Connection)`. |

`SimpleConnection.F90` and the other concrete types `use
mapl_StateRegistry_mod`: the connection family and the registry reference
each other's modules (§2.1). `initialize_advertise.F90`
(`process_connections`) iterates `component_spec%connections` and calls
`activate(registry)` on each.

### 1.5 `ComponentSpecParser/`: YAML to `ComponentSpec` (9 files, 1034 lines)

| File | Responsibility |
|---|---|
| `parse_component_spec.F90` | Top-level entry: one component's YAML block into a `ComponentSpec`. |
| `parse_children.F90`, `parse_child.F90` | The `children:` list into `ChildSpec`s. |
| `parse_connections.F90` | The `connections:` list into `Connection` objects. |
| `parse_var_specs.F90` | The variable (import/export/internal) list into `VariableSpec`s; calls `to_itemtype` per entry. |
| `parse_geometry_spec.F90` | The `geometry:` block into a `GeometrySpec`. |
| `parse_setservices.F90` | Resolves the named user `SetServices`. |
| `parse_timespec.F90` | Run-phase timestep/offset configuration. |
| `to_itemtype.F90` | Classifies a variable's item type from its YAML attributes. This is the compiled copy; see the top-level duplicate in §1.1. |

### 1.6 `specs/`: the spec and aspect type hierarchy (40 `.F90` files, 10,740 lines, including `GeomAspect/` and `VerticalGridAspect/`; largest subdirectory)

This is the metadata model the coupler machinery dispatches on. Three
loosely layered families.

**Spec containers** (what a component declares):
- `ComponentSpec.F90`: a component's full declaration (children,
  variables, connections, geometry spec, set-services reference).
- `ChildSpec.F90` + `ChildSpecMap.F90`: one child's declaration
  (`user_setservices`, `hconfig`, timestep, offset).
- `VariableSpec.F90` (plus `VariableSpecVector.F90`,
  `VariableSpec_private.F90`): one import/export/internal's declaration.
- `GeometrySpec.F90`, `GridSpec.F90`: declared geometry.
- `CheckpointControls.F90`: four independent logical flags
  (`import`/`export`/`internal`/`bootstrap`, all default `.false.`)
  controlling which state categories participate in restart I/O.

**Resolved state-item model** (what a connection resolves against):
- `StateItemSpec.F90`: the resolved representation of one state item: an
  `AspectMap` of its `StateItemAspect` values, its intent, its
  dependencies, and (comment "merged from StateItemExtension") producer
  and consumer tracking: a `ComponentDriverVector` of `consumers` and a
  `producer` pointer.
- `StateItemSpecVector.F90`, `StateItemSpecPtrVector.F90`,
  `StateItemSpecMap.F90`: containers.

**`StateItemAspect`/`ClassAspect` family** (per-axis metadata and the
connection-compatibility rules between them):
- `StateItemAspect.F90`: abstract base. Its header comment documents the
  table of allowed connections between `simple`/`mirror`/`timedep`
  aspects and when a coupler is required. Read it before changing any
  `can_connect_to`/`matches` override.
- `ClassAspect.F90`: abstract subclass for a state item's structural
  class. Concrete subclasses: `FieldClassAspect.F90` (+
  `FieldClassAspect_smod.F90`), `FieldBundleClassAspect.F90`,
  `StateClassAspect.F90`, `VectorClassAspect.F90` (+
  `ActualPtFieldAspectMap.F90`), `WildcardClassAspect.F90`,
  `ServiceClassAspect.F90`, `BracketClassAspect.F90` and
  `VectorBracketClassAspect.F90` (time-interpolation brackets, converted
  by `transforms/TimeInterpolateTransform.F90`), and
  `ExpressionClassAspect.F90` (formula-driven items; see
  `openspec/specs/generic/expression-vertical-dim-resolution/spec.md`).
- Value aspects, one per axis: `UnitsAspect.F90`, `TypekindAspect.F90`,
  `UngriddedDimsAspect.F90`, `GeomAspect.F90` (+
  `GeomAspect/make_transform.F90`), `VerticalGridAspect.F90` (+
  `VerticalGridAspect/make_transform.F90`), `AttributesAspect.F90`,
  `StandardNameAspect.F90`, `QuantityTypeAspect.F90`,
  `ConservationAspect.F90`, `NormalizationAspect.F90`.
- `AspectId.F90`, `AspectStatus.F90`: ids and statuses used by `AspectMap`
  (`AspectMap` itself is exercised by `tests/Test_AspectMap.pf`).

### 1.7 `transforms/`: the `ExtensionTransform` family (15 `.F90` files + `notes.md`, 3211 lines)

| File | Responsibility |
|---|---|
| `TransformId.F90` | Id/key type identifying a transform instance. |
| `ExtensionTransform.F90` | Abstract base of the coupler's transforms. |
| `NullTransform.F90` | No-op placeholder so a function returning an `ExtensionTransform` can allocate a return value under error conditions (its own comment). |
| `ExtendTransform.F90` | Near-no-op transform for an export that mirrors an aspect value. |
| `TransformVector.F90` | gFTL vector of `class(ExtensionTransform)`. |
| `RegridTransform.F90` | Horizontal regrid. |
| `VerticalRegridTransform.F90` | Vertical regrid, using `infrastructure/vertical/vertical/` numerics (`VerticalConservativeMap`, `VerticalLinearMap`, `CSR_SparseMatrix`). |
| `CopyTransform.F90` | Plain field copy. |
| `ConvertUnitsTransform.F90` | Units conversion (udunits2). |
| `NormalizationTransform.F90` | Normalization-aspect-driven rescaling. |
| `TimeInterpolateTransform.F90` | Bracket (two fields plus weights) to ordinary field. |
| `EvalTransform.F90` | Expression formula evaluation. |
| `ExtensionTransformUtils.F90` | Shared helpers. |
| `CouplerMetaComponent.F90`, `GenericCoupler.F90` | The ESMF gridcomp wrapper around a transform (a coupler as a child gridcomp). Listed in `transforms/CMakeLists.txt`. |
| `notes.md` | Free-form developer notes (connection/extension walk-through). |

`couplers/` contains only a `CMakeLists.txt` with the comment
`# Sources moved to transforms/: CouplerMetaComponent.F90, GenericCoupler.F90`.
It is still listed by `add_subdirectory(couplers)` in the parent
`CMakeLists.txt` and contributes no sources.

### 1.8 `vertical/`: layer-specific vertical grids (2 files, 905 lines)

| File | Responsibility |
|---|---|
| `ModelVerticalGrid.F90` | `mapl_ModelVerticalGrid_mod`: a vertical grid built from a component's declared levels; its `use` list pulls in `StateRegistry`, `ExtensionFamily` and most of the `specs/` aspects. |
| `FixedLevelsVerticalGrid.F90` | `mapl_FixedLevelsVerticalGrid_mod`: a grid with an explicit fixed level list, built on `mapl_VerticalGridSpec_mod`/`mapl_VerticalGridFactory_mod` from `infrastructure/vertical/vertical_grid/`. |

The general vertical-grid machinery (`VerticalGrid`, `VerticalGridSpec`,
`VerticalGridFactory`, regrid maps) is in
`infrastructure/vertical/vertical_grid/` and
`infrastructure/vertical/vertical/`, not in this directory.

## 2. Important interfaces and dependencies

### 2.1 Dependency direction among the subdirectories (from `use` statements)

- `specs/` aspects `use` transform modules to build couplers
  (`UnitsAspect.F90` uses `mapl_ConvertUnitsTransform_mod`,
  `mapl_ExtensionTransform_mod`, `mapl_NullTransform_mod`; the
  `make_transform.F90` submodules of `GeomAspect` and `VerticalGridAspect`
  produce transforms, and the former uses `mapl_ModelVerticalGrid_mod`).
  `transforms/ExtensionTransform.F90` uses `mapl_AspectId_mod` from
  `specs/`: the two directories reference each other.
- `registry/` uses `specs/` modules (7 of its files use
  `mapl_StateItemSpec_mod` or an aspect module) and `connection/` modules.
- `connection/` types `use mapl_StateRegistry_mod` (for example
  `SimpleConnection.F90`).
- `specs/ExpressionClassAspect.F90`, `ServiceClassAspect.F90` and
  `VariableSpec.F90` use `StateRegistry` or `ExtensionFamily`.
- `vertical/ModelVerticalGrid.F90` uses `StateRegistry` and the aspects.
- `ComponentSpecParser` is used by `OuterMetaComponent/add_child_by_spec.F90`,
  `OuterMetaComponent/SetServices.F90` and `MAPL_Generic.F90`.

Because of these mutual references, the subdirectories are not a strict
layering; Fortran module dependency order is resolved by the build.

### 2.2 `StateItemSpec`/`AspectMap`: the coupler's central value type

A `StateItemSpec` (`specs/StateItemSpec.F90`) is the resolved
representation of one import or export. It carries an `AspectMap` keyed by
`AspectId` of `StateItemAspect` values (one per axis), its intent, its own
declared dependencies, and `producer`/`consumers` driver tracking (see
§1.6).

Connection compatibility between two `StateItemSpec`s is decided
per aspect, through each aspect's `can_connect_to`/`matches` methods, not
by a single comparison. The header comment of `specs/StateItemAspect.F90`
is the statement of the cross-aspect-kind rules.

### 2.3 Initialize call order

`OuterMetaComponent/initialize_advertise.F90`, `initialize_advertise`:

```
recurse(this, phase_idx=MAPL_GENERIC_INIT_ADVERTISE)  ! children first
self_advertise(this)                    ! advertise_variable per VariableSpec
this%run_custom(ESMF_METHOD_INITIALIZE, 'GENERIC::INIT_ADVERTISE')
process_connections(this)               ! Connection%activate(registry)
registry%propagate_unsatisfied_imports
registry%propagate_exports
registry%add_to_states(user_states, mode='user')
```

The profiler (`DistributedProfiler`, `MpiTimerGauge`) is started at the top
of the routine. Other phases are in their own submodules (§1.2).

### 2.4 `superstructure/component/`: a dependency

`GriddedComponentDriver`, `ComponentDriver`, `ComponentDriverVector`,
`GriddedComponentDriverMap`, `GriddedComponentDriverVector`,
`ComponentDriverPtrVector` are defined in `superstructure/component/`, a
sibling of `superstructure/generic/`. `OuterMetaComponent` owns a
`GriddedComponentDriverMap` of its children and `StateItemSpec` tracks
`ComponentDriver` pointers, so the generic layer depends on
`superstructure/component/` (`MAPL.component` is a dependency in
`CMakeLists.txt`). `superstructure/CMakeLists.txt` adds `state`,
`component`, then `generic`. `superstructure/component/` is not documented
by this file.

## 3. `CMakeLists.txt` structure

`superstructure/generic/CMakeLists.txt` defines the `MAPL.generic` library
(`esma_set_this (OVERRIDE MAPL.generic)`, `TYPE SHARED`) with:
- a top-level `srcs` list (`API.F90`, `GenericGrid.F90`,
  `ComponentSpecParser.F90`, `UserSetServices.F90`, `MethodPhasesMap.F90`,
  `InnerMetaComponent.F90`, `OuterMetaComponent.F90`,
  `GenericGridComp.F90`, `MAPL_Generic.F90`, `MAPL3_Deprecated.F90`,
  `RestartHandler.F90`, `RunEntryPoint.F90`, `EntryPointVector.F90`,
  `OpenMP_Support.F90`; `Generic.F90` and the top-level `to_itemtype.F90`
  are not in it);
- `add_subdirectory()` for `specs` (with binary directory
  `generic_specs`; the file's own long comment explains a Spack/Intel
  `ifort`-under-GCC collision with a directory named `specs`),
  `registry`, `connection`, `transforms`, `couplers` (no sources),
  `vertical`; each subdirectory `CMakeLists.txt` calls
  `target_sources(MAPL.generic PRIVATE ...)`;
- two `esma_add_fortran_submodules()` calls: one for `OuterMetaComponent`
  (54 files) and one for `ComponentSpecParser` (9 files, including
  `to_itemtype.F90`). The macro is defined in the ESMA_cmake dependency,
  outside this repository. *Unverified:* its documented purpose is to avoid
  object-file name collisions between submodule directories.
- library dependencies: `MAPL.enums`, `MAPL.regridder_mgr`, `MAPL.geom`,
  `MAPL.vertical_grid`, `MAPL.GeomIO`, `MAPL.esmf`, `MAPL.field`,
  `MAPL.state`, `MAPL.utils`, `MAPL.vertical`, `MAPL.component`,
  `MAPL.field_bundle`, `MAPL.infrastructure`, plus `ESMF::ESMF`,
  `NetCDF::NetCDF_Fortran`, `udunits2f`, `PFLOGGER::pflogger`,
  `GFTL_SHARED::gftl-shared-v2`, `GFTL::gftl-v2`;
- `add_subdirectory(tests EXCLUDE_FROM_ALL)` guarded by `if (PFUNIT_FOUND)`.

## 4. Tests

`superstructure/generic/tests/CMakeLists.txt` defines six pFUnit ctest
targets, built by the `build-tests` target. All carry `LABELS "ESSENTIAL"`
and `MAX_PES 4`, and depend on `proto_extdata_gc proto_stat_gc` (fixture
gridcomps built from `tests/gridcomps/`: `ProtoExtDataGC.F90`,
`ProtoStatGridComp.F90`). All link the shared `MAPL.generic.test_common`
library (`MockUserGridComp.F90`, `MockAspect.F90`).

| Target | Test sources | Covers |
|---|---|---|
| `MAPL.generic.scenarios` | `Test_Scenarios.pf`, `Test_MemoryCheckpoint.pf` | End-to-end YAML scenario fixtures (copied from `tests/scenarios/`, 33 scenario directories) across the full initialize lifecycle. Runs under strict field-dictionary `ValidationMode` (`EXTRA_INITIALIZE Initialize_strict`); the other suites use `Initialize`. Also depends on the `copy_scenarios` target. |
| `MAPL.generic.transforms` | `Test_TimeInterpolateTransform.pf`, `Test_ConvertUnitsTransform.pf`, `Test_CopyTransform.pf`, `Test_ExtensionTransform.pf`, `Test_ExtensionTransformUtils.pf`, `Test_2DConservativeRegrid.pf`, `Test_NormalizationTransform.pf`, `Test_3DConservativeMixingRatio.pf`, `Test_IntegratedNormalization.pf`, `Test_RegridTransform.pf` | §1.7 `transforms/`. |
| `MAPL.generic.vertical` | `Test_VerticalRegridTransform.pf`, `Test_ModelVerticalGrid.pf`, `Test_VerticalLinearMap.pf`, `Test_VerticalConservativeMap.pf` | §1.8 `vertical/` and `infrastructure/vertical/` numerics. |
| `MAPL.generic.aspects` | `Test_BaseAspect.pf`, `Test_AspectMap.pf`, `Test_Aspects.pf`, `Test_TypekindAspect.pf`, `Test_UngriddedDimsAspect.pf`, `Test_BracketClassAspect.pf`, `Test_VectorBracketClassAspect.pf`, `Test_UnitsAspect.pf`, `Test_QuantityTypeAspect.pf`, `Test_GeomAspect.pf`, `Test_ConservationAspect.pf`, `Test_NormalizationAspect.pf`, `Test_ExpressionClassAspect.pf`, `Test_VerticalGridAspect_NoneMatch.pf` | §1.6 aspect family. |
| `MAPL.generic.components` | `Test_GenericGridComp.pf`, `Test_ComponentSpec.pf`, `Test_ComponentSpecParser.pf`, `Test_WriteYaml.pf`, `Test_ExtensionFamily.pf` | §1.5 parser, `GenericGridComp`, `ExtensionFamily`. |
| `MAPL.generic.core` | `Test_VirtualConnectionPt.pf`, `Test_ConnectionPt.pf`, `Test_FieldDictionary.pf`, `Test_FieldDictIntegration.pf`, `Test_StateRegistry.pf`, `Test_Couplers.pf`, `Test_CSR_SparseMatrix.pf`, `Test_timestep_propagation.pf`, `Test_propagate_time_varying.pf`, `Test_ClockGet.pf`, `Test_VariableSpec_private.pf`, `Test_VectorBasisKind.pf`, `Test_QuantityType.pf`, `Test_ConservationType.pf`, `Test_PolymorphicSetMirror.pf`, `Test_RestartBootstrap.pf` | §1.3 `registry/`, §1.4 `connection/`, field-dictionary integration, restart bootstrap. |

Test files present in `tests/` but not listed in any suite in
`tests/CMakeLists.txt`: `Test_AddVarSpec.pf`, `Test_BracketSpec.pf`,
`Test_FixedLevelsVerticalGrid.pf`, `Test_VerticalGridAspect.pf`.

## 5. Previous architectural decisions

Each item cites the `openspec/changes/archive/` directory that is the
primary record; read its `proposal.md`/`design.md` before making a related
change. This is a pointer list. Landed specs are under `openspec/specs/`.

- **Standard-name enforcement**
  (`openspec/changes/archive/2026-09-24-enforce-standard-name-convention/`;
  spec: `openspec/specs/generic/standard-name-enforcement/spec.md`). The
  header comment of `specs/StandardNameAspect.F90` says it enforces the
  standard-name convention between a connected Import and Export, modeled
  on `UnitsAspect`, with equal-or-unchecked/wildcard agreement. The
  detailed rules are in the spec.
- **Field-dictionary use in scenario tests**
  (`openspec/changes/archive/2026-09-24-use-field-dictionary-in-scenario-tests/`).
  `MAPL.generic.scenarios` runs under strict `ValidationMode`
  (`Initialize_strict`); the other suites use the permissive default
  (`Initialize`) (`tests/CMakeLists.txt`).
- **Expression vertical-dimension-spec resolution**
  (`openspec/changes/archive/2026-09-22-derive-expression-vertical-dim-spec/`;
  spec: `openspec/specs/generic/expression-vertical-dim-resolution/spec.md`).
  Concerns `specs/ExpressionClassAspect.F90`.
- **Field-name propagation**
  (`openspec/changes/archive/2026-09-21-field-name-propagation/`;
  spec: `openspec/specs/generic/field-name-propagation/spec.md`), and its
  expression counterpart
  (`openspec/changes/archive/2026-09-21-expression-field-name-propagation/`;
  `EvalTransform.F90`). The first proposal describes an Import's
  `ESMF_Field` as an `ESMF_NamedAlias` of the Export's field, with
  `standard_name`/`long_name` stored per alias.
- **Internal/memory checkpoint**
  (`openspec/changes/archive/2026-08-26-internal-checkpoint/`,
  `openspec/changes/archive/2026-08-26-add-memory-checkpoint/`; spec:
  `openspec/specs/internal-checkpoint/spec.md`). Phase-aware internal
  read/write restart hooks and an in-memory checkpoint
  (`ensure_memory_checkpoint.F90`, `get_memory_checkpoint_state.F90`,
  `RestartHandler.F90`); `CheckpointControls.F90` holds the
  `import`/`export`/`internal`/`bootstrap` flags.

The archived change `2026-09-15-grid-blurring` concerns coordinate
tolerance in the geometry manager (`openspec/specs/geom/` and
`openspec/specs/extdata/` hold the landed specs), not this layer.

## 6. Observations (noted, not fixed)

- `superstructure/generic/to_itemtype.F90` and `Generic.F90` are not
  compiled (§1.1).
- `couplers/` has no sources (§1.7).
- Four test files are not in any suite (§4).
- `MethodPhasesMap.F90` defines three modules in one file, the first named
  `mapl_MethodPhasesMap_private_mod`.
- The `specs/` and `transforms/` directories reference each other (§2.1).
