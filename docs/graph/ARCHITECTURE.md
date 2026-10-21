# MAPL3 Graph Architecture — Implementation Reference

**Audience:** coding agents and developers working on the graph-native
dependency architecture in this MAPL3 checkout.

**Scope of this file:** the tree as it is now. Session narratives,
debugging stories, and per-change task accounting live in
`docs/graph/history/`. Conventions live in `docs/graph/CONVENTIONS.md`;
legacy-to-graph vocabulary in `docs/graph/GLOSSARY.md`.

**Relationship to other docs (authority order if they disagree):**
1. The source (`superstructure/generic/`, `superstructure/generic/graph/`).
2. `openspec/specs/graph/*/spec.md` — landed, machine-checkable
   requirement/scenario specs, one directory per capability.
3. `docs/graph/spec/` — the narrative design specification (`01`-`20`).
   Can lag; `[RE-EVALUATED]` marks sections amended after implementation.
4. `docs/graph/*.md` topic files (see `docs/graph/README.md`).
5. **This file** — a navigational summary.

`openspec/changes/` currently contains only `archive/`; there is no
in-flight openspec change. Landed changes (including
`hierarchy-wide-component-graph`, phases 7a/7b/7c and 12) are under
`openspec/changes/archive/<date>-<name>/`. Active refactoring is planned
in `docs/graph/plans/separation-of-concerns.md`.

Facts below were verified against the tree on 2026-10-21 unless marked
**[ASSUMPTION]**.

## 1. Module map (`superstructure/generic/`)

### 1.1 Graph-neutral core (`superstructure/generic/graph/`)

89 non-test `.F90` files, about 12,300 lines (plus `graph/tests/`, 40
`.pf` files). No dependency on `OuterComponent`/`StateRegistry`
(REQ-CG-002). Layout: 34 files at the top level, and subdirectories
`characteristics/` (16), `transforms/` (7), `callback/` (6), `id_types/`
(5 + `IdTemplate.inc`), `containers/` (21 gFTL instantiations).

| File | Responsibility |
|---|---|
| `id_types/NodeId.F90`, `DependencyNetworkId.F90`, `PortId.F90`, `HierarchyId.F90`, `CallbackInterfaceId.F90` | Identity types generated from `id_types/IdTemplate.inc` |
| `HierarchyGraph.F90` | Hierarchy-wide `NodeId→GraphNode` and `DependencyNetworkId→DependencyNetwork` maps plus the shared id generators |
| `HierarchyResources.F90`, `HierarchyRegistry.F90` | Container for one `HierarchyGraph`; process-wide registry keyed by `HierarchyId` (`request_new_hierarchy`, `get_hierarchy_resources`, `release_hierarchy`) |
| `GraphNode.F90`, `BaseGraphNode.F90`, `StateItemNode.F90`, `OperationGraphNode.F90`, `MethodGraphNode.F90` | Node hierarchy |
| `GraphStateItem.F90`, `MAPLStateItemFlag.F90`, `StateItemVariantInfo.F90` | Payload of a `StateItemNode`: Field/FieldBundle/State handle (at most one allocated) **plus**, today, the characteristics map and the `declared_*`, `ungridded_dims`, `vertical_stagger` materialization inputs. Variant tag and its ESMF Info key. |
| `DependencyNetwork.F90` | Acyclic adjacency-list storage |
| `ComponentGraph.F90`, `ComponentGraph_DemandDrivenUpdate.F90` | Per-`OuterComponent` owner of resource index, import/export port tables, child-port bindings, lifecycle state; demand-driven `update()`. Node/network storage is in `HierarchyGraph`, reached via a stored `HierarchyId`. |
| `NodeRevision.F90`, `NodeLabel.F90` | Revision stamps; visualization labels |
| `PortSpec.F90`, `PortBindingKey.F90`, `PortBindingTable.F90`, `ArgumentSpec.F90`, `AccessSpec.F90` | Transform port declarations and external port-binding table; method argument specs |
| `ExtensionResolution.F90` | Mismatch detection (`find_mismatched_characteristics`), extension-chain build (`find_or_build_extension_chain`), reuse lookup |
| `ExtensionMaterialization.F90`, `CompositeStateMaterialization.F90`, `GeomProxyField.F90` | Real ESMF object creation for extension nodes, composite members, geometry nodes |
| `MethodInvocation*.F90`, `GridCompMethodInvocation.F90`, `StateMethodInvocation.F90`, `GridCompDriverPhaseInvoker.F90` | Method invocation layer |
| `callback/*` | Callback interface/method/state-binding data model and registry |
| `RouteHandleKey.F90` | Semantic key for route-handle reuse |
| `GraphExport.F90` | Graph-neutral DOT/JSON topology export |
| `GraphMode.F90` | `registry` / `graph` / `both` selector (`mapl_GraphMode_mod`) |
| `StringSortUtilities.F90` | Shared `sorted_names` helper |

#### Characteristics (`graph/characteristics/`)

Description axes of a state item, compared for mismatch detection and
(where a provider exists) repaired by a `Transform`.
`StateItemCharacteristic` (abstract; `ValueCharacteristic` and
`ReferenceCharacteristic` subtypes) has deferred `get_kind`,
`needs_extension_for`, `build_transform`, `get_signature`, and a
`CharacteristicStatus` (`INVALID`, `SPECIFIED`, `MIRRORED`, `UNCHECKED`,
`DEFERRED`).

| Characteristic | Kind constant | `build_transform` today |
|---|---|---|
| `PhysicalUnits` | `PHYSICAL_UNITS_CHARACTERISTIC_KIND` | Real: `UnitsConverterTransform` |
| `TypeKind` | `TYPE_KIND_…` | Real: `TypeKindConverterTransform` |
| `ItemShape` | `ITEM_SHAPE_…` | Real for `BRACKET`→`FIELD` only (`BracketTimeInterpolationTransform`); other pairings `_FAIL` naming both classes |
| `Geometry` | `GEOMETRY_…` | `_FAIL` — no horizontal-regrid `Transform` exists yet |
| `VerticalCoordinate` | `VERTICAL_COORDINATE_…` | `_FAIL` with three distinguishable messages (dimension-overlap outcomes); no vertical-regrid `Transform` yet |
| `Attributes`, `UngriddedDims`, `StandardName`, `QuantityType`, `Conservation`, `Normalization` | `…_CHARACTERISTIC_KIND` | `_FAIL` `'<Name>Characteristic: build_transform - no real provider exists for this axis'` — mismatch is detected, repair is a stub |

Other files: `CharacteristicStatus.F90`, `StateItemCharacteristicKind.F90`
(also `MOCK_CHARACTERISTIC_KIND`), `StateItemCharacteristicDetection.F90`,
`SetSharedCharacteristic.F90`.

A test that deliberately provokes a mismatch on a stub axis must consume
the exception first, e.g.
`@assertExceptionRaised('StandardNameCharacteristic: build_transform - no real provider exists for this axis')`.

#### Transforms (`graph/transforms/`)

`TransformBase`, `Transform` (abstract; `Transform` adds the compute
interface over named ports), `TransformGraphNode`, and four concrete
transforms: `UnitsConverterTransform`, `TypeKindConverterTransform`,
`BracketTimeInterpolationTransform` (built by
`ItemShapeCharacteristic%build_transform`), `ExpressionTransform` (built
by `graphbuilder_run_expression_hook`, not by a characteristic). No
horizontal-regrid or vertical-regrid `Transform` exists; geometry and
vertical-coordinate mismatches therefore fail in graph mode.

### 1.2 Integration layer (`superstructure/generic/`, top level)

`GraphBuilder.F90` (1206 lines) is the public module: the `GraphBuilder`
type (stateless; every method is `nopass` and takes the
`OuterMetaComponent`) and cross-submodule interfaces. Eleven submodules
(about 5,000 lines together):

| File | Lines | Responsibility |
|---|---|---|
| `GraphBuilder_Advertising_smod.F90` | 396 | `advertise_one` creates `StateItemNode`s from `VariableSpec`; `populate_characteristics` fills the characteristics map and `declared_*` fields |
| `GraphBuilder_ConnectionResolution_smod.F90` | 2532 | Ordinary/Simple/Match connection resolution (`resolve_one`, `for_each_matching_import`); extension-chain use; import mirroring (`run_propagate_hook`) and export propagation (`run_export_propagate_hook`); Expression hook (`run_expression_hook`); Wildcard fan-in (`resolve_wildcard_destination`); unsatisfied-import checks; qualified-name resolution |
| `GraphBuilder_Freeze_smod.F90` | 168 | Hierarchy-wide freeze and convergence check (`run_graph_freeze_hook`) |
| `GraphBuilder_Geometry_smod.F90` | 284 | Geometry push/pull (`run_geometry_hook`) |
| `GraphBuilder_CallbackWiring_smod.F90` | 670 | Wildcard/regex callback-argument wiring against the flattened qualified-export namespace |
| `GraphBuilder_LifecycleHooks_smod.F90` | 164 | Wrappers called from `OuterMetaComponent` lifecycle sites: advertise, activate, connect, populate-state hooks; logs `GraphResolutionEntry` results |
| `GraphBuilder_PortsAndProxies_smod.F90` | 89 | Export geom/vgrid resolution helpers (`resolve_export_geom`, `resolve_export_vgrid`). The name is historical: no proxy code remains. |
| `GraphBuilder_RunPhaseNode_smod.F90`, `GraphBuilder_RunPhasePull_smod.F90` | 149, 93 | `MethodGraphNode` construction; demand-driven pull for a run phase |
| `GraphBuilder_StatePopulation_smod.F90` | 403 | Populates the real `ESMF_State` from graph nodes (`graph`/`both` modes) |
| `GraphBuilder_Visualization_smod.F90` | 68 | DOT/JSON enrichment |

Also: `GraphResolutionEntry*.F90` (structured resolution report),
`GraphScenarioBootstrap.F90` (YAML-driven fixture setup used by scenario
tests and `apps/graph_diagnostic`), `OuterMetaComponent.F90` +
`OuterMetaComponent/` (64 submodule files; owns a `HierarchyId`, a
`ComponentGraph`, child drivers, and the legacy `StateRegistry`).

`superstructure/generic/tests/` holds `Test_GraphBuilder_*.pf` (one per
submodule area), `Test_GraphBuilderPropagation.pf`,
`Test_GraphBuilderEquivalence.pf`, `Test_GraphEquivalenceHarness.pf`,
`Test_GraphModeDemonstration.pf`, `Test_GraphMode.pf`,
`Test_GraphBuilderMaterializeGate.pf`, `Test_GraphGeometryHook.pf`, and
the fixtures `tests/scenarios/` (legacy YAML scenarios) and
`tests/graph-scenarios/` (graph-mode fixtures).

### 1.3 Specification-to-source correspondence

`docs/graph/spec/README.md` has the authoritative document map. Key
pairs: `03` ↔ `GraphNode` family; `05` ↔ `id_types/`; `06` ↔
`DependencyNetwork.F90`; `07` ↔ `ComponentGraph.F90`; `08` ↔
`GraphBuilder*.F90`; `02` ↔ `OuterMetaComponent`; `18` ↔
`characteristics/`.

## 2. Important interfaces and dependencies

### 2.1 Layering rule (hard; enforced by review, not by the compiler)

```
ComponentGraph / DependencyNetwork / GraphNode* / GraphStateItem
    MUST NOT depend on OuterComponent, StateRegistry, or any
    component-hierarchy implementation type.            (REQ-CG-002)

GraphBuilder
    is the ONLY layer permitted to depend on both the graph core
    AND OuterMetaComponent / StateRegistry.             (08-graph-builder.md §8.3)
```

Graph code also must not call into the legacy `specs/*Aspect`,
`registry/`, or `transforms/` layers; see `CONVENTIONS.md`.

### 2.2 `NodeId` scope

- `NodeId`/`DependencyNetworkId` are valid **hierarchy-wide**.
- A `ComponentGraph` holds a `HierarchyId` (value, not pointer) and
  resolves shared maps via `get_hierarchy_resources`.
- Each `ComponentGraph` keeps a private set of the ids it registered
  (`owned_node_ids()`).
- Cross-component edges connect two real `NodeId`s directly:
  `add_dependency`/`validate`/`bind_port`/`add_import_port`/
  `add_export_port` accept an id owned by `this` **or** registered
  anywhere in the hierarchy (`allowed_foreign_node`, checked via
  `get_node(id)` being associated; never by reverse lookup).
- There are no proxy nodes. `proxy_key`, `get_or_make_local_node_id`,
  `parent_geometry_proxy_key`, and `push_geometry_to_child` no longer
  exist.
- `freeze()` is one hierarchy-wide operation: the
  `GENERIC_INIT_GRAPH_FREEZE` phase runs once at the true hierarchy
  root and `GraphBuilder_Freeze_smod.F90` freezes every component's
  `ComponentGraph`, then runs the hierarchy-wide convergence check
  (any remaining unresolved required import is a hard failure).
  `graphbuilder_resolve_connections` is made idempotent by
  `connections_already_resolved()`/`mark_connections_resolved()`
  (`OuterMetaComponent/connections_resolved.F90`), not by `is_frozen()`.

### 2.3 Import-port table vs. resource index

- **Resource index** (`ComponentGraph%get_resource_index`): populated
  only for a directly advertised item (`advertise_one`).
- **Import-port table** (`get_import_ports`/`add_import_port`): directly
  advertised imports **and** imports mirrored up from a child. A mirrored
  entry deliberately has no resource-index entry.
- **Export-port table**: directly advertised exports and propagated
  exports. A propagated export's `StateItemNode%short_name` is never
  rewritten; `ComponentGraph%get_export_port_name()` (a sparse
  `PortId→qualified_name` side map) carries the ancestor-qualified name
  `child_name/short_name`, qualified once, at the immediate parent.
- `for_each_matching_import` walks the import-port table, so it finds
  mirrored imports like direct ones. Never re-derive a destination's
  `NodeId` with `resolve_local_node_id` (resource-index lookup); use the
  found `StateItemNode%get_node_id()`. Same for `find_export_node_id`.
- Composite members are not independently addressable: they appear in no
  resource index or port table.

### 2.4 Lifecycle call sites (all in `OuterMetaComponent/`)

| File | Sequence |
|---|---|
| `initialize_advertise.F90` | `recurse()` (children first) → `self_advertise` → `gb%run_advertise_hook` → legacy `process_connections` → `gb%run_activate_hook` → `gb%run_propagate_hook` → `registry%propagate_unsatisfied_imports` → `gb%run_export_propagate_hook` → `registry%propagate_exports` → `registry%add_to_states` |
| `initialize_accept_transfer.F90` | `gb%run_connect_hook`, then `gb%run_expression_hook`, then `gb%run_populate_state_hook` |
| `initialize_geom_b.F90` | `gb%run_geometry_hook` |
| `initialize_graph_freeze.F90` | `gb%run_graph_freeze_hook` (root only) |
| `run_user.F90` | `gb%run_pull_hook` |

`run_propagate_hook` must follow `run_activate_hook`: an import resolved
by a connection declared at this level must be reflected before this
level's mirroring step reads child state, or it is needlessly mirrored
upward. Legacy registry calls and graph hooks both run unconditionally;
`graph_mode()` only gates whether the graph result feeds the real
`ESMF_State` (see `docs/generic/ARCHITECTURE.md` §2.3).

### 2.5 `GraphBuilder.F90` and submodules: `public ::` rule

Any plain (non-`module` interface) helper declared in `GraphBuilder.F90`
and called from a submodule needs an explicit `public ::` line. NAG
accepts a missing one; gfortran fails at link with `Undefined symbols`.

### 2.6 Resolution coverage by item class

| Item class | Graph-mode path |
|---|---|
| FIELD, FIELDBUNDLE, STATE | Ordinary resolution + characteristic extension chain |
| BRACKET → FIELD | `ItemShapeCharacteristic` + `BracketTimeInterpolationTransform` |
| VECTORBRACKET | Not handled: legacy only. `to_itemtype` in the compiled parser does not parse `'vector_bracket'`. |
| EXPRESSION | `graphbuilder_run_expression_hook` + `ExpressionTransform` (N:1) |
| WILDCARD | `resolve_wildcard_destination` (fan-in of every matching producer as composite members) |
| SERVICE | Excluded from graph resolution (`is_ordinary_resolvable_import`); being retired in favor of callbacks |
| Callback arguments | `GraphBuilder_CallbackWiring_smod.F90` |

## 3. Known limitations (current)

- No geometry (horizontal regrid) or vertical-regrid `Transform`;
  `GeometryCharacteristic` and `VerticalCoordinateCharacteristic`
  mismatches `_FAIL`. This is the main reason legacy `regrid*` and
  `propagate_geom` scenarios cannot run under graph mode.
- Six characteristic axes detect mismatch but have no repair (see §1.1).
- A mirrored (propagated) import is not inserted into the real
  `ESMF_State` under a child-qualified name (archived task 6.7 of
  `hierarchy-wide-component-graph`; needs a new branch in
  `graphbuilder_populate_state`).
- Ordinary inout in the general case (Phase 5b2) is blocked on a design
  addendum (`spec/20-implementation-roadmap.md`).
- `GraphStateItem` mixes the ESMF handle with description data;
  `ordering` / `characteristic_ordering_table` is variant-keyed with no
  legacy counterpart. Both are targeted by
  `docs/graph/plans/separation-of-concerns.md` (B1, B3, B4).
- Legacy retirement (`registry/`, `specs/*Aspect`, legacy `transforms/`)
  is not started. Its graph-side prerequisites in roadmap §20.4.11 (3b2,
  7a, 7b, 7c, 12, 13) have all landed.

## 4. Build and test

Each shell call is a fresh shell. Use the wrappers (`with-nag` loads
`nag-stack/7.2.41`, `with-gfortran` loads `gfortran-stack`). NAG must be
exactly 7.2.41. Send logs to files; show only the tail.

```bash
with-nag cmake --build nag -j 10 --target build-tests > nag/build.log 2>&1; tail -3 nag/build.log
with-nag ctest --test-dir nag -L ESSENTIAL -j 6 > nag/ctest.log 2>&1; tail -15 nag/ctest.log

with-gfortran cmake --build gfortran -j 10 --target build-tests > gfortran/build.log 2>&1; tail -3 gfortran/build.log
with-gfortran ctest --test-dir gfortran -L ESSENTIAL -j 6 > gfortran/ctest.log 2>&1; tail -15 gfortran/ctest.log
```

Verified 2026-10-21 on this tree: `ctest -L ESSENTIAL` passes 65/65 on
both NAG 7.2.41 and gfortran. A full unlabeled `ctest` additionally runs
four `apps/tests/Regrid_Util/` regression tests (`ll-ll`, `cs-cs`,
`cs-ll`, `ll-cs`) that need `LOCAL_REGRESSION_DATA_DIR`; they fail
without it and are outside `ESSENTIAL`.

This checkout uses `superstructure/generic/` and `MAPL.generic.*` target
names, not the `generic3g`/`MAPL.generic3g.*` names in the generic
`mapl-build`/`mapl-testing` skills. Graph-relevant ctest targets:

| Target | Covers |
|---|---|
| `MAPL.generic.graph` | `graph/tests/` (core, characteristics, transforms, callbacks) |
| `MAPL.generic.components` | `Test_GraphBuilder*.pf`, propagation, mode tests, spec/parser tests |
| `MAPL.generic.components.quiet` | Two deliberate negative-path hook tests, run at `--level ERROR` |
| `MAPL.generic.scenarios` | `Test_Scenarios.pf`, equivalence harness, graph-mode demonstration (strict field dictionary) |
| `MAPL.generic.core`, `.aspects`, `.transforms`, `.vertical` | Legacy layer; see `docs/generic/ARCHITECTURE.md` §4.2 |

Fast path: `with-nag ctest --test-dir nag -R MAPL.generic.components --output-on-failure`.

## 5. Architectural decisions still in force

Primary records are under `openspec/changes/archive/`; read the relevant
`design.md` before changing the area.

- **Shared storage keyed by `HierarchyId`** (`2026-10-17-hierarchy-wide-component-graph`,
  D1): not threaded pointers, not a bare singleton.
- **Explicit ownership sets, no reverse `NodeId→owner` map** (D2).
- **Propagation is one recursive mirror step per level** (D3). An
  earlier "search the subtree" design was rejected: it needed a
  connection to trigger and materialized nothing at intermediate levels.
- **Export visibility is a query, not a push** (D4,
  `query_component_exports`).
- **Qualified-path resolution stays a tree walk** (D5).
- **Composite-member non-addressability and connection direction are
  explicit, test-covered requirements** (D6).
- **Freeze is one hierarchy-wide call** (D7).
- **Geometry is a graph node** (incomplete `ESMF_Field` tagged
  `MAPL_STATEITEM_GEOM`), so a component may derive its geom from
  children.
- **Graph shape dispatch is independent of legacy** (phases 7a/7b/7c):
  `ItemShapeCharacteristic`, `ExpressionTransform`, and wildcard
  fan-in duplicate rather than call `ClassAspect`/`ExtensionTransform`.
- **Characteristic dispatch** (phase 12,
  `2026-10-19-phase-12-characteristic-dispatch-wiring`): six further
  axes (Attributes, UngriddedDims, StandardName, QuantityType,
  Conservation, Normalization) wired into `populate_characteristics`
  alongside Units, TypeKind, and VerticalCoordinate; ItemShape was
  added by 7a.
- **Restart compatibility** (phase 13): `declared_restart_mode` is
  captured at advertise and written to ESMF Info by `populate_one`,
  mirroring `FieldClassAspect%add_to_state`.

## 6. Roadmap pointer

Each landed phase has an archived change under
`openspec/changes/archive/`: 5a/5a2 (characteristics), 5b (ordinary
inout, direct alias), 7a, 7b, 7c (Wildcard only), 9, 10a-d, 11a/b, 12,
16 (`hierarchy-wide-component-graph`), 17-19. Phase 13 (restart) and
phases 20-21 (`MAPL.generic.components.quiet`, `apps/graph_diagnostic`
21-1..21-3) landed as commits without an archived change of that name.
Not built: 5b2 (general inout), 5c (compiled execution), 21-4
(optional batch mode), 6a (legacy retirement). The authoritative list
is `docs/graph/spec/20-implementation-roadmap.md` §20.4; verify an
individual phase against `git log` before relying on this summary.
