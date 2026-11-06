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

One openspec change is in flight:
`openspec/changes/characteristic-separation-of-concerns/`, which
implements Phase B of `docs/graph/plans/separation-of-concerns.md`.
Where this file and that change's `design.md` disagree about
characteristics, the descriptor or status resolution, `design.md` wins.
Landed changes (including `hierarchy-wide-component-graph`, phases
7a/7b/7c and 12) are under `openspec/changes/archive/<date>-<name>/`.

Facts below were verified against the tree on 2026-10-21 unless marked
**[ASSUMPTION]**; updated 2026-11-06 through `cc98f77c3` (characteristics,
expression, §1.1-§1.2, §2.3, §2.6, §3, §4, §5).

## 1. Module map (`superstructure/generic/`)

### 1.1 Graph-neutral core (`superstructure/generic/graph/`)

No dependency on `OuterComponent`/`StateRegistry` (REQ-CG-002). Layout:
files at the top level, and subdirectories `characteristics/`,
`transforms/`, `callback/`, `id_types/` (plus `IdTemplate.inc`),
`containers/` (gFTL instantiations); tests are in `graph/tests/`.

| File | Responsibility |
|---|---|
| `id_types/NodeId.F90`, `DependencyNetworkId.F90`, `PortId.F90`, `HierarchyId.F90`, `CallbackInterfaceId.F90` | Identity types generated from `id_types/IdTemplate.inc` |
| `HierarchyGraph.F90` | Hierarchy-wide `NodeId→GraphNode` and `DependencyNetworkId→DependencyNetwork` maps plus the shared id generators |
| `HierarchyResources.F90`, `HierarchyRegistry.F90` | Container for one `HierarchyGraph`; process-wide registry keyed by `HierarchyId` (`request_new_hierarchy`, `get_hierarchy_resources`, `release_hierarchy`) |
| `GraphNode.F90`, `BaseGraphNode.F90`, `StateItemNode.F90`, `OperationGraphNode.F90`, `MethodGraphNode.F90` | Node hierarchy. `StateItemNode%has_short_name()` is false for a nameless node (extension, expression result); `get_short_name()` then returns `''`. |
| `GraphStateItem.F90` | Payload of a `StateItemNode`: Field/FieldBundle/State handle (at most one allocated) and the member maps. The variant tag is read and written only through `MAPL_FieldGet/Set(state_item_variant=)`, `MAPL_FieldBundleGet/Set(state_item_variant=)` and `MAPL_StateGet/Set(state_item_variant=)`; `MAPL_StateItem_Flag` lives in `enums/StateItemFlag.F90` and its Info key in `infrastructure/field/StateItemVariantInfo.F90` and `utils/MAPL_ESMF_InfoKeys.F90`. |
| `StateItemDescriptor.F90` | Description of a state item, owned by `StateItemNode` (`get_descriptor()` returns a pointer): the characteristics map (`has/get/set_characteristic`, `get_characteristics`) plus the `declared_*` values, and the (item type, kind) mirrored-export allowlist. Independent of any ESMF handle. |
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
`DEFERRED`). Value constructors set `SPECIFIED`; a default-initialized
characteristic is `INVALID`.

Status is resolved generically in `StateItemDescriptor.F90`
(`resolve_pair`, `resolve_characteristics`, `resolve_descriptors`) from
the two statuses alone; `needs_extension_for` runs only when both sides
are `SPECIFIED`. A `MIRRORED` import adopts the export's value
(`resolve_descriptors`, used by `resolve_one` and the inout-borrower
path); a `MIRRORED` export matches only via the (item type, kind)
allowlist (`is_mirrored_export_allowed`: EXPRESSION x GEOMETRY /
VERTICAL_GRID / VERTICAL_STAGGER / VERTICAL_ORIENTATION / TYPE_KIND). Each
entry also has `declared_wins` (see the Expression paragraph below);
`get_consumer_adopted_kinds` returns the kinds and the flags. Status conflicts are reported as
unsupported connections; `INVALID` is a `_FAIL`. A pair with a `DEFERRED`
side (`PAIR_WAIT`) is neither wired nor reported: its import is left
unsettled and `resolve_connections` retries it on a later ACCEPT_TRANSFER
pass (`OuterMetaComponent` `begin/end_connection_pass`,
`note_import_settled`; the once-only gate closes only when nothing waits). A kind on only one side
is not compared. At freeze, `run_graph_freeze_hook` calls
`StateItemDescriptor%check_resolved` on every owned `StateItemNode` and
reports any `DEFERRED`/`INVALID` entry through `report_if_failed`.
A `DEFERRED` value is filled in two ways: `resolve_deferred` offers the
component's default geom (`CharacteristicDefaults`) at GEOM_B, and
`graphbuilder_harvest_deferred` (at the start of the connect hook) reads
the geom, vertical stagger and quantity type/basis the component's own phase
set on the item's exported or internal field/bundle, read-only (the item's
handle is not bound); imports are skipped. An item with neither stays `DEFERRED`
and freeze reports it.
A `DEFERRED` characteristic that nothing supplied is converted to
`UNCHECKED` by `StateItemDescriptor%fallback_unsupplied_deferred` when
`is_deferred_fatal()` is false (`QuantityType` only, see below). The
connect hook and `graphbuilder_resolve_connections` (own component and
every descendant) both apply it, idempotently, before resolving; one
summary `GraphResolutionEntry` per kind that fell back (count plus the
first 5 item names) is logged by the hook.
`MAPL_TYPEKIND_MIRROR` advertises a valueless `MIRRORED` TYPE_KIND entry
(set by the `VariableSpec` factory, not by the characteristic constructor).

Extension chains: `StateItemCharacteristic%covered_kinds(tip, goal)`
(default: its own kind; the result includes its own kind and only kinds
present in both maps) names the kinds one repair step settles.
`build_chain` keeps a tip map per step, replaces every covered kind with
the goal's value, and skips later mismatches the tip already covers. An
extension node's descriptor is the tip after its step;
`intermediate_node_ids` has one entry per step. Materialization reads
each link's own descriptor.

Materialization: `materialize_field_extension(graph, node_id [, name],
rc)` creates an empty field, calls the node's `sync_to_info(STRUCTURAL)`,
runs `MAPL_FieldEmptyComplete`, and sets `allocation_status` to
`ALLOCATED`. A characteristic must therefore write in `write_info`
(STRUCTURAL) whatever `FieldEmptyComplete` needs. Geom and vgrid come
only from the descriptor; `GraphBuilder` names no characteristic kind or
concrete characteristic type.
`StateItemCharacteristic%is_complete()` (default `.true.`) feeds
`StateItemDescriptor%get_incomplete_kinds` and `check_materializable`
(failure reason `incomplete_characteristic:<KIND>`). `harvest(handle,
defaults)` (default no-op; `VerticalStagger` and `QuantityType`
override) fills characteristics from a realized field; the harvest loop
also visits nodes with incomplete kinds. `descriptor%get_expression`
returns the expression string; `add_expression_result_defaults` adds the
creation defaults (GEOM `horizontal_dims_spec`) for an expression result.

Info writes: `StateItemDescriptor%sync_to_info(handle, phase)` calls
each SPECIFIED/UNCHECKED characteristic's `write_info` (phases
`SYNC_PHASE_STRUCTURAL`/`SYNC_PHASE_DESCRIPTIVE`; `is_structural()`
kinds are skipped in DESCRIPTIVE). Every kind with a factory overrides
`write_info` (`Attributes` as a no-op: no Info key). The base version
fails, so a new kind without one fails `sync_to_info` loudly. Only `populate_one` calls into it, through `StateItemDescriptor%sync_per_alias_to_info` (entries whose `is_per_alias()` is true: `RestartMode`, `LongName`); extension and export field materialization calls
`sync_to_info(STRUCTURAL)` on the node's own descriptor. An import with no declared `long_name` inherits the export's: the owning field's alias-0 entry is written by the full STRUCTURAL sync. Factories live in
`specs/VariableSpec/make_characteristics.F90`: the type-bound
`VariableSpec%make_characteristics` loops over a registry of per-kind
factories (so far units, typekind, ungridded dims, geometry, vertical grid, vertical stagger, vertical orientation, horizontal dims spec, standard name, attributes, item shape, long name, restart mode, quantity type, conservation, normalization and expression). Procedure: `docs/graph/adding-a-characteristic.md`.

| Characteristic | Kind constant | `build_transform` today |
|---|---|---|
| `PhysicalUnits` | `PHYSICAL_UNITS_CHARACTERISTIC_KIND` | Real: `UnitsConverterTransform` |
| `TypeKind` | `TYPE_KIND_…` | Real: `TypeKindConverterTransform` |
| `ItemShape` | `ITEM_SHAPE_…` | Every shape. Real for `BRACKET`→`FIELD` only (`BracketTimeInterpolationTransform`); `VECTORBRACKET`→`VECTOR` unsupported; pairs the shape matrix rejects (including `VECTORBRACKET`→`VECTORBRACKET`) are incompatible. Owns `get_extension_order` |
| `Geometry` | `GEOMETRY_…` | Unsupported — `HorizontalRegridTransform` exists but `build_transform` does not build it yet. `select_regridder_param` holds the regridder-param rule |
| `VerticalGrid` | `VERTICAL_GRID_…` | Single overlap: unsupported (no vertical-regrid `Transform` yet); incompatible and ambiguous overlaps `_FAIL` with distinct messages |
| `VerticalStagger` | `VERTICAL_STAGGER_…` | Unsupported — mismatch is detected, repair is a stub |
| `VerticalOrientation` | `VERTICAL_ORIENTATION_…` | Unsupported — mismatch is detected, repair is a stub |
| `HorizontalDimsSpec` | `HORIZONTAL_DIMS_SPEC_…` | Incompatible — GEOM vs NONE is detected in both directions and can never connect |
| `Attributes`, `UngriddedDims`, `Conservation`, `Normalization` | `…_CHARACTERISTIC_KIND` | Incompatible — legacy rejects every mismatch of two non-mirror values, so a detected mismatch can never connect |
| `QuantityType` | `QUANTITY_TYPE_CHARACTERISTIC_KIND` | Incompatible for a quantity-type mismatch; unsupported when only the basis differs (both real bases; `BASIS_NONE` is a basis wildcard, `QUANTITY_UNKNOWN` is not a wildcard): a conversion is possible but not built. Factory status by intent: declared -> `SPECIFIED`; undeclared (or declared `QUANTITY_UNKNOWN`) -> `DEFERRED` on EXPORT/INTERNAL, `UNCHECKED` on IMPORT. `is_deferred_fatal` is `quantity_type_required()` (`ValidationPolicy.F90`, key `field_dictionary: require_quantity_type`, no legacy counterpart; strict validation alone does not set it). An unsupplied one falls back to `UNCHECKED` with one summary warning |
| `Expression` | `EXPRESSION_CHARACTERISTIC_KIND` | Never compared; the declared expression string (`ValueCharacteristic`, no Info key); `build_transform` is the unsupported stub. Only EXPRESSION items carry it. The `UngriddedDims`, `Attributes`, `QuantityType`, `Conservation`, `Normalization` and `RestartMode` factories also give an EXPRESSION item an entry (via `has_field_metadata` / `carries_field_description`), so an EXPRESSION export with ungridded dims against a FIELD import is a connection conflict |
| `StandardName` | `STANDARD_NAME_…` | By validation mode (`ValidationPolicy.F90`, design A25): strict, incompatible; permissive (default), a match with a connection warning (`match_warning`), the export's name wins. FIELD and BRACKET carry it |
| `LongName` | `LONG_NAME_…` | Never compared (`needs_extension_for` is always false); `build_transform` is the unsupported stub |
| `RestartMode` | `RESTART_MODE_…` | Never compared; `build_transform` is the unsupported stub. FIELD only; per-alias |

Other files: `CharacteristicStatus.F90`, `StateItemCharacteristicKind.F90`
(also `MOCK_CHARACTERISTIC_KIND`), `SetSharedCharacteristic.F90` (`set_shared_characteristic(kind, characteristic)` sets that one entry in the node's descriptor; the ESMF payload is left alone; only direct `StateItemNode` successors are reset, so a transform step feeding a changed import keeps stale state).

"Unsupported" means `build_transform` calls
`report_build_transform_unsupported(rc)` (`StateItemCharacteristic.F90`),
which sets `rc = BUILD_TRANSFORM_UNSUPPORTED` and raises nothing;
`ExtensionResolution` then reports the kind through
`unsupported_characteristics`. A test of a stub-axis mismatch asserts that
status or entry and needs no `@assertExceptionRaised`. A pair that can
never connect calls `report_build_transform_incompatible(rc)` instead
(`rc = BUILD_TRANSFORM_INCOMPATIBLE`, raises nothing).
`ExtensionResolution` returns it through the optional `conflict` argument
(kind plus both `get_signature()` values); `graphbuilder_resolve_connections`
collects it in `connection_conflicts` and the connect hook logs it as a
"connection conflict" warning, apart from the "no registered extension
provider" one. Status conflicts still go to `unsupported_characteristics`.
Any other nonzero rc is a genuine error.

Extension chain nodes (`find_or_build_extension_chain`, every chain)
carry a `StateItemDescriptor`: the export's characteristics plus the
import's values for the kinds converted so far.

Expression items: the EXPRESSION export is a placeholder with no edges
and is never materialized. `graphbuilder_run_expression_hook` is the names
check only (missing name -> unresolved; nested, self-reference or empty
expression -> unsupported); `ACCEPT_TRANSFER` fails on any reported problem
in every validation mode. At connect time `resolve_one_pair` builds one
evaluation per consumer (a result `StateItemNode` plus an
`ExpressionTransform` over the referenced exports) in the connecting
graph, reused through an `EXPREVAL:` resource key. The result copies the
expression's characteristics and takes the consumer's value for each kind in
`get_consumer_adopted_kinds` (the allowlist); a `SPECIFIED` value declared
on the expression wins where `declared_wins` is true (all but `TYPE_KIND`,
so a declared typekind has no effect). The result is then resolved
against the consumer as an ordinary pair. Each input reaches the adopted
kinds through its own extension chain: precision gets real steps, other
adopted kinds are unsupported; a `DEFERRED` adopted value waits. Chains
are built before the transform, so a failing input leaves no evaluation.

#### Transforms (`graph/transforms/`)

`TransformBase`, `Transform` (abstract; `Transform` adds the compute
interface over named ports), `TransformGraphNode`, and five concrete
transforms: `UnitsConverterTransform`, `TypeKindConverterTransform`,
`BracketTimeInterpolationTransform` (built by
`ItemShapeCharacteristic%build_transform`), `ExpressionTransform` (built
by the connect-time expression path in `GraphBuilder_ConnectionResolution_smod.F90`, not by a characteristic), and
`HorizontalRegridTransform` (field only; built by nothing yet). The
regrid Transform gets its regridder from `RegridderManager` at the first
`compute`, from the bound fields' geoms and typekinds, and gets a new one
when a geom or typekind changes. No vertical-regrid `Transform` exists.
Geometry and vertical-grid mismatches are reported as unsupported, so the
graph leaves those connections unwired.

### 1.2 Integration layer (`superstructure/generic/`, top level)

`GraphBuilder.F90` is the public module: the `GraphBuilder`
type (stateless; every method is `nopass` and takes the
`OuterMetaComponent`) and cross-submodule interfaces. Eleven submodules:

| File | Responsibility |
|---|---|
| `GraphBuilder_Advertising_smod.F90` | `advertise_one` creates `StateItemNode`s from `VariableSpec`; `populate_characteristics` fills the characteristics map and `declared_*` fields |
| `GraphBuilder_ConnectionResolution_smod.F90` | Ordinary/Simple/Match connection resolution (`resolve_one`, `for_each_matching_import`); extension-chain use; import mirroring (`run_propagate_hook`) and export propagation (`run_export_propagate_hook`); Expression names check (`run_expression_hook`) and per-consumer evaluation (`resolve_one_pair`); Wildcard fan-in (`resolve_wildcard_destination`); unsatisfied-import checks; qualified-name resolution |
| `GraphBuilder_Freeze_smod.F90` | Hierarchy-wide freeze and convergence check (`run_graph_freeze_hook`) |
| `GraphBuilder_Geometry_smod.F90` | Geometry push/pull (`run_geometry_hook`) |
| `GraphBuilder_CallbackWiring_smod.F90` | Wildcard/regex callback-argument wiring against the flattened qualified-export namespace |
| `GraphBuilder_LifecycleHooks_smod.F90` | Wrappers called from `OuterMetaComponent` lifecycle sites: advertise, activate, connect, populate-state hooks; logs `GraphResolutionEntry` results |
| `GraphBuilder_RunPhaseNode_smod.F90`, `GraphBuilder_RunPhasePull_smod.F90` | `MethodGraphNode` construction; demand-driven pull for a run phase |
| `GraphBuilder_StatePopulation_smod.F90` | Populates the real `ESMF_State` from graph nodes (`graph`/`both` modes) |
| `GraphBuilder_Visualization_smod.F90` | DOT/JSON enrichment |

Also: `GraphResolutionEntry*.F90` (structured resolution report),
`GraphScenarioBootstrap.F90` (YAML-driven fixture setup used by scenario
tests and `apps/graph_diagnostic`), `OuterMetaComponent.F90` +
`OuterMetaComponent/` (submodule files; owns a `HierarchyId`, a
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
  (`own_node_ids`, queried with `owns_node()`).
- Cross-component edges connect two real `NodeId`s directly:
  `add_dependency`/`validate`/`bind_port`/`add_import_port`/
  `add_export_port` accept an id owned by `this` **or** registered
  anywhere in the hierarchy (`allowed_foreign_node`, checked via
  `get_node(id)` being associated; never by reverse lookup).
- There are no proxy nodes. `get_or_make_local_node_id` and
  `parent_geometry_proxy_key` no longer exist. `push_geometry_to_child`
  (`GraphBuilder_Geometry_smod.F90`) remains, but connects the parent's
  and child's real geometry nodes rather than creating a proxy.
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
  `child_name/short_name`, qualified once, at the immediate parent. It returns `''` for a node that is not a candidate (not an exported port).
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
| VECTORBRACKET | Not handled: legacy only (matrix: VECTOR unsupported, others incompatible). `to_itemtype` in the compiled parser does not parse `'vector_bracket'`. |
| EXPRESSION | Placeholder export; names check in `graphbuilder_run_expression_hook`; one `ExpressionTransform` (N:1) evaluation per consumer, built in `resolve_one_pair` |
| WILDCARD | `resolve_wildcard_destination` (fan-in of every matching FIELD producer as composite members; other shapes are skipped) |
| SERVICE | Excluded from graph resolution (`is_ordinary_resolvable_import`); being retired in favor of callbacks |
| Callback arguments | `GraphBuilder_CallbackWiring_smod.F90` |

## 3. Known limitations (current)

- No vertical-regrid `Transform`, and `HorizontalRegridTransform` is not
  built by `GeometryCharacteristic%build_transform` yet. A Geometry
  mismatch or single-overlap VerticalGrid mismatch is reported unsupported
  (incompatible and ambiguous VerticalGrid overlaps `_FAIL`). This is the
  main reason legacy `regrid*` and `propagate_geom` scenarios cannot run
  under graph mode.
- An expression whose consumer has precision R8 (`expression_per_consumer_r8`)
  cannot run end to end: legacy couplers still run in graph mode and fail
  on the expression item. The scenario stays pending (see
  `docs/graph/scenario-parity.md`).
- Six characteristic axes detect mismatch but have no repair (see §1.1).
- A mirrored (propagated) import is not inserted into the real
  `ESMF_State` under a child-qualified name (archived task 6.7 of
  `hierarchy-wide-component-graph`; needs a new branch in
  `graphbuilder_populate_state`).
- Ordinary inout in the general case (Phase 5b2) is blocked on a design
  addendum (`spec/20-implementation-roadmap.md`).
- Mismatched kinds are ordered by the tip's
  `ItemShapeCharacteristic%get_extension_order` in
  `resolve_characteristics`; each step replaces its `covered_kinds` on the
  tip.
- Legacy retirement (`registry/`, `specs/*Aspect`, legacy `transforms/`)
  is not started. Its graph-side prerequisites in roadmap §20.4.11 (3b2,
  7a, 7b, 7c, 12, 13) have all landed.

## 4. Build and test

Each shell call is a fresh shell. Use the wrappers (`with-nag` loads
`nag-stack`, `with-gfortran` loads `gfortran-stack`; the module default
is the version MAPL needs, never load another). Send logs to files; show only the tail.

```bash
with-nag cmake --build nag -j 10 --target build-tests > nag/build.log 2>&1; tail -3 nag/build.log
with-nag ctest --test-dir nag -L ESSENTIAL -j 6 > nag/ctest.log 2>&1; tail -15 nag/ctest.log

with-gfortran cmake --build gfortran -j 10 --target build-tests > gfortran/build.log 2>&1; tail -3 gfortran/build.log
with-gfortran ctest --test-dir gfortran -L ESSENTIAL -j 6 > gfortran/ctest.log 2>&1; tail -15 gfortran/ctest.log
```

`ctest -L ESSENTIAL` passes 69/69 on both NAG and gfortran (Debug). NAG
Debug (`-C=dangling`) is a gate: a gFTL set or map iterated through a
non-`TARGET` local or dummy aborts there, so add `TARGET`. A full unlabeled `ctest` additionally runs
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
| `MAPL.generic.scenarios.shard0` .. `.shard3` | `Test_Scenarios.pf`, equivalence harness, graph-mode demonstration (strict field dictionary). One binary run in 4 processes via `MAPL_SCENARIO_SHARD=k/4`, because `ESMF_TestCase` leaks MPI communicators per test (MPICH limit). `-R MAPL.generic.scenarios.shard` runs all; the plain `MAPL.generic.scenarios` entry is disabled |
| `MAPL.generic.scenarios.pending` (label `PENDING`, not ESSENTIAL) | Scenarios not yet `graph_ready`, under graph/both; expected to fail; see `docs/graph/scenario-parity.md` |
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
  alongside Units, TypeKind, and VerticalGrid; ItemShape was
  added by 7a. The `Expression` characteristic (EXPRESSION items only)
  is a further factory in the same registry.
- **Restart compatibility** (phase 13): the item's `RESTART_MODE`
  characteristic is built at advertise and written to ESMF Info on the
  alias by `populate_one` (through `sync_per_alias_to_info`), mirroring
  `FieldClassAspect%add_to_state`.

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
