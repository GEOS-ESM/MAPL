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

Two openspec changes are active:
`openspec/changes/characteristic-separation-of-concerns/`, which
implements Phase B of `docs/graph/plans/separation-of-concerns.md`, and
`openspec/changes/hierarchy-wide-forward-network/` (plan task C4, one
forward network per hierarchy).
Where this file and the first change's `design.md` disagree about
characteristics, the descriptor or status resolution, `design.md` wins.
Landed changes (including `hierarchy-wide-component-graph`, phases
7a/7b/7c and 12) are under `openspec/changes/archive/<date>-<name>/`.

Facts below were verified against the tree on 2026-10-21 unless marked
**[ASSUMPTION]**; updated 2026-11-10 through `480af45c5` (§1.1-§1.2,
§2.2-§2.4, §2.6, §2.7, §3).

## 1. Module map (`superstructure/generic/`)

### 1.1 Graph-neutral core (`superstructure/generic/graph/`)

No dependency on `OuterComponent`/`StateRegistry` (REQ-CG-002). Layout:
files at the top level, and subdirectories `characteristics/`,
`transforms/`, `callback/`, `id_types/` (plus `IdTemplate.inc`),
`containers/` (gFTL instantiations); tests are in `graph/tests/` (for example `Test_ForwardNetwork.pf` for the
hierarchy's forward network, `Test_VerticalColumnRegridTransform.pf`
for both vertical Transforms, `Test_HorizontalBundleRegridTransform.pf` for
the bundle-only regrid, and `Test_UnitsConverterTransform.pf` and
`Test_TypeKindConverterTransform.pf`, which cover Fields and bundles).

| File | Responsibility |
|---|---|
| `id_types/NodeId.F90`, `DependencyNetworkId.F90`, `PortId.F90`, `HierarchyId.F90`, `CallbackInterfaceId.F90` | Identity types generated from `id_types/IdTemplate.inc` |
| `HierarchyGraph.F90` | Hierarchy-wide `NodeId→GraphNode` and `DependencyNetworkId→DependencyNetwork` maps plus the shared id generators. Also owns the hierarchy's one forward network (created in `initialize`), the transform `PortBindingTable`, and the reuse index (`add_reuse_entry`/`get_reuse_entry`; keys `EXTCHAIN:`, `EXPREVAL:`, `CALLBACK_COLLECTION:`, `WILDCARD_COLLECTION:`) (`HierarchyGraph.F90:43-57,62-74`) |
| `HierarchyResources.F90`, `HierarchyRegistry.F90` | Container for one `HierarchyGraph`; process-wide registry keyed by `HierarchyId` (`request_new_hierarchy`, `get_hierarchy_resources`, `release_hierarchy`) |
| `GraphNode.F90`, `BaseGraphNode.F90`, `StateItemNode.F90`, `OperationGraphNode.F90`, `MethodGraphNode.F90` | Node hierarchy. `StateItemNode%has_short_name()` is false for a nameless node (extension, expression result); `get_short_name()` then returns `''`. |
| `GraphStateItem.F90` | Payload of a `StateItemNode`: Field/FieldBundle/State handle (at most one allocated) and the member maps. The variant tag is read and written only through `MAPL_FieldGet/Set(state_item_variant=)`, `MAPL_FieldBundleGet/Set(state_item_variant=)` and `MAPL_StateGet/Set(state_item_variant=)`; `MAPL_StateItem_Flag` lives in `enums/StateItemFlag.F90` and its Info key in `infrastructure/field/StateItemVariantInfo.F90` and `utils/MAPL_ESMF_InfoKeys.F90`. |
| `StateItemDescriptor.F90` | Description of a state item, owned by `StateItemNode` (`get_descriptor()` returns a pointer): the characteristics map (`has/get/set_characteristic`, `get_characteristics`) plus the `declared_*` values, and the (item type, kind) mirrored-export allowlist. Independent of any ESMF handle. |
| `DependencyNetwork.F90` | Acyclic adjacency-list storage |
| `ComponentGraph.F90`, `ComponentGraph_DemandDrivenUpdate.F90` | Per-`OuterComponent` owner of resource index, import/export port tables, child-port bindings, its own write-back networks, lifecycle state; demand-driven `update(node_id, rc)` over the forward network. It has no network of its own for forward edges: `get_forward_network_id` returns the hierarchy's. Node/network storage is in `HierarchyGraph`, reached via a stored `HierarchyId`. |
| `NodeRevision.F90`, `NodeLabel.F90` | Revision stamps; visualization labels |
| `PortSpec.F90`, `PortBindingKey.F90`, `PortBindingTable.F90`, `ArgumentSpec.F90`, `AccessSpec.F90` | Transform port declarations and external port-binding table; method argument specs |
| `ExtensionResolution.F90` | Mismatch detection (`find_mismatched_characteristics`), extension-chain build (`find_or_build_extension_chain`), reuse lookup |
| `ExtensionMaterialization.F90`, `CompositeStateMaterialization.F90`, `GeomProxyField.F90` | Real ESMF object creation for extension nodes, composite members, geometry nodes |
| `MethodInvocation*.F90`, `GridCompMethodInvocation.F90`, `StateMethodInvocation.F90`, `GridCompDriverPhaseInvoker.F90` | Method invocation layer |
| `callback/*` | Callback interface/method/state-binding data model and registry |
| `RouteHandleKey.F90` | Semantic key for route-handle reuse |
| `GraphExport.F90` | Graph-neutral DOT/JSON topology export |
| `GraphOwnsUserStates.F90` | Test-only switch `set_graph_owns_user_states` (off by default); `graph_populates_user_states()` is the switch AND `GRAPH` mode, and every gate asks that, never the bare switch |
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

Materialization: `materialize_field_extension(graph, node_id [, name
[, with_members]], rc)` creates an empty field, calls the node's
`sync_to_info(STRUCTURAL)`, runs `MAPL_FieldEmptyComplete`, and sets
`allocation_status` to `ALLOCATED`. A characteristic must therefore write in `write_info`
(STRUCTURAL) whatever `FieldEmptyComplete` needs. Geom and vgrid come
only from the descriptor; `GraphBuilder` names no characteristic kind or
concrete characteristic type. The descriptor's `fill_value` (recorded by
`advertise_one` for every intent, applied only from the export's) is set on
the field after `FieldEmptyComplete`. `rebuild_field_extension(graph,
node_id, rc)` runs the same build for a node whose payload is already a
Field.
A node's Field is built in three steps on one object: `create_field_extension`
(empty Field, `allocation_status` `CREATED`), `describe_created_field`
(geometry written, ESMF GRIDSET) and `complete_field` (`FieldEmptyComplete`,
fill value, `ALLOCATED`). `materialize_field_extension` completes a CREATED
Field in place and is a no-op for any other existing Field;
`rebuild_field_extension` is a no-op on a created Field. "Graph-created"
means status `CREATED` and an original Field (alias id 0), so a legacy Field
that carries the same status key is not mistaken for one
(`payload_is_created` in `ExtensionMaterialization.F90`).
A node whose descriptor declares a VECTOR or BRACKET goes through the same
statuses with a `FieldBundle` of that type (`is_bundle_class`, `new_bundle`,
`complete_bundle`, `create_members`, `ExtensionMaterialization.F90:395-544`).
`create_bundle_extension` makes the empty `CREATED` bundle (alias id 0, with
the vector basis, default `NS`, or `bracket_updated`);
`materialize_field_extension` completes a `CREATED` bundle in place: the
descriptor is written, then, when `with_members` is true, a VECTOR gets two
members named for the components and a BRACKET `bracket_size` members (a
missing `bracket_size`, or more than 2, fails), each a complete Field with
units, typekind, geom and fill value (`create_members`); the status becomes
`ALLOCATED`. A completed bundle is left alone (`:101-109`). A chain link is
completed without members: the Transform that fills it creates them at
compute. `payload_is_created` is true for such a bundle (an original, with
an allocation status in its Info) as for a Field.
`advertise_one` creates the Field at advertise for INTERNAL, EXPORT and
IMPORT nodes of FIELD class when graph mode is not `REGISTRY`, and the empty
bundle (`create_bundle_extension`) for every VECTOR item and for a BRACKET
export, whichever way the user-state switch is set. A BRACKET import or
internal gets none (a bracket exists only for ExtData, as an export).
Composites, expressions, services, state items and plain FIELDBUNDLEs get
no payload (`GraphBuilder_Advertising_smod.F90:109-134`). A declared name
containing `/` gets a Field or bundle named by the part after the last slash.
`make_characteristics` records a VECTOR's `vector_basis_kind` and component
names (split from a `(n1,n2) suffix` standard name; none when there is no
tuple) and a BRACKET's `bracket_size` as plain descriptor attributes, like
`fill_value`: never compared or mirrored, only used to shape the members
(`StateItemDescriptor.F90:89-97`, `make_characteristics.F90:107-165`).
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
`SYNC_PHASE_STRUCTURAL`, `SYNC_PHASE_DESCRIPTIVE` and
`SYNC_PHASE_ADVERTISED`; `is_structural()` kinds are skipped in both
non-STRUCTURAL phases, `StateItemDescriptor.F90:719-727`; ADVERTISED is for
a Field that is only created, `StateItemCharacteristic.F90:86-94`, and
`PhysicalUnits` writes `units` in STRUCTURAL and ADVERTISED only,
`PhysicalUnitsCharacteristic.F90:130-133`). Every kind with a factory overrides
`write_info` (`Attributes` as a no-op: no Info key). The base version
fails, so a new kind without one fails `sync_to_info` loudly. Only `populate_one` calls into it, through `StateItemDescriptor%sync_per_alias_to_info` (entries whose `is_per_alias()` is true: `RestartMode`, `LongName`); extension and export field materialization calls
`sync_to_info(STRUCTURAL)` on the node's own descriptor. An import with no declared `long_name` inherits the export's: the owning field's alias-0 entry is written by the full STRUCTURAL sync. Factories live in
`specs/VariableSpec/make_characteristics.F90`: the type-bound
`VariableSpec%make_characteristics` loops over a registry of per-kind
factories (so far units, typekind, ungridded dims, geometry, vertical grid, vertical stagger, vertical orientation, horizontal dims spec, standard name, attributes, item shape, long name, restart mode, quantity type, conservation, normalization and expression). Procedure: `docs/graph/adding-a-characteristic.md`.

| Characteristic | Kind constant | `build_transform` today |
|---|---|---|
| `PhysicalUnits` | `PHYSICAL_UNITS_CHARACTERISTIC_KIND` | Real: `UnitsConverterTransform`, for a Field or, member by member, a VECTOR bundle (`UnitsConverterTransform.F90:114`; only VECTOR reaches it: plain FIELDBUNDLE links are refused at materialization) |
| `TypeKind` | `TYPE_KIND_…` | Real: `TypeKindConverterTransform`, for a Field or, member by member, a VECTOR bundle (`TypeKindConverterTransform.F90:101`; only VECTOR reaches it: plain FIELDBUNDLE links are refused at materialization) |
| `ItemShape` | `ITEM_SHAPE_…` | Every shape. Real for `BRACKET`→`FIELD` only (`BracketTimeInterpolationTransform`); `VECTORBRACKET`→`VECTOR` unsupported; pairs the shape matrix rejects (including `VECTORBRACKET`→`VECTORBRACKET`) are incompatible. Owns `get_extension_order` |
| `Geometry` | `GEOMETRY_…` | Real for a pair that both hold an `ESMF_Geom`, with the param from `select_regridder_param` (the side that declares one, else the default). An id-only pair (component-level geometry wiring) is unsupported; both sides declaring a regridder param is incompatible, checked first (`GeometryCharacteristic.F90:190-224`, shared by both item paths). Overrides `build_transform_for_item` to dispatch on the tip's `ITEM_SHAPE` item class: FIELD gets `HorizontalRegridTransform`; VECTOR and BRACKET get `HorizontalBundleRegridTransform`; FIELDBUNDLE, VECTORBRACKET, SERVICE, STATE and WILDCARD are unsupported. A tip with no `ITEM_SHAPE` entry (component-level wiring) is not restricted (`:230-263`). `get_signature` appends the regridder param's `to_string()`, so imports asking for different methods do not share a chain (`:267-303`) |
| `VerticalGrid` | `VERTICAL_GRID_…` | Single overlap: still unsupported (`vcoord_build_transform` does not build `VerticalColumnRegridTransform`, `VerticalGridCharacteristic.F90:252-286`); incompatible and ambiguous overlaps `_FAIL` with distinct messages |
| `VerticalStagger` | `VERTICAL_STAGGER_…` | Unsupported — mismatch is detected, repair is a stub (`VerticalStaggerCharacteristic.F90:98-108`) |
| `VerticalOrientation` | `VERTICAL_ORIENTATION_…` | Unsupported — mismatch is detected, `build_transform` does not build `VerticalFlipTransform` (`VerticalOrientationCharacteristic.F90:97-107`). A grid with no direction (a single level) leaves the entry `SPECIFIED` holding `UNSPECIFIED`; `write_info` writes nothing for it, so the Field keeps `with_grid` (`VerticalOrientationCharacteristic.F90:135-136`). An `INVALID` direction still fails `write_info`. `UNSPECIFIED` against an explicit up or down still compares unequal. `FixedLevelsVerticalGrid%derive_direction` sets a fixed-levels grid's direction from its levels and `physical_dimension` (exact, case-sensitive): `height` and `altitude` increasing = UP, decreasing = DOWN; `pressure`, `depth`, `layer`, any other name or an unset dimension: increasing = DOWN, decreasing = UP; one level is `UNSPECIFIED`; non-monotonic levels fail (`FixedLevelsVerticalGrid.F90:80-110`). `BasicVerticalGrid` and `MirrorVerticalGrid` are DOWN (the default in `VerticalGrid.F90:17`); `ModelVerticalGrid` is DOWN unless its YAML `direction:` is set (`ModelVerticalGrid.F90:47,330,445-449`). So by default a fixed height grid (UP) and a model height grid (DOWN) differ in direction |
| `HorizontalDimsSpec` | `HORIZONTAL_DIMS_SPEC_…` | Incompatible — GEOM vs NONE is detected in both directions and can never connect |
| `Attributes`, `UngriddedDims`, `Conservation`, `Normalization` | `…_CHARACTERISTIC_KIND` | Incompatible — legacy rejects every mismatch of two non-mirror values, so a detected mismatch can never connect |
| `QuantityType` | `QUANTITY_TYPE_CHARACTERISTIC_KIND` | Incompatible for a quantity-type mismatch; unsupported when only the basis differs (both real bases; `BASIS_NONE` is a basis wildcard, `QUANTITY_UNKNOWN` is not a wildcard): a conversion is possible but not built. Factory status by intent: declared -> `SPECIFIED`; undeclared (or declared `QUANTITY_UNKNOWN`) -> `DEFERRED` on EXPORT/INTERNAL, `UNCHECKED` on IMPORT. `is_deferred_fatal` is `quantity_type_required()` (`ValidationPolicy.F90`, key `field_dictionary: require_quantity_type`, no legacy counterpart; strict validation alone does not set it). An unsupplied one falls back to `UNCHECKED` with one summary warning |
| `Expression` | `EXPRESSION_CHARACTERISTIC_KIND` | Never compared; the declared expression string (`ValueCharacteristic`, no Info key); `build_transform` is the unsupported stub. Only EXPRESSION items carry it. The `UngriddedDims`, `Attributes`, `QuantityType`, `Conservation`, `Normalization` and `RestartMode` factories also give an EXPRESSION item an entry (via `has_field_metadata` / `carries_field_description`), so an EXPRESSION export with ungridded dims against a FIELD import is a connection conflict |
| `StandardName` | `STANDARD_NAME_…` | By validation mode (`ValidationPolicy.F90`, design A25): strict, incompatible; permissive (default), a match with a connection warning (`match_warning`), the export's name wins. FIELD and BRACKET carry it |
| `LongName` | `LONG_NAME_…` | Never compared (`needs_extension_for` is always false); `build_transform` is the unsupported stub |
| `RestartMode` | `RESTART_MODE_…` | Never compared; `build_transform` is the unsupported stub. FIELD only; per-alias |

Other files: `CharacteristicStatus.F90`, `StateItemCharacteristicKind.F90`
(also `MOCK_CHARACTERISTIC_KIND`), `SetSharedCharacteristic.F90` (`set_shared_characteristic(kind, characteristic)` sets that one entry in the node's descriptor; a structural characteristic whose payload is an `ESMF_Field` rebuilds that field from the descriptor through `rebuild_field_extension` (same name, fill value applied; a State or importer holding the old Field is not updated); only direct `StateItemNode` successors are reset, so a transform step feeding a changed import keeps stale state).

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
`ExpressionTransform` reads every input as a Field, so an input whose declared
item type is not FIELD is reported `unsupported_item_class:<class>` at resolve
time, before any aliasing (§2.7).

#### Transforms (`graph/transforms/`)

`TransformBase`, `Transform` (abstract; `Transform` adds the compute
interface over named ports), `TransformGraphNode` (`get_transformer()`
returns the held `Transform` read-only, `TransformGraphNode.F90:96-101`),
and eight concrete transforms: `UnitsConverterTransform`,
`TypeKindConverterTransform` (both also convert a VECTOR bundle member by
member), `BracketTimeInterpolationTransform` (built by
`ItemShapeCharacteristic%build_transform`), `ExpressionTransform` (built
by the connect-time expression path in `GraphBuilder_ConnectionResolution_smod.F90`, not by a characteristic),
`HorizontalRegridTransform` (Field; built by
`GeometryCharacteristic%build_transform_for_item`),
`HorizontalBundleRegridTransform` (VECTOR and BRACKET bundles; built by the
same method), and the two vertical Transforms below. The horizontal regrid Transform gets its regridder from
`RegridderManager` at the first `compute`, from the bound fields' geoms and
typekinds, and gets a new one when a geom or typekind changes.
`HorizontalBundleRegridTransform` regrids a bundle with one
`Regridder%regrid(fb_in, fb_out)` call, so the two vector components go
through one rotated regrid. It creates the output members at compute (a
clone of each input member on the output geom) and again when the input
member count changes. A regridded VECTOR takes the input `vector_basis_kind`.
A BRACKET is regridded when `bracket_updated` is true, and on the first
compute or after a regridder, geom, typekind or member-set change whatever
the flag says; it also copies the input interpolation weights to the output.
`BundleMemberSupport.F90` (`ensure_output_members`, `copy_bundle_attributes`,
`copy_vector_basis`, `copy_interpolation_weights`) holds the member code the
bundle regrid and the units and typekind converters share. Each creates the
output members at compute, one clone of each input member on the output
bundle's geom and typekind, and again when the input member count differs;
a created member takes the output bundle's typekind and units. The chain
links they fill are memberless bundles made by `materialize_extension_chain`
(§2.6). A units or typekind step never sees a BRACKET: its chain order
(`BRACKET_ORDER`, `ItemShapeCharacteristic.F90:206-207`) has neither kind.
`EsmfRegridderParam%to_string` (a deferred `RegridderParam` method; compact
JSON of `make_info`, `EsmfRegridder.F90:292-306`) is what the Geometry
signature, and so the chain key, carries.

`VerticalColumnRegridTransform` (Field, R4 only, linear only; copy or
flip when the two grids match, otherwise weights between the grids'
column coordinates at level centers, rebuilt when a grid, stagger or
alignment changes) and `VerticalFlipTransform` (Field, R4 and R8; index
reversal for an orientation-only mismatch) exist with isolated tests
(`Test_VerticalColumnRegridTransform.pf`) but **no characteristic builds
either one**: `VerticalGrid`, `VerticalStagger` and `VerticalOrientation`
all still report unsupported (table above). Import levels outside the
export's range take the end value with one logged warning per weight
build (`VerticalColumnRegridTransform.F90:296-314`); an R8 field fails
(`:395-401`, message shared through `vertical_regrid_r8_message`).
Vertical-grid and orientation mismatches are therefore reported as
unsupported and the graph leaves those connections unwired. A geometry
mismatch on a FIELDBUNDLE, VECTORBRACKET, SERVICE, STATE or WILDCARD item is
likewise unsupported.

### 1.2 Integration layer (`superstructure/generic/`, top level)

`GraphBuilder.F90` is the public module: the `GraphBuilder`
type (stateless; every method is `nopass` and takes the
`OuterMetaComponent`) and cross-submodule interfaces. Ten submodules:

| File | Responsibility |
|---|---|
| `GraphBuilder_Advertising_smod.F90` | `advertise_one` creates `StateItemNode`s from `VariableSpec`; `VariableSpec%make_characteristics` (called on the stored descriptor) fills the characteristics map, `declared_item_type` and `fill_value` |
| `GraphBuilder_ConnectionResolution_smod.F90` | Ordinary/Simple/Match connection resolution (`resolve_one`, `for_each_matching_import`); extension-chain use; import mirroring (`run_propagate_hook`) and export propagation (`run_export_propagate_hook`); Expression names check (`run_expression_hook`) and per-consumer evaluation (`resolve_one_pair`); Wildcard fan-in (`resolve_wildcard_destination`); unsatisfied-import checks; qualified-name resolution; aliasing of a legacy export's Field or FieldBundle (`try_alias_export_field`, `find_alias_owner`, §2.7); chain-link materialization (`materialize_extension_chain`, `check_materializable`) |
| `GraphBuilder_Freeze_smod.F90` | Hierarchy-wide freeze and convergence check (`run_graph_freeze_hook`): freezes every component's graph, then the hierarchy's forward network once (`freeze_forward_network`, `GraphBuilder_Freeze_smod.F90:49-54`) |
| `GraphBuilder_Geometry_smod.F90` | Geometry push/pull (`run_geometry_hook`); its edges go in the forward network. The push skips a component's own `FROM_CHILD` provider, which would close a cycle (`:300-306`) |
| `GraphBuilder_CallbackWiring_smod.F90` | Wildcard/regex callback-argument wiring against the flattened qualified-export namespace. Get arguments join the forward network; each method gets its own put (write-back) network (`:384-427`) |
| `GraphBuilder_LifecycleHooks_smod.F90` | Wrappers called from `OuterMetaComponent` lifecycle sites: advertise, activate, connect, populate-state hooks; logs `GraphResolutionEntry` results |
| `GraphBuilder_RunPhaseNode_smod.F90`, `GraphBuilder_RunPhasePull_smod.F90` | Dormant `MethodGraphNode` construction (no lifecycle hook builds the run-phase node; nothing reads or invokes it); run-phase pull and advance. `run_pull_hook` runs `ComponentGraph%update` on each of the component's own advertised imports before its user run, through the hierarchy's forward network, so it reaches a producer wherever the connection was declared; `run_advance_hook` advances the revision of its own advertised exports after a successful run. Both are no-ops outside `GRAPH` mode (`GraphBuilder_RunPhasePull_smod.F90:25-93`) |
| `GraphBuilder_StatePopulation_smod.F90` | Populates the real `ESMF_State` from graph nodes (`graph` mode only; a no-op in `registry` and `both`). Optional `own_only` for advertise time; `populate_descendants` walks every descendant; `graphbuilder_populate_outer_state` fills the parent-visible outer states at realize_accepted (switch on) |
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
`Test_GraphBuilderMaterializeGate.pf` (materialization gates, export
aliasing by owner walk and for bundle exports `test_alias_*`, graph-created
VECTOR and BRACKET bundles for exact-match exports and chain inputs, bundle
chain links, and graph-mode state population, including bundles
(`test_populate_state_*bundle*`) and the outer-state population tests
`test_populate_outer_state_*`), `Test_GraphGeometryHook.pf`,
`Test_ForwardNetworkErrors.pf` (the forward-producer error message),
`Test_GraphBuilder_ConnectionResolution.pf` (includes the Wildcard and
callback paths with a qualified source component such as `MID/SRC`: members
and descendants bind with names relative to the source, a missing source
fails with `status /= 0`), `Test_GraphBuilder_RunPhaseNode.pf` (the dormant run-phase builder, and that
`run_connect_hook` leaves the run-phase NodeId invalid), and
the fixtures `tests/scenarios/` (legacy YAML scenarios) and
`tests/graph-scenarios/` (graph-mode fixtures). `Test_Scenarios.pf` also has
`check_legacy_couplers` (§2.4) and the bundle scenarios `vector_regrid` and
`bracket_regrid`; the BRACKET export of the latter is declared in code by
`tests/gridcomps/ProtoBracketGridComp.F90`, since YAML cannot declare a
bracket.

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
- The hierarchy has exactly one **forward** `DependencyNetwork`, owned by
  `HierarchyGraph` and created with the hierarchy. Every edge that carries
  data toward a consumer goes into it (ordinary connections, extension
  chains, expression evaluations, collections, geometry edges, and the get
  side of a callback argument), whichever component's declaration produced
  the edge. Every `ComponentGraph` reports the same id from
  `get_forward_network_id`; there is no per-component default network and
  no `get_default_network_id`. A `ComponentGraph` owns only the
  **write-back** networks it makes with `create_network` (the put side of a
  callback method, the return edge of an inout borrower), and a pull never
  follows one. `ComponentGraph%update(node_id, rc)` takes no network id.
  The transform `PortBindingTable` and the reuse index are shared the same
  way; the advertised-item resource index stays per component.
- `add_dependency` into the forward network: an identical re-add is a
  no-op, and a second, different producer into a `StateItemNode` fails
  naming the item (`forward_producer_conflict`,
  `ComponentGraph.F90:365-433`). Edges into a `TransformGraphNode` are not
  limited. For ordinary and inout-borrower pairings, connection resolution
  checks first and records a pair-level `connection_conflicts` entry
  instead of failing the pass: the second producer is skipped and the first
  stays (`second_producer`, called at
  `GraphBuilder_ConnectionResolution_smod.F90:413,533`; the inout borrower
  case at `:1459-1464`). Only a second producer resolved in the same
  connect pass is seen: an import settled by an earlier pass is skipped
  silently (`:255-274`). Wildcard fan-in, expression, callback-collection
  and geometry edges have no pre-check and hit the `add_dependency`
  assertion. `Test_ForwardNetworkErrors.pf`
  and `Test_ForwardNetwork.pf` cover the graph side.
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
  `ComponentGraph` (each validates and freezes its own write-back
  networks), then validates and freezes the forward network once
  (`freeze_forward_network`, idempotent), then runs the hierarchy-wide
  convergence check (any remaining unresolved required import is a hard
  failure).
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
| `initialize_advertise.F90` | `recurse()` (children first) → `self_advertise` → `gb%run_advertise_hook` → `gb%run_reexport_hook` → legacy `process_connections` → `gb%run_activate_hook` → `gb%run_propagate_hook` → `registry%propagate_unsatisfied_imports` → `gb%run_export_propagate_hook` → `registry%propagate_exports` → either `gb%run_populate_state_hook(..., own_only=.true.)` (user-state switch on) or `registry%add_to_states(mode='user')` (`initialize_advertise.F90:48-123`) |
| `initialize_accept_transfer.F90` | `gb%run_connect_hook`, then `gb%run_expression_hook`, then `gb%run_populate_state_hook` |
| `initialize_geom_b.F90` | `gb%run_geometry_hook` |
| `initialize_realize_accepted.F90` | `recurse()` → user phase → legacy `registry%allocate` → `gb%allocate_internals` → user-state `add_to_states(mode='user')` (switch off only) → outer states: `gb%run_populate_outer_hook` (switch on, `GRAPH` mode) or `registry%add_to_states(mode='outer')` (`initialize_realize_accepted.F90:26-43`) |
| `initialize_graph_freeze.F90` | `gb%run_graph_freeze_hook` (root only) |
| `run_user.F90` | import couplers (legacy) → `gb%run_pull_hook` → user run → `gb%run_advance_hook` → export couplers (legacy); both coupler loops are skipped when `graph_populates_user_states()`, and so is the import-coupler initialize in `initialize_user.F90:32-38` (`run_user.F90:51-57,71-77`) |

`run_reexport_hook` runs after `run_advertise_hook` and before export
propagation: for each `ReexportConnection` it adds an export port named by
the re-export destination, bound to the source's existing node (an own
INTERNAL-intent node by resource index, or a descendant export's node by
port name). A repeat of the same name and node is a no-op; a different node
fails (`reexport_one`, `GraphBuilder_ConnectionResolution_smod.F90:957-1008`).
The internal gets no port or mirror of its own.

With the user-state switch off (default) legacy fills the real user states and
the graph creates, completes and aliases Fields, creates and completes VECTOR
and BRACKET bundles (§2.6), and aliases the legacy FieldBundle of a VECTOR,
SERVICE or BRACKET export (§2.7); `populate` inserts only completed Fields
(never a bundle) and skips INTERNAL. At accept_transfer, off: legacy fills
the real user states, then the call site passes scratch states (real import
state, throwaway export and internal) to the populate hook
(`initialize_accept_transfer.F90:62-86`). On, in `GRAPH` mode: legacy
user-state population is skipped (`initialize_advertise`,
`initialize_realize_accepted`), the call site passes the real user states,
and `populate` fills the real user import, internal and export states and
completes INTERNAL Fields. In that mode the legacy import and export couplers
do not run (`run_user.F90:51-57,71-77`), and the import couplers are not
initialized (`initialize_user.F90:32-38`): their payload Fields are never
written, so running them would convert the unwritten fill value. The graph
pulls the imports itself (`run_pull_hook`) and advances the exports
(`run_advance_hook`). With the switch off, in `REGISTRY` mode and in `BOTH`
mode the couplers still run.
`Test_Scenarios.pf` `check_legacy_couplers` asserts, for the four units
scenarios that set the switch (`invalidate`, `units_mismatch_run_phase`,
`grandparent_units_run_phase`, `units_and_typekind_mismatch`), that every
legacy coupler's `import[1]` and `export[1]` Field still holds the fill
sentinel, compared by bits (`Test_Scenarios.pf:1072-1090`).

With the switch on, population runs twice. At the end of ADVERTISE
(`initialize_advertise.F90:118-121`) the hook runs with `own_only`: only the
component's own user states, each item's own created Field, with the
non-structural keys written through `SYNC_PHASE_ADVERTISED` (units, standard
name, long name, ...; never geom or typekind), no child walk, no completion
and no allocation (`GraphBuilder_StatePopulation_smod.F90:51-55,84-86`;
`write_advertised_keys`, `ExtensionMaterialization.F90:233-247`). A user
gridcomp that reads its own state in `INIT_MODIFY_ADVERTISED` therefore
finds its items. `populate_one` is substate-aware (`get_target_substate`,
shared with the export-port walk): a qualified name `A/E_A1` is inserted as
`E_A1` in substate `A`. With the switch off, `own_only` is a no-op, which
keeps the key write off graph-created Fields. The advertise-time call is
hook style: a failure is logged as a warning by `report_if_failed`
(`GraphBuilder_LifecycleHooks_smod.F90:293-306`) and advertise continues. The accept_transfer pass then repeats the insert (add-or-replace
by alias name, so it is idempotent) and walks every descendant, not only
direct children (`populate_descendants`), each against the forward network;
below the direct children an import with no producer yet gets no
own-created fallback, because an ancestor whose connection supplies it
inserts the right Field and the fallback would replace it
(`GraphBuilder_StatePopulation_smod.F90:168-220`, `:277-283`). `allocate_internals`
(`initialize_realize_accepted.F90:30`) allocates each INTERNAL whose
descriptor is complete by then; an unconnected export stays created, never
allocated. With the switch on, `populate` also walks the component's
export-port table (`populate_export_ports`, `populate_export_port`,
`GraphBuilder_StatePopulation_smod.F90:319-399`). Each port's bound-node
Field is aliased into the user export state under the PORT's name: a renamed
re-export or a re-exported INTERNAL. A port is skipped only when the
declared export of the same name resolves to the same node; a same-named re-export of a child's export is a different node
and is added. An INTERNAL-bound port is completed and allocated; an
export-bound port is only described, so it stays `CREATED`/`GRIDSET`.
A node holding neither a Field nor a FieldBundle is skipped. A propagated port (a slash in the name)
is skipped by the user passes (advertise and accept_transfer): the
`include_propagated` argument is false for them
(`GraphBuilder_StatePopulation_smod.F90:111,352`), and a propagated child
export is written only to the outer export state, as legacy does
(`registry/StateRegistry_Actions_smod.F90:73`). `sync_alias_info` writes the
per-alias keys (long_name, restart mode) on each alias. With the switch off,
or in `BOTH` mode, the walk adds nothing.

Bundles: with the switch on, `populate_one` and `populate_export_port` read
the node's payload with `get_item_if_materialized` (a Field or a FieldBundle)
and insert a bundle as a named alias, like a Field
(`GraphBuilder_StatePopulation_smod.F90:233-336,565-599`). A VECTOR or
BRACKET export goes into the user and outer export states; an import is
found by the producer walk (`find_materialized_predecessor` accepts a bundle
payload that is not merely created); an unconnected VECTOR import keeps its
own empty bundle, and an unconnected export keeps its empty bundle too.
`allocate_internals` completes a VECTOR internal with its two members, as it
does a Field internal. With the switch off legacy fills the states and the
graph adds only Fields (`:299-301`). `sync_alias_info` does nothing for a
bundle alias.

The graph also fills the **outer** import and export states, the ones the
parent sees, at `INIT_REALIZE_ACCEPTED` (`initialize_realize_accepted.F90:38-44`,
not accept_transfer: a child's accept_transfer fill runs before the parent's
connect step). `gb%run_populate_outer_hook` calls
`graphbuilder_populate_outer_state`
(`GraphBuilder_StatePopulation_smod.F90:131-167`): a third populate call,
on the outer `MultiState`, reusing `populate_one` (declared IMPORT and
EXPORT items only, `own_only` false, own-created fallback allowed) and then
`populate_export_ports` with `include_propagated` true, so a propagated
`child/E` lands in a substate of the outer export state, made by
`get_substate` as `FieldClassAspect%add_to_state` does
(`populate_export_port`, `GraphBuilder_StatePopulation_smod.F90:363-399`). INTERNAL items and
descendants are skipped: each component fills its own outer states at its
own realize_accepted. The user and outer aliases are two named aliases of one
`ESMF_Field` (`Test_GraphModeDemonstration.pf`,
`test_switch_on_outer_states_share_user_fields`). A hook failure is a
warning, like the other hooks (`report_if_failed`,
`GraphBuilder_LifecycleHooks_smod.F90:268-276`). The legacy path that stays:
with the switch off, or outside `GRAPH` mode (the switch is gated by
`graph_populates_user_states()`, `graph/GraphOwnsUserStates.F90:32-34`),
`registry%add_to_states(mode='outer')` fills the outer states at
`initialize_realize_accepted.F90:42`; in `REGISTRY` and `BOTH` mode legacy also fills
them at accept_transfer (`initialize_accept_transfer.F90:89-91`). The graph
does not write position-labelled names such as `T(1)`; their expectation
items carry `legacy_only: true`, which `Test_Scenarios` skips when it applied
the switch (`Test_Scenarios.pf:563-568`).

`MAPL_SCENARIO_GRAPH_OWNS_USER_STATES` turns the switch on in
`Test_Scenarios` for every scenario, and a `scenario_table()` entry can set
`graph_owns_user_states` for one scenario (`scenario_reexport_twice`,
`regrid`, `regrid_r8`, `vector_regrid`, `bracket_regrid`, `invalidate`,
`expression_per_consumer_r8` (graph mode only), and the `graph-scenarios` `plain_field`,
`units_mismatch_run_phase`, `grandparent_units_run_phase`,
`units_and_typekind_mismatch` and `graphbuilder_propagation` do;
`Test_Scenarios.pf:180-181,195-203,210-211,218-232`). Either way the switch is
on only in `GRAPH` mode (`Test_Scenarios.pf:425-427`). Scenarios
that do not yet pass with the switch on are recorded in the tasks.md 3.41a,
3.41b and 3.44 Status; `scenario-parity.md` lists the measured rows.

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
| VECTOR, BRACKET (geometry mismatch) | The extension chain holds a `HorizontalBundleRegridTransform` link of the item's class (BRACKET→FIELD adds time interpolation after it). `materialize_extension_chain` completes a VECTOR or BRACKET link as an `ALLOCATED` memberless `FieldBundle` of that type; the Transforms create the members at compute. A link declared any other non-FIELD class is refused with `unsupported_item_class:<class>` (`GraphBuilder_ConnectionResolution_smod.F90:1732-1775`). `check_materializable` (`:1697-1724`) admits FIELD, VECTOR and BRACKET on both sides (a BRACKET import side is admitted for a chain input, where the export is passed as its own import); SERVICE and every other class give `unsupported_item_class:<class>`. A VECTOR or BRACKET export the graph consumes (exact match, or the input of a chain) is not aliased from legacy when none is there: the graph's advertise-created bundle is completed in place with its members by `try_materialize_field_extension` (`with_members`, `:1671-1690`); a legacy bundle in the owner's user export state is aliased instead when the switch is off (§2.7). A VECTOR with a vertical mismatch is unsupported at VERTICAL_GRID (no bundle vertical Transform). A chain link takes its declared class from its `ITEM_SHAPE` entry (`StateItemDescriptor%take_declared_item_type_from_shape`; an EXPRESSION shape gives FIELD) |
| BRACKET → FIELD | `ItemShapeCharacteristic` + `BracketTimeInterpolationTransform` |
| VECTORBRACKET | Not handled: legacy only (matrix: VECTOR unsupported, others incompatible). `to_itemtype` in the compiled parser does not parse `'vector_bracket'`. |
| EXPRESSION | Placeholder export; names check in `graphbuilder_run_expression_hook`; one `ExpressionTransform` (N:1) evaluation per consumer, built in `resolve_one_pair` |
| WILDCARD | `resolve_wildcard_destination` (fan-in of every matching FIELD producer as composite members; other shapes are skipped) |
| SERVICE | Resolved like any other item by `resolve_one`. `is_ordinary_resolvable_import` only keeps an unresolved SERVICE import from being propagated to an ancestor. `try_alias_export_field` aliases a SERVICE export's `FieldBundle` from the owner's user export state like any bundle, with no SERVICE special case (§2.7). The graph creates no SERVICE bundle (`check_materializable` refuses it), so with the user-state switch on a SERVICE connection is reported `unsupported_item_class:SERVICE`. Being retired in favor of callbacks |
| Callback arguments | `GraphBuilder_CallbackWiring_smod.F90` |

### 2.7 Aliasing the legacy export (`try_alias_export_field`)

With the user-state switch off, resolution aliases the export's real payload
from the legacy user export state instead of creating one. With the switch
on (`graph_populates_user_states()`) it returns `found = .false.` at once,
because the state holds the node's own created Field
(`GraphBuilder_ConnectionResolution_smod.F90:1499-1601`).

- It takes the export **port name** (`effective_source_name`, the key
  `find_export_node_id` uses), not the node's short name. A qualified source
  component (`a/b`) is resolved with `resolve_qualified_component`.
- `find_alias_owner` walks the export-port tables down from the source
  component to the component whose user export state holds the item, which is
  read there under the leaf name. A name without `/` is the component's own
  export or re-export. A `<child>/<leaf>` port is either passed up unchanged
  from a deeper level (a child carries the same port name: recurse) or is
  that child's own export (the child is named `<child>` and has a port
  `<leaf>`). Port names are qualified once, at the immediate parent, so a
  split at the last `/` is wrong for a port propagated through two or more
  ancestors (`:1592-1625`). No owner found gives `found = .false.`.
- By item kind in the owner's state: a Field is aliased with
  `MAPL_NamedAlias` and set as the node payload (a created placeholder is
  destroyed unless it shares its host with the alias); a `FieldBundle`
  (VECTOR, SERVICE or BRACKET export) is aliased the same way. A payload
  that is only created is replaced and destroyed: the placeholder Field of
  a node declared FIELD, or an advertise-created VECTOR or BRACKET bundle
  with its members (`MAPL_FieldBundleDestroy`). A legacy Field aliased over
  a created bundle destroys it the same way (`:1565-1573`). A bundle already completed
  by a connection is not destroyed when later replaced by an alias. Any
  other kind (a nested State), or no item, gives `found = .false.` without
  failure and the caller falls through to its existing path
  (`:1544-1600`).
- An expression input is not aliased unless its declared item type is FIELD:
  `ExpressionTransform` reads every input with `get_field`
  (`ExpressionTransform.F90:157`), so `build_expression_evaluation` reports
  `unsupported_item_class:<class>` at resolve time, before the alias call
  (`GraphBuilder_ConnectionResolution_smod.F90:2047-2062`).
- The Wildcard and callback paths resolve a qualified source component
  (`MID/SRC`) with `resolve_qualified_component`, like the other paths; the
  namespace entry names are relative to that source component.

## 3. Known limitations (current)

- A horizontal regrid is built only for a pair whose two sides hold an
  `ESMF_Geom` (§1.1). An id-only geometry pair, and a geometry mismatch on a
  FIELDBUNDLE, VECTORBRACKET, SERVICE, STATE or WILDCARD item, are reported
  unsupported, so the graph leaves those connections unwired. A VECTOR or
  BRACKET geometry mismatch builds a `HorizontalBundleRegridTransform` chain
  whose links are memberless bundles filled at compute; units and typekind
  steps on a VECTOR are built too. VECTORBRACKET and plain FIELDBUNDLE
  have no bundle Transform.
- The graph creates no SERVICE bundle: a SERVICE export is filled only by
  aliasing the bundle legacy put in the user export state (§2.7), and
  `check_materializable` reports an unaliased one
  `unsupported_item_class:SERVICE`. So the `service_*` scenarios do not set
  the user-state switch (plan task C2-service-exports).
- A VECTOR or BRACKET export is created by the graph (§1.1, §2.6), but a
  user that reads the members of a chain-tip import bundle at init sees
  none until the first compute: chain links are memberless. A BRACKET
  export with no `bracket_size`, or more than 2, fails at connect with a
  message.
- A bundle (VECTOR or BRACKET) link whose step is a vertical-grid mismatch
  is reported unsupported: `VerticalGridCharacteristic%build_transform`
  builds nothing, and there is no bundle vertical regrid (plan
  `C1-vertical-b`), so `vector_1` stays pending.
- Two equal `BasicVerticalGrid`s from different components still mismatch:
  `VerticalGridCharacteristic%needs_extension_for` compares the manager's grid
  ids (`VerticalGridCharacteristic.F90:145`), where legacy compares by level
  count (`scenario-parity.md`, difference rows). `vertical_dim_spec: center`
  on both sides therefore builds a (unsupported) vertical chain.
- `VerticalColumnRegridTransform` and `VerticalFlipTransform` exist, but no
  characteristic builds them: a single-overlap `VerticalGrid` mismatch, a
  `VerticalStagger` mismatch and a `VerticalOrientation` mismatch are all
  reported unsupported (incompatible and ambiguous VerticalGrid overlaps
  `_FAIL`). The `vertical_regridding*`, `vertical_alignment_regrid` and
  `vertical_alignment_with_grid` scenarios stay `graph_ready = .false.`
  (`Test_Scenarios.pf:183-184,202,204-205`).
  The vertical column regrid is R4 and linear only, at level centers.
- An expression whose consumer has precision R8 (`expression_per_consumer_r8`)
  cannot run end to end with the user-state switch off: the legacy couplers
  run and fail on the expression item. With the switch on they are skipped
  and the scenario passes: its table entry sets `graph_owns_user_states`
  and it runs ready, in graph mode only (see `docs/graph/scenario-parity.md`).
- No run-phase node is built at connect time: `graphbuilder_build_run_phase_node`
  and `OuterMetaComponent%get_run_phase_method_node_id` are dormant, because
  nothing invokes the node and its arguments are keyed by short name only (a
  component with an import and an export of one name cannot be built).
  Reviving it needs a graph-mode skip of the legacy run call. `statistics_real`
  stays pending for one failing value check (`A/QV_MIN`, `GRAPH` mode;
  `scenario-parity.md`). The `graph: nodes:` count of a scenario's root graph
  therefore holds no run-phase node.
- The outer states are written by the graph only with the user-state switch
  on in `GRAPH` mode (§2.4); otherwise legacy fills them. The outer
  population covers Fields and, with the switch on, VECTOR and BRACKET
  bundles (`graphbuilder_populate_outer_state` reuses `populate_one`,
  `GraphBuilder_StatePopulation_smod.F90:318-323`), and never writes
  position-labelled names (`T(1)`).
- User-state population covers Fields and, with the switch on, VECTOR and
  BRACKET bundles (empty at advertise, §2.4); states and composites are
  not populated at advertise or later. Items a
  user gridcomp advertises itself with `mapl_GridCompAdvertiseVariable` are
  not in the graph, so they are not populated (tasks.md 3.44 Status).
- Export-port population: a port name that collides with an existing
  non-port item replaces it silently (no duplicate-name assertion).
  The port walk reads the
  component's own export-port table and has no depth limit; the walk over
  the declared imports and exports of the descendants reaches every
  descendant (`populate_descendants`). Composites are not populated (and
  `vector_1` stays pending for its vertical-grid mismatch). The scenarios that still fail with the switch on are in the
  tasks.md 3.41b and 3.44 Status.
- `find_materialized_predecessor` skips a created-only Field and reports
  not found past one; payload readers must tolerate EMPTY and GRIDSET
  Fields.
- Some mismatches are detected but not repaired (see §1.1): VerticalGrid
  (single overlap), VerticalStagger, VerticalOrientation, a QuantityType
  basis-only mismatch, VECTORBRACKET→VECTOR, and Geometry for an id-only
  pair or a FIELDBUNDLE, VECTORBRACKET, SERVICE, STATE or WILDCARD item.
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
| `MAPL.generic.graph` | `graph/tests/` (core, characteristics, transforms, callbacks, forward network). The executable initializes through `GraphCoreTestInit.F90`: ESMF, udunits and `pfl_initialize()` (`GraphCoreTestInit.F90:11,32-34`), because code under test may log warnings (the vertical regrid does) and pflogger aborts if never set up |
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
- **One forward network per hierarchy** (plan task C4): owned by
  `HierarchyGraph`, not by each component. A pull through it reaches a
  producer whichever component declared the edge; per-component networks
  split one data path across networks. Write-back networks stay per
  component and are never pulled. Wording reference:
  `docs/graph/spec/06-dependency-network.md` §6.2 and
  `spec/07-component-graph.md` REQ-CG-004.
- **Geometry is a graph node** (incomplete `ESMF_Field` tagged
  `MAPL_STATEITEM_GEOM`), so a component may derive its geom from
  children.
- **Graph shape dispatch is independent of legacy** (phases 7a/7b/7c):
  `ItemShapeCharacteristic`, `ExpressionTransform`, and wildcard
  fan-in duplicate rather than call `ClassAspect`/`ExtensionTransform`.
- **Characteristic dispatch** (phase 12,
  `2026-10-19-phase-12-characteristic-dispatch-wiring`): six further
  axes (Attributes, UngriddedDims, StandardName, QuantityType,
  Conservation, Normalization) wired into `make_characteristics`
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
