# Glossary: legacy layer to graph layer

The legacy component-hierarchy/coupler layer (`superstructure/generic/`
outside `graph/`: `specs/`, `registry/`, `connection/`, `transforms/`)
and the graph-native layer (`superstructure/generic/graph/` plus
`GraphBuilder*.F90`) solve the same problem with different vocabulary.
This table maps one to the other. "Parallel" means a code-independent
reimplementation, not a wrapper: graph code must not call the legacy
item (`docs/graph/CONVENTIONS.md` §3).

The planned end state replaces the legacy column; nothing in it is
deprecated yet.

## Core mapping

| Legacy (`specs/`, `registry/`, `connection/`, `transforms/`) | Graph layer | Notes |
|---|---|---|
| `StateItemAspect` | `StateItemCharacteristic` (`ValueCharacteristic`, `ReferenceCharacteristic`) | One metadata axis of an item. Legacy compatibility is `can_connect_to`/`matches`/`make_transform`; graph is `needs_extension_for`/`build_transform`. |
| `AspectStatus` (`ASPECT_STATUS_INVALID`, `FROM_COMP`, `SPECIFIED`, `MIRRORED`, `DEFERRED`, `UNCHECKED`) | `CharacteristicStatus` (`INVALID`, `SPECIFIED`, `MIRRORED`, `UNCHECKED`, `DEFERRED`) | Graph has no `FROM_COMP`. The plan collapses it into `SPECIFIED` (plan D2). |
| `AspectId` | `StateItemCharacteristicKind` | Map key identifying the axis. |
| `AspectMap` (inside `StateItemSpec`) | `StateItemCharacteristicMap` (inside `StateItemDescriptor`) | The map lives in the descriptor owned by `StateItemNode`. |
| `ClassAspect` (`FieldClassAspect`, `BracketClassAspect`, `ExpressionClassAspect`, `WildcardClassAspect`, …) | `ItemShapeCharacteristic` | Structural class of an item. Every shape carries one. Graph converts Bracket→Field only through the characteristic; Expression and Wildcard are resolved by dedicated `GraphBuilder` code. |
| `ClassAspect%get_aspect_order(goal_aspects)` | `ItemShapeCharacteristic%get_extension_order(goal, rc)` | Per-shape lists from legacy; applied by `resolve_characteristics`. Kinds a list omits follow by kind name. |
| `UnitsAspect` | `PhysicalUnitsCharacteristic` | |
| `TypekindAspect` | `TypeKindCharacteristic` | |
| `GeomAspect` | `GeometryCharacteristic` | Geometry is also a graph node (`GeomProxyField`). |
| `VerticalGridAspect` | `VerticalGridCharacteristic` | Plan adds a separate vertical-stagger characteristic. |
| `UngriddedDimsAspect`, `StandardNameAspect`, `AttributesAspect`, `QuantityTypeAspect`, `ConservationAspect`, `NormalizationAspect` | `UngriddedDimsCharacteristic`, `StandardNameCharacteristic`, `AttributesCharacteristic`, `QuantityTypeCharacteristic`, `ConservationCharacteristic`, `NormalizationCharacteristic` | Same names minus `Aspect`. Mismatch detection works; `build_transform` reports incompatible, except `StandardName`, whose mismatch is a match with a warning in permissive `ValidationMode` and incompatible in strict mode (see `ARCHITECTURE.md` §1.1), and `QuantityType`'s basis-only mismatch (same quantity type, two real bases), which reports unsupported because a conversion can exist. |
| `StateItemSpec` | `StateItemNode` + `StateItemDescriptor` + `GraphStateItem` | Spec = description + producer/consumer. Graph = node (identity, revision) + descriptor (description) + payload (ESMF handle); producer/consumer are dependency edges. |
| `VariableSpec` | `VariableSpec` (unchanged) | Fortran-level user declaration. Graph reads it in `advertise_one`; plan D5 adds `make_characteristics` beside `make_aspects`. |
| `ExtensionTransform` (`ConvertUnitsTransform`, `CopyTransform`, `RegridTransform`, `VerticalRegridTransform`, `TimeInterpolateTransform`, `NormalizationTransform`, `EvalTransform`) | `Transform` / `TransformGraphNode` (`UnitsConverterTransform`, `TypeKindConverterTransform`, `BracketTimeInterpolationTransform`, `ExpressionTransform`, `HorizontalRegridTransform`, `VerticalColumnRegridTransform`, `VerticalFlipTransform`) | Graph transforms compute over named ports. `HorizontalRegridTransform` duplicates `RegridTransform` for Fields and is built by `GeometryCharacteristic%build_transform` when both sides hold a geom (a FieldBundle is not regridded in the graph). `VerticalColumnRegridTransform` and `VerticalFlipTransform` correspond to `VerticalRegridTransform` (column regrid, R4 linear only) and the orientation flip; no characteristic builds either yet (`ARCHITECTURE.md` §1.1). |
| `CouplerMetaComponent`, `GenericCoupler` (gridcomp wrapping a transform) | `TransformGraphNode` node in the dependency graph | |
| `StateRegistry` | `ComponentGraph` (per component) over `HierarchyGraph` (hierarchy-wide storage) | Legacy registry also builds the real `ESMF_State`; graph does it in `GraphBuilder_StatePopulation_smod.F90` (`graph` mode only). |
| `VirtualConnectionPt` | `NodeId` (item) and `PortId` (named port) | Legacy keys by declared name; graph keys by opaque, hierarchy-wide id. Declared names still match connections via `VirtualConnectionPt%matches()`. |
| `ActualConnectionPt` | `NodeId` of the resolved node | |
| `ExtensionFamily` (primary + derived specs per virtual pt) | `ExtensionResolution` (`find_mismatched_characteristics`, `find_or_build_extension_chain`) and extension `StateItemNode`s | Chain of extension nodes replaces the family set. |
| `StateRegistry%propagate_unsatisfied_imports` / `propagate_exports` | `graphbuilder_run_propagate_hook` / `graphbuilder_run_export_propagate_hook` | Run in sequence with the legacy calls in `initialize_advertise.F90`. |
| `Connection`, `SimpleConnection`, `MatchConnection`, `ReexportConnection` | Same types (shared) | Graph resolution dispatches on `SimpleConnection` and `MatchConnection`; `ReexportConnection` is handled by `run_reexport_hook`, which adds an export port. These are in the integration seam, not duplicated. |
| `ExpressionClassAspect` + `EvalTransform` | Expression placeholder + `graphbuilder_run_expression_hook` (names check) + per-consumer `ExpressionTransform` evaluation | N:1, not pairwise; one evaluation per consumer, built in `resolve_one_pair`. `ExpressionCharacteristic` holds the declared expression string. |
| `WildcardClassAspect` | `resolve_wildcard_destination` | Each match is a composite member of the import. |
| `ServiceClassAspect` | callbacks (`graph/callback/`) | Service is being retired, not migrated. |
| `GRAPH_MODE_*` | `mapl_GraphMode_mod` | `registry` (default, legacy only), `both`, `graph`. Set only by test code. |

## Graph-only terms

| Term | Meaning |
|---|---|
| Advertise | `advertise_one` creates a `StateItemNode` from a `VariableSpec`. |
| Activate / connect | Connection resolution (`run_activate_hook`) and post-freeze convergence (`run_connect_hook`, `run_graph_freeze_hook`). |
| Mirrored import | An import bubbled up from a child into the parent's import-port table (not the resource index). |
| Propagated export | A child's export in the parent's export-port table, named `child/short_name`. |
| Re-export port | A graph export port created by `run_reexport_hook`, named by the re-export destination and bound to the source's existing node (own internal or descendant export). Legacy: `ReexportConnection`. |
| Export port walk | `populate_export_ports` in `graphbuilder_populate_state`: aliases each export port's bound-node Field into the user export state under the port name (switch on, `GRAPH` mode). Legacy: the `add_to_states` primary pass in `user` mode, which skips child-owned names (non-empty `comp_name`); the graph walk does not. |
| `CREATED` / `ALLOCATED` / `ACTIVE` | `allocation_status` values on a node's Field: `CREATED` = empty Field made at advertise (legacy `FieldClassAspect%create`, called from `advertise_variable`); `ACTIVE` = legacy `activate` (`FieldClassAspect.F90:235`); `ALLOCATED` = completed and allocated (legacy `allocate_items` allocates, but never writes this status key). A graph-created Field is `CREATED` and an original (alias id 0). |
| Resource index | Per-`ComponentGraph` map of directly advertised items. |
| Forward network | The hierarchy's one `DependencyNetwork`, owned by `HierarchyGraph`: every edge that carries data toward a consumer, whichever component declared it. A pull (`ComponentGraph%update`) follows only this network. No legacy term. |
| Write-back network | A `DependencyNetwork` a `ComponentGraph` creates for the put side of a callback method or the return edge of an inout borrower. Never pulled. No legacy term. |
| Reuse index | `HierarchyGraph` map from a semantic key (`EXTCHAIN:`, `EXPREVAL:`, `CALLBACK_COLLECTION:`, `WILDCARD_COLLECTION:`) to the nodes already built, so a chain or evaluation is shared across components. No legacy term. |
| Extension | A framework-created `StateItemNode` plus `TransformGraphNode` inserted between a mismatched export and import. |
| Variant | `MAPL_StateItem_Flag` tag refining an ESMF kind (`GEOM`, …) in ESMF Info. |
| Frozen | After `freeze()` a `ComponentGraph` accepts no new nodes or edges. |
| Composite state / member | A `STATE`-typed item with named members; members have no ports of their own. |
| Expression placeholder | An EXPRESSION export: no edges, never materialized; each consumer gets its own evaluation (result node + `ExpressionTransform`). Legacy: `ExpressionClassAspect` item. |
| Consumer-adopted kinds | Kinds in the (item type, kind) mirrored-export allowlist (`get_consumer_adopted_kinds`): the evaluation result takes the consumer's value for them. No legacy term. |
| `declared_wins` | Allowlist flag: a `SPECIFIED` value declared on the expression beats the consumer's. False only for TYPE_KIND (precision is always the consumer's). No legacy term. |
| Deferred fallback | A `DEFERRED` characteristic nothing supplied (and not fatal, e.g. QuantityType) becomes `UNCHECKED` at connect, with one summary warning. No legacy term. |
| Tip | The characteristic map after an extension-chain step; the extension node's descriptor. |
| `covered_kinds(tip, goal)` | Kinds one repair step settles (default: its own kind; only kinds in both maps). |
| Incomplete kind | A characteristic whose `is_complete()` is false; reported as `incomplete_characteristic:<KIND>` by `check_materializable`. |
| `harvest` | `StateItemCharacteristic%harvest(handle, defaults)`: fill a characteristic from a realized field (`VerticalStagger`, `QuantityType`). |
| Characteristic status `UNCHECKED` | "Always match, user beware." |

## Terms in the plan not yet in code

`StateItemDescriptor`, `write_info`, `sync_to_info`, `get_extension_order`,
`VariableSpec%make_characteristics`. See
`docs/graph/plans/separation-of-concerns.md` D1-D5.
