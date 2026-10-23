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
| `ClassAspect` (`FieldClassAspect`, `BracketClassAspect`, `ExpressionClassAspect`, `WildcardClassAspect`, …) | `ItemShapeCharacteristic` | Structural class of an item. Graph handles Bracket→Field only through the characteristic; Expression and Wildcard are handled by dedicated `GraphBuilder` hooks. |
| `ClassAspect%get_aspect_order(goal_aspects)` | `GraphStateItem%ordering` / `characteristic_ordering_table` (variant-keyed, no legacy basis) | Plan D4 replaces it with `get_extension_order` owned by the shape. |
| `UnitsAspect` | `PhysicalUnitsCharacteristic` | |
| `TypekindAspect` | `TypeKindCharacteristic` | |
| `GeomAspect` | `GeometryCharacteristic` | Geometry is also a graph node (`GeomProxyField`). |
| `VerticalGridAspect` | `VerticalCoordinateCharacteristic` | Plan adds a separate vertical-stagger characteristic. |
| `UngriddedDimsAspect`, `StandardNameAspect`, `AttributesAspect`, `QuantityTypeAspect`, `ConservationAspect`, `NormalizationAspect` | `UngriddedDimsCharacteristic`, `StandardNameCharacteristic`, `AttributesCharacteristic`, `QuantityTypeCharacteristic`, `ConservationCharacteristic`, `NormalizationCharacteristic` | Same names minus `Aspect`. Mismatch detection works; `build_transform` is a stub (see `ARCHITECTURE.md` §1.1). |
| `StateItemSpec` | `StateItemNode` + `StateItemDescriptor` + `GraphStateItem` | Spec = description + producer/consumer. Graph = node (identity, revision) + descriptor (description) + payload (ESMF handle); producer/consumer are dependency edges. |
| `VariableSpec` | `VariableSpec` (unchanged) | Fortran-level user declaration. Graph reads it in `advertise_one`; plan D5 adds `make_characteristics` beside `make_aspects`. |
| `ExtensionTransform` (`ConvertUnitsTransform`, `CopyTransform`, `RegridTransform`, `VerticalRegridTransform`, `TimeInterpolateTransform`, `NormalizationTransform`, `EvalTransform`) | `Transform` / `TransformGraphNode` (`UnitsConverterTransform`, `TypeKindConverterTransform`, `BracketTimeInterpolationTransform`, `ExpressionTransform`) | Graph transforms compute over named ports. No graph regrid transform yet. |
| `CouplerMetaComponent`, `GenericCoupler` (gridcomp wrapping a transform) | `TransformGraphNode` node in the dependency graph | |
| `StateRegistry` | `ComponentGraph` (per component) over `HierarchyGraph` (hierarchy-wide storage) | Legacy registry also builds the real `ESMF_State`; graph does it in `GraphBuilder_StatePopulation_smod.F90` (modes `graph`/`both`). |
| `VirtualConnectionPt` | `NodeId` (item) and `PortId` (named port) | Legacy keys by declared name; graph keys by opaque, hierarchy-wide id. Declared names still match connections via `VirtualConnectionPt%matches()`. |
| `ActualConnectionPt` | `NodeId` of the resolved node | |
| `ExtensionFamily` (primary + derived specs per virtual pt) | `ExtensionResolution` (`find_mismatched_characteristics`, `find_or_build_extension_chain`) and extension `StateItemNode`s | Chain of extension nodes replaces the family set. |
| `StateRegistry%propagate_unsatisfied_imports` / `propagate_exports` | `graphbuilder_run_propagate_hook` / `graphbuilder_run_export_propagate_hook` | Run in sequence with the legacy calls in `initialize_advertise.F90`. |
| `Connection`, `SimpleConnection`, `MatchConnection`, `ReexportConnection` | Same types (shared) | Graph resolution dispatches on `SimpleConnection` and `MatchConnection`; `ReexportConnection` takes the `class default` path. These are in the integration seam, not duplicated. |
| `ExpressionClassAspect` + `EvalTransform` | `graphbuilder_run_expression_hook` + `ExpressionTransform` | N:1, dedicated hook (not pairwise). |
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
| Resource index | Per-`ComponentGraph` map of directly advertised items. |
| Extension | A framework-created `StateItemNode` plus `TransformGraphNode` inserted between a mismatched export and import. |
| Variant | `MAPL_StateItem_Flag` tag refining an ESMF kind (`GEOM`, …) in ESMF Info. |
| Frozen | After `freeze()` a `ComponentGraph` accepts no new nodes or edges. |
| Composite state / member | A `STATE`-typed item with named members; members have no ports of their own. |
| Characteristic status `UNCHECKED` | "Always match, user beware." |

## Terms in the plan not yet in code

`StateItemDescriptor`, `write_info`, `sync_to_info`, `get_extension_order`,
`VariableSpec%make_characteristics`. See
`docs/graph/plans/separation-of-concerns.md` D1-D5.
