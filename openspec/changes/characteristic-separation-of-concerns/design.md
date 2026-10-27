## Context

Plan: `docs/graph/plans/separation-of-concerns.md` (D1-D6, task B0).
Conventions: `docs/graph/CONVENTIONS.md` (§3 duplicate-not-delegate, §4
D6 target rule, §5a Info access), `docs/graph/GLOSSARY.md`,
`docs/graph/scenario-parity.md` (M1, M2, M3).

Current state, verified in source on 2026-10-22:

- `GraphStateItem` (`graph/GraphStateItem.F90`) holds the ESMF handle,
  the member maps, the `StateItemCharacteristicMap`, and seven
  description components (inventory in §6).
- `populate_characteristics` (`GraphBuilder_Advertising_smod.F90`)
  translates `VariableSpec` field by field, and encodes status by
  omission (for example `MAPL_TYPEKIND_MIRROR` produces no TYPE_KIND
  entry, and empty attributes produce no ATTRIBUTES entry).
- Only `StandardNameCharacteristic` reads `CharacteristicStatus`
  (UNCHECKED means no extension). `SetSharedCharacteristic` sets INVALID.
  No generic status logic exists.
- `ExtensionResolution%find_mismatched_characteristics` compares only
  kinds present on both maps. `StateItemCharacteristicDetection` (used
  only by its own unit test) also reports one-sided kinds.
- Ordering is `GraphStateItem%ordering` / `characteristic_ordering_table`:
  FIELD = [units, typekind, geometry], GEOM = [geometry, units,
  typekind]. Neither has a legacy basis.
- GraphBuilder names kinds in `GraphBuilder.F90` (use list),
  `_Advertising_smod` (every block), `_ConnectionResolution_smod`
  (`get_item_units`, `get_item_typekind`, `merged_item_for_link`), and
  `_Geometry_smod` (component-level GEOMETRY edges).
- The only graph-layer `MAPL_*Set` calls are `restart_mode=` (alias, in
  `populate_one`) and `state_item_variant=` (`CompositeStateMaterialization`,
  `GeomProxyField`). Extension fields are created with
  `FieldCreate(geom, typekind, ungridded_dims, vgrid, vert_staggerloc,
  units, name)`.

Legacy reference points (the behavior being duplicated):

- `ExtensionFamily%find_closest_spec` and `StateItemSpec%make_extension`
  walk `ClassAspect%get_aspect_order(goal)`. At each step,
  `make_extension` asserts `can_connect_to`, builds the transform for the
  **first** aspect that `needs_extension_for`, replaces that aspect with
  the goal's (`set_aspect(dst_aspect)`), and exits. `StateRegistry%extend`
  loops (at most 10 times) until no producer is created. The order list
  is therefore re-queried on each new chain tip, and after a CLASS step
  the tip has a different class.
- `StateItemSpec%update_payload_from_aspects` iterates the AspectMap in
  AspectId order and calls each aspect's `update_payload(field|bundle|state)`.
  It is called from `create()` (before allocation) and from
  `set_geometry()`. `FieldClassAspect%create` makes an EMPTY field;
  `allocate` calls `mapl_FieldEmptyComplete`, which reads geom, typekind,
  num_levels and ungridded_dims **back from Info**. This is the direct
  analog of D3's write method.
- `StateItemAspect%can_connect_to`: a single mirror can always connect,
  and two mirrors fail. `needs_extension_for`: a dst mirror never extends,
  a time-dependent side always extends, otherwise `.not. matches`.
  `GeomAspect`/`VerticalGridAspect%make_transform` return
  `ExtendTransform` (no-op; "export acquires the import's value") when
  src is a mirror.

## Goals / Non-Goals

**Goals**
- Fix the target shape of `StateItemDescriptor`, generic status, the Info
  facade, shape-owned ordering, and VariableSpec translation, precisely
  enough that B1-B5 are mechanical.
- Provide the tables B0 requires: legacy aspect -> characteristic (§1),
  shape -> extension order (§4), characteristic -> `MAPL_*Set` keyword
  (§3), `GraphStateItem` component / `populate_characteristics` block ->
  target characteristic (§6).
- Resolve the plan's open questions (§7), and list everything that needs
  the owner (§8).

**Non-Goals**
- No code. Geometry/vertical regrid Transforms (C1). Materializing
  VECTOR/BRACKET/EXPRESSION payloads in the graph (M3, B4).
- Post-legacy cleanup (deleting aspects/registry, renaming
  `StateItemCharacteristic*`): out of scope, per the plan.

## Decisions

### §1. Legacy aspect -> characteristic

| Legacy aspect (`AspectId`) | Characteristic (kind) | Value/Ref | Exists? | Notes |
|---|---|---|---|---|
| `CLASS_ASPECT_ID` (class identity) | `ItemShape` (`ITEM_SHAPE`) | value | yes, partial | Widened to every `MAPL_STATEITEM_*` shape (see §4.3). Replaces `declared_item_type`. |
| `CLASS_ASPECT_ID` (`ExpressionClassAspect%expression`) | `Expression` (new) | value | no | Expression string. Never mismatches (§2.5). |
| `CLASS_ASPECT_ID` (`FieldClassAspect%long_name`) | `LongName` (new, A8) | value | no | Never mismatches. Needed for Info parity (`SharedIO` reads `long_name`) and by History (mirror, A13). Owner A8. |
| `CLASS_ASPECT_ID` (`fill_value`, `bracket_size`, `vector_basis_kind`) | deferred to M3 | - | no | Only needed once the graph materializes those shapes (B4/M3). Owner A8. |
| `GEOM_ASPECT_ID` (geom) | `Geometry` (`GEOMETRY`) | reference | yes, component-level only | Item-level instance added by B3-Geometry (fixes M1). Also carries the regridder param (not compared). |
| `GEOM_ASPECT_ID` (`horizontal_dims_spec`) | `HorizontalDimsSpec` (new, A14) | value | no | Today carried inside `GeometryCharacteristic` and compared there, as legacy `GeomAspect%matches` does. Moves to its own kind. |
| `VERTICAL_GRID_ASPECT_ID` (grid) | `VerticalGrid` (`VERTICAL_GRID`) | reference | yes | References the item's `VerticalGrid`, which may carry several coordinates. Must also be set from the component vertical grid (M2). |
| `VERTICAL_GRID_ASPECT_ID` (stagger) | `VerticalStagger` (new) | value | no | Replaces `GraphStateItem%vertical_stagger`. |
| `VERTICAL_GRID_ASPECT_ID` (alignment) | `VerticalOrientation` (new, name provisional; Q7) | value | no | Vertical index order. Independent of VerticalStagger. |
| `UNITS_ASPECT_ID` | `PhysicalUnits` (`PHYSICAL_UNITS`) | value | yes | |
| `TYPEKIND_ASPECT_ID` | `TypeKind` (`TYPE_KIND`) | value | yes | |
| `UNGRIDDED_DIMS_ASPECT_ID` | `UngriddedDims` (`UNGRIDDED_DIMS`) | value | yes | Replaces `GraphStateItem%ungridded_dims`. |
| `ATTRIBUTES_ASPECT_ID` | `Attributes` (`ATTRIBUTES`) | value | yes | |
| `QUANTITY_TYPE_ASPECT_ID` | `QuantityType` (`QUANTITY_TYPE`) | value | yes | |
| `CONSERVATION_ASPECT_ID` | `Conservation` (`CONSERVATION`) | value | yes | |
| `NORMALIZATION_ASPECT_ID` | `Normalization` (`NORMALIZATION`) | value | yes | |
| `STANDARD_NAME_ASPECT_ID` | `StandardName` (`STANDARD_NAME`) | value | yes | |
| (none: `VariableSpec%restart_mode`, legacy uses it in `FieldClassAspect%add_to_state`) | `RestartMode` (new) | value | no | Never compared. Default `RESTART_REQUIRED` (A15). Per-alias Info key (§3.2). |
| (none) | `SetShared` is a mutator, not a characteristic | - | - | See §7.3. |

New kinds get the next `StateItemCharacteristicKind` integer values in
the order the B3 tasks land them. The B3 list in the plan gains
`LongName` (A8), `HorizontalDimsSpec` (A14) and `VerticalOrientation` (Q7).

Horizontal and vertical description stay separate characteristics. A
caller that wants them together can build a read-only view from the
descriptor's entries; no combined type is stored or compared. A transform
that needs several of them reads them while building (§4.1).

### §2. Generic status (D2)

#### §2.1 Values

`CharacteristicStatus` keeps `INVALID`, `SPECIFIED`, `MIRRORED`,
`UNCHECKED`, `DEFERRED`. No `FROM_COMP` (legacy `ASPECT_STATUS_FROM_COMP`
collapses into SPECIFIED or DEFERRED, see §5). INVALID found by the
descriptor during resolution or sync is a programming error and is
reported with `_FAIL`. The status setter stays on the base class; only
factories and the descriptor call it.

#### §2.2 Resolution table (implemented once, in the descriptor)

`resolve_pair(export_c, import_c)` (export and import of one kind):

| export \ import | SPECIFIED | MIRRORED | UNCHECKED | DEFERRED |
|---|---|---|---|---|
| SPECIFIED | `export_c%needs_extension_for(import_c)` | copy: `allocate(import_c, source=export_c)`, status SPECIFIED; match | match | wait |
| MIRRORED | error, unless `(export shape, kind)` is allowlisted (§2.4) | error | match | wait |
| UNCHECKED | match | error | match | wait |
| DEFERRED | wait | wait | wait | wait |

- "match" means no transform for this kind.
- "wait" is the pair outcome `PAIR_WAIT`: the pair is neither wired nor
  reported. Its import stays unsettled and `resolve_connections` retries
  it on a later pass. A DEFERRED value can outlive GEOM_B: a component
  with no default geom fills its items in its own init phase, which runs
  after GEOM_B and may run after the first connect pass (§5,
  `harvest_deferred`). A wait that is never retried to completion is
  reported at freeze (§2.3). An error cell still means a conflict.
- The copy in (SPECIFIED, MIRRORED) is generic. It is the only place a
  characteristic is cloned across items, and no subclass carries a mirror
  sentinel.
- Subclass `needs_extension_for` / `build_transform` / write methods are
  called only when both sides are SPECIFIED. The descriptor `_ASSERT`s
  this. The `StandardNameCharacteristic` status check moves into the
  descriptor, and the subclass's own check is deleted (B3-StandardName).

#### §2.3 Freeze check

At `run_graph_freeze_hook`, every characteristic of every StateItemNode
must be SPECIFIED or UNCHECKED, or must be a MIRRORED import that no
connection reached (an unconnected import, which is already reported
elsewhere). A DEFERRED entry fails freeze and names the item and the
kind. A MIRRORED **export** with no consumer is allowed (legacy allows
it: it is never extended). A component that still has a connection
waiting on a DEFERRED characteristic also fails freeze, naming the
component.

#### §2.4 MIRRORED-export allowlist

Legacy semantics (`ExpressionClassAspect`, `GeomAspect/make_transform`,
`VerticalGridAspect/make_transform`, `transforms/ExtendTransform.F90`):
an expression has no geometry of its own. For each consumer, the
framework creates a separate extension whose GEOM/VERTICAL_GRID equal the
consumer's (`ExtendTransform`, a no-op), and then the CLASS step
(`ExpressionClassAspect%make_transform`) builds an `EvalTransform` whose
inputs are the referenced variables, each **extended to that
consumer's goal spec** using ordinary field rules (with the goal's CLASS
replaced by Field and its StandardName replaced by unchecked).

Allowlist entries (shape, kind) and their resolution:

| Export shape | Kind | Resolution |
|---|---|---|
| EXPRESSION | GEOMETRY | Per consumer: the chain tip takes the import's value (no Transform step; the analog of `ExtendTransform`). The export's own descriptor is not modified, because several consumers may want different values. |
| EXPRESSION | VERTICAL_GRID | Same. |
| EXPRESSION | VERTICAL_STAGGER | Same. In legacy, stagger lives in `VerticalGridAspect`, and `ExpressionClassAspect%create` may adopt the stagger of its variables early. |
| EXPRESSION | VERTICAL_ORIENTATION | Same (added with the kind, B3-VerticalOrientation). |
| EXPRESSION | TYPE_KIND | Only when declared `MAPL_TYPEKIND_MIRROR`. Same resolution. |

The allowlist is a static table in the descriptor module, keyed by
(shape, kind). It is data, not GraphBuilder logic. Every other MIRRORED
export is an error (owner A1). An entry is added only when a compelling
use case is shown, and each new entry is its own reviewed change. The per-consumer
evaluation (inputs extended to each consumer's goal) is a change to the
Phase 7b hook design. B0 records it as the target and as owner Q2. Until
B3-Expression lands it, an EXPRESSION export with MIRRORED geometry
connected to an import with a different geometry stays a known gap
(`expression_defer_geom`, masked by M3).

#### §2.5 Non-comparing characteristics

`Expression`, `RestartMode`, and `LongName` have
`needs_extension_for = .false.` always, and they are not listed in any
shape order. They live in the map only so that the descriptor is the
single source of description (D1) and so that `sync_to_info` covers them.

#### §2.6 One-sided absence

"Absent" means "not applicable to this shape" (D2). Detection keeps
`ExtensionResolution`'s rule: a kind present on only one side is **not**
a mismatch. This matters only across shapes (for example a BRACKET export
has no STANDARD_NAME entry, see §4.3), and the shape step handles those.
`StateItemCharacteristicDetection` (stricter, and used only by its own
test) is aligned with this rule or deleted in B2. Its one-sided rule
would make every cross-shape pair mismatch.

### §3. Info facade (D3)

#### §3.1 Write method and dispatch

Each characteristic implements
`write_info(this, handle, phase, rc)`, where `handle` is a small
graph-layer wrapper (`GraphStateItem` itself) and `phase` is
`STRUCTURAL` (before `FieldEmptyComplete`) or `DESCRIPTIVE` (any time).
The characteristic inspects the handle's allocated kind and calls
`MAPL_FieldSet` / `MAPL_FieldBundleSet` / `MAPL_StateSet` once, with only
its own keywords. Rejected alternative: a keyword bag assembled by the
descriptor followed by one call. The setters have many optional
arguments of distinct types, so a bag would duplicate every setter
signature in the graph layer.

`StateItemDescriptor%sync_to_info(handle, phase, rc)` loops over the map
and skips characteristics that are not SPECIFIED or UNCHECKED.
UNCHECKED handling is in §3.3.

#### §3.2 Avoiding `FieldDelta`

`MAPL_FieldSet` enters `FieldDelta` only when the field is COMPLETE and
one of `geom`, `vgrid`, `typekind`, `num_levels`, `units` is present
(`FieldSet.F90`). The design never satisfies that condition:

1. **Materialization follows the legacy pattern.** The field is created
   empty, `sync_to_info(STRUCTURAL)` writes every keyword (geom goes
   through `FieldEmptySet`, and the rest into Info), and then
   `MAPL_FieldEmptyComplete` allocates from Info. This replaces
   `materialize_field_extension`'s scalar arguments (B4), and it is what
   lets materialization stay characteristic-agnostic.
2. **After completion, only DESCRIPTIVE-phase keywords are ever
   written.** A characteristic marked structural (`is_structural() =
   .true.`: Geometry, HorizontalDimsSpec, VerticalGrid, VerticalStagger,
   VerticalOrientation, TypeKind, UngriddedDims) is skipped in the DESCRIPTIVE phase. PhysicalUnits is
   descriptive, but `units=` is one of FieldSet's FieldDelta triggers.
   It is therefore written only in the STRUCTURAL phase. A post-completion
   units write is not needed (units are never DEFERRED, and imports do not
   write field-wide keys, §3.4).
3. Field-wide keys are written by the node that **owns** the ESMF field
   (the export or extension node). An import that aliases a provider's
   field writes only per-alias keys (`restart_mode`, `long_name`) onto its
   own `MAPL_NamedAlias`. This preserves the current `populate_one`
   behavior.

No change to `FieldSet`/`FieldDelta` is required. Optional hardening,
not required: drop `present(units)` from FieldSet's FieldDelta trigger.
`update_units` only writes Info, and `FieldInfoSetInternal` already
receives `units=`.

#### §3.3 UNCHECKED Info representation (resolves open question 1)

Legacy consumers surveyed:

- `UnitsAspect%update_payload` writes `'<unknown>'` for UNCHECKED, and
  `update_from_payload` maps both `'<unknown>'` and absent to UNCHECKED.
- ExtData writes `units='<unknown>'` (`ExtDataGridComp_private.F90`,
  `PrimaryExport.F90`).
- `SharedIO` writes a NetCDF `units` attribute whenever the value is
  allocated, so `'<unknown>'` reaches files.
- `StandardNameAspect` writes `'<unknown>'` (its `UNKNOWN`), and
  `FieldInfoGetInternal` already defaults an absent standard_name to
  `'<unknown>'`.
- History copies standard_name/long_name.
- No reader of `'/$MIRROR$'` exists outside `UnitsAspect`.

Decision: UNCHECKED writes the legacy marker `'<unknown>'` for `units`
and `standard_name`, and writes **no entry** for every other key. Legacy
writes nothing for UNCHECKED on the other axes, and those axes either
have no UNCHECKED state or `_RETURN_IF(mirror)`. MIRRORED is never
written (it cannot reach sync, §2). Legacy's `'/$MIRROR$'` units marker
is not reproduced (owner Q11).

#### §3.4 Characteristic -> setter keyword

`S` = written in the STRUCTURAL phase only (creation input); `D` =
descriptive, so the STRUCTURAL phase writes it and the DESCRIPTIVE phase
may rewrite it. "-" = not applicable to that handle kind (no map entry
for that shape).

| Characteristic | `MAPL_FieldSet` | `MAPL_FieldBundleSet` | `MAPL_StateSet` | Phase | Notes |
|---|---|---|---|---|---|
| ItemShape | `state_item_variant=` | `state_item_variant=`, `fieldBundleType=` (BRACKET/VECTOR/VECTORBRACKET/SERVICE) | `state_item_variant=` (`state_set_own`) | D | Replaces the direct calls in `CompositeStateMaterialization` and `GeomProxyField`. |
| Geometry | `geom=`, `regridder_param_info=` | `geom=`, `regridder_param_info=` | - | S | Bundle `geom=` cascades to members via FieldSet. Write it only before the members complete. |
| HorizontalDimsSpec | `horizontal_dims_spec=` | none | - | S | Moved from Geometry. No bundle keyword (legacy does not write it either). |
| VerticalGrid | `vgrid=` | `vgrid=` | - | S | Info stores `vgrid_id`. `num_levels` is derived on read. |
| VerticalStagger | `vert_staggerloc=` | `vert_staggerloc=` | - | S | Bundle path calls FieldSet on members. |
| VerticalOrientation | `vert_alignment=` | `vert_alignment=` | - | S | Bundle path calls FieldSet on members. |
| TypeKind | `typekind=` | `typekind=` | - | S | |
| UngriddedDims | `ungridded_dims=` | `ungridded_dims=` | - | S | |
| PhysicalUnits | `units=` | `units=` | - | S (see §3.2.2) | UNCHECKED writes `'<unknown>'`. |
| StandardName | `standard_name=` | `standard_name=` | - | D | Field-wide (unaliased). UNCHECKED writes `'<unknown>'`. |
| LongName | `long_name=` | `long_name=` | - | D | Per-alias: written on the item's own alias. |
| Attributes | none | none | - | - | Writes nothing (legacy `AttributesAspect%update_payload` is a no-op). `FieldSet`'s `attributes` dummy is unused (`_UNUSED_DUMMY`). No setter argument is added. |
| QuantityType | `quantity_type_metadata=` | `quantity_type_metadata=` | - | D | Writes only its own key. Legacy also wrote a derived `normalization_metadata`, which `NormalizationAspect` then overwrote (AspectMap order 9 < 11). Owner Q9. |
| Conservation | `conservation_metadata=` | `conservation_metadata=` | - | D | |
| Normalization | `normalization_metadata=` | `normalization_metadata=` | - | D | |
| RestartMode | `restart_mode=` | none | - | D | Per-alias, written on the item's own `MAPL_NamedAlias` (as `populate_one` does today). The bundle setter has no `restart_mode`: add it only when a bundle needs one (none does today). |
| Expression | none | none | - | - | No Info key in legacy. |

`MAPL_StateSet`: only `state_set_own(state_item_variant=)` is used. The
item-level `state_set` (wrong intents, three keywords) is not used by
this design. It is fixed only if a later task needs it (plan D3).

#### §3.5 When `sync_to_info` runs

- STRUCTURAL: once, at graph materialization of a field the graph owns
  (extension node now; others once M3 lands), between empty-create and
  complete.
- DESCRIPTIVE: when a DEFERRED characteristic becomes SPECIFIED at GEOM_B
  (only if the payload already exists; structural kinds are skipped, so
  this is in effect a no-op until the descriptive kinds can be DEFERRED),
  and on alias creation in `populate_one` (per-alias keys only).
- Legacy-created payloads (advertised items still materialized by
  `registry%add_to_states`) are not written by the graph. Both writers
  would produce the same keys, but the graph writing them is redundant
  until the legacy layer is retired.

Info stays write-only for graph code (no `MAPL_*Get` on description
keys). The fallback "Info only for user-visible characteristics" is not
needed.

### §4. Extension ordering (D4)

#### §4.1 Mechanism

`ItemShapeCharacteristic%get_extension_order(goal_descriptor, rc)`
returns the explicit list for the **current chain tip's** shape. The
chain builder works like legacy `extend`/`make_extension`:

```
tip := export descriptor
loop (bounded, as legacy MAX_ITERATIONS = 10):
  mismatched := kinds k present on both tip and goal with resolve_pair(tip[k], goal[k]) = "extend"
  exit if empty
  order := tip[ITEM_SHAPE]%get_extension_order(goal), then any mismatched kinds not listed, in kind-name order
  k := first kind in order that is in mismatched
  (step, covered) := tip[k]%build_transform(goal[k], tip, goal)
  tip := tip with tip[c] replaced by goal[c] for every c in covered   (generic; replaces merged_item_for_link)
```

Mismatch detection stays per kind and independent. Only building may
look across kinds: `build_transform` gets read-only access to the whole
tip and goal descriptors and returns the kinds its step resolves
(`covered`, default `[k]`). This lets one vertical regrid produce the
goal's VerticalGrid, VerticalStagger and VerticalOrientation together;
those kinds are then no longer mismatched on the new tip. A
VerticalStagger-only change (same grid) may read the tip's VerticalGrid
while building, never while comparing (Q7, A16). A VerticalOrientation-
only mismatch is its own zero-cost step (a reversed view) and needs no
grid. The covered-list signature and replacement land with B4 (4.1); the
vertical regrid that uses them is C1.

This refines the plan's "owned by the export's shape": after an
ITEM_SHAPE step (for example BRACKET -> FIELD), the remaining kinds are
ordered by the new shape, which is exactly what legacy does. The "kinds
not listed" fallback can still fire for shapes whose list is shorter than
their applicable set (see §4.3), and a golden test records it.

#### §4.2 Shape -> extension order (from legacy `get_aspect_order`)

Legacy -> graph kind mapping: CLASS->ITEM_SHAPE, GEOM->HORIZONTAL_DIMS_SPEC
then GEOMETRY, VERTICAL_GRID->VERTICAL_GRID then VERTICAL_STAGGER then
VERTICAL_ORIENTATION, UNITS->PHYSICAL_UNITS, TYPEKIND->TYPE_KIND, others
same name. In the orders below, "GEOM" stands for HORIZONTAL_DIMS_SPEC,
GEOMETRY and "VGRID" for VERTICAL_GRID, VERTICAL_STAGGER,
VERTICAL_ORIENTATION.

| Shape | Legacy source | Graph order |
|---|---|---|
| FIELD | `FieldClassAspect` | ITEM_SHAPE, ATTRIBUTES, UNGRIDDED_DIMS, QUANTITY_TYPE, CONSERVATION, GEOM, VGRID, NORMALIZATION, PHYSICAL_UNITS, STANDARD_NAME, TYPE_KIND |
| FIELDBUNDLE | `FieldBundleClassAspect` | FIELD order without STANDARD_NAME |
| VECTOR | `VectorClassAspect` (= Field order minus STANDARD_NAME) | FIELD order without STANDARD_NAME |
| BRACKET | `BracketClassAspect`, goal geom not time-dependent | ATTRIBUTES, UNGRIDDED_DIMS, QUANTITY_TYPE, GEOM, ITEM_SHAPE |
| BRACKET | same, goal geom time-dependent | ATTRIBUTES, UNGRIDDED_DIMS, QUANTITY_TYPE, ITEM_SHAPE, GEOM. Not implemented: owner Q4. |
| VECTORBRACKET | `VectorBracketClassAspect` | same as BRACKET |
| EXPRESSION | `ExpressionClassAspect` | GEOMETRY, VGRID, TYPE_KIND, ITEM_SHAPE |
| STATE | `StateClassAspect` lists CLASS, ATTRIBUTES, UNGRIDDED_DIMS, GEOM, VERTICAL_GRID, UNITS, TYPEKIND | ITEM_SHAPE only (plan D3: a State has no such characteristics). Owner Q5. |
| SERVICE | `ServiceClassAspect` | ITEM_SHAPE, GEOMETRY |
| WILDCARD | `WildcardClassAspect` (empty) | empty |
| GEOM (graph-only variant) | none | GEOMETRY |
| VERTICALGRID (graph-only variant) | none | VERTICAL_GRID |

Consequences:
- FIELD changes from [units, typekind, geometry] to the legacy order.
  Geometry precedes units, so once C1 lands a regrid happens before a
  units conversion, as in legacy.
- For BRACKET, units and typekind are not listed. The ITEM_SHAPE step
  (time interpolation) runs first, and the FIELD order then handles units
  and typekind on the new tip.
- `GraphStateItem%ordering` and `characteristic_ordering_table` are
  deleted (B4). `Test_GraphStateItem.pf`'s ordering tests are replaced
  by a golden test per shape for `get_extension_order`.

#### §4.3 Applicability (which kinds a shape's map holds)

The factory omits kinds that are not applicable, using legacy
`get_mandatory_aspect_ids`, standard-name exemptions, and plan D3:

| Shape | Applicable kinds |
|---|---|
| FIELD | all comparing kinds + RestartMode, LongName |
| BRACKET, VECTORBRACKET | all comparing kinds except STANDARD_NAME (legacy order omits it, and the field-side tip after ITEM_SHAPE adopts the import's) |
| VECTOR, FIELDBUNDLE | all comparing kinds except STANDARD_NAME |
| EXPRESSION | ITEM_SHAPE, GEOMETRY, VERTICAL_GRID, VERTICAL_STAGGER, VERTICAL_ORIENTATION, TYPE_KIND, Expression, and PHYSICAL_UNITS as UNCHECKED (owner Q3) |
| (any shape above) | VERTICAL_GRID, VERTICAL_STAGGER and VERTICAL_ORIENTATION apply only to an item with a vertical dimension: no VERTICAL_GRID or VERTICAL_ORIENTATION for `vertical_stagger == NONE`, and SERVICE, STATE and WILDCARD never have them (the VERTICALGRID composite is the one STATE with a VERTICAL_GRID) |
| (any shape with GEOMETRY except EXPRESSION, SERVICE) | HORIZONTAL_DIMS_SPEC. With NONE (A14) the item's GEOMETRY entry is never compared (see §5) |
| STATE | ITEM_SHAPE only |
| SERVICE | ITEM_SHAPE, GEOMETRY |
| WILDCARD | ITEM_SHAPE only |

The ItemShape `needs_extension_for` / `build_transform` matrix (legacy
`matches` / `supports_conversion_specific`):

| export \ import | FIELD | FIELDBUNDLE | STATE | WILDCARD | others |
|---|---|---|---|---|---|
| FIELD | match | error | error | match | error |
| BRACKET | `BracketTimeInterpolationTransform` | error | error | error | error |
| VECTORBRACKET | not handled (legacy only) | | | | |
| EXPRESSION | expression evaluation (§2.4, Q2) | error | error | error | error |
| FIELDBUNDLE | error | match | error | match | error |
| STATE | error | error | match | match | error |
| VECTOR | error | error | error | error | VECTOR: match |
| SERVICE | error | error | error | error | SERVICE: match |

WILDCARD imports keep their dedicated resolution
(`resolve_wildcard_destination`). The match cells exist so that the
matrix is total.

### §5. Translation from VariableSpec (D5)

`VariableSpec%make_characteristics(descriptor, rc)` lives in a
new submodule of `mapl_VariableSpec_mod` (`specs/`). It is a loop over a
fixed array of factory procedures, one per characteristic. The graph
already depends on `VariableSpec` (`CompositeStateMaterialization`).
`VariableSpec` does not yet depend on the graph; B5 adds a `specs/` ->
`graph/` dependency (descriptor and characteristic modules), which is
allowed: only graph -> `specs/*Aspect` is forbidden. The characteristic
modules do not use `VariableSpec`, so there is no cycle, but the
type-bound interface in `mapl_VariableSpec_mod` itself names
`StateItemDescriptor`; B5 must confirm the descriptor module does not
use `VariableSpec`. Each factory applies §4.3 applicability first.

Defaults (owner A12): a value the item does not declare comes from the
component, which supplies it later (GEOM_A/GEOM_B run after ADVERTISE),
so the factory sets DEFERRED. There is no "component value known at
ADVERTISE" path. The one exception is TypeKind, whose convention is a
concrete default: `VariableSpec%typekind` defaults to R4, so it is
SPECIFIED. VariableSpec fills in other defaults where it already does
(for example `horizontal_dims_spec`), and those are SPECIFIED. MIRRORED
comes only from an explicit mirror sentinel, never from absence.

Legacy sometimes treats "absent" as "mirror" (for example
`make_GeomAspect` returns MIRRORED when neither item nor component geom
is present, and `VerticalGridAspect%is_mirror` is true when the stagger
is unallocated). Each B3 task MUST list the absent-means-mirror cases it
finds in its characteristic's legacy aspect, and either map them to
DEFERRED (component supplies it) or report them to the owner. It must not
silently reproduce them as MIRRORED.

**Component-wide kinds (owner).** By long MAPL convention, Geometry and
VerticalGrid are component-wide: an absent value in `VariableSpec`
means DEFERRED, filled later from the component default. Typical
components rely on this and need no change. Components introduced or
reworked in MAPL3 (Statistics, History collections without `geom:`, and
similar) instead adopt their connected partner's value. They must say so
explicitly (`mirror_geom` / YAML `geometry_mirror: true` for Geometry,
`mirror_vgrid` / `vertical_grid_mirror: true` for VerticalGrid);
MIRRORED is never inferred from absence. EXPRESSION is the one implicit
MIRRORED case (§2.4 allowlist).

**String mirror sentinel (A13).** StandardName and LongName (and later
PhysicalUnits, if wanted) share one named string parameter exported by
`VariableSpec`. Declaring it gives MIRRORED, imports only (an export is an
error, A1). It replaces no existing keyword; legacy has none for these.

| Factory | VariableSpec input | Status rule |
|---|---|---|
| ItemShape | `itemType`, `state_item_variant` | SPECIFIED |
| Expression | `expression` (EXPRESSION only) | SPECIFIED |
| PhysicalUnits | `units` | allocated and not `'<unknown>'`: SPECIFIED; else UNCHECKED (legacy `new_UnitsAspect`, `set_units`) |
| StandardName | `standard_name` | string mirror sentinel: MIRRORED, imports only (A13); else allocated: SPECIFIED; else UNCHECKED (legacy) |
| TypeKind | `typekind` (defaults to R4) | `MAPL_TYPEKIND_MIRROR`: MIRRORED; else SPECIFIED. Never DEFERRED (A12). |
| Geometry | `geom`, `geom_id`, `regrid_param` | item geom/geom_id: SPECIFIED; `mirror_geom` or an EXPRESSION with no geom: MIRRORED; else DEFERRED (component supplies it; legacy FROM_COMP and absent-means-MIRRORED both map here, A12). Freeze fails if the component never supplies one. With HorizontalDimsSpec NONE the entry is never compared (A14): either it is omitted, or, if field creation needs the item's geom, it gets a "filled but not compared" form; B3-HorizontalDimsSpec finds which and reports before adding the second. |
| HorizontalDimsSpec | `horizontal_dims_spec` (YAML `dims: z` -> NONE) | defaults to GEOM: SPECIFIED. Explicit NONE: SPECIFIED. MIRRORED via an explicit mirror value, imports only (A14). GEOM vs NONE in either direction is a mismatch with no transform, i.e. an error (A14). |
| VerticalGrid | `vertical_grid`, `mirror_vgrid`, `vertical_stagger`; `state_item_variant == VERTICALGRID` (member names) | `vertical_stagger == NONE`: no entry (2D item). Item grid: SPECIFIED. `mirror_vgrid` or an EXPRESSION with no grid: MIRRORED. Else DEFERRED (component grid, M2). VERTICALGRID composite: SPECIFIED with dimensions only. A DEFERRED entry whose component has no vertical grid is dropped at GEOM_B (owner, B3-VerticalCoordinate): such an item is 2D as far as the graph can tell, and legacy distinguishes it only by the same absence. |
| VerticalStagger | `vertical_stagger`, `mirror_vgrid` | No built-in default (A17). Real stagger (CENTER/EDGE/NONE): SPECIFIED. `VERTICAL_STAGGER_MIRROR`: MIRRORED, imports only; on an export it is an error (A1, Q6). Unallocated with `mirror_vgrid`: MIRRORED (Q-mirror). Other unallocated (or `VERTICAL_STAGGER_INVALID`, which `parse_var_specs` passes when YAML gives no `vertical_dim_spec`): DEFERRED, not an error at advertise (a component such as Statistics may fill it only after other connections resolve). At GEOM_B a DEFERRED stagger becomes SPECIFIED NONE if the component has no vertical grid (A17); otherwise it stays DEFERRED until harvested from the component's realized field or bundle, and freeze fails if it never is. An explicit NONE stays SPECIFIED NONE even though VerticalGrid has no entry, so a 2D export still mismatches a 3D import. EXPRESSION `VERTICAL_STAGGER_INVALID`: MIRRORED. |
| VerticalOrientation | `vertical_alignment` (`upward`/`downward`/`with_grid`) | Vertical index order relative to physical up/down (Q7). Unspecified or `with_grid`: the item's grid direction, resolved to UP/DOWN once the grid is known (factory for an item grid, `resolve_deferred` for the component grid), DEFERRED until then. Explicit mirror value: MIRRORED, imports only; takes the export's resolved order. A MIRROR stagger does not imply a mirrored orientation. No entry when VerticalGrid has none. |
| UngriddedDims | `ungridded_dims` | `is_mirror()`: MIRRORED; else SPECIFIED |
| Attributes | `attributes` | SPECIFIED, **always inserted, even if empty**. This fixes a parity bug: today an empty export set is omitted, so an import requiring attributes is never flagged, whereas legacy `includes([], required)` mismatches. |
| QuantityType | `quantity_type` | SPECIFIED |
| Conservation | `conservation` | `is_mirror`: MIRRORED; else SPECIFIED |
| Normalization | `normalization` | `is_mirror`: MIRRORED; else SPECIFIED |
| RestartMode | `restart_mode` | allocated: SPECIFIED; else SPECIFIED `RESTART_REQUIRED` (A15). Never compared (§2.5). |
| LongName (A8) | `long_name` (field dictionary fills it first) | string mirror sentinel: MIRRORED, imports only (A13); else allocated: SPECIFIED; else the StandardName value if declared: SPECIFIED; else UNCHECKED (writes nothing) |

DEFERRED -> SPECIFIED happens through
`StateItemDescriptor%resolve_deferred(defaults, rc)`, where `defaults` is
a graph-layer value holding the component's geometry/geom id and
vertical grid. Each characteristic takes what it needs from it. After
that, `StateItemDescriptor%drop_unsupplied(defaults)` removes each
characteristic that is still DEFERRED and says (through
`drop_if_unsupplied`) that it does not apply without a value: today only
VerticalGrid, when the component supplied no vertical grid. Other
kinds, Geometry included, stay DEFERRED until a component fills them. For
geometry, `run_geometry_hook` calls `graphbuilder_resolve_deferred` for
each item in the component without naming a kind (D6), and
`graphbuilder_harvest_deferred` fills items from a realized field or
bundle at connect time. `graphbuilder_resolve_geometry` handles only the
component-level GEOMETRY edges. The component-level GEOMETRY edges in
`GraphBuilder_Geometry_smod.F90` move behind a descriptor/graph helper
in B3-Geometry.

#### §5.1 Defaults when a value is not declared

One row per characteristic. "Next source" lists, in priority order,
where the value comes from when the item does not declare it: the field
dictionary (applied by `VariableSpec` before either path sees it), the
component (resolved at GEOM_B, or harvested from the component's
realized field or bundle), or a built-in constant. "Nothing anywhere"
is the result when every source is empty. "Mirror" is the explicit way
to adopt the connected partner's value. The rules apply to imports and
exports alike, except that MIRROR on an export is an error unless §2.4
allows it. "Legacy" is what the aspect path (`VariableSpec%make_*Aspect`,
`specs/*Aspect.F90`) does when nothing is declared. The §5 table above
is authoritative for status rules; this table summarizes it.

| Characteristic | Declared by | Next source | Nothing anywhere | Mirror | Legacy | State |
|---|---|---|---|---|---|---|
| ItemShape | `itemType` | built-in FIELD | - | - | same | design |
| TypeKind | `typekind` | built-in R4 | - | `MAPL_TYPEKIND_MIRROR` | same; a MIRROR side matches anything | landed |
| PhysicalUnits | `units` | field dictionary | UNCHECKED | none (may adopt the string sentinel later) | UNCHECKED; writes `/$MIRROR$` for mirrored units (no reader) | landed |
| StandardName | `standard_name` | - | UNCHECKED | string sentinel, imports only (A13) | UNCHECKED | landed; mirror: design |
| LongName | `long_name` | field dictionary, then StandardName | UNCHECKED, writes nothing | string sentinel, imports only (A13) | carried on `FieldClassAspect`, not compared | design |
| UngriddedDims | `ungridded_dims` | built-in: none | - | `is_mirror()` | same | landed |
| Geometry | `geom`, `geom_id` | component geom, then harvest | freeze fails | `mirror_geom` / `geometry_mirror: true` | item geom, component geom, item id, component id (FROM_COMP); else MIRRORED | landed |
| HorizontalDimsSpec | `horizontal_dims_spec` / `dims: z` | built-in GEOM | - | explicit mirror value, imports only | compared inside `GeomAspect`, together with the geom | design |
| VerticalGrid | `vertical_grid` | component grid | no entry (2D item) | `mirror_vgrid` / `vertical_grid_mirror: true` | item grid, component grid; else MIRRORED (FROM_COMP if a component geom id exists) | landed |
| VerticalStagger | `vertical_stagger` / `vertical_dim_spec` | none: harvest from the component's field | NONE if the component has no vertical grid; else freeze fails | `MIRROR` (imports only); `mirror_vgrid` with no stagger | unallocated stagger is treated as mirror; `MIRROR` matches any stagger | design |
| VerticalOrientation | `vertical_alignment` | the item's grid direction (`with_grid`) | no entry (2D item) | explicit mirror value, imports only | `with_grid`; compared after resolving against the grid's direction; the import copies the export's on connect | design |
| Attributes | `attributes` | built-in: empty | - | - | empty; import's set must be included in export's | landed |
| QuantityType | `quantity_type` | built-in UNKNOWN | - | - | ignores the declared value: always UNKNOWN (Q9) | design |
| Conservation | `conservation` | built-in CONSERVE_NONE, not conservable | - | `is_mirror` | ignores the declared value: CONSERVE_NONE | design |
| Normalization | `normalization` | built-in NORMALIZE_NONE, scale 1 | - | `is_mirror` | ignores the declared value: NORMALIZE_NONE, scale 1 | design |
| RestartMode (new) | `restart_mode` | built-in `RESTART_REQUIRED` (A15) | - | - | carried on `FieldClassAspect`, not compared | design |
| Expression (new) | `expression` | - | - | - | n/a (EXPRESSION shape only) | design |

"State": landed = implemented by its B3 task; design = rule in §5, not
yet implemented; open = owner decision pending.

Later cleanup, not in the current task set (owner): one "Declared by" per
characteristic (Geometry `geom`/`geom_id`, VerticalStagger
`vertical_stagger`/`vertical_dim_spec`, HorizontalDimsSpec
`horizontal_dims_spec`/`dims:`); one mirror mechanism per characteristic,
a named sentinel value instead of the `mirror_geom`/`mirror_vgrid` flags;
renaming ItemShape and VerticalOrientation. Future: HorizontalDimsSpec
NONE -> GEOM by replication across the horizontal points (A14).

### §6. Inventory: current state -> target

#### §6.1 `GraphStateItem` components

| Component | Readers today | Target |
|---|---|---|
| `esmf_field`, `esmf_field_bundle`, `esmf_state` | everywhere | stays (handle) |
| `field_bundle_members_map`, `state_members_map` | composites, wildcard | stays (handle) |
| `characteristics` | ExtensionResolution, GraphBuilder | `StateItemDescriptor` (B1) |
| `ungridded_dims` | `_ConnectionResolution_smod` (materialization) | `UngriddedDims` characteristic (already in the map; duplicate removed in B3-UngriddedDims) |
| `vertical_stagger` | `_ConnectionResolution_smod`, `_PortsAndProxies_smod` | `VerticalStagger` (new, B3-VerticalStagger) |
| `declared_geom` | `_PortsAndProxies_smod%resolve_export_geom` | `Geometry` item-level (B3-Geometry). DEFERRED replaces the "read component geom live" fallback. |
| `declared_vertical_grid` | `_PortsAndProxies_smod` | `VerticalGrid` (landed as VerticalCoordinate in B3-VerticalCoordinate, M2; renamed by B3-RenameVerticalGrid); the characteristic holds the `VerticalGrid` object |
| `declared_item_type` | `_ConnectionResolution_smod` (WILDCARD / FIELD / BRACKET gates) | `ItemShape` widened to all shapes (B3-ItemShape). The gates become `descriptor%shape()` queries on the ItemShape value, through a generic accessor that returns the `MAPL_StateItem_Flag` (no concrete type named in GraphBuilder). |
| `declared_restart_mode` | `_StatePopulation_smod%populate_one` | `RestartMode` (new, B3-RestartMode); populate_one calls `sync_to_info(alias, DESCRIPTIVE)` |
| `declared_expression` | `_ConnectionResolution_smod` (expression hook) | `Expression` (new, B3-Expression) |

#### §6.2 `populate_characteristics` blocks

| Block (order in file) | Today | Target characteristic / factory | Status change |
|---|---|---|---|
| `allocated(units)` -> PHYSICAL_UNITS | absent if unallocated | PhysicalUnits | unallocated -> UNCHECKED |
| `typekind /= MAPL_TYPEKIND_MIRROR` -> TYPE_KIND | absent if mirror | TypeKind | mirror -> MIRRORED (B2 converts this first) |
| `allocated(vertical_grid)` -> VERTICAL_GRID(grid_id, dims) | item-level only | VerticalGrid | else DEFERRED to component grid (M2) |
| `state_item_variant == VERTICALGRID` -> VERTICAL_GRID(dims) | | VerticalGrid (same factory) | none |
| `allocated(standard_name)` -> STANDARD_NAME | absent if unallocated | StandardName | unallocated -> UNCHECKED; omitted for exempt shapes (§4.3) |
| `attributes%size() > 0` -> ATTRIBUTES | absent if empty | Attributes | always present (parity fix) |
| UNGRIDDED_DIMS (unconditional) | | UngriddedDims | mirror -> MIRRORED |
| QUANTITY_TYPE, CONSERVATION, NORMALIZATION (unconditional) | | each its own factory | mirror metadata -> MIRRORED |
| `set_ungridded_dims` | duplicate | removed (UngriddedDims) | |
| `set_vertical_stagger` | | VerticalStagger | new |
| `set_declared_geom` | | Geometry (item-level) | new kind on items (M1) |
| `set_declared_vertical_grid` | | VerticalGrid | |
| `set_declared_item_type` | | ItemShape | |
| ITEM_SHAPE for FIELD/BRACKET/VECTOR/VECTORBRACKET only | | ItemShape, all shapes | EXPRESSION->FIELD becomes a real matrix cell (§4.3) |
| `set_declared_restart_mode` | | RestartMode | new |
| `set_declared_expression` (EXPRESSION) | | Expression | new |

`populate_characteristics` is then a single call
`var_spec%make_characteristics(node%descriptor())` (B5).

#### §6.3 Other GraphBuilder kind references (removed in B4/B3)

| Location | Use | Replacement |
|---|---|---|
| `GraphBuilder.F90` use list | kind constants for submodules | removed |
| `_ConnectionResolution_smod%get_item_units`, `get_item_typekind` | scalar inputs to `materialize_field_extension` | materialize from tip descriptor (§3.2.1) |
| `_ConnectionResolution_smod%merged_item_for_link` | per-kind merge chain | generic tip replacement (§4.1) |
| `_Geometry_smod` (4 `GeometryCharacteristic` inserts) | component-level geometry edges | helper owned by the geometry characteristic/graph module (B3-Geometry) |

### §7. Open questions from the plan, resolved

1. **UNCHECKED Info representation**: `'<unknown>'` for units and
   standard_name, no entry otherwise (§3.3).
2. **Expression semantics**: documented in §2.4 / §4.2 / §4.3 from
   `ExpressionClassAspect`. The allowlist entries and the expression
   order are fixed. The per-consumer evaluation is the target, and the
   owner confirms it in Q2.
3. **`SetSharedCharacteristic` survives** (owner A10: runtime geometry
   change is a goal). Use cases, in priority order:
   - custom samplers under History (for example a satellite instrument
     trajectory), whose geometry changes over time. These are mostly
     **imports**.
   - ExtData exports whose data set has a different geometry in
     different epochs.
   - a nested grid that moves in time (for example a hurricane
     component). Low priority.

   It has no production caller yet. B3-SetShared retargets it to take
   `(graph, node_id, kind, characteristic)` and mutate the descriptor
   instead of installing a whole `GraphStateItem` payload. It also
   calls `sync_to_info` on affected payloads, which a structural change
   needs; that is a re-materialization, not an Info write, so it does not
   use the FieldDelta path (§3.2). Because the sampler case changes an
   **import's** geometry, the eager structural reset must handle an
   import-side change (the regrid between provider and changed import
   must be rebuilt). Today REQ-CHAR-016 only walks successors of the
   changed node. B3-SetShared checks this and reports, rather than
   implementing import-side resets. This is not a B1/B2 blocker.
4. **Post-legacy cleanup**: out of scope, unchanged.

### §8. Deviations from the plan's D1-D6 text

| Plan text | This design | Why |
|---|---|---|
| D2: `FROM_COMP` collapses into SPECIFIED | Component-supplied values start DEFERRED and become SPECIFIED at GEOM_A/GEOM_B; TypeKind's R4 default is SPECIFIED | Owner A12. Component values are not known at ADVERTISE (GEOM_A/B run after it). |
| D2 table has no DEFERRED column/row | DEFERRED on either side at connect time is `PAIR_WAIT`: the pair is retried on a later pass, and freeze fails if it never settles (§2.2, §2.3) | Owner-approved. A component with no default geom (History collection without `geom:`, Statistics) supplies geometry in its own init phase, after GEOM_B, so DEFERRED can legitimately outlive GEOM_B. |
| Absent geometry means DEFERRED for every item | An import that should adopt the connected export's geometry says so with `mirror_geom` (YAML `geometry_mirror: true`) and is MIRRORED; absence is still DEFERRED | Owner A12: no implicit absent-means-mirror. Statistics imports and History collection imports without `geom:` set it. |
| DEFERRED -> SPECIFIED only at GEOM_B | Also by `harvest_deferred`, which reads the geom off the component's field or bundle in the state once it has one, without binding the item's `GraphStateItem` handle (imports are skipped) | Owner-approved. Keeps DEFERRED resolution in the graph, with no per-component hook. |
| Plan scope excludes gridcomps | `StatisticsGridComp` and `TimeVariance` advertise only the chosen variance algorithm's internal fields and share `get_variance_algorithm` | Owner-approved scope change: unchosen algorithms' internal fields would otherwise stay DEFERRED forever and fail freeze. |
| D3: reference characteristics write a node/registry id | Geometry writes `geom=` (the ESMF object); VerticalGrid writes `vgrid=` (stored as `vgrid_id`) | No setter takes a geom id; legacy and every Info reader use the geom object. Adding a geom-id keyword has no consumer. |
| D3: e.g. descriptor makes one `MAPL_*Set` call per handle kind | Each characteristic calls the setter with its own keywords (§3.1) | A keyword bag would duplicate every setter signature in the graph. Plan left this to B0. |
| D3: `sync_to_info(handle, rc)` | `sync_to_info(handle, phase, rc)` | The phase argument is how FieldDelta is avoided (§3.2). |
| D4: order owned by the **export's** shape | Owned by the current **chain tip's** shape, re-queried each step | Legacy re-queries on each new tip; after a BRACKET->FIELD step, FIELD order applies (§4.1). |
| D1 new characteristics "restart mode, expression, vertical stagger, ..." | Those three, plus `LongName`, `HorizontalDimsSpec` and `VerticalOrientation` | LongName: Info parity for `SharedIO`/History (A8). The other two split out of Geometry and VerticalStagger (A14, Q7). |

## Risks / Trade-offs

- [The FIELD order change moves geometry before units] -> Only units and
  typekind have real transforms today, and their relative order (units
  before typekind) is unchanged. Golden tests pin the order.
- [The ATTRIBUTES "always insert" fix may turn a currently-passing
  scenario into a mismatch failure] -> That is the legacy behavior.
  B3-Attributes runs the scenarios and reports any change. It does not
  hide one.
- [UNCHECKED units now present where it used to be absent] -> D2 treats
  UNCHECKED as match, so the outcome equals today's one-sided absence.
- [Empty-create + Info + complete materialization depends on
  `FieldEmptyComplete` reading Info] -> This is the legacy path
  (`FieldClassAspect%allocate`). It is already exercised by every legacy
  scenario.
- [Per-consumer expression evaluation enlarges the expression hook] ->
  Deferred to B3-Expression, pending Q2.

## Migration Plan

B1 (mechanical move) -> B2 (status table, freeze check, allowlist;
TypeKind first) -> B3.x one characteristic at a time (B3-Units is the
template, and it writes `docs/graph/adding-a-characteristic.md`) -> B4
(dispatch removal, tip-descriptor materialization, ordering table
deletion) -> B5 (`make_characteristics`). Every step keeps ESSENTIAL
green on NAG and gfortran. There is no rollback beyond `git revert` per
task.

## Owner decisions

A1. Mirrored exports are disallowed by default. Only the EXPRESSION
    entries in §2.4 are allowlisted. Further entries need a compelling,
    demonstrated use case.
A8. Add `LongName`. Default chain: declared, then field dictionary, then
    the StandardName value. Supports the string mirror (A13).
A10. Runtime geometry change is a goal (History samplers on imports,
     ExtData epoch-dependent exports, moving nested grids). SetShared
     survives (§7.3).
A12. Values the component supplies are DEFERRED; TypeKind's R4 default is
     SPECIFIED. Watch for legacy cases where absent means MIRROR (§5).
A13. StandardName and LongName need a mirror (History). One named string
     sentinel in `VariableSpec`, imports only. Task B3-StringMirror, before
     B3-LongName.
A14. `horizontal_dims_spec` becomes its own characteristic,
     HorizontalDimsSpec (GEOM default, NONE for e.g. Ak/Bk with
     `dims: z`). GEOM vs NONE in either direction fails (NONE -> GEOM by
     replication is future work). With NONE on both sides Geometry is not
     compared, so such items connect across different horizontal grids.
     Mirror allowed on imports. Ordered before GEOMETRY (§4.2).
A15. RestartMode defaults to `RESTART_REQUIRED`. It is never compared and
     builds no transform. Imports sharing an export may differ; each
     writes its own alias, and the last restart phase wins (accepted).
A16. Combined steps: mismatches are detected per kind; one build step may
     resolve several kinds (§4.1). Fusing adjacent steps after freeze is
     future work.
A17. VerticalStagger has no built-in default. An undeclared stagger is
     DEFERRED at advertise (not an error), harvested from the component's
     realized field, NONE if the component has no vertical grid, and
     freeze fails otherwise.
A18. The vertical-grid characteristic is named `VerticalGrid`
     (`VerticalGridCharacteristic`, `VERTICAL_GRID_CHARACTERISTIC_KIND`,
     display `"VERTICAL_GRID"`). B3-RenameVerticalGrid renames
     VerticalCoordinate.

## Open Questions (deferred to the named B3 task)

Q2 (B3-Expression). Replicate legacy per-consumer instantiation (inputs
    extended to each consumer's goal; needed whenever inputs and the
    consumer differ in geometry/typekind), or keep Phase 7b's single
    instance and require such expressions to be SPECIFIED?
Q3 (B3-Expression). Expression units: legacy never compares them (UNITS
    is absent from `ExpressionClassAspect`'s order, and ExtData passes
    `'<unknown>'`). Is UNCHECKED for every expression correct? Also,
    legacy lists TYPEKIND before CLASS for expressions: was a typekind
    conversion on an expression's own (empty) payload ever meant to work?
Q4 (B3-ItemShape). `BracketClassAspect`'s time-dependent-geometry
    branch: no caller ever sets `is_time_dependent = .true.`. Can the
    graph implement only the non-time-dependent order? (A10's
    epoch-dependent ExtData geometry may be the intended user; check
    there first.)
Q5 (B3-ItemShape). `StateClassAspect` orders
    ATTRIBUTES/UNGRIDDED/GEOM/VGRID/UNITS/TYPEKIND for States. Plan D3
    says States have none. Confirm dropping them.
Q6 (B3-VerticalStagger). **Answered: confirmed.** `VERTICAL_STAGGER_MIRROR`
    maps to MIRRORED: the import adopts the export's stagger. It is for
    generic components (History writing another component's field in
    native form, where most characteristics are mirrored). It is legal on
    imports only; on an export it is an error (A1).
    Q-mirror (owner): `mirror_vgrid` does not replace the stagger keyword.
    When `mirror_vgrid` is set and no stagger is given, the stagger
    defaults to MIRRORED; an explicit stagger still wins (latent, no
    current user).
Q7 (B3-VerticalOrientation). **Answered (revised):** orientation (legacy
    "alignment"; name provisional) is the vertical index order relative to
    physical up/down, one value per item. CF `positive` is per-coordinate
    output metadata, not this. It is its own characteristic,
    `VerticalOrientation`, independent of VerticalStagger. Unspecified
    means the item's grid direction (`with_grid`), resolved to UP/DOWN once
    the grid is known (factory for an item grid, `resolve_deferred` for the
    component grid); it must not read VerticalGrid when comparing. An
    explicit mirror value (imports only) takes the export's resolved order;
    a MIRROR stagger does not imply it. If resolution does not work this
    way, stop and report.
Q9 (B3-QuantityType). Legacy `make_QuantityTypeAspect` ignores
    `VariableSpec%quantity_type` and writes default metadata plus a
    derived normalization that `NormalizationAspect` overwrites. The
    graph writes the declared value and only its own key. Confirm.
Q11 (B3-Units). **Answered: confirmed.** Legacy writes
    `units='/$MIRROR$'` into Info for mirrored units, and no reader
    exists. The graph never writes a MIRROR marker; the PhysicalUnits
    factory never produces MIRRORED (unallocated or `'<unknown>'` units
    are UNCHECKED), so only SPECIFIED and UNCHECKED reach `write_info`.
