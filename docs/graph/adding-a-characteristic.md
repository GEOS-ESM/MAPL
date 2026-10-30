# Adding or converting a characteristic

How to give a state-item description axis its target form: a
`StateItemCharacteristic` with generic status, an Info write method, and
a `VariableSpec` factory. PhysicalUnits (`B3-Units`) is the worked
example; TypeKind (`B3-TypeKind`) is the worked example for a
structural kind with a MIRRORED sentinel; UngriddedDims
(`B3-UngriddedDims`) adds a kind with its own shape set and a
descriptor-side duplicate removed; StandardName (`B3-StandardName`) is the
worked example for deleting a subclass's own status check; Attributes
(`B3-Attributes`) is the worked example for a kind with no Info key; LongName
(`B3-LongName`) is the worked example for a never-compared kind with a
default chain. Later
B3 tasks refine this file.

Design authority: `openspec/changes/characteristic-separation-of-concerns/design.md`
(§2 status, §3 Info facade, §4.3 applicability, §5 factories). Rules:
`docs/graph/CONVENTIONS.md`.

## Checklist

1. **Characteristic type** (`graph/characteristics/<Name>Characteristic.F90`)
   - Extends `ValueCharacteristic` (value held inline) or
     `ReferenceCharacteristic` (holds a `NodeId`).
   - Implements the deferred `get_kind`, `needs_extension_for`,
     `build_transform`, `get_signature`. A `build_transform` with no
     Transform for a pair calls `report_build_transform_unsupported(rc)`
     (raises nothing; `transformer` stays unallocated). A pair that can
     never connect calls `report_build_transform_incompatible(rc)`
     (also raises nothing; reported as a connection conflict). Use `_FAIL`
     only for genuine errors.
   - **No status logic in the subclass.** `needs_extension_for` compares
     values only; `StateItemDescriptor%resolve_pair` decides from the
     two statuses and calls it only when both sides are SPECIFIED.
   - Must tolerate a valueless instance (UNCHECKED, MIRRORED, DEFERRED):
     accessors and `get_signature` must not touch an unallocated
     component. `PhysicalUnitsCharacteristic%get_units` returns `''`.
2. **Status at construction**
   - The value constructor (`<Name>Characteristic(value)`) sets
     SPECIFIED. It does not interpret `VariableSpec` sentinels (such as
     `MAPL_TYPEKIND_MIRROR`); the factory does, and sets MIRRORED itself.
   - A default-initialized variable (`type(<Name>Characteristic) :: c`)
     is INVALID. INVALID fails `resolve_pair` at connect time and fails
     the freeze check (`StateItemDescriptor%check_resolved`).
   - Therefore a factory must either call the value constructor or set
     the status explicitly (`call c%set_status(CHARACTERISTIC_STATUS_UNCHECKED)`,
     MIRRORED, DEFERRED). Never insert a default-initialized
     characteristic.
3. **Info write** (`write_info`, `is_structural`)
   - Override `write_info(this, handle, phase, rc)`. The base version
     `_FAIL`s naming the kind, so `sync_to_info` on a descriptor holding
     an unconverted kind fails loudly. A kind with no Info key (design
     §3.4: Attributes, Expression) still overrides it, as a successful
     no-op, so that `sync_to_info` does not fail on it.
   - Inspect `handle%itemType()` and call `MAPL_FieldSet` /
     `MAPL_FieldBundleSet` / `MAPL_StateSet` once with only this
     characteristic's own keywords (design.md §3.4 table). Fail for a
     handle kind the characteristic does not apply to.
   - `sync_to_info` calls `write_info` only for SPECIFIED and UNCHECKED
     entries. Decide what UNCHECKED writes (§3.3: `'<unknown>'` for units
     and standard_name, nothing for every other key). MIRRORED and
     DEFERRED never reach `write_info`; do not invent a marker for them.
   - Phases (`SYNC_PHASE_STRUCTURAL`, `SYNC_PHASE_DESCRIPTIVE`, in
     `mapl_StateItemCharacteristic_mod`): STRUCTURAL runs between
     empty-create and complete; DESCRIPTIVE may run on a completed
     field. Override `is_structural()` to return `.true.` for creation
     inputs (Geometry, VerticalGrid, VerticalStagger, TypeKind,
     UngriddedDims); `sync_to_info` skips them in DESCRIPTIVE.
   - A descriptive characteristic whose keyword is a `FieldSet`
     FieldDelta trigger (`geom`, `vgrid`, `typekind`, `num_levels`,
     `units`) must also return early unless the phase is STRUCTURAL.
     PhysicalUnits does this.
4. **Factory** (`specs/VariableSpec/make_characteristics.F90`, a submodule
   of `mapl_VariableSpec_mod`)
   - Add a private `subroutine make_<name>_characteristic(spec, descriptor, rc)`
     to the submodule, and register it in the `factories` array in
     `make_characteristics` (size the array and add one
     `factories(n)%make => ...` line). `VariableSpec.F90` is not edited:
     only `make_characteristics` is type-bound.
   - Apply §4.3 applicability first, as an allowlist: insert only for the
     shapes the table lists for this kind and return for everything else.
     Kinds that apply to every field-carrying shape (FIELD, BRACKET,
     VECTOR, VECTORBRACKET, FIELDBUNDLE, EXPRESSION) call the submodule's
     `carries_field_description(spec%itemType)`; a kind with a different
     set writes its own list. A denylist silently covers `MAPL_STATEITEM_UNKNOWN`, the service
     provider/subscriber flags, and any shape added later.
   - Apply the §5 status rule. Values the component supplies later are
     DEFERRED, not MIRRORED. MIRRORED comes only from an explicit mirror
     sentinel. List any legacy "absent means mirror" cases you find in
     the commit message.
5. **Wire it in**: delete the kind's block in
   `GraphBuilder_Advertising_smod.F90%populate_characteristics` (the
   `call var_spec%make_characteristics(descriptor, _RC)` already there
   runs the registered factory), and drop the kind constant and concrete
   type from that submodule's use list.
   Materialization references (`get_item_*`, `merged_item_for_link`) are
   removed in B4, not here.
6. **Tests**
   - `graph/tests/Test_StateItemDescriptor.pf`: `sync_to_info` writes the
     SPECIFIED and UNCHECKED values in the right phases and skips the
     others, on both a Field and a FieldBundle handle. An empty field
     has no Info for a creation-input key, so a "not written" test seeds
     the key first and checks it is unchanged.
   - `tests/Test_VariableSpecCharacteristics.pf`: factory status and
     value per input, every applicable shape, and no entry for
     non-applicable shapes (including `MAPL_STATEITEM_UNKNOWN` and the
     service provider/subscriber flags).
   - Existing `Test_StatusResolution.pf` covers the status table; add a
     case only if the kind changes it.

## PhysicalUnits as implemented

| Input (`VariableSpec%units`) | Shape | Entry |
|---|---|---|
| allocated, not `'<unknown>'` | FIELD, BRACKET, VECTOR, VECTORBRACKET, FIELDBUNDLE, EXPRESSION | SPECIFIED with the value |
| unallocated or `'<unknown>'` | same | UNCHECKED, no value |
| any | every other flag (STATE, SERVICE, WILDCARD, UNKNOWN, ...) | none |

- UNCHECKED on either side matches (no conversion), which equals the
  old behavior of omitting the entry.
- `write_info` writes `units=` only in STRUCTURAL; UNCHECKED writes
  `'<unknown>'` (`UNKNOWN_UNITS`). Legacy's `'/$MIRROR$'` marker is not
  reproduced (owner Q11): units are never MIRRORED.
- Materialization still reads units through `get_item_units` until B4;
  an UNCHECKED import yields `''` there, as an absent entry did before.

## TypeKind as implemented

| Input (`VariableSpec%typekind`) | Shape | Entry |
|---|---|---|
| `MAPL_TYPEKIND_MIRROR` | FIELD, BRACKET, VECTOR, VECTORBRACKET, FIELDBUNDLE, EXPRESSION | MIRRORED, no value |
| any other flag, including the R4 default | same | SPECIFIED with the label (`'R4'`, `'R8'`, `'I4'`, `'I8'`, `'LOGICAL'`) |
| any | every other flag | none |

- Never DEFERRED: `VariableSpec%typekind` has a concrete default (design
  §5, owner A12).
- Legacy absent-means-mirror: `TypekindAspect()` with no argument is
  MIRRORED, but `make_TypekindAspect` always passes `this%typekind`
  (default R4), so no `VariableSpec` reaches it. None reproduced.
- `is_structural()` is `.true.`; `write_info` writes `typekind=` through
  `MAPL_FieldSet`/`MAPL_FieldBundleSet`. MIRRORED never reaches it
  (`sync_to_info` filters), and UNCHECKED is not a TypeKind state.
- `get_item_typekind` and `merged_item_for_link` still read the kind
  until B4.

## UngriddedDims as implemented

| Input (`VariableSpec%ungridded_dims`) | Shape | Entry |
|---|---|---|
| `is_mirror()` | FIELD, BRACKET, VECTOR, VECTORBRACKET, FIELDBUNDLE | MIRRORED, no value |
| anything else, including the empty default | same | SPECIFIED with the dims |
| any | every other flag, including EXPRESSION | none |

- Own shape set: design §4.3 lists no UNGRIDDED_DIMS for EXPRESSION,
  STATE, SERVICE or WILDCARD, so the factory does not use
  `carries_field_description` alone (it excludes EXPRESSION). Before this
  task the kind was inserted for every shape; the only difference is the
  missing entries on those shapes.
- Never DEFERRED: `VariableSpec%ungridded_dims` always has a value (empty
  by default).
- Legacy absent-means-mirror: `UngriddedDimsAspect()` with no argument is
  mirror, but `make_UngriddedDimsAspect` always passes
  `this%ungridded_dims` (non-mirror default), so only an explicit
  `UngriddedDims(is_mirror=.true.)` reaches it. None reproduced.
- The value constructor stores whatever it is given; the factory turns a
  mirror `UngriddedDims` into MIRRORED status, so a MIRRORED instance
  holds an empty `UngriddedDims` and never carries `is_mirror`.
- `is_structural()` is `.true.`; `write_info` writes `ungridded_dims=`
  through `MAPL_FieldSet`/`MAPL_FieldBundleSet`. MIRRORED never reaches it.
- `StateItemDescriptor` no longer has an `ungridded_dims` component or
  `get/set_ungridded_dims`. Materialization reads the value through
  `get_item_ungridded_dims` (a B4 removal, like `get_item_units`).

## Geometry as implemented

First kind with a DEFERRED value (a value the component supplies later).

| Input | Shape | Entry |
|---|---|---|
| `VariableSpec%geom` (with or without `geom_id`) or `geom_id` alone | FIELD, BRACKET, VECTOR, VECTORBRACKET, FIELDBUNDLE, EXPRESSION, SERVICE | SPECIFIED |
| `VariableSpec%mirror_geom` and no geom | same | MIRRORED |
| nothing declared | same, except EXPRESSION | DEFERRED |
| nothing declared | EXPRESSION | MIRRORED (the §2.4 allowlist case) |
| any | STATE, WILDCARD, other flags | none |

- A mirror on an export is rejected at advertise by the generic rule (A21):
  no MIRRORED entry is allowed on a non-EXPRESSION export. A characteristic
  that adds a mirror value adds it to `VariableSpec%requests_mirror()`.
- A DEFERRED entry is filled by `StateItemDescriptor%resolve_deferred`,
  which offers a `CharacteristicDefaults` (the component's geom and
  geom id) to every DEFERRED characteristic. `StateItemCharacteristic`'s
  default `resolve_deferred` does nothing; a new kind with a
  component-supplied value overrides it and adds its field to
  `CharacteristicDefaults`. GEOM_B (`graphbuilder_resolve_deferred`) is the
  first caller.
- An item with no declared geom and a component with no default geom stays
  DEFERRED. It is not papered over with UNCHECKED or MIRRORED. The
  component fills it in its own init phase by setting the geom on the
  item's field or bundle (`MAPL_FieldSet(geom=)`); `graphbuilder_harvest_deferred`
  reads that back at the start of connection resolution (exports and
  internals only; imports get geometry from the connected export). Statistics
  does this in `INIT_REALIZE_PROVIDED`.
- A connection with a DEFERRED side waits (`PAIR_WAIT`) and is retried on
  a later pass. A characteristic still DEFERRED at freeze is reported.
- No import is turned MIRRORED implicitly: absent geom is DEFERRED and
  waits like any other DEFERRED item. This differs from the legacy aspect,
  where an absent geom meant MIRRORED. A component whose imports should
  adopt the connected export's geometry says so: `mirror_geom=.true.` on
  `MAPL_GridCompAddSpec`, or `geometry_mirror: true` on a YAML variable.
  Statistics imports and History collection imports (when the collection
  declares no `geom:`) do this.
- Only the geometry identity and `horizontal_dims_spec` are compared; the
  `ESMF_Geom` and regridder param are carried for `write_info`.
- `is_structural()` is `.true.`; `write_info` writes `geom=`,
  `horizontal_dims_spec=` and `regridder_param_info=` through
  `MAPL_FieldSet` / `MAPL_FieldBundleSet`. DEFERRED and MIRRORED never
  reach it.
- `StateItemDescriptor` no longer has `declared_geom` or
  `get/set_declared_geom`; `resolve_export_geom` reads the item's own
  GEOMETRY characteristic and falls back to the owner's default geom.
- `build_transform` calls `report_build_transform_unsupported`, so a geometry
  mismatch is reported as unsupported rather than as an error.

## VerticalGrid as implemented

Second kind with a DEFERRED value; it reuses Geometry's machinery.

| Input | Shape | Entry |
|---|---|---|
| `vertical_stagger` NONE | any | none (no vertical dimension) |
| `VariableSpec%vertical_grid` | FIELD, BRACKET, VECTOR, VECTORBRACKET, FIELDBUNDLE | SPECIFIED, holding the `VerticalGrid` |
| `VariableSpec%mirror_vgrid` and no grid | same | MIRRORED |
| nothing declared | same | DEFERRED |
| nothing declared | EXPRESSION | MIRRORED (the §2.4 allowlist case) |
| `VERTICALGRID` composite | STATE | SPECIFIED, dimension names only |
| any | SERVICE, other STATE, WILDCARD | none |

- `CharacteristicDefaults` also carries the component's vertical grid;
  `graphbuilder_resolve_deferred` (GEOM_B) sets it from
  `OuterMetaComponent%get_vertical_grid`.
- A kind that does not apply when the component supplies nothing overrides
  `drop_if_unsupplied`; `StateItemDescriptor%drop_unsupplied` removes those
  entries after `resolve_deferred`. Only VerticalGrid does this, so a
  2D item in a component with no vertical grid stays valid. Geometry stays
  DEFERRED.
- `harvest` does not read `vgrid`: no DEFERRED VERTICAL_GRID is left
  by then.
- `VerticalStaggerLoc`'s `==` treats MIRROR as a wildcard; the factory uses
  `is_none()`.
- `write_info` writes `vgrid=` through `MAPL_FieldSet` /
  `MAPL_FieldBundleSet`; `is_structural()` is `.true.`. `FieldGet` finds the
  grid by id, so the grid must be known to the vertical grid manager.
- Hybrid `needs_extension_for` is kept; the single-overlap `build_transform`
  calls `report_build_transform_unsupported`.
- `StateItemDescriptor` no longer has `declared_vertical_grid`;
  `resolve_export_vgrid` reads the item's characteristic and falls back to
  the owner's vertical grid.
- Test fixtures that want a 2D item set `vertical_stagger = NONE`
  (`make_var` does).
- A mirror on an export is rejected at advertise by the generic rule (A21),
  not by a VerticalGrid check.

## VerticalStagger as implemented

A structural kind with a DEFERRED value and no built-in default.

| Input | Shape | Entry |
|---|---|---|
| CENTER, EDGE or an explicit NONE | FIELD, BRACKET, VECTOR, VECTORBRACKET, FIELDBUNDLE | SPECIFIED |
| `VERTICAL_STAGGER_MIRROR` | same | MIRRORED (mirror on an export: generic rule, A21) |
| unallocated or `VERTICAL_STAGGER_INVALID`, with `mirror_vgrid` | same | MIRRORED |
| unallocated or `VERTICAL_STAGGER_INVALID`, otherwise | same | DEFERRED |
| nothing declared | EXPRESSION | MIRRORED (the §2.4 allowlist case) |
| `VERTICALGRID` composite | STATE | none |
| any | SERVICE, other STATE, WILDCARD | none |

- `parse_var_specs` passes `VERTICAL_STAGGER_INVALID` when the YAML has no
  `vertical_dim_spec`; the factory treats that as undeclared.
- `CharacteristicDefaults` carries `component_scope` (set by GEOM_B, so
  "no vertical grid" means the component has none, not that it is unknown)
  and `vert_stagger` (harvested from a completed Field, or a bundle's first
  member, that recorded `vert_staggerloc`; `MAPL_FieldGet` returns
  `VERTICAL_STAGGER_INVALID` for a field that never recorded one). `resolve_deferred` takes the
  harvested value first; otherwise a component-scope default with no
  vertical grid gives SPECIFIED NONE; otherwise the entry stays DEFERRED
  until harvested, or freeze fails. `drop_if_unsupplied` is not
  overridden: a DEFERRED stagger is never dropped.
- An explicit NONE stays SPECIFIED NONE even though VerticalGrid has no
  entry for it, so a 2D export still mismatches a 3D import.
- `needs_extension_for` compares `to_string()` values because
  `VerticalStaggerLoc`'s `==` treats MIRROR as a wildcard.
- `write_info` writes `vert_staggerloc=` in the STRUCTURAL phase only.
- `resolve_export_vgrid` reads the stagger from the characteristic and
  returns it; a SPECIFIED NONE returns before the component's grid is
  consulted.
- Test fixtures that build a `VariableSpec` by hand must set
  `vertical_stagger` for the item to settle in a component with a grid.

## VerticalOrientation as implemented

A structural value kind: the vertical index order (UP or DOWN). Legacy
calls it alignment. It is independent of VerticalStagger.

| `vertical_alignment` | Shape | Entry |
|---|---|---|
| `upward` / `downward` | FIELD, BRACKET, VECTOR, VECTORBRACKET, FIELDBUNDLE, EXPRESSION | SPECIFIED UP / DOWN |
| `mirror` | same | MIRRORED (mirror on an export: generic rule, A21) |
| unallocated or `with_grid`, item has a `vertical_grid` | same | SPECIFIED with that grid's `get_coordinate_direction()` |
| unallocated or `with_grid`, with `mirror_vgrid`, or EXPRESSION | same | MIRRORED |
| unallocated or `with_grid`, otherwise | same | DEFERRED |
| any, `vertical_stagger` NONE; `VERTICALGRID` composite | | none |
| any | STATE, SERVICE, WILDCARD | none |

- `resolve_deferred` takes the direction from the component's vertical grid
  (`CharacteristicDefaults` already carries it). `drop_if_unsupplied` drops
  a DEFERRED entry when the component has no vertical grid.
- Comparison reads only the two direction values, never a `VerticalGrid`.
- `write_info` writes `vert_alignment=` in the STRUCTURAL phase for
  SPECIFIED entries only; `with_grid` is never written because the value is
  already resolved.
- A MIRROR stagger does not imply a mirrored orientation.
- `build_transform` reports unsupported (no reversal or vertical-regrid
  `Transform`).
- `parse_var_specs` reads the `vertical_alignment` key into
  `VariableSpec%vertical_alignment` (also set by `GridCompAddSpec`). The
  `vertical_alignment_*` scenarios now carry real orientations, but their
  orientation mismatch is masked: the graph reports only the first
  unsupported kind per connection, and `VERTICAL_GRID` comes first.
- The resolved direction is written, not `with_grid` (owner-accepted
  deviation from legacy; readers resolve either against the same grid). A
  SPECIFIED entry whose direction is neither up nor down fails the write.

## HorizontalDimsSpec as implemented

A structural value kind: GEOM (the item spans the horizontal geometry) or
NONE (`dims: z`, no horizontal dimensions). It was part of
`GeometryCharacteristic`; it is now its own kind.

| Input (`VariableSpec%horizontal_dims_spec`) | Shape | Entry |
|---|---|---|
| GEOM (default) or NONE | FIELD, BRACKET, VECTOR, VECTORBRACKET, FIELDBUNDLE | SPECIFIED with the value |
| MIRROR | same | MIRRORED, no value (mirror on an export: generic rule, A21) |
| any | EXPRESSION, SERVICE, STATE, WILDCARD | none |

- Comparison is plain equality. GEOM vs NONE in either direction is a
  mismatch, and `build_transform` reports
  `BUILD_TRANSFORM_INCOMPATIBLE` (no Transform can change it). The
  connection is reported in `connection_conflicts` as
  `incompatible HORIZONTAL_DIMS_SPEC: export <signature>, import
  <signature>`; the component's other connections still resolve.
- `write_info` writes `horizontal_dims_spec=` through `MAPL_FieldSet` in
  the STRUCTURAL phase (it decides the grid-to-field map at completion). A
  bundle has nothing to write: the bundle setter has no such keyword.
- Mirror. `HORIZONTAL_DIMS_MIRROR` (YAML `dims: mirror`) is a member of
  the `HorizontalDimsSpec` enum, not a value to compare: the characteristic
  built from it is MIRRORED with no value, so an import adopts the
  export's spec (`PAIR_ADOPT`). An export carrying it is rejected by the
  generic rule (A21).
  Legacy `GeomAspect` treats it as agreeing with any spec and adopts the
  export's at `connect_to_export`; field creation asserts GEOM or NONE.
- **Uncompared Geometry.** For a NONE item the Geometry factory builds the
  entry with `compared=.false.`: UNCHECKED instead of SPECIFIED or
  DEFERRED, so two NONE items connect across different horizontal grids.
  Field creation still needs a geom, so the entry may carry one:
  `write_info` writes an UNCHECKED Geometry only when it holds a geom, and
  `resolve_deferred` (now offered to UNCHECKED entries too) fills one that
  has none from the component default. A `mirror_geom` import fed by a NONE
  export adopts its UNCHECKED entry (design A20). This gives UNCHECKED a second meaning
  for Geometry only; design.md §3.3 records it.
- Extension order: every shape order that lists GEOMETRY puts this kind
  just before it (`ItemShapeCharacteristic%get_extension_order`). With NONE
  the Geometry never mismatches, so the order does not matter yet.
- Legacy `GeomAspect%matches` is true for two NONE items without comparing
  geom or geom_id, so legacy builds no regrid across different horizontal
  grids (`Test_Aspects`). A MIRROR spec still compares geoms.
- Test: scenario `graph-scenarios/dims_z_field` (children on different
  horizontal grids), plus unit tests for the pair outcomes and the factory.

## StandardName as implemented

| Input (`VariableSpec%standard_name`) | Shape | Entry |
|---|---|---|
| allocated, not `'<unknown>'` | FIELD | SPECIFIED with the value |
| unallocated or `'<unknown>'` | FIELD | UNCHECKED, no value |
| any | every other flag, including BRACKET, VECTOR, VECTORBRACKET, FIELDBUNDLE, EXPRESSION | none |

- Own shape set (design §4.3): only FIELD carries a standard name. Before
  this task the entry was inserted for any shape that declared one and
  omitted otherwise; the differences are that an undeclared FIELD now has
  an UNCHECKED entry, and a declared name on another shape is dropped. A
  shape with no entry is never compared (one-sided absence, design §2.6).
- The subclass no longer reads `CharacteristicStatus`: the UNCHECKED
  escape in `needs_extension_for` is gone, and `resolve_pair` makes
  UNCHECKED match. `needs_extension_for` is plain string inequality.
- Never DEFERRED. MIRRORED only through the `MAPL_STRING_MIRROR`
  sentinel (see StringMirror below). Legacy absent-means-mirror: none
  (`StandardNameAspect` is UNCHECKED when absent).
- `write_info` writes `standard_name=` through `MAPL_FieldSet` /
  `MAPL_FieldBundleSet` in both phases: it is not a `FieldDelta` trigger
  and is not a creation input, so `is_structural()` stays `.false.`.
  UNCHECKED writes `UNKNOWN_STANDARD_NAME` (`'<unknown>'`), as units do.
- Not reproduced from the legacy aspect: the warning when only one side
  declares a name, and the `ValidationMode` strict/permissive severity
  (they were already absent from the characteristic). Field-dictionary
  validation of the name stays in `VariableSpec` construction.

## StringMirror as implemented

A shared sentinel, not a characteristic of its own. `VariableSpec` exports
`MAPL_STRING_MIRROR = '<mirror>'`; YAML spells it `standard_name: "<mirror>"`
(written quoted).

- `make_VariableSpec` does not store the sentinel. It sets
  `standard_name_mirror = .true.` and leaves `standard_name` unallocated.
  The legacy readers (the FieldDictionary lookup, `StandardNameAspect`,
  `split_name` on vectors) therefore never see the literal string. An
  unallocated name makes the FieldDictionary lookup fall back to the
  `short_name` alias, so units and long_name still default on the import.
- The StandardName factory maps the flag to MIRRORED; an export carrying it
  is rejected by the generic rule (A21). Only FIELD carries the entry,
  as for every standard-name case.
- Against an export with no declared name (UNCHECKED), the mirrored import
  becomes UNCHECKED: there is nothing to copy and nothing is compared. This
  is the generic (UNCHECKED, MIRRORED) rule in the descriptor, for all kinds.
- LongName (B3-LongName) reuses the same sentinel and the same rule; see
  LongName below.
- A `MAPL_STRING_MIRROR` request on an export is rejected by the generic rule
  (A21), in `make_VariableSpec` and after the factory loop.

## LongName as implemented

A descriptive, never-compared kind: `needs_extension_for` is always false
and `build_transform` is the unsupported stub, so a connection never needs
a step for it.

| Input | Shape | Entry |
|---|---|---|
| `MAPL_STRING_MIRROR` (`long_name_mirror`) | FIELD, EXPRESSION | MIRRORED (imports only; exports: generic rule, A21) |
| `long_name` allocated, even empty | same | SPECIFIED with the value |
| unallocated, not an import (export, internal), `standard_name` declared and not `'<unknown>'` | same | SPECIFIED with the standard name |
| unallocated, otherwise (including every import) | same | UNCHECKED, no value, writes nothing |
| any | every other flag (BRACKET, VECTOR, VECTORBRACKET, FIELDBUNDLE, STATE, SERVICE, WILDCARD, ...) | none |

- Own shape set (design §4.3): legacy passes `long_name` only to
  `FieldClassAspect` and `ExpressionClassAspect`. A bundle can still be
  written by hand: `write_info` accepts a Field or FieldBundle handle.
- Default chain (design A8): declared, then the field dictionary (applied
  by `make_VariableSpec` before any factory runs), then the
  StandardName value. The last step applies to every intent except IMPORT
  (export and internal); legacy has no standard-name fallback, so this is
  a deliberate difference. An import with no `long_name` stays UNCHECKED
  and takes the export's through the generic UNCHECKED rule, as legacy
  `connect_to_export` (`mirror_name`) does. A mirrored `standard_name` is unallocated in the spec, so it does not
  count as declared. A mirrored `long_name` never falls back.
- Legacy absent-means-mirror: an import with no `long_name` inherits the
  export's value in `FieldClassAspect%connect_to_export` (`mirror_name`).
  The graph reproduces it as UNCHECKED, not MIRRORED: an UNCHECKED import
  stays valueless and writes nothing, so the field keeps the export's
  long_name. The exception is an import that declares its own `long_name`:
  that import is SPECIFIED and keeps its value (legacy: dst wins), and so
  does the graph.
- Mirror: `make_VariableSpec` strips `MAPL_STRING_MIRROR` into
  `long_name_mirror`, so no legacy reader sees the string;
  `requests_mirror()` includes it, so an export is rejected by the generic
  rule.
- `write_info` writes `long_name=` through `MAPL_FieldSet` /
  `MAPL_FieldBundleSet` in both phases (not structural). `MAPL_FieldSet`
  stores a field's long_name under the field's own alias; a bundle's is
  bundle-wide (id 0). UNCHECKED writes nothing (design §3.3).
- Not wired yet: `populate_one` does not call `sync_to_info`, and
  `is_per_alias()` does not exist. B3-RestartMode adds both and sets
  LongName's flag. Until then LongName is covered by unit tests only.
- `parse_var_specs` already reads `standard_name` and `long_name`; the YAML
  test covers `"<mirror>"` for both.

## Attributes as implemented

| Input (`VariableSpec%attributes`) | Shape | Entry |
|---|---|---|
| any, including the empty default | FIELD, BRACKET, VECTOR, VECTORBRACKET, FIELDBUNDLE | SPECIFIED with the names |
| any | every other flag, including EXPRESSION | none |

- Always inserted, even when empty (design §5). Before this task an empty
  set produced no entry, so an import requiring attributes was never
  flagged against an export that declared none; legacy
  `includes([], required)` is a mismatch, and now so is this. The
  comparison needs no new rule: an export without the names the import
  requires is `PAIR_EXTEND`, which has no Transform, so it is reported
  as unsupported (`BUILD_TRANSFORM_UNSUPPORTED`).
- Own shape set (design §4.3): like UngriddedDims, not EXPRESSION. Before
  this task the entry was inserted for any shape that declared attributes
  (including EXPRESSION, STATE, SERVICE and WILDCARD) and omitted otherwise.
- The comparison is asymmetric (export must provide everything the import
  requires; extra export names are fine). `resolve_pair` passes the
  export as `this`, so no change was needed.
- Never DEFERRED, MIRRORED or UNCHECKED: there is no mirror sentinel, no
  component-supplied value and no unknown marker. Legacy
  absent-means-mirror: none (`AttributesAspect` is constructed
  non-mirror and cannot mirror).
- `write_info` writes nothing and succeeds, in both phases and for any
  handle kind: attributes have no Info key (legacy
  `AttributesAspect%update_payload` is a no-op, and the `FieldSet`
  `attributes` dummy is unused). `is_structural()` stays `.false.`.
- Scenarios: no ESSENTIAL scenario changed. `MAPL.generic.scenarios.pending`
  fails the same 114 checks (`history_1`, `service_service`,
  `service_with_options`, `statistics`, `statistics_real`) at HEAD and
  with this change.

## ItemShape as implemented

| Input (`VariableSpec`) | Shape | Entry |
|---|---|---|
| `itemType`, optional `state_item_variant` | every shape | SPECIFIED with the class and the variant if declared |

- Factory `make_item_shape_characteristic` (factory 11); the block in
  `populate_characteristics` that inserted it for FIELD, BRACKET, VECTOR and
  VECTORBRACKET only is gone. `declared_item_type` stays: the
  `GraphBuilder` gates still read it (design §6.1 wants a generic
  `shape()` accessor for them; not done here).
- Matrix (design §4.3, legacy `matches` / `supports_conversion_specific`):
  FIELD, FIELDBUNDLE and STATE match themselves and WILDCARD; VECTOR and
  SERVICE match themselves; BRACKET -> FIELD builds
  `BracketTimeInterpolationTransform`; EXPRESSION -> FIELD matches (no
  shape step; evaluation is per consumer, design Q2, and a consumer that
  differs shows up as a non-shape mismatch until B3-Expression);
  VECTORBRACKET -> VECTOR is `BUILD_TRANSFORM_UNSUPPORTED` (no
  time-interpolation step yet); every other pair, including
  VECTORBRACKET -> VECTORBRACKET, is `BUILD_TRANSFORM_INCOMPATIBLE` and
  is reported as a connection conflict.
- `get_extension_order(goal, rc)` returns the design §4.2 list for the
  shape (GEOM variant: GEOMETRY; VERTICALGRID variant: VERTICAL_GRID).
  BRACKET/VECTORBRACKET use the non-time-dependent list only (owner Q4).
  STATE and WILDCARD return an empty list (owner Q5). `goal` is unused.
- Ordering happens in `resolve_characteristics` (`order_mismatched`), so
  every caller, including `find_mismatched_characteristics`, gets ordered
  kinds and `GraphBuilder` names no kind. Each step takes the first
  mismatched kind in the current tip's shape order, else the first
  remaining kind by `to_string()`. After the ITEM_SHAPE step the tip's shape
  is the import's, so the rest follow that order (BRACKET -> FIELD puts
  units after the shape step). Only the shape is replaced on the tip; the
  general covered-kind replacement is B4.
- `GraphStateItem%ordering` and `characteristic_ordering_table` are
  deleted; their tests are replaced by one golden test per shape in
  `Test_StateItemCharacteristicSubclasses` and ordering tests in
  `Test_ExtensionResolution`.
- `write_info` is minimal, like legacy (which never tags class payloads).
  It writes `state_item_variant=` only when a variant was declared
  (VERTICALGRID, GEOM), on a Field, FieldBundle or State. It writes
  `fieldBundleType=` for BRACKET, VECTOR, VECTORBRACKET and SERVICE
  bundles in the STRUCTURAL phase only (legacy sets it at
  `MAPL_FieldBundleCreate`). It writes nothing else. Reason: an EXPRESSION
  or a BRACKET -> FIELD node is materialized as a plain Field, and tagging
  it with its declared class would fail a FIELD kind check in
  `bind_port` or `MethodGraphNode%bind_argument`.
  `CompositeStateMaterialization` and `GeomProxyField` still call the
  setters directly.
- Standard name after BRACKET -> FIELD: not checked. Legacy
  `StandardNameAspect%connect_to_export` adopts the export's name, and a
  BRACKET has no StandardName entry, so the graph matches legacy
  PERMISSIVE mode. Strict-mode checking waits for a graph validation mode.
- Scenarios: no ESSENTIAL scenario changed, and the "no registered extension
  provider" warnings are identical to a clean HEAD baseline.
  `MAPL.generic.scenarios.pending` fails the same 114 checks with identical
  messages at HEAD and with this change.
