# Adding or converting a characteristic

How to give a state-item description axis its target form: a
`StateItemCharacteristic` with generic status, an Info write method, and
a `VariableSpec` factory. PhysicalUnits (`B3-Units`) is the worked
example; TypeKind (`B3-TypeKind`) is the worked example for a
structural kind with a MIRRORED sentinel; UngriddedDims
(`B3-UngriddedDims`) adds a kind with its own shape set and a
descriptor-side duplicate removed. Later B3 tasks refine this file.

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
     (raises nothing; `transformer` stays unallocated); use `_FAIL` only
     for genuine errors.
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
     an unconverted kind fails loudly.
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
     inputs (Geometry, VerticalCoordinate, VerticalStagger, TypeKind,
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

## VerticalCoordinate as implemented

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
  entries after `resolve_deferred`. Only VerticalCoordinate does this, so a
  2D item in a component with no vertical grid stays valid. Geometry stays
  DEFERRED.
- `harvest` does not read `vgrid`: no DEFERRED VERTICAL_COORDINATE is left
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
