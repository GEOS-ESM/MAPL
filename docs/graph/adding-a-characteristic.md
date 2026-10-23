# Adding or converting a characteristic

How to give a state-item description axis its target form: a
`StateItemCharacteristic` with generic status, an Info write method, and
a `VariableSpec` factory. PhysicalUnits (`B3-Units`) is the worked
example; TypeKind (`B3-TypeKind`) is the worked example for a
structural kind with a MIRRORED sentinel. Later B3 tasks refine this file.

Design authority: `openspec/changes/characteristic-separation-of-concerns/design.md`
(§2 status, §3 Info facade, §4.3 applicability, §5 factories). Rules:
`docs/graph/CONVENTIONS.md`.

## Checklist

1. **Characteristic type** (`graph/characteristics/<Name>Characteristic.F90`)
   - Extends `ValueCharacteristic` (value held inline) or
     `ReferenceCharacteristic` (holds a `NodeId`).
   - Implements the deferred `get_kind`, `needs_extension_for`,
     `build_transform`, `get_signature`.
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
