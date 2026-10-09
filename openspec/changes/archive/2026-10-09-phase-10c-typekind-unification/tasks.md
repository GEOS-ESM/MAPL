## 1. Amend the roadmap's own 10c entry

- [x] 1.1 In `docs/graph/spec/20-implementation-roadmap.md` §20.4.8,
      replace 10c's own "gains a `TYPE_KIND_CHARACTERISTIC_ID` entry...
      mirroring the existing `units`/`vertical_grid` entries" wording
      with design.md's own Decision: 10c builds on the existing,
      already-shipped `TypeKindCharacteristic`
      (`ValueCharacteristic`-based,
      `superstructure/generic/graph/TypeKindCharacteristic.F90`) via an
      adapter/bridge at the `CharacteristicMap`/`build_characteristics()`
      insertion point - explicitly NOT a second,
      `Characteristic`-extending `TypeKindCharacteristic`.
- [x] 1.2 Add an explicit sentence naming the collision this amendment
      prevents: the as-originally-worded 10c would collide (same module
      name `mapl_TypeKindCharacteristic_mod`, same type name
      `TypeKindCharacteristic`) with the already-shipped
      `state-item-characteristics` type - not merely a conceptual
      overlap like the `Units`/`PhysicalUnits`,
      `Geom`/`Geometry`, `VerticalGrid`/`VerticalCoordinate` pairs, but
      a literal compile-time name collision.
- [x] 1.3 Leave the choice between design.md's two bridge-shape options
      (optional adaptation method on `StateItemCharacteristic`, vs. a
      `Characteristic`-side wrapper holding a `TypeKindCharacteristic`
      by composition) explicitly open for 10c's own future design.md -
      do not pre-decide it in the roadmap text itself.
- [x] 1.4 Leave the rest of 10c's entry unchanged (the real precision-
      cast transform naming-collision-check note, the new
      two-characteristic fixture, the `invalidate/` fixture
      relationship, the 10a/10b dependency) - this amendment only
      changes which characteristic class 10c builds on, not its other
      scope.

## 2. Verify

- [x] 2.1 Re-read the edited §20.4.8 10c entry end to end to confirm it
      reads coherently with the amendment in place and does not
      contradict any other part of that entry or the surrounding 10a/
      10b/10d entries.
- [x] 2.2 Confirm no other section of the roadmap (or any other file
      under `docs/graph/spec/`) was modified - this change is scoped to
      10c's own entry only.
- [x] 2.3 Confirm the amendment states a real constraint (build on the
      existing `TypeKindCharacteristic`, no second one) without
      prescribing 10c's own future implementation details beyond that -
      matching design.md's own Non-Goals.
- [x] 2.4 `openspec validate phase-10c-typekind-unification --strict`
      reports the change valid.
