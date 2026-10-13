## 1. Give StateItemCharacteristic a build_transform and get_signature contract

- [x] 1.1 In `StateItemCharacteristic.F90`, add two new deferred methods
      to the `StateItemCharacteristic` abstract type:
      `build_transform(this, goal, transformer, rc)` (same signature
      `Characteristic%build_transform` already has post-
      `decouple-transform-from-graph` - `class(Transform), allocatable,
      intent(out) :: transformer`) and
      `get_signature(this) result(signature)` (`character(:),
      allocatable :: signature`, mirroring `Characteristic%get_signature`'s
      own role: a human-readable, value-distinguishing string). Add
      `use mapl_Transform_mod, only: Transform` to this module's `use`
      list (no `ComponentGraph`/`NodeId` import needed - confirm this
      stays true after adding the import).

## 2. Move the four real build_transform/get_signature implementations

- [x] 2.1 `PhysicalUnitsCharacteristic.F90`: add
      `build_transform => units_build_transform` and
      `get_signature => units_get_signature`, bodies moved verbatim from
      `UnitsCharacteristic%units_build_transform`/`units_get_signature`,
      reading `this%get_units()`/`goal%get_units()` (already exist) in
      place of the original's private-component access. `goal`'s
      `class(StateItemCharacteristic)` type (not `class(Characteristic)`)
      requires updating the `select type` branch's declared type
      accordingly.
- [x] 2.2 `TypeKindCharacteristic.F90`: add `build_transform =>
      typekind_build_transform` and `get_signature =>
      typekind_get_signature`, bodies moved from
      `TypeKindCharacteristicAdapter%adapter_build_transform`/
      `adapter_get_signature`. Move `label_to_typekind`/`typekind_to_label`
      helper functions from `TypeKindCharacteristicAdapter.F90` into this
      file (no other module needs them once the adapter is removed -
      confirm via grep before deleting the adapter in task 6).
- [x] 2.3 `VerticalCoordinateCharacteristic.F90`: add `dimensions`
      (`StringVector`) and `grid_id` (`character(:), allocatable`)
      components alongside the existing inherited `referenced_node_id`;
      add `get_grid_id`/`has_grid_id`/`get_dimensions` accessors (moved
      from `VerticalGridCharacteristic`); update the constructor
      (`new_VerticalCoordinateCharacteristic`) to accept optional
      `grid_id`/`dimensions` arguments alongside its existing
      `referenced_node_id` argument. Add `build_transform =>
      vcoord_build_transform` and `get_signature =>
      vcoord_get_signature`, bodies moved from
      `VerticalGridCharacteristic%vgrid_build_transform`/
      `vgrid_get_signature` (including `classify_dimension_overlap`/
      `DIMENSION_OVERLAP_SINGLE`/`DIMENSION_OVERLAP_INCOMPATIBLE`/
      `DIMENSION_OVERLAP_AMBIGUOUS`/`contains_name`/`same_set`/
      `sorted_names`/`join` helper functions - move all of them into
      this file, confirm no other module needs them before deleting the
      originals in task 6). `needs_extension_for`'s own existing
      node-identity comparison is unchanged - do not merge it with the
      new dimension-overlap logic.
- [x] 2.4 `GeometryCharacteristic.F90`: add `build_transform =>
      geometry_build_transform` (always `_FAIL`s - body moved from
      `GeomCharacteristic%geom_build_transform`, no real logic to
      adapt) and `get_signature => geometry_get_signature` (moved from
      `GeomCharacteristic%geom_get_signature`, reading
      `this%get_referenced_node_id()%to_string()` or equivalent -
      `GeomCharacteristic` stored its own `geom_id` string directly;
      confirm whether `GeometryCharacteristic`'s existing
      `referenced_node_id` can serve the same signature role or whether
      it also needs its own stored `geom_id` string component, mirroring
      task 2.3's precedent for `VerticalCoordinateCharacteristic` - check
      `GraphBuilder_Geometry_smod.F90`'s own `GeomCharacteristic(trim(id_buffer))`
      call sites (lines ~163/165/267/269) for what string value must be
      preserved).

## 3. Give the six provider-less subclasses explicit-failure build_transform and get_signature

- [x] 3.1 `AttributesCharacteristic.F90`: add `build_transform` that
      `_FAIL`s with a message naming the subclass (mirrors
      `GeomCharacteristic`'s/`VerticalGridCharacteristic`'s own `_FAIL`
      message shape); add `get_signature` deriving a stable string from
      `this%attribute_names` (e.g. sorted, comma-joined names - reuse
      `VerticalCoordinateCharacteristic`'s moved `sorted_names`/`join`
      helpers if visible, or a local equivalent). Update the module
      header's "No build_transform/adaptation method exists at this
      layer" comment to describe the new deferred-but-failing method
      instead of its absence.
- [x] 3.2 `ConservationCharacteristic.F90`: same pattern as 3.1 -
      `_FAIL`ing `build_transform`, `get_signature` derived from its own
      stored value, header comment updated.
- [x] 3.3 `NormalizationCharacteristic.F90`: same pattern.
- [x] 3.4 `QuantityTypeCharacteristic.F90`: same pattern.
- [x] 3.5 `StandardNameCharacteristic.F90`: same pattern.
- [x] 3.6 `UngriddedDimsCharacteristic.F90`: same pattern.

## 4. Re-type ExtensionResolution.F90

- [x] 4.1 In `ExtensionResolution.F90`, replace
      `use mapl_Characteristic_mod, only: Characteristic, CharacteristicMap, ...`
      with
      `use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, StateItemCharacteristicMap, ...`
      and `use mapl_CharacteristicId_mod` with
      `use mapl_StateItemCharacteristicKind_mod`. Update every dummy
      argument/local variable type
      (`CharacteristicMap`->`StateItemCharacteristicMap`,
      `CharacteristicId`->`StateItemCharacteristicKind`,
      `class(Characteristic)`->`class(StateItemCharacteristic)`) in
      `find_mismatched_characteristics`, `find_or_build_extension_chain`,
      `chain_key`, `build_chain`.
- [x] 4.2 Replace `find_mismatched_characteristics`'s own body with a
      thin forwarding call to
      `find_mismatched_state_item_characteristics`
      (`mapl_StateItemCharacteristicDetection_mod`) - keep the exported
      name `find_mismatched_characteristics` unchanged (design.md
      Decisions) so no call site needs updating for this rename;
      confirm the two functions' argument/result shapes already align
      (both take two map-bearing values and return an array of mismatched
      keys) or adapt the thin wrapper accordingly.
- [x] 4.3 Update `chain_key`'s reuse-key construction to call
      `goal%get_signature()` on the now-`class(StateItemCharacteristic)`
      value (unchanged call shape, new deferred method per task 1.1).
- [x] 4.4 Update `build_chain`'s one `build_transform` call site to the
      new four-argument form (already correct post-
      `decouple-transform-from-graph` for the dropped graph/NodeId
      arguments - this task only updates the dummy types flowing into
      it, per 4.1).

## 5. Re-type every GraphBuilder production call site

- [x] 5.1 In `GraphBuilder.F90`: replace
      `use mapl_Characteristic_mod, only: CharacteristicMap` and
      `use mapl_CharacteristicId_mod, only: CharacteristicId, ...` with
      the `StateItemCharacteristic`-side equivalents. Update
      `build_characteristics`'s own result type
      (`type(CharacteristicMap) :: characteristics` ->
      `type(StateItemCharacteristicMap)`) and every
      `characteristics%insert(...)` call to use the corresponding
      `StateItemCharacteristicKind` constant and
      `StateItemCharacteristic` subclass constructor
      (`UNITS_CHARACTERISTIC_ID`->`PHYSICAL_UNITS_CHARACTERISTIC_KIND`,
      `UnitsCharacteristic(...)`->`PhysicalUnitsCharacteristic(...)`, and
      likewise for the typekind/vertical-grid insertions). Update
      `merged_var_spec_for_link`'s own `CharacteristicId`-typed dummy
      argument and its three-way kind comparison
      (`UNITS_CHARACTERISTIC_ID`/`TYPE_KIND_CHARACTERISTIC_ID`/
      `VERTICAL_GRID_CHARACTERISTIC_ID`) to the
      `StateItemCharacteristicKind` equivalents.
- [x] 5.2 In `GraphBuilder_ConnectionResolution_smod.F90`: update the
      `use` list (`mapl_CharacteristicId_mod`->
      `mapl_StateItemCharacteristicKind_mod`,
      `mapl_TypeKindCharacteristicAdapter_mod`->
      `mapl_TypeKindCharacteristic_mod`, `mapl_UnitsCharacteristic_mod`->
      `mapl_PhysicalUnitsCharacteristic_mod`,
      `mapl_VerticalGridCharacteristic_mod`->
      `mapl_VerticalCoordinateCharacteristic_mod`). Update
      `resolve_one`'s/`resolve_inout_destination`'s local
      `CharacteristicMap`/`CharacteristicId` declarations to
      `StateItemCharacteristicMap`/`StateItemCharacteristicKind`.
- [x] 5.3 In `GraphBuilder_Geometry_smod.F90`: update the `use` list
      (`mapl_CharacteristicId_mod`->`mapl_StateItemCharacteristicKind_mod`,
      `mapl_GeomCharacteristic_mod`->`mapl_GeometryCharacteristic_mod`).
      Update both procedures' local `CharacteristicMap` declarations and
      `GeomCharacteristic(trim(id_buffer))`/`GEOM_CHARACTERISTIC_ID`
      construction calls to `GeometryCharacteristic(...)`/
      `GEOMETRY_CHARACTERISTIC_KIND` - resolve task 2.4's own open
      question about what `GeometryCharacteristic` needs to store before
      finishing this task, since these are the real call sites that
      determine it.
- [x] 5.4 In `GraphBuilder_CallbackWiring_smod.F90`: update the `use`
      list (`mapl_CharacteristicId_mod`->
      `mapl_StateItemCharacteristicKind_mod`) and the local
      `empty_characteristics`/`CharacteristicMap` declaration to
      `StateItemCharacteristicMap`.

## 6. Remove the retired Characteristic-side modules

- [x] 6.1 Grep-confirm (not just by memory) zero remaining references
      to `mapl_Characteristic_mod`, `mapl_CharacteristicId_mod`,
      `mapl_UnitsCharacteristic_mod`,
      `mapl_TypeKindCharacteristicAdapter_mod`,
      `mapl_VerticalGridCharacteristic_mod`,
      `mapl_GeomCharacteristic_mod` anywhere in `superstructure/`
      outside the six files being deleted in this task group and their
      own dedicated test files.
- [x] 6.2 Delete `Characteristic.F90`, `CharacteristicId.F90`,
      `UnitsCharacteristic.F90`, `TypeKindCharacteristicAdapter.F90`,
      `VerticalGridCharacteristic.F90`, `GeomCharacteristic.F90`. Remove
      their entries from `superstructure/generic/graph/CMakeLists.txt`.
- [x] 6.3 Delete `Test_Characteristic.pf`, `Test_GeomCharacteristic.pf`.
      Confirm every assertion either file made is already covered by an
      existing or newly-added (task 7) `StateItemCharacteristic`-side
      test before deleting - port any assertion with no existing
      equivalent instead of silently dropping coverage. Remove their
      entries from `superstructure/generic/graph/tests/CMakeLists.txt`.
- [x] 6.4 Update `Test_ExtensionResolution.pf`'s own `use` list and
      every `CharacteristicMap`/`CharacteristicId`/`class(Characteristic)`
      reference (including its local `FakeCharacteristic` test double's
      own `extends(Characteristic)`/`class(Characteristic), intent(in)
      :: goal` declarations and `build_transform`'s now-four-argument
      signature) to the `StateItemCharacteristic`-side equivalents.
      `FakeCharacteristic` also needs a new `get_signature`
      implementation (task 1.1's new deferred method) - a simple
      `'mock=' // this%value` mirrors its existing `get_signature`-style
      naming already used elsewhere in this hierarchy.

## 7. New test coverage

- [x] 7.1 Add a `build_transform` test to `Test_StateItemCharacteristicSubclasses.pf`
      for each of the six provider-less subclasses
      (`AttributesCharacteristic`/`ConservationCharacteristic`/
      `NormalizationCharacteristic`/`QuantityTypeCharacteristic`/
      `StandardNameCharacteristic`/`UngriddedDimsCharacteristic`),
      confirming each fails explicitly (mirrors
      `Test_GeomCharacteristic.pf`'s own
      `test_geom_characteristic_build_transform_fails_explicitly`
      precedent, before that file is deleted in task 6.3).
- [x] 7.2 Add a `get_signature` test for each of the same six
      subclasses, confirming two equal-value instances produce
      identical signatures and two different-value instances produce
      different ones (design.md Risks - sufficient for reuse-key
      correctness, the only property `chain_key` depends on).
- [x] 7.3 Add `build_transform`/`get_signature` tests to
      `Test_StateItemCharacteristicSubclasses.pf` for
      `PhysicalUnitsCharacteristic`/`TypeKindCharacteristic`/
      `VerticalCoordinateCharacteristic`/`GeometryCharacteristic`
      confirming the moved real implementations produce the same
      outcomes `Test_Characteristic.pf`/`Test_GeomCharacteristic.pf`
      already asserted before deletion (port the specific assertions,
      not just a smoke test).

## 8. Verification

- [x] 8.1 Build MAPL with NAG (`nag` build dir); run `cmake --build nag
      -j 8` and `build-tests`; confirm no new build regressions.
- [x] 8.2 Run every test file touched or added in task groups 6-7
      directly with `-v`; confirm every existing assertion passes
      unchanged and every new assertion passes.
- [x] 8.3 Run full `ctest` for the `MAPL.generic.*` label set; confirm
      no regression outside the same pre-existing, unrelated
      environment gaps earlier changes' own tasks.md files already
      documented.
- [x] 8.4 `openspec validate merge-characteristic-families --strict`
      reports the change valid.
