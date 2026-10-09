## 1. StateItemCharacteristicKind registration

- [x] 1.1 Add seven new named parameter constants to
      `superstructure/generic/graph/StateItemCharacteristicKind.F90`
      (design.md D7, values 4–10 in table order):
      `VERTICAL_COORDINATE_CHARACTERISTIC_KIND`,
      `ATTRIBUTES_CHARACTERISTIC_KIND`, `UNGRIDDED_DIMS_CHARACTERISTIC_KIND`,
      `QUANTITY_TYPE_CHARACTERISTIC_KIND`, `CONSERVATION_CHARACTERISTIC_KIND`,
      `NORMALIZATION_CHARACTERISTIC_KIND`, `STANDARD_NAME_CHARACTERISTIC_KIND`.
      Add each to the module's `public ::` list and to `to_string()`'s
      `select case`. Leave `INVALID_CHARACTERISTIC_KIND` (-1) and the
      test-only `MOCK_CHARACTERISTIC_KIND` (99) unchanged.
- [x] 1.2 Add pFUnit cases to `Test_StateItemCharacteristicKind.pf`: each new
      constant has a stable, distinct value (including distinct from the
      three already registered by 5a) and a correct `to_string()`.

## 2. VerticalCoordinateCharacteristic (ReferenceCharacteristic)

- [x] 2.1 Create `superstructure/generic/graph/VerticalCoordinateCharacteristic.F90`
      (design.md D1): extends `ReferenceCharacteristic`; mirrors
      `GeometryCharacteristic.F90`'s shape exactly — constructor takes
      `referenced_node_id`, `get_kind()` returns
      `VERTICAL_COORDINATE_CHARACTERISTIC_KIND`, `needs_extension_for`
      compares referenced `NodeId` equality via the inherited
      `get_referenced_node_id()` accessor, `class default` branch is
      `error stop` (same precedent as `GeometryCharacteristic`/
      `PhysicalUnitsCharacteristic`).
- [x] 2.2 Add the new file to
      `superstructure/generic/graph/CMakeLists.txt`'s `srcs` list.
- [x] 2.3 Add pFUnit cases (new or appended to `Test_StateItemCharacteristic.pf`):
      a constructed instance is a `ReferenceCharacteristic` and holds the
      constructor's `NodeId` (spec "A vertical-grid characteristic is a
      reference characteristic"); two instances constructed with the same
      `NodeId` compare as not needing extension, two with different
      `NodeId`s do (spec "Two state items share one vertical-grid
      reference").

## 3. Value characteristics reusing an existing, already-decoupled value type

- [x] 3.1 Create `superstructure/generic/graph/UngriddedDimsCharacteristic.F90`
      (design.md D3): extends `ValueCharacteristic`; stores one
      `type(UngriddedDims), allocatable` field (reuses
      `mapl_UngriddedDims_mod`, `infrastructure/esmf/UngriddedDims.F90`,
      as-is); `get_kind()` returns `UNGRIDDED_DIMS_CHARACTERISTIC_KIND`;
      `needs_extension_for` is a single `/=` call on the stored
      `UngriddedDims` values; `class default` is `error stop`.
- [x] 3.2 Create `superstructure/generic/graph/ConservationCharacteristic.F90`:
      extends `ValueCharacteristic`; stores one `type(ConservationMetadata)`
      field (reuses `mapl_ConservationMetadata_mod`,
      `enums/ConservationMetadata.F90`, as-is, including its own
      mirror-aware `operator(==)`); `get_kind()` returns
      `CONSERVATION_CHARACTERISTIC_KIND`; `needs_extension_for` is a single
      `/=` call; `class default` is `error stop`.
- [x] 3.3 Create `superstructure/generic/graph/NormalizationCharacteristic.F90`:
      extends `ValueCharacteristic`; stores one `type(NormalizationMetadata)`
      field (reuses `mapl_NormalizationMetadata_mod`,
      `enums/NormalizationMetadata.F90`, as-is); `get_kind()` returns
      `NORMALIZATION_CHARACTERISTIC_KIND`; `needs_extension_for` is a single
      `/=` call; `class default` is `error stop`.
- [x] 3.4 Add the three new files to
      `superstructure/generic/graph/CMakeLists.txt`'s `srcs` list.
- [x] 3.5 Add pFUnit cases for each of the three (new file or appended to
      `Test_StateItemCharacteristic.pf`): constructed instance is a
      `ValueCharacteristic` with the correct `get_kind()`; two instances
      with equal wrapped values do not need extension; two with unequal
      wrapped values do; for `ConservationCharacteristic`/
      `NormalizationCharacteristic` specifically, two mirror-constructed
      instances (via the wrapped type's own mirror constructor) do not need
      extension, exercising the reused type's own mirror-aware `operator(==)`
      through this new subclass (spec "Attributes, ungridded-dimensions,
      quantity-type, conservation, normalization, and standard-name
      characteristics are value characteristics").

## 4. QuantityTypeCharacteristic (partial field reuse)

- [x] 4.1 Create `superstructure/generic/graph/QuantityTypeCharacteristic.F90`
      (design.md D4): extends `ValueCharacteristic`; stores
      `quantity_type` (`MAPL_QuantityType`) and `basis`
      (`MAPL_MixingRatioBasis`) only (both from `mapl_enums_api`, no
      `dimensions`/`molecular_weight`); `get_kind()` returns
      `QUANTITY_TYPE_CHARACTERISTIC_KIND`; `needs_extension_for` ports
      `QuantityTypeAspect%matches()`'s own logic verbatim
      (`.not. ((quantity_type equal or either unknown) .and. (basis equal
      or either none))`); `class default` is `error stop`.
- [x] 4.2 Add the new file to
      `superstructure/generic/graph/CMakeLists.txt`'s `srcs` list.
- [x] 4.3 Add pFUnit cases: matching quantity_type+basis does not need
      extension; either side `MAPL_QUANTITY_UNKNOWN` does not need
      extension regardless of the other side's value; either side
      `MAPL_BASIS_NONE` does not need extension regardless of the other
      side's basis; a genuine quantity_type or basis mismatch (neither side
      unknown/none) does need extension.

## 5. AttributesCharacteristic (asymmetric comparison)

- [x] 5.1 Create `superstructure/generic/graph/AttributesCharacteristic.F90`
      (design.md D2): extends `ValueCharacteristic`; stores one
      `type(StringVector)` of attribute names; `get_kind()` returns
      `ATTRIBUTES_CHARACTERISTIC_KIND`; `needs_extension_for(this, goal)`
      ports `AttributesAspect%matches()`'s asymmetric "src provides every
      name dst requires" check — `needs_extension = .not. (every name in
      goal's set is present in this's set)`; document the asymmetry
      explicitly in the module's own header comment (this is the one
      asymmetric characteristic in the hierarchy); `class default` is
      `error stop`.
- [x] 5.2 Add the new file to
      `superstructure/generic/graph/CMakeLists.txt`'s `srcs` list.
- [x] 5.3 Add pFUnit cases exercising both call directions explicitly
      (design.md Risks): `this` providing a superset of `goal`'s required
      names does not need extension; `this` missing one of `goal`'s
      required names does need extension; swapping `this`/`goal` for the
      same two non-equal sets produces the opposite result in at least one
      constructed case, making the asymmetry visible in test output.

## 6. StandardNameCharacteristic (status-based escape hatch, not full legacy parity)

- [x] 6.1 Create `superstructure/generic/graph/StandardNameCharacteristic.F90`
      (design.md D5): extends `ValueCharacteristic`; stores one
      `standard_name` string; `get_kind()` returns
      `STANDARD_NAME_CHARACTERISTIC_KIND`; `needs_extension_for(this, goal)`:
      if either `this%get_status()` or `goal%get_status()` is
      `CHARACTERISTIC_STATUS_UNCHECKED`, `needs_extension = .false.`;
      otherwise `needs_extension = (this%standard_name /= goal%standard_name)`.
      No `ValidationMode`/`FieldDictionaryConfig`/`pflogger` dependency.
      Document the scope boundary (no severity grading, no logging) in the
      module's own header comment. `class default` is `error stop`.
- [x] 6.2 Add the new file to
      `superstructure/generic/graph/CMakeLists.txt`'s `srcs` list.
- [x] 6.3 Add pFUnit cases: equal standard names do not need extension;
      unequal standard names do; either side's status set to `UNCHECKED`
      (via the inherited `set_status`) means no extension is needed even
      when the names differ.

## 7. Build and verification

- [x] 7.1 Build MAPL with the NAG compiler (`module load nag/7.2.41 mpi
      baselibs` before any `cmake`/`make`/`ctest` invocation, per user
      instruction) and confirm a clean build with all ten new source files
      included. Note: `&` continuation is not supported inside FPP macro
      invocations by gfortran — keep every macro call (`_RC`, `_RETURN`,
      `_FAIL`, `_UNUSED_DUMMY`, etc.) on a single line in all new source,
      even though this build uses NAG, for cross-compiler portability.
      Confirmed: full `make` (all targets) completed cleanly; none of the
      seven new modules use any MAPL.h error-handling macro at all (plain
      comparison functions only), so the constraint is satisfied by
      construction - verified by grep.
- [x] 7.2 Run all new/updated pFUnit suites (task groups 1–6) directly with
      `-v` and confirm all pass.
      Confirmed: `ctest -R "^MAPL.generic.graph$" -V` - 259 tests, all
      passed (233 pre-existing + 26 new, including the full new
      Test_StateItemCharacteristicSubclasses suite and the two new
      Test_StateItemCharacteristicKind cases).
- [x] 7.3 Run the full `MAPL.generic.*` `ctest` label set and confirm no
      regressions relative to the pre-change baseline.
      Confirmed: `ctest -R "^MAPL.generic"` - 7/7 tests passed (graph,
      scenarios, transforms, vertical, aspects, components, core).
- [x] 7.4 Run `openspec validate state-item-characteristic-subclasses --strict`
      and confirm the change is valid.
      Confirmed: "Change 'state-item-characteristic-subclasses' is valid".
