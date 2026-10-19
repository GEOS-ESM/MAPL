## 1. VariableSpec: three new fields (design.md D2)

- [x] 1.1 Add `quantity_type` field to `superstructure/generic/specs/VariableSpec.F90`:
      `type(MAPL_QuantityType) :: quantity_type = QUANTITY_UNKNOWN`, using the
      already-imported `mapl_QuantityType_mod`/`mapl_enums_api` types. Plain
      field, no `make_VariableSpec` keyword, matching `callback_interface_id`'s
      own header-comment precedent.
- [x] 1.2 Add `conservation` field: `type(ConservationMetadata) ::
      conservation`, using `mapl_ConservationMetadata_mod` (already `use`d
      elsewhere in this file via `ConservationAspect`). Default-constructed
      (no explicit initializer needed - `ConservationMetadata`'s own default
      constructor already yields `CONSERVE_NONE`/non-conservable/non-mirror).
- [x] 1.3 Add `normalization` field: `type(NormalizationMetadata) ::
      normalization`, using `mapl_NormalizationMetadata_mod`. Default-
      constructed (yields `NORMALIZE_NONE`/scale 1.0/non-mirror).
- [x] 1.4 Add a `use` statement for `mapl_ConservationMetadata_mod`/
      `mapl_NormalizationMetadata_mod` if not already present in
      `VariableSpec.F90` (confirm by grep before adding - the file already
      `use`s `mapl_ConservationAspect_mod`/`mapl_NormalizationAspect_mod`,
      which may or may not re-export the metadata types).
- [x] 1.5 Add a short header comment block for the three new fields mirroring
      `callback_interface_id`'s own documentation style (design.md D2):
      "mark the item, not a new itemType... plain field assignment only, no
      make_VariableSpec keyword... no ComponentSpecParser/YAML entry point
      exists yet."

## 2. StateItemCharacteristicKind reuse (no new kinds needed)

- [x] 2.1 Confirm by reading
      `superstructure/generic/graph/characteristics/StateItemCharacteristicKind.F90`
      that `ATTRIBUTES_CHARACTERISTIC_KIND`/`UNGRIDDED_DIMS_CHARACTERISTIC_KIND`/
      `QUANTITY_TYPE_CHARACTERISTIC_KIND`/`CONSERVATION_CHARACTERISTIC_KIND`/
      `NORMALIZATION_CHARACTERISTIC_KIND`/`STANDARD_NAME_CHARACTERISTIC_KIND`
      are already registered (values 5-10, landed by 5a2) - no new kind
      registration needed in this change.

## 3. GraphBuilder_Advertising_smod.F90: populate_characteristics wiring (design.md D1)

- [x] 3.1 Add `use` statements for the six kind constants (above) and the six
      `*Characteristic` constructors (`AttributesCharacteristic`,
      `UngriddedDimsCharacteristic`, `StandardNameCharacteristic`,
      `QuantityTypeCharacteristic`, `ConservationCharacteristic`,
      `NormalizationCharacteristic`), alongside the existing
      `PhysicalUnitsCharacteristic`/`TypeKindCharacteristic`/
      `VerticalCoordinateCharacteristic` imports at the top of
      `GraphBuilder_Advertising_smod.F90`.
- [x] 3.2 In `populate_characteristics`, insert `StandardNameCharacteristic`
      gated on `allocated(var_spec%standard_name)`, mirroring the existing
      `allocated(var_spec%units)` branch exactly.
- [x] 3.3 Insert `AttributesCharacteristic` gated on
      `var_spec%attributes%size() > 0`, constructed from
      `var_spec%attributes` directly.
- [x] 3.4 Insert `UngriddedDimsCharacteristic` unconditionally, constructed
      from `var_spec%ungridded_dims` (non-allocatable, always has a value).
- [x] 3.5 Insert `QuantityTypeCharacteristic` unconditionally, constructed
      from `var_spec%quantity_type` (new field from Section 1).
- [x] 3.6 Insert `ConservationCharacteristic` unconditionally, constructed
      from `var_spec%conservation` (new field from Section 1).
- [x] 3.7 Insert `NormalizationCharacteristic` unconditionally, constructed
      from `var_spec%normalization` (new field from Section 1).
- [x] 3.8 Add a short comment at the top of each new insertion block (or one
      shared comment above the whole group) citing this change and
      design.md D1 for the gating-pattern rationale, matching this
      submodule's existing per-branch comment density.

## 4. GraphBuilder_ConnectionResolution_smod.F90: merged_item_for_link (design.md D3)

- [x] 4.1 Add `use` statements for the six new
      `*_CHARACTERISTIC_KIND` constants in
      `GraphBuilder_ConnectionResolution_smod.F90` (alongside the existing
      `PHYSICAL_UNITS_CHARACTERISTIC_KIND`/`TYPE_KIND_CHARACTERISTIC_KIND`/
      `VERTICAL_COORDINATE_CHARACTERISTIC_KIND` imports used by
      `merged_item_for_link`).
- [x] 4.2 In `merged_item_for_link`'s `do j = 1, i` loop, add six new
      `else if (mismatched_kinds(j) == ...)` branches (one per new kind),
      each a comment-only no-op matching the existing
      `VERTICAL_COORDINATE_CHARACTERISTIC_KIND` branch's own pattern and
      rationale ("no real build_transform exists, so this branch is
      unreachable in practice, but is enumerated explicitly rather than
      falling into the _FAIL default below").
- [x] 4.3 Confirm the final `else` (`_FAIL` on unrecognized kind) is
      unchanged and still reached only for a genuinely-unregistered kind.

## 5. Fixture: multi-axis mismatch (design.md D4)

- [x] 5.1 Create
      `superstructure/generic/tests/graph-scenarios/standard_name_and_ungridded_dims_mismatch/`
      with `cap.yaml`+`comp_src.yaml`+`comp_dst.yaml`, following
      `units_and_typekind_mismatch/`'s exact flat, `all_unsatisfied`-
      connection convention. (Renamed from the originally-planned
      `standard_name_and_quantity_type_mismatch`: `quantity_type` is
      YAML-unreachable - see 5.2 - so `ungridded_dims` is the second axis
      instead, established and compared on both sides as a live
      sanity-check of that axis's own unconditional insertion, matching
      rather than mismatching - see 5.3.)
- [x] 5.2 Confirmed `quantity_type`/`conservation`/`normalization`/
      `attributes` have no `ComponentSpecParser`/YAML wiring (Section 1's
      own plain-field-only scope) - YAML-unreachable, so excluded from
      this fixture. `standard_name` and `ungridded_dims` are both
      YAML-reachable; used instead.
- [x] 5.3 `comp_src.yaml`: export declares `standard_name: air_temperature`
      and one ungridded dimension (`foo1`, extent 3). `comp_dst.yaml`:
      import declares a different `standard_name: surface_temperature`
      and the SAME ungridded dimension (`foo1`, extent 3 - matching, not
      mismatching). Deliberate: an initial draft mismatched both axes,
      but legacy's `UngriddedDimsAspect%can_connect_to` rejects any
      ungridded-dims disagreement unconditionally (independent of
      `ValidationMode`), which hard-fails `process_connections()` before
      `GraphBuilder`'s own resolution ever runs - unlike `standard_name`,
      which legacy only rejects under strict `ValidationMode` and can
      therefore be exercised permissively. See fixture `description.md`.
- [x] 5.4 Confirmed via both a synthetic-node unit test
      (`Test_GraphBuilder_ConnectionResolution.pf`'s
      `test_standard_name_mismatch_fails_explicitly`) and a real YAML/
      init-phase pipeline test exercising this exact fixture
      (`Test_GraphModeDemonstration.pf`'s
      `test_graph_mode_leaves_newly_wired_axis_mismatch_unwired`):
      resolution detects the `STANDARD_NAME` mismatch and reports it
      through `unsupported_characteristics` with that exact reason
      string - not a silent pass-through, not an unrelated assertion
      failure. Both tests pass; `MAPL.generic` ctest suite (8/8) has zero
      regressions.
- [x] 5.5 Recorded the expected failure behavior (including the
      ungridded-dims-must-match deliberate-scope note above) in
      `description.md`, mirroring `units_and_typekind_mismatch`'s own
      precedent.

## 6. Unit coverage: matched declarations do not falsely mismatch

- [x] 6.1 Add pFUnit cases (new file or appended to `Test_GraphBuilder.pf`)
      exercising `populate_characteristics`'s real call path directly:
      two `VariableSpec`s with equal `attributes`/`ungridded_dims`/
      `standard_name`/`quantity_type`/`conservation`/`normalization` values
      produce `GraphStateItem`s whose characteristics do not mismatch
      (`find_mismatched_characteristics` returns an empty array) for each
      axis individually.
- [x] 6.2 Add a case confirming an item with none of the newly-wired
      optional fields set (default `VariableSpec`) still advertises
      successfully and produces no spurious characteristic entries beyond
      what Section 3's unconditional-insert axes always add.
- [x] 6.3 Add a case specifically for `AttributesCharacteristic`'s
      asymmetric comparison (design.md Risks): an export item whose
      declared attributes are a superset of the import's required
      attributes does not mismatch; an export item missing one of the
      import's required attributes does mismatch.

## 7. Regression and verification

- [x] 7.1 Build MAPL (NAG, per the active compiler-switching skill) with
      all modified/new files included - clean build, zero warnings/errors
      introduced by this change's own files.
- [x] 7.2 Ran `ctest -R "^MAPL.generic.graph$"` - passed, 0 failures.
- [x] 7.3 Ran `ctest -R "^MAPL.generic"` (the full `MAPL.generic.*` label
      set, including `MAPL.generic.scenarios`) - 8/8 suites passed, zero
      regressions. No existing real-YAML fixture reports a newly-detected
      mismatch on any of the six axes that previously went undetected.
- [x] 7.4 Ran the new fixture from Section 5 via a direct
      `Test_GraphModeDemonstration.pf` scenario test
      (`test_graph_mode_leaves_newly_wired_axis_mismatch_unwired`, real
      YAML/init-phase pipeline) plus a synthetic-node unit test
      (`Test_GraphBuilder_ConnectionResolution.pf`'s
      `test_standard_name_mismatch_fails_explicitly`) - both confirm the
      expected explicit `STANDARD_NAME` unsupported-characteristic
      failure.
- [x] 7.5 Ran `openspec validate phase-12-characteristic-dispatch-wiring
      --strict` - change is valid.
