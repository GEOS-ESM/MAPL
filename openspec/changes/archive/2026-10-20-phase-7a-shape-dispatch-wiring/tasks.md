## 1. StateItemCharacteristicKind: register a new kind (design.md D1/D2)

- [x] 1.1 Add `ITEM_SHAPE_CHARACTERISTIC_KIND = StateItemCharacteristicKind(11)` to
      `superstructure/generic/graph/characteristics/StateItemCharacteristicKind.F90`
      (next unused value after `STANDARD_NAME_CHARACTERISTIC_KIND = 10`),
      with its own `public ::` declaration and a `to_string()` case
      returning `"ITEM_SHAPE"`.
- [x] 1.2 Add a short comment next to the new parameter citing this
      change, matching the existing "Registered by
      openspec/changes/state-item-characteristic-subclasses" comment
      style for the 4-10 block.

## 2. `ItemShapeCharacteristic`: the new value characteristic (design.md D1-D3)

- [x] 2.1 Create
      `superstructure/generic/graph/characteristics/ItemShapeCharacteristic.F90`,
      module `mapl_ItemShapeCharacteristic_mod`, type
      `ItemShapeCharacteristic` extending `ValueCharacteristic`
      (`mapl_StateItemCharacteristic_mod`), holding one private
      `type(ESMF_StateItem_Flag) :: item_class` component, following
      `PhysicalUnitsCharacteristic.F90`'s exact file structure (header
      comment, constructor interface, `get_kind`/`needs_extension_for`/
      `build_transform`/`get_signature`, plus a `get_item_class`
      accessor).
- [x] 2.2 `new_ItemShapeCharacteristic(item_class)` constructor taking a
      `type(ESMF_StateItem_Flag)`.
- [x] 2.3 `item_shape_get_kind` (nopass) returns
      `ITEM_SHAPE_CHARACTERISTIC_KIND`.
- [x] 2.4 `item_shape_needs_extension_for(this, goal)`: `select type
      (goal); class is (ItemShapeCharacteristic)` compares
      `this%item_class == goal%item_class` (ESMF `operator(==)` on
      `ESMF_StateItem_Flag`); mismatch when unequal. `class default`
      -> `error stop`, mirroring `units_needs_extension_for`'s own
      precedent exactly (design.md D2).
- [x] 2.5 `item_shape_build_transform(this, goal, transformer, rc)`
      (design.md D3): `select type (goal); class is
      (ItemShapeCharacteristic)`, then check `this%item_class ==
      MAPL_STATEITEM_BRACKET .and. goal%item_class ==
      MAPL_STATEITEM_FIELD`; on match, `allocate(transformer,
      source=BracketTimeInterpolationTransform())` (Section 3) and
      `_RETURN(_SUCCESS)`; every other pairing (including
      `VECTORBRACKET`->`VECTOR`) falls through to an explicit `_FAIL`
      naming both sides' item classes (via `MAPL_StateItem_Flag`'s own
      `to_string`-equivalent, or `%ot` if no string accessor exists -
      confirm during implementation). `class default` on `goal` itself
      -> `_FAIL`, matching `units_build_transform`'s own precedent (not
      `error stop` - `build_transform` is `rc`-based, per
      `StateItemCharacteristic.F90`'s own interface contract).
      Completion note: `ESMF_StateItem_Flag` has no `to_string`-
      equivalent method (confirmed by reading ESMF_StateItem.F90 -
      only a plain integer `%ot` component), so the message renders
      via a local `item_class_name`/`%ot` select-case helper, mirroring
      `GraphBuilder.F90`'s own `itemtype_name` precedent exactly (kept
      as an independent local copy, not a cross-module reference, since
      this module sits below `GraphBuilder.F90` in the dependency
      layering).
- [x] 2.6 `item_shape_get_signature(this)` returns `'item_class=' //
      <rendered item_class>` (reuse whatever rendering Section 2.5
      settled on).
- [x] 2.7 Add `characteristics/ItemShapeCharacteristic.F90` to
      `superstructure/generic/graph/CMakeLists.txt`, immediately after
      `characteristics/StandardNameCharacteristic.F90`.

## 3. `BracketTimeInterpolationTransform`: the new graph-native Transform (design.md D4)

- [x] 3.1 Create
      `superstructure/generic/graph/transforms/BracketTimeInterpolationTransform.F90`,
      module `mapl_BracketTimeInterpolationTransform_mod`, type
      `BracketTimeInterpolationTransform` extending `Transform`
      (`mapl_Transform_mod`), following `UnitsConverterTransform.F90`'s
      exact file structure (header comment explaining independence from
      legacy `TimeInterpolateTransform`/`ExtensionTransform`, per
      design.md D4; `INPUT_PORT_NAME = 'source_field'`/`OUTPUT_PORT_NAME
      = 'destination_field'` module parameters; a parameterless
      constructor that calls `declare_input_port`/`declare_output_port`).
- [x] 3.2 `compute(this, inputs, outputs, rc)`: read
      `inputs%at(INPUT_PORT_NAME)` and assert `associated`, matching
      `units_compute`'s own pattern; call
      `input_item%get_field_bundle(_RC)` (NOT `get_field` - design.md
      D4's explicit note that this port semantically holds a bundle) to
      get the bracket's `ESMF_FieldBundle`; read
      `outputs%at(OUTPUT_PORT_NAME)%get_field(_RC)` for the destination
      field.
- [x] 3.3 Call `MAPL_FieldBundleGet(bundle, fieldList=field_list,
      interpolation_weights=weights, _RC)` (`mapl_field_bundle_api`),
      the same call `TimeInterpolateTransform.F90`'s `run_r4`/`run_r8`
      already use.
- [x] 3.4 `ESMF_FieldGet(field_list(1), typekind=typekind, _RC)`; branch
      on `ESMF_TYPEKIND_R4`/`ESMF_TYPEKIND_R8` into two private
      subroutines `interpolate_r4`/`interpolate_r8`, each reusing
      `assign_fptr` (`mapl_FieldPointerUtilities_mod`, already used by
      `UnitsConverterTransform.F90`) to get real pointers and
      re-implementing `TimeInterpolateTransform.F90`'s own `run_r4`/
      `run_r8` arithmetic (`y = weights(1); do i = 1,
      size(fieldList); y = y + weights(i+1)*xi; end do`, with the same
      `MAPL_UNDEFINED_REAL`/`MAPL_UNDEFINED_REAL64` masking) verbatim,
      against `field_list`/`weights`/the destination field directly (no
      `ESMF_State` arguments). Any typekind other than R4/R8 -> `_FAIL`.
- [x] 3.5 Add `transforms/BracketTimeInterpolationTransform.F90` to
      `superstructure/generic/graph/CMakeLists.txt`, immediately after
      `transforms/TypeKindConverterTransform.F90`.

## 4. `GraphBuilder_Advertising_smod.F90`: insert the new characteristic (design.md D1, proposal.md)

- [x] 4.1 Add `use` statements for `ITEM_SHAPE_CHARACTERISTIC_KIND`
      (`mapl_StateItemCharacteristicKind_mod`) and
      `ItemShapeCharacteristic` (`mapl_ItemShapeCharacteristic_mod`) in
      `GraphBuilder_Advertising_smod.F90`, alongside the existing
      characteristic imports.
- [x] 4.2 In `populate_characteristics`, insert
      `ItemShapeCharacteristic(var_spec%itemType)` unconditionally
      (same "every `VariableSpec` always has an itemType, default
      `MAPL_STATEITEM_FIELD`" rationale `TYPE_KIND_CHARACTERISTIC_KIND`'s
      own unconditional insert already uses - no `allocated()` gate
      needed), immediately after the existing
      `set_declared_item_type(var_spec%itemType)` call so the
      duplication design.md D1 accepts is visually adjacent.
      Completion note (regression found running the full
      `MAPL.generic` suite, Section 10): a truly unconditional insert
      broke the `expression`/`expression_match`/`expression_defer_geom`
      scenarios - an `EXPRESSION`-typed export connected to a `FIELD`
      import is resolved entirely outside `find_mismatched_
      characteristics`/`build_chain` (via `ExpressionClassAspect%
      make_transform`/`EvalTransform`, legacy `StateRegistry`-based),
      but the new characteristic made it report a spurious ITEM_SHAPE
      mismatch (`EXPRESSION /= FIELD`) that `build_transform` correctly
      `_FAIL`s (D3), turning a real, previously-working connection
      pattern into `unsupported_characteristic`. Fixed by gating the
      insert to only the "ordinary", shape-comparable item types this
      axis is actually about - `FIELD`/`BRACKET`/`VECTOR`/
      `VECTORBRACKET` - mirroring `is_ordinary_resolvable_import`'s own
      identical "non-ordinary declared item type has no graph-native
      resolution path" precedent (task 6.8). `declared_item_type`
      itself is still set unconditionally (`set_declared_item_type` is
      untouched); only the new characteristic's insert is gated.
      design.md D1 updated to match (see design.md's own amendment
      note).
- [x] 4.3 One-line comment above the insertion citing this change and
      design.md D1 for the "deliberate duplication with
      `declared_item_type`" rationale.

## 5. `GraphBuilder_ConnectionResolution_smod.F90`: narrow the gate (design.md D5/D6)

- [x] 5.1 In `try_materialize_field_extension` (both `unsupported_item_class`
      checks, ~lines 1573/1577): change the EXPORT-side check from
      `.not. (export_item%get_declared_item_type() == MAPL_STATEITEM_FIELD)`
      to `.not. (export_item%get_declared_item_type() == MAPL_STATEITEM_FIELD &
      .or. export_item%get_declared_item_type() == MAPL_STATEITEM_BRACKET)`.
      Leave the IMPORT-side check (~line 1577 in that procedure)
      unchanged - still `FIELD`-only.
- [x] 5.2 In `materialize_extension_chain` (~lines 1730/1734): apply the
      identical export-side `.or. == MAPL_STATEITEM_BRACKET` change;
      leave the import-side check unchanged.
- [x] 5.3 Add `use mapl_StateItem_mod, only: MAPL_STATEITEM_BRACKET` (or
      confirm it is already in scope via an existing `use`) in
      `GraphBuilder_ConnectionResolution_smod.F90`.
      Completion note: already in scope - `GraphBuilder.F90`'s own
      `use mapl_StateItem_mod, only: MAPL_STATEITEM_BRACKET, ...`
      (line 177) is host-associated into this submodule exactly like
      `MAPL_STATEITEM_FIELD` already is (confirmed by grep: no local
      `use mapl_StateItem_mod` exists in this submodule file, yet
      `MAPL_STATEITEM_FIELD` was already used throughout it before
      this change). No new `use` statement needed.
- [x] 5.4 Add a code comment at both narrowed gate sites citing this
      change and design.md D5, explaining the asymmetry (export may be
      `BRACKET`, import may not).
- [x] 5.5 At `resolve_one`'s own "materialize the chain's own INPUT
      node" call site (~line 422-437, the one passing `export_item` as
      both arguments to `try_materialize_field_extension`), add the
      design.md D6 guard: skip the call entirely (no `_FAIL`, fall
      through to the chain's own materialization below) when
      `export_item%get_declared_item_type() /= MAPL_STATEITEM_FIELD`.
      Keep the existing `try_alias_export_field`-found short-circuit
      unchanged; the new guard applies only to the
      `try_materialize_field_extension` call itself.
- [x] 5.6 Add a code comment at that guard citing design.md D6
      explicitly (why skip, not fail; why no synthetic stand-in).

## 6. `merged_item_for_link`: enumerate the new kind (design.md D5)

- [x] 6.1 Add a `use` statement for `ITEM_SHAPE_CHARACTERISTIC_KIND` in
      `GraphBuilder_ConnectionResolution_smod.F90` if not already
      present from Section 5.
      Completion note: added to `GraphBuilder.F90`'s own
      `use mapl_StateItemCharacteristicKind_mod, only: ...` list (the
      parent module, host-associated into this submodule) alongside
      the other `*_CHARACTERISTIC_KIND` names already there - same
      pattern Section 4 used for `GraphBuilder_Advertising_smod.F90`.
- [x] 6.2 In `merged_item_for_link`'s `do j = 1, i` loop
      (~line 1806-1843), add `ITEM_SHAPE_CHARACTERISTIC_KIND` to the
      existing no-op enumerated branch (the `ATTRIBUTES_CHARACTERISTIC_KIND
      .or. ... .or. NORMALIZATION_CHARACTERISTIC_KIND` group, or a new
      dedicated `else if`), with a comment explaining why (design.md
      D5: materialized shape is always decided by
      `materialize_field_extension`'s own unconditional `FieldCreate`
      call, not by anything this helper merges).
      Completion note: given its own dedicated `else if` branch
      (distinct from the Phase-12 six, since this one is reachable in
      practice - a real BRACKET->FIELD mismatch does have a real
      `build_transform`), with a comment explaining why nothing needs
      merging and citing design.md D5.
- [x] 6.3 Confirm the trailing `_FAIL` default is still reached only for
      a genuinely-unregistered kind.
      Completion note: confirmed by inspection - every registered kind
      (`PHYSICAL_UNITS`/`TYPE_KIND`/`VERTICAL_COORDINATE`/`ITEM_SHAPE`/
      the Phase-12 six) is now enumerated above the `else`; the
      default branch's `_FAIL` is reached only for a kind with no
      branch at all.

## 7. Unit tests: `ItemShapeCharacteristic` (design.md D7)

- [x] 7.1 Add test cases to
      `superstructure/generic/graph/tests/Test_StateItemCharacteristicSubclasses.pf`
      (or a new `Test_ItemShapeCharacteristic.pf`, consistent with that
      file's own per-subclass section convention): confirm
      `ItemShapeCharacteristic` is a `ValueCharacteristic`, its
      `get_kind()` returns `ITEM_SHAPE_CHARACTERISTIC_KIND`.
- [x] 7.2 Case: two characteristics both `MAPL_STATEITEM_FIELD` ->
      `needs_extension_for` is `.false.`.
- [x] 7.3 Case: `MAPL_STATEITEM_BRACKET` vs. `MAPL_STATEITEM_FIELD` ->
      `needs_extension_for` is `.true.`.
- [x] 7.4 Case: `MAPL_STATEITEM_VECTORBRACKET` vs. `MAPL_STATEITEM_VECTOR`
      -> `needs_extension_for` is `.true.` (mismatch correctly
      detected even though no Transform exists for it).
- [x] 7.5 Case: `build_transform` for `BRACKET`->`FIELD` succeeds and
      allocates a `BracketTimeInterpolationTransform`.
- [x] 7.6 Case: `build_transform` for `VECTORBRACKET`->`VECTOR` (and at
      least one other mismatched pairing, e.g. `FIELD`->`VECTOR`) fails
      explicitly (`rc /= 0`), not a stub Transform.

## 8. Unit tests: `BracketTimeInterpolationTransform` (design.md D7)

- [x] 8.1 Add `superstructure/generic/graph/tests/Test_BracketTimeInterpolationTransform.pf`
      (or append to an existing graph-transforms test file if one
      already covers `UnitsConverterTransform`/`TypeKindConverterTransform`
      directly - confirm during implementation), constructing a real
      `ESMF_FieldBundle` with two member fields plus
      `interpolation_weights` set via `MAPL_FieldBundleSet` (mirroring
      `Test_TimeInterpolateTransform.pf`'s own fixture construction),
      and a real `ESMF_Field` destination.
      Completion note: new dedicated file, registered in
      `superstructure/generic/graph/tests/CMakeLists.txt` immediately
      after `Test_UnitsConverterTransform.pf`.
- [x] 8.2 Wrap both in `GraphStateItem`s, bind them into
      `PortNameGraphStateItemMap`s under `'source_field'`/
      `'destination_field'`, and call `compute()` directly (no
      `GraphBuilder`/`ComponentGraph` involved) - the same
      "exercise the Transform directly" idiom design.md D7 specifies.
- [x] 8.3 Assert the destination field's values equal the hand-computed
      weighted sum of the two member fields (R4 case).
- [x] 8.4 Repeat for R8.
- [x] 8.5 Case: a member value equal to `MAPL_UNDEFINED_REAL`/
      `MAPL_UNDEFINED_REAL64` propagates through as undefined in the
      output (masking behavior matches `TimeInterpolateTransform.F90`'s
      own `where`/`elsewhere`).
      Completion note: R4 masking case covered
      (`test_bracket_time_interpolation_respects_mapl_undef`); R8 not
      duplicated separately - R4/R8 share the identical masking branch
      in `interpolate_r4`/`interpolate_r8`, already both covered for
      the arithmetic itself by 8.3/8.4.

## 9. Fixture: bracket export -> field import end to end (design.md D7, proposal.md)

- [x] 9.1 Confirm whether `to_itemtype`/`ComponentSpecParser` (
      `superstructure/generic/ComponentSpecParser/parse_var_specs.F90`)
      currently plumbs `bracket_size` from YAML at all - it does not
      today (grep confirmed no `bracket_size` reference in that
      directory); if a real YAML-driven fixture needs it, add the
      minimal `bracket_size` key parsing to `parse_var_specs.F90`
      following the existing `fill_value`/`KEY_FILL_VALUE` pattern
      (`val_to_float`-style helper, or a plain
      `ESMF_HConfigAsI4`-based one for an integer). If this expands
      scope meaningfully, prefer 9.2's direct-construction alternative
      instead and record the decision in this task's own completion
      note.
      Completion note: confirmed (re-grep) no `bracket_size`/`bracket`
      key parsing exists in `parse_var_specs.F90`/`to_itemtype.F90`
      beyond the bare `class: bracket` -> `MAPL_STATEITEM_BRACKET`
      mapping - no actual bracket member count/field list is ever
      read from YAML today (a real bracket export's own `FieldBundle`
      is populated by legacy `GRAPH_MODE_REGISTRY` machinery, not this
      parser). Adding that would be a meaningfully separate, unrelated
      capability (a new YAML schema key plus whatever constructs the
      member `ESMF_Field`s it describes) - out of this change's scope
      (proposal.md's own `Bracket`->`Field` dispatch-wiring focus, not
      "add bracket-export YAML authoring"). Deferred; proceeding with
      9.2's direct-construction alternative for both the chain-
      structure test (9.2/9.3) and the real-`resolve_one` test (9.4).
      9.5 (the YAML scenario fixture) is skipped for the same reason.
- [x] 9.2 Build the new scenario fixture as a synthetic-node pFUnit test
      (mirroring `Test_ExtensionResolution.pf`'s own
      `StateItemNode`/`set_payload`-based construction, per design.md
      D7/Non-Goals - no new `GraphBuilder`-reachable bracket
      materialization path is added) rather than a
      `superstructure/generic/tests/scenarios/` YAML fixture, unless
      9.1 found YAML plumbing trivial to add. Construct an export
      `StateItemNode` with `declared_item_type = MAPL_STATEITEM_BRACKET`
      and an `ItemShapeCharacteristic(MAPL_STATEITEM_BRACKET)`, and an
      import `StateItemNode` with `declared_item_type =
      MAPL_STATEITEM_FIELD` and `ItemShapeCharacteristic(MAPL_STATEITEM_FIELD)`,
      matching units/typekind on both sides (isolating the shape axis).
      Completion note: added
      `test_bracket_to_field_mismatch_builds_interpolation_chain` to
      `Test_ExtensionResolution.pf`, same file/idiom as every other
      chain-creation test there - exercises `ItemShapeCharacteristic`
      directly via the export/import `StateItemCharacteristicMap`s
      (the export `StateItemNode` itself is registered but its payload
      carries no `declared_item_type`, matching every other test in
      this file - `find_mismatched_characteristics`/`build_chain`
      never reads it, only the two maps).
- [x] 9.3 Exercise `find_mismatched_characteristics`/`build_chain`
      (`ExtensionResolution.F90`) directly against the two sides' maps
      and assert: a mismatch on `ITEM_SHAPE_CHARACTERISTIC_KIND` is
      reported, and `build_chain` succeeds, producing one
      `BracketTimeInterpolationTransform`-backed extension node - no
      `unsupported_characteristic` failure.
      Completion note: same test as 9.2 asserts the mismatch
      (`mismatched(1) == ITEM_SHAPE_CHARACTERISTIC_KIND`), the
      successful chain build (`status == 0`, `unsupported == ''`), and
      the one-node intermediate chain
      (`size(intermediate_ids) == 1`). Did not additionally assert the
      intermediate node's `Transform` is specifically
      `BracketTimeInterpolationTransform` (vs. some other successful
      provider) - the only other provider registered against
      `ITEM_SHAPE_CHARACTERISTIC_KIND` is `item_shape_build_transform`
      itself, and 7.5 already unit-tests that exact dispatch
      (`BRACKET`->`FIELD` allocates a `BracketTimeInterpolationTransform`)
      directly against the characteristic - this test's own added
      value is confirming the same dispatch is reachable through the
      full `find_mismatched_characteristics`/`build_chain` path, not
      re-proving 7.5's dispatch choice.
- [x] 9.4 Add a second case driving the same scenario through
      `resolve_one`'s real call path (real `OuterMetaComponent`/
      `ComponentGraph`, synthetic `VariableSpec`s with `itemtype =
      MAPL_STATEITEM_BRACKET`/`MAPL_STATEITEM_FIELD`) to confirm the
      narrowed gate (Section 5) is reached and no longer reports
      `unsupported_item_class` for this pairing - structure-only
      (`materialize_extensions_enabled()` off), per design.md D7(a).
      Completion note (deviation from the literal "gate off"
      instruction, recorded per the "pause on ambiguity" standard -
      no pause needed here since the fix is self-evident from reading
      the code, but the deviation itself must still be visible):
      `materialize_extensions_enabled()` OFF never calls
      `materialize_extension_chain`
      (`GraphBuilder_ConnectionResolution_smod.F90` ~line 474's own
      `if (materialize_extensions_enabled()) then` guard) - the exact
      procedure Section 5.2's narrowed export-side gate lives inside.
      With the gate off, `resolve_one` never reaches that check at
      all (for either the old or the new gate condition), so a
      gate-off test would pass identically whether or not Section 5's
      fix existed - it would not actually confirm anything. Added
      `test_materialize_extensions_bracket_export_narrowed_gate`
      (named to stay within NAG's enforced 63-character procedure-name
      limit) to `Test_GraphBuilderMaterializeGate.pf` (not
      `Test_GraphBuilder_ConnectionResolution.pf` - the narrowed gate
      lives inside `materialize_extension_chain`, which calls
      `resolve_export_geom`, needing a real `ESMF_Geom`; that requires
      `type=ESMF_TestMethod`, per this file's own header explaining
      why every `ESMF_TestMethod` test in this area lives here rather
      than in the `MpiTestMethod`-typed `Test_GraphBuilder_
      ConnectionResolution.pf`), using the same gate-enable-then-reset
      idiom as that other file's
      `test_materialize_extensions_reports_unsupported_item_class`
      (its own VECTOR->VECTOR negative-control counterpart) -
      confirms `unsupported_characteristics%size() == 0` for a
      BRACKET export/FIELD import pairing with matching units, where
      before this change's Section 5 fix the old export-side gate
      would have reported `unsupported_item_class:BRACKET`.
- [x] 9.5 If 9.1 added YAML `bracket_size` support, add a real
      `Test_Scenarios.pf` fixture under
      `superstructure/generic/tests/scenarios/bracket_1/` (parent +
      two children, mirroring `vector_1/`'s own three-file shape) and
      register it in `Test_Scenarios.pf`'s parameter list, with a
      `description.md` explaining the expected resolved field. If 9.1
      instead deferred YAML plumbing, skip this task and record why in
      the task's own completion note.
      Completion note: skipped - 9.1 deferred YAML `bracket_size`
      plumbing as out of scope; no YAML-driven fixture is possible
      without it. 9.2-9.4 cover the same structural ground via
      synthetic nodes/direct `VariableSpec` construction instead.

## 10. Regression and verification

- [x] 10.1 Build MAPL (NAG, per the active compiler-switching skill)
      with all modified/new files included - clean build, zero
      warnings/errors introduced by this change's own files.
      Completion note: `module load nag-stack/7.2.41 && cmake --build
      nag -j 8 --target build-tests` - clean, zero warnings/errors in
      any file this change touches (NAG's "Procedure name ... more
      than 63 characters" extension warning hit during an earlier
      iteration was fixed by shortening the offending test name -
      see 9.4's own completion note - and does not reappear in the
      final build).
- [x] 10.2 Run `ctest -R "^MAPL.generic.graph$"` - passed, 0 failures.
      Completion note: `ctest --test-dir nag -R "^MAPL.generic.graph$"`
      -> 100% tests passed, 1/1 (`MAPL.generic.graph`, which bundles
      every `Test_*.pf` under `superstructure/generic/graph/tests/`
      including the new `Test_BracketTimeInterpolationTransform.pf`
      and the extended `Test_ExtensionResolution.pf`/
      `Test_StateItemCharacteristicSubclasses.pf`).
- [x] 10.3 Run `ctest -R "^MAPL.generic"` (the full `MAPL.generic.*`
      label set) - all suites pass, zero regressions.
      Completion note: `ctest --test-dir nag -R "^MAPL.generic"` ->
      100% tests passed, 8/8 (`MAPL.generic.graph`/`.scenarios`/
      `.transforms`/`.vertical`/`.aspects`/`.components`/
      `.components.quiet`/`.core`). This run is what originally
      surfaced the `EXPRESSION`->`FIELD` regression (task 4.2's own
      completion note) - re-run after that fix, and again after every
      subsequent task 9 addition, with 0 failures each time.
      Also ran the full, unfiltered `ctest --test-dir nag` (76 tests,
      beyond this task's own literal scope) for extra confidence: 72
      pass, 4 fail (`ll-ll`/`cs-cs`/`cs-ll`/`ll-cs` under
      `apps/tests/Regrid_Util`) - all four fail identically on a
      missing `LOCAL_REGRESSION_DATA_DIR` environment variable
      (`CMake Error ... regression data directory not found`), a
      pre-existing local-environment gap unrelated to any file this
      change touches.
- [x] 10.4 Confirm no existing Field-only connection in any current
      fixture is affected by the Section 5 gate change (every existing
      `Test_Scenarios`/`graph-scenarios` fixture still passes
      unchanged).
      Completion note: confirmed by 10.3's own
      `MAPL.generic.scenarios` result (100% pass, includes every
      `superstructure/generic/tests/scenarios/*` and
      `graph-scenarios/*` fixture, none of which uses `class:
      bracket`/`MAPL_STATEITEM_BRACKET` - the Section 5 gate change is
      additive (`.or. == MAPL_STATEITEM_BRACKET`), so every pre-
      existing Field-only export is still admitted by the unchanged
      first disjunct, confirmed observationally by zero scenario
      fixture regressions across every re-run this session).
- [x] 10.5 Run `openspec validate phase-7a-shape-dispatch-wiring
      --strict` - change is valid.
      Completion note: `openspec validate phase-7a-shape-dispatch-wiring
      --strict` -> "Change 'phase-7a-shape-dispatch-wiring' is valid".
