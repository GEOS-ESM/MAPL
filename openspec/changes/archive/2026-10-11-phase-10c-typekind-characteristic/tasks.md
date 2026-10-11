## 1. CharacteristicId

- [x] 1.1 Add `TYPE_KIND_CHARACTERISTIC_ID = CharacteristicId(4)` to
      `superstructure/generic/graph/CharacteristicId.F90`, exported in
      the module's `public ::` list alongside the other three real ids.
- [x] 1.2 Add a `"TYPE_KIND"` branch to `to_string()`'s `select case`
      for the new id.

## 2. TypeKindCharacteristicAdapter (the Characteristic-side wrapper, D1)

- [x] 2.1 Create `superstructure/generic/graph/TypeKindCharacteristicAdapter.F90`,
      module `mapl_TypeKindCharacteristicAdapter_mod`, `type, extends(Characteristic)
      :: TypeKindCharacteristicAdapter` holding `type(TypeKindCharacteristic)
      :: inner` by composition (private component), per design.md D1.
- [x] 2.2 Implement `get_id()` (nopass) -> `TYPE_KIND_CHARACTERISTIC_ID`.
- [x] 2.3 Implement `get_signature(this)` -> `'typekind=' //
      this%inner%get_type_kind()`, mirroring `UnitsCharacteristic%get_signature`.
- [x] 2.4 Implement `needs_extension_for(this, goal)`: `select type (goal);
      class is (TypeKindCharacteristicAdapter)` -> delegate to
      `this%inner%needs_extension_for(goal%inner)`; `class default` ->
      `.true.` (mirrors `UnitsCharacteristic`'s own defensive default,
      not `TypeKindCharacteristic`'s own `error stop`).
- [x] 2.5 Implement `build_transform(this, graph, input_node_id,
      output_node_id, goal, transformer, rc)`: `select type (goal);
      class is (TypeKindCharacteristicAdapter)` -> allocate
      `TypeKindConverterTransform` (section 3) from
      `this%inner%get_type_kind()`/`goal%inner%get_type_kind()`;
      `class default` -> `_FAIL`.
- [x] 2.6 Add a public constructor interface
      `TypeKindCharacteristicAdapter(type_kind)` taking the plain
      `'R4'`/`'R8'` string label (constructs `inner` directly), plus a
      second convenience form taking an `ESMF_TypeKind_Flag` and
      converting it to the label internally (small local helper per
      design.md Risks - not exported beyond this module).

## 3. TypeKindConverterTransform (the real cast, D2)

- [x] 3.1 Confirm no name collision: grep
      `superstructure/generic/transforms/` and
      `superstructure/generic/graph/` for `TypeKindConverterTransform`/
      `mapl_TypeKindConverterTransform_mod` (expected: none found, per
      design.md D2 - re-verify at implementation time, not just at
      design time).
- [x] 3.2 Create `superstructure/generic/graph/TypeKindConverterTransform.F90`,
      module `mapl_TypeKindConverterTransform_mod`, `type, extends(Transform)
      :: TypeKindConverterTransform`, mirroring
      `UnitsConverterTransform.F90`'s shape: holds `class(ComponentGraph),
      pointer :: graph`, `input_node_id`/`output_node_id` (`NodeId`),
      `src_typekind`/`dst_typekind` (`ESMF_TypeKind_Flag`).
- [x] 3.3 Constructor declares the same two named ports
      (`INPUT_PORT_NAME = 'source_field'`, `OUTPUT_PORT_NAME =
      'destination_field'`) via `declare_input_port`/`declare_output_port`.
- [x] 3.4 Implement `compute(this, rc)`: fetch both bound `StateItemNode`s
      from `this%graph`, get their `ESMF_Field`s, dispatch on
      `(this%src_typekind, this%dst_typekind)` via
      `mapl_FieldPointerUtilities_mod::assign_fptr` - real `R4->R8` and
      `R8->R4` cast cases (`x_out(:) = real(x_in(:), kind=ESMF_KIND_R8)`
      and the reverse); any other combination (should not occur, since
      `needs_extension_for` already requires the labels to differ)
      `_FAIL`s explicitly, same discipline as
      `UnitsConverterTransform`'s own `_FAIL('unsupported typekind')`.
- [x] 3.5 Add a module header comment recording the collision check
      from 3.1 and this transform's relationship to legacy's
      `CopyTransform` and the `18-state-item-characteristics.md`
      placeholder name, mirroring `UnitsConverterTransform.F90`'s own
      header precedent.

## 4. GraphBuilder.F90 wiring

- [x] 4.1 In `build_characteristics()`, insert a
      `TypeKindCharacteristicAdapter` keyed by
      `TYPE_KIND_CHARACTERISTIC_ID` unconditionally for every
      `VariableSpec` (design.md D3 - no `allocated()` gate, since
      `var_spec%typekind` is non-allocatable with a default).
- [x] 4.2 In `try_materialize_field_extension`, change both
      `materialize_field_extension(...)` call sites' `typekind=`
      argument from `export_var_spec%typekind` to
      `import_var_spec%typekind` (design.md D4 - fixes the latent
      export-vs-import bug; matches the existing `units=
      import_var_spec%units` precedent already on both call sites).
- [x] 4.3 Add/update `use` statements in `GraphBuilder.F90` for
      `mapl_TypeKindCharacteristicAdapter_mod`/`TYPE_KIND_CHARACTERISTIC_ID`
      as needed by 4.1.

## 5. New fixture: units_and_typekind_mismatch

- [x] 5.1 Create
      `superstructure/generic/tests/graph-scenarios/units_and_typekind_mismatch/`
      with `cap.yaml`+`comp_src.yaml`+`comp_dst.yaml`, following
      `plain_field/`'s exact 3-file, flat, `all_unsatisfied`-connection
      convention (design.md D5).
- [x] 5.2 `comp_src.yaml`: export `T`, `units: 'hPa'`, `typekind: R4`, a
      static (no `run:`) numeric value chosen so both conversion steps
      (units then typekind) are exact in both precisions - compute and
      record the expected final value in a short fixture-local
      `description.md` (mirroring `invalidate/`'s own `description.md`
      precedent), so the hand-check is traceable without re-deriving it
      during review.
- [x] 5.3 `comp_dst.yaml`: import `T`, `units: 'Pa'`, `typekind: R8`.
- [x] 5.4 Confirm (by running the fixture through `GraphBuilder.F90`'s
      real resolution path, e.g. via a quick scratch test or the
      harness itself from section 6) that this fixture builds a real
      two-step chain (units characteristic then typekind
      characteristic, matching `invalidate/`'s own observed ordering)
      and does not hit the "unregistered characteristic fails loudly"
      path for either step.

## 6. Equivalence harness extension

- [x] 6.1 In `Test_GraphEquivalenceHarness.pf`'s
      `get_equivalence_fixtures()`, add a fourth
      `EquivalenceFixtureDescription` entry for
      `units_and_typekind_mismatch` (`has_run_phase=.false.`, matching
      `vertical_field`/`ungridded_dims`'s own static-fixture precedent).
- [x] 6.2 Lookup mechanism confirmed (design.md D6 correction): no new
      accessor needed. `proxy_key('comp_dst', ESMF_STATEINTENT_IMPORT,
      name)` resolves the import's own local `NodeId`; the chain's
      final item is that node's sole predecessor in the graph's default
      `DependencyNetwork` (`get_predecessors`), mirroring
      `Test_GraphBuilder.pf`'s own existing chain-walking pattern.
- [x] 6.3 In `check()`, add one new assertion block, gated on
      `this%fixture_name == 'units_and_typekind_mismatch'`, that
      resolves the chain's final item via 6.2's mechanism and asserts
      its materialized field's typekind and units match the import's
      own declared requirements (`R8`/`'Pa'`) - per the
      `graph/equivalence-harness` spec delta's new "Mismatched-
      connection chain's final item has the import's own typekind and
      units" scenario.
- [x] 6.4 **Corrected during implementation, twice (design.md D6):**
      the existing export-payload-parity block (proxy resolve through
      the Task 4.4 rank check) does NOT run correctly unmodified for
      this fixture. First correction: `legacy_field` (post-conversion)
      genuinely differs from the export's own value for this fixture,
      not by defect. Second correction, found only by running the
      suite: `resolve_one`'s mismatch branch never materializes the
      export's own node at all (only the chain's final item is, and
      only once task 6.5 enables that) - so `export_payload%itemType()`
      fails outright, not merely compares the wrong value. Fixed by
      gating the ENTIRE block on `this%fixture_name /=
      'units_and_typekind_mismatch'`, matching this change's own
      `graph/equivalence-harness` spec delta, which scopes "Static
      attributes agree" to an exact-match connection in the first
      place.
- [x] 6.5 **Added during implementation (design.md D6, second
      correction):** `equivalence_fixture_setup` must call
      `set_materialize_extensions(.true.)` immediately before the
      init-phase loop (alongside the existing
      `set_graph_mode(GRAPH_MODE_BOTH)`) and reset it to `.false.`
      immediately after (alongside `set_graph_mode(GRAPH_MODE_REGISTRY)`)
      - without it, the mismatch branch never materializes the chain's
      final item (or anything else) for `units_and_typekind_mismatch`.
      Mirrors `Test_GraphBuilderEquivalence.pf`'s own existing
      `set_materialize_extensions` usage exactly; harmless for the three
      existing exact-match fixtures (the flag is only consulted on the
      mismatch branch, which none of them take).

## 7. Regression and verification

- [x] 7.1 Build and run `MAPL.generic.aspects`/the existing
      `graph/extension-reuse` unit test suite
      (`Test_ExtensionResolution.pf`, `Test_Characteristic*.pf` if any)
      unmodified - confirm no existing test's expectations change due
      to the new unconditional typekind-characteristic insertion
      (design.md D3's own regression-check task). `ctest -R
      "MAPL.generic.graph$|MAPL.generic.aspects$|MAPL.generic.core$"`:
      100% passed, 0 failed.
- [x] 7.2 Run `Test_GraphBuilder.pf` and `Test_GraphBuilderEquivalence.pf`
      unmodified - confirm no existing assertion's result changes.
      `Test_GraphBuilder.pf` is in `MAPL.generic.core` (passed above);
      `Test_GraphBuilderEquivalence.pf` is in `MAPL.generic.scenarios`
      (passed, see 7.4).
- [x] 7.3 Run the full `Test_Scenarios.pf` suite (the ~30-fixture
      real-YAML regression suite) unmodified - confirm no existing
      fixture's expected output changes (this is the real-world check
      that no production fixture's two sides silently disagree on
      typekind in a way nobody noticed before this change made it
      detectable). `Test_Scenarios.pf` is in `MAPL.generic.scenarios`
      (passed, see 7.4) - no existing fixture's expectations changed.
- [x] 7.4 Run `Test_GraphEquivalenceHarness.pf` with the new fixture
      included - confirm all four parameterized fixtures pass,
      including the new typekind/units assertion from 6.3. `ctest -R
      "generic.scenarios"`: 323/323 tests passed (0 failed), including
      all four `Test_GraphEquivalenceHarness_suite.check[...]` cases.
- [x] 7.5 Confirm by direct reading that
      `ExtensionResolution.F90`'s `build_chain`/
      `find_mismatched_characteristics`/`find_or_build_extension_chain`
      were not modified by this change (design.md Non-Goals -
      verification, not implementation). Confirmed: `git diff` shows no
      changes to `ExtensionResolution.F90`.
- [x] 7.6 `openspec validate phase-10c-typekind-characteristic --strict`
      reports the change valid.
