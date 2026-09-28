## 1. FieldInfo: unaliased standard_name storage

- [x] 1.1 In `infrastructure/field/FieldInfo.F90`, add an unaliased
      `standard_name` set path: either a new optional argument on
      `field_info_set_internal` that writes `namespace_ // KEY_STANDARD_NAME`
      directly (no `named_alias_id` required), or a small dedicated
      `field_info_set_internal_standard_name` sibling - whichever avoids
      overload ambiguity with the existing alias-namespaced
      `long_name`/`standard_name` argument pair.
- [x] 1.2 Add the matching unaliased `standard_name` get path (mirrors
      `UnitsAspect`'s use of `field_info_get_internal`'s existing `units`
      argument, `FieldInfo.F90:334-337`), defaulting to `'<unknown>'` when
      absent.
- [x] 1.3 Remove `standard_name` from `field_info_set_internal`/
      `field_info_get_internal`'s alias-namespaced argument pair
      (`FieldInfo.F90:159-172` and its get-side counterpart); keep
      `long_name`'s alias-namespaced handling unchanged. Update the
      `named_alias_id`-required assertion (`FieldInfo.F90:160`) to reference
      only `long_name`.

## 2. StandardNameAspect: new aspect type

- [x] 2.1 Add `STANDARD_NAME_ASPECT_ID` to the aspect-id registry
      (wherever `UNITS_ASPECT_ID` etc. are defined, e.g. `mapl_AspectId_mod`).
- [x] 2.2 Create `superstructure/generic/specs/StandardNameAspect.F90`
      (`mapl_StandardNameAspect_mod`), modeled directly on
      `UnitsAspect.F90`: a `StateItemAspect` subtype carrying a single
      `character(:), allocatable :: standard_name`, with `is_unchecked`/
      wildcard support via the existing `set_characteristic_state`/
      `ASPECT_STATUS_*` machinery already used by `UnitsAspect`.
- [x] 2.3 Implement `matches(src, dst)`: `.true.` if either side is
      unchecked/wildcard or the two `standard_name` values are equal;
      otherwise `.false.` in `STRICT` `ValidationMode`, `.true.` (with a
      logged warning naming both values) in `PERMISSIVE` mode. Read the
      active mode from the `FieldDictionaryConfig` singleton (task 4.1).
- [x] 2.4 Implement `connect_to_export`: unconditionally adopt the (already
      matched) export's `standard_name`, mirroring
      `UnitsAspect.F90:137-152`.
- [x] 2.5 Implement `update_from_payload`/`update_payload` using the new
      unaliased `FieldInfo` helpers from task 1.1/1.2, mirroring
      `UnitsAspect.F90:217-278`.
- [x] 2.6 Implement `get_standard_name`/`set_standard_name` accessors and
      the `to_StandardNameAspect`/`get_aspect_id` boilerplate that mirrors
      `UnitsAspect`'s.

## 3. Wire StandardNameAspect into FieldClassAspect (and delegators)

- [x] 3.1 In `superstructure/generic/specs/FieldClassAspect.F90`, remove the
      `standard_name` member, its constructor argument, its slice of
      `get_standard_name`/`get_long_name`, the `mirror_name(this%standard_name, ...)`
      call in `connect_to_export` (`FieldClassAspect.F90:311`), and its
      participation in the `add_to_state` persistence guard/call
      (`FieldClassAspect.F90:497,511-513`).
- [x] 3.2 Add `STANDARD_NAME_ASPECT_ID` to `FieldClassAspect%get_aspect_order`
      and `get_mandatory_aspect_ids` (`FieldClassAspect.F90:129-169`),
      alongside `UNITS_ASPECT_ID`.
- [x] 3.3 Confirm (per design.md Decision 1/6) that `BracketClassAspect`
      (`BracketClassAspect.F90:139-146`) and `VectorBracketClassAspect`
      (`VectorBracketClassAspect.F90:146-153`) need no `get_aspect_order`/
      `get_mandatory_aspect_ids` changes because they delegate wholesale to
      a `FieldClassAspect` placeholder/component (both wrap N repeated
      copies of one plain, non-compound `standard_name`). `VectorClassAspect`
      is different (compound-encoded, 2-component name) - see tasks 3.6-3.9.
- [x] 3.4 Confirm `FieldBundleClassAspect.F90` and `StateClassAspect.F90` are
      left unchanged (they do not gain `STANDARD_NAME_ASPECT_ID`, matching
      their exemption).
- [x] 3.5 Fix constructor call sites broken by `FieldClassAspect`'s shorter
      signature: `BracketClassAspect.F90:91` and
      `VectorBracketClassAspect.F90:93` (`FieldClassAspect(standard_name,
      long_name, fill_value)` -> `FieldClassAspect(long_name=..., fill_value=...)`),
      and `ExpressionClassAspect.F90:330` (`FieldClassAspect(standard_name='',
      long_name='')` -> `FieldClassAspect(long_name='')`). Remove the now-dead
      `standard_name` member/constructor-arg from `BracketClassAspect`,
      `VectorBracketClassAspect`, `FieldBundleClassAspect`, `StateClassAspect`,
      and `ExpressionClassAspect` (including `ExpressionClassAspect`'s
      `get_standard_name` override, per design.md Decision 1 - `long_name`'s
      equivalent members/overrides are untouched). Update
      `VariableSpec.F90:make_ClassAspect` (lines ~685-731) to stop passing
      `standard_name=`/positional `this%standard_name` to any of these
      constructors.

- [x] 3.6 (design.md Decision 6) Add `type(StandardNameAspect) ::
      standard_name_aspects(2)` to `VectorClassAspect`
      (`VectorClassAspect.F90`), built in a new constructor path from two
      explicit `standard_name` strings (or two `StandardNameAspect` values).
- [x] 3.7 Extend `VectorClassAspect%matches` to additionally require
      `src%standard_name_aspects(i)%matches(dst%standard_name_aspects(i))`
      for `i = 1, 2` (beyond the existing dynamic-type check), and extend
      `connect_to_export` to forward to both components'
      `connect_to_export`. Extend the per-component field creation in
      `allocate()`/its private `update_payload` helper to also call
      `standard_name_aspects(i)%update_payload(field=...)` for each
      component field.
- [x] 3.8 Override `VectorClassAspect%get_aspect_order`/
      `get_mandatory_aspect_ids` to filter `STANDARD_NAME_ASPECT_ID` out of
      the list delegated from `component_specs(1)` (agreement is now checked
      directly in `matches`, task 3.7, not through the outer aspect map).
- [x] 3.9 Update `VariableSpec.F90`'s `MAPL_STATEITEM_VECTOR` case
      (`make_ClassAspect`, ~lines 697-715) to pass the already-computed
      `std_name_1`/`std_name_2` (from `split_name`) into the new
      `VectorClassAspect` constructor path (task 3.6) instead of only using
      them to build the two `FieldClassAspect` components.

## 4. FieldDictionaryConfig singleton and cap.yaml wiring

- [x] 4.1 In `infrastructure/field_dictionary/FieldDictionaryConfig.F90`, add
      a module-level singleton (`the_field_dictionary_config`,
      `set_field_dictionary_config`/`get_field_dictionary_config`) mirroring
      `FieldDictionary.F90:63,346-349`, defaulting to `FieldDictionaryConfig()`
      (permissive) if never explicitly set.
- [x] 4.2 In `mapl/MaplFramework.F90:initialize_field_dictionary`
      (lines 1188-1215), detect whether the `field_dictionary` HConfig node
      is a scalar string (today's form) or a mapping. For a mapping, build a
      `FieldDictionaryConfig(node)` (task 4.1's constructor) and store it via
      `set_field_dictionary_config`; for a scalar string, keep today's
      behavior (treat it as `path`, default/permissive mode) but still store
      a `FieldDictionaryConfig` via the singleton so downstream code has one
      consistent place to query the mode.
- [x] 4.3 Add/extend a scenario or unit test exercising the mapping form of
      `field_dictionary: {path: ..., validation_mode: strict}` in a real
      `cap.yaml`-shaped HConfig, and confirm the bare-string form still
      works unchanged (backward compatibility).

## 5. Automatic FieldDictionary defaulting (retire use_field_dictionary)

- [x] 5.1 In `superstructure/generic/specs/VariableSpec.F90`, make the call
      to `apply_field_dictionary_defaults_` (currently gated by
      `use_field_dictionary`, lines 259-261) unconditional whenever
      `standard_name` is present and the item type is not exempt
      (`get_field_dictionary_config()%is_exempt(item_type)`, task 4.1).
- [x] 5.2 Apply the same unconditional-subject-to-`is_exempt` treatment to
      the regrid-method defaulting path
      (`get_regrid_param`/`get_regrid_method_from_field_dict_`,
      `VariableSpec.F90:370-420`).
- [x] 5.3 In `apply_field_dictionary_defaults_`
      (`VariableSpec.F90:274-322`), change the "standard_name not found in
      dictionary" path: `_FAIL` in `STRICT` mode, keep the existing
      `logger%warning` in `PERMISSIVE` mode (`VariableSpec.F90:310-319`).
- [x] 5.4 Remove the `use_field_dictionary` argument from
      `make_VariableSpec` (`VariableSpec.F90:142,215,259-261`) and its call
      sites (`superstructure/generic/MAPL_Generic.F90:644,677,735`), once no
      longer referenced.

## 6. Test coverage

- [x] 6.1 (Approach changed during implementation - see 6.2-6.5, 6.9) A full
      YAML-driven scenario for `STRICT` mode turned out to be impractical:
      `ValidationMode` is set once, process-wide, via
      `MaplFramework%initialize_field_dictionary`'s single call during test
      binary startup (the pFUnit `extra_initialize_` hook) - all scenarios
      in `MAPL.generic.scenarios` share one process/one mode, so a
      per-scenario `cap.yaml` `field_dictionary: {validation_mode: strict}`
      would not actually take effect per-scenario. Coverage was implemented
      instead as direct `StandardNameAspect`/`VectorClassAspect` unit tests
      in `Test_Aspects.pf` (mirroring that file's existing `UnitsAspect`
      unit-test style), which call `set_field_dictionary_config` directly
      to exercise both modes deterministically and in isolation. No
      synthetic `FieldDictionary` YAML fixture was needed for this
      approach (the tests exercise `matches()` directly, not
      dictionary-lookup-driven defaulting, which is already covered by
      `Test_FieldDictIntegration.pf`).
- [x] 6.2 `Test_Aspects.pf:test_standardname_mismatch_strict` - Import/Export
      disagreeing `standard_name`, `STRICT` mode -> `can_connect_to` is
      `.false.`.
- [x] 6.3 `Test_Aspects.pf:test_standardname_mismatch_permissive` - same
      disagreement, `PERMISSIVE` mode -> `can_connect_to` is `.true.`
      (warning logged, not asserted).
- [x] 6.4 `Test_Aspects.pf:test_standardname_wildcard_import` - Import
      declares no `standard_name` (unchecked/wildcard) -> accepts any
      Export, in both `STRICT` and `PERMISSIVE` mode.
- [x] 6.5 `Test_Aspects.pf:test_standardname_export_unset` - Export declares
      no `standard_name`, Import declares one -> connects in both modes
      (warning logged, not asserted).
- [x] 6.6 Update `Test_FieldDictIntegration.pf`: rewrite
      `test_no_fd_lookup_when_flag_absent` (no longer applicable - lookup is
      now unconditional) and confirm `test_units_and_long_name_filled_from_dict`/
      `test_caller_units_override_dict`/`test_multiple_fields_from_dict`
      still pass without `use_field_dictionary=.true.`. Per design.md
      Decision 6/Non-Goals, dictionary defaulting is explicitly NOT extended
      to a Vector's compound-encoded `standard_name` - no new Vector
      defaulting test is needed; add a per-component Vector *agreement*
      test instead (task 6.9).
- [x] 6.9 `Test_Aspects.pf:test_vector_standardname_partial_mismatch_strict`/
      `test_vector_standardname_full_match` - two `VectorClassAspect`s whose
      compound `standard_name`s agree in component 1 but disagree in
      component 2 fail to connect in `STRICT` mode; a fully-agreeing pair
      connects cleanly.
- [x] 6.7 Update `Test_FieldDictionary.pf`'s existing `FieldDictionaryConfig`/
      `ValidationMode` tests (lines 444-516) if the singleton
      accessors (task 4.1) change their public shape; add a test for
      `get_field_dictionary_config()`'s default-when-unset behavior.
- [x] 6.8 Update the `names_1` scenario
      (`superstructure/generic/tests/scenarios/names_1`) and
      `Test_Scenarios.pf`'s `check_field_standard_name` to reflect
      single-shared-value `standard_name` semantics (no more "each endpoint
      declares independently" case for `standard_name`; keep it for
      `long_name`).

## 7. Regression pass

- [x] 7.1 Grepped all `MAPL_FieldGet(..., standard_name=`/
      `MAPL_FieldSet(..., standard_name=` call sites outside
      generic/tests: `infrastructure/geom_io/SharedIO.F90:131` (read-only,
      for I/O metadata - reading the field-wide value is correct, no
      change needed) and `gridcomps/history/HistoryCollectionGridComp_private.F90:143-144`
      (copies metadata from an old field to a newly-created field - a
      field-wide read+write is correct/simpler than the prior per-alias
      behavior; updated its stale comment to stop describing standard_name
      as per-alias). No other internal readers of
      `FieldClassAspect%standard_name` remain (removed in task 3.1).
- [x] 7.2 Built and ran `MAPL.generic.scenarios` (316/316) and
      `MAPL.generic.core` ctest suites - both pass (NAG build). Root-caused
      and fixed an unrelated build-system issue found along the way: stale
      compiled objects for files that `use`d changed modules across
      library/target boundaries (e.g. `gridcomps/statistics/StatisticsGridComp.F90`,
      `superstructure/generic/tests/gridcomps/ProtoStatGridComp.F90`) were
      not being recompiled by incremental `cmake --build`, causing a
      (misleading) segfault unrelated to any logic bug; resolved by forcing
      a full rebuild.
- [x] 7.3 Ran `Test_FieldDictionary.pf`, `Test_FieldDictIntegration.pf`,
      `Test_FieldNamedAlias.pf`, and `Test_FieldCreate.pf` individually (all
      part of `MAPL.field.test_fieldcreate`/`MAPL.generic.core`) - all pass
      after updating `Test_FieldNamedAlias.pf`'s two tests that asserted
      the old per-alias `standard_name` default/independence (now
      field-wide - see task 7.1's design.md Risk). `Test_AddVarSpec.pf` is
      an empty placeholder module (no tests, not part of any ctest target)
      - nothing to verify there. Full `ctest --test-dir nag` run: 67/74
      pass; the 7 failures (`ll-ll`, `cs-cs`, `cs-ll`, `ll-cs`,
      `MAPL3G_Comp_Test_case02/11/23`) are pre-existing/environmental
      (missing datasets, stale libraries per platform), confirmed
      unrelated to this change and present on the unmodified base branch.
- [ ] 7.4 Update `openspec/specs/generic/field-name-propagation/spec.md`
      per this change's delta (drop `standard_name` from the per-endpoint/
      inheritance requirements) when this change is archived.
