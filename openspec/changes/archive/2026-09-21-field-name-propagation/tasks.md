## 1. FieldInfo: per-alias name storage

- [x] 1.1 In `infrastructure/field/FieldInfo.F90`, add a `standard_name`/`long_name`
      per-alias set routine mirroring `field_info_set_internal_restart_mode`
      (namespace `INFO_INTERNAL_NAMESPACE // "/alias" // id_str`), accepting the
      `named_alias_id` plus optional `standard_name`/`long_name` and writing only
      the ones present.
- [x] 1.2 Add the matching per-alias get routine mirroring
      `field_info_get_internal_restart_mode`. Revised from the initial pass:
      rather than returning unallocated outputs and pushing the `'unknown'`
      fallback onto the caller, the getter itself now guarantees every
      *present* output comes back allocated - defaulting internally to
      `'unknown'` when the per-alias key is absent (do not `_ASSERT` on
      absence, unlike the existing unaliased `field_info_get_internal`).
      Sets the `'unknown'` default first, then unconditionally overwrites it
      if the per-alias key is present - avoids a nested if/else.
- [x] 1.3 Wire both into the existing `FieldInfoSetInternal`/`FieldInfoGetInternal`
      generic interfaces (or expose as clearly-named siblings if overload
      resolution is ambiguous with the existing `named_alias_id`/`restart_mode`
      overload).

## 2. FieldClassAspect: constructor default and persistence

- [x] 2.1 In `superstructure/generic/specs/FieldClassAspect.F90`
      (`new_FieldClassAspect`), remove the forced `= 'unknown'` default for
      `standard_name`/`long_name`; leave them unallocated unless the optional
      argument is present.
- [x] 2.2 In `allocate()`, remove `standard_name=`/`long_name=` from the
      `mapl_FieldSet(this%payload, ...)` call; leave `fill_value` handling
      unchanged.
- [x] 2.3 In `add_to_state()`, alongside the existing
      `if (allocated(this%restart_mode)) ...` block that already computes
      `alias_id` via `ESMF_NamedAliasGet`, add a call to the new per-alias setter
      (task 1.1) passing `this%standard_name`/`this%long_name`, guarded by
      `allocated(this%standard_name) .or. allocated(this%long_name)`.

## 3. Predecessor propagation

- [x] 3.1 In `connect_to_export` (`FieldClassAspect.F90`), add a private
      character-string helper analogous to the existing numeric `mirror` (used
      for `fill_value`) with the policy: destination (`this`, the Import side)
      keeps its own value if allocated; otherwise, if the source (`export_`, the
      predecessor) is allocated, adopt it; otherwise leave unallocated. No
      mismatch logging (differing names on both sides is expected, not an error).
- [x] 3.2 Apply the helper to both `standard_name` and `long_name` right after
      `this%payload = export_%payload`.
- [x] 3.3 Confirm `connect_to_import` is left unchanged (do not add reverse
      propagation there) - the Export's own declared name must never be
      overwritten by a downstream Import's declaration.

## 4. Read path

- [x] 4.1 In `infrastructure/field/FieldGet.F90` (`field_get`), for the
      `standard_name`/`long_name` optional outputs, derive `alias_id` via
      `ESMF_NamedAliasGet(field, id=alias_id, _RC)` once (if either is
      present), then call `FieldInfoGetInternal` for `standard_name` and for
      `long_name` in two separately-guarded `if (present(...))` blocks -
      never passing one as an actual argument to the other's keyword when it
      is not present, so neither name is ever referenced unless the caller
      actually asked for it.
- [x] 4.2 Fall back to `'unknown'` when the specific alias id has no per-alias
      value recorded (covers fields that never went through `add_to_state`,
      e.g. hand-built fields in unit tests). Implemented inside
      `field_info_get_internal_names` itself (task 1.2), not in `field_get` -
      `field_get` no longer needs any post-call `allocated(...)` fallback
      logic at all.
- [x] 4.3 Confirm no other internal reader of `FieldClassAspect%standard_name`/
      `long_name` exists that bypasses these paths (grep
      `superstructure/generic/specs/FieldClassAspect.F90` for `%standard_name`/
      `%long_name`).

## 5. Test coverage

- [x] 5.1 Re-run the existing `superstructure/generic/tests/scenarios/names_1`
      scenario against `Test_Scenarios.pf`'s `check_field_standard_name`/
      `check_field_long_name` checks; confirm it now passes with each side's own
      declared name intact.
- [x] 5.2 Extend `names_1` (or add a sibling scenario) with a connection where the
      Import declares no `standard_name`/`long_name`, asserting it reports the
      Export's declared value (covers spec requirement "Unassigned metadata is
      inherited from the connection predecessor").
- [x] 5.3 Add a scenario/case that forces an intermediate transform (e.g. reuse
      the topology style of `scenarios/regrid` or `scenarios/precision_extension`)
      between differently-named Export/Import endpoints, asserting the name
      propagates unchanged through the transform hop.
- [x] 5.4 Confirm the existing status-gating in `check_field_standard_name`/
      `check_field_long_name` (skip check unless `ESMF_FIELDSTATUS_COMPLETE`)
      still matches actual behavior after the persistence-timing change (task 2.3).

## 6. Regression pass

- [x] 6.1 Grep all `MAPL_FieldGet(..., standard_name=` / `long_name=` call sites
      (found so far: `infrastructure/geom_io/SharedIO.F90:131`) and confirm each
      operates on a field obtained from a specific `ESMF_State` context, not a
      pre-alias/root field handle. Confirmed: `SharedIO.F90`'s `add_variable`
      only ever receives fields pulled from a specific `ESMF_FieldBundle`
      (`add_variables` -> `MAPL_FieldBundleGet`), itself sourced from a specific
      state/collection context (e.g. a History collection) - always the correct
      alias for that context. No other call sites found.
- [x] 6.2 Build and run `MAPL.generic.scenarios` and `MAPL.generic.core` ctest
      suites (restart_mode and fill_value code paths share the edited
      subroutines). Both pass (NAG build): `MAPL.generic.scenarios` 306/306
      tests, `MAPL.generic.core` passed.
- [x] 6.3 Run `Test_FieldDictIntegration.pf` and `Test_VariableSpec_private.pf` to
      confirm no fallout from the constructor default change (task 2.1). Both
      pass (part of `MAPL.generic.core`, verified individually above).
