## 1. Implement mirror resolution in ExpressionClassAspect::create

- [x] 1.1 In `superstructure/generic/specs/ExpressionClassAspect.F90`, in
      `create`, compute
      `expression_variables = parser_variables_in_expression(this%expression)`
      (same call already used in `activate`/`make_transform`).
- [x] 1.2 Guard: only proceed if `expression_variables%size() > 0` (constant
      expressions are unaffected - see design.md Decisions/Non-Goals).
- [x] 1.3 Check whether the item's own vertical stagger (via
      `other_aspects%at(VERTICAL_GRID_ASPECT_ID)`,
      `to_VerticalGridAspect`/`get_vertical_stagger`) equals
      `VERTICAL_STAGGER_INVALID` (i.e. `vertical_dim_spec` was omitted). If so,
      retrieve the pointer to that same aspect entry and call
      `set_characteristic_state(ASPECT_STATUS_MIRRORED)` on it, forcing a
      genuinely unresolved (mirrored) state instead of whatever
      `VariableSpec::make_VerticalGridAspect` computed.
- [x] 1.4 Leave the explicit-`vertical_dim_spec` case untouched (stagger not
      `VERTICAL_STAGGER_INVALID` → no change to `create()`'s existing
      behavior).

## 2. Implement best-effort consistency check in ExpressionClassAspect::make_transform

- [x] 2.1 Alongside the existing per-variable loop in `make_transform` (the
      one that builds `inputs`/couplers via
      `parser_variables_in_expression`/`this%registry%get_primary_spec`),
      collect each referenced variable's current `VerticalGridAspect`.
      (Implemented as a separate `check_vertical_stagger_consistency`
      subroutine, called from `make_transform`, doing its own lookup pass.)
- [x] 2.2 For each variable, skip it if `is_mirror()` is true (not yet
      resolved at this point - see design.md Decisions for why this is
      best-effort, not a hard requirement).
- [x] 2.3 Among variables whose stagger is resolved, compare them to each
      other; on any disagreement, `_FAIL` with a message naming the
      conflicting variables and their staggers.
- [x] 2.4 Also compare the resolved variables' stagger(s) against the
      expression's own resolved stagger (via `other_aspects`, which is
      already resolved by the time `make_transform` runs for the
      `CLASS_ASPECT_ID` entry - see design.md Context); `_FAIL` with a message
      identifying the conflict if they disagree.
- [x] 2.5 Confirmed via unit tests: this check does not run (and does not
      fail) when there are zero referenced variables, or when every
      referenced variable is still unresolved
      (`test_consistency_check_skips_unresolved_variable`).

## 3. Regression tests

- [x] 3.1 Remove the `vertical_dim_spec: NONE` line from the `expr` item in
      `superstructure/generic/tests/scenarios/expression/A.yaml`.
- [x] 3.2 Build (NAG, `module load nag-stack`) and run the
      `MAPL.generic.scenarios` ctest target; confirm the `expression`
      scenario's `value` and `vertical_profile` checks both pass.
- [x] 3.3 Confirmed no regression in `expression_defer_geom` (its `expr` keeps
      an explicit `vertical_dim_spec`, so the new `create()` branch never
      fires there) - part of the same passing `MAPL.generic.scenarios` run.
- [x] 3.4 Added unit tests (`Test_ExpressionClassAspect.pf`, new file, added
      to `MAPL.generic.aspects`) covering: mirror forced when omitted with
      variables present; constant expression left alone; explicit value left
      alone; consistency check passes/fails/skips-unresolved. A full-scenario
      (YAML) negative test was not added, since the `Test_Scenarios.pf`
      harness has no support for expected-initialization-failure scenarios
      (any `_RC`-propagated failure registers the whole pFUnit test as
      failed) - the unit tests exercise `check_vertical_stagger_consistency`
      directly instead, including the exact failure message.
- [x] 3.5 Confirmed no regression in `Test_ComponentSpecParser.pf`'s
      expression-related cases (part of the passing `MAPL.generic.components`
      run).

## 4. Full verification

- [x] 4.1 Ran the full `MAPL.generic` ctest suite (NAG): all 6 targets
      (`scenarios`, `transforms`, `vertical`, `aspects`, `components`, `core`)
      pass. Ran the full repo-wide ctest suite (74 tests): 7 failures, all
      pre-existing/platform-related (regrid regression apps `ll-ll`/`cs-cs`/
      `cs-ll`/`ll-cs`, and `MAPL3G_Comp_Test_case02/11/23` - the latter
      visibly failing on a missing `netCDF4` Python package), none in the
      `generic`/`vertical`/`expression` area this change touches.
- [x] 4.2 Ran `openspec validate --change derive-expression-vertical-dim-spec --strict` -
      valid.

## 5. Follow-up: replace mirror-forcing with direct resolution

Tasks 1-4 above shipped and passed CI, but `MAPL3G_Comp_Test_case21` (ExtData's
"Derived" export feature) subsequently failed on `develop`/downstream CI: the
`create()`-time `ASPECT_STATUS_MIRRORED` forcing from task 1 does not survive
`StateItemSpec::set_geometry`'s unconditional per-registry override once the
owning component declares its own vertical grid resource - see design.md
Context, approach 4, for the full diagnosis. This section replaces tasks 1-2's
implementation; the regression tests from task 3 remain valid (updated where
their assertions checked `is_mirror()` instead of the actual resolved
stagger) and task 4's verification was re-run against the fix below.

- [x] 5.1 Removed `create()`'s `set_characteristic_state(ASPECT_STATUS_MIRRORED)`
      forcing entirely (task 1.3 above); `create()` no longer touches
      `characteristic_state`.
- [x] 5.2 Changed `check_vertical_stagger_consistency`'s signal for "this
      expression's own value was not actually declared" from `is_mirror()`
      alone to `vertical_stagger == VERTICAL_STAGGER_INVALID .or.
      is_mirror()` - see design.md Context/Decisions for why either signal
      alone is insufficient.
- [x] 5.3 When that signal is true and a consistent stagger (and, if needed, a
      vertical grid) is found among resolved referenced variables,
      `check_vertical_stagger_consistency` now adopts it directly onto the
      expression's own `VerticalGridAspect` (`set_vertical_stagger`/
      `set_vertical_grid`), rather than relying on the generic mirror/
      `ExtendTransform` machinery to pick up a forced `characteristic_state`.
- [x] 5.4 `create()` now calls `check_vertical_stagger_consistency` directly
      (best-effort - most referenced variables are not registered yet at this
      point), in addition to the existing call from `make_transform`
      (authoritative, at actual connection time).
- [x] 5.5 Changed the per-variable registry lookup to skip (not error on) a
      referenced variable not yet present in the registry at all
      (`has_virtual_pt` guard before `get_primary_spec`), since `create()` now
      runs this check much earlier than `make_transform` did alone.
- [x] 5.6 Updated `Test_ExpressionClassAspect.pf`: replaced the
      `is_mirror()`-based assertion for the omitted-with-agreeing-variables
      case with a direct check of the resolved stagger value; updated the
      mismatch test to register the disagreeing variables *after*
      `make_expr_item` (so `create()`'s own attempt finds nothing yet, and the
      mismatch is instead caught by the later, explicit call, keeping a single
      clean exception for `@assertExceptionRaised` rather than the multi-level
      `_VERIFY` trace produced by letting `create()` itself fail through the
      test-mode auto-asserting error macros); added a test for the
      not-yet-registered-variable skip.
- [x] 5.7 Re-ran full verification (via `ctest`, not direct binary
      invocation - the latter produces spurious, unrelated failures for this
      test binary regardless of this change): `MAPL.generic.scenarios` and
      `MAPL.generic.aspects` both pass; `MAPL3G_Comp_Test_case21` passes; full
      repo ctest suite is back to exactly the 7 known pre-existing
      environment failures (plus 2 confirmed-unrelated local-only issues: a
      NAG license error and a pre-existing `MAPL.GeomIO.tests` failure, both
      present identically on unmodified `develop`).
