## 1. ExpressionClassAspect: carry its own declared name

- [x] 1.1 In `superstructure/generic/specs/ExpressionClassAspect.F90`, add
      `character(:), allocatable :: standard_name` and `long_name` members to the
      `ExpressionClassAspect` type.
- [x] 1.2 Update `new_ExpressionClassAspect` to accept optional `standard_name`/
      `long_name` and store them when present (leave unallocated otherwise, matching
      `FieldClassAspect`'s current unassigned-by-default convention).
- [x] 1.3 Add `get_standard_name()`/`get_long_name()` functions returning
      `character(:), allocatable` (unallocated when not set).
- [x] 1.4 In `superstructure/generic/specs/VariableSpec.F90` (`make_ClassAspect`,
      `MAPL_STATEITEM_EXPRESSION%ot` case), pass `standard_name=this%standard_name,
      long_name=this%long_name` into the `ExpressionClassAspect(...)` constructor call.

## 2. Generic metadata hand-off hook

- [x] 2.1 In `superstructure/generic/specs/StateItemAspect.F90`, add a new
      non-deferred, default-no-op method `inherit_descriptive_metadata(this, predecessor,
      rc)` to the `StateItemAspect` base type, placed alongside the existing default
      `connect_to_import`.
- [x] 2.2 Add matching default (unallocated-returning) `get_standard_name()`/
      `get_long_name()` functions to the base type so any aspect can be queried
      uniformly (`FieldClassAspect`/`ExpressionClassAspect` override them; everything
      else keeps the default).
- [x] 2.3 In `superstructure/generic/specs/FieldClassAspect.F90`, override
      `inherit_descriptive_metadata`: if `this%standard_name`/`long_name` is not
      allocated and `predecessor%get_standard_name()`/`get_long_name()` returns an
      allocated value, adopt it - the same "own value wins, else inherit" policy as the
      existing `mirror_name` helper in `connect_to_export` (reuse/extract that helper
      rather than duplicating the logic). Override `get_standard_name`/`get_long_name`
      to expose `this%standard_name`/`this%long_name`.
- [x] 2.4 In `superstructure/generic/specs/StateItemSpec.F90` (`make_extension`), call
      `call dst_aspect%inherit_descriptive_metadata(src_aspect, _RC)` immediately before
      `call new_spec%set_aspect(dst_aspect, _RC)`.

## 3. Test coverage

- [x] 3.1 Add a new scenario reproducing the user's case: an `expression:` export with
      its own `standard_name`/`long_name`, consumed via an implicit same-name connection
      (i.e. exercised the same way History's `var_list: {source: ...}` does - a sibling
      component declaring an import with the same short name and no metadata of its
      own, wired automatically rather than via an explicit `connections:` entry).
      Assert the consumer's connection point reports the exporter's declared
      `standard_name`/`long_name` once the field is `ESMF_FIELDSTATUS_COMPLETE`. Done:
      new `scenarios/expression_match/` (`A` exports expression `E_sum` with its own
      name, `B` imports `E_sum` with no name of its own, wired via a `connections:`
      entry with `all_unsatisfied: true` - the same `MatchConnection` mechanism
      `MAPL_GridCompConnectAll`/History's `var_list` sourcing use). `B`'s `E_sum`
      correctly reports the exporter's declared `standard_name`/`long_name`.
- [x] 3.2 Add a scenario (or extend an existing one, e.g. `scenarios/expression`) that
      connects an `expression:` export to an import via an explicit `connections:` entry
      where the import declares no `standard_name`/`long_name`, asserting the import's
      connection point reports the exporter's declared values. Done: extended
      `scenarios/expression` with a second pair `expr2` (declares its own name) ->
      `I2` (declares none); `I2` (and the shared extension `A/expr2(1)`) correctly
      inherit `expr2`'s declared `standard_name`/`long_name`.
- [x] 3.3 Confirm an `expression:` export whose consumer *does* declare its own
      `standard_name`/`long_name` is unaffected by this change. Done via the existing
      `expr`/`I` pair in `scenarios/expression` (gave `I` its own `standard_name` *and*
      `long_name`): the shared extension aspect for that connection (`A/expr(1)` and
      `B`'s own `I`) keeps `I`'s own declared values throughout, unaffected by `expr`'s
      declared name. Note: because any expression connection requires a coupler, the
      exporter's own view and the consumer's view resolve to the *same* materialized
      aspect (this is pre-existing behavior, not something introduced by this change -
      see the equivalent `A/expr(1)`/`I` pairing in `expectations.yaml`), so this checks
      "own value always wins" at that single shared point rather than at two
      independently-preserved locations.

## 4. Verification

- [x] 4.1 Build (NAG) and run `MAPL.generic.scenarios` and `MAPL.generic.core`; confirm
      no regressions in the existing `names_1` scenario or elsewhere. Full `build-tests`
      succeeded; all `MAPL.generic.*` suites pass (`scenarios` 316/316, `core`,
      `transforms`, `vertical`, `aspects`, `components`). Ran the full `ctest` suite too:
      7 unrelated pre-existing failures (`ll-ll`/`cs-cs`/`cs-ll`/`ll-cs` - missing
      `LOCAL_REGRESSION_DATA_DIR`; `case02`/`case11`/`case23` - missing Python
      `netCDF4` package for `--narrow` dry-run verification), confirmed environmental
      and unrelated to this change.
- [x] 4.2 Re-ran the user's original repro; initially still showed `"unknown"` despite
      tasks 1-3 working correctly in isolation. Root-caused via targeted debug
      instrumentation (added, verified, then fully removed - no net diff from the
      investigation itself) to a separate, pre-existing gap in the per-alias
      namespacing design from `field-name-propagation`: `HistoryCollectionGridComp_
      private.F90`'s `create_alias_field` builds History's output-bundle field via
      `ESMF_FieldCreate(old_field, dataCopyFlag=ESMF_DATACOPY_REFERENCE, name=alias)`
      + a wholesale `ESMF_InfoSet(new_info, key="", value=info)` copy - not
      `ESMF_NamedAlias` - so the new field has its own alias id (`0`, untracked)
      distinct from the id under which the original field's name was written; the
      wholesale-copied Info entry under the *old* id is never looked up. Fixed per
      task 5 below (user-directed: id 0 is still a valid, self-consistent scope - no
      need to special-case it). Re-ran the repro after the fix:
      `E_sum:standard_name = "foo"` / `E_sum:long_name = "bar"` in
      `ncdump -h test_20040102_0000.nc4`, matching `GCM1.yaml`'s declaration; data
      values unchanged (`E_sum = 5`).

## 5. Fix alias-id mismatch for ESMF_FieldCreate-duplicated fields (History output)

Added during task 4.2's investigation (scope expansion authorized by user after the
pause report above) - not an expression-specific bug, but the reason the fix from
sections 1-2 wasn't visible in the user's actual repro. `MAPL_FieldGet`'s read side
(from `field-name-propagation`) scopes standard_name/long_name by the field's own
`ESMF_NamedAliasGet` id; `MAPL_FieldSet`'s write side still wrote to the single
unaliased slot, so any writer duplicating a field via `ESMF_FieldCreate` (which gets
its own, different id) rather than `ESMF_NamedAlias` (which is scoped by the
*original* field's id) would write/read from mismatched slots.

- [x] 5.1 In `infrastructure/field/FieldSet.F90` (`field_set`), stop routing
      `standard_name`/`long_name` through the unaliased `FieldInfoSetInternal` call;
      instead, when either is present, derive `alias_id` via
      `ESMF_NamedAliasGet(field, id=alias_id, _RC)` and write through the alias-scoped
      overload - symmetric with `FieldGet.F90`'s existing read-side fix. `id=0` (a
      field never placed via `ESMF_NamedAlias`) is treated as a valid, ordinary scope,
      not a special case.
- [x] 5.2 In `gridcomps/history/HistoryCollectionGridComp_private.F90`
      (`create_alias_field`), after the wholesale `ESMF_InfoSet` copy, explicitly
      `MAPL_FieldGet` the `standard_name`/`long_name` from `old_field` and
      `MAPL_FieldSet` them onto `new_field`, re-establishing them under `new_field`'s
      own (id=0) scope so the later `SharedIO.F90` read (same field, same id) finds
      them.
- [x] 5.3 Rebuild (NAG), run the full `ctest` suite (`build-tests` clean; all
      `MAPL.generic.*`, `MAPL.field.*`, `MAPL.history.tests` pass; same 7 pre-existing
      environmental failures as before this change, unrelated: `ll-ll`/`cs-cs`/
      `cs-ll`/`ll-cs` missing `LOCAL_REGRESSION_DATA_DIR`, `case02`/`case11`/`case23`
      missing Python `netCDF4`). Re-ran the user's repro end-to-end (task 4.2) to
      confirm the fix.
