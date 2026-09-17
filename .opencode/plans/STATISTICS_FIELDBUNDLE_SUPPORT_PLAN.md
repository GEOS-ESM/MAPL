# Statistics GridComp: ESMF_FieldBundle Support Plan

**Document Version:** 2.0
**Date:** 2026-09-17
**Status:** Field/Bundle support for TimeAverage, TimeMax, TimeMin, TimeAccumulate is
implemented AND now covered by an expanded `statistics_real` scenario test (all 4 actions
x both item types). TimeVariance + covariance kernels remain deferred (unstarted).

## Context and Motivation

`gridcomps/statistics/` implements a family of time-statistics gridded components
(`TimeAverage`, `TimeMax`, `TimeMin`, `TimeAccumulate`, `TimeVariance`), all extending
the abstract type `AbstractTimeStatistic` (`AbstractTimeStatistic.F90`). Each type wraps
one input quantity and produces one output quantity, updated every timestep and
"finalized" (averaged/reset/etc.) when its `MAPL_SimpleAlarm` rings.

Originally these all operated only on single `ESMF_Field`s. The goal of this effort is to
generalize them to also work when the import/export quantity is an `ESMF_FieldBundle`
(e.g. a vector quantity made of multiple component fields), driven by the `itemtype:
field|vector` setting in the statistics YAML config (`statistics.yaml`), which
`StatisticsGridComp.F90` maps to `MAPL_STATEITEM_FIELD` / `MAPL_STATEITEM_FIELDBUNDLE`
(`ESMF_StateItem_Flag`).

## Design Pattern (established with TimeAverage, replicated for TimeMax/Min/Accumulate)

For each `TimeXxx` type:

1. Add `logical :: is_bundle = .false.` component to the derived type, plus bundle-typed
   input/output components (`type(ESMF_FieldBundle) :: b`, `type(ESMF_FieldBundle) ::
   <result>_b`) alongside the existing `esmf_Field` components.
2. Add a second constructor `new_TimeXxx_fieldbundle(unusable, gridcomp, b, <result>_b,
   alarm, rc)` mirroring the field constructor but using `mapl_FieldBundleGet` /
   `mapl_fieldbundleset` (instead of `mapl_FieldGet`/`mapl_FieldSet`) to propagate
   geom/ungridded_dims/units/typekind/vgrid/vert_staggerloc metadata onto the output
   bundle and onto the internal-state bundle(s). Both constructors are combined via a
   generic `interface TimeXxx`.
3. Change `advertise_time_xxx_internal_fields(gridcomp, name, rc)` to
   `advertise_time_xxx_internal_fields(gridcomp, name, item_type, rc)` where `item_type`
   is `type(ESMF_StateItem_Flag), intent(in)`, and pass `itemtype=item_type` into every
   `MAPL_GridCompAddSpec(..., ESMF_STATEINTENT_INTERNAL, ...)` call. This causes the
   framework to mirror the *structure* (member fields) of the associated import bundle
   into the internal-state bundle once realized — this is what makes it valid to later
   call `MAPL_FieldBundleGet(internal_bundle, fieldList=...)` and get one member per
   input-bundle member, in the same add-order.
4. `destroy`: branch on `is_bundle` -> `MAPL_FieldBundleDestroy(this%<result>_b, rc)` vs
   `esmf_FieldDestroy(this%<result>_f, rc)`.
5. `reset`: branch on `is_bundle`. Bundle-mode variant fetches internal bundle(s) via
   `esmf_StateGet(internal_state, '<prefix>_'//name, fieldbundle=..., rc)` (where `name`
   now comes from `mapl_FieldBundleGet(this%b, short_name=name)`), extracts `fieldList`
   via `MAPL_FieldBundleGet(..., fieldList=...)`, and loops `do i = 1, size(fieldlist)`
   doing the same per-field math as the field-mode branch (`esmf_FieldFill`,
   `MAPL_AssignFptr`, etc.).
6. `update`/`update_r4`/`update_r8`: `update` determines `typekind` from
   `mapl_FieldBundleGet(this%b, typekind=typekind)` when `is_bundle`, else
   `mapl_FieldGet(this%f, typekind=typekind)` (assumes **uniform typekind across all
   bundle members** — confirmed acceptable design decision). Dispatches to new
   `update_bundle_r4`/`update_bundle_r8` subroutines (duplicated logic, not shared with
   `update_r4`/`update_r8`, per explicit decision below) which loop
   `MAPL_FieldBundleGet(..., fieldList=...)` in parallel across `this%b` and the
   relevant internal bundle(s), calling `MAPL_AssignFptr` + the same `where`-block math
   per member field index `i`.
7. `compute_result`/`compute_result_r4`/`compute_result_r8`: same pattern, with new
   `compute_result_bundle_r4`/`compute_result_bundle_r8` looping over `this%<result>_b`
   and the internal bundle(s) in parallel.

### Key decisions made (via user Q&A during this effort)

- **Discriminator:** plain `logical :: is_bundle` component (not an `ESMF_StateItem_Flag`
  field) — simpler, sufficient since the type only ever wraps Field-or-Bundle.
- **No shared/refactored field-level helpers:** bundle-mode math is duplicated in new
  `*_bundle_r4`/`*_bundle_r8` subroutines rather than factoring the `where`-block math
  out into small per-field helpers called from both paths. This keeps each code path
  self-contained at the cost of some duplication.
- **Uniform typekind per bundle:** we do NOT check typekind per-member-field inside the
  loop; we assume the whole bundle is one typekind (matches what
  `mapl_FieldBundleGet(bundle, typekind=...)` already returns as a single value for the
  whole bundle).
- **Fieldlist ordering:** we rely on `MAPL_FieldBundleGet`'s `ESMF_ITEMORDER_ADDORDER`
  fieldList ordering being consistent across the input bundle, the output bundle, and
  the internal-state bundle(s) — i.e. we match up members purely by index `i`, with **no
  explicit name-matching/verification**. This is consistent with how `mapl_fieldbundleset`
  propagates metadata to members (FieldBundleSet.F90) and how the framework mirrors
  structure via `MAPL_GridCompAddSpec(..., itemtype=MAPL_STATEITEM_FIELDBUNDLE, ...)`.
  Confirmed independently: `VectorClassAspect.F90` always splits a `class: vector` item
  into exactly 2 member fields, added to the bundle in a fixed order (component 1 =
  first name in the parenthesized `standard_name`, e.g. `eastward_wind`/U; component 2 =
  `northward_wind`/V), and member fields are **never given a distinguishing ESMF name**
  (`ESMF_FieldEmptyCreate()` with no `name=`) — add-order index is the only reliable way
  to address them. `gridcomps/componentDriverGridComp/componentDriverGridComp.F90` uses
  an analogous `"<bundle>;comp_<j>"` flat-key convention for the same reason, if a
  precedent is ever needed elsewhere.

## Work Completed (statistics gridcomp itself)

### `TimeAverage.F90` (done in an earlier session)
- Added `is_bundle`, `b`, `avg_b` components; `new_TimeAverage_fieldbundle` constructor.
- `destroy`, `reset` (split into `reset_field`/`reset_bundle`), `update` (+
  `update_bundle_r4`/`update_bundle_r8`), `compute_result` (+
  `compute_result_bundle_r4`/`compute_result_bundle_r8`) all bundle-aware.
- Internal fields: `sum_<name>`, `counts_<name>` (both mirrored as bundles when
  `is_bundle`).

### `TimeMax.F90`
- Added `is_bundle`, `b`, `max_b`; `new_TimeMax_fieldbundle`.
- Internal field: `temp_max<name>` (note: no underscore between `temp_max` and `name` —
  pre-existing naming quirk, preserved as-is).
- `destroy`, `reset`/`reset_field`/`reset_bundle`, `update`/`update_bundle_r4`/
  `update_bundle_r8`, `compute_result`/`compute_result_bundle_r4`/
  `compute_result_bundle_r8` all implemented.
- `advertise_time_max_internal_fields` signature changed to accept `item_type`.
- **Bug fixed this session:** `advertise_time_max_internal_fields` was seeding the
  internal `temp_max<name>` field with `fill_value=0.0` at advertise time instead of
  `MAPL_UNDEF` (the sentinel `reset()` uses for every period after the first). Since
  `reset()` is never called before the very first accumulation period completes, the
  first period's max was silently computed starting from `0.0` instead of "no value
  yet" — invisible for all-positive test data (masked bug), but incorrect in general
  (e.g. an all-negative input series would wrongly report a max of `0.0`). Fixed to
  `fill_value=MAPL_UNDEF`, matching `reset()`'s convention. (`gridcomps/statistics/TimeMax.F90:589`)

### `TimeMin.F90`
- Mirror of TimeMax with `min`/`min_f`/`min_b`/`temp_min<name>`.
- **Same fill_value bug found and fixed** (`gridcomps/statistics/TimeMin.F90:589`) —
  this one was NOT masked: with the test's ascending `1..24` series, `min(0.0, f)`
  stays `0.0` forever once seeded at `0.0`, so the very first `min` field-mode
  scenario-test run this session actually caught it (`QV_MIN` expected `1.`, got `0.`).
  This is what led to discovering/fixing the bug in both files.

### `TimeAccumulate.F90`
- Added `is_bundle`, `b`, `accum_b`; `new_TimeAccumulate_fieldbundle`.
- Internal field: `sum_<name>` (only one internal field, no counts).
- `reset` here was NOT split into separate `reset_field`/`reset_bundle` subroutines
  (unlike the others) — it's simple enough (`esmf_FieldFill` only, no `MAPL_AssignFptr`
  pointer math) that it was inlined as a single `if (this%is_bundle) then ... else ...
  end if` block directly in `reset`.
- `update`/`update_bundle_r4`/`update_bundle_r8`,
  `compute_result`/`compute_result_bundle_r4`/`compute_result_bundle_r8` implemented.
- `advertise_time_accumulate_internal_fields` signature changed to accept `item_type`.
- No fill_value bug here — internal `sum_<name>` correctly starts at `0.0`, which is the
  correct neutral element for a running sum (accumulate/average both start from 0
  legitimately; only min/max need a sentinel-not-a-number starting value).

### `StatisticsGridComp.F90`
- `advertise_item`'s `select case (action)`: the `'min'`, `'max'`, `'accumulate'` cases
  pass `itemtype=item_type` to the EXPORT `MAPL_GridCompAddSpec` call and forward
  `item_type` into their respective `advertise_time_*_internal_fields` calls (`'average'`
  did this already).
- `make_min_stat`, `make_max_stat`, `make_accumulate_stat`: dispatch on
  `MAPL_STATEITEM_FIELD` vs `MAPL_STATEITEM_FIELDBUNDLE` (via
  `mapl_StateGet(importState, itemName=name, itemtype=itemtype)`) to construct either
  the Field or FieldBundle variant of the stat, mirroring `make_average_stat`.
- `'variance'` case and `make_variance_stat` remain untouched (still Field-only) — see
  "Deferred Work" below.
- Stray pre-existing debug output (`_HERE, ' bmaa '` calls in `make_average_stat`/
  `make_item`) is STILL present — not yet cleaned up (see Remaining Tasks #5). It prints
  to stdout during every `statistics_real` scenario test run (`StatisticsGridComp.F90
  195/228/239  bmaa`) — harmless noise, not a correctness issue.

## Work Completed (test coverage — this session, 2026-09-17)

Previously **no** scenario test exercised FieldBundle/vector support at all, and the
Field-mode `min`/`accumulate` actions had never been numerically verified either
(`PS`'s pre-existing stat was actually testing `action: max`, despite being named/wired
downstream as `PS_min` — a leftover naming bug, now fixed, see below). This session
expanded `superstructure/generic/tests/scenarios/statistics_real/` into a full
"all 4 actions x both item types" test matrix, and along the way found + fixed the
TimeMin/TimeMax `fill_value` bug above and a real gap in the shared scenario-test
driver's bundle support.

### `superstructure/generic/tests/scenarios/statistics_real/A.yaml`
Added 5 new synthetic export quantities (all driven by the same `1..24`/`0..23` ramp
series already used by `TS`/`PS`/`UV`, via `ConfigurableGridComp`'s `run:` section):
- `QV_MIN` (field) — tests `action: min`
- `SLP_ACCUM` (field) — tests `action: accumulate`
- `UV_MIN`, `UV_MAX`, `UV_ACCUM` (vector/bundle) — test `min`/`max`/`accumulate` on a
  bundle, alongside the pre-existing `UV` (average/bundle).

For bundle quantities, the `run:` value is a **list of per-member sequences** (one
sub-list per bundle add-order member, each itself a per-timestep sequence) — e.g.:
```yaml
UV_MIN:
  - [1, 2, 3, ..., 24]   # component 1 (U)
  - [0, 1, 2, ..., 23]   # component 2 (V)
```

Full matrix now tested (all using the `daily` alarm / `numsteps: 24` in `cap.yaml`, so
each stat completes exactly one period by the end of the run):

| Quantity | Type | Action | Expected value(s) |
|---|---|---|---|
| `TS` | field | average | 12.5 |
| `PS` | field | max | 24 |
| `QV_MIN` | field | min | 1 |
| `SLP_ACCUM` | field | accumulate | 300 |
| `UV` | vector | average | [12.5, 11.5] |
| `UV_MIN` | vector | min | [1, 0] |
| `UV_MAX` | vector | max | [24, 23] |
| `UV_ACCUM` | vector | accumulate | [300, 276] |

### `superstructure/generic/tests/scenarios/statistics_real/stat.yaml`
Added 5 new `stats:` entries (`A/QV_MIN` min/field, `A/SLP_ACCUM` accumulate/field,
`A/UV_MIN` min/vector, `A/UV_MAX` max/vector, `A/UV_ACCUM` accumulate/vector), all using
`<<: *daily`. **Renamed nothing here** — `PS`'s entry was already `action: max`; only
its *downstream* name was wrong (see next item).

### `superstructure/generic/tests/scenarios/statistics_real/history.yaml`
- **Fixed pre-existing naming bug:** `dst_name: PS_min` → `dst_name: PS_max` (the `PS`
  stat's `action` was always `max`; the downstream connection name was simply wrong/
  misleading since some earlier session, `PS_min` computed and correctly matched a `max`
  value of `24.` under a confusingly-`_min`-suffixed name).
- Added 5 new connections routing the new `stat` outputs into `collection_1`, using the
  **same name on both ends** (no extra action suffix, since the source name already
  encodes the action): `A/QV_MIN→QV_MIN`, `A/SLP_ACCUM→SLP_ACCUM`, `A/UV_MIN→UV_MIN`,
  `A/UV_MAX→UV_MAX`, `A/UV_ACCUM→UV_ACCUM`.

### `superstructure/generic/tests/scenarios/statistics_real/collection_1.yaml`
- Renamed `PS_min` import key → `PS_max`.
- Added import declarations: `QV_MIN`, `SLP_ACCUM` (plain fields, `vertical_dim_spec:
  MIRROR`); `UV_MIN`, `UV_MAX`, `UV_ACCUM` (`class: vector`, `vertical_dim_spec: MIRROR`,
  mirroring `UV_avg`'s existing declaration).

### `superstructure/generic/tests/scenarios/statistics_real/expectations.yaml`
- Renamed `PS_min` → `PS_max` (value unchanged, `24.`).
- Added `status`/`class`/`value` checks for all 5 new quantities under
  `history/collection_1/<user>` (with numeric `value:`) and `history/collection_1`
  (status/class only), plus `A/QV_MIN`/`A/SLP_ACCUM`/`A/UV_MIN`/`A/UV_MAX`/`A/UV_ACCUM`
  entries under both `history/stat/<user>` and `history/stat` (mirroring the existing
  `A/TS`/`A/PS`/`A/UV` pattern, including numeric value checks on the `<user>` export
  side).
- Bundle `value:` checks use a **YAML list matching bundle add-order**, e.g.
  `UV_MIN: {status: complete, class: bundle, value: [1., 0.]}`.

### `superstructure/generic/tests/Test_Scenarios.pf` (the shared scenario-test driver)
This was the real gap uncovered by trying to write the above: **every** check routine
(`check_field_status`, `check_field_typekind`, `check_field_value`, `check_field_rank`,
`check_field_geom_name`, `check_field_vertical_profile`) previously started with:
```fortran
itemtype = get_itemtype(state, short_name, _RC)
if (itemtype /= ESMF_STATEITEM_FIELD) then ! that's ok
   rc = 0; return   ! silently PASSED without checking anything
end if
```
So `status:`/`value:` on any bundle-class item was a **silent no-op** — it could never
fail, and never actually verified anything (confirmed empirically: the pre-existing
`vector_1/expectations.yaml` only ever really exercised its `class: bundle` check via
`check_item_type`; its `status: complete` half was dead code).

Changes made:
- **`check_field_status`**: now also handles `ESMF_STATEITEM_FIELDBUNDLE` — fetches the
  bundle, loops `MAPL_FieldBundleGet(bundle, fieldList=...)` members, and checks each
  member's `ESMF_FieldGet(..., status=...)` against the same expected status. Added a
  guard to skip cleanly (not fail) when the `status:` key isn't present at all in the
  expectations block (needed because some pre-existing scenarios, e.g.
  `service_service`/`service_with_options`, declare bundle items with only `class:`/
  `fieldcount:` keys and no `status:` key — previously irrelevant since bundles were
  never really checked; now that they are, the missing-key case must be a no-op, not an
  `ESMF_HConfigAsString` lookup failure).
- **`check_field_value`**: same bundle branch. Expected value may be either a plain
  scalar (broadcast to all bundle members, backward compatible with field-mode) or a
  YAML sequence (`value: [v1, v2, ...]`, sized to match the bundle's member count,
  checked member-by-member in add-order) — refactored the rank/typekind farrayPtr
  comparison logic into a shared internal `check_one_field_value` helper called once
  per field (whether the single field in field-mode, or per-member in bundle-mode).
- **Missing `use` bug found and fixed:** `MAPL_FieldBundleGet` was never actually
  imported into this file's scope (no `use mapl_field_bundle_api`/`use MAPL`-style
  statement) — it was silently resolved as an implicit external procedure with no
  interface, which is legal Fortran *as long as you never call it with keyword
  arguments*. It had never been called with keyword args before (only used inside
  `check_fieldCount`'s dead/narrow code path). Once the new bundle branches called it
  with `fieldList=...`, NAG correctly errored with "Keyword argument requires an
  explicit interface" (misleadingly attributed by line-number to the following
  statement, due to the `_RC` macro's multi-statement-per-line expansion confusing
  NAG's diagnostic location tracking — cost significant time to isolate; confirmed via
  a minimal standalone repro in `/tmp/opencode/mini_test2.pf`). Fixed by adding:
  `use mapl_field_bundle_api, only: MAPL_FieldBundleGet` to the module's `use` list.

### `gridcomps/configurable/ConfigurableGridComp.F90` (`run()` subroutine)
Previously only handled scalar `ESMF_Field` export items in its `run:`-section
value-assignment loop (`esmf_StateGet(exportState, itemName=field_name, field=field,
...)` — would have failed outright once any `run:` key referred to a bundle/vector
item like `UV`). Now:
- Checks `itemtype` via `esmf_StateGet(exportState, itemName=field_name,
  itemtype=itemtype, _RC)` for every `run:` key.
- `ESMF_STATEITEM_FIELD`: unchanged scalar per-timestep assignment.
- `ESMF_STATEITEM_FIELDBUNDLE`: fetches `fieldList` via `MAPL_FieldBundleGet` (add-order),
  and for each member `j` pulls the `j`-th sub-sequence from the YAML value
  (`ESMF_HConfigCreateAt(member_seq_cfg, index=j)`), then the per-timestep scalar from
  that sub-sequence (`ESMF_HConfigAsI4(member_cfg, index=advanceCount+1)`), and assigns
  it into that member field via the same typekind-dispatched `mapl_AssignFptr` logic used
  for scalar fields.

### Verification performed this session
- Full incremental NAG build (`build-debug/`) of `MAPL`, `MAPL.statistics`,
  `configurable_gridcomp`, `build-tests` succeeds cleanly (only pre-existing/benign
  "unused variable" NAG remarks, none newly introduced beyond a couple of expected
  unused-dummy warnings in unrelated pre-existing code).
- `ctest -R MAPL.generic.scenarios`: **all 238 sub-tests pass**, including the full new
  8-combination statistics matrix, and no regressions in `vector_1`, `service_service`,
  `service_with_options` (which now get *real* bundle status checks instead of the old
  silent no-ops).
- `MAPL.statistics.tests`, `MAPL.history.tests`, `configurable_gridcomp.tests`: pass.
- Full `ctest --test-dir build-debug`: 69/74 pass. The 5 failures (`ll-ll`, `cs-cs`,
  `cs-ll`, `ll-cs` Regrid_Util app REGRESSION tests, and `MAPL3G_Comp_Test_case18`) are
  unrelated pre-existing/environmental failures — confirmed no reference to statistics/
  TimeMin/TimeMax/ConfigurableGridComp/Test_Scenarios.pf in any of them, and `case18`
  already had unrelated uncommitted WIP edits (`cap_driver1.yaml`/`history1.yaml`,
  changing `segment_duration`/history frequency) present in the working tree *before*
  this session started — not something introduced by this work.
- **Nothing has been committed to git** (per explicit user instruction throughout this
  session); all changes below are unstaged/uncommitted working-tree edits.

## Deferred Work: `TimeVariance.F90` + Covariance Kernels

`TimeVariance.F90` was explicitly **excluded** from all sessions so far because it is
architecturally different from the other four types and requires an interface change,
not just an additive one. Nothing has changed on this front since v1.0 of this plan.

### Why it's harder

`TimeVariance` delegates all actual per-timestep math to a pluggable
`class(AbstractCovarianceKernel)` (`AbstractCovarianceKernel.F90`), with two concrete
implementations: `WelfordCovarianceKernel.F90` (fields `mux_`, `muy_`, `c_`) and
`ShiftedCovarianceKernel.F90` (fields `kx_`, `ky_`, `ex_`, `ey_`, `exy_`).

Critically, the kernels **do their own internal-state lookups**: given an `esmf_Field
f_x`, each kernel method does:
```fortran
call mapl_FieldGet(f_x, short_name=name, _RC)
call esmf_StateGet(internal_state, 'mux_'//name, field=mux_f, _RC)   ! (Welford example)
```
i.e. it derives an internal-state item name from `f_x`'s own `short_name` and looks it
up as a top-level `esmf_State` item.

This breaks for bundle mode: if we mirror `mux_<bundle_name>` as a `FieldBundle` (the
same trick used for `sum_`/`counts_` in `TimeAverage`) and then loop over member fields
of `this%b` inside `TimeVariance`, each member's own `short_name` will NOT match a
top-level state item `mux_<something>` — and, per the vector-member-naming discovery
above, member fields of a `class: vector` bundle don't even reliably HAVE a distinct
`short_name` at all (they're created via `ESMF_FieldEmptyCreate()` with no name). The
kernel, with its current signature, has no way to reach into a nested bundle by member
index.

Per-member internal fields cannot be advertised individually at advertise-time either,
because member field names/count of an import bundle are not generally known until
realize-time (they arrive via the framework's structure-mirroring mechanism, same as
`sum_`/`counts_` bundles in `TimeAverage`).

### Proposed refactor (NOT yet implemented — for a future session)

Refactor `AbstractCovarianceKernel` so kernels no longer do their own internal-state
lookup by derived name. Instead:

- `TimeVariance` itself becomes responsible for all `internal_state`/bundle lookups
  (mirroring exactly what `TimeAverage` etc. do now), extracting the specific internal
  fields — either once (field mode) or per-member-field-in-a-loop (bundle mode) — and
  passing them explicitly into the kernel calls.
- Kernel abstract interfaces (`I_update`, `I_compute`, `I_initialize`, `I_action`/reset)
  change to accept the pre-resolved internal fields as an explicit argument instead of
  deriving names and doing `esmf_StateGet` internally.
- Since Welford (3 internal fields: mux, muy, c) and Shifted (5 internal fields: kx, ky,
  ex, ey, exy) have a *different* number/set of internal fields, the common abstract
  signature should pass them as an assumed-shape array argument, e.g.
  `internal_fields(:)` of `type(esmf_Field)`, with each concrete kernel indexing into it
  by its own known convention:
  - Welford: `internal_fields = [mux_f, muy_f, c_f]`
  - Shifted: `internal_fields = [kx_f, ky_f, ex_f, ey_f, exy_f]`
  (This exact approach — "array of esmf_Field" — was the user's explicit preference when
  asked, over "named per-kernel arguments" which would break the abstract-interface
  contract since arity would differ between concrete kernel types.)
- `kernel%advertise()` stays mostly as-is — it already just adds specs by name/prefix;
  it just needs the same `item_type` pass-through as the other `advertise_*` routines
  (`advertise_time_variance_internal_fields(gridcomp, name, rc)` -> `(gridcomp, name,
  item_type, rc)`, forwarded to `wk%advertise`/`sk%advertise` — those two would also
  need an `item_type` parameter added).
- `TimeVariance` then, in bundle mode, loops over member index `i` and calls something
  like `kernel%update_r4(gridcomp, x_fieldlist(i), y_fieldlist(i),
  counts_fieldlist(i), internal_fieldlists_at_i(:), rc)`.

This is a real architectural change touching `AbstractCovarianceKernel.F90`,
`WelfordCovarianceKernel.F90`, `ShiftedCovarianceKernel.F90`, and `TimeVariance.F90` —
not just an additive change like the other four stat types. It should be scoped as its
own follow-up task.

### Explicit decision from user (2026-09-16 session, unchanged)

When asked how to proceed, the user chose: **"Skip TimeVariance for now"** — i.e. do not
even add the outer-shell `is_bundle`/bundle-constructor to `TimeVariance` yet; leave it
exactly as-is (Field-only) until the kernel refactor is undertaken as separate work.

## Remaining / Follow-up Tasks

1. **TimeVariance + kernel refactor** (see above) — the main remaining piece.
2. Once TimeVariance supports bundles, update `StatisticsGridComp.F90`'s `'variance'`
   case in `advertise_item` (pass `itemtype=item_type` to the EXPORT spec + to
   `advertise_time_variance_internal_fields`) and `make_variance_stat` (add the
   `MAPL_STATEITEM_FIELD`/`MAPL_STATEITEM_FIELDBUNDLE` dispatch), matching the pattern
   already applied to `make_min_stat`/`make_max_stat`/`make_accumulate_stat`. Also add a
   `variance`/`covariance` entry (field + vector) to the `statistics_real` scenario test
   matrix, following the same pattern used for the other 4 actions this session.
3. Consider adding pFUnit tests for FieldBundle-mode behavior at the unit-test level too
   (`gridcomps/statistics/tests/` currently only has `Test_TimeAccumulate.pf` and
   `Test_TimeVariance.pf`; the new scenario-test coverage added this session is
   integration-level, not unit-level). Still explicitly out of scope unless requested.
4. Double check `MAPL_STATEITEM_VECTOR` vs `MAPL_STATEITEM_FIELDBUNDLE` semantics in
   `StatisticsGridComp.F90::advertise_item` — the YAML `itemtype: vector` maps to
   `MAPL_STATEITEM_VECTOR`, but the runtime dispatch in `realize_item`/`make_*_stat`
   checks against `MAPL_STATEITEM_FIELDBUNDLE`. This pre-existing wrinkle (not
   introduced by this effort) appears to be intentional/working correctly (confirmed
   empirically — all 3 new vector-mode stats in the scenario test pass), but is still
   worth documenting properly if it ever causes confusion.
5. Clean up stray pre-existing debug output (`_HERE, ' bmaa '` calls) in
   `StatisticsGridComp.F90`'s `make_average_stat`/`make_item` — not introduced by this
   effort, still present, prints to stdout on every scenario test run. Should be removed
   before this branch is finalized for review/PR.
6. Consider whether the `TimeMin`/`TimeMax` `fill_value=MAPL_UNDEF` fix should also be
   accompanied by a unit test (in `gridcomps/statistics/tests/`) specifically covering
   the first-period-with-no-prior-reset edge case, so a future refactor can't
   regress it silently (currently only caught indirectly via the `statistics_real`
   scenario test's `QV_MIN` check).
7. `git status` currently also shows unrelated pre-existing uncommitted edits to
   `tests/MAPL3G_Component_Testing_Framework/test_cases/case18/{cap_driver1,history1}.yaml`
   (segment_duration / history frequency changes) — not part of this effort, left
   untouched throughout. An untracked `case18/temp7788/` scratch directory also exists
   from a prior `MAPL3G_Comp_Test_case18` run and can likely be deleted.

## Files Touched (cumulative across all sessions, still uncommitted)

- `gridcomps/statistics/TimeAverage.F90` (prior session)
- `gridcomps/statistics/TimeMax.F90` (bundle support: prior session; `fill_value` bugfix:
  this session)
- `gridcomps/statistics/TimeMin.F90` (bundle support: prior session; `fill_value` bugfix:
  this session)
- `gridcomps/statistics/TimeAccumulate.F90` (prior session)
- `gridcomps/statistics/StatisticsGridComp.F90` (prior session)
- `gridcomps/configurable/ConfigurableGridComp.F90` (this session — bundle-aware `run()`)
- `superstructure/generic/tests/Test_Scenarios.pf` (this session — bundle-aware
  `check_field_status`/`check_field_value`, missing `use` fix)
- `superstructure/generic/tests/scenarios/statistics_real/A.yaml` (this session)
- `superstructure/generic/tests/scenarios/statistics_real/stat.yaml` (this session)
- `superstructure/generic/tests/scenarios/statistics_real/history.yaml` (this session)
- `superstructure/generic/tests/scenarios/statistics_real/collection_1.yaml` (this
  session)
- `superstructure/generic/tests/scenarios/statistics_real/expectations.yaml` (this
  session)

## Files NOT Touched (deliberately deferred)

- `gridcomps/statistics/TimeVariance.F90`
- `gridcomps/statistics/AbstractCovarianceKernel.F90`
- `gridcomps/statistics/WelfordCovarianceKernel.F90`
- `gridcomps/statistics/ShiftedCovarianceKernel.F90`
- `gridcomps/statistics/AbstractTimeStatistic.F90` (no changes needed — interfaces
  already generic enough)
- `gridcomps/statistics/tests/*.pf` (no new unit tests added; only the scenario-level
  `statistics_real` integration test was expanded)
</content>
