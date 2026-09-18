# Statistics GridComp: ESMF_FieldBundle Support Plan

**Document Version:** 3.0
**Date:** 2026-09-18
**Status:** Field/Bundle support for TimeAverage, TimeMax, TimeMin, TimeAccumulate is
implemented and covered by an expanded `statistics_real` scenario test (all 4 actions
x both item types) — all committed. **TimeVariance + covariance kernels are now fully
planned/designed (approved by user) but NOT YET IMPLEMENTED** — this is the next and
final piece of this overall effort. See "Deferred Work" section below for the
ready-to-implement design.

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
- **Committed to git** as `7e20e30e` ("add plan for vector stats") and `46b9ca0d`
  ("vector working") — see "Commit History" section near the end for exact commit
  hashes/file lists. The working tree is clean with respect to this effort as of
  2026-09-17.

## Deferred Work: `TimeVariance.F90` + Covariance Kernels — READY TO IMPLEMENT

`TimeVariance.F90` was explicitly **excluded** from all sessions so far because it is
architecturally different from the other four types and requires an interface change,
not just an additive one. As of 2026-09-18, the design below has been fully worked out
and **approved by the user** — this is the next session's starting point. Nothing has
been implemented yet; `git status` should be clean when resuming.

### Why it's harder (unchanged analysis, still accurate)

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

### Approved refactor design (ready to implement)

**Core idea:** kernels become pure computation with no state-lookup responsibility at
all. `TimeVariance` becomes responsible for ALL `internal_state`/bundle lookups
(mirroring exactly what `TimeAverage`/`TimeMax`/`TimeMin`/`TimeAccumulate` already do),
resolving the specific internal fields — either once (field mode) or per-member-in-a-
loop (bundle mode) — and passing them explicitly into the kernel calls as a
`type(esmf_Field), intent(inout) :: internal_fields(:)` array, indexed by a
kernel-specific fixed position. Since Welford (3 internal fields: mux, muy, c) and
Shifted (5 internal fields: kx, ky, ex, ey, exy) have different arity, a single deferred
kernel method — `get_internal_field_prefixes()` — lets `TimeVariance` generically
discover which top-level internal-state names to resolve and in what order, WITHOUT
`TimeVariance` hardcoding "3 for Welford, 5 for Shifted" anywhere.

#### 1. `AbstractCovarianceKernel.F90` — interface changes

- `I_advertise(this, gridcomp, name, item_type, rc)`: add
  `type(ESMF_StateItem_Flag), intent(in) :: item_type`, to be forwarded into every
  internal `MAPL_GridCompAddSpec(..., itemtype=item_type, ...)` call (same pattern as
  `advertise_time_max_internal_fields` etc.).
- **New deferred function:**
  ```fortran
  function I_get_internal_field_prefixes(this) result(prefixes)
     import AbstractCovarianceKernel
     class(AbstractCovarianceKernel), intent(in) :: this
     character(len=16), allocatable :: prefixes(:)
  end function I_get_internal_field_prefixes
  ```
  (fixed-length `character(len=16)` chosen for simplicity over a variable-length
  allocatable-character-array, which is awkward in Fortran; 16 chars is comfortably
  larger than the longest prefix, `'exy_'`.)
  - Welford implementation returns `['mux_', 'muy_', 'c_  ']` (order matters: matches
    the fixed positional convention below).
  - Shifted implementation returns `['kx_ ', 'ky_ ', 'ex_ ', 'ey_ ', 'exy_']`.
- `I_initialize(this, gridcomp, f_x, f_y, counts_f, internal_fields, rc)`: add
  `type(esmf_Field), intent(inout) :: internal_fields(:)`. Kernel no longer does
  `esmf_StateGet` — it just does `mapl_FieldSet(internal_fields(k), geom=..., ...)` for
  each `k` (metadata still derived from `f_x`, exactly as today), in the fixed order
  matching `get_internal_field_prefixes()`.
- `I_action(this, gridcomp, internal_fields, rc)` (used for both `reset` and `destroy`):
  replace the `f_x` argument with `internal_fields(:)`. `reset` does
  `esmf_FieldFill(internal_fields(k), ...)` for each `k`. `destroy` remains a no-op (as
  today — `TimeVariance%destroy` never actually calls `kernel%destroy` currently; this
  is just kept consistent for interface completeness).
- `I_update(this, gridcomp, f_x, f_y, counts_f, internal_fields, rc)`: add
  `internal_fields(:)`, remove all internal `esmf_StateGet`; unpack by fixed position
  (e.g. Welford: `mux_f => internal_fields(1)`, `muy_f => internal_fields(2)`,
  `c_f => internal_fields(3)`) and do the exact same `where`-block math as today.
- `I_compute(this, gridcomp, f_x, f_y, counts_f, cov_f, internal_fields, biased, rc)`:
  same treatment — unpack only the subset actually needed (Welford only needs `c_f =
  internal_fields(3)`; Shifted needs `ex_f=internal_fields(3)`, `ey_f=(4)`,
  `exy_f=(5)`).

#### 2. `WelfordCovarianceKernel.F90` / `ShiftedCovarianceKernel.F90`

- Add `get_internal_field_prefixes`.
- Rewrite `advertise` to accept + forward `item_type`.
- Rewrite `initialize`, `reset`, `update_r4`/`update_r8`, `compute_r4`/`compute_r8` to
  take `internal_fields(:)` and index into it by fixed position instead of doing
  `mapl_FieldGet(f_x, short_name=...)` + `esmf_StateGet`. The actual math (Welford's
  running-mean recurrence; Shifted's shifted-sum-of-products) is **unchanged** — this is
  purely a plumbing refactor of how the internal fields are obtained, not a change to
  the numerics.
- `destroy` keeps its current no-op body (just update signature to match `I_action`).

#### 3. `TimeVariance.F90`

- Add `logical :: is_bundle = .false.`, `type(ESMF_FieldBundle) :: b`, `type
  (ESMF_FieldBundle) :: var_b` components alongside existing `f`/`var_f`.
- Add `new_TimeVariance_fieldbundle(unusable, b, var_b, alarm, algorithm, biased)
  result(stat)` constructor mirroring the existing `new_TimeVariance` (note: unlike the
  other 4 stat types, TimeVariance's constructors don't take `gridcomp` and don't do any
  geom/metadata work at construction time — that's deferred to `initialize()`, invoked
  lazily on first `update()` via the existing `needs_initialization()` check. The bundle
  constructor should follow the exact same lazy pattern: just store `b`, `var_b`,
  `alarm`, `algorithm`, `biased`, `is_bundle=.true.`). Combine both constructors via a
  generic `interface TimeVariance`.
- **New private helper** (the crux of the refactor):
  ```fortran
  function resolve_internal_fields(this, gridcomp, member_index, rc) result(fields)
     class(TimeVariance), intent(inout) :: this
     type(esmf_GridComp), intent(inout) :: gridcomp
     integer, intent(in) :: member_index   ! ignored/ANY value when .not. this%is_bundle
     integer, optional, intent(out) :: rc
     type(esmf_Field), allocatable :: fields(:)
     ! 1. prefixes = this%kernel%get_internal_field_prefixes()
     ! 2. just_name = <name derived from this%f or this%b, stripped of any '/'-prefix,
     !    matching the existing advertise-time just_name convention>
     ! 3. allocate(fields(size(prefixes)))
     ! 4. do k = 1, size(prefixes)
     !       if (this%is_bundle) then
     !          esmf_StateGet(internal_state, trim(prefixes(k))//just_name, fieldbundle=tmp_b)
     !          MAPL_FieldBundleGet(tmp_b, fieldList=tmp_list)
     !          fields(k) = tmp_list(member_index)
     !       else
     !          esmf_StateGet(internal_state, trim(prefixes(k))//just_name, field=fields(k))
     !       end if
     !    end do
  end function resolve_internal_fields
  ```
  Called from `reset`, `update`, `compute_result`, and `initialize` — this is what makes
  the rest of the refactor generic across Welford/Shifted without `TimeVariance` ever
  hardcoding a field count.
- `destroy`: branch `is_bundle` → `MAPL_FieldBundleDestroy(this%var_b, rc)` vs
  `esmf_FieldDestroy(this%var_f, rc)`.
- `reset`: branch `is_bundle`.
  - Field mode: unchanged structurally, but now also calls `resolve_internal_fields`
    (member_index irrelevant) and passes the result to `kernel%reset`.
  - Bundle mode: fetch `counts_` bundle, `MAPL_FieldBundleGet(..., fieldList=...)`, loop
    `i = 1, size(fieldlist)`: zero counts member `i`; call
    `kernel%reset(gridcomp, resolve_internal_fields(this, gridcomp, i, _RC), _RC)`.
- `update`: determine `typekind` via `is_bundle` branch (as in the other 4 types). Bundle
  mode: get `fieldList` of `this%b` (x=y input) and of the `counts_` bundle; loop `i`:
  `kernel%update_r4/r8(gridcomp, b_fieldlist(i), b_fieldlist(i), counts_fieldlist(i),
  resolve_internal_fields(this, gridcomp, i, _RC), _RC)`. Field mode: single call as
  today, just with the extra `resolve_internal_fields(...)` argument.
- `compute_result`: same branching pattern, looping over `var_b`'s fieldList as the
  output (`cov_f`) alongside `b`'s and `counts_`'s fieldLists as inputs.
- `initialize`: kernel allocation (`select case (stat%algorithm)`) is unchanged/
  independent of `is_bundle`. Field mode: unchanged geom/metadata propagation onto
  `var_f`/`counts_f`, then calls `kernel%initialize(..., resolve_internal_fields(...),
  _RC)`. Bundle mode: loop `i = 1, size(b_fieldlist)`: get member `i` of `b`, `var_b`,
  `counts_b`; propagate geom/metadata onto `var_b`'s and `counts_b`'s member `i` (factor
  the existing per-field geom/ungridded_dims/units/typekind/vgrid/vert_staggerloc
  extraction-and-`mapl_FieldSet` logic into a small shared private helper,
  e.g. `propagate_metadata(f_i, var_f_i, counts_f_i, rc)`, called once per member to
  avoid duplicating it between field-mode and the bundle-mode loop); then call
  `kernel%initialize(gridcomp, f_i, f_i, counts_f_i, resolve_internal_fields(this,
  gridcomp, i, _RC), _RC)`.
- `advertise_time_variance_internal_fields(gridcomp, name, item_type, rc)`: add
  `item_type` parameter (currently missing it entirely — the only one of the 5
  `advertise_time_*_internal_fields` routines that doesn't take it yet), forward into
  the `counts_'//just_name` `MAPL_GridCompAddSpec` call and into `wk%advertise(gridcomp,
  just_name, item_type, _RC)` / `sk%advertise(gridcomp, just_name, item_type, _RC)`.

#### 4. `StatisticsGridComp.F90`

- `advertise_item`'s `'variance'` case: pass `itemtype=item_type` to the EXPORT
  `MAPL_GridCompAddSpec` call (currently the only action that doesn't) and to
  `advertise_time_variance_internal_fields(gridcomp, name, item_type, _RC)`.
- `make_variance_stat`: add the `MAPL_STATEITEM_FIELD`/`MAPL_STATEITEM_FIELDBUNDLE`
  dispatch via `mapl_StateGet(importState, itemName=name, itemtype=itemtype)`, exactly
  mirroring `make_min_stat`/`make_max_stat`/`make_accumulate_stat`: FIELD →
  `TimeVariance(f=f_in, var_f=f_out, alarm=alarm, algorithm=algorithm, biased=biased)`;
  FIELDBUNDLE → `TimeVariance(b=b_in, var_b=b_out, alarm=alarm, algorithm=algorithm,
  biased=biased)`.

#### Sanity check already performed: no analogous `fill_value` bug expected here

Unlike `TimeMin`/`TimeMax` (where the fix was `fill_value=0.0` → `fill_value=MAPL_UNDEF`
to match `reset()`'s sentinel), the variance/covariance internal fields (`mux_`, `muy_`,
`c_`, `kx_`, `ky_`, `ex_`, `ey_`, `exy_`, `counts_`) all correctly use `fill_value=0.0`
at advertise time, and `reset()` also fills them back to `0.0` via `esmf_FieldFill(...,
const1=0.d0)` — **consistent**, since `0` is a legitimate neutral starting value for
running sums/means/counts (unlike min/max, which need a true "no value yet" sentinel).
No fix needed here; this was double-checked by reading both kernels' `advertise`/`reset`
side by side before finalizing this plan.

#### Aside (explicitly out of scope): true two-field covariance

`stat.yaml`'s unused `monthly_covariance` YAML anchor has `action: variance` too —
there is no actual distinct "covariance of two different fields" action implemented
anywhere (`TimeVariance` always passes `f` as both `x` and `y`, i.e. `Cov(f,f) =
Var(f)`). The refactored kernel interface (`f_x`, `f_y` args, already distinct in the
signature) *could* support true two-field covariance later with fairly small additional
work in `TimeVariance`/`StatisticsGridComp.F90` (a new `action: covariance` taking two
`name`s), but this is a separate feature request, not part of this refactor, and was
not requested.

### Locked-in decisions from user Q&A (2026-09-17 planning session)

- **Kernel refactor approach approved as designed above** (pre-resolved
  `internal_fields(:)` array + new `get_internal_field_prefixes()` kernel method) — no
  alternative design was requested.
- **Test coverage for the new bundle/field variance support:** exercise **both**
  algorithms (`welford` AND `shifted`), for **both** item types (field and vector) — 4
  new scenario-test quantities total, doubling the coverage compared to the other 4
  actions (which only got 1 algorithm each, since they don't have a pluggable-kernel
  concept).
- **Only the default (unbiased/sample, `biased: false`) variance branch** needs testing
  — no `biased: true` variant required.

### Planned test-matrix addition (once implemented)

Add to `superstructure/generic/tests/scenarios/statistics_real/`, following the exact
same pattern established for the other 4 actions (`A.yaml` run-series + export decl,
`stat.yaml` stats entry, `history.yaml` connection, `collection_1.yaml` import,
`expectations.yaml` status/value checks):

| Quantity | Type | Action | Algorithm | Expected value(s) |
|---|---|---|---|---|
| `TS_VAR_W` (or similar name) | field | variance | welford (default) | 50.0 |
| `TS_VAR_S` | field | variance | shifted | 50.0 |
| `UV_VAR_W` | vector | variance | welford (default) | [50.0, 50.0] |
| `UV_VAR_S` | vector | variance | shifted | [50.0, 50.0] |

**Expected-value derivation** (already computed, reusable): using the existing
`1..24`/`0..23` ramp series, sample variance (`biased: false`, divide by `n-1`) of an
arithmetic sequence of `N` consecutive integers is `N(N+1)/12`; for `N=24` that's
`24*25/12 = 50.0` **exactly**. This is shift-invariant, so both the U-component
(`1..24`) and V-component (`0..23`) of the vector quantities give the same `50.0` —
convenient for expectations but worth double-checking empirically once implemented
(build+run, adjust if the actual computed value differs, exactly as was done for the
`QV_MIN`/`fill_value` bug in the prior session).

`stat.yaml` entries will need `algorithm: shifted` added explicitly for the `_S`
variants (default is `welford` when omitted, per `make_variance_stat`'s existing
`algorithm` key handling — unchanged by this refactor).

### Next-session implementation order (recommended)

1. `AbstractCovarianceKernel.F90` interface changes.
2. `WelfordCovarianceKernel.F90` refactor (simpler kernel, 3 fields — do this one first
   to validate the pattern).
3. `ShiftedCovarianceKernel.F90` refactor (5 fields, same pattern).
4. `TimeVariance.F90`: `resolve_internal_fields` helper, `is_bundle`/bundle constructor,
   branch all 4 methods (`destroy`/`reset`/`update`/`compute_result`) +
   `advertise_time_variance_internal_fields` + `initialize`.
5. `StatisticsGridComp.F90`: `advertise_item`'s `'variance'` case +
   `make_variance_stat` dispatch.
6. Build (`MAPL.statistics` target) and fix any compile errors before touching tests.
7. Expand `statistics_real` scenario files per the test-matrix table above; build
   `build-tests`; run `ctest -R MAPL.generic.scenarios`; adjust expected values if the
   actual computed numbers differ from the hand-derived `50.0`.
8. Full `ctest --test-dir build-debug` sanity pass (expect the same 5 pre-existing
   unrelated failures as before — `ll-ll`/`cs-cs`/`cs-ll`/`ll-cs`/`case18` — nothing
   else).
9. Update this plan document's status/commit-history sections once done; do not commit
   unless the user explicitly asks (per this effort's established norm).

### Explicit decision from user (2026-09-16 session, still applies to "skip until now")

When originally asked how to proceed (before this design was worked out), the user
chose: **"Skip TimeVariance for now"** — i.e. do not even add the outer-shell
`is_bundle`/bundle-constructor to `TimeVariance` until a proper kernel refactor was
designed. That refactor is now fully designed (this section) and approved — the "skip"
decision is superseded by the plan above for the next session.

## Remaining / Follow-up Tasks

1. **TimeVariance + kernel refactor + StatisticsGridComp.F90 wiring + scenario-test
   expansion** — see the fully-designed "Deferred Work" section above (approved,
   ready to implement step-by-step per its "Next-session implementation order"
   subsection) — the one remaining piece of this overall effort.
2. ~~Once TimeVariance supports bundles, update `StatisticsGridComp.F90`'s
   `'variance'` case...~~ — folded into item 1 above (see "Deferred Work" §4 and the
   test-matrix table).
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
7. ~~`git status` currently also shows unrelated pre-existing uncommitted edits to
   `tests/MAPL3G_Component_Testing_Framework/test_cases/case18/{cap_driver1,history1}.yaml`~~
   — **resolved/no longer present**: as of 2026-09-17 the working tree is clean aside
   from this plan document; those case18 edits (and the `case18/temp7788/` scratch
   directory) are gone (either reverted or handled outside this effort). No action
   needed here anymore.

## Files Touched (cumulative across all sessions — now committed, see Commit History)

- `gridcomps/statistics/TimeAverage.F90` (prior session; committed in `d45c1568`/`57883343`)
- `gridcomps/statistics/TimeMax.F90` (bundle support: prior session, `d45c1568`;
  `fill_value` bugfix: this session, `46b9ca0d`)
- `gridcomps/statistics/TimeMin.F90` (bundle support: prior session, `d45c1568`;
  `fill_value` bugfix: this session, `46b9ca0d`)
- `gridcomps/statistics/TimeAccumulate.F90` (prior session; committed in `d45c1568`)
- `gridcomps/statistics/StatisticsGridComp.F90` (prior session; committed in `d45c1568`)
- `gridcomps/configurable/ConfigurableGridComp.F90` (this session — bundle-aware `run()`;
  committed in `46b9ca0d`)
- `superstructure/generic/tests/Test_Scenarios.pf` (this session — bundle-aware
  `check_field_status`/`check_field_value`, missing `use` fix; committed in `46b9ca0d`)
- `superstructure/generic/tests/scenarios/statistics_real/A.yaml` (this session;
  committed in `46b9ca0d`)
- `superstructure/generic/tests/scenarios/statistics_real/stat.yaml` (this session;
  committed in `46b9ca0d`)
- `superstructure/generic/tests/scenarios/statistics_real/history.yaml` (this session;
  committed in `46b9ca0d`)
- `superstructure/generic/tests/scenarios/statistics_real/collection_1.yaml` (this
  session; committed in `46b9ca0d`)
- `superstructure/generic/tests/scenarios/statistics_real/expectations.yaml` (this
  session; committed in `46b9ca0d`)

## Files NOT Touched (deliberately deferred)

- `gridcomps/statistics/TimeVariance.F90`
- `gridcomps/statistics/AbstractCovarianceKernel.F90`
- `gridcomps/statistics/WelfordCovarianceKernel.F90`
- `gridcomps/statistics/ShiftedCovarianceKernel.F90`
- `gridcomps/statistics/AbstractTimeStatistic.F90` (no changes needed — interfaces
  already generic enough)
- `gridcomps/statistics/tests/*.pf` (no new unit tests added; only the scenario-level
  `statistics_real` integration test was expanded)

## Commit History (this effort)

```
46b9ca0d vector working                       <- this session: scenario-test expansion,
                                                  fill_value bugfix, Test_Scenarios.pf
                                                  bundle support, ConfigurableGridComp
                                                  bundle-aware run(), plan doc v2.0
7e20e30e add plan for vector stats             <- plan doc v1.0 (prior session's summary)
d45c1568 update all non-variance methods       <- prior session: TimeMax/TimeMin/
                                                  TimeAccumulate bundle support +
                                                  StatisticsGridComp.F90 dispatch
150b6a13 fix bug                               <- prior session (HistoryGridComp_private,
                                                  StatisticsGridComp minor fix)
57883343 more changes                          <- earlier session: initial TimeAverage
                                                  bundle work + History/FieldBundleGet
                                                  fixes
```

As of this writing, `git status` is fully clean — nothing left uncommitted for this
effort (see "Remaining Tasks" item 7 for a note on the now-resolved `case18` tangent).

</content>
