# Statistics GridComp: ESMF_FieldBundle Support Plan

**Document Version:** 1.0
**Date:** 2026-09-16
**Status:** Partially implemented (TimeAverage, TimeMax, TimeMin, TimeAccumulate done; TimeVariance deferred)

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

## Work Completed

### `TimeAverage.F90` (done in an earlier session)
- Added `is_bundle`, `b`, `avg_b` components; `new_TimeAverage_fieldbundle` constructor.
- `destroy`, `reset` (split into `reset_field`/`reset_bundle`), `update` (+
  `update_bundle_r4`/`update_bundle_r8`), `compute_result` (+
  `compute_result_bundle_r4`/`compute_result_bundle_r8`) all bundle-aware.
- Internal fields: `sum_<name>`, `counts_<name>` (both mirrored as bundles when
  `is_bundle`).
- `advertise_time_average_internal_fields` already had an `item_type` parameter before
  this effort — it was the "pioneer" file that already had bundle constructor stubbed in
  but non-functional update/compute_result/reset/destroy; this effort filled those in.

### `TimeMax.F90` (this session)
- Added `is_bundle`, `b`, `max_b`; `new_TimeMax_fieldbundle`.
- Internal field: `temp_max<name>` (note: no underscore between `temp_max` and `name` —
  pre-existing naming quirk, preserved as-is).
- `destroy`, `reset`/`reset_field`/`reset_bundle`, `update`/`update_bundle_r4`/
  `update_bundle_r8`, `compute_result`/`compute_result_bundle_r4`/
  `compute_result_bundle_r8` all implemented.
- `advertise_time_max_internal_fields` signature changed to accept `item_type`.

### `TimeMin.F90` (this session)
- Mirror of TimeMax with `min`/`min_f`/`min_b`/`temp_min<name>`.

### `TimeAccumulate.F90` (this session)
- Added `is_bundle`, `b`, `accum_b`; `new_TimeAccumulate_fieldbundle`.
- Internal field: `sum_<name>` (only one internal field, no counts).
- `reset` here was NOT split into separate `reset_field`/`reset_bundle` subroutines
  (unlike the others) — it's simple enough (`esmf_FieldFill` only, no `MAPL_AssignFptr`
  pointer math) that it was inlined as a single `if (this%is_bundle) then ... else ...
  end if` block directly in `reset`.
- `update`/`update_bundle_r4`/`update_bundle_r8`,
  `compute_result`/`compute_result_bundle_r4`/`compute_result_bundle_r8` implemented.
- `advertise_time_accumulate_internal_fields` signature changed to accept `item_type`.

### `StatisticsGridComp.F90` (this session)
- `advertise_item`'s `select case (action)`: the `'min'`, `'max'`, `'accumulate'` cases
  now pass `itemtype=item_type` to the EXPORT `MAPL_GridCompAddSpec` call and forward
  `item_type` into their respective `advertise_time_*_internal_fields` calls (previously
  only `'average'` did this).
- `make_min_stat`, `make_max_stat`, `make_accumulate_stat`: added the
  `MAPL_STATEITEM_FIELD` vs `MAPL_STATEITEM_FIELDBUNDLE` dispatch (via
  `mapl_StateGet(importState, itemName=name, itemtype=itemtype)`) that constructs either
  the Field or FieldBundle variant of the stat, mirroring the pre-existing
  `make_average_stat` pattern.
- `'variance'` case and `make_variance_stat` were **left untouched** (still Field-only) —
  see "Deferred Work" below.

### Verification performed
- Full incremental build of MAPL succeeded with the `ifx` compiler in
  `build-debug/` (`make -j 8`), including `MAPL.statistics`, `MAPL.history`, `GEOS.x`
  targets. Only pre-existing/benign "unused variable" compiler remarks were produced (no
  errors, no warnings introduced by these changes beyond unused `status`/`state` in a
  couple of subroutines that don't call any `_RC`-using statement on some branches).
- No new pFUnit tests were added (none existed for these stat types' field-mode either,
  per explicit user decision to skip test-writing for this effort).
- Nothing was committed to git (per explicit user instruction during the session); all
  changes are currently unstaged/uncommitted working-tree edits.

## Deferred Work: `TimeVariance.F90` + Covariance Kernels

`TimeVariance.F90` was explicitly **excluded** from this pass because it is
architecturally different from the other four types and requires an interface change,
not just an additive one.

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
of `this%b` inside `TimeVariance`, each member's own `short_name` (e.g. `"u"`) will NOT
match a top-level state item `mux_u` — it's nested inside the `mux_<bundle_name>` bundle
as one of its members instead. The kernel, with its current signature, has no way to
reach into that nested bundle by member index.

Per-member internal fields cannot be advertised individually at advertise-time either,
because member field names of an import bundle are not generally known until
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

### Explicit decision from user (2026-09-16 session)

When asked how to proceed, the user chose: **"Skip TimeVariance for now"** — i.e. do not
even add the outer-shell `is_bundle`/bundle-constructor to `TimeVariance` yet; leave it
exactly as-is (Field-only) until the kernel refactor is undertaken as separate work.

## Remaining / Follow-up Tasks

1. **TimeVariance + kernel refactor** (see above) — the main remaining piece.
2. Once TimeVariance supports bundles, update `StatisticsGridComp.F90`'s `'variance'`
   case in `advertise_item` (pass `itemtype=item_type` to the EXPORT spec + to
   `advertise_time_variance_internal_fields`) and `make_variance_stat` (add the
   `MAPL_STATEITEM_FIELD`/`MAPL_STATEITEM_FIELDBUNDLE` dispatch), matching the pattern
   already applied to `make_min_stat`/`make_max_stat`/`make_accumulate_stat`.
3. Consider adding pFUnit tests for FieldBundle-mode behavior across all five stat types
   (none currently exist even for Field-mode `TimeAverage`/`TimeMax`/`TimeMin`; only
   `Test_TimeAccumulate.pf` and `Test_TimeVariance.pf` exist under
   `gridcomps/statistics/tests/`, and it's unclear if those cover Field-mode
   comprehensively). This was explicitly out of scope for the current effort.
4. Double check `MAPL_STATEITEM_VECTOR` vs `MAPL_STATEITEM_FIELDBUNDLE` semantics in
   `StatisticsGridComp.F90::advertise_item` — the YAML `itemtype: vector` maps to
   `MAPL_STATEITEM_VECTOR`, but the runtime dispatch in `realize_item`/`make_*_stat`
   checks against `MAPL_STATEITEM_FIELDBUNDLE`. This pre-existing wrinkle (not
   introduced by this effort) may be intentional (vector items are realized as
   FieldBundles under the hood) but is worth confirming/documenting if issues arise.
5. Clean up stray pre-existing debug output (`_HERE, ' bmaa '` calls) in
   `StatisticsGridComp.F90`'s `make_average_stat`/`make_item` — not introduced by this
   effort, but noticed during exploration; likely leftover debugging that should be
   removed before this branch is finalized for review/PR.

## Files Touched This Session

- `gridcomps/statistics/TimeMax.F90`
- `gridcomps/statistics/TimeMin.F90`
- `gridcomps/statistics/TimeAccumulate.F90`
- `gridcomps/statistics/StatisticsGridComp.F90`

(`gridcomps/statistics/TimeAverage.F90` was completed in a prior session, not re-touched
in this session except for reference/comparison.)

## Files NOT Touched (deliberately deferred)

- `gridcomps/statistics/TimeVariance.F90`
- `gridcomps/statistics/AbstractCovarianceKernel.F90`
- `gridcomps/statistics/WelfordCovarianceKernel.F90`
- `gridcomps/statistics/ShiftedCovarianceKernel.F90`
- `gridcomps/statistics/AbstractTimeStatistic.F90` (no changes needed — interfaces
  already generic enough)
- `gridcomps/statistics/tests/*.pf` (no new tests added)
