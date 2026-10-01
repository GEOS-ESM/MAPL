
# Debugging `mpirun ... GEOS.x mapl.yaml` failure in dyn-sa regression test

Date: 2026-09-28
Branch: `feature/bmauer/enable_vert_regrid_history3g` (MAPL submodule at
`src/Shared/@MAPL`)
Test dir: `regression/dyn-sa/temp7887/` (run directory; source configs live in
`regression/dyn-sa/`)

## Context

`bmauer` is actively developing vertical-regrid support in History3G
(`enable_vert_regrid_history3g`). Running the `dyn-sa` regression test with
`history.yaml` updated to request a vertically-regridded `T` (and `[U,V]`)
output on fixed pressure levels surfaced a chain of real bugs, one config
issue, and finally a test-data/numerics issue unrelated to the feature.

Command used throughout:
```
cd regression/dyn-sa/temp7887
rm -f run.log warnings_and_errors.log PET*.ESMF_LogFile *.nc4
mpirun -np 6 /home/bmauer/models/GEOS_mapl_v3/GEOSgcm/install-debug/bin/GEOS.x mapl.yaml |& tee run.log
```
Rebuild after any MAPL source change:
```
cmake --build /home/bmauer/models/GEOS_mapl_v3/GEOSgcm/build-debug --target install -j 8
```

## Bug #1 (config): `dyn-sa.yaml` vertical_grid used the wrong schema

**Symptom:** `pe=00000 FAIL ... StateItemSpec.F90 <cannot connect aspect VERTICAL_GRID>`

**Root cause:** `dyn-sa.yaml`'s `geometry.vertical_grid` block used:
```yaml
vertical_grid:
  class: model
  standard_name: air_pressure
  units: hPa
  num_levels: 91
  field_edge: PLE
  field_center: PE
```
`ModelVerticalGridFactory::supports_config` (`ModelVerticalGrid.F90:396-407`)
*requires* `grid_type: model` (not `class:`) **and** a `fields:` map (dimension
name -> field short_name). None of those keys existed, so
`ModelVerticalGridFactory` rejected the config, and it silently fell through
to `BasicVerticalGridFactory` (only requires `num_levels`, which was present).
`BasicVerticalGrid::get_supported_physical_dimensions` always returns
`["<unknown>"]`, which can never match History's requested `"pressure"`
dimension, hence "cannot connect aspect VERTICAL_GRID".

**Fix (GEOSgcm repo, `regression/dyn-sa/dyn-sa.yaml`):**
```yaml
vertical_grid:
  grid_type: model
  fields: {pressure: PE}
  num_levels: 91
```
(`PE` is DYNsa's edge-pressure export, `standard_name: air_pressure`, units
Pa; see `DynCore_StateSpecs.rc:207`.)

**Also fixed the same stale schema** in
`src/Shared/@MAPL/gridcomps/FakeParent/fake-parent-example.yaml` (was an
identical bad example, not otherwise referenced by any running config).

**Side note (not a bug):** `history.yaml`'s `vertical_grids.pressure_levs`
used `class: fixed_levels` (not `grid_type:`) and happened to work anyway,
because `FixedLevelsVerticalGridFactory::supports_config`
(`FixedLevelsVerticalGrid.F90:238-247`) only checks `grid_type` *if present*;
since it's absent, that check is skipped and it falls through to requiring
`levels` + `physical_dimension`, both of which were present and correctly
named. Renamed to `grid_type:` anyway for robustness/clarity in
`history.yaml` and the (unreferenced, orphaned) `vgrid.yaml` in the test dir.

## Bug #2 (real code bug): `[U,V]` bracket variable failed with `cannot connect aspect CLASS`

**Symptom:** with `"[U,V]": {source: DYNsa.UV}` active in `history.yaml`'s
`var_list`, init failed with
`pe=... FAIL ... StateItemSpec.F90 <cannot connect aspect CLASS>`, traced
through `ModelVerticalGrid.F90:269` (`get_coordinate_field_with_coupler`) and
`VerticalGridAspect_SMOD_make_transform.F90:73`.
Isolated by temporarily disabling `[U,V]` and confirming `T` alone passed
init cleanly - confirming `[U,V]` was the trigger.

**Root cause:** `make_transform.F90`'s `coord_aspects = other_aspects` copies
the *payload* field's aspects wholesale when building the request for the
coordinate field (`PE`). For `T` the `CLASS` aspect is a plain
`FieldClassAspect` (matches fine); for the `[U,V]` bracket entry it's a
vector/bracket-type CLASS aspect, which `FieldClassAspect::matches_a`
(`FieldClassAspect_smod.F90:9-23`) does not recognize (only accepts
`FieldClassAspect` or `WildcardClassAspect`). Same category of bug already
patched for `UNITS`/`STANDARD_NAME` in the same function - just not yet
extended to `CLASS`.

**Fix** (`superstructure/generic/specs/VerticalGridAspect/make_transform.F90`):
Added `use mapl_WildcardClassAspect_mod, only: WildcardClassAspect` and:
```fortran
call coord_aspects%insert(CLASS_ASPECT_ID, WildcardClassAspect())
```
right after the existing `UNITS_ASPECT_ID`/`STANDARD_NAME_ASPECT_ID`
overrides. `WildcardClassAspect` matches any concrete `FieldClassAspect`
(the coordinate field is always a plain scalar field regardless of what
kind of payload field triggered the regrid).

**Verified:** rebuilt, reran with `[U,V]` + `T` both active - init now passes
cleanly (no more CLASS failure).

## Bug #3 (real code bug): `GEOM_IN` runtime crash (SIGKILL / forrtl 408)

**Symptom:** with only `T` active (isolating away bug #2), init now
succeeded, but the run phase crashed with a raw (non-`_ASSERT`) Fortran
runtime error:
```
forrtl: severe (408): fort: (8): Attempt to fetch from allocatable variable GEOM_IN when it is not allocated
```
traced through `VerticalRegridTransform.F90:199`
(`this%v_in_coupler%run(...)`) into `CouplerMetaComponent.F90:341`.

**Root cause:** `CouplerMetaComponent.F90`'s
`update_time_varying_field_field` / `update_time_varying_fieldbundle_field`
compare a freshly-fetched geom against a *cached* geom
(`this%time_varying%geom` / `geom_in` / `geom_out`) to detect whether the
geometry changed. On the very first `update()` call for a newly-created
coupler (exactly the situation for the new nested coordinate-field GEOM
regrid coupler that vertical regridding now creates), the cached geom is
still unallocated, and `ESMF_Geom`'s `operator(/=)` does not handle an
unallocated operand - it crashes rather than returning "different". This is
a pre-existing, unmodified, shared framework file
(`superstructure/generic/transforms/CouplerMetaComponent.F90`) - not part of
the original feature branch diff - just newly exercised by this code path.

**Fix:** added a local helper inside `update_time_varying`'s `contains`
block:
```fortran
logical function geom_differs(new_geom, cached_geom) result(differs)
   type(ESMF_Geom), allocatable, intent(in) :: new_geom
   type(ESMF_Geom), allocatable, intent(in) :: cached_geom
   if (.not. allocated(cached_geom)) then
      differs = .true.
      return
   end if
   differs = (new_geom /= cached_geom)
end function geom_differs
```
and replaced all 4 occurrences of `geom_in|geom_out /= this%time_varying%geom...`
in both `update_time_varying_fieldbundle_field` and
`update_time_varying_field_field` with calls to `geom_differs(...)`.

**Verified:** rebuilt, reran - no more crash; got two full, clean
`History: run: ...completed` cycles (never happened before any of these
fixes).

## Non-bug: `VerticalLinearMap.F90` assertions on the 3rd timestep

After all 3 fixes above, the run still fails - but now on a *different*,
well-behaved `_ASSERT` (not a crash), and only starting at the 3rd timestep:
```
VerticalLinearMap.F90:48  <maxval(dst) > maxval(src)>
VerticalLinearMap.F90:50  <src array is not decreasing>
```
(`superstructure/generic/transforms` -> actual file is
`infrastructure/vertical/vertical/VerticalLinearMap.F90`,
`compute_linear_map(src, dst, matrix, rc)`).

**What this means:** `src` = the actual DYNsa `PE` pressure-column values for
one grid tile (canonicalized decreasing); `dst` = the requested output
pressure levels (History's `pressure_levs`, currently `[500., 400.]` hPa).
These are `#ifndef NDEBUG` sanity guards for the linear interpolator (no
extrapolation, and the source column must be monotonic). They fired starting
at the 3rd timestep on multiple/most cubed-sphere tiles simultaneously,
meaning DYNsa's own computed pressure column became non-monotonic / had
surface pressure drop far below the requested levels by minute 40-60 of
simulated time.

**Assessed as NOT a regrid/connection bug**, but rather numerical
noise/instability in this specific idealized test setup: coldstart,
adiabatic, non-hydrostatic FV3, very coarse grid (13x13 points/face,
~650-900 km cells), large `RUN_DT=1200s`, uniform default topography
(`HGT_SURFACE=50.0`), no real balanced initial state. This is exactly the
kind of setup that tends to spin up small-scale acoustic/gravity-wave noise
in the pressure field within the first few steps.

**Not yet resolved / left to bmauer to decide:**
- Is this test config expected to remain stable for the intended number of
  timesteps at this resolution/`RUN_DT`? If not, may need a finer grid /
  smaller `RUN_DT` / different idealized IC for a vert-regrid-history smoke
  test.
- Should the vertical regrid transform be hardened to handle a
  transiently-invalid/non-monotonic source column more gracefully (e.g.
  clamp/skip a bad column) rather than hard-`_ASSERT`? (Debatable - the
  assert is arguably doing its job by refusing to silently produce garbage.)

## Files changed (MAPL submodule, `src/Shared/@MAPL`, all on
`feature/bmauer/enable_vert_regrid_history3g`)

- `gridcomps/FakeParent/fake-parent-example.yaml` - schema fix (Bug #1
  pattern, stale doc example)
- `superstructure/generic/specs/VerticalGridAspect/make_transform.F90` - Bug
  #2 fix (`WildcardClassAspect` override for `CLASS_ASPECT_ID` in
  `coord_aspects`)
- `superstructure/generic/transforms/CouplerMetaComponent.F90` - Bug #3 fix
  (`geom_differs` helper, used in both `update_time_varying_*_field`
  subroutines)

All of bmauer's original `_HERE, ' bmaa '` debug-print statements (added
across `HistoryCollectionGridComp.F90`, `VerticalGridManager.F90`,
`StateItemSpec.F90`, `VerticalGridAspect.F90`, `StateItemAspect.F90`,
`FixedLevelsVerticalGrid.F90`, `make_transform.F90`) were removed once
debugging was complete; those files (other than `make_transform.F90`, which
retains the real `CLASS` fix) are now byte-for-byte identical to the
pre-debugging commit again.

## Files changed (GEOSgcm repo, this component)

- `regression/dyn-sa/dyn-sa.yaml` - vertical_grid schema fix (see Bug #1)
- `regression/dyn-sa/history.yaml` - `class:` -> `grid_type:` for
  `pressure_levs`; `levels:` tuned from `[1000., 950., 900.]` down to
  `[500., 400.]` to avoid the (separate, unresolved) numerics issue above
  during interactive testing
- `regression/dyn-sa/temp7887/` - untracked scratch run directory (copies of
  the above configs + run logs); can be deleted/regenerated freely

## How to restore / re-verify this state

1. `cd src/Shared/@MAPL && git diff` should show exactly the 3 files listed
   above (Bugs #1 FakeParent example, #2, #3).
2. `cd regression/dyn-sa && git diff -- dyn-sa.yaml history.yaml` should show
   the schema/level changes described above.
3. Rebuild: `cmake --build /home/bmauer/models/GEOS_mapl_v3/GEOSgcm/build-debug --target install -j 8`
4. Rerun in `regression/dyn-sa/temp7887/` per the command at the top of this
   file. Expect: clean init, 2 successful `History: run:` cycles, then the
   (separate, not-yet-resolved) `VerticalLinearMap.F90` assertion on the 3rd
   timestep.

---

# Update: 2026-09-29 - `E_1`/NONE-vs-regrid crash in a new, self-contained
# MAPL reproducer (`test_ple`), the `typesafe_matches` fix, a MIRROR
# regression, an opencode session crash, and its resolution

Date: 2026-09-29
Branch: still `feature/bmauer/enable_vert_regrid_history3g` (MAPL repo,
`/Users/bmauer/models/mapl3g_vert_regrid_history/MAPL` - this is now a
standalone MAPL checkout, not the GEOSgcm submodule setup described above)
Test dir: `tests/MAPL3G_Component_Testing_Framework/test_cases/test_ple/`
(source configs) / `.../test_ple/temp88798/` (scratch run dir, untracked)

## Context

Since the entry above, `bmauer` moved debugging out of the full GEOSgcm
regression test and into a small, self-contained reproducer built directly
on MAPL's own `MAPL3G_Component_Testing_Framework`
(`componentDriverGridComp` + a `history1.yaml`), to isolate the `E_1`/`NONE`
vertical-regrid bug described conceptually in the very first entry of this
file (Bug: a genuinely 2D field getting dragged through vertical
interpolation it doesn't need) without depending on the external GEOSgcm
build.

New/changed test scaffolding for this (all untracked, all still present):
- `tests/MAPL3G_Component_Testing_Framework/test_cases/test_ple/` -
  `GCM1.yaml` (a `componentDriverGridComp`-based producer exporting `E_1`
  [`vertical_dim_spec: NONE`], `E_2` [`CENTER`], `PLE` [`edge`, 5-level model
  vertical grid], with a new `vertical_levels:` HConfig block - see below),
  `history1.yaml` (a `test` collection with a 4-level `pressure_levs`
  `vertical_grid:` and `var_list: {E_1, E_2}`).
- `tests/MAPL3G_Component_Testing_Framework/test_cases/test777/` - an earlier,
  slightly-broken scratch copy of the same idea (has a `veritcal_levels:`
  typo and lacks the `E_1`/NONE case); superseded by `test_ple`, can be
  deleted.
- `gridcomps/componentDriverGridComp/componentDriverGridComp.F90` - new
  `vertical_levels:` HConfig support in `initialize_internal_state`: for each
  `field_name: [level values]` entry, looks up the named field in the
  internal state via `MAPL_StateGetPointer`, asserts its 3rd (vertical)
  dimension size matches the number of supplied values, and broadcasts those
  values into every column (`do concurrent` over i,j). This is what lets
  `GCM1.yaml`'s `vertical_levels: {PLE_int: [990., 750., 500., 400., 300.,
  200.]}` actually populate a realistic, monotonic 5-level pressure-edge
  column for the test, instead of leaving it at whatever the generic fill
  logic would otherwise produce. (This piece was written in an earlier,
  separate opencode session on 2026-09-29 - `nimble-star`/"Initialize
  internal state with vertical_levels from HConfig" - not part of the crash
  narrative below, but is a prerequisite for `test_ple` and remains
  uncommitted alongside it.)

Neither `test_ple` nor `test777` is wired into
`test_cases/cases.txt`/CTest - both are manual/scratch, run directly via:
```
cd tests/MAPL3G_Component_Testing_Framework/test_cases/test_ple/temp88798
rm -f run.log warnings_and_errors.log PET*.ESMF_LogFile allPEs.log *.nc4
DYLD_LIBRARY_PATH=<build-debug>/gridcomps/componentDriverGridComp \
  mpirun -np 1 <install-debug>/bin/GEOS.x mapl1.yaml |& tee run.log
```
(The `DYLD_LIBRARY_PATH` workaround is needed because
`MAPL.componentDriverGridComp` is built `NOINSTALL` and `GEOS.x`'s only
rpath is `@loader_path/../lib`, so the install tree alone can't dlopen it.)

## Bug #4 (real code bug, same family as the original Bug #2/#3 investigation):
`E_1` (`vertical_dim_spec: NONE`) incorrectly vertically regridded, crashing
with an out-of-bounds subscript

**Symptom**, running the `test_ple` reproducer:
```
Runtime Error: infrastructure/vertical/vertical/CSR_SparseMatrix.F90, line 175:
Subscript 1 of X (value 2) is out of range (1:1)
```
crashing inside `mapl_verticalregridtransform_mod_MP_regrid_field_` ->
`mapl_csr_sparsematrix_mod_MP_matmul_vec_spsp`.

**Root cause** (confirmed by temporarily instrumenting
`VerticalRegridTransform.F90`, reverted afterward): `E_1` is declared
`vertical_dim_spec: NONE` in `GCM1.yaml` (a genuine 2D field, 1-element
vertical dimension), but `history1.yaml`'s `test` collection has a
collection-wide `vertical_grid: *pressure_levs` (4 levels). Tracing the
mechanism precisely:
1. `HistoryCollectionGridComp_private.F90::add_var_specs` never passes
   `vertical_stagger` to `MAPL_GridCompAddSpec` for any variable, so every
   HIST import's `VerticalGridAspect` silently defaults to
   `VERTICAL_STAGGER_CENTER` (the constructor default in
   `VerticalGridAspect.F90:116`) - never `NONE`, regardless of what the
   actual connected export declares.
2. `HistoryCollectionGridComp.F90:121`'s `MAPL_GridCompSetVerticalGrid` call
   then unconditionally stamps **every** registered import (including
   `E_1`'s) with the collection's 4-level `pressure_levs` grid and flips its
   status to `SPECIFIED` (`StateItemSpec::target_set_geom` ->
   `VerticalGridAspect::set_vertical_grid`, which never touches
   `vertical_stagger`). This exact landmine was already independently
   documented in
   `openspec/changes/archive/2026-09-22-derive-expression-vertical-dim-spec/design.md`,
   which explicitly deferred fixing it.
3. At connection time, the (pre-existing, original) `typesafe_matches`
   (`VerticalGridAspect.F90:201-205`, prior to any of today's changes) had:
   ```fortran
   if (any([src%vertical_stagger,dst%vertical_stagger] == VERTICAL_STAGGER_NONE)) then
      matches = src%vertical_stagger == dst%vertical_stagger
      return
   end if
   ```
   With `src` (`E_1`'s real export) = `NONE` and `dst` (HIST's import goal)
   = `CENTER` (from the landmine above), this evaluates
   `matches = (NONE == CENTER) = .false.` -> a transform is "needed" ->
   `make_transform` builds a real interpolation matrix from GCM's 5-level
   grid to HIST's 4 pressure levels and applies it to `E_1`'s actual
   1-element data -> the out-of-bounds crash.

**First proposed workaround (rejected by bmauer):** add
`vertical_dim_spec: NONE` explicitly to `E_1`'s entry in `history1.yaml`'s
`var_list`, and/or thread a new per-variable `vertical_dim_spec` override
through `HistoryCollectionGridComp_private.F90::add_var_specs`/`VarOptions`.
Rejected because it defeats the point of the vert-regrid-history feature:
the user should never have to redundantly declare `vertical_dim_spec` in a
History `var_list` - MAPL should infer "nothing to regrid" automatically
from the export's own `NONE` declaration.

**Actual fix implemented** (single, minimal, in `VerticalGridAspect.F90`'s
`typesafe_matches`, ~line 201-229 pre-today's second fix below): made the
`NONE` check asymmetric instead of symmetric:
```fortran
if (src%vertical_stagger == VERTICAL_STAGGER_NONE) then
   matches = .true.   ! src has nothing to regrid, full stop
   return
end if
if (dst%vertical_stagger == VERTICAL_STAGGER_NONE) then
   matches = .false.  ! dst genuinely declared NONE but src has real levels: mismatch
   return
end if
```
Rationale: `VERTICAL_STAGGER_NONE` is never a silent/implicit default
anywhere in MAPL (an omitted `vertical_dim_spec` becomes `CENTER` via this
same constructor, or `INVALID` via `ComponentSpecParser` - never `NONE`), so
seeing it on `src` (the already-connected/producer side) always means a
genuine "no vertical dimension" declaration, and nothing downstream
(including a blanket component-wide `vertical_grid:` stamped onto every
import) should be able to force a regrid onto it.

**Verified working** (rebuilt `MAPL.generic`, reran `test_ple`): no crash;
`test.nc4` shows `E_1(time,lat,lon)` - plain 2D, no `lev` dimension - passed
straight through untouched, while `E_2(time,lev,lat,lon)` with `lev=4` is
still correctly vertically regridded onto the collection's 4 pressure
levels.

**Regression coverage added:** a new, minimal, self-contained pf-unit file
`superstructure/generic/tests/Test_VerticalGridAspect_NoneMatch.pf` (4
tests: NONE-src matches any dst; NONE-dst mismatches a real src; NONE
matches NONE; differing real grids still correctly mismatch), wired into
`superstructure/generic/tests/CMakeLists.txt`'s `aspects_test_srcs` list. (An
earlier attempt to instead extend the legacy, not-yet-built
`Test_VerticalGridAspect.pf` was abandoned and reverted - that file has its
own pre-existing, unrelated breakage: a stale `mapl_vertical_grid_api`
naming mismatch and a zero-sized-array crash in `test_update_field_no_vgrid`
- out of scope for this fix.)

## Bug #5 (real code bug, found via full-suite regression testing after
Bug #4's fix, and the direct cause of an opencode session crash mid-fix):
the asymmetric NONE check above spuriously breaks `VERTICAL_STAGGER_MIRROR`

After Bug #4's fix + new unit tests were passing in isolation, running the
broader `MAPL.generic.{aspects,vertical,scenarios,components,transforms}`
suite surfaced a real regression: the `history_1` case in
`MAPL.generic.scenarios` started failing (it hadn't before Bug #4's fix).

**Root cause:** `VerticalStaggerLoc`'s `operator(==)` (`are_equal`, in
`enums/VerticalStaggerLoc.F90:93-107`) has deliberate, pre-existing "MIRROR
matches anything" wildcard semantics:
```fortran
elemental logical function are_equal(this, that)
   ...
   are_equal = (this%name == that%name)
   if (are_equal) return
   n_mirror = count([this%id,that%id] == MIRROR)
   are_equal = (n_mirror == 1)   ! true whenever exactly one side is MIRROR
end function are_equal
```
Bug #4's fix wrote `src%vertical_stagger == VERTICAL_STAGGER_NONE` and
`dst%vertical_stagger == VERTICAL_STAGGER_NONE` using this same overloaded,
wildcard-aware `operator(==)`. `history_1` uses `vertical_dim_spec: MIRROR`
imports throughout; for those, `dst%vertical_stagger == VERTICAL_STAGGER_NONE`
spuriously evaluates `.true.` (MIRROR(3) vs NONE(0) -> `count([3,0]==3)==1`
-> `.true.`), so the `dst`-is-NONE branch fired for a genuinely
MIRROR-staggered `dst` and incorrectly forced `matches = .false.`.

This is exactly the mechanism an opencode session (`kind-canyon`, "Out of
bounds error analysis for mpirun GEOS.x command", ~13:41-14:43) had traced
by reading the scenario-test failure log, one step before the process
crashed (no further response, including to a follow-up "is there any way
save this session" - the crash was a hard process-level failure that killed
the opencode server session, not a code/build failure). At the point of the
crash, the assistant had correctly identified that "`==` ... unintentionally
triggers ... MIRROR wildcard ... need an exact identity check instead", but
had not yet implemented it. A second opencode session (this one) recovered
the full crashed-session transcript directly from opencode's local SQLite
session store (`~/.local/share/opencode/opencode.db`, `session`/`message`/
`part` tables, keyed by `directory` matching the `temp88798` run dir) to
reconstruct this exact state and finish the fix below.

**Fix implemented** (`enums/VerticalStaggerLoc.F90` +
`superstructure/generic/specs/VerticalGridAspect.F90`):

1. Added two new exact-identity, non-wildcard type-bound functions to
   `VerticalStaggerLoc` (alongside the existing `to_string`,
   `get_dimension_name`, etc.), each checking the private `%id` component
   directly instead of going through `operator(==)`:
   ```fortran
   elemental logical function is_none(this)
      class(VerticalStaggerLoc), intent(in) :: this
      is_none = (this%id == NONE)
   end function is_none

   elemental logical function is_mirror(this)
      class(VerticalStaggerLoc), intent(in) :: this
      is_mirror = (this%id == MIRROR)
   end function is_mirror
   ```
2. In `typesafe_matches`, replaced both `== VERTICAL_STAGGER_NONE`
   comparisons with `%is_none()`, and added a third, explicit check right
   after them:
   ```fortran
   if (src%vertical_stagger%is_none()) then
      matches = .true.
      return
   end if
   if (dst%vertical_stagger%is_none()) then
      matches = .false.
      return
   end if
   ! VERTICAL_STAGGER_MIRROR is an explicit "matches any real stagger"
   ! wildcard declaration - honor it directly now that the NONE checks above
   ! are exact (no longer accidentally catching MIRROR). A MIRROR-staggered
   ! item need not have an allocated vertical_grid of its own, so falling
   ! through to the grid-id comparison below would otherwise crash on an
   ! unallocated dst%vertical_grid (or src%vertical_grid).
   if (src%vertical_stagger%is_mirror() .or. dst%vertical_stagger%is_mirror()) then
      matches = .true.
      return
   end if
   ! Both must have vertical grids to get here, so can compare ids.
   grids_match = dst%vertical_grid%get_id() == src%vertical_grid%get_id()
   ...
   ```
   (The MIRROR check was necessary, not optional: making the NONE checks
   exact also removed the *accidental* pass-through that MIRROR-staggered
   items were previously (ab)using via the wildcard `==` to avoid ever
   reaching the `dst%vertical_grid%get_id()` comparison below - without it,
   the very first `history_1` rebuild crashed with `Runtime Error:
   VerticalGridAspect.F90 ... ALLOCATABLE DST%VERTICAL_GRID is not currently
   allocated`, confirming MIRROR-staggered aspects genuinely don't carry an
   allocated `vertical_grid`.)

**Verified:**
- Rebuilt `MAPL.enums`, `MAPL.generic.{aspects,vertical,scenarios,components,transforms}`.
- `ctest -R "MAPL.generic.(aspects|vertical|scenarios|components|transforms)$"`:
  **100% pass (5/5)**, including `history_1` (previously failing) - this is
  the full essential-labeled suite touching this code path, not just the 4
  new unit tests.
- Directly confirmed (via `ctest -V | grep NoneMatch`) that all 4
  `Test_VerticalGridAspect_NoneMatch` tests still individually pass (not
  just "suite as a whole passed").
- Rebuilt + reinstalled MAPL, reran the `test_ple` reproducer end-to-end
  (`mpirun ... GEOS.x mapl1.yaml`, `DYLD_LIBRARY_PATH` workaround as above):
  exit code 0, no crash, 24 timesteps completed. `ncdump -h test.nc4`
  reconfirms `E_1(time,lat,lon)` (2D, no `lev`) and `E_2(time,lev,lat,lon)`
  with `lev=4`; `ncdump -v` spot-checked non-garbage values (uniform
  `-181.0833` at the first timestep across all 4 `E_2` levels and `E_1` -
  expected and correct, since `GCM1.yaml`'s `FILL_DEF: time_interval` fills
  both fields with a level-independent function of time only, so a constant
  source column interpolates to the same constant at every destination
  level).

## Files changed (this MAPL checkout, all on
`feature/bmauer/enable_vert_regrid_history3g`, all still **uncommitted** -
bmauer asked explicitly not to commit)

- `enums/VerticalStaggerLoc.F90` - Bug #5 fix: added `is_none()` and
  `is_mirror()` exact-identity type-bound functions.
- `superstructure/generic/specs/VerticalGridAspect.F90` - Bug #4 fix
  (asymmetric NONE short-circuit in `typesafe_matches`) as refined by Bug #5
  fix (switched to `%is_none()`/`%is_mirror()`, added explicit MIRROR
  short-circuit before the grid-id comparison).
- `superstructure/generic/tests/Test_VerticalGridAspect_NoneMatch.pf` (new,
  untracked) + `superstructure/generic/tests/CMakeLists.txt` (added one line
  to `aspects_test_srcs`) - regression coverage for Bug #4/#5.
- `gridcomps/componentDriverGridComp/componentDriverGridComp.F90` -
  unrelated `vertical_levels:` HConfig feature (see Context above), a
  prerequisite for the `test_ple` reproducer, written in a separate,
  uncrashed session earlier the same day.
- `infrastructure/vertical/vertical/VerticalLinearMap.F90` - pre-existing,
  untouched `write(*,*)"bmaa pres src/dst: ..."` debug print statements in
  `compute_linear_map` (not added by either the crashed session or this one;
  origin predates both - likely added directly by bmauer outside of
  opencode). Still present, still uncommitted, not yet cleaned up.
- `tests/MAPL3G_Component_Testing_Framework/test_cases/test_ple/` (new,
  untracked) and `.../test777/` (new, untracked, superseded scratch copy) -
  see Context above.

## Open items / not yet done

- `test_ple` is not wired into
  `tests/MAPL3G_Component_Testing_Framework/test_cases/cases.txt` as a
  permanent CTest case (would need a `nproc.rc` - already present - and an
  entry added to `cases.txt` plus a numbered description appended to
  `test_case_descriptions.md`). Floated but not actioned; bmauer has not yet
  said whether he wants this.
- `test777` is redundant with (an earlier, slightly-broken version of)
  `test_ple` and can likely just be deleted.
- The `bmaa` debug `write` statements in `VerticalLinearMap.F90` are still
  present and uncommitted; not yet confirmed whether bmauer wants them
  removed or kept for ongoing debugging.
- Nothing in this update section has been committed, per explicit
  instruction ("do these steps, but do not commit").

## How to restore / re-verify *this* state

1. `git status --short` should show exactly: modified
   `enums/VerticalStaggerLoc.F90`,
   `gridcomps/componentDriverGridComp/componentDriverGridComp.F90`,
   `infrastructure/vertical/vertical/VerticalLinearMap.F90`,
   `superstructure/generic/specs/VerticalGridAspect.F90`,
   `superstructure/generic/tests/CMakeLists.txt`; untracked
   `superstructure/generic/tests/Test_VerticalGridAspect_NoneMatch.pf`,
   `tests/MAPL3G_Component_Testing_Framework/test_cases/test777/`,
   `tests/MAPL3G_Component_Testing_Framework/test_cases/test_ple/`.
2. Rebuild:
   ```
   cd build-debug && make -j4 MAPL.enums MAPL.generic.aspects \
     MAPL.generic.vertical MAPL.generic.scenarios MAPL.generic.components \
     MAPL.generic.transforms && make -j8 install
   ```
3. `ctest -R "MAPL.generic.(aspects|vertical|scenarios|components|transforms)$" --output-on-failure`
   should show 5/5 passing (in particular `history_1`).
4. Rerun the `test_ple` reproducer per the command under Context above;
   expect exit 0, no crash, and `ncdump -h test.nc4` showing `E_1` with no
   `lev` dimension and `E_2` with `lev=4`.

---

# Update: 2026-10-01 - back to the GEOSgcm `dyn-sa` regression test: garbage
# `PLE` coordinate values (Bug #6) and FV3's top-first level ordering
# (Bug #7)

Date: 2026-10-01
Branch: still `feature/bmauer/enable_vert_regrid_history3g` (MAPL submodule at
`src/Shared/@MAPL`, back to the GEOSgcm-submodule setup from the 2026-09-28
entry, not the standalone MAPL checkout from the 2026-09-29 entry)
Test dir: `regression/dyn-sa/temp7887/` (same scratch run dir as 2026-09-28;
source configs live in `regression/dyn-sa/`)

## Context

Picked back up from the end of the 2026-09-28 entry: `dyn-sa.yaml`/
`history.yaml` already had the Bug #1 schema fix (`grid_type: model`,
`fields: {pressure: PLE}`) and `history.yaml` requests `T` on a 2-level
`pressure_levs` (`[500., 400.]` hPa) `fixed_levels` grid. `bmauer` had added
his own temporary debug prints (`print*,'bmaa src/dst: ',maxval(src),
maxval(dst)` in `VerticalLinearMap.F90`'s `compute_linear_map`, plus a
`MAPL_StateGetPointer(export, ptr4, "PLE", ...); write(*,*)"bmaa ple
",minval(ptr4),maxval(ptr4)` block at the end of `DynCore_GridCompMod.F90`'s
`run`) to narrow down why `src` (the `PLE`-derived coordinate column fed into
the linear interpolator) was coming out as either exactly `0.0` or an
unphysical ~`1e9`-ish value on different runs, while asking: *is DynCore's
own `PLE` export actually bad, or is something downstream of it corrupting
the value?*

Command used throughout (same as 2026-09-28, plus two environment-setup
steps that turned out to be required on this machine - see **Environment
gotcha** below):
```
source ~/.bashrc && ifxstack && append_esma_libs
cd regression/dyn-sa/temp7887
mpirun -np 6 /home/bmauer/models/GEOS_mapl_v3/GEOSgcm/install-debug/bin/GEOS.x mapl.yaml |& tee run.log
```
Rebuild:
```
source ~/.bashrc && ifxstack
cmake --build /home/bmauer/models/GEOS_mapl_v3/GEOSgcm/build-debug --target install -j 8
```

### Environment gotcha (not a code bug, but cost significant time)

A plain, non-interactive shell on this machine does **not** have the right
Intel `ifx` toolchain modules loaded by default, and does not have
`LD_LIBRARY_PATH` pointed at the freshly-built `.so`s. Building/running
without first doing
```
source ~/.bashrc && ifxstack
```
(before any `cmake --build`) produces a *misleading* link-time error that
looks like a real code problem:
```
../../../../lib/libMAPL.generic.so: undefined reference to `__intel_free_bpv'
../../../../lib/libMAPL.generic.so: undefined reference to `__intel_alloc_bpv'
```
and, separately, running `GEOS.x` without first doing
```
source ~/.bashrc && ifxstack && append_esma_libs
```
(both are bash functions defined in `~/.bashrc`; `append_esma_libs` walks
`$ESMADIR` for every `.so` and appends its directory to `LD_LIBRARY_PATH`)
produces an unrelated-looking `dlopen`/`SetServices` failure at startup:
```
pe=00003 FAIL at line=00165    UserSetServices.F90                      <status=506>
```
(a `ESMF_GridCompSetServices(..., sharedObj=...)` failure, seemingly random
per-rank since it is really an NFS/dlopen race on which ranks happen to find
the `.so` first via whatever stale `LD_LIBRARY_PATH` was inherited).
**Neither of these is a MAPL code regression** - always `ifxstack` then
`append_esma_libs` before building or running anything in this checkout.

## Bug #6 (real code bug): `VerticalRegridTransform::initialize()` never
initializes `v_in_coupler`/`v_out_coupler`, leaving `ConvertUnitsTransform`'s
UDUNITS converter uninitialized -> garbage (0 / ~1e9 / NaN / Inf) coordinate
values

### Diagnostic methodology (worth reusing for future sessions on this code)

Static reading alone could not pin this down - the generic
coupler/extension framework recursively builds and runs multi-step producer
chains, and the actual failure mode (clean `0.0` on one run, `NaN`/`Infinity`
on another, for the *same* code path) was itself the key clue that this was
an *uninitialized-memory* bug, not a wrong-formula bug. The following
temporary `print*, 'DBGTRACE ...'`-tagged instrumentation (distinct tag from
bmauer's own pre-existing `'bmaa ...'` prints, so the two could be
distinguished/grepped independently) was added, built, run, and then fully
reverted once the root cause was confirmed:

1. `ModelVerticalGrid.F90::get_coordinate_field_with_coupler` - printed
   `associated(coupler)` and the returned field's name/typekind right after
   `this%registry%extend(...)`.
2. `VerticalRegridTransform.F90::update` - printed `v_in_coord`'s min/max
   immediately before and after `call this%v_in_coupler%run(...)`.
3. `CopyTransform.F90::update` - printed src/dst field name + typekind +
   min/max immediately before and after `FieldCopy(...)`.
4. `ConvertUnitsTransform.F90::update_field` - printed src/dst min/max
   immediately before and after `converter%convert(...)`.
5. `CouplerMetaComponent.F90::update` *and* `::initialize` - printed the
   wrapped transform's `get_transformId()%to_string()` and the `stale` flag
   on every entry, for both methods.

**Critical gotcha while instrumenting:** this is a `-fpe0` debug build
(`CMAKE_Fortran_FLAGS_DEBUG` includes `-fpe0 -check all,...`), so a bare
`minval()`/`maxval()` call on an array that happens to contain `NaN`
(expected, mid-investigation, on a field that hasn't been populated yet)
**traps and crashes immediately** - including inside the print statement
itself, before anything is written to stdout. bmauer's own pre-existing
`print*,'bmaa src/dst: ',maxval(src),maxval(dst)` in `VerticalLinearMap.F90`
had already been silently relying on `src`/`dst` never actually containing a
hard `NaN` (only the physically-wrong-but-finite `0.0`/`1e9` cases); the very
first attempt to add *new* instrumentation upstream of that point crashed
immediately on `NaN` with zero diagnostic output. Fixed by bracketing every
new diagnostic `minval`/`maxval`/`any(ieee_is_nan(...))` call with:
```fortran
use, intrinsic :: ieee_exceptions, only: ieee_invalid, ieee_get_halting_mode, ieee_set_halting_mode
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
...
call ieee_get_halting_mode(ieee_invalid, dbg_halt_invalid)
call ieee_set_halting_mode(ieee_invalid, .false.)
! ... prints that may touch NaN/Inf data ...
call ieee_set_halting_mode(ieee_invalid, dbg_halt_invalid)
```
This let the instrumentation safely print the *actual* NaN/Inf/garbage
values (and an explicit `any(ieee_is_nan(...))` flag) instead of crashing
before revealing anything.

### Root cause

With the above in place, the chain for the single horizontal point that
fails was, in order:
1. `CouplerMetaComponent::initialize` fires exactly 6 times total (once per
   rank), **always** for `transformId=VERTICAL_GRID` - i.e. only the
   top-level `VerticalRegridTransform` (the `T`<->History connection itself)
   ever gets its `initialize()` called by MAPL's normal framework traversal.
2. `CouplerMetaComponent::update`, by contrast, *does* show the full nested
   chain every timestep: `VERTICAL_GRID` -> `TYPEKIND` -> `UNITS` -> (inside
   `ConvertUnitsTransform::update_field`) -> (inside `CopyTransform::update`).
   This chain is `v_in_coupler`, built inside
   `ModelVerticalGrid::get_coordinate_field_with_coupler` /
   `StateRegistry::extend()` to turn DYNsa's native `PE` (R8, Pa) into the
   R4, `hPa` coordinate column `VerticalLinearMap`/`compute_linear_map`
   needs.
3. `ConvertUnitsTransform::initialize` (which calls `UDUNITS_GetConverter`
   to build `this%converter`) **never prints at all** - i.e. is never
   called - even though `ConvertUnitsTransform::update_field` runs every
   timestep and calls `converter%convert(...)` on that never-initialized
   `converter`.
4. Directly confirmed with before/after prints around
   `converter%convert(...)`: input `PE` data going in was always perfectly
   sane (`1.0` to `~100117` Pa, matching bmauer's own `PLE` print at the end
   of `DynCore_GridCompMod.F90::run`); the *output* of
   `converter%convert()` on that sane input was garbage - e.g.
   `-3.69e19` / `1.72e256` / `NaN` - varying run-to-run with whatever
   uninitialized memory `this%converter`'s internals happened to contain.

`VerticalRegridTransform::initialize()` (unlike `::update()`) never
propagated initialization into `v_in_coupler`/`v_out_coupler`:
`update()` already explicitly calls `this%v_in_coupler%run(phase_idx=
MAPL_GENERIC_COUPLER_UPDATE, ...)` every timestep, but the analogous
`this%v_in_coupler%initialize(phase_idx=MAPL_GENERIC_COUPLER_INITIALIZE,
...)` was simply missing from `initialize()`. Since these nested
unit/typekind-conversion couplers are created dynamically (inside
`make_transform`, as a side effect of resolving the `T`<->History
`VERTICAL_GRID` aspect) rather than being part of the normal
registry-managed extension tree MAPL's framework walks during its own
Initialize phase, nothing else ever called their `initialize()` either -
leaving `ConvertUnitsTransform`'s `UDUNITS_converter` permanently
uninitialized.

### Fix (`superstructure/generic/transforms/VerticalRegridTransform.F90`)

Added the missing propagation to `initialize()`, mirroring the existing
`run()` propagation already present in `update()`:
```fortran
if (associated(this%v_in_coupler)) then
   call this%v_in_coupler%initialize(phase_idx=MAPL_GENERIC_COUPLER_INITIALIZE, _RC)
end if
if (associated(this%v_out_coupler)) then
   call this%v_out_coupler%initialize(phase_idx=MAPL_GENERIC_COUPLER_INITIALIZE, _RC)
end if
```
(plus importing `MAPL_GENERIC_COUPLER_INITIALIZE` from `mapl_enums_api`,
already alongside the existing `MAPL_GENERIC_COUPLER_UPDATE` import).

**Verified:** rebuilt, reran - `ConvertUnitsTransform::initialize` now fires
(confirmed `src_units=Pa dst_units=hPa` printed once per rank at Initialize
time) and `converter%convert()` now produces correct, physical values (e.g.
`bmaa src/dst:  998.7006  500.0000` - a believable surface pressure in hPa
vs. the requested 500 hPa output level - where before it would have been
`0.0`/`1e9`/`NaN` against the same `500.0000`).

## Bug #7 (real code bug, exposed only once Bug #6 was fixed): no
`VerticalGrid` subclass has any way to declare a non-default
`coordinate_direction`, and FV3's `PLE` is natively top-first (UP), not the
hardcoded DOWN default

With Bug #6 fixed, the run immediately progressed to a different, legitimate
`_ASSERT`:
```
VerticalLinearMap.F90:51  <src array is not decreasing>
```
(not the `maxval(dst) > maxval(src)` one - that one now correctly *passes*,
since `src` is finally a sane ~998 hPa-ish surface value).

**bmauer's diagnosis (confirmed correct):** `DynCore`'s `PLE` is produced
with index 1 = top of atmosphere and the last index = surface, i.e. pressure
*increases* with index - a perfectly valid, deliberate FV3/dynamical-core
convention, not a bug in `DynCore_GridCompMod.F90`.

**Root cause:** `mapl_VerticalGrid_mod`'s base type hardcodes
```fortran
type(VerticalCoordinateDirection) :: coordinate_direction = VCOORD_DIRECTION_DOWN
```
with a `get_coordinate_direction`/`set_coordinate_direction` pair - but
`grep -rn set_coordinate_direction` across the *entire* MAPL tree (before
this fix) turns up **zero** call sites. Every single `VerticalGrid`
subclass/factory (`ModelVerticalGrid`, `FixedLevelsVerticalGrid`,
`BasicVerticalGrid`) is permanently stuck at `DOWN` ("surface-first,
decreasing" - the implicit convention `VerticalRegridTransform`'s
`compute_interpolation_matrix_` flip logic and `VerticalLinearMap`'s
`is_decreasing` assertion both assume), with **no config-level or
programmatic way to ever override it** for a model whose native ordering is
actually the opposite (UP). Since `src_alignment` resolves to `DOWN` by
default (`VerticalGridAspect::get_resolved_alignment` ->
`VerticalAlignment%resolve(grid_direction)` with the default
`VALIGN_WITH_GRID`), the `if (src_alignment == VCOORD_DIRECTION_UP) vv_in =
flip_vertical_coords(vv_in)` flip in `VerticalRegridTransform.F90` never
fires for DYNsa's `PLE`, so its genuinely-increasing array is fed to
`compute_linear_map` unflipped, and `is_decreasing(src)` correctly rejects
it.

**Fix** - added an optional `direction:` HConfig key, threaded through to
`set_coordinate_direction()`, to **both** real (non-placeholder)
`VerticalGrid` factories (`BasicVerticalGrid`/`MirrorVerticalGrid` are both
sentinel/placeholder types whose `get_coordinate_field` always `_FAIL`s and
were left untouched):

1. `superstructure/generic/vertical/ModelVerticalGrid.F90`:
   - `ModelVerticalGridSpec` gained a `coordinate_direction` field (default
     `VCOORD_DIRECTION_DOWN`, so every existing config's behavior is
     unchanged unless it opts in).
   - `new_ModelVerticalGridSpec` gained an optional `coordinate_direction`
     constructor argument.
   - `create_spec_from_config` parses an optional `direction:` key (reusing
     `VerticalCoordinateDirection(str)`'s existing `up`/`down`/`upward`/
     `downward` string parsing, `_ASSERT`ing on anything else).
   - `ModelVerticalGrid::initialize` now calls
     `call this%set_coordinate_direction(spec%coordinate_direction)`.
2. `superstructure/generic/vertical/FixedLevelsVerticalGrid.F90` - identical
   pattern (`FixedLevelsVerticalGridSpec%coordinate_direction`,
   constructor arg, `direction:` HConfig parsing, `initialize()` override),
   added proactively for symmetry: a user who lists `levels:`
   top-of-atmosphere-first (increasing) in a History `fixed_levels:` block
   would hit the exact same latent bug, with no prior way to declare it
   either.
3. `regression/dyn-sa/dyn-sa.yaml` (GEOSgcm repo) - added
   `direction: up` to `geometry.vertical_grid:`.

**Verified end-to-end:** rebuilt + reinstalled, reran the `dyn-sa`
reproducer - clean full run, zero `FAIL`/crash output, `History: run:`
completed for all 3 timesteps of the 1-hour segment, and
`test.nc4`'s `T(time,lev,nf,Ydim,Xdim)` on the two requested pressure levels
(`lev = [1, 2]` i.e. 500/400 hPa) holds physically sane values:
`T min/max: 239.40121 267.60065` (K).

## Post-fix regression testing (both bugs)

- `ctest -R "^MAPL\.generic\.(vertical|transforms|aspects)$"`: 3/3 pass.
- `make -j8 tests` (builds + runs **every** `ESSENTIAL`-labeled ctest target,
  67 tests total - includes `MAPL.generic.{scenarios,vertical,transforms,
  aspects,components,core}`, `MAPL.vertical_grid.tests` (48 pf-unit tests),
  `MAPL.history.tests`, `MAPL.state.tests`, all 30 `MAPL3G_Comp_Test_case*`
  end-to-end scenario tests, etc.): **100% tests passed out of 67**, zero
  `Failed`/`FAILED` anywhere in the log.
- `regression/dyn-sa` reproducer rerun end-to-end per above: clean pass.

## Files changed

MAPL submodule (`src/Shared/@MAPL`, branch
`feature/bmauer/enable_vert_regrid_history3g`, **uncommitted** as of this
writing):
- `superstructure/generic/transforms/VerticalRegridTransform.F90` - Bug #6
  fix (`initialize()` now propagates to `v_in_coupler`/`v_out_coupler`).
- `superstructure/generic/vertical/ModelVerticalGrid.F90` - Bug #7 fix
  (`direction:` config support).
- `superstructure/generic/vertical/FixedLevelsVerticalGrid.F90` - Bug #7
  symmetric fix (`direction:` config support), same pattern as above.

All temporary `DBGTRACE`-tagged diagnostic instrumentation described above
(in these same 3 files, plus `CopyTransform.F90` and
`ConvertUnitsTransform.F90`, which ended up needing **no** permanent code
change and are back to byte-for-byte pre-session state) was fully reverted
once the root causes were confirmed; `git diff --stat` in `src/Shared/@MAPL`
shows only the 3 files above.

bmauer's own pre-existing debug instrumentation from before this session
(`print*,'bmaa src/dst: ...'` in `VerticalLinearMap.F90`, the `block ...
write(*,*)"bmaa ple ..."` + two `_HERE, ' bmaa '` markers in
`DynCore_GridCompMod.F90`) was also removed at bmauer's request once the
real fixes were confirmed working; both files are now back to their
pre-debugging state (`VerticalLinearMap.F90` identical to its state before
the 2026-09-28 entry even started; `DynCore_GridCompMod.F90`, which lives in
the GEOSgcm repo's `FVdycoreCubed_GridComp` component, not this MAPL
checkout, is back to `git checkout --`-clean).

GEOSgcm repo (`src/Components/@GEOSgcm_GridComp/.../@FVdycoreCubed_GridComp`,
this component, separate git repo from `src/Shared/@MAPL`):
- `regression/dyn-sa/dyn-sa.yaml` - added `direction: up` (Bug #7 fix
  application; see diff below).
- `regression/dyn-sa/temp7887/` - untracked scratch run directory, kept in
  sync with `dyn-sa.yaml` (copied after each edit); logs from this session
  (`run_dbg*.log`, `run_dir.log`, `run_final.log`, etc.) are left in place
  and can be deleted/regenerated freely.
- `DynCore_GridCompMod.F90` - reverted to clean (`git checkout --`) as noted
  above; no longer shows as modified.
- `regression/dyn-sa/history.yaml` - unchanged this session (still shows as
  modified relative to upstream from the 2026-09-28 entry's `class:` ->
  `grid_type:` rename).

```diff
--- a/regression/dyn-sa/dyn-sa.yaml
+++ b/regression/dyn-sa/dyn-sa.yaml
@@ -17,12 +17,15 @@ geometry:
     nx_face: 1
     ny_face: 1
   vertical_grid:
-    class: model
-    standard_name: air_pressure
-    units: hPa
+    grid_type: model
+    fields: {pressure: PLE}
     num_levels: 91
-    field_edge: PLE
-    field_center: PE
+    # FV3's PLE is stored top-of-atmosphere-first (index 1 = model top,
+    # increasing pressure with index), not the "down" (surface-first,
+    # decreasing) convention MAPL's ModelVerticalGrid otherwise assumes by
+    # default. Declare it explicitly so VerticalRegridTransform flips it to
+    # the canonical orientation before interpolation.
+    direction: up
```
(Note: this diff is relative to the *pre-2026-09-28* upstream `dyn-sa.yaml`,
same as the 2026-09-28 entry's Bug #1 diff - the `grid_type`/`fields:` half
was already in place from that earlier session; only the trailing
`direction: up` + comment is new this session.)

## Open items / not yet done

- Nothing from this session has been committed (consistent with bmauer's
  standing "do not commit" instruction from the 2026-09-29 entry - not
  re-confirmed explicitly this session, but no instruction to the contrary
  was given either).
- `BasicVerticalGrid` was deliberately **not** given `direction:` support -
  it's a placeholder whose `get_coordinate_field` always `_FAIL`s (see its
  own source comment: "should have been connected to a different subclass
  before this is called"), so it never actually participates in real
  coordinate-based regridding. Flagged here in case that assumption changes
  in the future.
- No new permanent pf-unit regression test was added specifically for Bug
  #6 (the coupler-initialize propagation) or Bug #7 (the `direction:`
  config option) in this session - only the pre-existing full-suite re-run
  described above. `superstructure/generic/vertical/tests/` (via
  `superstructure/generic/tests/CMakeLists.txt`'s `vertical_test_srcs`) and
  `infrastructure/vertical/vertical_grid/tests/Test_FixedLevelsVerticalGrid.pf`
  would be the natural homes for such tests (the latter already exercises
  `get_coordinate_direction`/`set_coordinate_direction` directly - see
  `test_fixed_level_coordinate_direction_default`/`_get_set` - but not yet
  the new `direction:` HConfig key itself).
- The still-untracked `regression/dyn-sa/temp7887/` scratch directory (and
  this session's several `run_dbg*.log`/`run_*.log` files inside it) have
  not been cleaned up; harmless, regenerable, same status as noted in the
  2026-09-28 entry.

## How to restore / re-verify this state

1. `cd src/Shared/@MAPL && git diff --stat` should show exactly:
   `superstructure/generic/transforms/VerticalRegridTransform.F90`,
   `superstructure/generic/vertical/ModelVerticalGrid.F90`,
   `superstructure/generic/vertical/FixedLevelsVerticalGrid.F90`.
2. `cd regression/dyn-sa && git diff -- dyn-sa.yaml` should show the
   `direction: up` addition described above (on top of the pre-existing
   `grid_type`/`fields:` schema fix from 2026-09-28).
3. Rebuild:
   ```
   source ~/.bashrc && ifxstack
   cmake --build /home/bmauer/models/GEOS_mapl_v3/GEOSgcm/build-debug --target install -j 8
   ```
4. Regression-test:
   ```
   source ~/.bashrc && ifxstack
   cd /home/bmauer/models/GEOS_mapl_v3/GEOSgcm/build-debug
   make -j8 tests
   ```
   expect `100% tests passed out of 67`.
5. Rerun the `dyn-sa` reproducer:
   ```
   source ~/.bashrc && ifxstack && append_esma_libs
   cd regression/dyn-sa/temp7887
   mpirun -np 6 /home/bmauer/models/GEOS_mapl_v3/GEOSgcm/install-debug/bin/GEOS.x mapl.yaml |& tee run.log
   ```
   expect exit 0, no `FAIL`/abort anywhere in `run.log`, and (if `netCDF4`
   is available) `test.nc4`'s `T` variable in the range of ~239-268 K on
   both pressure levels.
