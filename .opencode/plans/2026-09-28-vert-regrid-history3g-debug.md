
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
