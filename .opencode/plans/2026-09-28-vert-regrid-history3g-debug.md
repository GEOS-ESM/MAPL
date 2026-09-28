
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
