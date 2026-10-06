# Vertical regrid in History3G: bugs found and fixes

Branch: `feature/bmauer/enable_vert_regrid_history3g`

Requesting vertically-regridded output (e.g. `T`, `[U,V]` on fixed pressure
levels) from History3G exposed the bugs below. Each entry gives the symptom,
root cause, and fix.

## Bug 1: wrong `vertical_grid` schema in `dyn-sa.yaml` (config)

- **Symptom:** `StateItemSpec.F90 <cannot connect aspect VERTICAL_GRID>`.
- **Cause:** `geometry.vertical_grid` used `class: model` / `field_edge` /
  `field_center`. `ModelVerticalGridFactory::supports_config` requires
  `grid_type: model` plus a `fields:` map. The config was rejected and fell
  through to `BasicVerticalGridFactory`, which only supports `<unknown>`
  and so can never match History's `pressure` dimension.
- **Fix:**
  ```yaml
  vertical_grid:
    grid_type: model
    fields: {pressure: PLE}
    num_levels: 91
  ```
  The same stale schema was fixed in
  `gridcomps/FakeParent/fake-parent-example.yaml`. `history.yaml`'s
  `pressure_levs` was also changed from `class:` to `grid_type:` for clarity.

## Bug 2: `[U,V]` bracket variable fails with `cannot connect aspect CLASS`

- **Cause:** `make_transform.F90` builds the coordinate-field request with
  `coord_aspects = other_aspects`, which copies the payload's `CLASS` aspect.
  For a `[U,V]` entry that is a vector/bracket class, which
  `FieldClassAspect::matches_a` does not accept. `UNITS` and `STANDARD_NAME`
  were already overridden; `CLASS` was not.
- **Fix** (`superstructure/generic/specs/VerticalGridAspect/make_transform.F90`):
  override with a wildcard, since the coordinate field is always a plain
  scalar field:
  ```fortran
  call coord_aspects%insert(CLASS_ASPECT_ID, WildcardClassAspect())
  ```

## Bug 3: `GEOM_IN` runtime crash (forrtl 408) in the coupler

- **Symptom:** `Attempt to fetch from allocatable variable GEOM_IN when it is
  not allocated` in `CouplerMetaComponent.F90`.
- **Cause:** `update_time_varying_field_field` and
  `update_time_varying_fieldbundle_field` compare the new geom against the
  cached geom with `/=`. On the first `update()` of a new coupler, which is
  what the nested coordinate-field regrid coupler now is, the cached geom is
  unallocated and `ESMF_Geom`'s `/=` crashes.
- **Fix** (`superstructure/generic/transforms/CouplerMetaComponent.F90`): added
  a `geom_differs(new_geom, cached_geom)` helper that returns `.true.` when the
  cached geom is not allocated. It replaces all four `/=` comparisons.

## Bug 4: `vertical_dim_spec: NONE` fields were vertically regridded

- **Symptom:** out-of-bounds subscript in `CSR_SparseMatrix.F90` during
  `regrid_field_` for a 2D field (`E_1`) in a collection with a
  collection-wide `vertical_grid`.
- **Cause:** History imports never set `vertical_stagger`, so they default to
  `CENTER`. `MAPL_GridCompSetVerticalGrid` then stamps every import with the
  collection's grid. The original `typesafe_matches` treated NONE
  symmetrically (`src == NONE` vs `dst == CENTER` gave a mismatch), so a
  transform was built and applied to 1-element data.
- **Fix** (`superstructure/generic/specs/VerticalGridAspect.F90`,
  `typesafe_matches`): make the NONE check asymmetric. If `src` is NONE it
  matches (nothing to regrid). If `dst` is NONE but `src` has real levels it
  does not match. Users do not need to repeat `vertical_dim_spec` in a History
  `var_list`.
- **Test:** `superstructure/generic/tests/Test_VerticalGridAspect_NoneMatch.pf`
  (4 tests), added to `aspects_test_srcs` in the tests `CMakeLists.txt`.

## Bug 5: Bug 4's fix broke `VERTICAL_STAGGER_MIRROR`

- **Symptom:** the `history_1` scenario test started failing.
- **Cause:** `VerticalStaggerLoc`'s `operator(==)` treats MIRROR as matching
  anything. `dst%vertical_stagger == VERTICAL_STAGGER_NONE` was therefore true
  for MIRROR, forcing a spurious mismatch.
- **Fix:**
  - `enums/VerticalStaggerLoc.F90`: added exact-identity, non-wildcard
    `is_none()` and `is_mirror()` type-bound functions.
  - `VerticalGridAspect.F90::typesafe_matches`: use `%is_none()` for the NONE
    checks, and add an explicit MIRROR short-circuit (`matches = .true.`)
    before the grid-id comparison. MIRROR aspects have no allocated
    `vertical_grid`, so reaching that comparison would crash.

## Bug 6: nested coordinate couplers never initialized

- **Symptom:** garbage coordinate values (0, ~1e9, NaN, Inf) in the
  `PLE`-derived column fed to `compute_linear_map`, varying run to run.
- **Cause:** `VerticalRegridTransform::update()` runs `v_in_coupler` /
  `v_out_coupler`, but `initialize()` never initialized them. These nested
  unit/typekind-conversion couplers are created inside `make_transform` and are
  not part of the registry's extension tree, so nothing else initialized them.
  `ConvertUnitsTransform`'s UDUNITS converter stayed uninitialized, and its
  output was garbage.
- **Fix** (`superstructure/generic/transforms/VerticalRegridTransform.F90`):
  `initialize()` now calls `initialize(phase_idx=MAPL_GENERIC_COUPLER_INITIALIZE,
  _RC)` on `v_in_coupler` and `v_out_coupler` when they are associated.

## Bug 7: no way to declare a `VerticalGrid`'s coordinate direction

- **Symptom:** `VerticalLinearMap.F90 <src array is not decreasing>` for FV3's
  `PLE`, which is top-of-atmosphere first (pressure increases with index).
- **Cause:** `VerticalGrid%coordinate_direction` is hardcoded to
  `VCOORD_DIRECTION_DOWN`, and `set_coordinate_direction` had no callers. The
  flip in `VerticalRegridTransform` therefore never fired for sources that are
  natively UP.
- **Fix:** added an optional `direction:` config key (`up`/`down`), wired
  through to `set_coordinate_direction()`. The default is unchanged (DOWN).
  - `superstructure/generic/vertical/ModelVerticalGrid.F90`: new
    `coordinate_direction` field on the spec, an optional constructor
    argument, `direction:` parsing in `create_spec_from_config`, and a
    `set_coordinate_direction` call in `initialize`.
  - `superstructure/generic/vertical/FixedLevelsVerticalGrid.F90`: the same
    change, for symmetry.
  - `regression/dyn-sa/dyn-sa.yaml` (GEOSgcm): added `direction: up`.
  - `BasicVerticalGrid` and `MirrorVerticalGrid` were left alone. They are
    placeholders whose `get_coordinate_field` always fails.

## Files changed

MAPL:
- `superstructure/generic/specs/VerticalGridAspect/make_transform.F90` (Bug 2)
- `superstructure/generic/transforms/CouplerMetaComponent.F90` (Bug 3)
- `superstructure/generic/specs/VerticalGridAspect.F90` (Bugs 4, 5)
- `enums/VerticalStaggerLoc.F90` (Bug 5)
- `superstructure/generic/tests/Test_VerticalGridAspect_NoneMatch.pf` (new) and
  `superstructure/generic/tests/CMakeLists.txt` (Bugs 4, 5)
- `superstructure/generic/transforms/VerticalRegridTransform.F90` (Bug 6)
- `superstructure/generic/vertical/ModelVerticalGrid.F90` and
  `superstructure/generic/vertical/FixedLevelsVerticalGrid.F90` (Bug 7)
- `gridcomps/FakeParent/fake-parent-example.yaml` (Bug 1, stale example)
- `gridcomps/componentDriverGridComp/componentDriverGridComp.F90`: new
  `vertical_levels:` HConfig support, used by the `test_ple` reproducer under
  `tests/MAPL3G_Component_Testing_Framework/test_cases/`

GEOSgcm (`regression/dyn-sa/`):
- `dyn-sa.yaml`: `grid_type: model`, `fields: {pressure: PLE}`, `direction: up`
- `history.yaml`: `class:` renamed to `grid_type:` for `pressure_levs`

## Verification

- `make tests` (all 67 ESSENTIAL ctest targets, including `history_1`) passes.
- The `dyn-sa` regression runs cleanly. `T` on the 500/400 hPa levels has
  sane values (about 239-268 K).
- The `test_ple` reproducer keeps `E_1` (NONE) 2D and regrids `E_2` onto the
  4 pressure levels.

## Open items

- No pf-unit tests yet for Bug 6 (coupler initialize propagation) or Bug 7
  (the `direction:` key).
- `test_ple` is not yet in `test_cases/cases.txt`. The `test777` scratch copy
  is superseded by it and can be deleted.
- Nothing is committed.
