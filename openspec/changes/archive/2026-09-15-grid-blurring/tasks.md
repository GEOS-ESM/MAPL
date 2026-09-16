## 1. Prerequisite fix

- [x] 1.1 Confirm `infrastructure/geom/LatLon/LatLonGeomSpec.F90` compiles
      cleanly (the `procedure :: equal_to` / `procedure :: get_horz_ij_index_r4`
      merge artifact has already been corrected on this branch) by building
      the `nag` tree before starting new work.

## 2. Read tolerance attribute directly in the geom layer (no pfio changes)

- [x] 2.1 In `make_LatLonGeomSpec_from_metadata.F90`, call
      `file_metadata%has_attribute("coordinate_tolerance")` /
      `get_attribute(...)` (existing generic `pfio/FileMetadata.F90` API)
      inline to obtain the tolerance, defaulting to `0.0_R8` when absent.
      Do NOT add any new method to `pfio/FileMetadata.F90` or
      `base/FileMetadataUtilities.F90`.
- [x] 2.2 Add a focused unit test at the geom-spec level (not in pfio)
      covering: attribute present vs. absent vs. an unexpected
      type/negative value on the `FileMetadata` passed to
      `make_LatLonGeomSpec_from_metadata`.

## 3. CoordinateAxis tolerance support

- [x] 3.1 Add a `tolerance` field (real(R8), default `0.0_R8`) to
      `CoordinateAxis` (`infrastructure/geom/CoordinateAxis.F90`).
- [x] 3.2 Update `CoordinateAxis::equal_to`
      (`infrastructure/geom/CoordinateAxis/equal_to.F90`) to combine
      `a%tolerance`/`b%tolerance` via `max()` and replace the exact
      `all(a%centers == b%centers)` / `all(a%corners == b%corners)` checks
      with `all(abs(a%centers - b%centers) <= tol)` /
      `all(abs(a%corners - b%corners) <= tol)`, keeping the existing
      `size(...)` short-circuits unchanged.
- [x] 3.3 Add/extend unit tests for `CoordinateAxis::equal_to` covering:
      equal within tolerance, not equal outside tolerance, size mismatch
      short-circuits regardless of tolerance, default tolerance (0) is
      strict.
      Post-review correction: the `max(a%tolerance, b%tolerance)`
      combination rule in 3.2 was wrong (see design.md Decision 2) and
      was replaced with a one-sided rule using only `b`'s (the new
      candidate's) tolerance, scaled by `b`'s own local grid spacing
      (DX) per Decision 4 - not an absolute value. Tests were extended
      accordingly with `test_equal_to_is_directional_not_symmetric` and
      `test_equal_to_single_point_axis_has_no_spacing`.

## 4. Thread tolerance through LatAxis/LonAxis/LatLonGeomSpec

- [x] 4.1 Update `LatAxis`/`LonAxis` construction paths
      (`infrastructure/geom/LatLon/LatAxis.F90`, `LonAxis.F90`) so the
      tolerance set on their underlying `CoordinateAxis` is preserved
      (no change needed to their `equal_to`, which already forwards to
      `CoordinateAxis::equal_to`).
- [x] 4.2 Update `make_LatAxis`/`make_LonAxis` (called from
      `make_LatLonGeomSpec_from_metadata.F90`) to accept/set the
      coordinate tolerance obtained from `FileMetadata` (task 2.1) on the
      constructed axis.
- [x] 4.3 Update `make_LatLonGeomSpec_from_metadata.F90` to call the
      inline `FileMetadata` attribute read (task 2.1) and pass the result
      through to `make_LonAxis`/`make_LatAxis`.
- [x] 4.4 Confirm `make_LatLonGeomSpec_from_hconfig.F90` (the non-file
      construction path) leaves tolerance at its default (`0.0_R8`) -
      no hconfig-driven tolerance in this change.

## 5. ExtData: per-collection tolerance config and attribute stamping

- [x] 5.1 Add an optional `real, allocatable :: coordinate_tolerance`
      field to `ExtDataCollection` (`gridcomps/extdata/ExtDataCollection.F90:10-24`).
- [x] 5.2 In `new_ExtDataCollection`, parse an optional
      `"coordinate_tolerance"` YAML key (same `ESMF_HConfigIsDefined`/
      `ESMF_HConfigAsR4` pattern used for `valid_range`), leaving the
      field unallocated when absent.
- [x] 5.3 Add `get_coordinate_tolerance`/`is_coordinate_tolerance_allocated`
      accessors on `ExtDataCollection`, mirroring the existing
      `valid_range` accessors (`ExtDataCollection.F90:189-212`).
- [x] 5.4 In `PrimaryExport` (`gridcomps/extdata/PrimaryExport.F90`),
      capture the collection's configured tolerance (if any) at
      construction time (`new_PrimaryExport`, alongside how
      `client_collection_id` is captured).
- [x] 5.5 At both places `PrimaryExport` loads file metadata and is about
      to request a geom (`PrimaryExport.F90:152-154` and `:211-213`),
      call `metadata%metadata%add_attribute("coordinate_tolerance", ...)`
      (existing generic `FileMetadata` API) when a tolerance was
      configured, immediately before `geom_mgr%get_mapl_geom_from_metadata`.
      Do NOT modify `DataCollection.F90` or `DataSetNode.F90`.
- [x] 5.6 Add/extend ExtData unit tests: config parsing (`coordinate_tolerance`
      present/absent in a collection's YAML), and that `PrimaryExport`
      stamps (or omits) the attribute on the `FileMetadata` it passes to
      `GeomManager` accordingly.
      Post-review correction: PR review determined ExtData's default
      (when the key is absent) must be nonzero, not strict/0, to
      preserve MAPL2's historical default-tolerant behavior for
      existing users (see design.md Decision 6). `coordinate_tolerance`
      is no longer `allocatable`; it always has an effective value
      (explicit or `DEFAULT_COORDINATE_TOLERANCE = 0.1`), the
      `is_coordinate_tolerance_allocated` accessor was removed, and
      `PrimaryExport` now stamps the attribute unconditionally rather
      than only "when a tolerance was configured".

## 6. Tests: GeomManager and LatLonGeomSpec (tolerant comparison)

- [x] 6.1 In `infrastructure/geom/tests/Test_LatLonGeomSpec.pf`, add
      `equal_to` tests for `LatLonGeomSpec` mirroring the `CoordinateAxis`
      cases (task 3.3) at the spec level, including a decomposition
      mismatch case that must reject regardless of tolerance.
- [x] 6.2 In `infrastructure/geom/tests/Test_GeomManager.pf`, add
      `test_reuse_geom_within_tolerance`: two `FileMetadata` objects with
      slightly perturbed lat/lon coordinates, each with a
      `coordinate_tolerance` attribute set directly via `add_attribute`
      (simulating what ExtData now does) large enough to cover the
      perturbation, result in the same cached `MaplGeom` via
      `get_mapl_geom_from_metadata`.
- [x] 6.3 In the same file, add
      `test_do_not_reuse_geom_outside_tolerance`: same setup but with a
      perturbation exceeding the declared tolerance (or no tolerance
      attribute set) results in two distinct `MaplGeom`s.
- [x] 6.4 Verify `test_do_not_reuse_geom` (differing im_world) still
      passes unchanged - grids with different point counts must never be
      merged regardless of tolerance.

## 7. Build and verification

- [x] 7.1 Build the `nag` tree (per `nag-builder`/`compiler-switching`
      skills) and confirm the geom, geom_io, and gridcomps/extdata targets
      compile with no new warnings from the modified files. `pfio` should
      require no changes/rebuild for this feature.
- [x] 7.2 Run the affected pFUnit suites from the `nag` build directory
      (`ctest` filtered to `Test_GeomManager`, `Test_LatLonGeomSpec`,
      `Test_LatLonGeomFactory`, and the `gridcomps/extdata` collection/
      config tests) and confirm all pass, including the new tolerance
      cases.
- [x] 7.3 Run the full `nag` `ctest` suite once targeted tests pass, to
      catch any unexpected regressions in `RouteHandleSpec`/`RegridderSpec`
      or `ExtData` tests that indirectly depend on geom equality.
      Result: 68/75 pass; the 7 failures (`ll-ll`, `cs-cs`, `cs-ll`,
      `ll-cs`, `MAPL3G_Comp_Test_case02/11/23`) are pre-existing
      environment gaps (missing `LOCAL_REGRESSION_DATA_DIR` and missing
      Python `netCDF4` package for dry-run verification), unrelated to
      this change - none reference geom/ExtData/coordinate_tolerance.
</content>
