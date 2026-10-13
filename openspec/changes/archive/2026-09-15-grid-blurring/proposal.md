## Why

MAPL's `GeomManager` currently matches file-based LatLon grids by exact
coordinate equality (`CoordinateAxis::equal_to`, bitwise `==` on center/corner
arrays). Two files that describe the "same" physical grid but differ by
tiny floating-point noise (e.g. float32 vs float64 round-trip, or
reprocessed/re-exported files) fail this exact match, so `GeomManager`
mints a brand-new `MaplGeom` for every such file. `ExtData` hits this on
every file swap in a rolling/time-varying file collection
(`PrimaryExport.F90` `complete_export_spec`/`update_export_spec`), causing
redundant `ESMF_Geom` construction and redundant `RouteHandle`/regridder
creation that could otherwise be reused. See GEOS-ESM/MAPL#5385.

## What Changes

- Add an optional coordinate-comparison tolerance to LatLon grid equality
  (`CoordinateAxis`, `LatAxis`/`LonAxis`, `LatLonGeomSpec::equal_to`), so
  two LatLon grids whose center/corner coordinates differ by no more than
  the tolerance are treated as the same grid for `GeomManager` caching
  and RouteHandle/regridder reuse purposes.
- Source the tolerance from a `coordinate_tolerance` global attribute on
  `FileMetadata`, read using `FileMetadata`'s existing generic
  `has_attribute`/`get_attribute` API - no new tolerance-specific method
  is added to `pfio`/`FileMetadata`/`FileMetadataUtilities`. The generic
  geom layer stays agnostic of who sets the attribute or why.
- The attribute is not expected to be present in the netCDF file itself.
  Instead, `ExtData` (`gridcomps/extdata`) is the sole producer of this
  attribute: it gains a new optional per-collection config field
  (`coordinate_tolerance` in the collection's YAML entry), and after
  loading a file's `FileMetadata`, `PrimaryExport` explicitly calls the
  existing generic `add_attribute` on that `FileMetadata` object to
  stamp the collection's effective tolerance onto it before handing it
  to `GeomManager`. This avoids any specialized logic in `pfio` or in
  shared `geom_io` infrastructure.
- The geom layer's own default (when `FileMetadata` has no
  `coordinate_tolerance` attribute at all) remains `0` (strict/bitwise)
  - that generic contract is unchanged and client-agnostic. `ExtData`,
  however, always stamps *some* tolerance: a collection that does not
  set `coordinate_tolerance` gets a nonzero default
  (`DEFAULT_COORDINATE_TOLERANCE`, 10% of a grid's own spacing) rather
  than no attribute at all, because MAPL2 treated slightly-differing
  file-based grids as the same grid by default and existing `ExtData`
  users depend on that behavior; a collection can set
  `coordinate_tolerance: 0` explicitly to opt into strict comparison -
  the override MAPL2 itself provided.
- Scope is LatLon grids only for this change; `Mesh`, `LocStream`, and
  `EASE` geom comparisons are untouched.
- Fix the accidental syntax error in
  `infrastructure/geom/LatLon/LatLonGeomSpec.F90` (missing newline between
  two `procedure ::` statements, already corrected on the working branch)
  as a prerequisite so the module compiles.

## Capabilities

### New Capabilities
- `geom/latlon-coordinate-tolerance`: LatLon grid coordinate comparison
  supports an optional per-grid tolerance (sourced from a generic
  `coordinate_tolerance` attribute on `FileMetadata`, whoever sets it)
  so near-identical file-based grids are treated as equal for caching
  and RouteHandle/regridder reuse.
- `extdata/file-collection-coordinate-tolerance`: ExtData file
  collections may declare an optional `coordinate_tolerance` in their
  config; ExtData stamps that value onto each file's `FileMetadata`
  before requesting a geom for it, so collections whose files have
  numerically noisy-but-identical grids can reuse geoms/RouteHandles.

### Modified Capabilities
(none — no existing spec files cover LatLon geom comparison behavior yet)

## Impact

- `infrastructure/geom/CoordinateAxis/equal_to.F90` — comparison logic
  gains tolerance support.
- `infrastructure/geom/LatLon/LatAxis.F90`, `LonAxis.F90` — thread
  tolerance through to `CoordinateAxis`.
- `infrastructure/geom/LatLon/LatLonGeomSpec.F90` and
  `LatLonGeomSpec/equal_to.F90` — carry/apply tolerance at the spec
  level; syntax fix at line 23-24.
- `infrastructure/geom/LatLon/LatLonGeomSpec/make_LatLonGeomSpec_from_metadata.F90`
  — read the `coordinate_tolerance` attribute directly off `FileMetadata`
  via its existing `has_attribute`/`get_attribute` methods and stash it
  on the resulting spec. No changes to `pfio/FileMetadata.F90` or
  `base/FileMetadataUtilities.F90`.
- `infrastructure/geom/GeomManager/get_mapl_geom_from_spec.F90` — no
  interface change; behavior change is internal to `equal_to`.
- `gridcomps/extdata/ExtDataCollection.F90` — new optional
  `coordinate_tolerance` config field, parsed from the collection's
  YAML entry, with a getter.
- `gridcomps/extdata/PrimaryExport.F90` — captures the collection's
  effective `coordinate_tolerance` (explicit or defaulted) at
  construction; before calling `geom_mgr%get_mapl_geom_from_metadata`,
  unconditionally calls
  `metadata%metadata%add_attribute("coordinate_tolerance", ...)` on the
  loaded `FileMetadata`.
- `infrastructure/geom_io/FieldBundleRead.F90` and other `GeomManager`
  consumers are unaffected - they continue to call the same
  `GeomManager`/`MAPL_SameGeom` APIs and only benefit from tolerant
  matching if they themselves stamp the attribute.
- Tests: `infrastructure/geom/tests/Test_GeomManager.pf`,
  `Test_LatLonGeomSpec.pf` gain tolerance-aware reuse/non-reuse cases;
  `gridcomps/extdata` tests gain config-parsing and attribute-stamping
  coverage.
</content>
