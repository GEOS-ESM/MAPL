## Context

See `proposal.md` - Why. Relevant existing code (branch
`feature/#5385-grid-blurring`, off `develop`):

- `GeomSpecVector::find` (`infrastructure/geom/GeomManager/get_mapl_geom_from_spec.F90`)
  linearly scans cached `GeomSpec`s using `operator(==)`, which dispatches
  to each concrete spec's deferred `equal_to`.
- `LatLonGeomSpec::equal_to` (`infrastructure/geom/LatLon/LatLonGeomSpec/equal_to.F90`)
  compares `lon_axis`, `lat_axis`, then `decomposition`, short-circuiting
  on the first mismatch.
- `LatAxis`/`LonAxis::equal_to` forward to `CoordinateAxis::equal_to`
  (`infrastructure/geom/CoordinateAxis/equal_to.F90`), which does exact
  `size` and elementwise `==` checks on `centers`/`corners` real(R8)
  arrays.
- `LatLonGeomSpec` is built from a file via
  `make_LatLonGeomSpec_from_metadata` (`infrastructure/geom/LatLon/LatLonGeomSpec/make_LatLonGeomSpec_from_metadata.F90`),
  which receives a `pfio::FileMetadata` and constructs `lon_axis`/`lat_axis`/
  `decomposition` from it.
- `FileMetadata` (`pfio/FileMetadata.F90`) already exposes a generic
  global-attribute mechanism (`has_attribute`/`get_attribute`/
  `add_attribute` with scalar `add_attribute_0d`, `FileMetadata.F90:43-45,192-203`)
  that can carry an arbitrary named attribute without a new stored
  field or new pfio-level method.
- `ExtDataCollection` (`gridcomps/extdata/ExtDataCollection.F90:10-24`)
  is the Fortran type backing one entry of ExtData's `Collections:` YAML
  map; existing optional fields (`freq`, `ref_time`, `valid_range`) are
  parsed in `new_ExtDataCollection` (`ExtDataCollection.F90:31-149`)
  using `ESMF_HConfigIsDefined`/`ESMF_HConfigAsR4`-style calls, all
  snake_case keys.
- `PrimaryExport` (`gridcomps/extdata/PrimaryExport.F90`) is constructed
  with a pointer to its `ExtDataCollection` (`new_PrimaryExport`,
  `PrimaryExport.F90:57-61`) but today only extracts template/frequency/
  valid_range into its `file_selector`; it does not retain the
  collection itself. It later loads file metadata via
  `this%file_selector%get_dataset_metadata` and passes it straight to
  `geom_mgr%get_mapl_geom_from_metadata` (`PrimaryExport.F90:152-154`,
  duplicated at `:211-213`).
- The syntax break at `infrastructure/geom/LatLon/LatLonGeomSpec.F90:23`
  (two `procedure ::` statements on one line) has already been corrected
  on the working branch; verified as part of this planning pass.

## Goals / Non-Goals

**Goals:**
- Let `LatLonGeomSpec::equal_to` treat two grids as equal when
  corresponding coordinate values differ by no more than a tolerance.
- Derive that tolerance from a generic `coordinate_tolerance` attribute
  on `FileMetadata`, read via the existing generic attribute API, so no
  new arguments are threaded through `GeomManager`, `GeomFactory`, or
  `GeomSpecVector` interfaces, and no tolerance-specific code is added
  to `pfio` or shared `geom_io` infrastructure.
- Make ExtData the sole owner of deciding/producing the tolerance value:
  a new optional per-collection config field, explicitly stamped onto
  each file's `FileMetadata` by ExtData client code using the metadata's
  existing generic `add_attribute` call - not a new specialized API.
- Preserve exact-match behavior by default (tolerance = 0 / attribute
  absent) when a collection does not opt in.

**Non-Goals:**
- Extending tolerant comparison to Mesh, LocStream, or EASE geoms (see
  proposal.md - explicitly out of scope).
- Adding an explicit tolerance argument to any public
  `GeomManager`/`GeomFactory` API (the issue's own follow-up comments
  moved away from this toward metadata-driven tolerance).
- Adding any tolerance-aware method to `pfio/FileMetadata.F90`,
  `base/FileMetadataUtilities.F90`, or `infrastructure/geom_io/DataCollection.F90`
  - those layers only ever see a generically-named attribute they don't
  interpret.
- Changing how `GeomManager` caches/keys geoms structurally (e.g. no new
  index or hashing scheme) - only the equality predicate changes.
- Reworking `RouteHandleSpec`/`RegridderSpec` (`infrastructure/regridder_mgr/`);
  they key off `ESMF_Geom`/`MaplGeom` ids assigned by `GeomManager`, which
  is unaffected once two specs compare equal and share one `MaplGeom`.
- Deriving a tolerance automatically from the netCDF file's own
  attributes or grid spacing - the value always comes from ExtData's
  collection config, set explicitly by the user/collection author.

## Decisions

### 1. Tolerance travels on `LatLonGeomSpec`/`CoordinateAxis`, not as a comparison argument
`equal_to` is a deferred, parameterless (besides `a`/`b`) type-bound
function required by the `GeomSpec` abstract interface and invoked
generically through `operator(==)` from `GeomSpecVector`'s gFTL `T_EQ`
macro. Adding a tolerance argument there would require changing the
abstract interface, the gFTL container instantiation, and every other
geom type's `equal_to`, even though only LatLon needs it.

Instead, each `CoordinateAxis` (and therefore `LatAxis`/`LonAxis` and
`LatLonGeomSpec`) stores its own tolerance value, populated once at
construction time from `FileMetadata`. `equal_to(a, b)` then uses
`max(a%tolerance, b%tolerance)` (or another agreed combination rule -
see below) with no signature change.

**Alternative considered:** thread `coordinate_tolerance` as an optional
argument through `get_mapl_geom_from_metadata` → `make_geom_spec` →
`LatLonGeomSpec` constructor → `equal_to`. Rejected per the issue
author's own follow-up: "would require lots of interface changes
throughout geom manager," and the `GeomSpec` base class's `equal_to` is
shared across all geom types, most of which have no tolerance concept.

### 2. Tolerance combination rule when comparing two specs with different stored tolerances
Two `LatLonGeomSpec` instances being compared may have been built from
different files with different declared tolerances (e.g. the
already-cached grid was built strictly, tolerance=0, and the new file
declares tolerance=0.01). Decision: use `max(a%tolerance, b%tolerance)`
so that if either side opts into blurring, the comparison is tolerant.
This favors reuse (the issue's stated goal) and matches the "average
spacing" framing in the issue body, where the tolerance is a property of
one grid's own resolution, not an interaction between two files.

**Alternative considered:** always use tolerance from `a` (the
already-cached/first-seen grid) — simpler but order-dependent and
surprising (result of `a==b` could differ from `b==a`), so rejected in
favor of the symmetric `max` rule.

### 3. Read `coordinate_tolerance` directly off `FileMetadata` via its existing generic-attribute API - no new pfio surface
No new stored field or method is added to `FileMetadata` or
`FileMetadataUtilities`. `make_LatLonGeomSpec_from_metadata` calls
`file_metadata%has_attribute("coordinate_tolerance")` /
`get_attribute(...)` (existing methods, `pfio/FileMetadata.F90:43-48`)
inline, directly in the geom layer. If absent, tolerance defaults to
`0.0` (strict). The geom layer treats `"coordinate_tolerance"` as just a
string key it happens to look for; it has no opinion on who sets it,
which keeps the pfio/`FileMetadata` layer, and shared
`infrastructure/geom_io` infrastructure, fully generic.

**Alternative considered (rejected by user):** add a dedicated
`get_coordinate_tolerance(file_metadata)` accessor in
`pfio/FileMetadata.F90` or `base/FileMetadataUtilities.F90`. Rejected
because `coordinate_tolerance` is an ExtData-specific concept and
`pfio`/`FileMetadataUtilities` should not carry specialized, single-client
logic - the generic `has_attribute`/`get_attribute` calls are simple
enough to inline at the one call site that needs them.

**Alternative considered (rejected by issue author):** compute a default
nonzero tolerance automatically (issue body's original
`DEFAULT_TOLERANCE = 0.1 * min_dx` proposal). Rejected per the issue
author's own follow-up comments, which settled on an explicitly-supplied
tolerance with an implicit default of zero when absent, to keep behavior
conservative and backward compatible.

### 4. Comparison implementation in `CoordinateAxis::equal_to`
Replace the exact elementwise `all(a%centers == b%centers)` /
`all(a%corners == b%corners)` checks with
`all(abs(a%centers - b%centers) <= tol)` /
`all(abs(a%corners - b%corners) <= tol)`, guarded by the existing
`size(...)` equality short-circuits (unchanged - size mismatch is never
"tolerant"). `tol` is the combined value from Decision 2, passed down
from `LatLonGeomSpec::equal_to` through `LatAxis`/`LonAxis::equal_to`
into `CoordinateAxis::equal_to`.

### 5. ExtData owns producing the attribute: new per-collection config field, stamped explicitly by client code
`ExtDataCollection` (`gridcomps/extdata/ExtDataCollection.F90:10-24`)
gains an optional `real, allocatable :: coordinate_tolerance` field,
parsed in `new_ExtDataCollection` the same way as `valid_range`
(`ESMF_HConfigIsDefined`/`ESMF_HConfigAsR4` on a new `"coordinate_tolerance"`
YAML key), with a `get_coordinate_tolerance`/`is_coordinate_tolerance_allocated`
accessor pair mirroring the existing `valid_range` accessors
(`ExtDataCollection.F90:189-212`).

`PrimaryExport` is constructed with a pointer to its `ExtDataCollection`
(`PrimaryExport.F90:57-61`) already; it additionally captures the
collection's configured tolerance (if any) into its own field at
construction time, the same way it captures `client_collection_id`.
Then, at each of the two places it loads file metadata and is about to
request a geom (`PrimaryExport.F90:152-154` and `:211-213`), it calls the
existing generic
`metadata%metadata%add_attribute("coordinate_tolerance", this%coordinate_tolerance, _RC)`
when a tolerance was configured, immediately before
`geom_mgr%get_mapl_geom_from_metadata`. No other module (not
`DataCollection.F90`, not `DataSetNode.F90`) is touched - the stamp
happens in ExtData's own gridcomp code, on the `FileMetadata` object
ExtData is about to hand to `GeomManager`, matching the requirement that
only `gridcomps/extdata` client code adds this attribute.

**Alternative considered (rejected by user):** stamp the attribute
inside `infrastructure/geom_io/DataCollection.F90` at the point a file's
`FileMetadata` is first read from disk (`DataCollection.F90:85-91`),
since that is the single shared ingestion point used by all `GeomManager`
consumers. Rejected because `DataCollection` is generic shared
infrastructure (also used outside ExtData), and threading an
ExtData-specific config value into it would leak ExtData-specific
concerns into shared code - exactly what the user asked to avoid.

## Risks / Trade-offs

- **[Risk]** A large `coordinate_tolerance` configured on an ExtData
  collection could silently merge grids that a user intended to keep
  distinct (e.g. two physically different but coarsely similar grids
  served by the same collection over time). → **Mitigation**: default
  is strict (0/absent); tolerance is opt-in per collection, set
  explicitly by whoever authors that collection's config; existing
  decomposition and point-count checks still apply before any
  coordinate comparison.
- **[Risk]** Because the attribute is stamped by ExtData at metadata-load
  time rather than being intrinsic to the file, two different
  `ExtDataCollection`s pointing at physically the same file could
  configure different tolerances, giving inconsistent geom-reuse
  behavior depending on which collection loaded it first (whichever
  `FileMetadata` instance/cache entry is used). → **Mitigation**: this
  is a per-collection, per-load concern by design (the value describes
  how *that collection* wants to treat its files, not a property of the
  file itself); document this scoping clearly; out of scope to dedupe
  across collections in this change.
- **[Risk]** `max(a%tolerance, b%tolerance)` combination means a single
  loosely-tolerant file can cause it to match an already-cached strict
  grid, which may be surprising to a user who only intended the
  tolerance to apply to comparisons among files sharing that same
  tolerance. → **Mitigation**: document the symmetric `max` rule
  clearly in code comments and in the requirement scenarios; revisit if
  real-world usage shows this is too permissive.
- **[Risk]** Introducing a `tolerance` field on `CoordinateAxis`/`LatAxis`/
  `LonAxis`/`LatLonGeomSpec` changes their size/derived-type layout,
  which could affect any existing serialization or equality-based tests
  that construct these types directly (e.g. `Test_LatLonGeomSpec.pf`,
  `Test_GeomManager.pf`, hconfig-based construction path). →
  **Mitigation**: give the new field a default value of `0.0` so
  hconfig-based construction (which has no file metadata) is unaffected
  and existing tests continue to pass unchanged; add new tests
  specifically for the metadata + tolerance path.

## Migration Plan

- No data migration. This is an internal comparison-semantics change;
  on-disk file formats and MAPL public APIs (`GeomManager`, `GeomSpec`)
  are unaffected. The only new user-facing surface is the optional
  `coordinate_tolerance` key in ExtData collection YAML; existing
  collection configs need no changes and get identical (strict) behavior.
- Rollout: land the fix for the pre-existing syntax break first (already
  done on the branch, verified in this planning pass), then land the
  tolerance-aware `equal_to` changes (a no-op until something stamps the
  attribute), then land the ExtData config field and stamping code.
- Rollback: reverting the `equal_to`/`make_LatLonGeomSpec_from_metadata`
  changes restores exact-match behavior; reverting the `ExtDataCollection`/
  `PrimaryExport` changes independently removes the only producer of the
  attribute, also restoring strict behavior. No persistent state to
  unwind either way.
</content>
