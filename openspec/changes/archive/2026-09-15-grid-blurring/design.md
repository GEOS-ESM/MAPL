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
- Let `LatLonGeomSpec::equal_to` treat a new (not-yet-registered) grid
  as equal to an already-registered one when corresponding coordinate
  values differ by no more than that new grid's own declared tolerance
  (a fraction of its own coordinate spacing/DX) - each grid decides for
  itself, using only its own tolerance and resolution, never the
  already-registered grid's.
- Derive that tolerance from a generic `coordinate_tolerance` attribute
  on `FileMetadata`, read via the existing generic attribute API, so no
  new arguments are threaded through `GeomManager`, `GeomFactory`, or
  `GeomSpecVector` interfaces, and no tolerance-specific code is added
  to `pfio` or shared `geom_io` infrastructure.
- Make ExtData the sole owner of deciding/producing the tolerance value:
  a per-collection config field, always stamped onto each file's
  `FileMetadata` by ExtData client code using the metadata's existing
  generic `add_attribute` call - not a new specialized API.
- At the generic geom layer, preserve exact-match behavior by default
  (tolerance = 0) when no `coordinate_tolerance` attribute is present at
  all - that layer has no opinion on defaults and stays neutral for any
  future client.
- At the `ExtData` layer specifically, default to a nonzero tolerance
  (not zero) when a collection does not configure
  `coordinate_tolerance`, because MAPL2 treated slightly-differing
  file-based grids as the same grid by default and existing `ExtData`
  users depend on that behavior; an explicit `coordinate_tolerance: 0`
  remains available to opt into strict comparison.

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
construction time from `FileMetadata`. `equal_to(a, b)` then derives the
effective threshold from `b` alone (see Decision 2 below) with no
signature change.

**Alternative considered:** thread `coordinate_tolerance` as an optional
argument through `get_mapl_geom_from_metadata` → `make_geom_spec` →
`LatLonGeomSpec` constructor → `equal_to`. Rejected per the issue
author's own follow-up: "would require lots of interface changes
throughout geom manager," and the `GeomSpec` base class's `equal_to` is
shared across all geom types, most of which have no tolerance concept.

### 2. Only the not-yet-registered ("candidate") grid's own tolerance and spacing matter - not a symmetric combination
`equal_to(a, b)` is invoked by gFTL's `find()` as
`T_EQ(container_element, lookup_value)`, i.e. `a` is always an
already-registered `GeomSpec`/`CoordinateAxis` in `GeomManager`'s cache,
and `b` is always the new candidate being looked up (see
`GeomSpecVector.F90:4`, `GeomManager/get_mapl_geom_from_spec.F90`). Each
grid decides for itself, using its own declared tolerance and its own
coordinate spacing, whether an existing registry entry is close enough
to reuse - the already-registered entry already made its own
accept/reject decision, using its own tolerance, at the time it was
itself inserted, so its tolerance is irrelevant to a later lookup
against it. Decision: `equal_to` uses **only `b`'s** tolerance (and
`b`'s own coordinate spacing - see Decision 4) and never consults `a`'s
tolerance at all. This makes `equal_to` intentionally directional:
`a == b` and `b == a` are generally not equivalent when `a` and `b`
declare different tolerances.

**Alternative considered (this project's original, incorrect,
decision):** combine both sides via `max(a%tolerance, b%tolerance)`,
on grounds that "if either side opts into blurring, the comparison
should be tolerant." Rejected on review: it lets an already-registered
grid's tolerance affect a lookup that grid has no part in, which
contradicts the "each grid decides for itself" model, and even by that
same (rejected) either-side-opts-in reasoning, a symmetric combination
should logically have been the more conservative `min` (both sides must
agree the values are close enough), not `max` (which lets the looser of
the two override the other's implicit request for strict comparison).
Both flaws are moot once the rule is correctly understood to be
one-sided rather than a combination at all.

**Alternative considered:** always use tolerance from `a` (the
already-cached/first-seen grid). Rejected because it puts the decision
in the hands of whichever grid happened to be inserted first, rather
than the grid that is actually asking "is this existing entry close
enough for me" - exactly backwards from the intended per-grid,
self-determined semantics.

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

### 4. Comparison implementation in `CoordinateAxis::equal_to`, with tolerance scaled by the candidate's own grid spacing (DX)
Per the original issue's own framing - "take the average spacing DX in
[the relevant] grid, and set the tolerance to be say 1% of that" - the
declared `coordinate_tolerance` is a dimensionless **fraction of grid
spacing**, not an absolute coordinate difference. Per Decision 2, "the
relevant grid" is `b` (the candidate), not `a`. Replace the exact
elementwise `all(a%centers == b%centers)` / `all(a%corners ==
b%corners)` checks with `all(abs(a%centers - b%centers) <= abs_tol)` /
`all(abs(a%corners - b%corners) <= abs_tol)`, guarded by the existing
`size(...)` equality short-circuits (unchanged - size mismatch is never
"tolerant"), where:
```
abs_tol = b%tolerance * min_spacing(b%centers)
min_spacing(centers) = minval(abs(centers(2:) - centers(:size-1)))   ! 0 if size(centers) < 2
```
`b%tolerance` (the fraction) is populated at construction time from
`FileMetadata` (Decision 3), and `min_spacing` is computed directly from
`b`'s own `centers` array - no min/max combination with `a` at all,
consistent with Decision 2's one-sided rule. A degenerate single-point
axis has no defined spacing and yields `abs_tol = 0` (strict) for that
axis regardless of the declared fraction.

### 5. ExtData owns producing the attribute: per-collection config field, stamped unconditionally by client code
`ExtDataCollection` (`gridcomps/extdata/ExtDataCollection.F90`) gains a
`real(kind=ESMF_KIND_R8) :: coordinate_tolerance` field (always has an
effective value - see Decision 6 for its default), parsed in
`new_ExtDataCollection` the same way as `valid_range`
(`ESMF_HConfigIsDefined`/`ESMF_HConfigAsR8` on a `"coordinate_tolerance"`
YAML key), with a `get_coordinate_tolerance` accessor.

`PrimaryExport` is constructed with a pointer to its `ExtDataCollection`
already; it additionally captures the collection's effective tolerance
into its own field at construction time, the same way it captures
`client_collection_id`. Then, at each of the two places it loads file
metadata and is about to request a geom, it unconditionally calls the
existing generic
`metadata%metadata%add_attribute("coordinate_tolerance", this%coordinate_tolerance, _RC)`
immediately before `geom_mgr%get_mapl_geom_from_metadata` - there is no
longer an "is a tolerance configured" branch, because every collection
now has an effective tolerance (explicit or defaulted). No other module
(not `DataCollection.F90`, not `DataSetNode.F90`) is touched - the stamp
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

### 6. ExtData defaults `coordinate_tolerance` to a nonzero value (0.1); the geom layer's own default stays 0
The generic geom layer's default - when `FileMetadata` carries no
`coordinate_tolerance` attribute at all - remains `0` (strict/bitwise).
That default is correct and unchanged: the geom layer is client-agnostic
and must not assume any particular policy.

`ExtData` is a specific client with a specific historical contract,
though: per the issue itself, "MAPL2 allows 2 file-based grids that
differ slightly in their coordinates to be treated as the same grid...
An override is given if someone wants to insist that the file grid be
respected." I.e. MAPL2's default was tolerant, not strict, and the
strict behavior was the opt-in override. If `ExtData`'s new
`coordinate_tolerance` config field simply left the tolerance at 0 when
unset, every existing `ExtData` collection config (which never
mentions this brand-new key) would silently switch from MAPL2's
historical tolerant matching to strict matching - the opposite of a
compatible default, and precisely the regression this project must not
introduce for existing users.

Decision: `ExtDataCollection` defines
`DEFAULT_COORDINATE_TOLERANCE = 0.1` (10% of a grid's own coordinate
spacing/DX - matching the issue's own `DEFAULT_TOLERANCE = 0.1`
pseudocode) and uses it whenever a collection's config omits
`coordinate_tolerance`. A collection can set `coordinate_tolerance: 0`
explicitly to opt into strict comparison - preserving MAPL2's own
override mechanism, just expressed through the new config key instead
of a separate flag. Because `ExtDataCollection%coordinate_tolerance` now
always has an effective value, the previous `allocatable` field and its
`is_coordinate_tolerance_allocated` accessor are unnecessary and were
removed; `PrimaryExport` now unconditionally stamps the attribute
(Decision 5) rather than conditionally doing so.

**Alternative considered:** default to `0` (strict) at the `ExtData`
layer, matching the geom layer's own generic default, on grounds of
"least surprise" for a brand-new capability. Rejected because it
inverts MAPL2's actual historical default and would change existing
users' grid-reuse (and therefore RouteHandle-reuse and performance)
behavior the moment they upgrade, without them touching their config at
all - a worse practical outcome than an imperfect default value.

**Alternative considered:** derive the default automatically from
`min_dx` per grid rather than a fixed fraction (matching one reading of
the issue's original pseudocode, `coordinate_tolerance_ = 0.1;
... coordinate_tolerance_ * min_dx`). This is, in fact, exactly what
happens: `0.1` is the fraction, and `CoordinateAxis::equal_to` (Decision
4) already multiplies it by the candidate's own `min_dx` at comparison
time. There is no separate "automatic" derivation to reject here - the
scaling-by-DX and the default-fraction-value are two different, both
necessary, pieces of the same mechanism.

## Risks / Trade-offs

- **[Risk]** A large `coordinate_tolerance` (explicit or the default
  0.1) configured on an ExtData collection could silently merge grids
  that a user intended to keep distinct (e.g. two physically different
  but coarsely similar grids served by the same collection over time).
  → **Mitigation**: existing decomposition and point-count checks still
  apply before any coordinate comparison; the value is a *fraction* of
  the grid's own spacing, so it scales with resolution rather than
  being a fixed, potentially-too-loose absolute value; a collection can
  set `coordinate_tolerance: 0` to disable tolerant matching entirely.
- **[Risk]** Defaulting `ExtData`'s `coordinate_tolerance` to a nonzero
  value (rather than 0) means every existing collection, without any
  config change, now compares its file-based grids tolerantly rather
  than bit-exactly - a real, deliberate change in observable behavior
  for a codebase-wide default, not merely an "opt-in, no-op unless
  configured" feature. → **Mitigation**: this default is intentionally
  chosen to *restore* MAPL2's own historical default behavior (which
  this change is otherwise re-implementing from scratch, per the
  originating issue) rather than to introduce new behavior; it is not a
  regression relative to what users of pre-MAPL3 GEOS actually
  experienced. Collections that genuinely need bit-exact matching can
  set `coordinate_tolerance: 0` explicitly.
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
- **[Risk]** Because comparison is directional (only the candidate's
  tolerance/spacing govern a lookup), `a == b` and `b == a` are
  generally not equivalent, which could surprise a caller used to
  ordinary symmetric equality. → **Mitigation**: document the
  directional contract clearly on `CoordinateAxis`'s `tolerance` field
  and in `equal_to`'s own comments, and in the requirement scenarios;
  the asymmetry is intentional and matches "each grid decides for
  itself" - it is exercised directly by
  `test_equal_to_is_directional_not_symmetric` in both
  `Test_CoordinateAxis.pf` and `Test_LatLonGeomSpec.pf`.
- **[Risk]** Expressing `coordinate_tolerance` as a fraction of the
  candidate's own grid spacing (rather than an absolute coordinate
  difference) means the same numeric config value (e.g. `0.01`) implies
  very different absolute tolerances on a coarse grid versus a fine
  one. → **Mitigation**: this scaling is deliberate (per the original
  issue's own "1% of DX" framing) and keeps the same fractional config
  value meaningful across grids of different resolution; document the
  fraction-of-DX semantics prominently wherever `coordinate_tolerance`
  is read or configured.
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
  collection configs need no changes to keep their historical (MAPL2)
  default-tolerant grid-reuse behavior - `ExtData` applies a nonzero
  default tolerance automatically. A collection can add
  `coordinate_tolerance: 0` if it wants to newly opt into strict
  comparison.
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
