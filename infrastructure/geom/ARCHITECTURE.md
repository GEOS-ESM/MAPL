# MAPL geometry subsystem (`infrastructure/geom`)

**Audience:** coding agents and developers working on `infrastructure/geom` in MAPL.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [`../ARCHITECTURE.md`](../ARCHITECTURE.md),
[`../../docs/ARCHITECTURE.md`](../../docs/ARCHITECTURE.md),
[`../../AGENTS.md`](../../AGENTS.md).
Related: [`Mesh/README.md`](Mesh/README.md) (Mesh design notes, not
re-verified here) and the specification
[`../../openspec/specs/geom/latlon-coordinate-tolerance/spec.md`](../../openspec/specs/geom/latlon-coordinate-tolerance/spec.md)
(the only specification under `openspec/specs/geom` on this branch).

## 1. Purpose

`MAPL.geom` turns a description of a horizontal geometry (a configuration
node, or the metadata of a file) into a `GeomSpec`, builds the
`ESMF_Geom` from it through a per-kind factory, and caches the result as a
`MaplGeom` inside a process-wide `GeomManager`, so that two requests for an
equal geometry return the same object.

## 2. Directory and module map

All sources belong to one library target, `MAPL.geom`; subdirectories add
sources with `target_sources(MAPL.geom PRIVATE ...)` and
`esma_add_fortran_submodules` (`geom/CMakeLists.txt` and each subdirectory's
`CMakeLists.txt`). Many routines live in Fortran submodule files named after
the routine (for example `GeomManager/get_mapl_geom_from_spec.F90`).

| Path | Module(s) | Role |
|---|---|---|
| `GeomSpec.F90` | `mapl_GeomSpec_mod` | Abstract `GeomSpec`: deferred `equal_to`, `get_horz_ij_index_r4/r8`; optional name (`set_name`, `get_name`, `has_name`); `operator(==)` |
| `NullGeomSpec.F90` | `mapl_NullGeomSpec_mod` | `NULL_GEOM_SPEC`, placeholder spec |
| `GeomFactory.F90` | `mapl_GeomFactory_mod` | Abstract `GeomFactory` (see section 4) |
| `GeomId.F90` | `mapl_GeomId_mod` | `GeomId` (integer, `is_assigned` when positive), `GeomIdManager` counter, `get_geom_id_manager` |
| `GeomUtilities.F90` | `mapl_GeomUtilities_mod` | `GeomSetId`, `GeomGetId`, `SameGeom`; the id is stored in the `ESMF_Geom` info under key `mapl/geom/id` |
| `MaplGeom.F90`, `MaplGeom/` | `mapl_MaplGeom_mod` | Cached record: spec, `ESMF_Geom`, factory, id, file metadata, gridded dimensions, variable attributes, vector bases (`get_basis`) |
| `GeomManager.F90`, `GeomManager/` | `mapl_GeomManager_mod` | Singleton registry and cache (section 5) |
| `GeomFactoryVector.F90`, `GeomSpecVector.F90`, `IntegerMaplGeomMap.F90` | gFTL containers used by the manager | |
| `CoordinateAxis.F90`, `CoordinateAxis/` | `mapl_CoordinateAxis_mod` | `CoordinateAxis` (centers, corners, extent, tolerance, periodicity), `get_dim_name`, `get_coordinates`, `AxisRanges` |
| `LatLon/` | `LonAxis`, `LatAxis`, `LatLonDecomposition`, `LatLonGeomSpec`, `LatLonGeomFactory` | Regular latitude-longitude grids |
| `CubedSphere/` | `CubedSphereGeomSpec`, `CubedSphereGeomFactory`, `CubedSphereDecomposition` | Cubed-sphere grids, including Schmidt stretching parameters |
| `XY/` | `XYGeomSpec`, `XYGeomFactory` | Grids read from a grid file with a projection (`grid_file_name`) |
| `EASE/` | `EASEGeomSpec`, `EASEGeomFactory`, `EASECoords`, `EASEConversion`, `EASEDecomposition` | EASE (Equal-Area Scalable Earth) grids selected by `grid_name` |
| `LocStream/` | `LocStreamGeomSpec`, `LocStreamGeomFactory`, `LocStreamDecomposition` | Unstructured point sets (`ESMF_LocStream`) |
| `Mesh/` | `MeshGeomSpec`, `MeshGeomFactory`, `MeshDecomposition`; `raster_to_mesh/` | Unstructured meshes (`ESMF_Mesh`); `raster_to_mesh` is added only when `BUILD_WITH_FARGPARSE` |
| `GridGet.F90`, `GridGet/` | `mapl_GridAccessors_mod` | `GridGet`, `GridGetCoordinates`, `grid_get_interior`, `grid_get_corners`, `grid_has_DE` |
| `GeomGet.F90` | `mapl_GeomAccessors_mod` | `GeomGet`, `GeomGetHorzIJIndex`, `GridGetHorzIJIndex` |
| `GridGetGlobal.F90` | `mapl_GridGetGlobal_mod` | `GridGetGlobalCellCountPerDim` |
| `GridComms.F90` | `mapl_GridComms_mod` | `mapl_CollectiveGather3D`, `mapl_CollectiveScatter3D` |
| `Subgrid.F90` | `mapl_Subgrid_mod` | `Interval`, `make_subgrids`, `find_bounds` |
| `VectorBasis.F90`, `VectorBasis/` | `mapl_VectorBasis_mod` | `VectorBasis`, `NS_VectorBasis`, `GridVectorBasis`, `GridGetCoords`; coordinate conversions (`latlon2xyz`, `xyz2latlon`) |
| `API.F90` | `mapl_geom_api` | Umbrella re-export (section 3) |

## 3. Public interfaces

`mapl_geom_api` (`geom/API.F90`) re-exports, with a `mapl_` or `MAPL_`
prefix:
- Types: `MaplGeom`, `GeomSpec`, `GeomId`, `GeomIdManager`, `GeomManager`,
  `Interval`, `CubedSphereGeomSpec`, `CubedSphereDecomposition`.
- Functions: `get_geom_manager`, `get_mapl_geom`, `get_geom_id_manager`,
  `SameGeom`, `GeomGetId`, `GeomGet`, `GeomGetHorzIJIndex`, `GridGet`,
  `GridGetCoordinates`, `grid_has_DE` (as `mapl_GridHasDE`),
  `GridGetGlobalCellCountPerDim`, `make_subgrids`, `find_bounds`,
  `make_CubedSphereGeomSpec`, `make_CubedSphereDecomposition`,
  `MAPL_CollectiveGather3D`, `MAPL_CollectiveScatter3D`.
- Not in the umbrella (use the module directly): the other geometry kinds
  (`LatLonGeomSpec`, `XYGeomSpec`, `EASEGeomSpec`, `LocStreamGeomSpec`,
  `MeshGeomSpec` and their factories), `CoordinateAxis`, `VectorBasis`.

Typical use by other layers:
- `superstructure/generic/ComponentSpecParser/parse_geometry_spec.F90` calls
  `geom_mgr%make_geom_spec` on the `esmf_geom` section of a component's
  configuration (the `geometry` section).
- `superstructure/generic/OuterMetaComponent/initialize_geom_a.F90` calls
  `geom_mgr%get_mapl_geom(spec)` and sets the geom on the component.
- `infrastructure/regridder_mgr` and `infrastructure/geom_io` use
  `get_mapl_geom` and `GeomGetId`; `superstructure/state/StateGetGeom.F90`,
  `gridcomps/extdata/PrimaryExport.F90` and
  `gridcomps/history/HistoryCollectionGridComp_private.F90` also use the
  manager.
- `GridGetHorzIJIndex` in `GeomGet.F90` is annotated "deprecated stub: always
  fails" in its public statement.

## 4. Specifications and factories

A `GeomSpec` holds only the parameters that define a geometry and can be
compared with `==` (`GeomSpec.F90`, `equal_to`). A `GeomFactory`
(`GeomFactory.F90`) has deferred procedures:

| Procedure | Purpose |
|---|---|
| `make_spec` (`make_geom_spec_from_hconfig`, `make_geom_spec_from_metadata`) | Build a spec from an `ESMF_HConfig` node or from pFIO `FileMetadata` |
| `supports` (`supports_spec`, `supports_hconfig`, `supports_metadata`) | Whether this factory handles the spec, node or metadata |
| `make_geom` | Build the `ESMF_Geom` from a spec |
| `make_file_metadata` | Metadata (dimensions, coordinate variables) to write the geometry to a file |
| `make_gridded_dims` | Names of the horizontal dimensions |
| `make_variable_attributes` | Attributes for the geometry's coordinate variables |

Kinds, with the configuration key that selects them (`class`, in each
`supports_hconfig`):

| Kind | `class` value | Further keys checked or read |
|---|---|---|
| LatLon | `latlon` | longitude and latitude axis keys (`LonAxis/supports_hconfig.F90`, `LatAxis/supports_hconfig.F90`; for example `lon_range`, `im_world`, `dateline`) |
| CubedSphere | `CubedSphere` (exact string) | `im_world`; optional `stretch_factor`, `target_lon`, `target_lat`, `ims`/`jms`, `nx_face`/`ny_face` (`CubedSphereGeomSpec_smod.F90`) |
| XY | `xy` | `grid_file_name`; optional `n_peri_dim`, `LM`, `coord_class`, `index_name_x/y`, `var_name_x/y/proj`, `att_name_proj` (`XYGeomSpec/make_XYGeomSpec_from_hconfig.F90`) |
| EASE | `ease` | `grid_name` required (`EASEGeomSpec/supports_hconfig.F90`) |
| LocStream | `locstream`, case-insensitive | either `lon` and `lat`, or `file`; both is an error (`LocStreamGeomFactory.F90`, `supports_hconfig`) |
| Mesh | `mesh`, case-insensitive | `file` required (`MeshGeomSpec_smod.F90`, `supports_hconfig_`) |

Metadata detection (`supports_metadata`): LatLon requires longitude and
latitude axes and distinct longitude/latitude dimensions identified by units
`degrees_east` and `degrees_north` (`LatLon/LatLonGeomSpec/supports_metadata.F90`);
Mesh requires `nodeCount`, `elementCount` and connectivity, which the source
comments say distinguishes it from LocStream (`Mesh/MeshGeomSpec_smod.F90`).

LatLon specs compare coordinates with a tolerance; the behaviour is specified
in the OpenSpec specification linked above.

## 5. Creation from configuration and GeomManager caching

`GeomManager` (`GeomManager.F90`) holds a vector of factories, a vector of
specs and ids, a map from id to `MaplGeom`, and a `GeomIdManager`.
`get_geom_manager` (`GeomManager/get_geom_manager.F90`) returns the module
singleton `geom_manager`, calling `initialize` on first use.

Sequence for `get_mapl_geom(hconfig)` (`GeomManager/get_mapl_geom_from_hconfig.F90`):
1. `make_geom_spec(hconfig)` (`make_geom_spec_from_hconfig.F90`): the first
   registered factory whose `supports(hconfig)` is true builds the spec;
   otherwise the error "No factory supports hconfig."
2. `get_mapl_geom(spec)` (`get_mapl_geom_from_spec.F90`): searches the spec
   vector for a spec equal to the request (`==`). If found, returns the cached
   `MaplGeom` (a pointer).
3. Otherwise `add_mapl_geom` (`add_mapl_geom.F90`): `make_mapl_geom`
   (`make_mapl_geom_from_spec.F90`) picks the first factory that supports the
   spec, calls its `make_geom`, `make_file_metadata`, `make_gridded_dims` and
   `make_variable_attributes`, and builds a `MaplGeom`. A new id comes from
   the `GeomIdManager` (counter from 1; assertion "Too many geoms created." if
   above `MAX_ID` = 10000), is written into the `ESMF_Geom` info
   (`MaplGeom/set_id.F90`, `GeomSetId`), and spec, id and geom are stored.

Other entry points: `get_mapl_geom(metadata)` follows the same path with
file metadata; `get_mapl_geom(id)` returns by id; the free function
`get_mapl_geom(esmf_geom)` reads the id from the `ESMF_Geom` info
(`GeomGetId`) and looks it up. `delete_mapl_geom(spec)` erases the cached
`MaplGeom` of that spec.

Registered factories: `GeomManager/initialize.F90` adds, in this order,
`CubedSphereGeomFactory`, `LatLonGeomFactory`, `XYGeomFactory`,
`EASEGeomFactory`. The singleton path uses `initialize`; the separate
constructor `new_GeomManager` (`GeomManager/new_GeomManager.F90`) registers
`LatLon`, `CubedSphere`, `LocStream` and `XY` instead. `new_GeomManager` is
the structure constructor (`interface GeomManager`), so `GeomManager()`
creates a local manager separate from the singleton; the lists are not
exclusive when a caller also calls `initialize`:
`Mesh/raster_to_mesh/mesh_benchmark.F90` calls `mapl_GeomManager()` and then
`geom_mgr%initialize()`, and `regridder_mgr/tests/Test_RegridderManager.pf`
and `Test_RouteHandleManager.pf` also use the constructor.

Geometry for a component comes from its configuration:
`parse_geometry_spec` reads the component's `geometry` section (`kind`,
`esmf_geom`, `vertical_grid`), and `initialize_geom_a` resolves it in the
generic layer (see the generic-layer documents in `docs/`).

## 6. Tests

`geom/tests/CMakeLists.txt` defines one pFUnit executable, `MAPL.geom.tests`,
(`EXCLUDE_FROM_ALL`, up to 8 processes, `LABELS "ESSENTIAL"`). Test files
(`geom/tests/`): `Test_GeomManager`, `Test_CoordinateAxis`, `Test_LonAxis`,
`Test_LatAxis`, `Test_LatLon{Decomposition,GeomSpec,GeomFactory,ZeroDE,Distribution}`
(`Test_LatLonDistribution.pf` exists in the directory but is not in the
`TEST_SRCS` list), `Test_CubedSphereGeom{Spec,Factory}`, `Test_GridGet`,
`Test_HorzIJIndex`, `Test_LocStream*`, `Test_Mesh*`, `Test_XYGeom*`,
`Test_EASEGeom*`, with helpers `LatLonTestHelper.F90`, `MeshTestHelper.F90`,
`XYTestHelper.F90`.

## 7. Known traps

- A new geometry kind must be added in two places to be used: its factory
  class, and a registration line in `GeomManager/initialize.F90` (the
  singleton) — `LocStream` and `Mesh` factories are not registered there.
- Factory order matters: the first factory that `supports` the input wins.
- `get_mapl_geom` returns a pointer into the manager's map; do not
  deallocate it.
- `CubedSphere` is matched case-sensitively; `locstream` and `mesh` are
  matched case-insensitively; `latlon`, `xy`, `ease` are compared as written.
- `GeomManager` is process-global; ids are positive integers from a single
  counter and are never reused in the code read.
- `GridGetHorzIJIndex` always fails; use `GeomGetHorzIJIndex`.

## 8. Observations (noted, not fixed)

- `initialize` (used by the singleton; also callable on a constructed
  manager) and `new_GeomManager` register different factory sets; only `new_GeomManager` includes LocStream; no
  routine in the tree registers `MeshGeomFactory` with the manager.
- `delete_mapl_geom` erases the map entry but not the spec or id vectors;
  `get_mapl_geom_from_spec` indexes the map by the spec's position
  (equal to the id while no other id source is used).
  *Inferred:* a spec found after deletion would fail in the map lookup.
- `Test_LatLonDistribution.pf` is not listed in `geom/tests/CMakeLists.txt`.
- `mapl_GeomGetHorzIJIndex` and `GeomGet` share the module
  `mapl_GeomAccessors_mod` whose source file is `GeomGet.F90`; the grid
  accessor module `mapl_GridAccessors_mod` is in `GridGet.F90`.
- Open question: none.
