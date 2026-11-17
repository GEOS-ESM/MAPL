# MAPL repository architecture (MAPL3 line)

**Audience:** coding agents and developers working on MAPL and on GEOS gridded components.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [AGENTS.md](../AGENTS.md). Parent: the GEOSgcm architecture document (`../../../docs/ARCHITECTURE.md` in the fixture).

## 1. Purpose
MAPL is a Fortran framework library on top of ESMF used by GEOS gridded components (`README.md`). `develop` is organized in layers, each a directory with its own CMake library target (top `CMakeLists.txt`, `add_subdirectory` calls). Layers are described in the layer documents below; this document gives the map and the cross-layer facts. Documents of the MAPL2 line live on `release/v2` and do not describe this tree.

## 2. Directory and module map
Layer directories (each links to its document):
- `enums/`: small enumeration modules (section 8).
- [`utils/`](../utils/ARCHITECTURE.md): MPI-free utilities (error handling, strings, time parsing, constants, regex, udunits2f wrapper). Target `MAPL.utils`; sub-targets `MAPL.constants`, `udunits2f` (`utils/CMakeLists.txt`, `utils/Constants/`, `utils/udunits2f/`). Comment in `utils/CMakeLists.txt`: "Tier 1 in MAPL v3 restructuring".
- [`mp_utils/`](../mp_utils/ARCHITECTURE.md): MPI-dependent utilities, including the profiler (`mp_utils/profiler/`), communicator splitting, saturation-vapor functions. Target `MAPL.mp_utils` ("Tier 2", `mp_utils/CMakeLists.txt`).
- [`pfio/`](../pfio/ARCHITECTURE.md): parallel client–server I/O (`MAPL.pfio`).
- [`infrastructure/`](../infrastructure/ARCHITECTURE.md): ESMF wrappers, fields, field bundles, field dictionary, geometry ([geom](../infrastructure/geom/ARCHITECTURE.md)), geometry I/O, regridder manager, vertical grids ("Tier 3"). Target `MAPL.infrastructure` consolidates the field dictionary sources; other subdirectories are separate targets (`infrastructure/CMakeLists.txt`).
- [`base/`](../base/ARCHITECTURE.md): remaining legacy core (file I/O, location streams, `SimpleBundle`, `SunOrbit`, memory utilities). Target `MAPL.base`.
- [`superstructure/`](../superstructure/ARCHITECTURE.md): `component/`, `generic/`, `state/` ("Tier 4"). Target `MAPL.superstructure`. Generic layer: [docs/generic/ARCHITECTURE.md](generic/ARCHITECTURE.md).
- [`mapl/`](../mapl/ARCHITECTURE.md): the `use MAPL` umbrella module, the framework object, the `GEOS.x` driver (section 4).
- [`gridcomps/`](../gridcomps/ARCHITECTURE.md): framework gridded components: `cap/` ([doc](../gridcomps/cap/ARCHITECTURE.md)), `history/` ([doc](../gridcomps/history/ARCHITECTURE.md)), `extdata/` ([doc](../gridcomps/extdata/ARCHITECTURE.md)), `orbit/`, `statistics/`, `configurable/`, `FakeParent/`, `componentDriverGridComp/` (`gridcomps/CMakeLists.txt`).

Other directories: section 8. Non-source: `@GEOS_FieldDictionary/` (an external repository checkout; its `geos_field_dictionary.yaml` is installed to `etc`, top `CMakeLists.txt`), `openspec/` (change proposals), `docs/`.

## 3. Dependency order
Build order is the `add_subdirectory` order in the top `CMakeLists.txt`: `enums`, `utils`, `mp_utils`, `pfio`, `infrastructure`, `include`, `base`, `superstructure`, `mapl`, `gridcomps`; then, if `BUILD_WITH_FARGPARSE`, `docs` and `benchmarks`; then `python`, `apps`, `tests`. (`pflogger_stub` is added earlier, only when `BUILD_WITH_PFLOGGER` is off.)

Declared library dependencies (`esma_add_library ... DEPENDENCIES`), MAPL targets only (external libraries omitted). `infrastructure/field_dictionary/CMakeLists.txt` has its own `esma_add_library` (its `DEPENDENCIES` include `MAPL.shared`, which is not a target in this tree), but `infrastructure/CMakeLists.txt` does not add that directory; its sources are compiled into `MAPL.infrastructure`.

| Target | Depends on |
|---|---|
| `MAPL.utils` | `MAPL.constants` |
| `MAPL.enums` | `MAPL.utils` |
| `MAPL.mp_utils` | `MAPL.utils`, `MAPL.constants` |
| `MAPL.pfio` | `MAPL.utils`, `MAPL.mp_utils` |
| `MAPL.esmf` | `MAPL.enums`, `MAPL.utils`, `MAPL.mp_utils` |
| `MAPL.geom` | `MAPL.pfio`, `MAPL.enums`, `MAPL.utils`, `MAPL.esmf`, `MAPL.mp_utils` |
| `MAPL.field` | `MAPL.enums`, `MAPL.vertical_grid`, `MAPL.utils`, `MAPL.esmf`, `MAPL.geom` |
| `MAPL.field_bundle` | `MAPL.field`, `MAPL.utils`, `MAPL.esmf`, `MAPL.geom` |
| `MAPL.regridder_mgr` | `MAPL.geom`, `MAPL.field_bundle`, `MAPL.utils` |
| `MAPL.GeomIO` | `MAPL.field`, `MAPL.field_bundle`, `MAPL.geom`, `MAPL.pfio`, `MAPL.base`, `MAPL.utils`, `MAPL.esmf`, `MAPL.regridder_mgr` |
| `MAPL.infrastructure` | `MAPL.esmf`, `MAPL.field`, `MAPL.field_bundle`, `MAPL.geom`, `MAPL.regridder_mgr`, `MAPL.GeomIO`, `MAPL.vertical` |
| `MAPL.base` | `MAPL.utils`, `MAPL.geom`, `MAPL.esmf`, `MAPL.pfio`, `MAPL.mp_utils`, `MAPL.field_bundle`, `MAPL.constants` |
| `MAPL.vertical_grid` | `MAPL.enums`, `MAPL.pfio` |
| `MAPL.vertical` | `MAPL.enums`, `MAPL.utils`, `MAPL.pfio`, `udunits2f` |
| `udunits2f` (`utils/udunits2f/`) | `MAPL.utils` |
| `MAPL.python_bridge` | `MAPL.generic` (`python/CMakeLists.txt`, variable `dependencies`) |
| `MAPL.component` | `MAPL.enums`, `MAPL.esmf` |
| `MAPL.state` | `MAPL.field_bundle`, `MAPL.utils`, `MAPL.esmf`, `MAPL.geom` |
| `MAPL.generic` | `MAPL.enums`, `MAPL.regridder_mgr`, `MAPL.geom`, `MAPL.vertical_grid`, `MAPL.GeomIO`, `MAPL.esmf`, `MAPL.field`, `MAPL.state`, `MAPL.utils`, `MAPL.vertical`, `MAPL.component`, `MAPL.field_bundle`, `MAPL.infrastructure` |
| `MAPL.superstructure` | `MAPL.state`, `MAPL.component`, `MAPL.generic` |
| `MAPL` | `MAPL.superstructure`, `MAPL.infrastructure`, `MAPL.pfio`, `MAPL.field`, `MAPL.state`, `MAPL.python_bridge`, `MAPL.base`, `MAPL.regridder_mgr`, `MAPL.esmf`, `MAPL.mp_utils`, `MAPL.GeomIO`, `${EXTDATA_TARGET}` |
| `MAPL.cap`, `MAPL.extdata`, `MAPL.orbit`, `MAPL.statistics` | `MAPL` (`MAPL.history` also `MAPL.statistics`) |
| `MAPL.componentDriverGridComp`, `configurable_gridcomp`, `fakeparent_gridcomp` | `MAPL.generic`, `MAPL` |

Sources: `CMakeLists.txt` of each directory. Direction in words: `enums`/`utils` at the bottom, then `mp_utils` and `pfio`, then `infrastructure` and `base`, which depend on each other's parts (`MAPL.base` uses `MAPL.geom`, `MAPL.field_bundle`; `MAPL.GeomIO` uses `MAPL.base`), then `superstructure`, then the `MAPL` umbrella, then `gridcomps`, which use `MAPL`; the umbrella does not depend on any gridcomp target, and `GEOS.x` links `MAPL` and `MAPL.cap` (`mapl/CMakeLists.txt`). The superstructure comment says "Consolidates: component/, generic3g/, state/" (`superstructure/CMakeLists.txt`). `${EXTDATA_TARGET}` is used in `mapl/CMakeLists.txt`; no `CMakeLists.txt` in this tree sets it (grep). Whether it is set by a parent build is not verified.

## 4. The `use MAPL` umbrella and public API
`mapl/MAPL.F90` (module `MAPL`) `use`s `mapl_Constants`, `mapl_Profiler_mod` (renaming `initialize`/`finalize` to `profiler_initialize`/`profiler_finalize`), `mapl_enums_api`, `mapl_utils_api`, `mapl_mp_utils_api`, `mapl_infrastructure_api`, `mapl_superstructure_api`, `mapl_base_api`, `mapl_pfio_api`, `mapl_MaplFramework_mod`, `mapl_PythonBridge_api`. `MAPL_Public_API.md` states the umbrella has no `private` statement, so everything it uses is re-exported, and lists the entities. It names `initialize_profiler`/`finalize_profiler`; the source renames are `profiler_initialize`/`profiler_finalize` (observation, section 10). Each layer's `API.F90` is its export module; `superstructure/API.F90` is `mapl_superstructure_api`. `mapl/` also holds `MaplFramework.F90`, `PfioServerGridComp.F90`, `CapDriver.F90`, `GEOS.F90` (executable `GEOS.x`), and default resources `mapl.yaml`, `cap_driver.yaml`. Details: [mapl/ARCHITECTURE.md](../mapl/ARCHITECTURE.md).

Public headers (macros) are copied to the build's `include/MAPL` directory (`include/CMakeLists.txt`, list `MAPL_PUBLIC_HEADERS`): `MAPL_ErrLog.h`, `MAPL_ErrLogMain.h`, `MAPL_Exceptions.h`, `MAPL_Generic.h`, `MAPL.h`, `NUOPC_ErrLog.h`, `unused_dummy.H`. `include/README.md` documents the error-handling macros.

## 5. Build structure
- Project `MAPL` 3.0.0, languages Fortran, C++, C; ESMF minimum 9.0.0 (`ESMA_ESMF_MIN_VERSION`); minimum gFTL 1.15.1, gFTL-shared 1.10.0, pFlogger 1.15.0, fArgParse 1.9.0 (top `CMakeLists.txt`).
- Uses ESMA_cmake (`include(esma)`), found under `ESMA_cmake`, `@ESMA_cmake` or `ESMA_cmake@`; `MAPL_STANDALONE` is set when found. Libraries are made with `esma_add_library`, named with `esma_set_this (OVERRIDE ...)`; the libraries listed in section 3 are `TYPE SHARED` where the `esma_add_library` call says so.
- Helper CMake modules in `cmake/`: `mapl_acg.cmake` (generator for gridcomp spec code), `mapl_create_stub_component.cmake`, `CheckCompilerCapabilities.cmake` with the `support_for_*.F90` probes, `NAGModtimeWorkaround.cmake`, `FindESMF.cmake`, `Findudunits.cmake`. The `mapl_*.cmake` files are installed.
- Options: `BUILD_WITH_FARGPARSE` (default ON), `BUILD_WITH_PFLOGGER` (default ON; OFF adds `pflogger_stub/`). `CMakePresets.json` and `presets/` (`CMakeDefaultPresets.json`, `CMakeNCCSPresets.json`) hold CMake presets.
- Strict extra flags (`STANDARD_F18` etc.) are added for Fortran in Debug builds when ESMA_cmake is present (top `CMakeLists.txt`).
- Installs: `BUILD_INFO.rc` (from `BUILD_INFO.rc.in`), `ecbuild_install_project`.

## 6. Tests
- Unit tests: pFUnit, `add_pfunit_ctest`, in `<dir>/tests/` directories added `EXCLUDE_FROM_ALL` when `PFUNIT_FOUND` (for example `utils/tests`, `superstructure/generic/tests`, `superstructure/state/tests`). The top-level `pfunit/` directory holds MAPL's pFUnit support sources (`MAPL_Initialize.F90`, `MAPL_pFUnit_Throw.F90`, `ESMF_TestCase.F90`, ...) and is `EXCLUDE_FROM_ALL`.
- Ctest labels (from `set_tests_properties` ... `LABELS`): `ESSENTIAL` (for example `MAPL.utils.tests`, `MAPL.pfio.tests`, `MAPL.state.tests`, `MAPL.generic.scenarios`, `MAPL.generic.transforms`, `MAPL.generic.vertical`, `MAPL.generic.aspects`, `MAPL.generic.components`, `MAPL.generic.core`, `MAPL.generic.graph`, `udunits2f.tests` in `utils/udunits2f/tests`, component tests in `tests/MAPL3G_Component_Testing_Framework`), `COMP_TESTS_BIG`, and `PERFORMANCE` (the `pFIO_tests_*` tests in `pfio/tests/CMakeLists.txt`). `CTestDashboard.cmake` runs the `ESSENTIAL` label (`INCLUDE_LABEL`).
- `tests/` (target `MAPL.test_utilities`): `MAPL3G_Component_Testing_Framework/` (cases in `test_cases/`, described in `test_case_descriptions.md`, built when `BUILD_WITH_FARGPARSE`), `GetHorzIJIndex/`, `generate_extdatadriver_input.py`.
- `apps/tests/` (`acg3`, `Regrid_Util`), `mapl/tests`, per-layer `tests/` directories.

## 7. Parent
The parent is the GEOSgcm fixture: MAPL is checked out at `src/Shared/@MAPL` and built from source when no external MAPL is found (`if (NOT MAPL_FOUND) ... esma_add_subdirectory (MAPL)` in `src/Shared/CMakeLists.txt` in the fixture; otherwise an external MAPL is used); see the fixture's architecture document at `../../../docs/ARCHITECTURE.md` (from the MAPL root). Standalone builds use `MAPL_STANDALONE` (top `CMakeLists.txt`). GEOS gridded components use MAPL by `use MAPL` and the macros in `include/`.

## 8. Small directories
- `enums/` (`MAPL.enums`): one module per enumeration, for example `ConservationType`, `NormalizationType`, `QuantityType`, `VerticalStaggerLoc`, `VerticalAlignment`, `VectorBasisKind`, `GenericPhases`, `CouplerPhases`, `ValidationMode`; `enums/API.F90` (`mapl_enums_api`) re-exports them with the `MAPL_` prefix. Depends on `MAPL.utils`.
- `include/`: preprocessor headers (section 4). Not compiled; `MAPL_private_state.h` and `MAPL_TestErr.h` are not in `MAPL_PUBLIC_HEADERS`.
- `udunits2f/` (top level): Fortran wrapper of the udunits library (`UDSystem.F90`, `udunits2f.F90`, ...). The target used by the build is the copy under `utils/udunits2f/`, added by `utils/CMakeLists.txt`; no `CMakeLists.txt` adds the top-level directory (observation).
- `pflogger_stub/`: stub of the pFlogger interface, built only when `BUILD_WITH_PFLOGGER` is off; target named `pflogger`.
- `python/` (`MAPL.python_bridge`): Fortran side `PythonBridge.F90` and `API.F90`; the Python side (`MAPL_PythonBridge/`) is compiled only if `BUILD_PYTHONBRIDGE` is set; `python/MAPL/` is a Python package of utilities. `python/README.md` calls the bridge experimental.
- `apps/`: Python and Perl scripts installed to `etc`/`bin` (`MAPL_GridCompSpecs_ACG.py`, `mapl_acg.pl`, `mapl_stub.pl`, `combine_restarts.py`, `split_restart.py`, `latlon_to_face.py`, `forcing_converter/`), the executable `Regrid_Util.x` (when `BUILD_WITH_FARGPARSE`), and the executable `graph_diagnostic.x` (graph DOT/JSON export; built by the `build-tests` target). See `apps/README_ACG_WRITER.md`.
- `benchmarks/`: `esmf/` and `io/` benchmark programs, when `BUILD_WITH_FARGPARSE`.
- `tests/`: section 6.
- `presets/`, `CMakePresets.json`, `cmake/`: section 5.

## 9. Differences from the MAPL2 line
Taken from `docs/mapl3/diffs-from-mapl2.md` and `docs/mapl3/api-changes.md` (*Unverified* unless cited to code here); the MAPL2-line documents are on `release/v2`.
- Layered directories (`utils`, `mp_utils`, `infrastructure`, `superstructure`) replace the MAPL2 `shared`, `profiler`, `generic`, `oomph`, `MAPL_cfio`, `griddedio`, `field_utils` layout (this tree has none of them; the `README.md` list is out of date).
- The generic component is `superstructure/generic/` ([doc](generic/ARCHITECTURE.md)); `MAPL_MetaComp`-style APIs are stubbed out (`superstructure/generic/MAPL3_Deprecated.F90`; list in `api-changes.md`).
- Component structure and connections are declared in YAML (`ComponentSpecParser` in `superstructure/generic/`).
- Cap, History, ExtData are new implementations (`gridcomps/`); only one ExtData (`gridcomps/extdata/`); a Statistics gridcomp exists (`gridcomps/statistics/`).
- ESMF 9.0.0 or newer is required (`ESMA_ESMF_MIN_VERSION`).
- Default build produces `GEOS.x` from `mapl/GEOS.F90`.

## 10. Observations (noted, not fixed)
- `MAPL_Public_API.md` names `initialize_profiler`/`finalize_profiler` as the renamed entities; `mapl/MAPL.F90` renames `initialize`/`finalize` to `profiler_initialize`/`profiler_finalize`.
- `README.md` lists MAPL2-layout directories.
- `${EXTDATA_TARGET}` is referenced in `mapl/CMakeLists.txt` and not set in this tree; the top `CMakeLists.txt` prints "Building with ExtData2G" unconditionally.
- Top-level `udunits2f/` is not added by any `add_subdirectory`; `utils/udunits2f/` has its own `CMakeLists.txt` with the same target name.
- `MAPL.base` is built after `infrastructure` (top `CMakeLists.txt`) and depends on `MAPL.geom` and `MAPL.field_bundle`; `MAPL.GeomIO` (in `infrastructure/`) depends on `MAPL.base`.
