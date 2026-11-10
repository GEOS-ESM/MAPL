# MAPL superstructure layer (MAPL3 line)

**Audience:** coding agents and developers working on MAPL and on GEOS gridded components.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [docs/ARCHITECTURE.md](../docs/ARCHITECTURE.md), [AGENTS.md](../AGENTS.md).

## 1. Purpose
`superstructure/` holds the layer that defines how gridded components are built and driven: `component/` (drivers that run a component with its states and clock), `generic/` (the generic gridded component that wraps user components), and `state/` (operations on ESMF states). Comment in `superstructure/CMakeLists.txt`: "Superstructure layer (Tier 4 in MAPL v3 restructuring). Consolidates: component/, generic3g/, state/."

## 2. Directory and module map
- `API.F90`: module `mapl_superstructure_api`, which `use`s `mapl_generic_api`, `mapl_state_api`, `mapl_component_api`. `CMakeLists.txt`: target `MAPL.superstructure` (`TYPE SHARED`), `add_subdirectory` order `state`, `component`, `generic`; dependencies `MAPL.state`, `MAPL.component`, `MAPL.generic`, pFlogger, gFTL-shared, ESMF.
- `component/` (target `MAPL.component`):
  - `ComponentDriver.F90`: abstract `ComponentDriver` with deferred `initialize`, `run`, `finalize`, `write_restart`, `read_restart` (each with optional `phase_idx`) and `get_states`; `ComponentDriverPtr`; the `DriverInitializePhases` helper that calls `initialize` for each phase in a list.
  - `GriddedComponentDriver.F90` and `GriddedComponentDriver/` (submodules `initialize`, `run`, `finalize`, `read_restart`, `write_restart`, `clock_advance`, `get_states`, `get_clock`, `set_clock`, `get_gridcomp`, `get_name`, `run_export_couplers`, `run_import_couplers`, `add_export_coupler`, `add_import_coupler`): `GriddedComponentDriver` extends `ComponentDriver`; holds an `ESMF_GridComp`, a `MultiState`, an `ESMF_Clock`, and vectors of export and import coupler drivers.
  - `MultiState.F90`: `MultiState` bundles import, export and internal `ESMF_State`s; `get_state` by string or ESMF intent.
  - `ComponentDriverVector.F90`, `ComponentDriverPtrVector.F90`, `GriddedComponentDriverMap.F90`, `GriddedComponentDriverVector.F90`: containers.
- `generic/` (target `MAPL.generic`): see [docs/generic/ARCHITECTURE.md](../docs/generic/ARCHITECTURE.md) (module map and interfaces; not repeated here) and the scoped `superstructure/generic/AGENTS.md`. Subdirectories listed in its `CMakeLists.txt`: `specs`, `registry`, `connection`, `transforms`, `couplers`, `vertical`, `ComponentSpecParser`, `OuterMetaComponent`, `tests`.
- `state/` (target `MAPL.state`): operations on `ESMF_State`: `StateGet`, `StateSet`, `StateGetPointer`, `StateGetGeom`, `StateAddMethod`, `StateMerge`, `StateUtils`, `StateArithmeticParser` (`ArthParser.md` describes its expression rules), `StateMasking`, `StateFilter`, `StateDestroy`, with templates `get_array_ptr_template.H`, `overload.macro`, `undef.macro`.

## 3. Public interfaces
- `mapl_component_api` (`component/API.F90`) exports `MAPL_DriverInitializePhases` and `MAPL_GriddedComponentDriver`.
- `mapl_state_api` (`state/API.F90`) exports `MAPL_StateGet`, `MAPL_StateSet`, `MAPL_StateGetPointer`, `MAPL_StateGetGeom`, `mapl_StateAddMethod`, `MAPL_StateDestroy`, `MAPL_StateMask`, `MAPL_StateMerge`, `mapl_StateEval`, `MAPL_ParserVariablesInExpression`.
- `mapl_generic_api` (`generic/API.F90`): the `mapl_GridComp*` procedures (for example `mapl_GridCompAddChild`, `mapl_GridCompAddVarSpec`, `mapl_GridCompGetResource`, `mapl_GridCompSetEntryPoint`), `UserSetServices` and related, the spec types; see the generic-layer document.
- All three are re-exported by the `MAPL` umbrella (`mapl/MAPL.F90` uses `mapl_superstructure_api`).

## 4. Dependencies
From the `CMakeLists.txt` files:
- `MAPL.component` depends on `MAPL.enums`, `MAPL.esmf`; `MAPL.state` on `MAPL.field_bundle`, `MAPL.utils`, `MAPL.esmf`, `MAPL.geom`; `MAPL.generic` on `MAPL.enums`, `MAPL.regridder_mgr`, `MAPL.geom`, `MAPL.vertical_grid`, `MAPL.GeomIO`, `MAPL.esmf`, `MAPL.field`, `MAPL.state`, `MAPL.utils`, `MAPL.vertical`, `MAPL.component`, `MAPL.field_bundle`, `MAPL.infrastructure`.
- Used by: `MAPL` (umbrella) and through it the gridcomps; `MAPL.componentDriverGridComp`, `configurable_gridcomp`, `fakeparent_gridcomp` list `MAPL.generic` directly; `MAPL.Apps.tests.acg3` (`apps/tests/acg3/CMakeLists.txt`) links `MAPL.state` and `MAPL.generic`; the `python/` target `MAPL.python_bridge` (`python/CMakeLists.txt`) lists `MAPL.generic`.
- `MAPL.component` and `MAPL.state` do not depend on each other or on `MAPL.generic`. A comment in `GriddedComponentDriver.F90` says `run_self()` is in a submodule "to avoid circular dependency on OuterMetaComponent".

## 5. Build and tests
- Targets: `MAPL.superstructure`, `MAPL.state`, `MAPL.component`, `MAPL.generic`. `MAPL.component` and `MAPL.generic` define Fortran submodules with `esma_add_fortran_submodules`.
- Tests: `state/tests` (ctest `MAPL.state.tests`, label `ESSENTIAL`); `generic/tests` (ctests `MAPL.generic.scenarios`, `.transforms`, `.vertical`, `.aspects`, `.components`, `.core`, each `ESSENTIAL`); `component/` has no `tests/` directory. Tests are pFUnit, added when `PFUNIT_FOUND`, `EXCLUDE_FROM_ALL`.

## 6. Known traps
- `generic/CMakeLists.txt` adds `specs` with the binary directory `generic_specs`; the comment explains that a build directory named `specs` can make some compilers fail (spack Intel toolchain).
- `GriddedComponentDriver` operations are `recursive` and take `phase_idx`; initialization is by phase list (`DriverInitializePhases`), not a single call.

## 7. Observations (noted, not fixed)
- `generic/CMakeLists.txt` lists `MAPL.esmf` and `MAPL.utils` twice in `DEPENDENCIES`.
