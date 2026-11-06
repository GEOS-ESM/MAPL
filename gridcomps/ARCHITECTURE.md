# MAPL `gridcomps/`: gridded components shipped with MAPL

**Audience:** coding agents and developers working on MAPL's built-in gridded components.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [repository architecture](../docs/ARCHITECTURE.md), [AGENTS.md](../AGENTS.md).
The parent in the GEOSgcm fixture is `../../../docs/ARCHITECTURE.md` (a code
path, not a link).

## Purpose

`gridcomps/` holds ESMF gridded components that MAPL itself provides:
the top-level application component (cap), the diagnostic-output and
input-data components (history, extdata), a statistics component used by
history, one physics component (orbit), and test/support components. Each
is a separate shared library. They are created at run time from a YAML
`setServices: sharedObj:` key (Verified: `gridcomps/cap/tests/basic_captest/history.yaml`,
`extdata.yaml`, `GCM.yaml`), not linked into user code.
The generic component layer (`superstructure/generic/`) supplies the
lifecycle that all of them plug into (its own documents:
`docs/generic/ARCHITECTURE.md`, written separately).

## Directory and module map

`gridcomps/CMakeLists.txt` adds, in order: `orbit`, `cap`, `history`,
`configurable`, `extdata`, `statistics`, `FakeParent`, `componentDriverGridComp`.

| Directory | Library target | Role | Class |
|---|---|---|---|
| `cap/` | `MAPL.cap` | Top of the component tree: runs extdata, the root component, history each step; see [cap/ARCHITECTURE.md](cap/ARCHITECTURE.md) | framework |
| `history/` | `MAPL.history` | One child per output collection; writes through the output server; see [history/ARCHITECTURE.md](history/ARCHITECTURE.md) | framework |
| `extdata/` | `MAPL.extdata` | Fills export fields from files per rules in YAML; see [extdata/ARCHITECTURE.md](extdata/ARCHITECTURE.md) | framework |
| `statistics/` | `MAPL.statistics` | Time statistics (average, accumulate, min, max, variance) of fields; created by history as a helper child | framework (history helper) |
| `orbit/` | `MAPL.orbit` | Satellite-orbit swath masks (`MAPL_OrbGridCompMod.F90`, `MAPL_NominalOrbitsMod.F90`, `MAPL_OrbGridComp.rc`) | physics component |
| `configurable/` | `configurable_gridcomp` | Component whose exports are fabricated from YAML (`mapl: states: export:`); used in tests | test/support |
| `FakeParent/` | `fakeparent_gridcomp` | Stand-in parent that runs children's `Run1`/`Run2` phases | test/support |
| `componentDriverGridComp/` | `MAPL.componentDriverGridComp` | Fills, compares or generates states to test another component | test/support |

"Class" is the author's reading of the code and of the CMake `NOINSTALL`
marker (*Inferred*).

## Entry points by component

All use `MAPL_GridCompSetEntryPoint` inside a `setServices` routine. cap,
history, extdata, statistics, configurable, componentDriverGridComp and
FakeParent also define a bare external `setServices` wrapper at the end of
the source file; orbit (`MAPL_OrbGridCompMod.F90`) has none, and history's
collection child has none (it is added by procedure reference).
*Inferred:* loading by `sharedObj` name needs such a wrapper. Phase names are the generic-layer names
(`enums/GenericPhases.F90`).

| Component | Registered entry points |
|---|---|
| cap (`CapGridComp.F90`) | initialize `GENERIC::INIT_USER` (empty); run `run` |
| history (`HistoryGridComp.F90`) | initialize `GENERIC::INIT_ADVERTISE`; run `run` |
| history collection (`HistoryCollectionGridComp.F90`) | initialize `GENERIC::INIT_GEOM_A` (`init_geom`), initialize default phase (`init`); run `run` |
| extdata (`ExtDataGridComp.F90`) | initialize `GENERIC::INIT_REALIZE_PROVIDED`; run `run`; finalize |
| statistics (`StatisticsGridComp.F90`) | initialize `GENERIC::INIT_REALIZE_PROVIDED`; run `run` |
| orbit (`MAPL_OrbGridCompMod.F90`) | initialize, run (unnamed phases) |
| configurable | initialize (fills exports), run `run` |
| FakeParent | run (default phase); runs children phases `Run1`, `Run2` as selected by `run_phases` in YAML (`fake-parent-example.yaml`) |
| componentDriver | initialize, run `run`; run mode chosen from `runMode` (modes named in `componentDriverGridComp.F90`: GenerateExports, FillExportsFromImports, FillImports, CompareImportsToReference, CompareImportsToExpression) |

## Public interfaces

- Each component's only public symbol is `setServices` (module
  `mapl_<Name>GridComp_mod`) plus the external `setServices` wrapper.
- Configuration is YAML read through the component's `hconfig`; field
  specs are YAML or `MAPL_GridCompAddSpec` calls (history, extdata,
  orbit) rather than MAPL2 `StateSpecs.rc` files.
- Components are normally instantiated by YAML `mapl: children:` entries whose
  config has `mapl: setServices: sharedObj: lib<target>`. History instantiates its
  collection and statistics children directly with `MAPL_GridCompAddChild`
  and a procedure reference.

## Dependencies

(From the `CMakeLists.txt` files in each directory.)
- All depend on library `MAPL`; `MAPL.history` also on `MAPL.statistics`
  and `PFLOGGER::pflogger`; `MAPL.extdata` on `ESMF::ESMF` and
  `PFLOGGER::pflogger`; `configurable_gridcomp`, `fakeparent_gridcomp`,
  `MAPL.componentDriverGridComp` on `MAPL.generic` and `MAPL`.
- The executable `GEOS.x` (`mapl/CMakeLists.txt`) lists `MAPL.cap` as a
  dependency. The library `MAPL` lists `${EXTDATA_TARGET}` in its
  `DEPENDENCIES` and sets `BUILD_WITH_EXTDATA2G` from `$<BOOL:${USE_EXTDATA2G}>`;
  neither variable is set in this repository (*Unverified:* where they are
  set, if anywhere). The other gridcomp libraries are shared libraries
  loaded by name at run time.
- Used by: the executable driver (`mapl/CapDriver.F90`), tests in
  `gridcomps/cap/tests`, and scenario tests under
  `superstructure/generic/tests/scenarios`.

## Build and tests

- Targets are listed above. `cap/tests` is added unconditionally with
  `EXCLUDE_FROM_ALL`; `orbit`, `FakeParent`, `componentDriverGridComp` have no tests directory.
- `history`, `statistics`, `extdata`, `configurable`: `tests/` added when
  pFUnit is found (`PFUNIT_FOUND`), `EXCLUDE_FROM_ALL`.
  `history/tests`: `Test_HistoryGridComp.pf`, `Test_HistoryCollectionGridComp.pf`.
- `cap/tests/cases.txt` lists five end-to-end cases, each run as a ctest
  with label `ESSENTIAL` (`cap/tests/CMakeLists.txt`).
- `MAPL.componentDriverGridComp` is `NOINSTALL` and a dependency of
  `build-tests`.
- `extdata_dryrun_check.py` is installed to `bin` (`extdata/CMakeLists.txt`).

## Known traps

- Loadable components are looked up by library name (`libMAPL.history`,
  `libMAPL.extdata`, `libMAPL.cap`, `libconfigurable_gridcomp`); the target
  names in the table differ (`MAPL.history` etc., and `configurable_gridcomp`
  has no `MAPL.` prefix). `FakeParent` target is `fakeparent_gridcomp`.
- Source directories and module names do not always agree: directory
  `componentDriverGridComp` holds module
  `mapl_ComponentDriverDriverGridComp_mod`.
- History's collection phases are not run by the user: only history's
  `run` calls `MAPL_GridCompRunChildren(phase_name='run')`.
- `history/schema.yml` (with `???` placeholders and alternative syntaxes) and
  `history/collection.yml` (keys `time_handling`, `archive`, `file_format`,
  `regrid_method`) are sketches, not what the code reads.

## Observations (noted, not fixed)

- `cap`'s `init` only retrieves its private state; the entry point is
  registered but does nothing (`CapGridComp.F90`, `init`).
- `statistics/statistics.yaml` is an example input; history builds the
  statistics child's `stats:` list programmatically (`history/HistoryGridComp_private.F90`, `add_stats_gc`).
- `docs/user_guide/README.md` links a History document path that is not in
  this tree.
