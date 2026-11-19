# mapl/: umbrella module, framework object, server utilities and the `GEOS.x` driver (library `MAPL`)

**Audience:** coding agents and developers working on MAPL `mapl/`.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [../docs/ARCHITECTURE.md](../docs/ARCHITECTURE.md) (repository map; in the fixture, the parent
repository document is `../../../docs/ARCHITECTURE.md` from the repository root). Rules for agents: [../AGENTS.md](../AGENTS.md).

## Purpose
`mapl/` builds the top library `MAPL` that components `use`, and the executable `GEOS.x`. It holds the umbrella module `MAPL`, the
framework singleton (`MaplFramework`: ESMF, logger, profilers, server set-up and shutdown), helpers that partition processes among
the model and servers, the server gridded component, and the driver that creates the cap and runs the time loop.
The cap gridded component itself is in `gridcomps/cap/` and is documented in [../gridcomps/cap/ARCHITECTURE.md](../gridcomps/cap/ARCHITECTURE.md); it is not described here.

## Directory and module map
| File | Module / program | Role |
|---|---|---|
| `MAPL.F90` | `MAPL` | Umbrella. `use`s `mapl_Constants`, `mapl_Profiler_mod` (renamed `profiler_initialize`, `profiler_finalize`), `mapl_enums_api`, `mapl_utils_api`, `mapl_mp_utils_api`, `mapl_infrastructure_api`, `mapl_superstructure_api`, `mapl_base_api`, `mapl_pfio_api`, `mapl_MaplFramework_mod`, `mapl_PythonBridge_api`. No `private` statement or code. |
| `MaplFramework.F90` | `mapl_MaplFramework_mod` | Type `MaplFramework`, the module singleton `the_mapl_object`, and procedural wrappers (below). |
| `MaplServerUtilities.F90` | `mapl_MaplServerUtilities_mod` | `ServerResources` type; `pets_on_ssis`, `get_num_ssis`, `get_app_petCount`, `get_server_hconfigs`, `get_ssis_per_server`, `create_server_comms`, `make_server_gridcomp`. |
| `PfioServerGridComp.F90` | `mapl_PfioServerGridComp_mod` plus an external `setServices` | ESMF gridded component that runs a pFIO server. |
| `CapDriver.F90` | `mapl_CapDriver_mod` | `MAPL_CapCreate`, `MAPL_CapRun`. |
| `GEOS.F90` | program `geos` (executable `GEOS.x`) | Calls the framework and driver in order. |
| `cap_driver.yaml` | data | Driver configuration (below). |
| `mapl.yaml` | data | Top-level application configuration example: `app.config`, `esmf`, `pflogger`, `app_petcount`, `servers`. |
| `tests/` | | pFUnit tests of `MaplServerUtilities`. |

## Program flow (`GEOS.F90`)
1. `MAPL_Initialize(configFileNameFromArgNum=1, app_config=config)`: initializes the framework and returns the hconfig named by the top-level
   `app: config:` key (here `cap_driver.yaml`).
2. `MAPL_CreateServers(servers)`: builds communicators and server gridded components.
3. `MAPL_CapCreate(driver, config=config)`: builds the cap and its clock.
4. `MAPL_RunServers(servers)`: server processes block here serving requests; model processes return at once.
5. `MAPL_CapRun(driver, config=config)`: on model processes only, runs the initialize phases, the time loop and finalize.
6. `MAPL_Finalize`.

## `MaplFramework` (mapl/MaplFramework.F90)
- Public: type `MaplFramework`, `MAPL_initialize`, `MAPL_finalize`, `MAPL_Get`, `MAPL_CreateServers`, `MAPL_RunServers`, `MAPL_ConnectToServer`, `MAPL_PublishServer`.
- `initialize` runs, in order: `initialize_esmf`, `initialize_pflogger` (if `BUILD_WITH_PFLOGGER`), `initialize_profilers`, `validate_resources`,
  `initialize_udunits`, `initialize_field_dictionary`, `initialize_field_fill_defaults`, `initialize_default_servers`, and then registers two vertical-grid
  factories with the vertical-grid manager: `"FixedLevels"` and `"Model"`. It asserts that the object is not already initialized.
- Two configuration paths (comments in the file): (a) ESMF not yet initialized: MAPL calls `ESMF_Initialize`, reading the file given by
  `configFilenameFromArgNum` with config key `esmf`; (b) ESMF already initialized: an `hconfig` argument is required. In both cases `this%hconfig` is the
  whole top-level configuration and `this%mapl_hconfig` is its `mapl:` subsection. `normalize_mapl_hconfig` copies root-level `app_petcount`, `servers` and `pflogger`
  (as `pflogger_cfg_file`) into the `mapl:` section when absent there.
- Default servers: `initialize_default_servers` always creates two local servers and clients, `MAPL_DEFAULT_INPUT_SERVER` (a `ClientThread`) and
  `MAPL_DEFAULT_OUTPUT_SERVER` (a `FastClientThread`), on a duplicate of the world communicator, so programs that never call `MAPL_CreateServers` can still use pFIO.
- `create_servers`: with no `servers:` key it returns an empty array. Otherwise it replaces `model_comm` with the model-only partition (first `app_petcount` PETs), creates
  `local: true` servers on model PETs (`initialize_configured_local_servers`), and builds one ESMF gridded component per remote server (`initialize_non_default_servers`,
  `make_server_gridcomp`). Server processes are given contiguous single-system-image (node) ranges after the model's.
- Server `subclass` key: `MpiServer` (default) or `MultiGroupServer` (with `nwriter_per_node`, default 1); anything else asserts.
- `validate_resources` asserts that model plus server node counts fit the available nodes and logs a warning for unused nodes.
- `run_servers`: model PETs return; server PETs call `ESMF_GridCompRun` on each server gridded component.
- `MAPL_ConnectToServer(server_name)` and `MAPL_PublishServer` go through `directory_service` (`connect_to_server`, `publish`, `connect_to_client`). `History` calls `MAPL_ConnectToServer` (gridcomps/history/HistoryGridComp.F90).
- `finalize` order: servers, directory-service resources, `model_comm`, profiler, logger, ESMF.
- `MAPL_Get` returns `directory_service`, `is_model_pet`, `hconfig`.

## Server gridded component (`PfioServerGridComp.F90`)
Registers only a run phase. `run` takes the gridded component name as server name, reads the `ServerResources` private state stored by `make_server_gridcomp`,
creates an `MpiServer` or `MultiGroupServer`, calls `MAPL_PublishServer` (blocks until a client connects), then `server%start`.
A top-level external `setServices` wraps the module procedure so ESMF can find it by symbol when the library is named as `sharedObj` in a `servers:` entry;
`make_server_gridcomp` reads `sharedObj` and `userRoutine` from the server entry. In `mapl.yaml` the `pfio:` entry has only `num_nodes`, and the `mit:` entry names `libmit` and `init_comm_`.

## Driver (`CapDriver.F90`, `cap_driver.yaml`)
- `MAPL_CapCreate`: reads the gridded-component config named by `cap_gridcomp_config`, merges the driver-level keys into it (`merge_hconfig`), builds `CapOptions`
  (`name` default `CAP`; `checkpointing`), makes the clock and creates the cap with `ESMF_GridCompCreate` plus `mapl_GenericSetServices`, wrapped in a `MAPL_GriddedComponentDriver`.
  If model PETs are a subset of the PETs, the gridded component is created with its own VM on the model PET list (`make_driver`, `get_model_pets`).
- `MAPL_CapRun`: on model PETs only. `MAPL_DriverInitializePhases` with `MAPL_GENERIC_INIT_PHASE_SEQUENCE`; `integrate`; `driver%finalize`; `update_restart`.
- `integrate`: while current time < stop time, runs `MAPL_GENERIC_RUN_USER` (only at times listed in `run_times` when that key is defined), then advances the clock, then writes a checkpoint.
  A final checkpoint follows the loop.
- `cap_driver.yaml` keys: `cap_gridcomp_config` (file `cap_gridcomp.yaml`), `restart` (`cap_restart.yaml`), `skip_restart_write`, `clock` (`start`, `stop`, `dt`, `segment_duration`,
  optional `repeat_duration`), `checkpointing` (`enabled`, `final`, `path`, `alarms` with `frequency`+`refTime`, `times`, `offsets`). Two of these keys do not act as written; see Known traps and [../gridcomps/cap/ARCHITECTURE.md](../gridcomps/cap/ARCHITECTURE.md) for detail.
- Checkpoint directories use a timestamp path with a symbolic link (`make_directory`, `make_symlink`, `remove_symlink`, `get_timestamp`). Details of what is checkpointed belong to the cap gridded component and `superstructure/generic`.

## Public interfaces
Components get everything through `use MAPL`. `GEOS.F90` also uses `mapl_CapDriver_mod` directly. External programs use `MAPL_Initialize`, `MAPL_Finalize`, `MAPL_Get`.

## Dependencies
- `MAPL` `DEPENDENCIES`: `MAPL.superstructure`, `MAPL.infrastructure`, `MAPL.pfio`, `MAPL.field`, `MAPL.state`, `MAPL.python_bridge`, `MAPL.base`, `MAPL.regridder_mgr`,
  `MAPL.esmf`, `MAPL.mp_utils`, `MAPL.GeomIO`, `${EXTDATA_TARGET}`, ESMF, NetCDF-Fortran, MPI, pFlogger (mapl/CMakeLists.txt). `${EXTDATA_TARGET}` is referenced but not set in any `CMakeLists.txt` or `.cmake` file in this repository (searched).
- `GEOS.x` is built with `ecbuild_add_executable` from `GEOS.F90`, depends on `MAPL`, `MAPL.cap`, ESMF, and is added as a dependency of `build-tests`.
- `PRIVATE` compile definition `BUILD_WITH_EXTDATA2G` when `USE_EXTDATA2G` is true. `USE_EXTDATA2G` appears only in that `target_compile_definitions` line and is set nowhere in this repository (searched), like `${EXTDATA_TARGET}`.

## Build and tests
- `tests/CMakeLists.txt` defines pFUnit test `MAPL.mapl.server_utilities` from `Test_MaplServerUtilities.pf` (`MAX_PES 2`, label `ESSENTIAL`), 17 `@test` procedures.
  Only the server-utilities helpers are tested there.

## Known traps
- Server names double as ESMF gridded-component names and as directory-service port names (`PfioServerGridComp.F90`, `run`).
- `model_comm` is first a duplicate of the world communicator (`initialize_default_servers`) and is replaced by the model-only partition in `create_servers` when a `servers:` section exists.
- `MAPL_RunServers` blocks on server processes until the clients terminate; nothing after it runs for them except `MAPL_Finalize`.
- `clock: stop` does not stop the run: `make_clock` parses and logs it, then sets the clock stop time to `currTime + segment_duration` (mapl/CapDriver.F90, `make_clock`).
- `checkpointing: alarms` creates no alarms: `add_record_alarms` is defined in `CapDriver.F90` but never called (searched). In `checkpoint`, `_RETURN_UNLESS(is_record_time .neqv. final)` with `is_record_time` true only when an alarm rings, so only the final checkpoint is written (when `enabled` and `final`).
- `app_config` is parsed once in `MAPL_Initialize` and passed to `MAPL_CapCreate` and `MAPL_CapRun`.

## Observations (noted, not fixed)
- `BUILD_WITH_EXTDATA2G` is defined for `MAPL` when `USE_EXTDATA2G` is true, but nothing in this repository sets `USE_EXTDATA2G`, and no source in `mapl/` references the definition (searched).
- Server subclasses are chosen by string match in two places (`add_local_server` in `MaplFramework.F90` and `run` in `PfioServerGridComp.F90`); both accept the same two names, and the local path hard-codes `nwriter_per_node=1`.
- Comments in `MaplFramework.F90` `finalize` retain commented-out communicator cleanup.
