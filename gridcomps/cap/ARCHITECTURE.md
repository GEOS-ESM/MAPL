# MAPL cap gridded component and executable driver

**Audience:** coding agents and developers working on the MAPL application driver.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [gridcomps layer](../ARCHITECTURE.md), [repository architecture](../../docs/ARCHITECTURE.md),
[AGENTS.md](../../AGENTS.md). The executable side (`mapl/`) is described in
[mapl/ARCHITECTURE.md](../../mapl/ARCHITECTURE.md).

## 1. Purpose

Two pieces together form the application. The **cap gridded component**
(`gridcomps/cap/CapGridComp.F90`, library `MAPL.cap`) is the top of the
component tree: each step it runs extdata, the root component, and history,
and it connects them. The **driver** (`mapl/CapDriver.F90`, program
`mapl/GEOS.F90`) is plain Fortran, not a component: it builds the clock,
creates the cap component, runs the time loop, writes checkpoints and
`cap_restart.yaml`.

## 2. Place in the hierarchy

- The driver creates the cap with `ESMF_GridCompCreate` and
  `esmf_GridCompSetServices(cap, mapl_GenericSetServices)` (`CapDriver.F90`,
  `make_driver`). The generic layer then reads the cap's config
  (`cap_gridcomp.yaml`, merged with `cap_driver.yaml`) and calls the user
  `setServices` named by `mapl: setServices: sharedObj: libMAPL.cap`.
- Children are not added in Fortran. They come from the `mapl: children:` map
  in `cap_gridcomp.yaml`: the root component (default name key `root_name`,
  example `GCM`), `EXTDATA` (`libMAPL.extdata`), `HIST` (`libMAPL.history`),
  each with a `config_file` (`gridcomps/cap/cap_gridcomp.yaml`;
  `gridcomps/cap/tests/basic_captest/`).
- Cap has no parent; the driver is its caller (`driver%run`, `driver%finalize`
  on a `MAPL_GriddedComponentDriver`).

## 3. Entry points

Cap (`CapGridComp.F90`, module `mapl_CapGridComp_mod`):
- `setServices`: registers initialize `GENERIC::INIT_USER` (`init`) and run
  `run`; attaches private state `CapGridComp`; reads resources (see 5);
  connects children (see 4).
- `init`: only fetches the private state; does nothing else.
- `run`: if `run_extdata`, run child `extdata_name` (default phase); always
  run the root child (`MAPL_GridCompRunChild`, default phase); if
  `run_history`, run child `history_name` with `phase_name='run'`.
- No finalize entry point and no restart of its own.

Driver (`CapDriver.F90`, public `MAPL_CapCreate`, `MAPL_CapRun`), called from
`GEOS.F90` in this order: `MAPL_Initialize(configFileNameFromArgNum=1, app_config=config)`,
`MAPL_CreateServers`, `MAPL_CapCreate`, `MAPL_RunServers`, `MAPL_CapRun`, `MAPL_Finalize`.

- `MAPL_CapCreate` (all processes): reads `cap_gridcomp_config` from
  `cap_driver.yaml` (the `config` argument is the already parsed
  application config), loads that file, merges the driver config into it
  (`merge_hconfig`), builds options and the clock (`make_clock`), creates the
  component on the model processes only (`get_model_pets`).
- `MAPL_CapRun`: returns at once on non-model processes (server processes).
  On model processes: `MAPL_DriverInitializePhases(driver, phases=MAPL_GENERIC_INIT_PHASE_SEQUENCE)`;
  `integrate`; `driver%finalize`; `update_restart`.
- `integrate` time loop (`do while (currTime < stopTime)`): if the time is in
  the optional `run_times` list (empty list means every step),
  `driver%run(phase_idx=MAPL_GENERIC_RUN_USER)`; then `advance_clock`, which runs
  phase `MAPL_GENERIC_RUN_CLOCK_ADVANCE` and calls `driver%clock_advance`;
  then `checkpoint(final=.false.)` if the step ran. After the loop,
  `checkpoint(final=.true.)`.
- `checkpoint`: returns unless `checkpointing: enabled`; then returns unless
  `is_record_time .neqv. final` (`is_record_time` = any alarm ringing on the
  clock), i.e. a non-final call writes only when an alarm rings and the final
  call writes only when none rings. When writing, makes
  `<path>/<ISO time>/`, removes symlink `last`, calls `driver%write_restart`,
  re-creates symlink `last` (all on root only, with barrier).

## 4. State

- Cap private state `CapGridComp`: names of the three children, flags
  `run_extdata`, `run_history`. No imports, exports or internal state are
  declared in the cap.
- Connections: if `run_extdata`, `MAPL_GridCompConnectAll(src=extdata, dst=root)`;
  if `run_history`, `MAPL_GridCompConnectAll(src=root, dst=history)`.
- Restart: cap itself has none. The driver writes `cap_restart.yaml`
  (key `currTime`, ISO string; key `repeatCount`) at the end of the run
  (`update_restart`) unless `skip_restart_write: true`. `make_clock`
  reads the same file at start (`currTime`, optional `repeatCount`) and
  asserts `currTime >= clock: start`. Component restarts are written by
  `driver%write_restart` into the checkpoint directory.

## 5. Configuration that changes structure

`cap_driver.yaml` (driver; `app: config:` in `mapl.yaml` names it):
`cap_gridcomp_config`, `restart`, `skip_restart_write`, `name` (default `CAP`),
`clock: {start, stop, dt, segment_duration, repeat_duration}`, `run_times`,
`checkpointing: {enabled, final, path, alarms}` (`mapl/cap_driver.yaml`,
`CapDriver.F90`, `make_cap_options`, `make_clock`).
`cap_gridcomp.yaml` (component): `root_name` (required: no default),
`extdata_name` (default `EXTDATA`), `history_name` (default `HIST`),
`run_extdata`, `run_history` (both default `.false.`), and `mapl: children:`.
`cap_restart.yaml`: `currTime`, optional `repeatCount`.
The clock stop time is `currTime + segment_duration`, not `clock: stop`.

## 6. Dependencies

`mapl_CapGridComp_mod` uses module `mapl`; the driver uses `MAPL`,
`mapl_HConfigUtilities_mod`, `pflogger`, `esmf`. `GEOS.x` links `MAPL` and
`MAPL.cap` (`mapl/CMakeLists.txt`); `MAPL.cap` depends on `MAPL`.

## 7. Known traps

- `run_extdata` and `run_history` default to false: a cap config that omits
  them runs only the root, and the extdata and history connections are not made.
  `gridcomps/cap/cap_gridcomp.yaml` omits both; every case under
  `gridcomps/cap/tests/` sets both true.
- The clock stops at the segment end, so `clock: stop` only needs to
  be at or beyond it; it is parsed and logged, not used to stop the loop.
- Cap children run in Fortran order extdata, root, history; the connections
  are `ConnectAll`, not per-field.
- The driver reads `cap_restart.yaml` from the path in `restart:`, relative
  to the working directory.
- Non-model processes (output servers) never enter `integrate`.

## 8. Decisions

### Driver is plain Fortran around a cap component
- Pattern: the time loop lives in `CapDriver.F90`, not in a component run phase; cap `run` is one step (`CapDriver.F90`, `integrate`; `CapGridComp.F90`, `run`). [Verified]
- Likely reason: the driver owns clock, checkpointing and servers, the component only sequences children. [Inferred]

## 9. Observations (noted, not fixed)

- `add_record_alarms` (`CapDriver.F90`) is defined but not called anywhere in
  the repository, so the `checkpointing: alarms` entries create no alarms
  (Verified by search). *Inferred:* with no alarms ever
  ringing, `checkpoint(final=.false.)` never writes and the final call
  always does (when enabled). The `final` key is stored (`do_final`, read by
  `make_checkpointing_options` only when `enabled` is defined) but never consulted.
- In `add_recurring_alarm`, `refTime` is read from `refTime` again after the
  default (current time) was chosen, so the default is overwritten. That
  routine is itself unused (see above).
- `init` of the cap is a no-op.
- Tests: `gridcomps/cap/tests/cases.txt` lists `basic_captest`,
  `parent_child_captest`, `write_restart`, `cap_restart_tests`,
  `pfio_server_captest`; each runs `GEOS.x mapl.yaml` through
  `run_captest.cmake`; `cap_restart_tests` also compares `cap_restart.yaml`
  with `cap_restart_expected.yaml`.
