# MAPL Cap: architecture

**Audience:** coding agents and developers working on the MAPL driver (`MAPL_Cap`), the Cap gridded component and the NUOPC wrapper.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `gridcomps/Cap/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [../ARCHITECTURE.md](../ARCHITECTURE.md). Existing user document:
[CapGridComp.md](CapGridComp.md) (CAP.rc and `cap_restart` for users; not
restated here). Siblings: [../History/ARCHITECTURE.md](../History/ARCHITECTURE.md),
[../ExtData/ARCHITECTURE.md](../ExtData/ARCHITECTURE.md),
[../ExtData2G/ARCHITECTURE.md](../ExtData2G/ARCHITECTURE.md).

## 1. Purpose

Cap is the top of every MAPL application. Two layers:

- `MAPL_Cap` (driver object, module `MAPL_CapMod`): starts MPI and ESMF,
  splits the communicator for pFIO servers and ensemble members, then creates
  and drives the Cap gridded component.
- `MAPL_CapGridComp` (module `MAPL_CapGridCompMod`): an ESMF gridded
  component that owns the clock, reads `CAP.rc`, creates the application's root
  component and the `HIST` and `EXTDATA` children, and runs the time loop.

## 2. Build

`CMakeLists.txt` defines `MAPL.cap`. Sources: `MAPL_Cap.F90`,
`MAPL_CapGridComp.F90`, `MAPL_NUOPCWrapperMod.F90`, `CapOptions.F90`,
`ExternalGCStorage.F90`; `FlapCLI.F90` only if `BUILD_WITH_FLAP`,
`FargparseCLI.F90` only if `BUILD_WITH_FARGPARSE`.
Dependencies: `MAPL.shared`, `MAPL.constants`, `MAPL.base`, `MAPL.profiler`,
`MAPL.history`, `MAPL.ExtData`, `${EXTDATA2G_TARGET}`; public link to gftl,
gftl-shared, ESMF, NetCDF Fortran; private link to MPI and OpenMP (and FLAP /
fargparse when enabled). Compile definition `BUILD_WITH_EXTDATA2G` is set when
`USE_EXTDATA2G` is on (see [../ARCHITECTURE.md](../ARCHITECTURE.md)); without it,
requesting ExtData2G at run time fails with "ExtData2G requested but not built".

## 3. Contents

| File | Module / type | Role |
|---|---|---|
| `MAPL_Cap.F90` | `MAPL_CapMod`, `MAPL_Cap` | driver object; constructors from a `set_services` routine or from a DSO name |
| `CapOptions.F90` | `mapl_CapOptionsMod`, `MAPL_CapOptions` | plain option record (communicator, `cap_rc_file`, `egress_file`, `root_dso`, server sizes, ensemble, logging) |
| `FargparseCLI.F90` | `MAPL_FargparseCLIMod`, `MAPL_FargparseCLI` | command-line parser (fargparse) returning a `MAPL_CapOptions` |
| `FlapCLI.F90` | FLAP-based equivalent | same job with FLAP |
| `MAPL_CapGridComp.F90` | `MAPL_CapGridCompMod`, `MAPL_CapGridComp` | the Cap gridded component (about 2000 lines) |
| `MAPL_NUOPCWrapperMod.F90` | `MAPL_NUOPCWrapperMod` | NUOPC model component that wraps a `MAPL_Cap` |
| `ExternalGCStorage.F90` | `MAPL_ExternalGCStorage` | two small types (`t_extdata_state`: an ESMF state and gridcomp; `extdata_wrap`) |

## 4. The driver: `MAPL_Cap`

Constructors: `MAPL_Cap(name, set_services, cap_options=...)` (the root's
`SetServices` is passed as a procedure) and `MAPL_Cap(name, cap_options=...)`
(root loaded from the shared object named by `cap_options%root_dso`).

`cap%run()` calls, in order:

1. `run_ensemble`: `create_member_subcommunicator` splits the world
   communicator into `n_members` member communicators; for the
   member this rank belongs to:
2. `initialize_io_clients_servers(comm)`: `cap_server%initialize` (pFIO
   `ServerManager`) with the input/output server sizes from `CapOptions`
   (`npes_input_server`, `nodes_output_server`, `oserver_type`, ...). It splits
   the member communicator into a model part and server parts. See
   [../../pfio/ARCHITECTURE.md](../../pfio/ARCHITECTURE.md).
3. `run_member`: only ranks in the split communicator named `model` call
   `run_model`; server ranks stay inside the server initialization.
4. `finalize_io_clients_servers` (terminates the input and output clients on
   model ranks, then finalizes the server manager) and `splitter%free_sub_comm`.
5. `finalize_mpi`.

`run_model(comm)`: picks the ESMF configuration file (`ESMF.rc`, or the file
named by environment variable `ESMF_CONFIG_FILE`; if it exists it is passed to
`ESMF_Initialize`, otherwise `esmf_logging_mode` from the options is used);
calls `ESMF_Initialize` on `comm`; refuses an `mpiuni` ESMF; sets the MOAB
mesh flag; then `initialize_cap_gc`, and on `cap_gc`: `set_services`,
`initialize`, `run`, `finalize`; then `ESMF_Finalize(endflag=ESMF_END_KEEPMPI)`
and reports throughput. `initialize_cap_gc` reads the pin flag from the
CAP.rc file (`GetPinFlagFromConfig`) and calls `MAPL_CapGridCompCreate` with
the root `set_services` or `root_dso`, the Cap rc file and the egress file.
`step_model` and `rewind_model` exist for callers that drive the model step
by step (used by the NUOPC wrapper and other embedding drivers).

Command line: `MAPL_FargparseCLI()` (or `MAPL_FlapCLI`) produces the
`MAPL_CapOptions`; the program passes it to the constructor.

## 5. The Cap gridded component

`MAPL_CapGridCompCreate(cap, cap_rc, name, final_file, n_run_phases,
root_set_services, root_dso)` loads `cap_rc` into `cap%config` and creates the
ESMF gridcomp `cap%gc`. Entry points (`set_services_gc`): initialize
`initialize_gc`, run `run_gc` (one registration per run phase, `n_run_phases`),
finalize `finalize_gc`. Type-bound wrappers: `set_services`, `initialize`,
`run`, `finalize`, plus accessors (`get_model_duration`, `get_heartbeat_dt`,
`get_current_time`, step counter, `set_grid`, `set_clock`, `rewind_clock`,
`get_field_from_import`/`_internal`, `record_state`/`refresh_state`).
`set_grid` and `set_clock` let an embedding driver supply a grid or clock;
with an external clock Cap assumes one step per call (`nsteps = 1`).

### 5.1 Keys read

Where the code reads them (the `CAP.rc` file is `cap%config`; resources are
read through `MAPL_GetResource` on Cap's own MAPL object):

| Key | Read in | Default / note |
|---|---|---|
| `ROOT_NAME:` | `initialize_gc` | `ROOT`; name of the root child |
| `ROOT_CF:` | `initialize_gc` | `ROOT.rc`; loaded as `cf_root` and given to the root |
| `HIST_CF:` | `initialize_gc` | `HIST.rc`; loaded as `cf_hist` |
| `EXTDATA_CF:` | `initialize_gc` | `ExtData.rc`; loaded as `cf_ext`, also stored under `CF_EXTDATA:` |
| `USE_EXTDATA2G:` | `initialize_gc` | `.false.` in code (`.TRUE.` in the GEOSgcm template) |
| `HEARTBEAT_DT:` | `initialize_gc`, `MAPL_ClockInit` | required; must equal `RUN_DT:` in the root rc and in the ExtData rc where present, else the run fails; also written into the ExtData config as `RUN_DT:` |
| `BEG_DATE:`, `END_DATE:` | `MAPL_ClockInit` | packed `YYYYMMDD HHMMSS`; replaced by `BEG_YY`... `END_S` when absent; the start time is overridden by `cap_restart` |
| `JOB_SGMT:` | `MAPL_ClockInit` | duration; falls back to `JOB_DURATION:` then `DUR_YY`...`DUR_S`; `nsteps = duration / timestep` |
| `NUM_DT:`, `DEN_DT:` | `MAPL_ClockInit` | fractional time step, defaults 0 and 1 |
| `CALENDAR:` | `MAPL_ClockInit` | default `GREGORIAN` |
| `MAPL_ENABLE_TIMERS:` | `initialize_gc` | default `NO`; `MAPL_TIMER_MODE:` read when on |
| `MAPL_ENABLE_MEMUTILS:`, `MAPL_MEMUTILS_MODE:` | `initialize_gc` | memory-use reporting |
| `USE_SHMEM:` | `initialize_gc` | default 0 |
| `PRINTSPEC:` | `initialize_gc` | 0 off; >0: print the root's specs and skip initialize, run and finalize |
| `PERPETUAL_YEAR:`, `_MONTH:`, `_DAY:` | `initialize_gc` | default -999; enables the perpetual clock |
| `CoresPerNode:`, `USER_DIRPATH:` | `initialize_gc` | optional |
| `CAP_IMPORTS`, `CAP_EXPORTS`, `EXTDATA_IMPORTS` | `initialize_extdata` via `get_vec_from_config` | optional lists of component names whose exports are exposed to ExtData (not examined further) |

`NUM_SGMT:` is in the template but no `NUM_SGMT` read appears in Cap
(`CapGridComp.md` also says it is unused).
Keys read from other files by Cap: `RUN_DT:`, `NX:`, `NY:`, `DYCORE:`,
`REPLAY_MODE:` from the root rc; `EXPID:`, `EXPDSC:` from the History rc;
Cap writes `NX:`, `NY:`, `EXPID:` into the ExtData config.

### 5.2 Initialize

`initialize_gc` (timed as `Initialize`):
1. read the keys above, create `cap%clock` (`MAPL_ClockInit`; start time from
   `cap_restart` when the file exists, see below) and a copy `clock_hist`
   used for History and checkpoints;
2. load `ROOT_CF`, `HIST_CF`; `MAPL_AddChild` the root (by `set_services`
   or DSO `setservices_`), then `HIST` (`HIST_SetServices`, History
   [document](../History/ARCHITECTURE.md)), then load `EXTDATA_CF` and add
   `EXTDATA` (`ExtData2G_SetServices` if `USE_EXTDATA2G` else
   `ExtData1G_SetServices`);
3. `MAPL_Get` the children's gridcomps and import/export states; inject an
   externally set grid into the root if present;
4. if `PRINTSPEC > 0`: print and stop; else
   `ESMF_GridCompInitialize` the root, then `initialize_history` (all root
   exports are added to History's import state, then History is initialized),
   then `initialize_extdata` (ExtData's exports are matched to the root's
   imports, which are marked `RESTART = MAPL_RestartSkip`; then ExtData is
   initialized);
5. for `REPLAY_MODE: Regular`, the ExtData gridcomp and export state are
   stored in the GCM's internal state `ExtData_state` (type in
   `ExternalGCStorage.F90`).

The order root, History, ExtData is deliberate in the code: History and
ExtData need the root's states to exist.

### 5.3 Run

`run_gc` calls `run_MAPL_GridComp`, a loop `n = 1..nsteps`: stop early if
`clock_hist` is at its stop time (unless perpetual), then `step(phase)`.
One `step`:
1. (phase 1 only) `ESMF_GridCompRun` **ExtData** with `cap%clock`;
2. (phase 1 only) `ESMF_GridCompWriteRestart` on the **root** and then on
   **History**, with `clock_hist` (intermediate checkpoint hook; see 5.5);
3. `ESMF_GridCompRun` the **root** (phase `phase_`);
4. (last phase only) advance `clock` and `clock_hist`, update the perpetual
   clock if used, `ESMF_GridCompRun` **History** with `clock_hist`, print
   throughput.

So order per heartbeat: ExtData, restart write hooks, root, clock advance,
History. History sees the clock already advanced.

### 5.4 Finalize

`finalize_gc`: finalize root, History, ExtData (in that order); write
`cap_restart` with the time of `clock_hist` (`CAP_Finalize`, root rank only,
format `i8.8,1x,i6.6`); destroy the four configs; `MAPL_FinalizeShmem`;
create the empty egress file named by `final_file` (the `--egress_file`
option) on the root rank.

### 5.5 Checkpoints and restarts

- Start time: `MAPL_ClockInit` reads `cap_restart` (all ranks); if absent the
  `BEG_DATE` value is used. Cap writes `cap_restart` at finalize only.
- Component restart files: Cap does not write them itself. `MAPL_Generic`
  registers `MAPL_GenericRecord` as the `WRITERESTART` method of every MAPL
  component (`generic/MAPL_Generic.F90`), and Cap calls
  `ESMF_GridCompWriteRestart` each step on root and History; MAPL decides
  from `RECORD_FREQUENCY:` alarms in the component's rc (and
  `*_CHECKPOINT_FILE:` labels) whether to write.
  *Unverified:* whether a final checkpoint is written inside the root's
  finalize; not read for this document.

## 6. NUOPC wrapper

`MAPL_NUOPCWrapperMod` is a NUOPC model component: `SetServices` registers
IPDv05 phases for advertise (`advertise_fields`) and realize
(`realize_fields`), and specializes `SetClock`, `CheckImport`, `DataInitialize`,
`Advance` (`model_advance`) and `Finalize` (`model_finalize`). `initialize_p0`
creates a `MAPL_Cap` from `MAPL_CapOptions(cap_rc_file=...)` and keeps it in
the component's internal state `MAPL_Cap`. `add_wrapper_comp` adds such a
component to a NUOPC driver. Nothing in the GEOSgcm fixture was checked
for use of it.

## 7. Who uses it

- `src/Applications/@GEOSgcm_App/GEOSgcm.F90` (`program GEOS5_Main`):
  `MAPL_FargparseCLI()`, `MAPL_CapOptions(cli)`, `MAPL_Cap('GCM',
  ROOT_SetServices, cap_options=...)`, `cap%run()`.
- Stand-alone FV3 drivers: `StandAlone_AdvCore.F90`, `StandAlone_DynAdvCore.F90`,
  `StandAlone_FV3_Dycore.F90` (under `@FVdycoreCubed_GridComp`);
  `idfupd.F90` in the GEOSgcm application directory.
- `@GOCART/ESMF/UFS/Aerosol_Cap.F90` and neighbours reference `MAPL_Cap`
  (UFS-style caps; not examined further).
- Configuration: `src/Applications/@GEOSgcm_App/CAP.rc.tmpl` sets `ROOT_NAME: GCM`,
  `ROOT_CF: AGCM.rc`, `HIST_CF: HISTORY.rc`, `BEG_DATE`, `END_DATE`,
  `JOB_SGMT`, `NUM_SGMT`, `HEARTBEAT_DT` (from `@DT`), `USE_SHMEM`,
  `USE_EXTDATA2G: .TRUE.`, `MAPL_ENABLE_TIMERS: YES`,
  `MAPL_ENABLE_MEMUTILS: NO`, `PRINTSPEC: 0`. It does not set `EXTDATA_CF`
  (default `ExtData.rc` applies).

## 8. Known traps

- `HEARTBEAT_DT` must match `RUN_DT:` in the root rc (`ROOT_CF`) and in the ExtData rc when present (`initialize_gc`, "inconsistent values of HEARTBEAT_DT and RUN_DT").
- `USE_EXTDATA2G` defaults to `.false.` in code, so a CAP.rc without it
  selects ExtData first generation, whatever the build option says.
- Only run phase 1 runs ExtData; History runs only on the last phase.
- With `PRINTSPEC > 0` the model never runs, and no `cap_restart` is written.
- The time loop ends on `nsteps` or on `clock_hist` reaching the end date,
  whichever comes first.

## 9. Observations

- `CapGridComp.md` describes `USE_EXTDATA2G` without a default; the code
  default is `.false.`. It lists `NUM_SGMT` as unused by Cap, consistent
  with the code.
- `MAPL_CapGridComp.F90` mixes `MAPL_GetResource` reads (on Cap's MAPL
  object) with direct `ESMF_ConfigGetAttribute` reads of the same CAP.rc.
- `ExternalGCStorage.F90` has no comments; its only use found here is the
  replay-mode block of `initialize_gc`.
