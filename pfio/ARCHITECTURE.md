# pfio: parallel client-server I/O (library `MAPL.pfio`)

**Audience:** coding agents and developers working on MAPL I/O.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `pfio/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
pFIO is MAPL's netCDF I/O layer. Clients (the model processes) send
metadata and data messages to servers (other processes or process groups) that
read or write netCDF files, so the model need not wait for I/O. History writes
through it; ExtData2G reads through it. Overview, command lines, tuning and
examples: [pfio.md](pfio.md) and [README.md](README.md) (not repeated here).

## Build
- Library `MAPL.pfio` (`esma_set_this (OVERRIDE MAPL.pfio)`), depends on
  `MAPL.shared MAPL.profiler NetCDF::NetCDF_Fortran NetCDF::NetCDF_C`; public
  links gFTL, gFTL-shared, pFlogger; private MPI and OpenMP
  (pfio/CMakeLists.txt).
- Probes netCDF-C for `NC_HAS_QUANTIZE` and `NC_HAS_ZSTD` and defines
  `NF_HAS_QUANTIZE` / `NF_HAS_ZSTD`; ZSTD is forced off when `Baselibs_FOUND`
  (pfio/CMakeLists.txt).
- Executables `pfio_open_close.x`, `pfio_server_demo.x`,
  `pfio_collective_demo.x`, `pfio_writer.x` are built beside the library.
  pFUnit tests in `tests/` only when `PFUNIT_FOUND` (EXCLUDE_FROM_ALL).
- Present but not compiled: `OpenMPServer.F90` (commented out of `srcs`),
  `pfio_parallel_netcdf_reproducer.F90`.
- The umbrella module is `pFIO` (pFIO.F90); modules are named `pFIO_<Name>Mod`.

## Structure
Layers, bottom up:
1. **Metadata**: `FileMetadata` (dimensions, variables, attributes) built from
   `Variable`, `CoordinateVariable`, `Attribute`, `UnlimitedEntity`,
   `StringVariableMap`. Holds names, not netCDF ids.
2. **File access**: `NetCDF4_FileFormatter` (open/create/read/write through
   netCDF-Fortran; `NetCDF4_get_var.H`/`NetCDF4_put_var.H` are type-generated
   includes), `NetCDF_Supplement` and `pfio_nf90_supplement.c` (calls missing
   from netCDF-Fortran, e.g. quantize/zstd).
3. **Messages**: `AbstractMessage` subclasses (Terminate, Done, HandShake,
   AddExtCollection, AddHistCollection, ModifyMetadata, ReplaceMetadata,
   Prefetch/Stage Data and Done, Collective variants, ForwardData). Dispatch is
   a visitor: `MessageVisitor` has one `handle` per message type, implemented
   by `ServerThread` and `ClientThread` (ServerThread.F90, ClientThread.F90).
   Payload memory is an `AbstractDataReference`: `ArrayReference`,
   `LocalMemReference`, `ShmemReference`, `RDMAReference`.
4. **Transport**: `AbstractSocket` with `MpiSocket` (client-server over MPI)
   and `SimpleSocket` (same-process); `AbstractRequestHandle` for waits.
   `DirectoryService` (`AbstractDirectoryService`, `MpiMutex`) lets clients
   and servers find each other through MPI one-sided windows
   (DirectoryService.F90).
5. **Threads**: `BaseThread` -> `ServerThread` (one per connected client,
   executes messages) and `ClientThread` -> `FastClientThread` (copies data
   before send; selected by Cap option `--fast_oclient`).
6. **Servers**: `AbstractServer` -> `BaseServer` -> the concrete servers.
7. **Client side**: `ClientManager` owns a vector of `ClientThread`s (one per
   server) and exposes `add_hist_collection`, `add_ext_collection`,
   `collective_stage_data`, `collective_prefetch_data`, `done_*`, `wait`,
   `post_wait`, `split_server_pools`, `set_optimal_server`. Two module-level
   instances exist: `i_Clients` (input) and `o_Clients` (output)
   (ClientManager.F90).

## Servers
| Type | Idea (from module code and pfio.md) |
|---|---|
| `MpiServer` | Server ranks do receive and write; default type `single`. |
| `MultiLayerServer` | Front-end ranks receive, back-end ranks write. |
| `MultiGroupServer` | Captain/soldier back end: front end receives, back-end groups write whole collections; type `multigroup`. |
| `MultiCommServer` | Back end in separate communicators; type `multicomm`. |
| `OpenMPServer` | Not compiled (see Build). |
Selection is in `ServerManager` (base/ServerManager.F90, `initialize`): type
`multicomm`, `multilayer`, `multigroup`, else `MpiServer`. Assertions there:
multilayer/multigroup need at least two backend processes
(`nodes * npes_backend_pernode`), multicomm at least one.

## How a GEOS run configures output servers
1. `gcm_run.j` (src/Applications/@GEOSgcm_App/gcm_run.j) reads `IOSERVER_NODES`
   and `NUM_BACKEND_PES` from `AGCM.rc`, and `USE_IOSERVER` from the template.
   When `USE_IOSERVER == 1` it adds output-server nodes to the node count and
   passes `--npes_model $MODEL_NPES --nodes_output_server $NUM_OSERVER_NODES
   --oserver_type multigroup --npes_backend_pernode $NUM_BACKEND_PES`;
   otherwise no server options (`IOSERVER_OPTIONS`, `IOSERVER_EXTRA`).
2. `FargparseCLI` (gridcomps/Cap/FargparseCLI.F90) defines the options:
   `--npes_model`, `--npes_input_server`, `--npes_output_server`,
   `--nodes_input_server`, `--nodes_output_server` (vectors; one entry per
   server group), `--oserver_type` (default `single`), `--npes_backend_pernode`
   (default 0), `--compress_nodes`, `--fast_oclient`, `--one_node_output`.
   Giving both `--npes_output_server` and `--nodes_output_server` is
   asserted against. `FlapCLI.F90` is the alternative parser.
3. They land in `CapOptions` (gridcomps/Cap/CapOptions.F90); defaults
   `oserver_type='single'`, server sizes `[0]`.
4. `MAPL_Cap` `initialize_io_clients_servers` forwards them to
   `ServerManager%initialize` (gridcomps/Cap/MAPL_Cap.F90), which splits the
   communicator into `model` and server groups, creates
   `DirectoryService`, and builds the input and output servers. `run_member`
   runs the model only on the `model` communicator.
See the Cap document: [../gridcomps/Cap/ARCHITECTURE.md](../gridcomps/Cap/ARCHITECTURE.md).

## Who uses it
- History: `use pFIO_ClientManagerMod, only: o_Clients`; calls
  `add_hist_collection`, `split_server_pools`, `set_optimal_server`,
  `done_collective_stage`, `post_wait`
  (gridcomps/History/MAPL_HistoryGridComp.F90).
- ExtData2G: `use pFIO_ClientManagerMod, only : i_Clients`; calls
  `done_collective_prefetch`, `wait` (gridcomps/ExtData2G/ExtDataGridCompNG.F90).
- Via [../griddedio/ARCHITECTURE.md](../griddedio/ARCHITECTURE.md) for both.
- `MAPL.base` links `MAPL.pfio` (base/CMakeLists.txt); `ServerManager` is in base/.
- Document links: [../gridcomps/History/ARCHITECTURE.md](../gridcomps/History/ARCHITECTURE.md),
  [../gridcomps/ExtData2G/ARCHITECTURE.md](../gridcomps/ExtData2G/ARCHITECTURE.md).

## Known traps
- `ServerManager` is in `base/`, not `pfio/`; server choice is a string.
- multigroup/multilayer abort at start-up with fewer than two backend
  processes; with `nodes_output_server` the count is nodes times
  `npes_backend_pernode` (base/ServerManager.F90).
- Output is asynchronous: a `post_wait`/`wait` is needed before reusing buffers
  (unless `--fast_oclient` copies).
- Both `i_Clients` and `o_Clients` are module-level singletons.
- Collections are addressed by integer id returned from `add_*_collection`.

## Observations (noted, not fixed)
- `pfio/README.md` names the "Simple Server" and "MultiGroupServer"; the code
  also provides `multilayer` and `multicomm` types, and `OpenMPServer.F90` is
  present but disabled in CMake.
- `pfio/README.md` describes the Simple Server as compute and I/O nodes overlapping.
- `pfio/README.md` links a GitHub `main` tree for History; not checked.
