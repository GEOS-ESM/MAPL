# pfio/: parallel client-server netCDF I/O (library `MAPL.pfio`)

**Audience:** coding agents and developers working on MAPL `pfio/`.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [../docs/ARCHITECTURE.md](../docs/ARCHITECTURE.md) (repository map; in the fixture, the parent
repository document is `../../../docs/ARCHITECTURE.md` from the repository root). Rules for agents: [../AGENTS.md](../AGENTS.md).

## Purpose
pFIO (parallel file input/output) is MAPL's netCDF I/O layer. Clients (the model processes) send metadata and data
messages to servers, which read or write netCDF files so that the model need not wait for I/O. History writes
through it and ExtData reads through it (`gridcomps/history/HistoryGridComp.F90`, `gridcomps/extdata/ExtDataFileReader.F90`:
both call `mapl_get_client`). The prose guides `pfio/pfio.md` and `pfio/README.md` are not repeated here.

## Directory and module map
Module names are `pFIO_<Name>Mod` (for example `pFIO_ClientManagerMod`) except `mapl_DefaultServerNames_mod` (`ServerNames.F90`) and `mapl_pfio_api` (`API.F90`).
| Group | Files | Role |
|---|---|---|
| Metadata | `FileMetadata`, `Variable`, `CoordinateVariable`, `Attribute`, `UnlimitedEntity`, `StringVariableMap`, `IntArray` | Description of a file: dimensions, variables, attributes (names, not netCDF ids). |
| File access | `NetCDF4_FileFormatter`, `NetCDF_Supplement`, `pfio_nf90_supplement.c`, `NetCDF4_get_var.H`, `NetCDF4_put_var.H` | Open/read/write through netCDF-Fortran; the `.H` files are typed include bodies; the supplement wraps calls missing from netCDF-Fortran. |
| Messages | `AbstractMessage` and the `*Message.F90` files (Terminate, Done, HandShake, AddReadDataCollection, AddWriteDataCollection, ModifyMetadata, ReplaceMetadata, Prefetch/Stage Data and Done, Collective variants, Forward) ; `MessageVisitor`, `ProtocolParser` | One type per request; dispatch through the `MessageVisitor` type. |
| Data references | `AbstractDataReference`, `ArrayReference`, `LocalMemReference`, `ShmemReference`, `RDMAReference` | Where message payload lives. |
| Transport | `AbstractSocket`, `MpiSocket`, `SimpleSocket`, `AbstractRequestHandle`, `AbstractDirectoryService`, `DirectoryService`, `MpiMutex` | Client-server connection; the directory service publishes and connects named ports. |
| Threads | `BaseThread`, `ServerThread`, `ClientThread`, `FastClientThread` | One server thread per connected client; client thread issues requests. |
| Servers | `AbstractServer`, `BaseServer`, `MpiServer`, `MultiLayerServer`, `MultiCommServer`, `MultiGroupServer` | See below. |
| Client registry | `ClientManager` | Module `pFIO_ClientManagerMod` with `add_client` and `get_client` only. |
| Names | `ServerNames.F90` | `MAPL_DEFAULT_INPUT_SERVER`, `MAPL_DEFAULT_OUTPUT_SERVER`. |
| Collections | `ExtDataCollection`, `HistoryCollection` | Per-client collection records (*Inferred* from names) |
| Containers | `*Vector`, `*Map`, `StringIntegerMapUtil`, `StringVectorUtil` | gFTL instantiations. |
| Umbrella | `pFIO.F90` (module `pFIO`), `API.F90` (module `mapl_pfio_api`), `pfio_base.F90`, `pFIO_Constants.F90`, `pFIO_Utilities.F90` | See "Public interfaces". |
| Programs | `programs/` | `pfio_open_close.F90`, `pfio_server_demo.F90`, `pfio_collective_demo.F90`, `pfio_writer.F90`. |
| Not compiled | `OpenMPServer.F90` (commented out of `srcs`) | Present in the directory only. |

### Servers
- `MpiServer`: server ranks receive and write.
- `MultiLayerServer`, `MultiCommServer`, `MultiGroupServer`: front end receives, back end writes (compiled; not named in the framework code below).
- The framework (`mapl/MaplFramework.F90`, `add_local_server`; `mapl/PfioServerGridComp.F90`, `run`) creates only `MpiServer` and
  `MultiGroupServer`, chosen by the `subclass` key of a `servers:` entry; any other value fails with "Unknown server subclass".
  See [../mapl/ARCHITECTURE.md](../mapl/ARCHITECTURE.md).

### Client registry
`ClientManager.F90` holds one module-private `StringClientThreadMap` named `client_map`. `add_client(name, client)` stores a client under a
server name; `get_client(name)` returns a pointer. There is no `ClientManager` type and no `i_Clients`/`o_Clients` pair on this tree.
Client operations are type-bound procedures of `ClientThread`: `collective_stage_data`, `collective_prefetch_data`, `stage_data`,
`prefetch_data`, `done_*`, `wait`, `wait_all`, `post_wait_all`, `modify_metadata`, `replace_metadata`, `terminate`
(pfio/ClientThread.F90). `FastClientThread` extends it (used for the default output client).

## Public interfaces
- `mapl_pfio_api` (API.F90), used by `mapl/MAPL.F90`: `mapl_FileMetadata`, `MAPL_Variable`, `mapl_StringVariableMap` and its iterator,
  `mapl_NetCDF4_FileFormatter`, `mapl_ArrayReference`, `MAPL_DEFAULT_INPUT_SERVER`, `MAPL_DEFAULT_OUTPUT_SERVER`, `mapl_get_client`,
  `ClientThread`, `mapl_pfio_read`, `mapl_string_in_stringvector`, `operator(==)`, `operator(/=)`.
- `pFIO` (pFIO.F90) re-exports the whole pFIO module set; `base/NCIO.F90` and the I/O code under `infrastructure/geom_io/` use it directly.
- Server start-up and connection from the framework: `MAPL_CreateServers`, `MAPL_RunServers`, `MAPL_ConnectToServer`, `MAPL_PublishServer`
  (mapl/MaplFramework.F90). pFIO provides `DirectoryService` (`publish`, `connect_to_server`, `connect_to_client`) for them.

## Dependencies
- Library `MAPL.pfio`, type SHARED. `DEPENDENCIES`: `MAPL.utils`, `MAPL.mp_utils`, NetCDF-Fortran, NetCDF-C. Public links: gFTL, gFTL-shared, pFlogger;
  private links: MPI, OpenMP (pfio/CMakeLists.txt).
- Used by (CMake): `MAPL` (`mapl/`), `MAPL.base`, `infrastructure/geom`, `infrastructure/geom_io`, `infrastructure/vertical/vertical`,
  `infrastructure/vertical/vertical_grid`, `infrastructure/geom/Mesh/raster_to_mesh`.
- Source users outside the library: `gridcomps/history`, `gridcomps/extdata`, `infrastructure/geom_io` (`GridPFIO.F90`, `FieldBundleRead.F90`, `GeomPFIO.F90`), `superstructure/generic/RestartHandler.F90` (`get_client` on the default output server).

## Build and tests
- `CMakeLists.txt` checks netCDF-C for `NC_HAS_QUANTIZE` and `NC_HAS_ZSTD` (defines `NF_HAS_QUANTIZE`, `NF_HAS_ZSTD`); ZSTD is forced off when `Baselibs_FOUND`.
  Defines `SUPPORT_FOR_MPI_ALLOC_MEM_CPTR` when that option is set, and `ODD_IFX_BUG` for `ArrayReference.F90` under IntelLLVM.
- `programs/CMakeLists.txt` builds `pfio_open_close.x`, `pfio_server_demo.x`, `pfio_collective_demo.x`, `pfio_writer.x`.
- `tests/CMakeLists.txt` (only when `PFUNIT_FOUND`, `EXCLUDE_FROM_ALL`): pFUnit target `MAPL.pfio.tests` (`Test_*.pf`, with mocks `MockClient*.F90`,
  `MockServer*.F90`, `MockSocket.F90`; `MAX_PES 8`; label `ESSENTIAL`). Executable `pfio_ctest_io.x` runs through the ctest tests
  `pFIO_tests_mpi`, `pFIO_tests_simple`, `pFIO_tests_hybrid`, `pFIO_tests_mpi_2comm`, `pFIO_tests_mpi_2group`, all labelled `PERFORMANCE`
  (12 to 24 processes each; `pFIO_tests_mpi_2layer` is commented out).

## Known traps
- Servers are looked up by name through `DirectoryService`; the default names are in `ServerNames.F90`.
- `pFIO.F90` still uses the module files of all server types; only two are reachable from the framework configuration.
- `pfio/pfio.md` describes command-line options `--npes_model`, `--npes_output_server`, `--oserver_type`. Those strings do not occur in any `.F90` or `.yaml` file on this tree (searched), so the options are those of the MAPL2 command-line interface; server configuration here is the `servers:` section of the application YAML. *Unverified:* which parts of the guide still apply.
- `SimpleSocket` stores a pointer to the client, so the client must live in stable storage before it is connected (comment in `MaplFramework` `add_local_server`).

## Comparison with MAPL2 (`origin/release/v2:pfio/ARCHITECTURE.md`)
- Carries over (checked in the files above): the layering metadata / file access / messages / transport / threads / servers; the visitor-style message
  handling; `MpiSocket` and `SimpleSocket`; `DirectoryService`; the `MultiLayerServer`, `MultiCommServer`, `MultiGroupServer` set; `OpenMPServer.F90` not compiled;
  the netCDF-C quantize and zstd probes.
- Differs: the `ClientManager` is a name-keyed registry of `add_client`/`get_client` (MAPL2: `i_Clients`/`o_Clients` with methods); `ServerManager` and
  the `--npes_*` options live in MAPL2 `base/` and `gridcomps/Cap` and are not in `pfio/` or this tree; the framework object `MaplFramework` creates servers.

## Observations (noted, not fixed)
- The MAPL2 option strings (`--npes_model`, `--npes_output_server`, `--oserver_type`) occur in `pfio/pfio.md` only; `pfio/README.md` has only generic wording. The `.png` figures are not checked.
