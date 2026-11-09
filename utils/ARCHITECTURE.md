# utils/: MPI-free foundations, constants, interpolation, regex, udunits2f (libraries `MAPL.utils`, `MAPL.constants`, `udunits2f`)

**Audience:** coding agents and developers working on MAPL `utils/`.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [../docs/ARCHITECTURE.md](../docs/ARCHITECTURE.md) (repository map; in the fixture, the parent
repository document is `../../../docs/ARCHITECTURE.md` from the repository root). Rules for agents: [../AGENTS.md](../AGENTS.md).

## Purpose
`utils/` is the lowest layer: error handling, small operating-system and string utilities, date/time parsing, sorting and
hashing, physical constants, linear interpolation, a regular-expression wrapper, and a Fortran interface to the udunits2 library.
The CMake comment calls it "MPI-free foundational utilities (Tier 1 in MAPL v3 restructuring)" and says the library "must NOT depend on MPI"
(utils/CMakeLists.txt). The MPI-dependent counterpart is [../mp_utils/ARCHITECTURE.md](../mp_utils/ARCHITECTURE.md).

## Directory and module map
Three library targets are built from this directory tree.
| Target | Sources | Notes |
|---|---|---|
| `MAPL.utils` | all top-level `*.F90`/`*.c`, `Interp/`, plus `regex/` (added with `target_sources`) | The main library. |
| `MAPL.constants` | `Constants/` | Own `esma_add_library`; no other MAPL dependency listed. |
| `udunits2f` | `udunits2f/` | Own library; depends on `MAPL.utils`; links `udunits::udunits` and `EXPAT::EXPAT`. |

| Group | Files (module) |
|---|---|
| Umbrella | `API.F90` (`mapl_utils_api`): re-exports with `mapl_` prefixes; its comments say it imports only the intended public symbols. |
| Errors | `ErrorHandling.F90` (`mapl_ErrorHandling_mod`), `MAPL_Throw.F90`; public `MAPL_Assert`, `MAPL_Verify`, `MAPL_Return` (utils/API.F90). The `_RC`, `_ASSERT`, `_RETURN` macros are in `include/MAPL.h`, which source files include. |
| OS and files | `OS.F90` (`mapl_os_mod`: directory and path helpers), `FileSystemUtilities.F90`, `MAPL_DirPath.F90`, `DSO_Utilities.F90` (shared-object name handling; uses `SYSTEM_DSO_SUFFIX`) |
| Strings | `String.F90`, `StringUtilities.F90`, `StringDictionary.F90`, `KeywordEnforcer.F90` (the `unusable` sentinel type) |
| Numeric | `MAPL_Hash.F90`+`hash.c`, `MAPL_Sort.F90`+`sort.c`, `MAPL_MinMax.F90`, `MAPL_Range.F90`, `ShaveMantissa.c`/`.h`, `Interp/` |
| Time | `TimeUtilities.F90`, `MAPL_ISO8601_DateTime.F90`, `MAPL_DateTime_Parsing.F90`, `MAPL_CF_Time.F90`, `MAPL_Sleep.F90` |
| Other | `MemInfo.F90`, `Validation.F90`, `MAPL_ESMF_InfoKeys.F90` (keys for `ESMF_Info` objects) |
| `Constants/` | `InternalConstants`, `MathConstants`, `PhysicalConstants`, `EarthConstants`, `EarthAtmosphericConstants`, and `Constants.F90` (module `MAPL_Constants`) |
| `Interp/` | `Interp.F90` (module `mapl_Interp_mod`, interfaces) and `Interp_implementation.F90` (a submodule); linear interpolation specifics named `INTERP_LIN_<ranks>_<n>` |
| `regex/` | `regex_F.c`, `regex_module.F90` |
| `udunits2f/` | `udunits2f.F90`, `UDSystem.F90`, `CptrWrapper.F90`, `encoding.F90`, `interfaces.F90`, `status_codes.F90`, `ut_set_ignore_error_message_handler.c` |

## Public interfaces
`use MAPL` brings in `mapl_Constants` (module `MAPL_Constants`, `Constants/Constants.F90`) and `mapl_utils_api` (mapl/MAPL.F90).
`utils/API.F90` is `private` by default. Examples of what it exports: the error routines, `mapl_KeywordEnforcer`, the `mapl_*` OS routines
(`mapl_MakeDirectory`, `mapl_PathJoin`, ...), `mapl_PackDate`/`mapl_UnpackDate`, the ISO 8601 types, `mapl_MemInfo`/`mapl_MemInfoWrite`, plus whole
modules for hash, min/max, range, sort and interpolation. The `public` list in the file is the source of truth.

## Dependencies
- `MAPL.utils` `DEPENDENCIES`: `MAPL.constants`, ESMF, gFTL-shared (v1 and v2), pFlogger; OpenMP private (utils/CMakeLists.txt).
  Despite "MPI-free", the library depends on ESMF.
- Used by (CMake): `MAPL.mp_utils`, `MAPL.pfio`, `MAPL.base`, `enums`, `superstructure/generic`, `superstructure/state`, and most `infrastructure/` libraries.
  `MAPL.constants` is used by `MAPL.mp_utils`, `MAPL.base`, and `MAPL.utils`. `udunits2f` is used by `superstructure/generic`, `infrastructure/field`, `infrastructure/vertical/vertical`.

## Build and tests
- Option `USE_CODATA_2018_CONSTANTS` (default OFF) defines `CODATA_2018_CONSTANTS` for `MAPL.constants` (utils/Constants/CMakeLists.txt).
- Tests (only when `PFUNIT_FOUND`, `EXCLUDE_FROM_ALL`): `utils/tests/CMakeLists.txt` defines `MAPL.utils.tests` (label `ESSENTIAL`, `MAX_PES 2`),
  linking `MAPL.utils`, `MAPL.mp_utils`, `MAPL.constants`. Files: `test_String`, `test_StringUtilities`, `test_DSO_Utilities`,
  `test_FileSystemUtilities`, `test_MAPL_DateTime_Parsing`, `test_MAPL_CF_Time`, `test_OS`, `test_sort`, `test_DirPath`, `Test_MaxMin`, `Test_AreaMean`, `Test_Partition`;
  `Test_MemInfoWrite` only on Linux. `test_StringDictionary.pf` and `test_MAPL_ISO8601_DateTime.pf` are in the directory; the list in CMake does not name them
  (the ISO 8601 entry is commented out).
- `udunits2f/tests`: `udunits2f.tests` (label `ESSENTIAL`; the CMake comment says the directory is added only when pFUnit is found, and refers to a GNU test fix).
- `ErrorHandling`, `ShaveMantissa.c` and `Interp` have no dedicated test file in this directory.

## Known traps
- `Test_MaxMin.pf`, `Test_AreaMean.pf`, `Test_Partition.pf` test modules that are in `mp_utils/` (`ArrayReductions`, `Partition`); they are built into `MAPL.utils.tests` because that target links `MAPL.mp_utils`.
- The ifx 2025.2 workaround escapes quotes in `SYSTEM_DSO_SUFFIX` (utils/CMakeLists.txt).
- The regex code is compiled into `MAPL.utils`, not its own library.

## Observations (noted, not fixed)
- `utils/API.F90` imports `mapl_TimeUtilities_mod` twice (once with `only:` renames, once whole); the comment above the ISO 8601 imports mentions avoiding a name conflict.
- `MemInfo.F90` has `use mpi` although the layer is described as MPI-free (utils/MemInfo.F90, utils/CMakeLists.txt comment).
