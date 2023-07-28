# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

### Changed

### Fixed

### Removed

### Deprecated

## [2.40.0] - 2023-07-29

### Added

- Add ability of ExtData to fill variables on MAPL "tile" grids.
- Added print of regrid method during History initialization
- Added ability to use an `ESMF.rc` file to pass in pre-`ESMF_Initialize` options to ESMF (see [ESMF Docs](https://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/node4.html#SECTION04024000000000000000) for allowed flags.
  - NOTE: File *must* be called `ESMF.rc`
- Added ability to run ExtDataDriver.x on a MAPL "tile" grid
- Add ability to introduce a time-step delay in ExtDataDriver.x to simulate the timestep latency of a real model
- Added a MAPL\_Sleep function, equivalent to some vendor supplied but non-standard sleep function
- sampling IODA file with trajectory sampler (step-1): make it run
- Convert ExtData to use ESMF HConfig for YAML parsing rather than YaFYAML
  - Set required ESMF version to 8.5.0
- Add StationSamplerMod for station sampler
- Added ReplaceMetadata message and method to replace oserver's metadata
- Added field utilities to perform basic numeric operations on fields
- Update arithemetic parser to work with any rank and type of ESMF fields
- For ExtDataDriver.x only, added logging config to Tests/ExtDataDriverMod.F90 to enable Logger there
- Added new fill option and run mode for ExtDataDriver.x

### Changed

- Updates to GFE library dependency
  - Require gFTL v1.10.0
  - Require gFTL-shared v1.6.1
  - Require fArgParse v1.5.0
  - Require pFlogger v1.9.5
  - Removed yaFyaml as dependency
- Updated programs using FLAP for command line parsing to use fArgParse instead
- Updated `components.yaml` to use Baselibs 7.14.0
  - ESMA_env v4.9.1 → v4.19.0
    - Baselibs 7.14.0
      - esmf v8.5.0
      - GFE v1.11.0
      - curl 8.2.1
      - HDF5 1.10.10
      - netCDF-C 4.9.2
      - netCDF-Fortran 4.6.1
      - CDO 2.2.1
      - NCO 5.1.7
    - Move to MPT 2.28 at NAS, and other various changes for TOSS4 at NAS
    - Remove Haswell from `build.csh`
  - ESMA_cmake v3.28.0 → v3.31.0
    - Clean up for TOSS4 changes at NAS
    - Add `QUIET_DEBUG` flag
    - Suppress some common warnings with Intel Debug
- Make the GEOSadas CI build separate as it often fails due to race conditions in GSI
- Update CI to use BCs v11.1.0 and Baselibs 7.14.0
- Updates to support building MAPL with spack instead of Baselibs
  - Add `FindESMF.cmake` file to `cmake` directory (as it can't easily be found via spack)
  - Move `CMAKE_MODULE_PATH` append statement up to find `FindESMF.cmake` before we `find_package(ESMF)`
  - Default `BUILD_WITH_FLAP` to `OFF` as we don't build it in spack
  - Explicitly build GEOSadas in CI with `-DBUILD_WITH_FLAP=ON` as GEOSadas is still behind in moving to use fArgParse

### Fixed

- Created cubed-sphere grid factory with files split by face
- Removed unneeded and confusing default in History Grid Comp (see #2081)
- Fixes in CMake for fArgParse transition

### Deprecated

- Deprecate the use of FLAP for command line parsing in favor of fArgParse. FLAP support will be removed in MAPL 3

## [2.39.7] - 2023-07-18

### Fixed

- Fix a bug so that MultigroupServer does not allow a file written by multiple processes at the same time.

## [2.39.6] - 2023-07-18

### Changed

- Relaxed restriction in the tripolar grid factory so that grids can be made even when the decomposition deos not evenly divide the grid dimension so that the factory can be used in utilities where the core count makes such a condition impossible to satisfiy

### Fixed

- Fix a bug in `time_ave_util.x` so that it can work with files with no vertical coordinate

## [2.39.5] - 2023-07-10

### Fixed

- Fixed logic in generating the names of the split fields. If the alias field in the History.rc has separators (;), each substring is used to name the resulting fields. If there are no separators, this will be the exact name of the first split field

## [2.39.4] - 2023-06-23

### Fixed

- Added bug fix when using climatology option in ExtData2G under certain scenarios

## [2.39.3] - 2023-06-12

### Fixed

- Fixed a bug when performing vertical regridding in History when the output grid cannot be decomposed so that every core has a DE

## [2.39.2] - 2023-05-30

### Fixed

- Fix unintentional PFLOGGER requirements in geom and pfio

## [2.39.1] - 2023-05-16

### Fixed

- Fixed the handling of ungridded dims for averaged collections. When the grid rank is 2, the code erroneously assigned vertical dimension to the ungridded dims

## [2.39.0] - 2023-05-12

### Added

- Added minimal support for NetCDF "groups".    `getvar*` can now optionally specify a `group_name` and the data will be returned from the named group rather than the default group.

- New feature to allow for single component testing as code is refactored
  - Created `Comp_Testing_Driver.F90` in Apps
  - Created `subset_fields.py` to grab a subset of columns
  - Created shell script to perform subsetting, run the driver, and evaluate differences
  - Instructions for use can be viewed under Apps/Comp_Testing_Info.md
- Added module to process datetime strings from NetCDF to ESMF\_Time and ESMF\_TimeInterval variables and vice versa

### Fixed

- `READ_RESTART_BY_FACE` has been fixed and now can read restarts from full CS grids as well as restarts that have been separated by face via `WRITE_RESTART_BY_FACE`.    The current implementation requires that both `num_readers` and `num_writers` must be multiple of 6.
- 'config.py' has been fixed. python3 requires // for integer divides.

## [2.38.1] - 2023-05-10

### Added

- Added new option "RUN_AT_INTERVAL_START:" so force component alarms ring at start of the interval, default still to ring at the end

## [2.38.0] - 2023-05-01

### Added

- Printed time to write files to the disk in MultiGroupServer
- Added merge function to Filemetadata to merge two meta objects
- Added support for "DEPENDS_ON" and "DEPENDS_ON_CHILDREN" for export_specs. The typical usage on this feature is when the calculation of a variable involves other export variables, either from the same component (DEPENDS_ON specifies the list on such variables), or in the children (in this case the expectation is that all of the children have the SAME export). In both cases MAPL performs automatic allocation of these export variables.
- Added support for use of pFlogger simTime in logging (only if `-DBUILD_WITH_PFLOGGER=ON`)
  - Note: Due to bug in pFlogger v1.9.3 and older, you *must* specify a `dateFmt` in your logging configuration file in the
    formatter when using `simTime` (see [pFlogger issue #90](https://github.com/Goddard-Fortran-Ecosystem/pFlogger/issues/90)).
    This is fixed in pFlogger v1.10.0
- Add geom subdirectory and contents for MAPL Geom framework

### Changed

- Add logic to GriddedIO class so that if an uninitialized time object is passed, the resulting file will not depend on time
- Make sure the pfio files are closed when the time and speed appear in logfile
- Enable more tests in CircleCI

### Fixed

- Move ioserver_profiler%stop call to the right place
- Caught an untrapped error condition when writing to NetCDF

## [2.37.3] - 2023-04-25

### Fixed

- Fixed the handling of vector regridding for a subdomain with no-DE.
- Fixed the no-DE case related to vertical regridding.

## [2.37.2] - 2023-04-17

### Fixed

- Pulled call MAPL_GridGetCorners out of the condition to avoid hang in MAPL_GetGlobalHorzIJIndex

## [2.37.1] - 2023-04-14

### Fixed

 - Various nonstandard uses of Fortran caught by NAG compiler
 - Various workarounds for NAG 7.1.113 where the compiler is wrong.

## [2.37.0] - 2023-04-03

### Added

- New feature in History to allow the accumulation in non-instantaneous collections to be reset independently of the writing frequency if requested.
- new macro `_RETURN_IF(cond)` to provide a succinct early return in procedures with return codes.

### Fixed

- Fixed message in _ASSERT statement in ESMFL_Mod.F90
- Fixed bug in CubedSphereGridFactory when constructing a grid from a file
- Cleaned up cubed-sphere grid factory and NCIO to produce files with consistent capitalization and types for the stretching factor

## [2.36.0] - 2023-03-23

### Added

- Added config array overload to `MAPL_GetResource`
- Implemented new generic XY grid factory to create regional grids on any input set of 2D lons and lats
  - **NOTE**: This grid factory is experimental and the API may change or it might be superseded by another grid factory

### Changed

- Updated `components.yaml` to match GEOSgcm v10.25.1
  - ESMA_env v4.8.0 → v4.9.1 (Move to Baselibs 7.8.1: ESMF v8.4.1)
  - ESMA_cmake v3.24.0 → v3.28.0 (Detection of additional sites, updated Intel Fortran flags, updates for Python3 support)
- Converted files in `Python/MAPL` to Python 3.
  - **NOTE 1**: This will require changes to codes that call MAPL's Python layer.
  - **NOTE 2**: If building with F2PY support, you will need to use ESMA_cmake v3.28.0 or later if using in a mixed Python 2/3 environment.

## [2.35.3] - 2023-03-17

### Fixed

- Fixed bug in climatologies with ExtDataV2 when wrapping around the year

### Added

- Added Ford documentation

## [2.35.2] - 2023-03-06

### Changed

- Updated ExtData test case2 to handle a wider range of test times for more robust testing

### Fixed

- Fixed bug in climatology hanlding of data in ExtDataV2 that occurred depending on the start date of the application relative to the length of the application run time
- Fixed set_grid. The cf_root is not created when it is called. Instead, use config from CAP.rc

### Removed

- Removed test case 2 for ExtDataV1 as there as an apparent bug in climatologies that has been fixed for ExtDataV2 but there appears to be a different but in ExtDataV1 that causes the extended case 2 to fail

## [2.35.1] - 2023-03-01

### Fixed

- Fix issue when running with monthly history collections

## [2.35.0] - 2023-03-01

### Added

- Added subroutines to read char type in Netcdf
- Added a subroutine add_variable to Netcdf4_Fileformatter
- Add a function to get the area of a spherical polygon to the spherical geometry module
- Created layout independent version of the "DownBit"/"pFIO_ShaveMantissa" routines when running in MPI codes
- Added subroutine `MAPL_SunGetLocalSolarHourAngle()` in `base/MAPL_sun_uc.F90`. This
  provides a convenient local solar hour angle diagnostic which will be used to detect local
  solar noon via the `EXAMPLE OF USE` in the subroutine header. See `DESCRIPTION` in code
  for more details. Provides the TRUE local solar hour angle (i.e., with equation of time
  included), but can also provide the MEAN value (without EOT) via `FORCE_MLSHA=.TRUE.`
  optional argument.
- Add `shavemantissa` f2py code. This is used by AeroApps.
  - NOTE: If you do not have a need for this code, build with `-DUSE_F2PY=OFF`. Note that even if you try to build the f2py code, it might fail anyway due to issues with the Python stack on the machine. ESMA_cmake has code that "tests" if f2py works. If it doesn't, it should failover gracefully.

### Changed

- Changed set_grid method so users have a chance to specify the grid type
- Renamed `get_regrid_method` and `translate_regrid_method` to `regrid_method_string_to_int` and `regrid_method_int_to_string`
  respectively in `RegridMethods.F90`. This was done so we could add `get_regrid_method` to the AbstractRegridder. The new names
  more accurately reflect what the RegridMethods functions do.
- Changed call to `MAPL_SunOrbitCreate()` inside `MAPL_Generic.F90` to call to new function
  `MAPL_SunOrbitCreateFromConfig()`, the latter which get the orbital parameters from the MAPL
  state's Config. In this way no default orbital parameter values need appear in `MAPL_Generic.F90`.
  Rather, these default values are encapsulated where they belong in `Sun_Mod` in `base/MAPL_sun_uc.F90`
  and are now explicitly named and commented on at the head of the module. This is a structural
  zero-diff change.
- Created `MAPL.profiler` logger and moved throughput, per-component, and global timers to use it
- Moved most of the MAPL_GetResource generic subroutine to a new module, MAPL_ResourceMod, in base.
  The specific subroutines remain in MAPL_GenericMod to maintain the interface in one module, but
  most of the functionality is in MAPL_ResourceMod now.
- Update "build like UFS" CI test
- Converted the History Gridded Component to use `_RC` and `_STAT` macros

### Fixed

- Changed the type of output counters to INT64 for large file.
- Tested optional arguments arrdes in MAPL_WriteVars
- Added the correct values to halo corner of LatLon grid
- Fixed range in halo of LatLonGridFactory
- Corrected issue with native output having metadata saying it was bilinearly regridded. Now sets these files to have
  `regrid_method: identity`
- Fix bug in `mapl_acg.cmake` that caused unnecessary rebuilds
- Fixed error handling for refactored MAPL_GetResource

## [2.34.3] - 2023-02-14

### Added

- Added test cases 28 and 29 to ExtData testing framework

### Fixed

- Fix bug in ExtData2G when used in "Replay" in the GEOSgcm

## [2.34.2] - 2023-01-19

### Fixed

- Fixed bug with ExtDataDriver.x when enabling oserver on dedicated resources

## [2.34.1] - 2023-01-13

### Fixed

- Fixed bug when writing 4D fields to checkpoint files with the PFIO server via the WRITE_RESTART_BY_OSERVER option

## [2.34.0] - 2023-01-05

### Added

- Added `MAPL_find_bounds => find_bounds` and `MAPL_Interval => Interval` to `MAPL.F90` for use when doing component level OpenMP
- Added requirement for ESMF 8.4.0 in `find_package()` call
- Modified Apps/MAPL_GridCompSpecs_ACG.py to use the * capability for `LONG_NAME` like `SHORT_NAME`
- Added CMake code to apply stricter debug flags when building MAPL as Debug
- Added subroutine MAPL_MethodAdd to MAPL_Generic.F90
- Added subroutines get_callbacks and copy_callbacks to OpenMP_Support.F90
  - These added subroutines are to support "callback" procedures when inside OpenMP parallel region for mini states for component level threading.
- Added ability to expand "%d" in the long name when we split fields for History

### Changed

- Update `components.yaml`
  - ESMA_cmake v3.24.0 (defines stricter debug flags for Intel, preliminary support for `ifx`)
- Reduced amount of CI tests to reduce cost
- Added `message` to label enforcer (requires v3)
- Fixed the naming convention of the split field name (#1874)
  - NOTE: This could change the name of any field in HISTORY using field splitting. The data will be the same, but the name will be
    different.

### Fixed

- Fixed procedure "find" in CFIOCollection.F90 that was missing a `_RETURN(_SUCCESS)` at the end

## [2.33.0] - 2022-12-08

### Added

- Added new test case for ExtData2G to test multiple input files

### Fixed

- Fixed bug in tutorial case parent_no_children when attempting recommended exercises
- Implemented workaround for gfortran runtime issue with YafYaml when parsing multiple ExtData yaml files for ExtData2G

## [2.32.0] - 2022-12-02

### Added

- Add fArgParse CLI argument parser
- Added subroutines for reading 4d integers in NetCDF4_FileFormatter
- Added new option to allow for gaps in datasets ingested by ExtDataNG

### Changed

- Updated to ESMA_cmake v3.21.0
  - Adds support for a generic `x86_64` processor for GNU
- Updated to ESMA_env v4.8.0
   - Baselibs v7.7.0
      - fArgParse v1.4.1
      - pFUnit v4.6.1
- Move to use GitHub Actions for label enforcement

## [2.31.0] - 2022-11-28

### Fixed

- Fixed the subroutine that removes a variable from FileMetadata Object
- Defined real64 constants in consistent way
- Workarounds for NVIDIA compiler

### Added

- Added a function to remove a variable from FileMetadata object.
- Added assert to NetCDF4_get_var.H to print variable name if data retrieval fails
- Added to asserts in `NetCDF4_get_var.H` to print out file if there are error getting variables
- Added capability to Automatic Code Generator to detect inconsistencies in spec files at build time
- Add support for netCDF quantize.
  - Note this requires netCDF-C v4.9.0 and netCDF-Fortran v4.6.0, but our CMake does have a test
    to make sure the netCDF library was compiled with support. This test uses `NETCDF_INCLUDE_DIRS` and as such
    within GEOS requires the use of ESMA_cmake v3.20.0 or later to (possibly) succeed.
- Added subroutine to calculate IJ indexes of given lats lons for cubed-sphere grid

### Changed

- Updated to ESMA_cmake v3.20.0
  - This is to support the `NETCDF_INCLUDE_DIRS` used in the quantize support test
- Updated to ESMA_env v4.7.0
  - This has many updates including moving to netCDF-C v4.9.0 and netCDF-Fortran v4.6.0. The other updates:
    - Baselibs v7.6.0
      - ESMF v8.4.0
      - zlib 1.2.13
      - curl 7.86.0
      - netCDF-C 4.9.0
      - netCDF-Fortran 4.6.0
      - NCO 5.1.1
      - CDO 2.1.0
- Removed some unneeded `use` statements in `ExtDataGridCompNG.F90`. This seemed to let this new quantize support build with Intel
- Changed `nbits` internally to be `nbits_to_keep`. Note that externally, you still use `nbits:` in `HISTORY.rc`

## [2.30.3] - 2022-11-15

### Fixed

- Fixed bug when writing fields in History with ungridded dimension when the split option is not requested

## [2.30.2] - 2022-11-09

### Fixed

-  When a grid is received from outside of MAPL (e.g., NOAA UFS), MAPL must provide a mechanism to specify a grid-type for internal processing.
Add an option to set_grid to set GridType explicitly.

## [2.30.1] - 2022-11-07

### Fixed

- Fix for building `time_ave_util.x` at NAS using MPT

## [2.30.0] - 2022-11-03

### Added

- Introduced a new time averaging utility code to perform time averages of datasets

### Changed

- Moved Regrid_Util.F90 to the Apps directory

## [2.29.0] - 2022-10-28

### Fixed

- Add define for `-Dsys${CMAKE_SYSTEM_NAME}` to fix build issue with macOS and Intel (#1695)
- Fix handling of return macros for programs and subroutines (#1194)

### Added

- Add Coupled MOM6 GCM run to CI (ifort only)
- Added ability to pass in extra options to FLAP CLI arguments
- Added option to pass vertical metadata to FieldBundleWriter class for output file
- Expose FileMetaUtilities class MAPL_VerticalDataMod via "use MAPL"

### Changed

- Change argument arrdes of MAPL_Read and Write NC Var 3d to optional
- Error codes in `shared/MAPL_Error_Handling.F90` are now consistent with `_FAILURE = 1` in `include/MAPL_ErrLog.h`
- Updated `components.yaml
  - ESMA_env v4.4.0 → v4.5.0 (Update for GMI builds)
  - ESMA_cmake v3.18.0 → v3.19.0 (Add `-save-temps` to GNU debug, check if install prefix is writable)

## [2.28.0] - 2022-10-17

### Fixed

- Fix checkpoint `target_lon` and `target_lat` to be in degrees rather than radians

### Added

- Implemented create from filemetdata for Tripolar Grid Factory and add corners to output when writing files on Tripolar grid
- Added `Dockerfile` and CircleCI workflow to build and push container to both Docker Hub and GitHub Container Registry on
  MAPL releases. Uses a GitHub Action to trigger CircleCI pipeline on releases.

### Changed

- Removed URLs and emails from MAPL per request of NOAA.
  - Changed the default `Contact:` in History Gridded Component to be blank. To keep metadata the same in History
    output, please set `CONTACT:` in your `HISTORY.rc` file.
- Reorganized the file `Tests/pfio_MAPL_demo.F90` to mimic the steps of `MAPL_Cap`.
- Extend ACG
  - Add options (`MAPL_DataSpec`) / columns (`read_specs`)
  - Add constants for literals for uniformity (`CATEGORIES`, `ALLOC`, `DELIMITER`, `TERMINATOR`)
  - Fix lookups in aliases (dict) to find key (alias) or value (alias target) to allow unaliased options are possible
  - Add the `alloc` option to `emit_get_pointers`
  - Update outdated comments

## [2.27.1] - 2022-10-04

### Fixed

- Fixed a missing parentGC argument to addChildFromDSO

## [2.27.0] - 2022-10-03

### Fixed

- Removed ESMFL_UnitsRadians from ESMFL_Mod.F90 and replaced it with MAPL_UnitsRadians in InternalConstants.F90. Did a global search/replace. This avoid the circular dependency build error when ESMF_UnitsRadians is used in MaplGrid.F90

### Added

- Changes consistent with MAPL for accessor functions to get number of threads and current thread
- Several changes to support component level hybrid MPI/OpenMP.
  - See files OpenMP_Support.F90, EntryPointVector.F90, RunEntryPoint.F90, MaplGenericComponent.F90, MAPL_Generic.F90, and BaseProfiler.F90. The first three are new and the rest  are modified.
- Implement ISO 8601 (Date/Time) support for MAPL
- Added ability to apply masks to expressions in ExtData2G
- Added a subroutine that adds all exports from a child

## [2.26.0] - 2022-09-16

### Fixed

- Change the logic to check if the field is already connected to a valid grid. If yes, we bypass the checks for tilegrid (issue #1654)
- Removed unnecessary DSO extension assert
- Fixed bug that required a /dev/null ExtData entry to still have a file variable name
- Fixed bug with checking for duplicate alias in collection
- Added protection in History to only allow `instantaneous` or `time-averaged` modes
- Check userRC after ESMF_GridCompInitialize in MAPL_GenericInitialize

### Added

- Added Ninja build of MAPL to CI tests

### Changed

- Have `MAPL_AddChildFromDSO` call `MAPL_AddChildFromDSOMeta` (#1598)

### Removed

- Removed unused code from History GridComp

## [2.25.0] - 2022-09-01

### Fixed

- Fix setting stretched grid target latitude and longitude from restart file metadata

### Added

- Added member function get_global_var to FileMetadata
- Added option to build source tarfile when building MAPL standalone. By default this is `OFF`, but can be enabled with
  `-DINSTALL_SOURCE_TARFILE=ON`
- Added `regrid_method` metadata to History output
- Added an overloaded interface for MAPL_BalanceWork to handle both REAL32 and REAL64

### Changed

- Updated `components.yaml` to match GEOSgcm v10.22.5 (actually a bit beyond)
  - ESMA_env v4.2.0 → v4.4.0 (Update to Intel 2022.1, Add TOSS4 Support at NAS)
  - ESMA_cmake v3.17.0 → v3.18.0 (Updates to CPack and Provisional M2 Support)

### Removed

- Removed `LatLonGridFactory_basic` factory constructor (dead code)
## [2.24.0] - 2022-08-08

### Fixed

- Fix error trapping in bundleio test

### Added

- Add tutorials
- Check for duplicate entries in the History.rc file
- Check that a user-provided chunking in the History.rc is compatible with the output grid
- If a user requests CFIOasync in the History.rc, print warning and set to CFIO
- Added option allow writing to pre-existing files with History

### Changed

- Replaced deprecated __RC__ macro with _RC and remove unused code in ExtData2G
- Moved to `checkout@v3` action due to git safe directory issue
- Added tutorials to CI

## [2.23.1] - 2022-07-15

### Fixed

- Fixed the History file existence detection to only run for netCDF output. For still unknown reasons, this detection has an issue
  with binary output (see https://github.com/GEOS-ESM/GEOSldas/pull/568)
- Fix GitHub Actions

## [2.23.0] - 2022-07-06

### Added

- Check return codes for YAML files when parsing in ExtData2G

### Changed

- Updated the ESMA_env version to v4.2.0 (Baselibs 7.5.0 → GFE v1.4.0)
  - With this update, MAPL now **requires** these versions of GFE libraries
    - yaFyaml v1.0.4 (if building with ExtData2G support)
    - pFlogger v1.9.1 (if building with pFlogger support)
- Update the CI for Baselibs 7.5.0, BCs version 10.22.3

## [2.22.0] - 2022-06-24

### Fixed

- By pass the check of the missing value of Nan
- Update CI to work with latest GEOSadas `develop` (Uses a special branch of GEOSadas)
- Fix bundleio tests
- HistoryGridComp now checks if a file exists already before writing and errors out if so
- Minor updates for FORD documentation testing
  - Add `program` statements to some test programs
  - Remove `pfio/pfio_io_demo.F90` as dead code
  - Fix redefinition of `_RETURN` in `pflogger_stub.F90`
  - Removed unused `Test_SimpleClient.pf`
- Update CMake to require NetCDF C components and add `NetCDF::NetCDF_C` to pfio CMake

### Added

- Updated MAPL_CapGridComp to mark fields going to ExtData to not be checkpointed by components
- Add debug loggers for start/stop during stages in MAPL_Generic
- Handling for double precision input when retrieving single precision attributes
- Enable GCM run test in CircleCI (1-hour, no ExtData)
- Added monotonic regridding option
- Make availalbe to History and ExtData2G all supported regridding methods
- Add test cases for ExtData
- Add YAML validator GitHub Action
  - This action makes sure all YAML files are valid (to a relaxed standard)

### Changed

- Modified error messages in FileMetadataUtilities to be unique and print filename
- Updated the ESMA_env version to v3.14.0
- Updated the ESMA_cmake version to v3.17.0
- Updated GitHub Actions MAPL build tests
- Added assert for missing file with ExtData2G
- Re-enable bundleio tests in CI
- Updated CircleCI to use latest Baselibs
- Updates for Spack support
   - Add `find_package(MPI)` for non-Baselibs builds
   - Add explicit interface dependence of `MPI` for `ESMF` target
   - Add `esmf` alias library for `ESMF` for compatibility

## [2.21.3] - 2022-06-07

### Fixed

- Fixed bug in non cubed-sphere grid path in MAPL_GetHorzIJIndex

## [2.21.2] - 2022-05-31

### Fixed

- Initialize `cap%lperp` in `MAPL_CapGridComp.F90` (see Issue #1530)

## [2.21.1] - 2022-05-24

### Fixed

- Adding missing _RETURN and _VERIFY macros in GriddedIO.F90
- Update CircleCI to work with latest GEOSadas

## [2.21.0] - 2022-05-05

### Fixed

- Fixed incorrect legend when using PRINTSPEC option in MAPL Cap
- Fix ESMF errors exposed by monthly collections

### Added

- Added has_variable function to FileMetadata
- Added information about the container type for each item in state when using PRINTSPEC option

### Changed

- Change many instances of `_ASSERT(.false.,"msg")` to `_FAIL("msg")`
- Extended format width for exclusive and inclusive times in profiler from `f9.2` to `f10.2` (see #1420)

## [2.20.0] - 2022-04-19

### Fixed

- Removed one redundant get_file_extension call
- Fix issue where ACG was called when no file had changed
- Add missing `rc=status` in `MAPL_GetResourceFromMAPL_scalar`
- Fixed bugs with next generation ExtData
- Fixed variable PTR40 declaration in GenericCplComp.F90

### Added

- Added support for 4d variables in the coupler. Intentionally decided not to support 4d in the coupler's ReadRestart and WriteRestart to catch errors
- Added ability to use multiple rules for different time periods in next generation ExtData

### Changed

- Cleaned up a bit of old CMake
- Updated CircleCI config to use new orb `build` job
- Updated `components.yaml` to match GEOSgcm v10.22.1
  - ESMA_env v3.13.0
  - ESMA_cmake v3.12.0

## [2.19.2] - 2022-03-28

### Fixed

-  Provided workaround for  GNU bug when defining file metadata in cubed-sphere grid factory (similar to Issue #1433 and its solution)

## [2.19.1] - 2022-03-24

### Fixed

- Fix a bug deallocating a pointer potentially pointing to shared memory allocated by MAPL_Shmem

## [2.19.0] - 2022-03-18

### Fixed

- Fixed duration of the clock to be the smaller of the user specified duration and (END_DATE - currTime)
- Fixes for compiling on M1 Macs (remove REAL128)
- Fix for CMake when `esmf` is already a target

### Added

- New cmake option USE_EXTDATA2G to enable the next generation of ExtData for development, by default uses 1st generation ExtData
- MAPL_ESMFFieldBundleRead/Write modules are now available in when using MAPL

### Changed

- Replaced a wild card "*" in any position of a string in MAPL_GridCompSpecs_ACG.py
- Updated `MAPL_SunGetSolarConstantFromNRLFile` to open NRL Solar Table file only on root and broadcast the tables to all processes.  Now all processes do interpolation.
- Add voting interpolation method as optional argument to SimpleBundleRead method

## [2.18.3] - 2022-03-15

### Fixed

- Fixed bug in 2.18.2 release when computing lats in degrees lat-lon grid factory
- Fixed GNU bug when defining file metadata in lat-lon grid factory

## [2.18.2] - 2022-03-11

### Fixed

- Save copy of original lat/lons in degrees when creating lat-lon grid factory to use in file metadata to eliminate floating point conversion noise

## [2.18.1] - 2022-03-07

### Fixed

- Fix build bug with NAG in `cub2latlon_regridder.F90`
- Fixes DO_NOT_CONNECT errors when calling MAPL_TerminateAnyImport
- Fixed the alarms in the couplers to account if they are called before ESMF_ClockAdvance is called
- Reverted generic/VarSpec.F90 to hash b02e8ff (fix for #1410)

## [2.18.0] - 2022-02-23

### Fixed

- Fixed issue in `CMakePresets.json` where Ninja presets were broken
- Fixed io profiler report format
- Fixed issue on macOS where enabling memutils caused crash

### Added

- Option to force integer time variable in History output via the History.rc file (IntegerTime: .true./.false. default .false.) rather than the default float time variable if allowed by frequency of output
- Added mapl_StubComponent to MAPL package
- Updates to CircleCI
  - Added GEOSadas CI ifort build test
  - Add "like-UFS" build to CI. This is no FLAP and pFlogger, and static build
- Added new `_STAT` and `_IOSTAT` macros a la `_RC`

### Changed

- Changed the naming convention for the split name(s): we now take the entries from the field alias(es) without appending any digits. Also allowing the user to specify more entries in the alias, so that HISTORY.rc does not need to change when running GOCART with more wavelengths
- A small performance improvement. cycle => exit in MAPL_Generic.F90
- Made history global metadata configurable. This can be done in two ways
  1. Globally for all collections by setting `COMMENT:`, `CONTACT:`, `CONVENTIONS:`, `INSTITUTION:`, `REFERENCES:`, and `SOURCE:` at the top of `HISTORY.rc` like `EXPDSC:`
  2. On a per-collection bases by setting `collection.comment:`, `collection.contact:`, `collection.conventions:`, `collection.institution:`, `collection.references:`, and `collection.source:`
  - The default settings for these are to match that of MAPL 2.17.0
- Updated `components.yaml`. These changes are to support using Spack to build MAPL
  - ESMA_cmake v3.10.0 (add `FindESMF.cmake` from NOAA-EMC)
  - ecbuild geos/v1.2.0 (updat `FindNetCDF.cmake` to that from NOAA-EMC)
- Improved file-not-found error handling in ExtData for non-templated filenames

## [2.17.2] - 2022-02-16

### Fixed

- Fixes for Global Attributes to match FP 5.27
  - Changed `lev` variable `standard_name` to `model_layers`
  - Changed global attribute `Convention` to `Conventions`
  - Fill `Source` with a string "<EXPSRC> experiment_id: <EXPID>" where EXPSRC and EXPID are from the `EXPSRC:` and `EXPID:` lines in HISTORY.rc

## [2.17.1] - 2022-02-04

### Fixed

- Add explicit dependence of gFTL-v1 and -v2 whenever both are used (currently profiler and generic)

## [2.17.0] - 2022-01-26

### Fixed

- io profiler is fixed due to the change of profiler finalize
- Files read by GriddedIOMod now respect the file defined _FillValue rather than assuming it is MAPL_UNDEF
- Fix bug so that fields with an ungridded dimension of size 1 can be written via History
- Updated `components.yaml`
  - ESMA_env v3.11.0 (Mainly updates for NAS)

### Changed

- History initialization will display chunksize for any collections that are chunked
- Removed include of `ESMC_ReturnCodes.h`. Changed some `ESMC_RC` codes to `ESMF_RC` codes

## [2.16.0] - 2022-01-13

### Fixed

- A bug in splitting the fields for History when an alias contains a single entry (#1296)
- Trap more errors in pFIO
- Free types and operation created by profiler
- Finalize profiler in MAPL_finalize call
- Fix how a retry-build is done in CI
- Fix issue with logger message and single quotes
- Fix bug with profiler reporting in programs

### Changed

- Updated README.md
- Refactored MAPL_Generic.F90 - lots of changes.
  - consistent indentation (emacs mode)
  - extracted helper procedures in GenericInitialize
- Added new step to CircleCI to try and re-run build on failure.
- Updated CircleCI config to use new [GEOS-ESM/circleci-tools orb](https://circleci.com/developer/orbs/orb/geos-esm/circleci-tools)
- Added Markdown rules to `.editorconfig`

## [2.15.1] - 2022-01-06

### Changed

- Changes to make MAPL 2 History output match GEOS FP file spec
  - Coordinate Variables
    - `lon` and `lat` are now 64-bit (double) in lat-lon History files rather than 32-bit (single)
    - `lons`, `lats`, `corner_lons`, and `corner_lats` are now 64-bit (double) in  History files rather than 32-bit (single)
  - Variable Metadata
    - Added `fmissing_value` (equal to `_FillValue` aka `MAPL_UNDEF`)
    - Added `missing_value` (equal to `_FillValue` aka `MAPL_UNDEF`)
    - Added `vmin` (equal to `-MAPL_UNDEF`)
    - Added `vmax` (equal to `+MAPL_UNDEF`)
    - Added `add_offset` (equal to 0.0)
    - Added `scale_factor` (equal to 1.0)
    - Added `standard_name` (equal to `long_name`)
  - Global Metadata
    - Added `Title`, `History`, `Source`, `Contact`, `Convention`, `Institution`, `References`, `Filename`, `Comment`
      - These currently have hardcoded values roughly equivalent to the GEOS FP 5.27 output

## [2.15.0] - 2022-01-04

### Fixed

- Free types and operation created by profiler

### Changed

- Refactored MAPL_Generic.F90 - lots of changes.
- Make EXPID and EXPDSC optional. Default is empty string
- Updated `changelog-enforcer` to version 3
- Compress CircleCI artifacts
- Updated VarRead_2d_r8 interface to match VarRead_2d_r4 interface in NCIO module

## [2.14.1] - 2021-12-20

### Fixed

- gfortran can not associate an allocated string. Such blocks are changed

### Changed

- Updated `components.yaml`
  - ESMA_env v3.8.0 (Use Intel 2021.3)
  - ESMA_cmake v3.8.0 (Use `-march=core-avx2` for Intel Fortran)
  - These are non-zero-diff for GEOS
- Updated the Intel CI image to Intel 2021.3

## [2.14.0] - 2021-12-16

### Fixed

- Move out allocatable string from if condition block in `VarConn.F90`
- Updates to `mapl_tree.py` to let it work in the git GEOS
  - Note that the interface has changed since the last time this worked, please see script help usage
  - Disabled the `chname` style of running it as that does not work at the moment

### Changed

- Refactored support for using DSO's for components.  No change to interfaces.
- Updated MAPL to exclusively use new timers - with improved format.

### Removed

- Legacy timer profiling

### Deprecated

- An interface for `MAPL_AddChild` allowing specification of a DSO has
been deprecated due to non-conventional ordering of its arguments.  A
new interface with conventional ordering has been introduced.

## [2.13.0] - 2021-12-08

### Fixed

- Return 0 when there is no data for bit shave
- Removed tab characters from Fortran (and C) code

### Added

- MAPL_TimerOn/Off now invoke new timers as well as legacy timers.
- Add  pfio_open_close.F90 file
- Add the i and j index as variables to use to generate synthetic data in ExtDataDriver.x
- Added ability to generate monthly checkpoints (fixes issue #1065)

### Changed

- Changed the way how a writer is chosen. Previously, a writing processor is chosen as long as it is idle.
  Now, an idle processor is chosen from a node with the most idle processors.
- Changed error checking `_ASSERT` to use `__RC__` macro and `_VERIFY` for UserRC
- Changed `_ASSERT` with `.and.` conditional to separate `_ASSERT` to improve error message
- Changed usage of MAPL_IO subroutines in CubedSphere and LatLon Grid Factories to open command with newunit clause
- Updated `components.yaml`
  - ESMA_env v3.7.0 (Use MPT 2.25 at NAS on TOSS)
  - ESMA_cmake v3.7.2 (Fixes FindBaselibs issue found by @sdrabenh, f2py order fix)
- Made the `MAPL_AddChildFromDSO` function system agnostic by using the CMake detected DSO suffix
- Component level timer formatting is changed to provide more information.

### Removed

### Deprecated

## [2.12.1] - 2021-11-08

### Fixed

- Fixes #1186.  Fragile CMake logic for checking minimum version requirements for gFTL.

## [2.12.0] - 2021-11-03

### Fixed

- Fixes #951. Adjusted the size for the internal write, which is compiler dependent. For reals: 15 for Inter, 16 for NAG and Portland group, 18 for gfortran.
- Fixed bug when comparing grid equality in the cubed-sphere factory
- Fixes handling of nested states in MAPL. Removed the requirement to specify horizontal or vertical grid specs for such states. Added a public method to retrieve rootGC

### Added

- Add find_package() calls to main `CMakeLists.txt` for all paths (Baselibs or not). Needed so these calls can be removed in
  `FindBaselibs.cmake` in ESMA_cmake

### Changed

- Relocated CapOptions related modules to `./gridcomps/Cap`. Also simplified the
  FLAP options layer. Had to introduce some minor naming kludges to keep high level GEOS interfaces working.
  FlapCLI.F90 and CapOptions.F90 changes that should be revisited in 3.0 has been commented for backward compatibility.
  This should be revisited under 3.0.
- Updated to ESMA_cmake v3.7.0

### Removed

- Removed MKL dependency in `Tests/`
- Removed support for +/- option for restart names in MAPL_Generic.F90.   Found to be unused, and kludgy.

## [2.11.0] - 2021-10-29

### Fixed

- Fixed bug with MAPL_FindChild gfortran debug compilation
- Fixes #1115. NAG flagged several issues, related to how different derived type are brought in MAPL by different modules, which quite possibly are violation of the standard. Similarly, a procedure call was used as an argument with intent(INOUT).
- Fixed issues with NAG and Tests (#1106)
  - Changed non-Fortran Standard `call exit` to `stop` in `ExtDataDriver.F90`
  - Changed `kind=8` to `kind=REAL64` in `pfio_MAPL_demo.F90`
  - Reenabled build with NAG (works with NAG 7.0.7048)
- Added PFIO support for `NF90_STRING` (as opposed to `NF90_CHAR`).  This fixes use of some netCDF files.

### Added

- Added ability to regrid multiple files in one execution of Regrid_Util.x

### Changed

- Removed last `NETCDF_LIBRARIES` reference from CMake
- OOMPH: Lots of work to tease apart low level "specs" into separate
         files/classes.  At the same time new classes (mostly unused
         as yet) are being introduced for nextgen specs.
   Some details:
   - Introduced new oomph subdirectory and namespace.
   - Replaced some "manual containers" with gFTL Vectors.
   - Updated some gFTL containers to v2 containers.
- Require gFTL v1.5.1
  - Updated `components.yaml` to ESMA_env v3.5.0 (Baselibs 6.2.8)
  - Update CI images to Baselibs 6.2.8

### Removed

- Removed yaFyaml dependency when building with BUILD_WITH_PFLOGGER=NO

## [2.10.0] - 2021-10-22

### Fixed

- Reduced runtime impact of communication barriers in Cap gridded component

### Added

- Exposed `TO_NAME` argument in `MAPL_StateAddExportSpecFrmChld()` to
  allow renaming of EXPORTS. Needed for GOCART-2G.

## [2.9.0] - 2021-10-19

### Added

- Added option to flip vertical orientation of checkpoint files from the provided orientation which is assumed to be down (TOA -> surface) as index increases. User can provide a per grid comp INTERNAL_CHECKPOINT_POSITIVE: and IMPORT_CHECKPOINT_POSITIVE: option with the default as down. If this is set to up 3D fields that are vlocationedge or vlocationcenter will be flipped on writing and positive in the lev variable will be up. Likewise restarts with positive up will be flipped relative to the orientation in the file.
- Added GEOSldas CI build test
- Added option to regrid to a regional lat-lon grid in the Regrid_Util.x utility
- Added [scc](https://github.com/boyter/scc) badges to README
- Added Service-Services functionality. Components could advertise services they can provide, they can request services to be done
  to a list of variables, and current components could connect services.
- Added [EditorConfig](https://editorconfig.org/) file
  - 4 spaces for Python
  - 2 spaces for CMake and YAML
- Preload available macros in CMake package configuration file.

### Changed

- Moved newcfio modules from base into new griddedio directory
    - Renamed newCFIO modules and routines to GriddedIO
- Refactored ExtData modules. Because of the dependencies, the following changes were also done:
    - Moved Collection ExtData modules into griddedio directory. Removed the Ext prefix for collection modules and subroutines and types
    - Moved BundleRead and BundleWrite modules from base to griddedio
    - Moved Regrid_Util.F90 from base to griddedio  due to griddedio dependency on base. Executable still generated in install/bin
- Updated `components.yaml`
    - ESMA_cmake v3.6.5 (Bug fix for NAG, support for mepo styles, `Release` flags are now vectorized, Fix for `BASEDIR`)
    - ESMA_env v3.4.1 (Support for Cascade Lake, moves to Intel 2021.2)
- Refactored MAPL_IO by separating it into a Binary (BinIO.F90) and NetCDF (NCIO.F90) modules. Shared subroutines and
  types have been moved to FileIOShared.F90. MAPL_IO becomes a package module to hold these aforementioned three modules.

### Fixed

- Fixed #338. Added a workaround for a gfortran bug that handles end-of-file incorrectly (returns IOSTAT=5001).
- Fixed ESMF logging errors from MAPL_IO (#1032)
- Make `BUILD_WITH_PFLOGGER` a CMake `option()`
- MAPL finds yaFyaml in CMake through `PFLOGGER::pflogger`, so if you build the stub, specifically add it as a dependency
- Fix annoying misspelling of FLAP

## [2.8.10] - 2021-10-15

### Fixed

- Fixed a missing copy of the output after ESMF_FieldHalo (see #1090)

## [2.8.9] - 2021-10-15

### Fixed

- Added a proper handling for new segment logic in History. This addressed issues #1064 and #1067

## [2.8.8] - 2021-10-13

### Fixed

- Reverts the change in 2.8.7, #1069, as this caused bad History behavior (see #1074)

## [2.8.7] - 2021-10-12

### Fixed

- Fixes #1064. This is bug has been in MAPL for a long time. It shows only when the user specifies a non-default duration, and the last step of the duration interval is written to a new, separate file

## [2.8.6] - 2021-09-13

### Added

- Added the feature which can use nbits ( shave bit) in history binary output
- Added script to automatically make a complete, mepo'd tarball on release

### Changed

- Refactored MAPL_Generic.F90 and MAPL_GenericCplComp.F90 from base to generic. This removes generic dependency from
  base
- Renamed MAPL_GenericCplComp.F90 to GenericCplComp.F90
- Moved MAPL_ExtDataGridCompMod.F90, MAPL_OrbGridCompMod.F90, and MAPL_OrbGridComp.rc from base to gridcomps
  subdirectories
- Renamed Base.F90, Base_implementation.F90, and MAPL_Mod.F90 to Base_Base.F90, Base_Base_implementation.F90, and
  Base.F90 respectively.

### Fixed

- Fixed issue #486. Empty state restarts will now be ignored (with warning) for writing (the code also protects reading, but the existing code already had a different protection)
- Added default `CMAKE_BUILD_TYPE` for MAPL standalone. Defaults to `Release` build if not set on command line

## [2.8.5] - 2021-09-03

### Fixed

- Added missing recursive declaration to MAPL_GenericWrapper

## [2.8.4] - 2021-08-27

### Added

- Added `esma_cpack` include for tarring ability

### Changed

- Updated ESMA_cmake to v3.5.4

### Fixed

- Fix bug when restart name has a "-" at the beginning

## [2.8.3] - 2021-08-19

### Removed

- Removed Pandas dependency
- Removed unused functions from NominalOrbits Module

### Added

- Added error message to pFIO_NetCDF4_FileFormatterMod if nf90_open() fails.
- Add option to flip native level output in History relative to input
- Added `MAPL_AllocNodeArray_6DR8` and `MAPL_DeAllocNodeArray_6DR8` to Shmem
- Refactors Constants into its own library and consolidated mathematical/physical constants used throughout code to use those from library
- Added single precision Degrees to Radian Conversion

### Changed

- Simplified implementation of MAPL_FieldCopyAttributes
- Updated `components.yaml`
  - ESMA_cmake v3.5.3

### Fixed

- Added npes for pfio_MAPL_demo.F90 when --npes_model is not specified in command line
- Fixed bug in ExtData when doing vector pairs

## [2.8.2] - 2021-07-29

### Removed

- Removed unneeded `.gitrepo` files

### Fixed

- Only check the restart grid compared to component if component grid is Cubed-Sphere. Other factories not yet supported.

## [2.8.1] - 2021-07-28

### Removed

- Removed MAPL_OldCubedShereGridFactory.F90 and consilidated with MAPL_CubedSphereGridFactory.F90

### Added

- Add stretch parameters to restarts and check the file grid compared to MAPL grid
  when reading restarts
- Add `CMakePresets.json` file
  - Note: requires CMake 3.21.0 to use
  - Per CMake advice, add `CMakeUserPresets.json` to `.gitignore`

### Changed

- Widened the throughput timer format

### Fixed

- Fixed bug with tripolar grids and restarts to not check the file grid matches the application grid if application grid is tripolar

## [2.8.0] - 2021-07-12

### Added

- Unit tests can now use the `_RC` macro for checking results from
  calls to ESMF.  The file must first CPP define either `I_AM_PFUNIT`
  or `I_AM_FUNIT` (serial) and then `#include "MAPL_ErrLog.h"`.

### Changed

- Activated ESMF logging for unit tests.
- Fixed problem in unit testing framework that results in
  "harmless" warnings/errors in the ESMF log.
- Update CMake to exclusively use GFE Namespace. This means that when building MAPL, users should use the latest versions of GFE libraries (gFTL, gFTL-Shared, pFlogger, fArgParse, yaFyaml, pFUnit)
- Update ESMA_cmake to v3.5.1 (macOS fix)
- Updated the CI to do both GNU and Intel builds of MAPL and GEOSgcm on CircleCI. (Note that for now the GEOSfvdycore build is turned off due to cost of CI)
- Updated the CI to store logfile artifacts from CircleCI builds

### Fixed

- Fix format for writing out large number
- Fixed CMAKE_Fortran_MODULE_DIRECTORY for some directories
- Update handling of file coordinates when creating grids from file. Now if identified as a standard grid compute coordinates. Option to allow this to be overrided and use file coordinates. Fixed issue if two files are identified as a standard grid but has very slightly different coordinates causing one or the other to be used depending on which file is used first.
- Fixed bug with corner case in the new logic to compute lons if matching one of our standard grids
- Fix for NAG Build

## [2.7.3] - 2021-06-24

### Fixed

- Ensure grid coordinates are always provided in radians

## [2.7.2] - 2021-06-23

### Fixed

- Add support for GNU Fortran 9.3.0

## [2.7.1] - 2021-06-11

### Added

- Add more function in pfio_MAPL_demo.F90
- Add option BUILD_SHARED_MAPL to build shared or static library
- Regrid_Util.x now checks if file exsts and captures the units and long_name of the input for the output file
- Add `--with_esmf_moab` to enable MOAB Mesh in ESMF

### Changed

- Set logical values in flap commmand line without true or false values
- Set required CMake Version to 3.17
- Updates to enable use of GFE namespaces (requires Baselibs v6.2.4 or higher)
  - ESMA_cmake v3.5.0
  - ESMA_env v3.3.0
  - Update CI to use 6.2.4 CI images

### Fixed

- Allow MAPL to build if subrepos are cloned with any mepo style (prefix, postfix, naked)
- Add missing variable declaration preventing MAPL from building if H5_HAVE_PARALLEL is defined
- Protect against trying to flip 2D variable in ExtData if there are mixed 2D/3D in file

## [2.7.0] - 2021-05-25

### Removed

- Remove file `MAPL_FlapCapOptions.F90`

### Added

- Added a file `MAPL_FlapCLI.F90`

### Changed

- Added a MAPL_CapOptions constructor
- Change FlapCapOptions to FlapCLI which is not a sub class of
  MAPL_CapOptions any more. This update means code that before did:
  ```fortran
   type (MAPL_Cap) :: cap
   type (MAPL_FlapCapOptions) :: cap_options

   cap_options = MAPL_FlapCapOptions(description = 'GEOS AGCM', &
                                     authors     = 'GMAO')
   ```
   now must do:
   ```fortran
   type (MAPL_FlapCLI) :: cli
   type (MAPL_CapOptions) :: cap_options

   cli = MAPL_FlapCLI(description = 'GEOS AGCM', &
                      authors     = 'GMAO')
   cap_options = MAPL_CapOptions(cli)
   ```
   This was changed to facilitate working with UFS.

## [2.6.8] - 2021-05-21

### Changed

- Adopting Fortran submodules to improve compilation.
- Added a pfio demo for MAPL FLAP users

### Fixed

- Fixed pfio_MAPL_demo.F90
- Fixed mismatch of ESMF_Initialize() and ESMF_Finalize()
- Fixed bug in MAPL_Shmem causing infinite loop when relesing shared memory
- Moved down adding pflogger in CMakeLists.txt
- Added condition to find pflogger

## [2.6.7] - 2021-05-12

### Added

- New interface to MAPL_GetResource to pass config rather than MAPL object

### Changed

- Re-org subroutine finalize_io_clients_servers to avoid missing calls
- Use `ESMF_Finalize` instead of `MPI_Finalize` in Cap
- Allow the NRL Solar Data table read function to skip commented lines
- Updated `components.yaml` to be in sync with GEOSgcm:
   - ESMA_env v3.2.1
   - ESMA_cmake v3.4.0

### Fixed

- Added return code in start_global_profiler()
- Fixed during-run timer output for perpetual year runs
- Fixed bug prevent "little" cfio from reading new cubed sphere files

## [2.6.6] - 2021-04-29

### Fixed

- Fixed bug in `SimpleCommSplitter.F90`

## [2.6.5] - 2021-04-28

### Removed

-  pFIO/KeywordEnforcer.F90 duplicated functionality now in
   shared/KeywordEnforcer.F90, and has been removed in favor of the
   other.

### Added

- A new flag to timestamp average collections at the beginning of the averaging interval
- Ability to run MultiGroupServer and model in a single node
- Add command line option --one_node_output
- Ability to split fields with ungridded dimensions (and not only 4d).
- Ability to add alias names to the split fields
- Added MAPL_SimpleBundleCreateEmpty procedure to MAPL_SimpleBundleCreate.
- Add MAPL_TransposeaRegridderMod to MAPL_Mod
- Nearest-neighbor interpolation option for ExtData (keyword: 'E')
- Added pflogger_stub directory. With `-DBUILD_WITH_PFLOGGER=OFF`, it is built and linked to replace pFlogger library.
- Added new CI test using Intel oneAPI
- Add function to free communicators that is split by SimpleCommSplitter
- Add with_io_profiler option

### Changed

- Changed the interface to TimeData to have an optional "funits" argument (defaults to "minutes")
- Changed time units to "days" for monthly collections
- Simplified the logic for timestamping offsets
- Setting and getting UNGRIDDED_DIMS attribute uses now single quoted string
- Do not output `cubed_sphere` and `orientation` variables in native
  History output as pFIO at present does not handle string variables
- Updated Python scripts to work with Python 2 or 3. Scripts were:
   - `base/mapl_tree.py`
   - `base/mapl_vlist.py`
   - `Apps/MAPL_GridCompSpecs_ACG.py`
   - Nullified pointers for deactivated optional state elements for
     the grid compe spec code generator ACG.
- Updated `components.yaml`:
   - ESMA_env v3.2.0 (Baselibs 6.1.0 <==> ESMF 8.1.0)
   - ESMA_cmake v3.3.9 (adds ability to see GFE namespace option, `BUILD_WITH_PFLOGGER`)
- Update CI images to 6.1.0
- Updated MAPL to have the ability to use the new GFE namespace in CMake. (`gftl` --> `GFTL::gftl`).
   - The default in ESMA_cmake v3.3.8 is *not* enabled. To enable use `-DESMA_USE_GFE_NAMESPACE=ON`.
   - NOTE: This requires Baselibs 6.2.0 or higher when using Baselibs.
- Updated the non-PersistSolar branch of `MAPL_SunGetSolarConstantFromNRLFile` to use Solar Cycle 24 as we are now in Cycle 25.

### Fixed

- Add _RETURN(_SUCCESS) to MAPL_SimpleBundle routines
- Fixed possibly uninitialized values when handling members of Segment_T derived type. Helps on the Rome nodes.
- Fixed print diagnostics for monthly collections (proper reporting of frequency, duration, eliminated acc_interval)
- Fixed another bug related to the incorrect time increment for monthly averaged collections
- Fixed few memory leaks (average and stampOffset arrays were allocated twice)
- Fixed a bug related to incorrect time increment attribute for a monthly collection
- Fixed a bug related to the naming scheme for split fields when ungrid size is 1
- Fixed unset UNGRIDDED_DIMS attribute bug
- Fixes ESMF logging errors related to expressions in History
- Fixed error handling in profiler/BaseProfiler.F90
- Fix memory leak when using fast_oserver in write_restart_by_oserver
- Bumped cube version to 2.91 in global metadata
- Change calls to `system_clock()` to be `INT64` (#511)
- CMake updates to allow NAG Fortran build
- Converted some remaining `real*8`-type declarations to be `real(kind=REAL64)`-style
- Eliminated (almost) all compiler warnings for Intel compiler
- Removed conditional around declaring pointers in code emitted by grid comp ACG.
- Fixed bugs in ESMFL and MAPL_CFIOReadParallel to support GEOSadas
- Remove some unnecessary MPI_Comm_dup calls. Some of those call are actually bugs

## [2.6.4] - 2021-03-18

### Added

- Add support for multi-run-phase for root gridcomp

### Fixed

- Fixed spliiting the same field in multiple collections
- Fix out-of-bound access when printing pFIO message
- Removed program tstqsat.F90 from MAPL.base library.  A followup
  should add cmake logic to create an executable or just delete the
  file.
- CMake workaround for macOS + Intel oneAPI FLAP bug (#644)
- Fixed size of unallocated array for gfortran
- Fixed counting of backend npes for assert

## [2.6.3] - 2021-03-09

### Added

- Disable throughput reporting if an external clock is driving CapGridComp
- Comment out profiler in output server
- Add profiler for output server
- New overload for MAPL_ConfigSetAttribute to support array of integers
- New overload for MAPL_ConfigSetAttribute to support array of reals
- Add return code to constructor method for MAPL Cap gridded component to allow applications
  to fail gracefully if an error occurs
- Added ability to "attach" to the pfafstetter grid for land tiles for components running directly on the catchments

### Changed

- Change to non-blocking send and receive from frontend to beckend in the class MultiGroupServer
- Change one sided mpi_put to mpi_send and receive pair in the class MultiGroupServer
- Change command line interface to --npes_backend_pernode to avoid confusion
- Remove self-defined-in-file MAPL macros

### Fixed

- Fix bug in HorzIJ routine to place geospatial points when the grid units are degrees
- Have CMake automatically gitignore build and install dirs
- Properly set return code for MAPL Cap methods
- Remove some GFORTRAN workarounds in MAPL_LocStreamMod (some still
  needed for GNU layout regression, #733)
- Fix issue with History when field names have "." in them

## [2.6.2] - 2021-02-19

### Added

- Completed new capability to conseratively regrid horizontal fluxes
  Important constraint is that the input grid must be a refinement of
  the output grid.
- Improvement to ExtDataDriver when generating synthetic data
- Allow user to specify decomposition used by grids in History output, useful for testing
- Add %S as seconds token to grads style StringTemplate
- Add new bundle IO routines for non performance critical IO to eventually depreciate MAPL_CFIO and MAPL_cfio

### Fixed

- Fix MAPL_AddChildFromDSO

## [2.6.1] - 2021-02-16

### Changed

- Move to ESMA_cmake v3.3.6
- Remove requirement for HDF5 Fortran bindings in MAPL

### Fixed

- Fixes build of MAPL on non-Baselibs machines

## [2.6.0] - 2021-02-12

### Added

- Add option to compute variance of tiles when doing T2G locstream transform
- Add option fast_oclient that waits before using oserver. It would not wait after done message are sent
- Added new `is_valid_date()` and `is_valid_time()` functions to make
  sure invalid times and dates are not sent to MAPL

### Changed

- For MultiGroupServer, the backend and frontend share each node
- Moved `MAPL_HistoryGridComp.F90` and related files to `gridcomps/History`
- Updated `Python/MAPL/constants.py` to have the same constants as
  `MAPL_Constants.F90`

- Major refactoring related to MAPL generic capabilities

    0. Created new subdirectory  "generic"

    1. Modified interface to MAPL_Get() to use ALLOCATABLE for GEX, GIM,
       GCS, and GCnamelist.  Original interface assumed these are
       contiguous which will not be sustainable under the planned changes.

    2. (Re)Introduce fundamental classes in generic subdir.  Made
       MAPL_MetaComp an extension of AbstractComponentNode, and provided
       stub implementations for 3 methods. (Commenting out other
       interfaces in the abstract class for now.)

    3. Activate get_parent(), add_child(), and num_children()

    4. Introduced AbstractComposite and ConcreteComposite

       These are isolate the responsibility for managing the component hierarchy.
       CompositeComponite then blends in ConcreteComposite into the
       AbstractFrameworkComponent class.

    5. Extracted internal state from MAPL_MetaComp

    6. Started moving derived types related to import/export specification
       and such.    The goal will be to then refactor into proper classes
       with encapsulation.

    7. Introducing Vector container for array of pointers to VarSpecType.

       First brute force attempt resulted in run-time issues that were
       difficult to trace.  So going gradually.  Have introduced a
       StateSpecification type that hold the legacy array of pointers and a
       vector and methods that will enable keeping both representations
       consistent.

       New representation is not used yet.

       Various attempts to update use of MAPL_VarSpec based
       upon vectors were failing due to multiple pointer associations
       across objects.

       The basic vector was modified to be MAPL_VarSpec instead of
       MAPL_VarSpecType, and now all works.

       Wrapped new procedure with legacy interface.

       HistoryGridComp uses the older interface in a way that is
       not immediately fixable.

    8. MAPL_GenericGrid
       - Created new module in generic for and renamed to MaplGrid
       - moved component from MAPL_MetaComp to BaseFrameworkComponent

    9. Moved 'lgr' component to baseFrameworkComponent.
       - Add obsolete warning to deprecated interface
       - Migrated some grid methods to new class.
       - This necessitated moving some procedures from MAPL_Base into
         MAPL_Generic.

    10. Prepping VarSpecMod for relocation.
        - Removed dependencies on MAPL_IO and MAPL_comms.  Used pflogger
          to replicate the functionality.
        - Eliminated dependence of VarSpec on Base.
        - Moved some parameters into new Enumerator module in ./shared.
          MAPL_Base still needs to republish them to avoid issues with external
          codes.

    11. Introduced a MaplShared package to export everything from

    12. Migrated VarSpec to ./generic.

    13. Eliminated use of MAPL_Communicators in MAPL_CFIO.F90.
        - Side benefit - eliminated lots of unused routines and logic associated
          with old and new o-server.

    14. Lots of things related to MAPL_Communicators and old O-server
        were eliminated.

    15. Kludgy relocation of component logger.

    16. MAPL_Initialize is needed in each test layer so pulled
        it over to the pfunit directory.

### Fixed

- Fixes for allowing forked PRs to pass CI
- Fixes incorrect referrencing of R4 variables when reading R8 tile vars in base/MAPL_IO.F90
- Use integer to represent logical internally in pFIO utilities to avoid non-standard transfer
- Modified horizontal ij search algorithm in MAPL_Base.F90 to use general bisection search to fix capaibility issues with non-gmao created cubed sphere grids

## [2.5.0] - 2021-01-08

### Added

- Added an `ExternalGridManager`, to allow MAPL to have knowledge of external grids (for NUOPC).
- Added command line interface option `--isolate_nodes`. By default it is `.true.`
- Add stretching factors to file if applicable in cubed-sphere output via History and uptick to cube version 2.91
- Ability to register protoype (non-ESMF) regridders in NewRegridderManager
- Make the default clobber rather than no clobber in NetCDF formatter in PFIO
- Add basic check that the restart files match the application grid
- Add new `MAPL_AddChildFromDSO` module procedure for `MAPL_AddChild`

### Changed

- Move MAPL_Cap, CapGridComp, and NUOPCwrapper to new directory in anticipation of refactored ExtData that will  not live in base
- Convert `file(GLOB)` to explicit list
- Moved modules into separate files.
- Updated various modules to use the standard `_ASSERT()` and `_VERIFY()` macros instead of ad-hoc ones.
- MAPL (sub-)libraries are now built as `TYPE SHARED` (needed for DSO work)
- Updated `components.yaml`
  - ESMA_env v3.1.3
  - ESMA_cmake v3.3.5
- Update CI image to use Baselibs v6.0.27

## [2.4.0] - 2020-11-20

### Added

- Added ability to inject grid into root child GridComp (for NUOPC).
- Added ability to use external clock (for NUOPC).
- Enabled building and installing as a standalone library, using external dependencies.

### Changed

- Change CI Image to 6.0.22.
- Updated `components.yaml`
  - Move to ESMA_env v3.1.0
  - Move to ESMA_cmake v3.3.0 (**REQUIRED** due to Baselibs detection changes and `find_package(FLAP)` moved to MAPL)

### Fixed

- Bug in pfio tests when compiled with Debug flag
- Bug in injecting grid into root child GridComp (for NUOPC).
- Bug preventing components from advancing when an external clock is used

## [2.3.6] - 2020-11-12

### Added

- Added an external grid and clock setter (for NUOPC).

### Fixed

- Fixed logic to allow proper termination of all imports except those specified

## [2.3.5] - 2020-11-06

### Added

- Added fixture entry to `components.yaml` (requires mepo v1.23.0 or higher)

### Fixed

- Fixed integer overflow in memutils for big memory systems
- Fix bug with segment alarm when processing a monthly mean collection


## [2.3.4] - 2020-10-20

### Changed

- Increased the width of the `#-cycles` column in profiler output
- Added Docker authentication to CI
- Updated mepo components to match GEOSgcm

### Fixed

- Fixed bug with reading in cubed-sphere files that have the corners

## [2.3.3] - 2020-10-14

## Fixed

- Fix bug with using coarse grids in History and ExtData

## [2.3.2] - 2020-10-09

### Fixed

- Fixed a bug in ExtData when extrapolating on a Leap Day (#563)

### Added

- Added a deflate and bit shaving option to Regrid_Util.x

### Changed

- Updated Github Docker CI image

## [2.3.1] - 2020-10-08

### Fixed

- Fixed bug in ExtData when using debug logger

## [2.3.0] - 2020-10-02

### Added

- Add MultiComm, MultiGroupi and MultiLayer to include front ends and back ends in the oserver
- Added routine to finalize the ioservers so that it can be called by another application using cap, like JEDI
- Re-added CircleCI with FV3 standalone test
- Add ability to run multiple forward time integrations within one execution for JEDI (#529)
- Added mpeu `StrTemplate` replacement to MAPL

### Changed

- Automate the server pool split and history work distribution when there are multiple oservers
- Moved more code to use pFlogger
- Update to ESMA_cmake v3.2.1 and ESMA_env v3.0.0
- Update GitHub Actions to use Ubuntu 20/GCC 10 image
- Updated CircleCI image to use 6.0.16 Baselibs
- Refactor the option WRITE_RESTART_BY_OSERVER
- Change the writing rank calculation in ServerThread.F90
- Cleanup of the NUOPC Wrapper's error handling using macros


### Fixed

- Removed non-standard OpenMP pragma
- Fixed problem with name mangling in ACG
- Fix MAPL comm bug with NUOPC
- Fixed pointer issue exposed by GNU
- Fixed ESMF logging errors with non-72-level runs (#480)
- Remove unneeded `use Env;` in stub generator
- Fixed tripolar metadata output (#528)
- Fixed `MAPL_ErrLogMain.h` for use with GSI_App
- Added MAPL_CFIOReadParallel change from Ricardo for TimeList

### Removed

- Remove MAPL_ioClients.F90 and move some subroutines to ClientManager.F90
- Remove unneeded GNU make file

## [2.2.7] - 2020-06-26

### Changed

- Update to ESMA_env v2.1.6

## [2.2.6] - 2020-06-26

### Fixed

- Fixed double timers from profiler (#472)

## [2.2.5] - 2020-06-24

### Fixed

- Fix for the `_VERIFY()` macro (#464)

## [2.2.4] - 2020-06-23

### Fixed

- Fix to `sun.H` to allow CFMIP SCM cases to run

## [2.2.3] - 2020-06-23

### Added

- CMake option BUILD_WITH_FLAP which is default ON.  When set to OFF, the build
  skips layers that require FLAP.  (Supports GCHP)

## [2.2.2] - 2020-06-22

### Changed

- Updated Github Actions to not build GCM if trivial PR

## [2.2.1] - 2020-06-22

### Fixed

- Fixed Python ACG to work with Python 2.x

## [2.2.0] - 2020-06-22

### Added

- Added LLC grid factory
- Added support for wildcard expansions (using regex)
- Added "public" for 2 interfaces: ESMFL_Diff, and ESMFL_statistics
- Added support for sampling along a 1-D timeseries in History
- Introduced generic subdirectory
- String.F90 - encapsulates deferred length strings
- Added target "build-tests" that will build all tests.  This will enable
  ctest to be more selective about which tests.
- Added ability of MAPL_GridCompGetFriendlies to recurse its children
- Added `esma_add_subdirectory(ESMA_env)` to `CMakeLists.txt` to allow
  installation of various files to the `bin` and `etc` directories under
  install prefix
- Added wildcard support for short name in automatic code generator for
  gridded components.
- Added new CI test for building GCM on pull request


### Changed

- Refactored aliases in python automatic code generator.  Now aliases
  are tailored per column.  This allows T/F to be safely used as
  aliases for .true./.false. without risking things like the short
  name of Temperature.
- Move to use Baselibs 6.0.13
  - Update CI to use Baselibs 6.0.13 (GCC 9.3.0)
  - Update `components.yaml`
    - ESMA_env v2.1.5
    - ESMA_cmake v3.0.3
- Updates for JEDI/ecbuild compatibility
  - Updates to CMake to use `NOINSTALL`
  - Update `components.yaml` to use `NOINSTALL`
    - ESMA_cmake v3.0.6
    - ecbuild geos/v1.0.5
- Renamed MAPL_Profiler executable demo.x to profiler.x
- Renamed directories.   Sub-libraries now named MAPL.<sub>

  - `./MAPL_Base` => `./base` (`MAPL.base`)
  - `./GMAO_pFIO` => `./pfio`
  - `./MAPL_Profiler` => `./profiler`
  - `./MAPL_Shared` => `./shared`

- Updated `components.yaml` to use `ESMA_env` and `ESMA_cmake` if
  building MAPL as standalone

### Removed

- Removed duplicate `Python` directory
- Removed CircleCI

## [2.1.6] - 2020-07-08

### Changed

- Updates for JEDI/ecbuild compatibility
  - Updates to CMake to use `NOINSTALL`
  - Updates to `components.yaml` to support use of `NOINSTALL`

## [2.1.5] - 2020-06-11

### Changed

- Added MODE argument to MAPL_RecordAlarmIsNeeded. This is in sync with "yuri-s2s3-unstable-SLES12" CVS tag. Needed for "dual ocean"

### Fixed

- Fixed logger creation (similar fix was already applied to develop branch to fix issue #397)

## [2.1.4] - 2020-05-21

### Fixed

- Initialize pFlogger in `Regrid_Util.x`

## [2.1.3] - 2020-05-04

### Changed

- MAPL now requires Baselibs 6.0.12 (pFlogger v1.4.1)
  - Update CI to use Baselibs 6.0.12
  - Update `components.yaml`
    - ESMA_env v2.1.2
    - ESMA_cmake v3.0.2

### Fixed

- Fix for `MPI_Finalize` error with Intel MPI
- Fix `ArrDescrInit` routine
- Fixes for ESMF logging (when run with MULTI)

### Removed

- Remove `MPI_comm_set_errhandler` workaround

## [2.1.2] - 2020-04-24

### Fixed

- Fixed bug when output fields on tripolar grid in History
- Fixed bug during replay when the refresh template in ExtData is a time interval

## [2.1.1] - 2020-04-20

### Fixed

- Added a default initialization clause for pFlogger
  so that INFO messages go to console.
- Workaround for MPT 2.17 build bug with `MPI_Comm_set_errhandler`


## [2.1.0] - 2020-04-16

### Changed

- Corrected handling of Equation of Time in orbit (off by default)
- Made ASSERT in ExtData more explicit in case of missing variables.
- (re) Introduced MAPL Profiling package
- Improved diagnostic message in HistoryGridComp for misspelled fields/bundles
- Removed CVS keywords

### Fixed

- Corrected Python code generator scripts for component import/export specs.
- Add directories to `.gitignore` for building with `mepo`
- Bug building with mixed Intel/GCC compilers
- Implemented workaround to cmake error that happens when building tests in parallel.
- Set correct ESMA_env tag in `components.yaml`
- Updated `components.yaml` to be inline with GEOSgcm
- Minor problem in GMAO_pFIO Cmakelists (consistency with PRIVATE)

### Removed

- Removed support for `checkout_externals` and moved solely to `mepo`
  - Removed `Externals.cfg`
  - Removed `checkout_externals` code in `CMakeLists.txt`

### Added
- Added record capabilites for the History restarts

- Added configuration for CircleCI and Github Actions
  - Builds MAPL using GCC 9.2.0 and Open MPI 4.0.2
  - Builds and runs `pFIO_tests` and `MAPL_Base_tests`
- Add precession of equinox (not on by default)
- Imported Python/MAPL subdir (old, but never imported to GitHub)
- Python automatic code generator for grid comp include files
- Added support to use pFlogger for logging
  - Command line option: `--logging_config=<file>`
- Added ability for History to do monthly mean. This also involves reading and writing MAPL_GenericCpl checkpoints

## [2.0.6] - 2020-04-15

### Fixed

- Added code to suppress (seemingly) spurious MPI_Finalize errors at end
  of model run. Suppression does not happen if built with
  `-DCMAKE_BUILD_TYPE=Debug`

## [2.0.5] - 2020-04-13

### Fixed

- Fixes an issue with a too-large MPI tag.

## [2.0.4] - 2020-04-03

### Fixed

- Fixes an issue when regridding thru the locstream in the history component.

## [2.0.3] - 2020-03-19

### Fixed

- Fixed a logic bug in the MAPL Profilers that make affect certain runs when using NUOPC.

## [2.0.2] - 2020-03-10

### Fixed

- Fix for handling coarse grids at high-resolution in ExtData

## [2.0.1] - 2020-03-02

### Fixed

- Restoring functionality with the tripolar grid that was lost when the develop branch was merged into master for version 2.0.0

## [2.0.0] - 2020-02-07

### Added

- New IO server implemented in PFIO library.
- New command line arguments to the MAPL_Cap to run multiple input and output servers on dedicated resources.

### Changed

- Code that uses MAPL should now `use MAPL` instead of `use MAPL_Mod`.
- CMakeLists.txt using MAPL should now have dependencies to `MAPL` and not `MAPL_Base`.
- History and ExtData component use the PFIO IO server for all file access. Default mode is to run the IO servers on the same resources as the application.
- The ExtData and History components use ESMF regridding for all operations and replace the FV3 regridding routines used for bilinear regridding and the MAPL tiling regridder for conservative regridding.

## [1.1.13] - 2019-12-09

### Fixed

- Correct handling of vector regridding in MAPL_CFIO.F90 layer

## [1.1.12] - 2019-12-03

### Added

- Added `CHANGELOG.md`

### Fixed

- Check status of round robin and make sure that the nodearray is allocated
- Allow per-cell counters to be properly reset (if they are needed)
- Must create file unit on all processors (`all_pes=.true.`) when writing binary History output

## [1.1.11] - 2019-09-24

### Added

- Added the option to add a prefix to the name in `GetFriendlies`

## [1.1.10] - 2019-09-18

### Fixed

- Fix a bug with exact reply using binary files

## [1.1.9] - 2019-09-18

### Changed

- Make `MAPL_ESMFStateReadFromFile` routine public

## [1.1.8] - 2019-09-18

### Added

- Add ability to change step size for average sun angle for LDAS

## [1.1.7] - 2019-08-15

### Fixed

- Fixes for the EASE index

### Added

- Add support for LDAS ensembles

## [1.1.6] - 2019-08-01

### Fixed

- Updates to allow MAPL to better support the tripolar grid

## [1.1.5] - 2019-07-26

### Fixed

- Fixes made to `MAPL_TilingRegridder.F90`

## [1.1.4] - 2019-07-26

### Fixed

- Changes to correct for new pressure after horizontal regridding

## [1.1.3] - 2019-07-25

### Changed

- Moved to use ESMA_cmake v1.0.9
- Use the new `LATEX_FOUND` capability of `ESMA_cmake` to determine if TeX processing can occur

## [1.1.2] - 2019-07-24

### Added

- Added `CODEOWNERS` file

### Removed

- Deleted unneeded GNUmakefiles

## [1.1.1] - 2019-07-08

### Changed

- Install Perl script missed in last release

## [1.1.0] - 2019-07-03

### Added

- First commit of MAPL 1.x with semantic versioning on GitHub
