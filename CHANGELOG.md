# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Completed new capability to conseratively regrid horizontal fluxes
  Important constraint is that the input grid must be a refinement of
  the output grid.

### Changed

### Fixed

### Removed

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
