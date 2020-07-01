# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
### Changed
### Fixed
### Removed

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
