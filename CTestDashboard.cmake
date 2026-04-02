## ---------------------------------------------------------------------------
## CTestDashboard.cmake -- CTest dashboard driver script for MAPL
##
## Usage:
##   ctest -S CTestDashboard.cmake [options] -V
##
## Optional command-line overrides (pass with -D before -S):
##   -D model=Nightly          # default: Experimental
##   -D build_type=Debug       # default: Release
##   -D generator=Ninja        # default: Ninja
##   -D jobs=8                 # default: 6
##   -D site=myhost            # default: auto-detected hostname
##   -D build_name=my-label    # default: auto-generated
##
## Environment variables (optional):
##   CTEST_EXTRA_CMAKE_ARGS   Space-separated extra args passed to cmake
##                            configure.  Example:
##                              export CTEST_EXTRA_CMAKE_ARGS="-DCMAKE_Fortran_COMPILER=gfortran -DMPIEXEC_PREFLAGS=--oversubscribe"
##   CDASH_AUTH_TOKEN         CDash authentication token for submissions.
##                            Generate one at my.cdash.org → Project Settings
##                            → Authentication tokens.  Keep this out of git!
##
## Example - quick experimental submit:
##   ctest -S CTestDashboard.cmake -V
##
## Example - nightly with 8 jobs:
##   ctest -D model=Nightly -D jobs=8 -S CTestDashboard.cmake -V
##
## CDash project: https://my.cdash.org/index.php?project=MAPL
## ---------------------------------------------------------------------------

cmake_minimum_required(VERSION 3.27)

## ---------------------------------------------------------------------------
## 0. Defaults (can be overridden via -D on the ctest command line)
## ---------------------------------------------------------------------------
if(NOT DEFINED model)
  set(model "Experimental")
endif()

if(NOT DEFINED build_type)
  set(build_type "Release")
endif()

if(NOT DEFINED generator)
  set(generator "Ninja")
endif()

if(NOT DEFINED jobs)
  set(jobs 6)
endif()

## ---------------------------------------------------------------------------
## 1. Source and binary directories
## ---------------------------------------------------------------------------
# Source directory is the directory containing this script
get_filename_component(CTEST_SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_FILE}" DIRECTORY)

# Build directory mirrors the preset naming convention: build-<preset>
# For Release+Ninja -> build-Release-Ninja
# For Release+Unix Makefiles -> build-Release
if(generator STREQUAL "Ninja")
  set(_preset_suffix "${build_type}-Ninja")
else()
  set(_preset_suffix "${build_type}")
endif()
set(CTEST_BINARY_DIRECTORY "${CTEST_SOURCE_DIRECTORY}/build-${_preset_suffix}")

## ---------------------------------------------------------------------------
## 2. Site and build name
## ---------------------------------------------------------------------------
cmake_host_system_information(RESULT _hostname QUERY HOSTNAME)
cmake_host_system_information(RESULT _os_name  QUERY OS_NAME)
cmake_host_system_information(RESULT _os_rel   QUERY OS_RELEASE)

if(NOT DEFINED site)
  set(site "${_hostname}")
endif()
set(CTEST_SITE "${site}")

# Auto-detect compiler from environment if possible
if(NOT DEFINED build_name)
  # Try to identify the Fortran compiler for a descriptive name
  if(DEFINED ENV{FC})
    get_filename_component(_fc_name "$ENV{FC}" NAME)
  elseif(EXISTS "${CTEST_BINARY_DIRECTORY}/CMakeCache.txt")
    # Read from an existing cache
    file(STRINGS "${CTEST_BINARY_DIRECTORY}/CMakeCache.txt" _cache_fc
         REGEX "^CMAKE_Fortran_COMPILER:FILEPATH=")
    if(_cache_fc)
      string(REGEX REPLACE ".*=(.*)" "\\1" _fc_path "${_cache_fc}")
      get_filename_component(_fc_name "${_fc_path}" NAME)
    endif()
  endif()
  if(NOT _fc_name)
    set(_fc_name "unknown-FC")
  endif()

  # Try to grab the current git branch name
  execute_process(
    COMMAND git rev-parse --abbrev-ref HEAD
    WORKING_DIRECTORY "${CTEST_SOURCE_DIRECTORY}"
    RESULT_VARIABLE _git_res
    OUTPUT_VARIABLE _git_branch
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  if(_git_res EQUAL 0 AND NOT _git_branch STREQUAL "HEAD" AND NOT _git_branch STREQUAL "")
    string(REPLACE "/" "-" _safe_branch "${_git_branch}")
    set(build_name "${_safe_branch}-${_os_name}-${_os_rel}-${_fc_name}-${build_type}-${generator}")
  else()
    set(build_name "${_os_name}-${_os_rel}-${_fc_name}-${build_type}-${generator}")
  endif()
endif()
set(CTEST_BUILD_NAME "${build_name}")

## ---------------------------------------------------------------------------
## 3. CMake configure command (equivalent to cmake --preset <name>)
##    We call cmake directly so the script controls the preset selection.
## ---------------------------------------------------------------------------
# Use the same cmake that is running this script -- avoids picking up
# CMake.app or other GUI wrappers via find_program() on macOS.
set(CTEST_CMAKE_COMMAND "${CMAKE_COMMAND}")
find_program(CTEST_GIT_COMMAND NAMES git)

# Map generator name to cmake -G argument
if(generator STREQUAL "Ninja")
  set(_cmake_generator "Ninja")
else()
  set(_cmake_generator "Unix Makefiles")
endif()

# We pick up the preset's installDir convention too
set(_install_dir "${CTEST_SOURCE_DIRECTORY}/install-${_preset_suffix}")

# Pick up any extra cmake args from the environment
# (e.g. -DCMAKE_Fortran_COMPILER=gfortran set by CI)
set(_extra_cmake_args "$ENV{CTEST_EXTRA_CMAKE_ARGS}")

# CTEST_CONFIGURE_COMMAND must be a single string (not a CMake list) --
# CTest passes it directly to the shell via 'sh -c'.
# Rules:
#   - Quote the cmake binary path (may contain spaces on some systems).
#   - Quote the generator name (may contain spaces, e.g. "Unix Makefiles").
#   - Do NOT add extra shell-level quotes around -D values; cmake receives
#     them after shell word-splitting and the literal " chars corrupt the value.
#   - Quote the source directory path at the end.
set(CTEST_CONFIGURE_COMMAND "\"${CTEST_CMAKE_COMMAND}\"")
string(APPEND CTEST_CONFIGURE_COMMAND " -G \"${_cmake_generator}\"")
string(APPEND CTEST_CONFIGURE_COMMAND " -DCMAKE_BUILD_TYPE=${build_type}")
string(APPEND CTEST_CONFIGURE_COMMAND " -DCMAKE_INSTALL_PREFIX=${_install_dir}")
if(_extra_cmake_args)
  string(APPEND CTEST_CONFIGURE_COMMAND " ${_extra_cmake_args}")
endif()
string(APPEND CTEST_CONFIGURE_COMMAND " \"${CTEST_SOURCE_DIRECTORY}\"")

## ---------------------------------------------------------------------------
## 4. Build commands  (all must be single strings)
##    Step 1: install (the main build)
##    Step 2: build-tests (MAPL test executables are a separate target)
## ---------------------------------------------------------------------------
set(CTEST_BUILD_COMMAND
    "\"${CTEST_CMAKE_COMMAND}\" --build \"${CTEST_BINARY_DIRECTORY}\" --parallel ${jobs} --target install"
)
# Second build step for tests -- called explicitly after ctest_build()
set(_build_tests_command
    "\"${CTEST_CMAKE_COMMAND}\" --build \"${CTEST_BINARY_DIRECTORY}\" --parallel ${jobs} --target build-tests"
)

## ---------------------------------------------------------------------------
## 5. CMake 4.3 instrumentation: add cdashSubmit + dynamicSystemInformation
##    for this dashboard run.  CMakeLists.txt already enables trace +
##    staticSystemInformation for all builds; we add the CDash-specific
##    options here so they only apply when running via this script.
## ---------------------------------------------------------------------------
if(CMAKE_VERSION VERSION_GREATER_EQUAL "4.3")
  set(_query_dir
      "${CTEST_BINARY_DIRECTORY}/.cmake/instrumentation/v1/query")
  file(MAKE_DIRECTORY "${_query_dir}")
  file(WRITE "${_query_dir}/dashboard-cdash.json"
[=[{
  "version": 1,
  "options": [
    "dynamicSystemInformation",
    "cdashSubmit"
  ]
}
]=])
  message(STATUS "CDash instrumentation query written to ${_query_dir}/dashboard-cdash.json")
endif()

## ---------------------------------------------------------------------------
## 6. CTest Use Launchers
##    Enables per-compile-line error/warning capture in Build.xml on CDash.
## ---------------------------------------------------------------------------
set(CTEST_USE_LAUNCHERS ON)
set(ENV{CTEST_USE_LAUNCHERS_DEFAULT} 1)

## ---------------------------------------------------------------------------
## 7. CDash authentication token
##    Set CDASH_AUTH_TOKEN in your environment (never commit the value).
##    On GitHub Actions this comes from a repository secret.
##    CDash requires an HTTP Authorization Bearer header -- CTEST_AUTH_TOKEN
##    is not a real CMake variable; the token must be passed via HTTPHEADER
##    in every ctest_submit() call.
## ---------------------------------------------------------------------------
if(DEFINED ENV{CDASH_AUTH_TOKEN})
  set(_cdash_auth_header "Authorization: Bearer $ENV{CDASH_AUTH_TOKEN}")
  message(STATUS "CDash auth token loaded from CDASH_AUTH_TOKEN env var")
else()
  set(_cdash_auth_header "")
  message(WARNING
    "CDASH_AUTH_TOKEN is not set -- submission will likely be rejected.\n"
    "Generate a token at https://my.cdash.org then:\n"
    "  export CDASH_AUTH_TOKEN=<your-token>")
endif()

## ---------------------------------------------------------------------------
## 8. Dashboard pipeline
## ---------------------------------------------------------------------------
message(STATUS "=== MAPL CDash Dashboard ===")
message(STATUS "  Model:      ${model}")
message(STATUS "  Site:       ${CTEST_SITE}")
message(STATUS "  Build name: ${CTEST_BUILD_NAME}")
message(STATUS "  Source:     ${CTEST_SOURCE_DIRECTORY}")
message(STATUS "  Binary:     ${CTEST_BINARY_DIRECTORY}")
message(STATUS "  Generator:  ${_cmake_generator}")
message(STATUS "  Build type: ${build_type}")
message(STATUS "  Jobs:       ${jobs}")
message(STATUS "============================")

# Start the dashboard submission
ctest_start(${model})

# Update step (records git revision info; non-fatal if no remote)
ctest_update(RETURN_VALUE _update_result)
message(STATUS "Update result: ${_update_result}")

# Configure (runs cmake to generate build system)
ctest_configure(RETURN_VALUE _configure_result)
if(_configure_result)
  message(WARNING "Configure step returned ${_configure_result} -- submitting configure errors")
  if(_cdash_auth_header)
    ctest_submit(PARTS Configure HTTPHEADER "${_cdash_auth_header}")
  else()
    ctest_submit(PARTS Configure)
  endif()
  return()
endif()

# Build (install target)
ctest_build(
  NUMBER_ERRORS   _build_errors
  NUMBER_WARNINGS _build_warnings
  RETURN_VALUE    _build_result
)
message(STATUS "Build: ${_build_errors} error(s), ${_build_warnings} warning(s)")

# Stop early if the main build failed -- don't overwrite its log with the
# build-tests step, and don't attempt to run tests against broken binaries.
if(_build_errors GREATER 0)
  message(WARNING "Main build had ${_build_errors} error(s) -- skipping test step")
  if(_cdash_auth_header)
    ctest_submit(PARTS Configure Build HTTPHEADER "${_cdash_auth_header}"
                 RETURN_VALUE _submit_result)
  else()
    ctest_submit(PARTS Configure Build RETURN_VALUE _submit_result)
  endif()
  if(_submit_result)
    message(WARNING "CDash submission returned ${_submit_result}")
  else()
    message(STATUS "Build errors submitted to https://my.cdash.org/index.php?project=MAPL")
  endif()
  return()
endif()

# Build test executables (separate target in MAPL)
message(STATUS "Building test executables (build-tests target)...")
set(CTEST_BUILD_COMMAND "${_build_tests_command}")
ctest_build(
  TARGET          build-tests
  NUMBER_ERRORS   _build_tests_errors
  NUMBER_WARNINGS _build_tests_warnings
  RETURN_VALUE    _build_tests_result
  APPEND
)
message(STATUS "Build-tests: ${_build_tests_errors} error(s), ${_build_tests_warnings} warning(s)")

# Run ESSENTIAL tests only (matching CI behaviour).
# Run serially (parallel 1) as in CI to avoid MPI oversubscription issues.
ctest_test(
  INCLUDE_LABEL   "ESSENTIAL"
  PARALLEL_LEVEL  1
  RETURN_VALUE    _test_result
)
message(STATUS "Test result: ${_test_result}")

# If any tests failed, rerun only the failing ones once (matching CI behaviour)
if(_test_result)
  message(STATUS "Re-running only failing tests...")
  ctest_test(
    INCLUDE_LABEL  "ESSENTIAL"
    PARALLEL_LEVEL 1
    RERUN_FAILED
    RETURN_VALUE   _test_result
    APPEND
  )
  message(STATUS "Rerun result: ${_test_result}")
endif()

# Submit everything to CDash
if(_cdash_auth_header)
  ctest_submit(
    PARTS Configure Build Test
    HTTPHEADER "${_cdash_auth_header}"
    RETURN_VALUE _submit_result
  )
else()
  ctest_submit(
    PARTS Configure Build Test
    RETURN_VALUE _submit_result
  )
endif()
if(_submit_result)
  message(WARNING "CDash submission returned ${_submit_result}")
else()
  message(STATUS "Results submitted to https://my.cdash.org/index.php?project=MAPL")
endif()
