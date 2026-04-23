## ---------------------------------------------------------------------------
## CTestCustom.cmake -- CTest warning/error suppression rules for MAPL
##
## This file is read by CTest from the BUILD directory at the start of each
## dashboard run.  It is copied there by CMake during configure (because it
## lives in the source root, CMake copies it automatically).
##
## See: https://cmake.org/cmake/help/latest/manual/ctest.1.html#customizing-testing
## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## Suppress known-noisy linker warnings on macOS (clang + gfortran toolchain)
##
## These all originate from ESMF and Baselibs CMake config files injecting
## raw -Wl,-rpath and -l flags multiple times into the transitive link
## closure.  They are upstream issues, not MAPL bugs.
## ---------------------------------------------------------------------------
list(APPEND CTEST_CUSTOM_WARNING_EXCEPTION
  # Apple ld: duplicate -rpath entries (same Baselibs lib dir added multiple times)
  "ld: warning: duplicate -rpath"

  # Apple ld: duplicate libraries (same -l flag appears more than once)
  "ld: warning: ignoring duplicate libraries"

  # Apple ld: gfortran Cellar path mismatch (15.2.0 vs 15.2.0_1 Homebrew suffix)
  "ld: warning: search path.*not found"
)

## ---------------------------------------------------------------------------
## Coverage exclusions: restrict gcov/lcov reporting to MAPL source only.
##
## CTEST_CUSTOM_COVERAGE_EXCLUDE is a list of regexes matched against the
## full path of each source file.  Files matching any pattern are excluded
## from the CDash coverage report.
##
## We exclude:
##   - System / compiler headers          (/usr/*)
##   - Baselibs installs (any prefix)     (*/installed/*)
##   - pFUnit framework internals         (*/pfunit/*)
##   - ESMF library sources               (*/ESMF/*)
##   - The MAPL install trees             (*/install-*/*)
##     (installed headers duplicated from build tree)
## ---------------------------------------------------------------------------
list(APPEND CTEST_CUSTOM_COVERAGE_EXCLUDE
  "/usr/"
  ".*/installed/.*"
  ".*/pfunit/.*"
  ".*/ESMF/.*"
  ".*/install-Coverage/.*"
  ".*/install-Coverage-Ninja/.*"
  ".*/install-Debug/.*"
  ".*/install-Release/.*"
)
