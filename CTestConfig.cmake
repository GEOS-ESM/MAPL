## ---------------------------------------------------------------------------
## CTestConfig.cmake -- CDash submission configuration for MAPL
##
## This file is read by:
##   - CTest when run as a dashboard client (ctest -D Experimental, etc.)
##   - ctest -S dashboard scripts
##   - CMake's CTest module (include(CTest)) at configure time
##
## CDash project: https://my.cdash.org/index.php?project=MAPL
## ---------------------------------------------------------------------------

set(CTEST_PROJECT_NAME       "MAPL")
set(CTEST_NIGHTLY_START_TIME "00:00:00 UTC")

set(CTEST_DROP_METHOD        "https")
set(CTEST_DROP_SITE          "my.cdash.org")
set(CTEST_DROP_LOCATION      "/submit.php?project=MAPL")
set(CTEST_DROP_SITE_CDASH    TRUE)
