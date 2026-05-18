set(MODULE_DIRECTORY "${esma_include}/MAPL.utils/tests")

set (TEST_SRCS
  Test_MaxMin.pf
  Test_AreaMean.pf
)

# Test_MemInfoWrite only works on Linux (requires /proc/self/status)
if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
  list(APPEND TEST_SRCS Test_MemInfoWrite.pf)
endif()

add_pfunit_ctest(MAPL.utils.tests
  TEST_SOURCES ${TEST_SRCS}
  LINK_LIBRARIES MAPL.utils MAPL.pfunit
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  MAX_PES 2
)
set_target_properties(MAPL.utils.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  set_tests_properties(MAPL.utils.tests PROPERTIES ENVIRONMENT "I_MPI_OFI_PROVIDER=psm3")
endif()

add_dependencies(build-tests MAPL.utils.tests)
