set(MODULE_DIRECTORY "${esma_include}/MAPL.utils/tests")

set (TEST_SRCS
  Test_MaxMin.pf
  Test_AreaMean.pf
  test_String.pf
  test_StringUtilities.pf
  test_DSO_Utilities.pf
  test_FileSystemUtilities.pf
#  test_MAPL_ISO8601_DateTime.pf
  test_MAPL_DateTime_Parsing.pf
  test_MAPL_CF_Time.pf
  Test_Partition.pf
  test_OS.pf
  test_sort.pf
  test_DirPath.pf
)

# Test_MemInfoWrite only works on Linux (requires /proc/self/status)
if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
  list(APPEND TEST_SRCS Test_MemInfoWrite.pf)
endif()

add_pfunit_ctest(MAPL.utils.tests
  TEST_SOURCES ${TEST_SRCS}
  LINK_LIBRARIES MAPL.utils MAPL.mp_utils MAPL.constants MAPL.pfunit
  EXTRA_INITIALIZE Initialize
  EXTRA_USE mapl_pFUnit_Initialize_mod
  MAX_PES 2
)
set_target_properties(MAPL.utils.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
target_include_directories(MAPL.utils.tests PRIVATE ${esma_include}/MAPL.utils)
set_tests_properties(MAPL.utils.tests PROPERTIES LABELS "ESSENTIAL")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  set_tests_properties(MAPL.utils.tests PROPERTIES ENVIRONMENT "I_MPI_OFI_PROVIDER=psm3")
endif()

add_dependencies(build-tests MAPL.utils.tests)

