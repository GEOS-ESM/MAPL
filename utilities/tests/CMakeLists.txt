set(MODULE_DIRECTORY "${esma_include}/MAPL.utilities/tests")

set (TEST_SRCS
  Test_MaxMin.pf
  Test_AreaMean.pf
)

add_pfunit_ctest(MAPL.utilities.tests
  TEST_SOURCES ${TEST_SRCS}
  LINK_LIBRARIES MAPL.utilities MAPL.pfunit
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  MAX_PES 2
)
set_target_properties(MAPL.utilities.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.utilities.tests PROPERTIES LABELS "ESSENTIAL")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  set_tests_properties(MAPL.utilities.tests PROPERTIES ENVIRONMENT "I_MPI_OFI_PROVIDER=psm3")
endif()

add_dependencies(build-tests MAPL.utilities.tests)
