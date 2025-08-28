set(MODULE_DIRECTORY "${esma_include}/MAPL.profiler/tests")

set (TEST_SRCS
  test_AdvancedMeter.pf
  test_NameColumn.pf
  test_ExclusiveColumn.pf
  test_PercentageColumn.pf
  test_TimeProfiler.pf
  test_ProfileReporter.pf
  test_MeterNode.pf
  test_MeterNodeIterator.pf
  test_DistributedMeter.pf
  )


add_pfunit_ctest (
  MAPL.profiler.tests
  TEST_SOURCES ${TEST_SRCS}
  LINK_LIBRARIES MAPL.profiler MAPL.pfunit
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  MAX_PES 8
  )
set_target_properties(MAPL.profiler.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.profiler.tests PROPERTIES LABELS "ESSENTIAL")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  set_tests_properties(MAPL.profiler.tests PROPERTIES ENVIRONMENT "I_MPI_OFI_PROVIDER=psm3")
endif()

add_dependencies (build-tests MAPL.profiler.tests)

