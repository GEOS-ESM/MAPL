set(MODULE_DIRECTORY "${esma_include}/MAPL_StatisticsGridComp.tests")

set (test_srcs)
  Test_ShiftedVariance.pf
  Test_WelfordVariance.pf
  Test_Variance.pf
  )

add_pfunit_ctest(MAPL_StatisticsGridComp.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL_StatisticsGridComp MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                MAX_PES 1
                )
set_target_properties(MAPL_StatisticsGridComp.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL_StatisticsGridComp.tests PROPERTIES LABELS "ESSENTIAL")

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()

set(TEST_ENV "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/gridcomps:$ENV{${LD_PATH}}")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  list(APPEND TEST_ENV "I_MPI_OFI_PROVIDER=psm3")
endif()

set_property(TEST MAPL_StatisticsGridComp.tests PROPERTY ENVIRONMENT "${TEST_ENV}")

add_dependencies(build-tests MAPL_StatisticsGridComp.tests)
