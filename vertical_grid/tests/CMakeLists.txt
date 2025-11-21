set(MODULE_DIRECTORY "${esma_include}/MAPL.vertical_grid/tests")

set (TEST_SRCS
  Test_VerticalStaggerLoc.pf
  Test_BasicVerticalGrid.pf
  Test_FixedLevelsVerticalGrid.pf
  Test_VerticalGridManager.pf
  )

add_pfunit_ctest(MAPL.vertical_grid.tests
                TEST_SOURCES ${TEST_SRCS}
#                OTHER_SOURCES ${SRCS}
                LINK_LIBRARIES MAPL.vertical_grid MAPL.shared MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 8
                )
set_target_properties(MAPL.vertical_grid.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.vertical_grid.tests PROPERTIES LABELS "ESSENTIAL")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  set_tests_properties(MAPL.vertical_grid.tests PROPERTIES ENVIRONMENT "I_MPI_OFI_PROVIDER=psm3")
endif()

add_dependencies(build-tests MAPL.vertical_grid.tests)
