set(MODULE_DIRECTORY "${esma_include}/MAPL.regridder_mgr/tests")
set(this MAPL.regridder_mgr.tests)

set (TEST_SRCS
  Test_RouteHandleManager.pf
  Test_RegridderManager.pf
  )

add_pfunit_ctest(${this}
                TEST_SOURCES ${TEST_SRCS}
#                OTHER_SOURCES ${SRCS}
                LINK_LIBRARIES MAPL.regridder_mgr MAPL.geom MAPL.shared MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 8
                )
set_target_properties(${this} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(${this} PROPERTIES LABELS "ESSENTIAL")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  set_tests_properties(MAPL.regridder_mgr.tests PROPERTIES ENVIRONMENT "I_MPI_OFI_PROVIDER=psm3")
endif()

add_dependencies(build-tests ${this})


