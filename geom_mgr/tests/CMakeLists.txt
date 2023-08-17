set(MODULE_DIRECTORY "${esma_include}/MAPL.geom_mgr/tests")

set (TEST_SRCS
  Test_LatLonAxis.pf
  Test_LatLonGeomSpec.pf
  #    Test_LatLonGeomFactory.pf
  )

add_pfunit_ctest(MAPL.geom_mgr.tests
                TEST_SOURCES ${TEST_SRCS}
#                OTHER_SOURCES ${SRCS}
                LINK_LIBRARIES MAPL.geom_mgr MAPL.shared MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 8
                )
set_target_properties(MAPL.geom_mgr.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

add_dependencies(build-tests MAPL.geom_mgr.tests)


