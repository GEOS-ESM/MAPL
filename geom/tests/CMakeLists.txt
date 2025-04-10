set(MODULE_DIRECTORY "${esma_include}/MAPL.geom/tests")

set (TEST_SRCS
  Test_GeomManager.pf
  Test_LatLonDecomposition.pf
  Test_CoordinateAxis.pf
  Test_LonAxis.pf
  Test_LatAxis.pf
  Test_LatLonGeomFactory.pf
  Test_CubedSphereGeomFactory.pf
  )

add_pfunit_ctest(MAPL.geom.tests
                TEST_SOURCES ${TEST_SRCS}
#                OTHER_SOURCES ${SRCS}
                LINK_LIBRARIES MAPL.geom MAPL.shared MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 8
                )
set_target_properties(MAPL.geom.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.geom.tests PROPERTIES LABELS "ESSENTIAL")

add_dependencies(build-tests MAPL.geom.tests)


