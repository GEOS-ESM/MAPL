set(MODULE_DIRECTORY "${esma_include}/MAPL.geom/tests")

set (TEST_SRCS
  Test_GeomManager.pf
  Test_LatLonDecomposition.pf
  Test_CoordinateAxis.pf
  Test_LonAxis.pf
  Test_LatAxis.pf
  Test_LatLonGeomSpec.pf
  Test_LatLonGeomFactory.pf
  Test_CubedSphereGeomSpec.pf
  Test_CubedSphereGeomFactory.pf
  Test_GridGet.pf
  Test_LocStreamDecomposition.pf
  Test_LocStreamGeomFactory.pf
  Test_LocStreamGeomFactory_Metadata.pf
  Test_MeshDecomposition.pf
  Test_MeshGeomSpec.pf
  Test_MeshGeomFactory.pf
  Test_MeshGeomFactory_Metadata.pf
  Test_MeshGeomFactory_File.pf
)

set (OTHER_SRCS
  MeshTestHelper.F90
)

add_pfunit_ctest(MAPL.geom.tests
  TEST_SOURCES ${TEST_SRCS}
  OTHER_SOURCES ${OTHER_SRCS}
  LINK_LIBRARIES MAPL.geom MAPL.shared MAPL.pfunit
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  MAX_PES 8
)

set_target_properties(MAPL.geom.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.geom.tests PROPERTIES LABELS "ESSENTIAL")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  set_tests_properties(MAPL.geom.tests PROPERTIES ENVIRONMENT "I_MPI_OFI_PROVIDER=psm3")
endif()

add_dependencies(build-tests MAPL.geom.tests)
