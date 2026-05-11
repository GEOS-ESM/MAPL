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
  Test_MeshGeomFactory_Distribution.pf
  Test_XYGeomSpec.pf
  Test_XYGeomFactory.pf
  Test_EASEGeomSpec.pf
  Test_EASEGeomFactory.pf
)

set (OTHER_SRCS
  MeshTestHelper.F90
  XYTestHelper.F90
)

# ---------------------------------------------------------------------------
# Standalone MPI integration test for CubedSphereGeomSpec%get_horz_ij_index
# (moved from Tests/GetHorzIJIndex; uses MAPL_Cap with 6 PEs)
# ---------------------------------------------------------------------------

set (GETHORZIJINDEX_SRCS
  GetHorzIJIndexGridComp.F90
  GetHorzIJIndexDriver.F90
)

ecbuild_add_executable(
  TARGET GetHorzIJIndexDriver.x
  SOURCES ${GETHORZIJINDEX_SRCS})
target_link_libraries(GetHorzIJIndexDriver.x PRIVATE MAPL MAPL.geom FARGPARSE::fargparse ESMF::ESMF OpenMP::OpenMP_Fortran)

add_dependencies(build-tests GetHorzIJIndexDriver.x)

# Detect Open MPI and add oversubscribe flag if needed
string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)
if(MPI_Fortran_LIBRARY_VERSION_FIRSTWORD MATCHES "Open")
  list(APPEND MPIEXEC_PREFLAGS "-oversubscribe")
endif()

set (GETHORZIJINDEX_TEST_CASES NO_OMP OMP_1_thread OMP_4_thread)

foreach(TEST_CASE ${GETHORZIJINDEX_TEST_CASES})
  add_test(
    NAME "GetHorzIJIndex_${TEST_CASE}"
    COMMAND ${CMAKE_COMMAND}
      -DTEST_CASE=${TEST_CASE}
      -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE}
      -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
      -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
      -DMPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}
      -DTEST_CASES_DIR=${CMAKE_CURRENT_SOURCE_DIR}/GetHorzIJIndex_test_cases
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_gethorzijindex.cmake
  )
  set_tests_properties("GetHorzIJIndex_${TEST_CASE}" PROPERTIES LABELS "THREADING_TESTS;ESSENTIAL")
endforeach()

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
