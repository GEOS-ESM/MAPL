set(MODULE_DIRECTORY "${esma_include}/MAPL.base/tests")

add_definitions(-DUSE_MPI)
set (TEST_SRCS
    test_Mapl_Base.pf
    test_sort.pf
#    Test_CFIO_Bundle.pf
    Test_SimpleMAPLcomp.pf
    Test_StringGridFactoryMap.pf
    Test_GridManager.pf
    Test_LatLon_GridFactory.pf
    Test_SphericalToCartesian.pf
    Test_LatLon_Corners.pf
    Test_MAPL_Config.pf
    test_DirPath.pf
  )

# SRCS are mostly mocks to facilitate tests
set (SRCS
  MockGridFactory.F90
  MockRegridder.F90
  )

# This file needs to be in a library because CMake cannot detect the
# dependency of the pFUnit driver on it.  This is due to the use of
# preprocesor in the driver for specifying the include file.
#add_library (base_extras
#  MAPL_Initialize.F90
#  )
#target_link_libraries (base_extras MAPL.shared MAPL.pfunit
#                                   esmf NetCDF::NetCDF_Fortran)

add_pfunit_ctest(MAPL.base.tests
                TEST_SOURCES ${TEST_SRCS}
                OTHER_SOURCES ${SRCS}
#                LINK_LIBRARIES MAPL.base MAPL.shared MAPL.pfio base_extras MAPL.pfunit
                LINK_LIBRARIES MAPL.base MAPL.shared MAPL.constants MAPL.pfio MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 8
                )
set_target_properties(MAPL.base.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

add_dependencies(build-tests MAPL.base.tests)

set(TESTIO mapl_bundleio_test.x)
ecbuild_add_executable (
  TARGET ${TESTIO}
  NOINSTALL
  SOURCES mapl_bundleio_test.F90
  LIBS MAPL.base MAPL.shared MAPL.constants MAPL.pfio MAPL.griddedio NetCDF::NetCDF_Fortran MPI::MPI_Fortran
  DEFINITIONS USE_MPI)
set_target_properties(${TESTIO} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

add_test(NAME bundleio_tests_latlon
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TESTIO} -nx 2 -ny 1 -ogrid PC90x47-DE)

add_test(NAME bundleio_tests_cube
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 6 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TESTIO} -nx 1 -ny 6 -ogrid PE12x72-CF)

add_dependencies(build-tests ${TESTIO})

