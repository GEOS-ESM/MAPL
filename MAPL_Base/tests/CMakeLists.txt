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
    Test_VarSpec.pf
#    Test_SimpleCommSplitter.pf
  )

# SRCS are mostly mocks to facilitate tests
set (SRCS
  MockGridFactory.F90
  MockRegridder.F90
  )

# This file needs to be in a library because CMake cannot detect the
# dependency of the pFUnit driver on it.  This is due to the use of
# preprocesor in the driver for specifying the include file.
add_library (base_extras
  MAPL_Initialize.F90
  )
target_link_libraries (base_extras PUBLIC ${ESMF_LIBRARIES} MAPL_Base MAPL_pFUnit)
target_include_directories (base_extras PUBLIC ${INC_ESMF})
target_include_directories (base_extras PUBLIC ${INC_NETCDF})


add_pfunit_ctest(MAPL_Base_tests 
                TEST_SOURCES ${TEST_SRCS}
                OTHER_SOURCES ${SRCS}
                LINK_LIBRARIES MAPL_Base GMAO_pFIO base_extras MAPL_pFUnit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 8
                )

add_dependencies(tests MAPL_Base_tests)


