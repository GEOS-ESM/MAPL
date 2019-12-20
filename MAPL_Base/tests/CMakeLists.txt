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
  MAPL_Initialize.F90
  MockGridFactory.F90
  MockRegridder.F90
  )

add_pfunit_ctest(MAPL_Base_tests 
                TEST_SOURCES ${TEST_SRCS}
                OTHER_SOURCES ${SRCS}
                LINK_LIBRARIES MAPL_Base GMAO_pFIO MAPL_pFUnit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUNIT_Initialize
                MAX_PES 8
                )

add_dependencies(tests MAPL_Base_tests)


