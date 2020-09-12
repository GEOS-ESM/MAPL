add_definitions(-DUSE_MPI)
set(TEST_SRCS
    Test_NUOPCmap.pf
    Test_NUOPC_MAPLcap_Class.pf
    )

set(SRCS
    )


add_pfunit_ctest(MAPL.NUOPC.tests
    TEST_SOURCES ${TEST_SRCS}
    OTHER_SOURCES ${SRCS}
    LINK_LIBRARIES MAPL.NUOPC MAPL.base MAPL.pfio base_extras MAPL_pFUnit
    EXTRA_INITIALIZE Initialize
    EXTRA_USE MAPL_pFUnit_Initialize
    MAX_PES 4
    )

add_dependencies(build-tests MAPL.NUOPC.tests)

set_tests_properties(MAPL.NUOPC.tests PROPERTIES LABELS "NUOPC")
