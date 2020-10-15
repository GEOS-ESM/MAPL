add_definitions(-DUSE_MPI)
set(TEST_SRCS
    Test_NUOPCmap.pf
    Test_NUOPC_MAPLcap_Class.pf
    Test_NUOPC_MAPLconfig.pf
    Test_NUOPC_MAPLfields.pf
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

add_dependencies(build-tests base_extras MAPL.base.tests MAPL.NUOPC.tests)

target_link_libraries (base_extras PUBLIC ${ESMF_LIBRARIES} MAPL.base MAPL.NUOPC MAPL_pFUnit)

set_tests_properties(MAPL.NUOPC.tests PROPERTIES LABELS "NUOPC")

configure_file(NUOPC_config.yaml        NUOPC_config.yaml        COPYONLY)
configure_file(test_NUOPC_config.rc     test_NUOPC_config.rc     COPYONLY)
configure_file(test_NUOPC_config_bad.rc test_NUOPC_config_bad.rc COPYONLY)
