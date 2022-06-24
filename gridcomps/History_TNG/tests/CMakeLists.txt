add_definitions(-DUSE_MPI)

set (TEST_SRCS
    test_FieldEntry.pf
    test_FieldRegistry.pf
    test_FieldGroupEntry.pf
    test_FieldGroup.pf
    test_Group.pf
    test_GroupRegistry.pf
    test_Collection.pf
    test_CollectionRegistry.pf
    test_HistoryConfig.pf

    test_NUOPCmap.pf
)

add_pfunit_ctest(History_TNG_tests.x
        TEST_SOURCES ${TEST_SRCS}
        LINK_LIBRARIES MAPL MAPL.pfio MAPL.pfunit MAPL.history_tng
        EXTRA_INITIALIZE Initialize
        EXTRA_USE MAPL_pFUnit_Initialize
        MAX_PES 4
        )


add_dependencies(build-tests History_TNG_tests.x)

set_tests_properties(History_TNG_tests.x PROPERTIES LABELS "History_TNG")

configure_file(test_History.yaml test_History.yaml COPYONLY)
