set (test_srcs
    test_String.pf
    Test_Simple_CommSplitter.pf
  )


add_pfunit_ctest(MAPL.shared.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.shared
                )

add_dependencies(build-tests MAPL.shared.tests)


