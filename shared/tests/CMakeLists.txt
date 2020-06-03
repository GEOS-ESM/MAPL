set (test_srcs
    test_String.pf
  )


add_pfunit_ctest(MAPL.shared.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.shared
                )

add_dependencies(tests MAPL.shared.tests)


