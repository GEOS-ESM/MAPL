set(MODULE_DIRECTORY "${esma_include}/MAPL.shared/tests")

set (test_srcs
    test_String.pf
    test_TimeUtils.pf
  )


add_pfunit_ctest(MAPL.shared.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.shared
                )
set_target_properties(MAPL.shared.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

add_dependencies(build-tests MAPL.shared.tests)
