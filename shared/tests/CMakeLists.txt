set(MODULE_DIRECTORY "${esma_include}/MAPL.shared/tests")

set (test_srcs
    test_String.pf
    test_StringUtilities.pf
    test_TimeUtils.pf
    test_DSO_Utilities.pf
    test_FileSystemUtilities.pf
#    test_MAPL_ISO8601_DateTime.pf
    test_MAPL_DateTime_Parsing.pf
    test_MAPL_CF_Time.pf
    test_OS.pf
    Test_Partition.pf
  )


add_pfunit_ctest(MAPL.shared.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.shared
                )
set_target_properties(MAPL.shared.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.shared.tests PROPERTIES LABELS "ESSENTIAL")

add_dependencies(build-tests MAPL.shared.tests)
