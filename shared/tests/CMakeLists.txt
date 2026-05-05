set(MODULE_DIRECTORY "${esma_include}/${MAPL_TARGET_PREFIX}.shared/tests")

set (test_srcs
    test_String.pf
    test_TimeUtils.pf
    test_DSO_Utilities.pf
    test_FileSystemUtilities.pf
#    test_MAPL_ISO8601_DateTime.pf
    test_MAPL_DateTime_Parsing.pf
    test_MAPL_CF_Time.pf
    Test_Partition.pf
  )


add_pfunit_ctest(${MAPL_TARGET_PREFIX}.shared.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES ${MAPL_TARGET_PREFIX}.shared
                )
set_target_properties(${MAPL_TARGET_PREFIX}.shared.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
target_include_directories(${MAPL_TARGET_PREFIX}.shared.tests PRIVATE ${esma_include}/${MAPL_TARGET_PREFIX}.shared)
set_tests_properties(${MAPL_TARGET_PREFIX}.shared.tests PROPERTIES LABELS "ESSENTIAL")

add_dependencies(build-tests ${MAPL_TARGET_PREFIX}.shared.tests)
