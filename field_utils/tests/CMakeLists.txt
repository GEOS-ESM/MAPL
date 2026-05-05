set(MODULE_DIRECTORY "${esma_include}/${MAPL_TARGET_PREFIX}.field_utils/tests")

set (test_srcs
  Test_FieldBLAS.pf
  Test_FieldArithmetic.pf
  )


add_pfunit_ctest(${MAPL_TARGET_PREFIX}.field_utils.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES ${MAPL_TARGET_PREFIX}.field_utils ${MAPL_TARGET_PREFIX}.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                OTHER_SOURCES field_utils_setup.F90
#		OTHER_SOURCES MockUserGridComp.F90 MockItemSpec.F90
                MAX_PES 4
                )
set_target_properties(${MAPL_TARGET_PREFIX}.field_utils.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
target_include_directories(${MAPL_TARGET_PREFIX}.field_utils.tests PRIVATE
  ${esma_include}/${MAPL_TARGET_PREFIX}.pfunit
  ${esma_include}/${MAPL_TARGET_PREFIX}.field_utils
  ${esma_include}/${MAPL_TARGET_PREFIX}.shared)
set_tests_properties(${MAPL_TARGET_PREFIX}.field_utils.tests PROPERTIES LABELS "ESSENTIAL")

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()
set_property(TEST ${MAPL_TARGET_PREFIX}.field_utils.tests PROPERTY ENVIRONMENT "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/field_utils:$ENV{${LD_PATH}}")

add_dependencies(build-tests ${MAPL_TARGET_PREFIX}.field_utils.tests)

