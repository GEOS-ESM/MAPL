set(MODULE_DIRECTORY "${esma_include}/${MAPL_TARGET_PREFIX}.state/tests")

set (test_srcs
  Test_StateMask.pf
  Test_StateFilter.pf
  Test_StateArithmetic.pf
  )


add_pfunit_ctest(${MAPL_TARGET_PREFIX}.state.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES ${MAPL_TARGET_PREFIX}.state_utils ${MAPL_TARGET_PREFIX}.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                OTHER_SOURCES state_utils_setup.F90
                MAX_PES 1
                )
set_target_properties(${MAPL_TARGET_PREFIX}.state.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
target_include_directories(${MAPL_TARGET_PREFIX}.state.tests PRIVATE
  ${esma_include}/${MAPL_TARGET_PREFIX}.pfunit
  ${esma_include}/${MAPL_TARGET_PREFIX}.state_utils
  ${esma_include}/${MAPL_TARGET_PREFIX}.shared)
set_tests_properties(${MAPL_TARGET_PREFIX}.state.tests PROPERTIES LABELS "ESSENTIAL")

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()
set_property(TEST ${MAPL_TARGET_PREFIX}.state.tests PROPERTY ENVIRONMENT "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/state:$ENV{${LD_PATH}}")

add_dependencies(build-tests ${MAPL_TARGET_PREFIX}.state.tests)

