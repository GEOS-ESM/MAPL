set(MODULE_DIRECTORY "${esma_include}/MAPL.state/tests")

set (test_srcs
  Test_StateMask.pf
  Test_StateFilter.pf
  Test_StateArithmetic.pf
  )


add_pfunit_ctest(MAPL.state.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.state_utils MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                OTHER_SOURCES state_utils_setup.F90
                MAX_PES 1
                )
set_target_properties(MAPL.state.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.state.tests PROPERTIES LABELS "ESSENTIAL")

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()
set_property(TEST MAPL.state.tests PROPERTY ENVIRONMENT "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/state:$ENV{${LD_PATH}}")

add_dependencies(build-tests MAPL.state.tests)

