set(MODULE_DIRECTORY "${esma_include}/MAPL.state/tests")

set (test_srcs
  Test_StateMask.pf
  Test_StateFilter.pf
  Test_StateArithmetic.pf
  Test_StateDestroy.pf
  Test_StateGetBundle.pf
  Test_StateGetGeom.pf
  Test_StateGetPointerVector.pf
  Test_StateMerge.pf
  Test_StateVariant.pf
  Test_ConfigurableCallback.pf)

add_pfunit_ctest(MAPL.state.tests
  TEST_SOURCES ${test_srcs}
  LINK_LIBRARIES MAPL.state MAPL.geom MAPL.pfunit
  EXTRA_INITIALIZE Initialize
  EXTRA_USE mapl_pFUnit_Initialize_mod
  OTHER_SOURCES state_utils_setup.F90
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 1)

# Test_ConfigurableCallback reads these by relative path at run time.
add_custom_target(MAPL.state.tests.copy_scenarios
  COMMAND ${CMAKE_COMMAND} -E copy_directory
    ${CMAKE_CURRENT_SOURCE_DIR}/configurable_callback_scenarios
    ${CMAKE_CURRENT_BINARY_DIR}/configurable_callback_scenarios
  COMMENT "Copying configurable callback test scenarios to build directory"
  VERBATIM
)
add_dependencies(MAPL.state.tests MAPL.state.tests.copy_scenarios)

set_target_properties(MAPL.state.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
target_include_directories(MAPL.state.tests PRIVATE
  ${esma_include}/MAPL.pfunit
  ${esma_include}/MAPL.state_utils
  ${esma_include}/MAPL.shared)
set_tests_properties(MAPL.state.tests PROPERTIES LABELS "ESSENTIAL")

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()

set(TEST_ENV "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/state:$ENV{${LD_PATH}}")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  list(APPEND TEST_ENV "I_MPI_OFI_PROVIDER=psm3")
endif()

set_property(TEST MAPL.state.tests PROPERTY ENVIRONMENT "${TEST_ENV}")

add_dependencies(build-tests MAPL.state.tests)

