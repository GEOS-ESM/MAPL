# Build configuration for MAPL Orbital Grid Component tests

set(test_srcs
  Test_OrbHelperFunctions.pf
  Test_OrbMasking.pf
  Test_OrbGridComp.pf
)

# Additional sources (mock modules)
set(other_srcs
  mock_nominal_orbits.F90
)

# Create test executable
add_pfunit_ctest(
  OrbGridComp.tests
  TEST_SOURCES ${test_srcs}
  OTHER_SOURCES ${other_srcs}
  LINK_LIBRARIES MAPL.orbit MAPL.base MAPL.pfunit
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  MAX_PES 4
)

# Copy test configuration file to build directory
configure_file(
  MAPL_OrbGridComp.rc
  ${CMAKE_CURRENT_BINARY_DIR}/MAPL_OrbGridComp.rc
  COPYONLY
)
