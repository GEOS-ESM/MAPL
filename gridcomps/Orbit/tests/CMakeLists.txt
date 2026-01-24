# Build configuration for MAPL Orbital Grid Component tests

set(test_srcs
  Test_OrbHelperFunctions.pf
  Test_OrbMasking.pf
  Test_OrbMasking_Regression.pf
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

# Copy regression test reference data files to build directory
file(GLOB REFERENCE_FILES "reference_data/ref_*.txt")
foreach(ref_file ${REFERENCE_FILES})
  get_filename_component(filename ${ref_file} NAME)
  configure_file(
    ${ref_file}
    ${CMAKE_CURRENT_BINARY_DIR}/${filename}
    COPYONLY
  )
endforeach()

# Build standalone program to generate reference masks
# First compile mock_nominal_orbits as a separate object
add_library(mock_orbits OBJECT mock_nominal_orbits.F90)
target_link_libraries(mock_orbits PUBLIC MAPL.base)
set_target_properties(mock_orbits PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_executable(generate_reference_masks 
  generate_reference_masks.F90
  $<TARGET_OBJECTS:mock_orbits>
)

target_link_libraries(generate_reference_masks MAPL.orbit MAPL.base ESMF::ESMF)
target_include_directories(generate_reference_masks PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
set_target_properties(generate_reference_masks PROPERTIES
  LINKER_LANGUAGE Fortran
  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
add_dependencies(generate_reference_masks mock_orbits)
