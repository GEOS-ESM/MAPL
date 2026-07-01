set(MODULE_DIRECTORY "${esma_include}/MAPL/tests")

add_pfunit_ctest(MAPL.mapl.server_utilities
  TEST_SOURCES Test_MaplServerUtilities.pf
  LINK_LIBRARIES MAPL MAPL.pfunit
  EXTRA_INITIALIZE Initialize
  EXTRA_USE mapl_pFUnit_Initialize_mod
  MAX_PES 1
)
set_target_properties(MAPL.mapl.server_utilities PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
target_include_directories(MAPL.mapl.server_utilities PRIVATE
  ${esma_include}/MAPL ${MAPL_SOURCE_DIR}/include)
set_tests_properties(MAPL.mapl.server_utilities PROPERTIES LABELS "ESSENTIAL")
add_dependencies(build-tests MAPL.mapl.server_utilities)
