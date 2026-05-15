set(MODULE_DIRECTORY "${esma_include}/MAPL.base3g/tests")

set(TEST_SRCS
  Test_PackedTime.pf
)

add_pfunit_ctest(MAPL.base3g.tests
  TEST_SOURCES ${TEST_SRCS}
  LINK_LIBRARIES MAPL.base3g MAPL.pfunit
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  MAX_PES 1
)
set_target_properties(MAPL.base3g.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

add_dependencies(build-tests MAPL.base3g.tests)
