set(MODULE_DIRECTORY "${esma_include}/MAPL.base/tests")

set(TEST_SRCS
  Test_PackedTime.pf
)

add_pfunit_ctest(MAPL.base.tests
  TEST_SOURCES ${TEST_SRCS}
  LINK_LIBRARIES MAPL.base MAPL.pfunit
  EXTRA_INITIALIZE Initialize
  EXTRA_USE mapl_pFUnit_Initialize_mod
  MAX_PES 1
)
set_target_properties(MAPL.base.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

add_dependencies(build-tests MAPL.base.tests)
