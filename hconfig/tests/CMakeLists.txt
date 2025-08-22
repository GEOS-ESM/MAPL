set(MODULE_DIRECTORY "${esma_include}/MAPL.hconfig/tests")

set (TEST_SRCS
  Test_HConfigAs.pf
  )

add_pfunit_ctest(MAPL.hconfig.tests
                TEST_SOURCES ${TEST_SRCS}
                LINK_LIBRARIES MAPL.hconfig MAPL.shared MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 1
                )
set_target_properties(MAPL.hconfig.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.hconfig.tests PROPERTIES LABELS "ESSENTIAL")

add_dependencies(build-tests MAPL.hconfig.tests)
