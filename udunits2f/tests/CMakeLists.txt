set(MODULE_DIRECTORY "${esma_include}/udunits2f.tests")

set (test_srcs
  Test_UDSystem.pf
  Test_udunits2f.pf
  )

add_pfunit_ctest(udunits2f.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES udunits2f
                )
set_target_properties(udunits2f.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

add_dependencies(build-tests udunits2f.tests)

