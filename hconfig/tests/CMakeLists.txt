set(MODULE_DIRECTORY "${esma_include}/hconfig/tests")

set (test_srcs
  Test_hconfig_get.pf
  )


add_pfunit_ctest(MAPL.hconfig.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.hconfig MAPL.shared MAPL.pfunit
                EXTRA_USE MAPL_pFUnit_Initialize
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                MAX_PES 4
                )
set_target_properties(MAPL.hconfig.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()
set_property(TEST MAPL.hconfig.tests PROPERTY ENVIRONMENT "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/gridcomps:$ENV{${LD_PATH}}")

add_dependencies(build-tests MAPL.hconfig.tests)
