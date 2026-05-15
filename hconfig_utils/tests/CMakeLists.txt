set(MODULE_DIRECTORY "${esma_include}/hconfig_utils/tests")

set (test_srcs
  Test_hconfig_get_private.pf
  Test_HConfigUtilities.pf
  )


add_pfunit_ctest(MAPL.hconfig_utils.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.hconfig_utils MAPL.shared MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                MAX_PES 4
                )
set_tests_properties(MAPL.hconfig_utils.tests PROPERTIES LABELS "ESSENTIAL")
set_target_properties(MAPL.hconfig_utils.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()

# We need to build up a variable to pass to set_tests_properties

set(TEST_ENV "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/gridcomps:$ENV{${LD_PATH}}")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  list(APPEND TEST_ENV "I_MPI_OFI_PROVIDER=psm3")
endif()
set_tests_properties(MAPL.hconfig_utils.tests PROPERTIES ENVIRONMENT "${TEST_ENV}")

add_dependencies(build-tests MAPL.hconfig_utils.tests)
