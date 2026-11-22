set(MODULE_DIRECTORY "${esma_include}/callback_test_gridcomp.tests")

set (test_srcs
  Test_ConfigurableCallbackHandler.pf
  )

add_pfunit_ctest(callback_test_gridcomp.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES callback_test_gridcomp MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE mapl_pFUnit_Initialize_mod
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                MAX_PES 1
                )
set_target_properties(callback_test_gridcomp.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(callback_test_gridcomp.tests PROPERTIES LABELS "ESSENTIAL")

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()

set(TEST_ENV "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/gridcomps:$ENV{${LD_PATH}}")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  list(APPEND TEST_ENV "I_MPI_OFI_PROVIDER=psm3")
endif()

set_property(TEST callback_test_gridcomp.tests PROPERTY ENVIRONMENT "${TEST_ENV}")

add_dependencies(build-tests callback_test_gridcomp.tests)
