set(MODULE_DIRECTORY "${esma_include}/gridcomps/ExtData2G/tests")

set (test_srcs
  Test_ExtDataUpdatePointer.pf
  )

add_pfunit_ctest(MAPL.ExtData2G.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.ExtData2G MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                MAX_PES 1
                )
set_target_properties(MAPL.ExtData2G.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.ExtData2G.tests PROPERTIES LABELS "ESSENTIAL")

# With this test, it was shown that if you are building with the GNU Fortran
# compiler and *not* on APPLE, then you need to link with the dl library.
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT APPLE)
  target_link_libraries(MAPL.ExtData2G.tests ${CMAKE_DL_LIBS})
endif ()

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  set_tests_properties(MAPL.ExtData2G.tests PROPERTIES ENVIRONMENT "I_MPI_OFI_PROVIDER=psm3")
endif()

add_dependencies(build-tests MAPL.ExtData2G.tests)
