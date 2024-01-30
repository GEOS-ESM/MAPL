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

# With this test, it was shown that if you are building with the GNU Fortran
# compiler and *not* on APPLE, then you need to link with the dl library.
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT APPLE)
  target_link_libraries(udunits2f.tests ${CMAKE_DL_LIBS})
endif ()

add_dependencies(build-tests udunits2f.tests)

