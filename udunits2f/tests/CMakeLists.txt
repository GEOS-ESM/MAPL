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
set_tests_properties(udunits2f.tests PROPERTIES LABELS "ESSENTIAL")

# With this test, it was shown that if you are building with the GNU Fortran
# compiler and *not* on APPLE, then you need to link with the dl library.
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT APPLE)
  target_link_libraries(udunits2f.tests ${CMAKE_DL_LIBS})
endif ()

# This test requires UDUNITS2_XML_PATH to be set to the location of the udunits2.xml file
# This is found by Findudunits.cmake and stored in the variable udunits_XML_PATH
set_tests_properties(udunits2f.tests PROPERTIES ENVIRONMENT "UDUNITS2_XML_PATH=${udunits_XML_PATH}")

add_dependencies(build-tests udunits2f.tests)

