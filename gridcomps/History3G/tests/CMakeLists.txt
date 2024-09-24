set(MODULE_DIRECTORY "${esma_include}/MAPL.history3g.tests")

set (test_srcs
  Test_HistoryGridComp.pf
  Test_HistoryCollectionGridComp.pf
  Test_FieldDimensionInfo.pf
  )

add_pfunit_ctest(MAPL.history3g.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.history3g MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                MAX_PES 1
                )
set_target_properties(MAPL.history3g.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.history3g.tests PROPERTIES LABELS "ESSENTIAL")

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()
set_property(TEST MAPL.history3g.tests PROPERTY ENVIRONMENT "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/gridcomps:$ENV{${LD_PATH}}")

add_dependencies(build-tests MAPL.history3g.tests)

