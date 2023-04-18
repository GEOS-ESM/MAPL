set(MODULE_DIRECTORY "${esma_include}/MAPL.geom/tests")

set (test_srcs
  Test_FieldBLAS.pf
  )


add_pfunit_ctest(MAPL.geom.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.geom MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
#		OTHER_SOURCES MockUserGridComp.F90 MockItemSpec.F90
                MAX_PES 4
                )
set_target_properties(MAPL.geom.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()
set_property(TEST MAPL.geom.tests PROPERTY ENVIRONMENT "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/geom:$ENV{${LD_PATH}}")

add_dependencies(build-tests MAPL.geom.tests)

