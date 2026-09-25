set(MODULE_DIRECTORY "${esma_include}/MAPL.GeomIO/tests")

set (test_srcs
  Test_SharedIO.pf
  Test_FieldBundleIO.pf
  )

add_pfunit_ctest(MAPL.GeomIO.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.GeomIO MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE mapl_pFUnit_Initialize_mod
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                MAX_PES 1
                )
set_target_properties(MAPL.GeomIO.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.GeomIO.tests PROPERTIES LABELS "ESSENTIAL")

# A separate executable/target from MAPL.GeomIO.tests above: that target
# also links a real pfio client/server pair whose internal singleton
# assumes it owns the entire VM. Bumping its MAX_PES beyond 1 to
# accommodate a multi-PE test deadlocks the *other*, pre-existing tests
# in that target (e.g. Test_FieldBundleIO.pf), which is why
# Test_pFIOServerBounds.pf (whose highest-PE-count case needs 6 PEs for
# a cubed-sphere geom) gets its own target/communicator here instead.
add_pfunit_ctest(MAPL.pFIOServerBounds.tests
                TEST_SOURCES Test_pFIOServerBounds.pf
                LINK_LIBRARIES MAPL.GeomIO MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE mapl_pFUnit_Initialize_mod
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                MAX_PES 6
                )
set_target_properties(MAPL.pFIOServerBounds.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.pFIOServerBounds.tests PROPERTIES LABELS "ESSENTIAL")

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()
#set_property(TEST MAPL.GeomIO.tests PROPERTY ENVIRONMENT "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/gridcomps:$ENV{${LD_PATH}}")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  set_tests_properties(MAPL.GeomIO.tests PROPERTIES ENVIRONMENT "I_MPI_OFI_PROVIDER=psm3")
  set_tests_properties(MAPL.pFIOServerBounds.tests PROPERTIES ENVIRONMENT "I_MPI_OFI_PROVIDER=psm3")
endif()

add_dependencies(build-tests MAPL.GeomIO.tests MAPL.pFIOServerBounds.tests)
