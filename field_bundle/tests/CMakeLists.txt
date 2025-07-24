set(MODULE_DIRECTORY "${esma_include}/MAPL.field_bundle.tests")

add_pfunit_ctest(MAPL.field_bundle.tests
                TEST_SOURCES   Test_FieldBundleDelta.pf
                LINK_LIBRARIES MAPL.field_bundle MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 4
                )
add_dependencies(build-tests MAPL.field_bundle.tests)

set_target_properties(MAPL.field_bundle.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.field_bundle.tests PROPERTIES LABELS "ESSENTIAL")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  set_tests_properties(MAPL.field_bundle.tests PROPERTIES ENVIRONMENT "I_MPI_OFI_PROVIDER=psm3")
endif()

apply_esmf_fpe0_workaround()
