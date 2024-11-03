set(MODULE_DIRECTORY "${esma_include}/MAPL.field.tests")

add_pfunit_ctest(MAPL.field.test_fieldcreate
                TEST_SOURCES Test_FieldCreate.pf
                LINK_LIBRARIES MAPL.field MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 1
                )

add_pfunit_ctest(MAPL.field.test_fieldreset
                TEST_SOURCES Test_FieldReset.pf
                LINK_LIBRARIES MAPL.field MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 1
                )

add_dependencies(build-tests MAPL.field.test_fieldcreate)
