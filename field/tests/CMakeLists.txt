set(MODULE_DIRECTORY "${esma_include}/MAPL.field/tests")

add_pfunit_ctest(MAPL.field.test_fieldcreate
                TEST_SOURCES Test_FieldCreate.pf
                LINK_LIBRARIES MAPL.field MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 1
                )
set_target_properties(MAPL.field.test_fieldcreate PROPERTIES Fortran_MODULE_DIRECTORY "${MODULE_DIRECTORY}/create")
set_tests_properties(MAPL.field.test_fieldcreate PROPERTIES LABELS "ESSENTIAL")

add_pfunit_ctest(MAPL.field.test_utils
                TEST_SOURCES   Test_FieldBLAS.pf Test_FieldArithmetic.pf Test_FieldCondensedArray_private.pf
                               Test_FieldDelta.pf Test_FieldInfo.pf
                LINK_LIBRARIES MAPL.field MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                OTHER_SOURCES field_utils_setup.F90
                MAX_PES 4
                )
set_target_properties(MAPL.field.test_utils PROPERTIES Fortran_MODULE_DIRECTORY "${MODULE_DIRECTORY}/utils")
set_tests_properties(MAPL.field.test_utils PROPERTIES LABELS "ESSENTIAL")

add_dependencies(build-tests MAPL.field.test_fieldcreate MAPL.field.test_utils)

