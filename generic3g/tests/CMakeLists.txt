set(MODULE_DIRECTORY "${esma_include}/MAPL.generic3g/tests")

set (test_srcs
    Test_ConcreteComposite.pf
    Test_CompositeComponent.pf
    Test_VarSpec.pf
  )


add_pfunit_ctest(MAPL.generic3g.tests
                TEST_SOURCES ""
                LINK_LIBRARIES MAPL.generic3g MAPL.shared MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
		OTHER_SOURCES MockUserGridComp.F90
                MAX_PES 1
                )
set_target_properties(MAPL.generic3g.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

add_dependencies(build-tests MAPL.generic3g.tests)
