set (test_srcs
    Test_ConcreteComposite.pf
    Test_CompositeComponent.pf
  )


add_pfunit_ctest(MAPL.generic.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.generic MAPL.shared MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
		MAX_PES 1
                )

add_dependencies(build-tests MAPL.generic.tests)
	      


