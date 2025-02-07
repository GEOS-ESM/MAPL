set(MODULE_DIRECTORY "${esma_include}/MAPL.generic3g/tests")

add_subdirectory(gridcomps)

set (test_srcs
  Test_BaseAspect.pf
  Test_AspectMap.pf

  Test_VirtualConnectionPt.pf

  Test_ConfigurableGridComp.pf

  Test_ComponentSpecParser.pf
  Test_Aspects.pf
  Test_BracketClassAspect.pf
  Test_ExtensionFamily.pf

  Test_ConnectionPt.pf
  Test_FieldDictionary.pf

  Test_StateRegistry.pf

  Test_Scenarios.pf
  Test_WriteYaml.pf
  Test_HConfigMatch.pf

  Test_GenericGridComp.pf

  Test_TimeInterpolateAction.pf
  
  Test_ModelVerticalGrid.pf
  Test_FixedLevelsVerticalGrid.pf
  Test_VerticalLinearMap.pf

  Test_CSR_SparseMatrix.pf
  Test_AccumulatorAction.pf
  Test_MeanAction.pf
  Test_MaxAction.pf
  Test_MinAction.pf
  Test_ExtensionAction.pf
)

add_pfunit_ctest(
  MAPL.generic3g.tests
  TEST_SOURCES ${test_srcs}
  LINK_LIBRARIES MAPL.generic3g MAPL.shared MAPL.pfunit configurable_gridcomp
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  OTHER_SOURCES MockUserGridComp.F90 accumulator_action_test_common.F90 MockAspect.F90
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_target_properties(MAPL.generic3g.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.generic3g.tests PROPERTIES LABELS "ESSENTIAL")

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()

# This test also requires UDUNITS2_XML_PATH to be set to the location of the udunits2.xml file
# This is found by Findudunits.cmake and stored in the variable udunits_XML_PATH

set_tests_properties(
  MAPL.generic3g.tests
  PROPERTIES ENVIRONMENT "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/gridcomps:${CMAKE_BINARY_DIR}/lib:$ENV{${LD_PATH}};UDUNITS2_XML_PATH=${udunits_XML_PATH}"
)

add_dependencies(build-tests MAPL.generic3g.tests)

file(COPY scenarios DESTINATION .)

