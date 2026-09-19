set(MODULE_DIRECTORY "${esma_include}/MAPL.generic.graph.tests")

add_library(MAPL.generic.graph.test_support SHARED
  IdGeneratorTestSupport.F90
  MockPayload.F90
  MockPayloadConcrete.F90
  NodeIdMockPayloadMap.F90
  StateItemTestSupport.F90
  GraphCoreTestInit.F90
)
target_link_libraries(MAPL.generic.graph.test_support
  PUBLIC MAPL.generic MAPL.pfunit ESMF::ESMF
)
set_target_properties(MAPL.generic.graph.test_support PROPERTIES
  Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY}
)
target_include_directories(MAPL.generic.graph.test_support PUBLIC
  $<BUILD_INTERFACE:${MODULE_DIRECTORY}>
)

set(test_srcs
  Test_NodeId.pf
  Test_DependencyNetworkId.pf
  Test_PortId.pf
  Test_CallbackInterfaceId.pf
  Test_NodeIdMapCompatibility.pf
  Test_NodeIdLabelMap.pf
  Test_GraphNodeHierarchy.pf
  Test_GraphStateItem.pf
  Test_NodeRevision.pf
  Test_DependencyNetwork.pf
  Test_ComponentGraph.pf
  Test_ComponentGraph_Enumeration.pf
  Test_TransformGraphNode.pf
  Test_ComponentGraph_PortBindings.pf
  Test_DemandDrivenUpdate.pf
  Test_AccessSpec.pf
  Test_ArgumentSpec.pf
  Test_GridCompMethodInvocation.pf
  Test_StateMethodInvocation.pf
  Test_MethodGraphNode.pf
  Test_GraphExport.pf
  Test_Characteristic.pf
  Test_ExtensionResolution.pf
  Test_ExtensionMaterialization.pf
  Test_UnitsConverterTransform.pf
)

add_pfunit_ctest(MAPL.generic.graph
  TEST_SOURCES ${test_srcs}
  LINK_LIBRARIES MAPL.generic.graph.test_support
  EXTRA_USE GraphCoreTestInit_mod
  EXTRA_INITIALIZE GraphCoreTest_Initialize
  EXTRA_FINALIZE GraphCoreTest_Finalize
  MAX_PES 1
)
set_tests_properties(MAPL.generic.graph PROPERTIES LABELS "ESSENTIAL")
# Test_UnitsConvertTransform.pf needs udunits2f initialized
# (GraphCoreTestInit.F90), which needs the udunits2.xml database location -
# same convention as udunits2f/tests/CMakeLists.txt and
# superstructure/generic/tests/CMakeLists.txt's own TEST_ENV.
set_tests_properties(MAPL.generic.graph PROPERTIES ENVIRONMENT "UDUNITS2_XML_PATH=${udunits_XML_PATH}")

add_dependencies(build-tests MAPL.generic.graph)
