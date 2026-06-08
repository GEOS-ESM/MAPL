set(MODULE_DIRECTORY "${esma_include}/MAPL.generic/tests")

add_subdirectory(gridcomps)

# Copy scenarios directory to build directory at build time
# Define this early so it can be used as a dependency
# Using COMMAND_EXPAND_LISTS and VERBATIM for robustness
add_custom_target(copy_scenarios ALL
  COMMAND ${CMAKE_COMMAND} -E copy_directory
    ${CMAKE_CURRENT_SOURCE_DIR}/scenarios
    ${CMAKE_CURRENT_BINARY_DIR}/scenarios
  COMMENT "Copying scenarios directory to build directory"
  VERBATIM
)

# Copy field_dictionary_test.yaml to the binary dir so tests that call
# load_field_dictionary can find it by relative path at runtime.
configure_file(field_dictionary_test.yaml field_dictionary_test.yaml COPYONLY)

# Create a shared library for common test sources to avoid parallel build race conditions
# When multiple test suites compile the same OTHER_SOURCES files to the same MODULE_DIRECTORY,
# they can conflict during parallel builds
add_library(MAPL.generic.test_common SHARED
  MockUserGridComp.F90
  MockAspect.F90
)
target_link_libraries(MAPL.generic.test_common PUBLIC MAPL.generic MAPL.utils MAPL.pfunit)
set_target_properties(MAPL.generic.test_common PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
target_include_directories(MAPL.generic.test_common PUBLIC $<BUILD_INTERFACE:${MODULE_DIRECTORY}>)

# Common dependencies for all test suites
set(COMMON_LINK_LIBRARIES MAPL.generic.test_common MAPL.pfunit configurable_gridcomp MAPL.statistics)

# 1. MAPL.generic.scenarios - End-to-end integration test scenarios
set(scenarios_test_srcs
  Test_Scenarios.pf
)

add_pfunit_ctest(
  MAPL.generic.scenarios
  TEST_SOURCES ${scenarios_test_srcs}
  LINK_LIBRARIES ${COMMON_LINK_LIBRARIES}
  EXTRA_INITIALIZE Initialize
  EXTRA_USE mapl_pFUnit_Initialize_mod
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_tests_properties(MAPL.generic.scenarios PROPERTIES LABELS "ESSENTIAL")
add_dependencies(MAPL.generic.scenarios proto_extdata_gc proto_stat_gc copy_scenarios)

# 2. MAPL.generic.transforms - Data transformation tests
set(transforms_test_srcs
  Test_TimeInterpolateTransform.pf
  Test_ConvertUnitsTransform.pf
  Test_CopyTransform.pf
  Test_ExtensionTransform.pf
  Test_ExtensionTransformUtils.pf
  Test_2DConservativeRegrid.pf
  Test_NormalizationTransform.pf
  Test_3DConservativeMixingRatio.pf
  Test_IntegratedNormalization.pf
  Test_RegridTransform.pf
)

add_pfunit_ctest(
  MAPL.generic.transforms
  TEST_SOURCES ${transforms_test_srcs}
  LINK_LIBRARIES ${COMMON_LINK_LIBRARIES}
  EXTRA_INITIALIZE Initialize
  EXTRA_USE mapl_pFUnit_Initialize_mod
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_tests_properties(MAPL.generic.transforms PROPERTIES LABELS "ESSENTIAL")
add_dependencies(MAPL.generic.transforms proto_extdata_gc proto_stat_gc)

# 3. MAPL.generic.vertical - Vertical regridding and grid tests
set(vertical_test_srcs
  Test_VerticalRegridTransform.pf
  Test_ModelVerticalGrid.pf
  Test_VerticalLinearMap.pf
  Test_VerticalConservativeMap.pf
)

add_pfunit_ctest(
  MAPL.generic.vertical
  TEST_SOURCES ${vertical_test_srcs}
  LINK_LIBRARIES ${COMMON_LINK_LIBRARIES}
  EXTRA_INITIALIZE Initialize
  EXTRA_USE mapl_pFUnit_Initialize_mod
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_tests_properties(MAPL.generic.vertical PROPERTIES LABELS "ESSENTIAL")
add_dependencies(MAPL.generic.vertical proto_extdata_gc proto_stat_gc)

# 4. MAPL.generic.aspects - Aspect system tests
set(aspects_test_srcs
  Test_BaseAspect.pf
  Test_AspectMap.pf
  Test_Aspects.pf
  Test_TypekindAspect.pf
  Test_UngriddedDimsAspect.pf
  Test_BracketClassAspect.pf
  Test_VectorBracketClassAspect.pf
  Test_UnitsAspect.pf
  Test_QuantityTypeAspect.pf
  Test_GeomAspect.pf
  Test_ConservationAspect.pf
  Test_NormalizationAspect.pf
)

add_pfunit_ctest(
  MAPL.generic.aspects
  TEST_SOURCES ${aspects_test_srcs}
  LINK_LIBRARIES ${COMMON_LINK_LIBRARIES}
  EXTRA_INITIALIZE Initialize
  EXTRA_USE mapl_pFUnit_Initialize_mod
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_tests_properties(MAPL.generic.aspects PROPERTIES LABELS "ESSENTIAL")
add_dependencies(MAPL.generic.aspects proto_extdata_gc proto_stat_gc)

# 5. MAPL.generic.components - Component and specification tests
set(components_test_srcs
  Test_ConfigurableGridComp.pf
  Test_GenericGridComp.pf
  Test_ComponentSpec.pf
  Test_ComponentSpecParser.pf
  Test_WriteYaml.pf
  Test_HConfigMatch.pf
  Test_ExtensionFamily.pf
)

add_pfunit_ctest(
  MAPL.generic.components
  TEST_SOURCES ${components_test_srcs}
  LINK_LIBRARIES ${COMMON_LINK_LIBRARIES}
  EXTRA_INITIALIZE Initialize
  EXTRA_USE mapl_pFUnit_Initialize_mod
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_tests_properties(MAPL.generic.components PROPERTIES LABELS "ESSENTIAL")
add_dependencies(MAPL.generic.components proto_extdata_gc proto_stat_gc)

# 6. MAPL.generic.core - Core functionality tests
set(core_test_srcs
  Test_VirtualConnectionPt.pf
  Test_ConnectionPt.pf
  Test_FieldDictionary.pf
  Test_FieldDictIntegration.pf
  Test_StateRegistry.pf
  Test_Couplers.pf
  Test_CSR_SparseMatrix.pf
  Test_timestep_propagation.pf
  Test_propagate_time_varying.pf
  Test_ClockGet.pf
  Test_VariableSpec_private.pf
  Test_VectorBasisKind.pf
  Test_QuantityType.pf
  Test_ConservationType.pf
  Test_PolymorphicSetMirror.pf
)

add_pfunit_ctest(
  MAPL.generic.core
  TEST_SOURCES ${core_test_srcs}
  LINK_LIBRARIES ${COMMON_LINK_LIBRARIES}
  EXTRA_INITIALIZE Initialize
  EXTRA_USE mapl_pFUnit_Initialize_mod
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_tests_properties(MAPL.generic.core PROPERTIES LABELS "ESSENTIAL")
add_dependencies(MAPL.generic.core proto_extdata_gc proto_stat_gc)


if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()

# All test suites require UDUNITS2_XML_PATH to be set to the location of the udunits2.xml file
# This is found by Findudunits.cmake and stored in the variable udunits_XML_PATH

set(TEST_ENV "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/gridcomps:${CMAKE_BINARY_DIR}/lib:$ENV{${LD_PATH}};UDUNITS2_XML_PATH=${udunits_XML_PATH}")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  list(APPEND TEST_ENV "I_MPI_OFI_PROVIDER=psm3")
endif()

# Apply environment settings to all test suites
set_tests_properties(MAPL.generic.scenarios PROPERTIES ENVIRONMENT "${TEST_ENV}")
set_tests_properties(MAPL.generic.transforms PROPERTIES ENVIRONMENT "${TEST_ENV}")
set_tests_properties(MAPL.generic.vertical PROPERTIES ENVIRONMENT "${TEST_ENV}")
set_tests_properties(MAPL.generic.aspects PROPERTIES ENVIRONMENT "${TEST_ENV}")
set_tests_properties(MAPL.generic.components PROPERTIES ENVIRONMENT "${TEST_ENV}")
set_tests_properties(MAPL.generic.core PROPERTIES ENVIRONMENT "${TEST_ENV}")

# Add all test suites to build-tests target
add_dependencies(build-tests 
  MAPL.generic.scenarios
  MAPL.generic.transforms
  MAPL.generic.vertical
  MAPL.generic.aspects
  MAPL.generic.components
  MAPL.generic.core
)
