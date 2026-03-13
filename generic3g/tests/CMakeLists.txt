set(MODULE_DIRECTORY "${esma_include}/MAPL.generic3g/tests")

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

# Create a shared library for common test sources to avoid parallel build race conditions
# When multiple test suites compile the same OTHER_SOURCES files to the same MODULE_DIRECTORY,
# they can conflict during parallel builds
add_library(MAPL.generic3g.test_common SHARED
  MockUserGridComp.F90
  accumulator_transform_test_common.F90
  MockAspect.F90
)
target_link_libraries(MAPL.generic3g.test_common PUBLIC MAPL.generic3g MAPL.shared MAPL.pfunit)
set_target_properties(MAPL.generic3g.test_common PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

# Common dependencies for all test suites
set(COMMON_LINK_LIBRARIES MAPL.generic3g.test_common MAPL.pfunit configurable_gridcomp MAPL_StatisticsGridComp)

# 1. MAPL.generic3g.scenarios - End-to-end integration test scenarios
set(scenarios_test_srcs
  Test_Scenarios.pf
)

add_pfunit_ctest(
  MAPL.generic3g.scenarios
  TEST_SOURCES ${scenarios_test_srcs}
  LINK_LIBRARIES ${COMMON_LINK_LIBRARIES}
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_target_properties(MAPL.generic3g.scenarios PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.generic3g.scenarios PROPERTIES LABELS "ESSENTIAL")
add_dependencies(MAPL.generic3g.scenarios proto_extdata_gc proto_stat_gc copy_scenarios)

# 2. MAPL.generic3g.transforms - Data transformation tests
set(transforms_test_srcs
  Test_AccumulatorTransform.pf
  Test_MeanTransform.pf
  Test_MaxTransform.pf
  Test_MinTransform.pf
  Test_TimeInterpolateTransform.pf
  Test_ConvertUnitsTransform.pf
  Test_CopyTransform.pf
  Test_ExtensionTransform.pf
  Test_ExtensionTransformUtils.pf
  Test_2DConservativeRegrid.pf
  Test_NormalizationTransform.pf
  Test_3DConservativeMixingRatio.pf
)

add_pfunit_ctest(
  MAPL.generic3g.transforms
  TEST_SOURCES ${transforms_test_srcs}
  LINK_LIBRARIES ${COMMON_LINK_LIBRARIES}
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_target_properties(MAPL.generic3g.transforms PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.generic3g.transforms PROPERTIES LABELS "ESSENTIAL")
add_dependencies(MAPL.generic3g.transforms proto_extdata_gc proto_stat_gc)

# 3. MAPL.generic3g.vertical - Vertical regridding and grid tests
set(vertical_test_srcs
  Test_VerticalRegridTransform.pf
  Test_ModelVerticalGrid.pf
  Test_VerticalLinearMap.pf
  Test_VerticalConservativeMap.pf
)

add_pfunit_ctest(
  MAPL.generic3g.vertical
  TEST_SOURCES ${vertical_test_srcs}
  LINK_LIBRARIES ${COMMON_LINK_LIBRARIES}
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_target_properties(MAPL.generic3g.vertical PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.generic3g.vertical PROPERTIES LABELS "ESSENTIAL")
add_dependencies(MAPL.generic3g.vertical proto_extdata_gc proto_stat_gc)

# 4. MAPL.generic3g.aspects - Aspect system tests
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
  Test_ExportNormalization.pf
  Test_ImportNormalization.pf
)

add_pfunit_ctest(
  MAPL.generic3g.aspects
  TEST_SOURCES ${aspects_test_srcs}
  LINK_LIBRARIES ${COMMON_LINK_LIBRARIES}
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_target_properties(MAPL.generic3g.aspects PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.generic3g.aspects PROPERTIES LABELS "ESSENTIAL")
add_dependencies(MAPL.generic3g.aspects proto_extdata_gc proto_stat_gc)

# 5. MAPL.generic3g.components - Component and specification tests
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
  MAPL.generic3g.components
  TEST_SOURCES ${components_test_srcs}
  LINK_LIBRARIES ${COMMON_LINK_LIBRARIES}
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_target_properties(MAPL.generic3g.components PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.generic3g.components PROPERTIES LABELS "ESSENTIAL")
add_dependencies(MAPL.generic3g.components proto_extdata_gc proto_stat_gc)

# 6. MAPL.generic3g.core - Core functionality tests
set(core_test_srcs
  Test_VirtualConnectionPt.pf
  Test_ConnectionPt.pf
  Test_FieldDictionary.pf
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
  MAPL.generic3g.core
  TEST_SOURCES ${core_test_srcs}
  LINK_LIBRARIES ${COMMON_LINK_LIBRARIES}
  EXTRA_INITIALIZE Initialize
  EXTRA_USE MAPL_pFUnit_Initialize
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAX_PES 4
)
set_target_properties(MAPL.generic3g.core PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.generic3g.core PROPERTIES LABELS "ESSENTIAL")
add_dependencies(MAPL.generic3g.core proto_extdata_gc proto_stat_gc)


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
set_tests_properties(MAPL.generic3g.scenarios PROPERTIES ENVIRONMENT "${TEST_ENV}")
set_tests_properties(MAPL.generic3g.transforms PROPERTIES ENVIRONMENT "${TEST_ENV}")
set_tests_properties(MAPL.generic3g.vertical PROPERTIES ENVIRONMENT "${TEST_ENV}")
set_tests_properties(MAPL.generic3g.aspects PROPERTIES ENVIRONMENT "${TEST_ENV}")
set_tests_properties(MAPL.generic3g.components PROPERTIES ENVIRONMENT "${TEST_ENV}")
set_tests_properties(MAPL.generic3g.core PROPERTIES ENVIRONMENT "${TEST_ENV}")

# Add all test suites to build-tests target
add_dependencies(build-tests 
  MAPL.generic3g.scenarios
  MAPL.generic3g.transforms
  MAPL.generic3g.vertical
  MAPL.generic3g.aspects
  MAPL.generic3g.components
  MAPL.generic3g.core
)
