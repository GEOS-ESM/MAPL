set(MODULE_DIRECTORY "${esma_include}/MAPL.generic3g/tests")

add_library(scratchpad SHARED scratchpad.F90)

add_subdirectory(gridcomps)

set (test_srcs
  Test_VirtualConnectionPt.pf
  
#  Test_AddVarSpec.pf
  Test_SimpleLeafGridComp.pf
  Test_SimpleParentGridComp.pf
  Test_Traverse.pf
  Test_RunChild.pf

  Test_AddFieldSpec.pf
  Test_ComponentSpecParser.pf

  Test_ConnectionPt.pf
  Test_FieldDictionary.pf
  Test_GenericInitialize.pf

  Test_HierarchicalRegistry.pf
  )


add_pfunit_ctest(MAPL.generic3g.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.generic3g MAPL.shared MAPL.pfunit scratchpad
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
		OTHER_SOURCES MockUserGridComp.F90 MockItemSpec.F90
                MAX_PES 4
                )
set_target_properties(MAPL.generic3g.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()
set_property(TEST MAPL.generic3g.tests PROPERTY ENVIRONMENT "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/gridcomps:$ENV{${LD_PATH}}")

add_dependencies(build-tests MAPL.generic3g.tests)

#add_custom_target(copy ALL COMMENT "Copying files: ${GLOBPAT}")
#add_target_d
#add_custom_command(
#  TARGET copy
#  COMMAND ${CMAKE_COMMAND} -E copy configs .)
#  )


file(COPY configs DESTINATION .)


add_executable(repro_alias.x repro_alias.F90)
target_link_libraries (repro_alias.x PUBLIC esmf)
