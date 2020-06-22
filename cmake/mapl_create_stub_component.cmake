macro (mapl_create_stub_component srcs module)

  list (APPEND ${srcs} ${module}.F90)
  set(STUB_GENERATOR ${esma_etc}/MAPL/MAPL_stub.pl)

  add_custom_command (
    OUTPUT ${module}.F90
    COMMAND ${STUB_GENERATOR} ${module}Mod > ${module}.F90
    MAIN_DEPENDENCY ${STUB_GENERATOR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Making component stub for ${module}Mod in ${module}.F90"
    )

  add_custom_target(stub_${module} DEPENDS ${module}.F90)

endmacro ()

