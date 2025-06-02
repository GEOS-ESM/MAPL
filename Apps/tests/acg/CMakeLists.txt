esma_set_this (OVERRIDE MAPL.Apps.tests.acg)

set (srcs
    ACG.F90
    )

add_library (${this} ${srcs})
target_link_libraries(${this} PRIVATE MAPL.shared MAPL.field MAPL.state MAPL.base MAPL ESMF::ESMF)
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>)
set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

mapl_acg (${this}   compile_test.acg
          IMPORT_SPECS acg_imports.h
          EXPORT_SPECS acg_exports.h
          INTERNAL_SPECS acg_internals.h
          GET_POINTERS acg_get_pointers.h
          DECLARE_POINTERS acg_declare_pointers.h
          )

# Add test 
add_test (NAME "${this}.compile"
  COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target ${this}
  )
set_tests_properties("${this}.compile" PROPERTIES LABELS "ESSENTIAL")
