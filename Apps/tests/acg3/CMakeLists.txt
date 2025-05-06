esma_set_this (OVERRIDE MAPL.Apps.tests.acg3)

set (srcs
    ACG3.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.field MAPL.state MAPL.generic3g MAPL TYPE SHARED)
target_link_libraries(${this} PRIVATE ESMF::ESMF)
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

mapl_acg (${this}   ACG_StateSpecs.acg
          IMPORT_SPECS acg3_imports.h
          EXPORT_SPECS acg3_exports.h
          INTERNAL_SPECS acg3_internal.h
          GET_POINTERS acg3_get_pointers.h
          DECLARE_POINTERS acg3_declare_pointers.h)
