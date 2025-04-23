esma_set_this (OVERRIDE MAPL.acg)

set (srcs
    ACG_GridComp.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL TYPE SHARED)

target_link_libraries(${this} PRIVATE ESMF::ESMF OpenMP::OpenMP_Fortran)

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

mapl_acg (${this}   ACG_StateSpecs.acg
          IMPORT_SPECS EXPORT_SPECS
          GET_POINTERS DECLARE_POINTERS)
