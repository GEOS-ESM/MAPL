esma_set_this (OVERRIDE MAPL.parent_no_children)
set (srcs
    ParentNoChildren_GridComp.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL TYPE ${MAPL_LIBRARY_TYPE})
target_link_libraries (${this} PRIVATE esmf $<$<BOOL:BUILD_WITH_OPENMP>:OpenMP::OpenMP_Fortran>)
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
#target_compile_definitions(${this} PRIVATE SYSTEM_DSO_SUFFIX="${CMAKE_SHARED_LIBRARY_SUFFIX}")
