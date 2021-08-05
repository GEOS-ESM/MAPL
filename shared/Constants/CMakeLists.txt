esma_set_this (OVERRIDE MAPL.constants)

set (srcs
    InternalConstants.F90
    MathConstants.F90
    PhysicalConstants.F90
    Constants.F90
    )

esma_add_library (${this} SRCS ${srcs} TYPE ${MAPL_LIBRARY_TYPE})

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
