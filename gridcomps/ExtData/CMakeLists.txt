esma_set_this (OVERRIDE MAPL.ExtData)

set (srcs
        ExtData_IOBundleMod.F90
        ExtData_IOBundleVectorMod.F90
        ExtDataGridCompMod.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.constants MAPL.base MAPL.generic MAPL.pfio
        MAPL.griddedio MAPL_cfio_r4  TYPE SHARED)
target_link_libraries (${this} PUBLIC GFTL::gftl GFTL_SHARED::gftl-shared ESMF::ESMF NetCDF::NetCDF_Fortran
                               PRIVATE MPI::MPI_Fortran)

# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(${this} PRIVATE OpenMP::OpenMP_Fortran)
endif ()
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

# NOTE: ExtDataGridCompMod.F90 takes 192 seconds to compile at O3 and 12 seconds at O1
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel AND CMAKE_BUILD_TYPE MATCHES Release)
  set_source_files_properties(ExtDataGridCompMod.F90 PROPERTIES COMPILE_OPTIONS ${FOPT1})
endif ()
