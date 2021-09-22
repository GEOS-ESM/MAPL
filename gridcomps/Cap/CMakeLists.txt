esma_set_this (OVERRIDE MAPL.cap)

set (srcs
     MAPL_Cap.F90
     MAPL_CapGridComp.F90
     MAPL_NUOPCWrapperMod.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.constants MAPL.base MAPL.profiler MAPL.history
                  MAPL.ExtData TYPE ${MAPL_LIBRARY_TYPE})
target_link_libraries (${this} PUBLIC GFTL::gftl GFTL_SHARED::gftl-shared esmf NetCDF::NetCDF_Fortran
                               PRIVATE MPI::MPI_Fortran $<$<BOOL:${BUILD_WITH_FLAP}>:FLAP::FLAP>)

# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(${this} PRIVATE OpenMP::OpenMP_Fortran)
endif ()
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
