esma_set_this (OVERRIDE MAPL.history)

set (srcs
     MAPL_HistoryCollection.F90
     MAPL_HistoryGridComp.F90
     Sampler/MAPL_EpochSwathMod.F90
     Sampler/MAPL_GeosatMaskMod.F90
     Sampler/MAPL_GeosatMaskMod_smod.F90
     Sampler/MAPL_StationSamplerMod.F90
     Sampler/MAPL_TrajectoryMod.F90
     Sampler/MAPL_TrajectoryMod_smod.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.constants MAPL.base MAPL.generic MAPL.profiler MAPL.griddedio
                                                    TYPE ${MAPL_LIBRARY_TYPE})
target_link_libraries (${this} PUBLIC GFTL::gftl GFTL_SHARED::gftl-shared ESMF::ESMF NetCDF::NetCDF_Fortran
                               PRIVATE MPI::MPI_Fortran)

# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(${this} PRIVATE OpenMP::OpenMP_Fortran)
endif ()
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
