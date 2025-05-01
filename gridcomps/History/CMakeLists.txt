esma_set_this (OVERRIDE MAPL.history)

set (srcs
     MAPL_HistoryCollection.F90
     MAPL_HistoryGridComp.F90
     Sampler/MAPL_EpochSwathMod.F90
     Sampler/MAPL_MaskMod.F90
     Sampler/MAPL_MaskMod_smod.F90
     Sampler/MAPL_StationSamplerMod.F90
     Sampler/MAPL_TrajectoryMod.F90
     Sampler/MAPL_TrajectoryMod_smod.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.constants MAPL.base MAPL.generic MAPL.profiler MAPL.griddedio MAPL.state
                                                    TYPE SHARED)
target_link_libraries (${this} PUBLIC GFTL::gftl GFTL_SHARED::gftl-shared ESMF::ESMF NetCDF::NetCDF_Fortran
                               PRIVATE MPI::MPI_Fortran OpenMP::OpenMP_Fortran)
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
