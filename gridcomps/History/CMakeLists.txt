esma_set_this (OVERRIDE MAPL.history)

#add_subdirectory(History)

set (srcs
     MAPL_HistoryCollection.F90
     history/checkIfStateHasField.F90
     history/CopyStateItems.F90
     history/get_acc_offset.F90
     history/get_DateStamp.F90
     history/Get_Tdim.F90 
     history/GradsCtlWrite.F90
     history/regen_rcx_for_obs_platform.F90
     history/RegridTransformT2G2G.F90
     history/RegridTransformT2G.F90
     history/RunExpression.F90
     history/SetExpression.F90
     history/shavebits.F90
     history/StateDestroy.F90
     history/StateGet.F90
     history/RegridTransform.F90
     history/record_restart.F90
     history/init.F90
     history/run.F90
     history/final.F90
     history/setservices.F90
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
