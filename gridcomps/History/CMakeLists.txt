esma_set_this (OVERRIDE MAPL.history)

#add_subdirectory(History)

set (srcs
     MAPL_HistoryCollection.F90
     History/checkIfStateHasField_History.F90
     History/CopyStateItems_History.F90
     History/get_acc_offset_History.F90
     History/get_DateStamp_History.F90
     History/Get_Tdim_History.F90 
     History/GradsCtlWrite_History.F90
     History/regen_rcx_for_obs_platform_History.F90
     History/RegridTransformT2G2G_History.F90
     History/RegridTransformT2G_History.F90
     History/RunExpression_History.F90
     History/SetExpression_History.F90
     History/shavebits_History.F90
     History/StateDestroy_History.F90
     History/StateGet_History.F90
     History/RegridTransform_History.F90
     History/record_restart_History.F90
     History/Initialize_History.F90
     History/Run_History.F90
     History/Finalize_History.F90
     History/SetServices_History.F90
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
