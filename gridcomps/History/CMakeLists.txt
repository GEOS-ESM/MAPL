esma_set_this (OVERRIDE MAPL.history)

set (srcs
     MAPL_HistoryGridComp.F90
     MAPL_HistoryCollection.F90 MAPL_HistoryTrajectoryMod.F90)

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.base)
target_link_libraries (${this} PUBLIC gftl gftl-shared ${ESMF_LIBRARIES}
                               PRIVATE OpenMP::OpenMP_Fortran MPI::MPI_Fortran)
target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF}
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
