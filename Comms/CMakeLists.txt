esma_set_this ()

set (srcs
  MAPL_Comms.F90
  )
esma_add_library (${this} SRCS ${srcs} DEPENDENCIES ${ESMF_LIBRARIES} Shared pflogger  gftl-shared gftl MPI::MPI_Fortran)
target_include_directories (${this} PRIVATE ${MAPL_SOURCE_DIR}/include ${INC_ESMF} ${INC_NETCDF})
