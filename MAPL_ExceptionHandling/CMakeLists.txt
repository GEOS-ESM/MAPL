esma_set_this ()

set (srcs
  MAPL_ExceptionHandling.F90
  MAPL_ErrorHandling.F90
  MAPL_Throw.F90
  )
esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MPI::MPI_Fortran)
#esma_add_library (${this} SRCS ${srcs} DEPENDENCIES gftl-shared gftl MPI::MPI_Fortran)
target_include_directories (${this} PRIVATE ${MAPL_SOURCE_DIR}/include)

file (COPY MAPL_ErrLogMain.h DESTINATION ${esma_include}/${this})
file (COPY MAPL_ErrLog.h DESTINATION ${esma_include}/${this})
file (COPY MAPL_Exceptions.h DESTINATION ${esma_include}/${this})

#if (PFUNIT_FOUND)
  #add_subdirectory (tests)
#endif ()

  
