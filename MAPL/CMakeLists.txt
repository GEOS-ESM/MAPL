esma_set_this()


esma_add_library (${this}
  SRCS MAPL.F90
  DEPENDENCIES MAPL.base MAPL.pfio MAPL_cfio_r4 MAPL.gridcomps
               esmf NetCDF::NetCDF_Fortran MPI::MPI_Fortran
  TYPE SHARED
  )

target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
