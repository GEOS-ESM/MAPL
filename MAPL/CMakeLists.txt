esma_set_this()


# GCHP note: customized
esma_add_library (${this}
  SRCS MAPL.F90
  DEPENDENCIES MAPL.base MAPL.pfio MAPL_cfio_r4 MAPL.gridcomps ESMF
   )
#---- MAPL original ----
#esma_add_library (${this}
#  SRCS MAPL.F90
#  DEPENDENCIES MAPL.base MAPL.pfio MAPL_cfio_r4 MAPL.gridcomps
#               esmf NetCDF::NetCDF_Fortran MPI::MPI_Fortran
#  TYPE SHARED
#  )
#-----------------------

# GCHP note: added next two lines
target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF})
target_link_libraries (${this} PUBLIC ${ESMF_LIBRARIES} ${NETCDF_LIBRARIES} ${MPI_Fortran_LIBRARIES})

target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
