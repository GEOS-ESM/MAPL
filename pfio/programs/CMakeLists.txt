set(this MAPL.pfio)

ecbuild_add_executable (
   TARGET pfio_open_close.x
   SOURCES pfio_open_close.F90
   LIBS ${this})
set_target_properties (pfio_open_close.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

ecbuild_add_executable (
   TARGET pfio_server_demo.x
   SOURCES pfio_server_demo.F90
   LIBS ${this} MPI::MPI_Fortran)
set_target_properties (pfio_server_demo.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
target_link_libraries(pfio_server_demo.x OpenMP::OpenMP_Fortran)

ecbuild_add_executable (
   TARGET pfio_collective_demo.x
   SOURCES pfio_collective_demo.F90
   LIBS ${this} MPI::MPI_Fortran)
set_target_properties (pfio_collective_demo.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
target_link_libraries(pfio_collective_demo.x OpenMP::OpenMP_Fortran)

ecbuild_add_executable (
  TARGET pfio_writer.x
  SOURCES pfio_writer.F90
  LIBS ${this} NetCDF::NetCDF_Fortran NetCDF::NetCDF_C MPI::MPI_Fortran)
set_target_properties (pfio_writer.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
