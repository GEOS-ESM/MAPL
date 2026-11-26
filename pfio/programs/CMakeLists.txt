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

ecbuild_add_executable (
  TARGET nc4_parallel_cs_writer.x
  SOURCES nc4_parallel_cs_writer.F90
  LIBS NetCDF::NetCDF_Fortran NetCDF::NetCDF_C MPI::MPI_Fortran)

# ADIOS2 is optional; Baselibs installs its CMake config under ${BASEDIR}/lib/cmake/adios2.
find_package(adios2 QUIET COMPONENTS Fortran MPI HINTS ${BASEDIR})
if (adios2_FOUND)
  message(STATUS "ADIOS2 ${adios2_VERSION} found: building adios2_parallel_cs_writer.x")
  ecbuild_add_executable (
    TARGET adios2_parallel_cs_writer.x
    SOURCES adios2_parallel_cs_writer.F90
    LIBS adios2::fortran_mpi MPI::MPI_Fortran)
  # The static ADIOS2 libraries are C++, which would otherwise make CMake link with the C++ driver.
  set_target_properties (adios2_parallel_cs_writer.x PROPERTIES LINKER_LANGUAGE Fortran)
else ()
  message(STATUS "ADIOS2 not found: skipping adios2_parallel_cs_writer.x")
endif ()
