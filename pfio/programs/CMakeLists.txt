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

ecbuild_add_executable (
  TARGET nc4_multifile_cs_writer.x
  SOURCES nc4_multifile_cs_writer.F90
  LIBS NetCDF::NetCDF_Fortran NetCDF::NetCDF_C MPI::MPI_Fortran)

# ADIOS2 is optional; Baselibs installs its CMake config under ${BASEDIR}/lib/cmake/adios2.
find_package(adios2 QUIET COMPONENTS Fortran MPI HINTS ${BASEDIR})
if (adios2_FOUND)
  message(STATUS "ADIOS2 ${adios2_VERSION} found: building the ADIOS2 cs_writer programs")
  foreach (prog adios2_parallel_cs_writer adios2_multifile_cs_writer)
    ecbuild_add_executable (
      TARGET ${prog}.x
      SOURCES ${prog}.F90
      LIBS adios2::fortran_mpi MPI::MPI_Fortran)
    # The static ADIOS2 libraries are C++, which would otherwise make CMake link with the C++ driver.
    set_target_properties (${prog}.x PROPERTIES LINKER_LANGUAGE Fortran)
  endforeach ()

  # NetCDF-4 plugin engine, loaded at run time through PluginLibrary. With static
  # ADIOS2/NetCDF the plugin takes their symbols from the executable (ENABLE_EXPORTS),
  # so the executable links all of the ADIOS2 C++ binding and the NetCDF calls it uses.
  add_library (adios2_nc4_plugin MODULE adios2_nc4_plugin.cpp)
  target_include_directories (adios2_nc4_plugin PRIVATE
    $<TARGET_PROPERTY:adios2::cxx11_mpi,INTERFACE_INCLUDE_DIRECTORIES>
    $<TARGET_PROPERTY:NetCDF::NetCDF_C,INTERFACE_INCLUDE_DIRECTORIES>)
  target_compile_definitions (adios2_nc4_plugin PRIVATE ADIOS2_USE_MPI)
  target_link_libraries (adios2_nc4_plugin PRIVATE MPI::MPI_CXX)
  set_target_properties (adios2_nc4_plugin PROPERTIES
    CXX_STANDARD 14
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

  foreach (prog adios2_nc4plugin_cs_writer adios2_nc4plugin_multifile_cs_writer)
    ecbuild_add_executable (
      TARGET ${prog}.x
      SOURCES ${prog}.F90
      LIBS adios2::fortran_mpi "$<LINK_LIBRARY:WHOLE_ARCHIVE,adios2::cxx>"
           NetCDF::NetCDF_C MPI::MPI_Fortran)
    set_target_properties (${prog}.x PROPERTIES
      LINKER_LANGUAGE Fortran ENABLE_EXPORTS ON)
    foreach (sym nc_create_par nc_set_fill nc_def_dim nc_def_var nc_def_var_chunking nc_put_att
                 nc_put_att_text nc_enddef nc_redef nc_var_par_access nc_put_vara nc_close nc_strerror)
      target_link_options (${prog}.x PRIVATE "LINKER:-u,${sym}")
    endforeach ()
    target_link_libraries (${prog}.x stdc++)
    add_dependencies (${prog}.x adios2_nc4_plugin)
  endforeach ()
else ()
  message(STATUS "ADIOS2 not found: skipping the ADIOS2 cs_writer programs")
endif ()
