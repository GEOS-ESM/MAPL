#--------------------
# Copy include files that are used by other libraries.
# We could leave these in the source directory, and just broaden the search path
# in the other libaries, but this make it explicit which aspects are externally
# used.
#
# Note: We use configure_file(... COPYONLY) instead of file(COPY ...) here
# so that CMake registers these scripts as dependencies. If you edit the source 
# scripts, the build system will automatically detect the change and re-copy 
# them to the build/install directories without needing a manual re-run of CMake.

configure_file (MAPL_GridCompSpecs_ACG.py ${esma_etc}/MAPL/MAPL_GridCompSpecs_ACG.py COPYONLY)
configure_file (MAPL_GridCompSpecs_ACGv3.py ${esma_etc}/MAPL/MAPL_GridCompSpecs_ACGv3.py COPYONLY)
configure_file (mapl_acg.pl ${esma_etc}/MAPL/mapl_acg.pl COPYONLY)
configure_file (mapl_stub.pl ${esma_etc}/MAPL/mapl_stub.pl COPYONLY)

install (PROGRAMS
  MAPL_GridCompSpecs_ACG.py
  MAPL_GridCompSpecs_ACGv3.py
  combine_restarts.py
  split_restart.py
  mapl_acg.pl
  mapl_stub.pl
  TYPE SYSCONF
  )

install(
  PROGRAMS forcing_converter/binary_forcing_converter.py
  PROGRAMS forcing_converter/ll_formatter.py
  PROGRAMS forcing_converter/cs_formatter.py
  PROGRAMS forcing_converter/tile_formatter.py
  PROGRAMS forcing_converter/utilities.py
  DESTINATION bin/forcing_converter)

ecbuild_add_executable (TARGET Regrid_Util.x SOURCES Regrid_Util/Regrid_Util.F90)
target_link_libraries (Regrid_Util.x PRIVATE MAPL MAPL.base MAPL.profiler MPI::MPI_Fortran ESMF::ESMF OpenMP::OpenMP_Fortran)
target_include_directories (Regrid_Util.x PRIVATE $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

#ecbuild_add_executable (TARGET time_ave_util.x SOURCES time_ave_util.F90)
#target_link_libraries (time_ave_util.x PRIVATE MAPL2 MPI::MPI_Fortran ESMF::ESMF OpenMP::OpenMP_Fortran)
#target_include_directories (time_ave_util.x PRIVATE $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
