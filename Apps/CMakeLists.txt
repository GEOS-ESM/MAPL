#--------------------
# Copy include files that are used by other libraries.
# We could leave these in the source directory, and just broaden the search path
# in the other libaries, but this make it explicit which aspects are externally
# used.

file (COPY MAPL_GridCompSpecs_ACG.py DESTINATION ${esma_etc}/MAPL)
file (COPY MAPL_GridCompSpecs_ACGv3.py DESTINATION ${esma_etc}/MAPL)
file (COPY mapl_acg.pl DESTINATION ${esma_etc}/MAPL)
file (COPY mapl_stub.pl DESTINATION ${esma_etc}/MAPL)

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

esma_add_library(configurable_reader_gridcomp
  SRCS Regrid_Util/ConfigurableReaderGridComp.F90
  DEPENDENCIES MAPL.generic3g MAPL regrid_util_support_mod
  TYPE SHARED)

esma_add_library(configurable_writer_gridcomp
  SRCS Regrid_Util/ConfigurableWriterGridComp.F90
  DEPENDENCIES MAPL.generic3g MAPL regrid_util_support_mod
  TYPE SHARED)

esma_add_library(reader_writer_coupler_gridcomp
  SRCS Regrid_Util/ReaderWriterCouplerGridComp.F90
  DEPENDENCIES MAPL.generic3g MAPL configurable_reader_gridcomp configurable_writer_gridcomp
  TYPE SHARED)

esma_add_library(regrid_util_support_mod
  SRCS Regrid_Util/regrid_util_support_mod.F90
  DEPENDENCIES MAPL
  TYPE SHARED)

ecbuild_add_executable (TARGET Regrid_Util.x SOURCES Regrid_Util/Regrid_Util.F90)
target_link_libraries (Regrid_Util.x PRIVATE regrid_util_support_mod configurable_reader_gridcomp configurable_writer_gridcomp reader_writer_coupler_gridcomp MAPL MPI::MPI_Fortran ESMF::ESMF OpenMP::OpenMP_Fortran)
target_include_directories (Regrid_Util.x PRIVATE $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

ecbuild_add_executable (TARGET time_ave_util.x SOURCES time_ave_util.F90)
target_link_libraries (time_ave_util.x PRIVATE MAPL MPI::MPI_Fortran ESMF::ESMF OpenMP::OpenMP_Fortran)
target_include_directories (time_ave_util.x PRIVATE $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
