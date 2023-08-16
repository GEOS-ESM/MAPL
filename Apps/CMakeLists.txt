#--------------------
# Copy include files that are used by other libraries.
# We could leave these in the source directory, and just broaden the search path
# in the other libaries, but this make it explicit which aspects are externally
# used.

file (COPY MAPL_GridCompSpecs_ACG.py DESTINATION ${esma_etc}/MAPL)
file (COPY mapl_acg.pl DESTINATION ${esma_etc}/MAPL)
file (COPY mapl_stub.pl DESTINATION ${esma_etc}/MAPL)

install (PROGRAMS
  MAPL_GridCompSpecs_ACG.py
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

ecbuild_add_executable (TARGET Regrid_Util.x SOURCES Regrid_Util.F90)
target_link_libraries (Regrid_Util.x PRIVATE MAPL MPI::MPI_Fortran esmf)
target_include_directories (Regrid_Util.x PRIVATE $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
  target_link_libraries(Regrid_Util.x PRIVATE OpenMP::OpenMP_Fortran)
endif ()

ecbuild_add_executable (TARGET time_ave_util.x SOURCES time_ave_util.F90)
target_link_libraries (time_ave_util.x PRIVATE MAPL MPI::MPI_Fortran esmf)
target_include_directories (time_ave_util.x PRIVATE $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
  target_link_libraries(time_ave_util.x PRIVATE OpenMP::OpenMP_Fortran)
endif ()

ecbuild_add_executable (TARGET Comp_Testing_Driver.x SOURCES Comp_Testing_Driver.F90)
target_link_libraries (Comp_Testing_Driver.x PRIVATE MAPL MPI::MPI_Fortran esmf)
target_include_directories (Comp_Testing_Driver.x PRIVATE $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
  target_link_libraries(Comp_Testing_Driver.x PRIVATE OpenMP::OpenMP_Fortran)
endif ()
