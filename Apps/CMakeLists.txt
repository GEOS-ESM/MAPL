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

ecbuild_add_executable (TARGET Regrid_Util.x SOURCES Regrid_Util.F90 DEPENDS esmf )
target_link_libraries (Regrid_Util.x PRIVATE MAPL ${MPI_Fortran_LIBRARIES})
# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
           target_link_libraries(Regrid_Util.x PRIVATE OpenMP::OpenMP_Fortran)
   endif ()

ecbuild_add_executable (TARGET time_ave_util.x SOURCES time_ave_util.F90 DEPENDS esmf )
target_link_libraries (time_ave_util.x PRIVATE MAPL ${MPI_Fortran_LIBRARIES})
# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
           target_link_libraries(time_ave_util.x PRIVATE OpenMP::OpenMP_Fortran)
   endif ()
