set(exe checkpoint_simulator.x)

ecbuild_add_executable (
  TARGET ${exe}
  SOURCES checkpoint_simulator.F90 
  DEFINITIONS USE_MPI)

target_link_libraries (${exe} PRIVATE MAPL.shared MPI::MPI_Fortran FARGPARSE::fargparse esmf )
target_include_directories (${exe} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(${exe} PRIVATE OpenMP::OpenMP_Fortran)
endif ()
