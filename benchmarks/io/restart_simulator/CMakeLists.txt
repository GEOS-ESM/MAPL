set(exe restart_simulator.x)
set(MODULE_DIRECTORY ${esma_include}/benchmarks/io/restart_simulator)

ecbuild_add_executable (
  TARGET ${exe}
  SOURCES restart_simulator.F90
  DEFINITIONS USE_MPI)

target_link_libraries (${exe} PRIVATE MAPL.shared MAPL.base MPI::MPI_Fortran FARGPARSE::fargparse ESMF::ESMF )
target_include_directories (${exe} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
set_target_properties (${exe} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(${exe} PRIVATE OpenMP::OpenMP_Fortran)
endif ()
