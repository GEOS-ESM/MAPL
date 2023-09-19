set(MODULE_DIRECTORY "${esma_include}/MAPL.benchmarks.raw_bw")

set(exe combo.x)

ecbuild_add_executable (
  TARGET ${exe}
  SOURCES Kernel.F90 GathervKernel.F90 BW_Benchmark.F90 ComboSpec.F90 driver.F90
  DEFINITIONS USE_MPI)

target_link_libraries (${exe} PRIVATE MAPL.shared MPI::MPI_Fortran FARGPARSE::fargparse)
target_include_directories (${exe} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(${exe} PRIVATE OpenMP::OpenMP_Fortran)
endif ()

set_target_properties(${exe} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

