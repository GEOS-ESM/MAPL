#set(MODULE_DIRECTORY "${esma_include}/MAPL.benchmarks.raw_bw")

ecbuild_add_executable (
  TARGET raw_bw.x
  SOURCES BW_Benchmark.F90 BW_BenchmarkSpec.F90 driver.F90
  DEFINITIONS USE_MPI)

target_link_libraries (raw_bw.x PRIVATE MAPL.shared MPI::MPI_Fortran FARGPARSE::fargparse)
target_include_directories (raw_bw.x PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(raw_bw.x PRIVATE OpenMP::OpenMP_Fortran)
endif ()

#set_target_properties(raw_bw.x PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

