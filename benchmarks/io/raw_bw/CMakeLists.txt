set(MODULE_DIRECTORY ${esma_include}/benchmarks/io/raw_bw)

ecbuild_add_executable (
  TARGET raw_bw.x
  SOURCES BW_Benchmark.F90 BW_BenchmarkSpec.F90 driver.F90
  DEFINITIONS USE_MPI)

target_link_libraries (raw_bw.x PRIVATE MAPL.shared MPI::MPI_Fortran FARGPARSE::fargparse OpenMP::OpenMP_Fortran NetCDF::NetCDF_Fortran)
target_include_directories (raw_bw.x PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
set_target_properties (raw_bw.x PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
