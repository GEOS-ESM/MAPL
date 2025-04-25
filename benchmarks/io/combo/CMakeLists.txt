set(exe combo.x)
set(MODULE_DIRECTORY ${esma_include}/benchmarks/io/combo)

ecbuild_add_executable (
  TARGET ${exe}
  SOURCES Kernel.F90 GathervKernel.F90 BW_Benchmark.F90 ComboSpec.F90 driver.F90
  DEFINITIONS USE_MPI)

target_link_libraries (${exe} PRIVATE MAPL.shared MPI::MPI_Fortran FARGPARSE::fargparse OpenMP::OpenMP_Fortran)
target_include_directories (${exe} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
set_target_properties (${exe} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
