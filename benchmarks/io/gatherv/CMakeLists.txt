set(MODULE_DIRECTORY ${esma_include}/benchmarks/io/gatherv)

ecbuild_add_executable (
  TARGET gatherv.x
  SOURCES GathervKernel.F90 GathervSpec.F90 driver.F90
  DEFINITIONS USE_MPI)

target_link_libraries (gatherv.x PRIVATE MAPL.shared MPI::MPI_Fortran FARGPARSE::fargparse OpenMP::OpenMP_Fortran)
target_include_directories (gatherv.x PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
set_target_properties (gatherv.x PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
