set(exe checkpoint_simulator.x)
set(MODULE_DIRECTORY ${esma_include}/benchmarks/io/checkpoint_simulator)

ecbuild_add_executable(
  TARGET ${exe}
  SOURCES checkpoint_simulator.F90
  DEFINITIONS USE_MPI
)

target_link_libraries(
  ${exe}
  PRIVATE
    MAPL.shared
    MPI::MPI_Fortran
    FARGPARSE::fargparse
    ESMF::ESMF
    OpenMP::OpenMP_Fortran
)
target_include_directories(${exe} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
set_target_properties(
  ${exe}
  PROPERTIES
    Fortran_MODULE_DIRECTORY
      ${MODULE_DIRECTORY}
)
