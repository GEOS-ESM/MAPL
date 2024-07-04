set(exe gridcomp_create.x)
set(MODULE_DIRECTORY ${esma_include}/benchmarks/gridcomp_creation)

ecbuild_add_executable (
  TARGET ${exe}
  SOURCES GridCompCreationTester.F90 GridCompCreator.F90 shared_constants.F90
)

#target_link_libraries (${exe} PRIVATE MAPL.shared MPI::MPI_Fortran FARGPARSE::fargparse)
target_link_libraries (${exe} PRIVATE MAPL.shared ESMF::ESMF )
target_include_directories (${exe} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
set_target_properties (${exe} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(${exe} PRIVATE OpenMP::OpenMP_Fortran)
endif ()
