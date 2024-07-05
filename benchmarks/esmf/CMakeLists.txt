set(exe gc_run.x)

add_subdirectory(gridcomp_creation)
ecbuild_add_executable (
  TARGET ${exe}
  SOURCES gc_run.F90)

target_link_libraries(${exe} PRIVATE MAPL.shared ESMF::ESMF)
target_include_directories (${exe} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(${exe} PRIVATE OpenMP::OpenMP_Fortran)
endif ()
