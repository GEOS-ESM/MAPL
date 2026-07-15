set(exe noleap_calendar_driver.x)

ecbuild_add_executable(
  TARGET  ${exe}
  SOURCES noleap_calendar_driver.F90
)

target_link_libraries(${exe} PRIVATE MAPL.esmf ESMF::ESMF MPI::MPI_Fortran)

# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
  target_link_libraries(${exe} PRIVATE OpenMP::OpenMP_Fortran)
endif ()

# Copy the default YAML config alongside the built executable so it can be
# run directly from the build directory:
#   mpirun -np 1 noleap_calendar_driver.x noleap_calendar.yaml
configure_file(noleap_calendar.yaml ${CMAKE_CURRENT_BINARY_DIR}/noleap_calendar.yaml COPYONLY)
