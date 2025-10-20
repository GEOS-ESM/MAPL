add_executable(profiler_demo.x demo.F90)
target_link_libraries(profiler_demo.x MAPL.profiler)

add_executable(mpi_demo.x mpi_demo.F90)
target_link_libraries(mpi_demo.x MAPL.profiler MPI::MPI_Fortran)

install(TARGETS profiler_demo.x
        DESTINATION bin)

add_test(NAME Profiler_Demo_Basic
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 1 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/profiler_demo.x
  )
set_tests_properties (Profiler_Demo_Basic PROPERTIES LABELS "ESSENTIAL")

add_dependencies(build-tests profiler_demo.x)
