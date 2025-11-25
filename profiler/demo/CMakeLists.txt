add_executable(profiler_demo.x demo.F90)
target_link_libraries(profiler_demo.x MAPL.profiler)

add_executable(mpi_demo.x mpi_demo.F90)
target_link_libraries(mpi_demo.x MAPL.profiler MPI::MPI_Fortran OpenMP::OpenMP_Fortran)

add_executable(hybrid_demo.x hybrid_demo.F90)
target_link_libraries(hybrid_demo.x MAPL.profiler MPI::MPI_Fortran OpenMP::OpenMP_Fortran)

install(TARGETS profiler_demo.x mpi_demo.x hybrid_demo.x
        DESTINATION bin)

add_test(NAME Profiler_Demo_Basic
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 1 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/profiler_demo.x
  )

add_test(NAME Profiler_Demo_MPI
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/mpi_demo.x
  )

add_test(NAME Profiler_Demo_Hybrid
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/hybrid_demo.x
  )
set_tests_properties(Profiler_Demo_Hybrid  PROPERTIES ENVIRONMENT "OMP_NUM_THREADS=4;KMP_AFFINITY=compact")

set_tests_properties (Profiler_Demo_Basic PROPERTIES LABELS "ESSENTIAL")
set_tests_properties (Profiler_Demo_MPI PROPERTIES LABELS "ESSENTIAL")
set_tests_properties (Profiler_Demo_Hybrid PROPERTIES LABELS "ESSENTIAL")

add_dependencies(build-tests profiler_demo.x)
add_dependencies(build-tests mpi_demo.x)
add_dependencies(build-tests hybrid_demo.x)
