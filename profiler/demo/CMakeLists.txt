add_executable(demo.x demo.F90)
target_link_libraries(demo.x MAPL_Profiler)

add_executable(mpi_demo.x mpi_demo.F90)
target_link_libraries(mpi_demo.x MAPL_Profiler MPI::MPI_Fortran)
