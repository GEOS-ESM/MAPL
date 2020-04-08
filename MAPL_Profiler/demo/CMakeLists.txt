add_executable(demo.x demo.F90)
target_link_libraries(demo.x MAPL_Profiler)

add_executable(mpi_demo.x mpi_demo.F90)
target_link_libraries(mpi_demo.x MAPL_Profiler ${MPI_Fortran_LIBRARIES})
target_include_directories (mpi_demo.x PUBLIC ${MPI_Fortran_INCLUDE_DIRS})
target_include_directories (mpi_demo.x PUBLIC ${CMAKE_BINARY_DIR}/src)
