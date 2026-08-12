set (SRCS
  GridComp.F90
  driver.F90
  )

ecbuild_add_executable (TARGET create_stretched_cs_grid.x SOURCES create_stretched_cs_grid.F90)
target_link_libraries (create_stretched_cs_grid.x PRIVATE MAPL MPI::MPI_Fortran ESMF::ESMF OpenMP::OpenMP_Fortran)
target_include_directories (create_stretched_cs_grid.x PRIVATE $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

#set(dependencies MAPL GFTL_SHARED::gftl-shared esmf OpenMP::OpenMP_Fortran)

ecbuild_add_executable (
  TARGET GetHorzIJIndexDriver.x
  SOURCES ${SRCS} )
target_link_libraries(GetHorzIJIndexDriver.x  PRIVATE MAPL FARGPARSE::fargparse ESMF::ESMF  OpenMP::OpenMP_Fortran)

# GetHorzIJIndexDriver.x is needed for 'make tests'
add_dependencies(build-tests GetHorzIJIndexDriver.x)

# Detect if we are using Open MPI and add oversubscribe
string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)
if(MPI_Fortran_LIBRARY_VERSION_FIRSTWORD MATCHES "Open")
  list(APPEND MPIEXEC_PREFLAGS "-oversubscribe")
endif()

set (TEST_CASES
  NO_OMP
  OMP_1_thread
  OMP_4_thread
  )

foreach(TEST_CASE ${TEST_CASES})

  add_test(
    NAME "GetHorzIJIndex_${TEST_CASE}"
    COMMAND ${CMAKE_COMMAND}
      -DTEST_CASE=${TEST_CASE}
      -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE}
      -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
      -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
      -DMPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_gethorzijindex.cmake
  )

    set_tests_properties ("GetHorzIJIndex_${TEST_CASE}" PROPERTIES LABELS "THREADING_TESTS;ESSENTIAL")

endforeach()
