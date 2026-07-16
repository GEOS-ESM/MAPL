file(STRINGS "cases.txt" TEST_CASES)

if(APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
  # OpenMPI flags
  string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
  list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)
  if(MPI_Fortran_LIBRARY_VERSION_FIRSTWORD MATCHES "Open")
    set(MPIEXEC_PREFLAGS "--oversubscribe;--bind-to;core;--map-by;core" CACHE STRING "Flags for mpiexec" FORCE)
  endif()
endif()

set(REGRID_UTIL_PATH Regrid_Util)

# Regression tests
foreach(TEST_CASE ${TEST_CASES})
  add_test(
    NAME "${TEST_CASE}"
    COMMAND ${CMAKE_COMMAND}
      -DTEST_CASE=${TEST_CASE}
      -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE}
      -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
      -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
      "-DMPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}"
      -DREGRID_UTIL_PATH=${REGRID_UTIL_PATH}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_case.cmake
  )
  set_tests_properties("${TEST_CASE}" PROPERTIES
    LABELS "MAPL_APPS;REGRESSION"
    ENVIRONMENT "${LD_PATH}=${CMAKE_BINARY_DIR}/lib:$ENV{${LD_PATH}}"
  )
endforeach()
||||||| parent of b33dbd78 (Feature/bmauer/fixes #5249 (#5257))
=======
file(STRINGS "cases.txt" TEST_CASES)

if(APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
  # OpenMPI flags
  string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
  list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)
  if(MPI_Fortran_LIBRARY_VERSION_FIRSTWORD MATCHES "Open")
    set(MPIEXEC_PREFLAGS "--oversubscribe;--bind-to;core;--map-by;core" CACHE STRING "Flags for mpiexec" FORCE)
  endif()
endif()

set(REGRID_UTIL_PATH Regrid_Util) 

# Regression tests
foreach(TEST_CASE ${TEST_CASES})
  add_test(
    NAME "${TEST_CASE}"
    COMMAND ${CMAKE_COMMAND}
      -DTEST_CASE=${TEST_CASE}
      -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE}
      -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
      -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
      "-DMPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}"
      -DREGRID_UTIL_PATH=${REGRID_UTIL_PATH}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_case.cmake
  )
  set_tests_properties("${TEST_CASE}" PROPERTIES
    LABELS "MAPL_APPS"
    ENVIRONMENT "${LD_PATH}=${CMAKE_BINARY_DIR}/lib:$ENV{${LD_PATH}}"
  )
endforeach()
>>>>>>> b33dbd78 (Feature/bmauer/fixes #5249 (#5257))
