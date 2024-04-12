# Detect if we are using Open MPI and add oversubscribe
string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)

file(STRINGS "cases.txt" TEST_CASES)

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()

foreach(TEST_CASE ${TEST_CASES})
  add_test(
     NAME "${TEST_CASE}"
     COMMAND ${CMAKE_COMMAND}
       -DTEST_CASE=${TEST_CASE}
       -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE}
       -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
       -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
       -DMPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}
       -P ${CMAKE_CURRENT_SOURCE_DIR}/run_captest.cmake
     )
   set_tests_properties("${TEST_CASE}" PROPERTIES LABELS "ESSENTIAL")
   set_tests_properties("${TEST_CASE}"
     PROPERTIES ENVIRONMENT "${LD_PATH}=${CMAKE_BINARY_DIR}/lib:$ENV{${LD_PATH}};UDUNITS2_XML_PATH=${udunits_XML_PATH}"
     )
endforeach()
