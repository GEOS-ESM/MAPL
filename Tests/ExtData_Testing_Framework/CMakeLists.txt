# Detect if we are using Open MPI and add oversubscribe
string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)
if(MPI_Fortran_LIBRARY_VERSION_FIRSTWORD MATCHES "Open")
   list(APPEND MPIEXEC_PREFLAGS "-oversubscribe")
endif()

file(STRINGS "test_cases/extdata_1g_cases.txt" TEST_CASES_1G)

set(cutoff "7")

foreach(TEST_CASE ${TEST_CASES_1G})
  if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/test_cases/${TEST_CASE}/nproc.rc)
     file(READ ${CMAKE_CURRENT_LIST_DIR}/test_cases/${TEST_CASE}/nproc.rc num_procs)
  else()
     set(num_procs "1")
  endif()
  add_test(
     NAME "ExtData1G_${TEST_CASE}"
     COMMAND ${CMAKE_COMMAND} 
       -DTEST_CASE=${TEST_CASE} 
       -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE} 
       -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
       -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
       -DMPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}
       -DIS_EXTDATA2G=NO
       -P ${CMAKE_CURRENT_SOURCE_DIR}/run_extdata.cmake
       )
  if (${num_procs} LESS ${cutoff})
     set_tests_properties ("ExtData1G_${TEST_CASE}" PROPERTIES LABELS "EXTDATA1G_SMALL_TESTS")
  else()
     set_tests_properties ("ExtData1G_${TEST_CASE}" PROPERTIES LABELS "EXTDATA1G_BIG_TESTS")
  endif()

endforeach()

file(STRINGS "test_cases/extdata_2g_cases.txt" TEST_CASES_2G)

foreach(TEST_CASE ${TEST_CASES_2G})
     
  add_test(
     NAME "ExtData2G_${TEST_CASE}"
     COMMAND ${CMAKE_COMMAND} 
       -DTEST_CASE=${TEST_CASE} 
       -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE} 
       -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
       -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
       -DMPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}
       -DIS_EXTDATA2G=YES
       -P ${CMAKE_CURRENT_SOURCE_DIR}/run_extdata.cmake
     )
  if (${num_procs} LESS ${cutoff})
     set_tests_properties ("ExtData2G_${TEST_CASE}" PROPERTIES LABELS "EXTDATA2G_SMALL_TESTS")
  else()
     set_tests_properties ("ExtData2G_${TEST_CASE}" PROPERTIES LABELS "EXTDATA2G_BIG_TESTS")
  endif()
endforeach()
