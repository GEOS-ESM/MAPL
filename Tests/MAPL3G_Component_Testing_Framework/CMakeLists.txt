# Detect if we are using Open MPI and add oversubscribe
string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)
if(MPI_Fortran_LIBRARY_VERSION_FIRSTWORD MATCHES "Open")
  list(APPEND MPIEXEC_PREFLAGS "-oversubscribe")
endif()

file(STRINGS "test_cases/cases.txt" TEST_CASES)

message(STATUS "Proessing ${TEST_CASES}")

set(LD_PATH "LD_LIBRARY_PATH")
set(TEST_ENV "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/gridcomps:${CMAKE_BINARY_DIR}/lib:$ENV{${LD_PATH}};UDUNITS2_XML_PATH=${udunits_XML_PATH}")

foreach(TEST_CASE ${TEST_CASES})

  if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/test_cases/${TEST_CASE}/nproc.rc)
    file(READ ${CMAKE_CURRENT_LIST_DIR}/test_cases/${TEST_CASE}/nproc.rc num_procs)
  else()
    set(num_procs "1")
  endif()
  add_test(
    NAME "MAPL3G_Comp_Test_${TEST_CASE}"
    COMMAND ${CMAKE_COMMAND}
      -DTEST_CASE=${TEST_CASE}
      -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE}
      -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
      -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
      -DMPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_comp_tester.cmake
  )
  set_tests_properties ("MAPL3G_Comp_Test_${TEST_CASE}" PROPERTIES LABELS "ESSENTIAL")
  set_tests_properties ("MAPL3G_Comp_Test_${TEST_CASE}" PROPERTIES ENVIRONMENT "${TEST_ENV}")
endforeach()
