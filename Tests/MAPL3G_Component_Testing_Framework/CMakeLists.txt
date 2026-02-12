# Detect if we are using Open MPI and add oversubscribe
string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)
if(MPI_Fortran_LIBRARY_VERSION_FIRSTWORD MATCHES "Open")
  list(APPEND MPIEXEC_PREFLAGS "-oversubscribe")
endif()

file(STRINGS "test_cases/cases.txt" TEST_CASES)

message(STATUS "Proessing ${TEST_CASES}")

# Parse test case descriptions from test_case_descriptions.md
file(STRINGS "${CMAKE_CURRENT_LIST_DIR}/test_case_descriptions.md" DESC_LINES)
foreach(LINE ${DESC_LINES})
  # Match lines like "1. description text" or "13. description text"
  if(LINE MATCHES "^([0-9]+)\\. (.+)$")
    set(case_num ${CMAKE_MATCH_1})
    set(case_desc ${CMAKE_MATCH_2})
    # Pad single digit case numbers to match case01, case02, etc.
    if(case_num MATCHES "^[0-9]$")
      set(case_name "case0${case_num}")
    else()
      set(case_name "case${case_num}")
    endif()
    set(TEST_DESC_${case_name} "${case_desc}")
  endif()
endforeach()

set(LD_PATH "LD_LIBRARY_PATH")
set(TEST_ENV "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/gridcomps:${CMAKE_BINARY_DIR}/lib:$ENV{${LD_PATH}};UDUNITS2_XML_PATH=${udunits_XML_PATH}")

foreach(TEST_CASE ${TEST_CASES})

  if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/test_cases/${TEST_CASE}/nproc.rc)
    file(READ ${CMAKE_CURRENT_LIST_DIR}/test_cases/${TEST_CASE}/nproc.rc num_procs)
  else()
    set(num_procs "1")
  endif()
  
  # Get description for this test case
  set(test_description "")
  if(DEFINED TEST_DESC_${TEST_CASE})
    set(test_description "${TEST_DESC_${TEST_CASE}}")
  endif()
  
  add_test(
    NAME "MAPL3G_Comp_Test_${TEST_CASE}"
    COMMAND ${CMAKE_COMMAND}
      -DTEST_CASE=${TEST_CASE}
      "-DTEST_DESCRIPTION=${test_description}"
      -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE}
      -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
      -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
      -DMPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_comp_tester.cmake
  )
  set_tests_properties ("MAPL3G_Comp_Test_${TEST_CASE}" PROPERTIES LABELS "ESSENTIAL")
  set_tests_properties ("MAPL3G_Comp_Test_${TEST_CASE}" PROPERTIES ENVIRONMENT "${TEST_ENV}")
endforeach()
