# Detect if we are using Open MPI and add oversubscribe
string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)
if(MPI_Fortran_LIBRARY_VERSION_FIRSTWORD MATCHES "Open")
  list(APPEND MPIEXEC_PREFLAGS "-oversubscribe")
endif()

file(STRINGS "test_cases/cases.txt" TEST_CASES)

message(STATUS "Processing ${TEST_CASES}")

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

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif()
set(TEST_ENV "${LD_PATH}=$<TARGET_FILE_DIR:configurable_gridcomp>:$<TARGET_FILE_DIR:MAPL.componentDriverGridComp>:${CMAKE_CURRENT_BINARY_DIR}/gridcomps:${CMAKE_BINARY_DIR}/lib:$ENV{${LD_PATH}};UDUNITS2_XML_PATH=${udunits_XML_PATH}")

foreach(TEST_CASE ${TEST_CASES})

  # Keep the source case directories as case45-case49, but expose the PFIO
  # regression tests under a compact PFIO-specific test namespace.
  set(TEST_NAME "${TEST_CASE}")
  if (TEST_CASE STREQUAL "case45")
    set(TEST_NAME "pfio_case01")
  elseif (TEST_CASE STREQUAL "case46")
    set(TEST_NAME "pfio_case02")
  elseif (TEST_CASE STREQUAL "case47")
    set(TEST_NAME "pfio_case03")
  elseif (TEST_CASE STREQUAL "case48")
    set(TEST_NAME "pfio_case04")
  elseif (TEST_CASE STREQUAL "case49")
    set(TEST_NAME "pfio_case05")
  endif()

  if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/test_cases/${TEST_CASE}/nproc.rc)
    set(TEST_CASE_PATH "test_cases/${TEST_CASE}")
    file(READ ${CMAKE_CURRENT_LIST_DIR}/${TEST_CASE_PATH}/nproc.rc num_procs)
  elseif (TEST_CASE STREQUAL "case45")
    set(TEST_CASE_PATH "test_cases/pfio/case01")
    file(READ ${CMAKE_CURRENT_LIST_DIR}/${TEST_CASE_PATH}/nproc.rc num_procs)
  elseif (TEST_CASE STREQUAL "case46")
    set(TEST_CASE_PATH "test_cases/pfio/case02")
    file(READ ${CMAKE_CURRENT_LIST_DIR}/${TEST_CASE_PATH}/nproc.rc num_procs)
  elseif (TEST_CASE STREQUAL "case47")
    set(TEST_CASE_PATH "test_cases/pfio/case03")
    file(READ ${CMAKE_CURRENT_LIST_DIR}/${TEST_CASE_PATH}/nproc.rc num_procs)
  elseif (TEST_CASE STREQUAL "case48")
    set(TEST_CASE_PATH "test_cases/pfio/case04")
    file(READ ${CMAKE_CURRENT_LIST_DIR}/${TEST_CASE_PATH}/nproc.rc num_procs)
  elseif (TEST_CASE STREQUAL "case49")
    set(TEST_CASE_PATH "test_cases/pfio/case05")
    file(READ ${CMAKE_CURRENT_LIST_DIR}/${TEST_CASE_PATH}/nproc.rc num_procs)
  elseif (EXISTS ${CMAKE_CURRENT_LIST_DIR}/test_cases/pfio/${TEST_CASE}/nproc.rc)
    set(TEST_CASE_PATH "test_cases/pfio/${TEST_CASE}")
    file(READ ${CMAKE_CURRENT_LIST_DIR}/${TEST_CASE_PATH}/nproc.rc num_procs)
    file(READ ${CMAKE_CURRENT_LIST_DIR}/${TEST_CASE_PATH}/nproc.rc num_procs)
  else()
    set(TEST_CASE_PATH "test_cases/${TEST_CASE}")
    set(num_procs "1")
  endif()
  
  # Get description for this test case
  set(test_description "")
  if(DEFINED TEST_DESC_${TEST_CASE})
    set(test_description "${TEST_DESC_${TEST_CASE}}")
  endif()
  
  add_test(
    NAME "MAPL3G_Comp_Test_${TEST_NAME}"
    COMMAND ${CMAKE_COMMAND}
      -DTEST_CASE=${TEST_CASE}
      "-DTEST_DESCRIPTION=${test_description}"
      -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE}
      -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
       -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
       -DTEST_CASE_PATH=${TEST_CASE_PATH}
       -DMPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_comp_tester.cmake
  )
   set_tests_properties ("MAPL3G_Comp_Test_${TEST_NAME}" PROPERTIES LABELS "ESSENTIAL")
   set_tests_properties ("MAPL3G_Comp_Test_${TEST_NAME}" PROPERTIES ENVIRONMENT "${TEST_ENV}")
endforeach()
