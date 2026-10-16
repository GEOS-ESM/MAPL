# Detect if we are using Open MPI and add oversubscribe
string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)
if(MPI_Fortran_LIBRARY_VERSION_FIRSTWORD MATCHES "Open")
  list(APPEND MPIEXEC_PREFLAGS "-oversubscribe")
endif()

file(STRINGS "test_cases/cases.txt" TEST_CASES)

set(cutoff "7")

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

# Tests with a dryrun.rc run extdata_dryrun_check.py, which needs python netCDF4
find_package(Python3 COMPONENTS Interpreter QUIET)
set(NETCDF4_PY_RESULT 1)
if (Python3_EXECUTABLE)
  execute_process(
    COMMAND ${Python3_EXECUTABLE} -c "import netCDF4"
    RESULT_VARIABLE NETCDF4_PY_RESULT
    OUTPUT_QUIET ERROR_QUIET
  )
endif()
if (NETCDF4_PY_RESULT EQUAL 0)
  set(HAVE_PYTHON_NETCDF4 TRUE)
else()
  set(HAVE_PYTHON_NETCDF4 FALSE)
endif()
set(SKIPPED_DRYRUN_CASES "")

foreach(TEST_CASE ${TEST_CASES})

  if (NOT HAVE_PYTHON_NETCDF4 AND EXISTS ${CMAKE_CURRENT_LIST_DIR}/test_cases/${TEST_CASE}/dryrun.rc)
    list(APPEND SKIPPED_DRYRUN_CASES ${TEST_CASE})
    continue()
  endif()

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
      -DPython3_EXECUTABLE=${Python3_EXECUTABLE}
      -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
      -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
      -DMPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_comp_tester.cmake
  )
  if (${num_procs} GREATER ${cutoff}) 
     set_tests_properties ("MAPL3G_Comp_Test_${TEST_CASE}" PROPERTIES LABELS "COMP_TESTS_BIG")
     set_tests_properties ("MAPL3G_Comp_Test_${TEST_CASE}" PROPERTIES ENVIRONMENT "${TEST_ENV}")
  else()
     set_tests_properties ("MAPL3G_Comp_Test_${TEST_CASE}" PROPERTIES LABELS "ESSENTIAL")
     set_tests_properties ("MAPL3G_Comp_Test_${TEST_CASE}" PROPERTIES ENVIRONMENT "${TEST_ENV}")
  endif()
endforeach()

if (SKIPPED_DRYRUN_CASES)
  list(JOIN SKIPPED_DRYRUN_CASES ", " _skipped)
  message(WARNING
    "Python3 netCDF4 package not found: SKIPPING MAPL3G component tests that require it (${_skipped}). Install netCDF4 for the Python3 interpreter to enable them.")
endif()
