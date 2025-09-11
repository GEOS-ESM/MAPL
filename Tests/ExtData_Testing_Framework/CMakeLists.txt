# Detect if we are using Open MPI and add oversubscribe
string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)
if(MPI_Fortran_LIBRARY_VERSION_FIRSTWORD MATCHES "Open")
  list(APPEND MPIEXEC_PREFLAGS "-oversubscribe")
endif()

set(cutoff "7")

# We want to make a list of tests that are slow and can
# be skipped for ESSENTIAL testing. Most ExtData tests
# take 1-2 seconds at most, but some take 20-30 seconds.
set(SLOW_TESTS
  "case06"
  "case14"
  "case15"
  "case16"
  "case20"
  "case22"
  "case23"
)

# We have 3 tests that require netcdf Quantize support
set(QUANTIZE_TESTS
  "case32"
  "case33"
  "case34"
)

# We have one test that requires netcdf zstandard support
set(ZSTD_TESTS
  "case35"
)

# We have one test which requires *both* Quantize and ZSTD support
set(QUANTIZE_AND_ZSTD_TESTS
  "case36"
)

file(STRINGS "test_cases/extdata_2g_cases.txt" TEST_CASES_2G)

foreach(TEST_CASE ${TEST_CASES_2G})

  # Skip tests that require Quantize support if we don't have it
  if (NOT NETCDF_HAS_QUANTIZE AND ${TEST_CASE} IN_LIST QUANTIZE_TESTS)
    continue()
  endif()

  # Skip tests that require ZSTD support if we don't have it
  if (NOT NETCDF_HAS_ZSTD AND ${TEST_CASE} IN_LIST ZSTD_TESTS)
    continue()
  endif()

  # Skip tests that require both Quantize and ZSTD support if we don't have it
  if (NOT (NETCDF_HAS_QUANTIZE AND NETCDF_HAS_ZSTD) AND ${TEST_CASE} IN_LIST QUANTIZE_AND_ZSTD_TESTS)
    continue()
  endif()

  if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/test_cases/${TEST_CASE}/nproc.rc)
    file(READ ${CMAKE_CURRENT_LIST_DIR}/test_cases/${TEST_CASE}/nproc.rc num_procs)
  else()
    set(num_procs "1")
  endif()
  add_test(
    NAME "ExtData2G_${TEST_CASE}"
    COMMAND ${CMAKE_COMMAND}
      -DTEST_CASE=${TEST_CASE}
      -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE}
      -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
      -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
      -DMPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}
      -DIS_EXTDATA1G=NO
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_extdata.cmake
  )
  if (${num_procs} GREATER ${cutoff})
    set_tests_properties ("ExtData2G_${TEST_CASE}" PROPERTIES LABELS "EXTDATA2G_BIG_TESTS")
  elseif (${TEST_CASE} IN_LIST SLOW_TESTS)
    set_tests_properties ("ExtData2G_${TEST_CASE}" PROPERTIES LABELS "EXTDATA2G_SLOW_TESTS")
  else()
    set_tests_properties ("ExtData2G_${TEST_CASE}" PROPERTIES LABELS "EXTDATA2G_SMALL_TESTS;ESSENTIAL")
  endif()
endforeach()

