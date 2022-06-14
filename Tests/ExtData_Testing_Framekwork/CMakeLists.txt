file(STRINGS "test_cases/cases.txt" TEST_CASES)

option(TEST_EXTDATA1G "Whether to text ExtData 1G in addition to 2G" OFF)
option(TEST_EXTDATA2G "Whether to text ExtData 1G in addition to 2G" ON)

set(cutoff "7")
foreach(TEST_CASE ${TEST_CASES})
  if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/test_cases/${TEST_CASE}/nproc.rc)
     file(READ ${CMAKE_CURRENT_LIST_DIR}/test_cases/${TEST_CASE}/nproc.rc num_procs)
  else()
     set(num_procs "1")
  endif()
  if (${TEST_EXTDATA1G})
     add_test(
        NAME "ExtData1G_${TEST_CASE}"
        COMMAND ${CMAKE_COMMAND} 
          -DTEST_CASE=${TEST_CASE} 
          -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE} 
          -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
          -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
          -P ${CMAKE_CURRENT_SOURCE_DIR}/run_extdata1g.cmake
     if (${num_procs} LESS ${cutoff})
        set_tests_properties ("ExtData1G_${TEST_CASE}" PROPERTIES LABELS "EXTDATA1G_SMALL_TESTS")
     else()
        set_tests_properties ("ExtData1G_${TEST_CASE}" PROPERTIES LABELS "EXTDATA1G_BIG_TESTS")
     endif()
        )
  endif()
  if (${TEST_EXTDATA2G})
     add_test(
        NAME "ExtData2G_${TEST_CASE}"
        COMMAND ${CMAKE_COMMAND} 
          -DTEST_CASE=${TEST_CASE} 
          -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE} 
          -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
          -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
          -P ${CMAKE_CURRENT_SOURCE_DIR}/run_extdata2g.cmake
        )
     if (${num_procs} LESS ${cutoff})
        set_tests_properties ("ExtData2G_${TEST_CASE}" PROPERTIES LABELS "EXTDATA2G_SMALL_TESTS")
     else()
        set_tests_properties ("ExtData2G_${TEST_CASE}" PROPERTIES LABELS "EXTDATA2G_BIG_TESTS")
     endif()
  endif()
endforeach()
