file(STRINGS "test_cases/cases.txt" TEST_CASES)

option(TEST_EXTDATA1G "Whether to text ExtData 1G in addition to 2G" OFF)
option(TEST_EXTDATA2G "Whether to text ExtData 1G in addition to 2G" ON)

if (${TEST_EXTDATA1G})
foreach(TEST_CASE ${TEST_CASES})
  add_test(
     NAME "ExtData1G_${TEST_CASE}"
     COMMAND ${CMAKE_COMMAND} 
       -DTEST_CASE=${TEST_CASE} 
       -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE} 
       -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
       -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
       -P ${CMAKE_CURRENT_SOURCE_DIR}/run_extdata1g.cmake
     )
  endforeach()
endif()

if (${TEST_EXTDATA2G})
  foreach(TEST_CASE ${TEST_CASES})
  add_test(
     NAME "ExtData2G_${TEST_CASE}"
     COMMAND ${CMAKE_COMMAND} 
       -DTEST_CASE=${TEST_CASE} 
       -DMPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE} 
       -DMPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}
       -DMY_BINARY_DIR=${CMAKE_BINARY_DIR}/bin
       -P ${CMAKE_CURRENT_SOURCE_DIR}/run_extdata2g.cmake
     )
  endforeach()
endif()
