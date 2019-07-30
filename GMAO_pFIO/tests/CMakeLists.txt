add_definitions(-DUSE_MPI)
set (TEST_SRCS
    Test_UnlimitedEntity.F90
    Test_Attribute.F90
    Test_Variable.F90
    Test_CoordinateVariable.F90
    Test_PrefetchDataMessage.F90
    Test_FileMetadata.F90
    Test_NetCDF4_FileFormatter.F90
    Test_pFIO_Utilities.F90
    Test_SimpleSocket.F90
    Test_MpiSocket.F90
    Test_ServerThread.F90
    Test_Client.F90
    Test_ProtocolParser.F90
    Test_DirectoryService.F90
  )

# SRCS are mostly mocks to facilitate tests
set (SRCS
  pFIO_Initialize.F90
  MockServerThread.F90
  MockClient.F90
  MockServer.F90
  MockSocket.F90
  )

foreach(file ${TEST_SRCS})
   get_filename_component(basename ${file} NAME_WE)
   add_custom_command (
      OUTPUT ${file}
      COMMAND python
      ARGS ${PFUNIT_PATH}/bin/pFUnitParser.py ${CMAKE_CURRENT_SOURCE_DIR}/${basename}.pf ${file}
      DEPENDS ${basename}.pf
   )
endforeach()

add_library(pFIO_tests EXCLUDE_FROM_ALL ${TEST_SRCS} ${SRCS})
target_link_libraries(pFIO_tests GMAO_pFIO  MAPL_pFUnit ${PFUNIT_LIBRARIES})

include_directories(
   ${PFUNIT_INCLUDE_DIRS}
   ${CMAKE_CURRENT_SOURCE_DIR}
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/..)
include_directories(${include_GMAO_pFIO})
include_directories(${include_MAPL_pFUnit})

set(EXE pfio_tests.x)
add_executable (${EXE} EXCLUDE_FROM_ALL ${PFUNIT_PATH}/include/driver.F90 testSuites.inc)
target_compile_definitions (${EXE} PRIVATE USE_MPI PFUNIT_EXTRA_INITIALIZE=pFIO_Initialize)
set_target_properties(${EXE} PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_link_libraries(${EXE} pFIO_tests ${ESMF_LIBRARIES})


set(MPI_NPES 8)

if (CMAKE_SYSTEM_NAME STREQUAL Darwin)
  list(APPEND MPIEXEC_PREFLAGS --oversubscribe)
endif()

add_test(NAME pFIO_tests_unit
  COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPI_NPES} ${MPIEXEC_PREFLAGS} ./${EXE} ${MPIEXEC_POSTFLAGS}
  )

set(TESTO pfio_ctest_io.x)
add_executable (${TESTO} EXCLUDE_FROM_ALL pfio_ctest_io.F90)
target_compile_definitions (${TESTO} PRIVATE USE_MPI )
set_target_properties(${TESTO} PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_link_libraries(${TESTO} GMAO_pFIO ${ESMF_LIBRARIES})



set(TESTO_FLAGS
  -nc 6 -nsi 6 -nso 6 -ngo 1 -ngi 1 -v T,U )
add_test(NAME pFIO_tests_mpi
  COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 18 ${MPIEXEC_PREFLAGS} ./${TESTO} ${TESTO_FLAGS} -s mpi
  )

add_test(NAME pFIO_tests_simple
  COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 24 ${MPIEXEC_PREFLAGS} ./${TESTO} ${TESTO_FLAGS} -s simple
  )

add_test(NAME pFIO_tests_hybrid
  COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 12 ${MPIEXEC_PREFLAGS} ./${TESTO} ${TESTO_FLAGS} -s hybrid
  )

set(TESTPERF pfio_performance.x)
add_executable (${TESTPERF} EXCLUDE_FROM_ALL pfio_performance.F90)
target_compile_definitions (${TESTPERF} PRIVATE USE_MPI )
set_target_properties(${TESTPERF} PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_link_libraries(${TESTPERF} GMAO_pFIO ${ESMF_LIBRARIES})

add_dependencies(tests ${EXE})
add_dependencies(tests ${TESTO})
add_dependencies(tests ${TESTPERF})
