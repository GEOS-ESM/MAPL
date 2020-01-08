add_definitions(-DUSE_MPI)
set (TEST_SRCS
    Test_UnlimitedEntity.pf
    Test_LocalMemReference.pf
    Test_Attribute.pf
    Test_Variable.pf
    Test_CoordinateVariable.pf
    Test_PrefetchDataMessage.pf
    Test_FileMetadata.pf
    Test_NetCDF4_FileFormatter.pf
    Test_pFIO_Utilities.pf
    Test_SimpleSocket.pf
    Test_MpiSocket.pf
    Test_ServerThread.pf
    Test_Client.pf
    Test_ProtocolParser.pf
    Test_DirectoryService.pf
  )

# SRCS are mostly mocks to facilitate tests
set (SRCS
  pFIO_Initialize.F90
  MockServerThread.F90
  MockClientThread.F90
  MockClient.F90
  MockServer.F90
  MockSocket.F90
  )

add_pfunit_ctest(pFIO_tests 
                TEST_SOURCES ${TEST_SRCS}
                OTHER_SOURCES ${SRCS}
                LINK_LIBRARIES GMAO_pFIO MAPL_pFUnit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE pFIO_pFUNIT_Initialize
                MAX_PES 8
                )


include_directories(
   ${CMAKE_CURRENT_SOURCE_DIR}
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/..)
include_directories(${include_GMAO_pFIO})
include_directories(${include_MAPL_pFUnit})


set(TESTO pfio_ctest_io.x)
ecbuild_add_executable (
  TARGET ${TESTO}
  PROPERTIES EXCLUDE_FROM_ALL=TRUE
  SOURCES pfio_ctest_io.F90
  LIBS GMAO_pFIO ${NETCDF_LIBRARIES}
  DEFINITIONS USE_MPI)
set_target_properties(${TESTO} PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_link_libraries(${TESTO} GMAO_pFIO ${NETCDF_LIBRARIES})

# Detect if we are using Open MPI and add oversubscribe
string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)
if(MPI_Fortran_LIBRARY_VERSION_FIRSTWORD MATCHES "Open")
   list(APPEND MPIEXEC_PREFLAGS "-oversubscribe")
endif()

set(TESTO_FLAGS
  -nc 6 -nsi 6 -nso 6 -ngo 1 -ngi 1 -v T,U )
add_test(NAME pFIO_tests_mpi
  COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 18 ${MPIEXEC_PREFLAGS} ${CMAKE_BINARY_DIR}/bin/${TESTO} ${TESTO_FLAGS} -s mpi
  )

add_test(NAME pFIO_tests_simple
  COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 24 ${MPIEXEC_PREFLAGS} ${CMAKE_BINARY_DIR}/bin/${TESTO} ${TESTO_FLAGS} -s simple
  )

add_test(NAME pFIO_tests_hybrid
  COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 12 ${MPIEXEC_PREFLAGS} ${CMAKE_BINARY_DIR}/bin/${TESTO} ${TESTO_FLAGS} -s hybrid
  )

set(TESTPERF pfio_performance.x)
ecbuild_add_executable (
  TARGET ${TESTPERF}
  PROPERTIES EXCLUDE_FROM_ALL=TRUE
  SOURCES pfio_performance.F90
  DEFINITIONS USE_MPI
  LIBS GMAO_pFIO ${NETCDF_LIBRARIES})
set_target_properties(${TESTPERF} PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_link_libraries(${TESTPERF} GMAO_pFIO ${NETCDF_LIBRARIES})

add_dependencies(tests pFIO_tests)
add_dependencies(tests ${TESTO})
add_dependencies(tests ${TESTPERF})
