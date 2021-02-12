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
  MockServerThread.F90
  MockClientThread.F90
  MockClient.F90
  MockServer.F90
  MockSocket.F90
  )

# This file needs to be in a library because CMake cannot detect the
# dependency of the pFUnit driver on it.  This is due to the use of
# preprocesor in the driver for specifying the include file.
#add_library (pfio_extras
#  pFIO_Initialize.F90
#  )
#target_link_libraries (pfio_extras PUBLIC MAPL.pfunit MAPL.shared)

add_pfunit_ctest(MAPL.pfio.tests
                TEST_SOURCES ${TEST_SRCS}
                OTHER_SOURCES ${SRCS}
                LINK_LIBRARIES MAPL.pfio MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 8
                )


include_directories(
   ${CMAKE_CURRENT_SOURCE_DIR}
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/..)
include_directories(${include_MAPL.pfio})
include_directories(${MAPL_SOURCE_DIR}/include)


set(TESTO pfio_ctest_io.x)
ecbuild_add_executable (
  TARGET ${TESTO}
  NOINSTALL
  SOURCES pfio_ctest_io.F90
  LIBS MAPL.shared MAPL.pfio ${NETCDF_LIBRARIES} MPI::MPI_Fortran
  DEFINITIONS USE_MPI)
# CMake has an OpenMP issue with Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(${TESTO} OpenMP::OpenMP_Fortran)
endif ()

# Detect if we are using Open MPI and add oversubscribe
string(REPLACE " " ";" MPI_Fortran_LIBRARY_VERSION_LIST ${MPI_Fortran_LIBRARY_VERSION_STRING})
list(GET MPI_Fortran_LIBRARY_VERSION_LIST 0 MPI_Fortran_LIBRARY_VERSION_FIRSTWORD)
if(MPI_Fortran_LIBRARY_VERSION_FIRSTWORD MATCHES "Open")
   list(APPEND MPIEXEC_PREFLAGS "-oversubscribe")
endif()

set(TESTO_FLAGS
  -nc 6 -nsi 6 -nso 6 -ngo 1 -ngi 1 -v T,U )
add_test(NAME pFIO_tests_mpi
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 18 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TESTO} ${TESTO_FLAGS} -s mpi
  )
add_test(NAME pFIO_tests_simple
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 24 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TESTO} ${TESTO_FLAGS} -s simple
  )
add_test(NAME pFIO_tests_hybrid
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 12 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TESTO} ${TESTO_FLAGS} -s hybrid
  )

add_test(NAME pFIO_tests_mpi_2layer
  COMMAND env FI_PROVIDER=verbs ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 18 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TESTO} ${TESTO_FLAGS} -s multilayer -nw 3 -w ${CMAKE_BINARY_DIR}/bin/pfio_writer.x
  )

add_test(NAME pFIO_tests_mpi_2comm
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 18 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TESTO} ${TESTO_FLAGS} -s multicomm -nw 3
  )

add_test(NAME pFIO_tests_mpi_2group
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 18 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TESTO} ${TESTO_FLAGS} -s multigroup -nw 3
  )

set_tests_properties (pFIO_tests_mpi pFIO_tests_simple pFIO_tests_hybrid  pFIO_tests_mpi_2layer pFIO_tests_mpi_2comm pFIO_tests_mpi_2group  PROPERTIES LABELS "PERFORMANCE")

set(TESTPERF pfio_performance.x)

ecbuild_add_executable (
  TARGET ${TESTPERF}
  NOINSTALL
  SOURCES pfio_performance.F90
  DEFINITIONS USE_MPI
  LIBS MAPL.pfio ${NETCDF_LIBRARIES} MPI::MPI_Fortran)
# CMake has an OpenMP issue with Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(${TESTPERF} OpenMP::OpenMP_Fortran)
endif ()
target_link_libraries(${TESTPERF} MAPL.pfio ${NETCDF_LIBRARIES})
add_test(NAME pFIO_performance
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 12 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TESTO} ${TESTO_FLAGS} -s hybrid
  )
set_tests_properties (pFIO_performance PROPERTIES LABELS "PERFORMANCE")

add_dependencies(build-tests MAPL.pfio.tests)
add_dependencies(build-tests ${TESTO})
add_dependencies(build-tests pfio_writer.x)
add_dependencies(build-tests ${TESTPERF})

