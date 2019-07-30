add_definitions(-DUSE_MPI)
set (TEST_SRCS
    test_Mapl_Base.F90
    test_sort.F90
    Test_CFIO_Bundle.F90
    Test_SimpleMAPLcomp.F90
    Test_StringGridFactoryMap.F90
    Test_GridManager.F90
    Test_LatLonGridFactory.F90
    Test_SphericalToCartesian.F90
    Test_LatLonCorners.F90
    Test_MAPL_Config.F90
    test_DirPath.F90
    Test_VarSpecMod.F90
#    Test_SimpleCommSplitter.F90
  )

# SRCS are mostly mocks to facilitate tests
set (SRCS
  MAPL_Initialize.F90
  MockGridFactory.F90
  MockRegridder.F90
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

add_library(MAPL_Base_tests EXCLUDE_FROM_ALL ${TEST_SRCS} ${SRCS})
target_link_libraries(MAPL_Base_tests MAPL_Base GMAO_pFIO MAPL_pFUnit ${PFUNIT_LIBRARIES})

include_directories(
   ${PFUNIT_INCLUDE_DIRS}
   ${CMAKE_CURRENT_SOURCE_DIR}
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/..)
include_directories(${include_GMAO_pFIO})
include_directories(${include_MAPL_Base})
include_directories(${include_MAPL_pFUnit})


set(EXE MAPL_Base_tests.x)
add_executable (${EXE} EXCLUDE_FROM_ALL ${PFUNIT_PATH}/include/driver.F90 testSuites.inc)
target_compile_definitions (${EXE} PRIVATE USE_MPI PFUNIT_EXTRA_INITIALIZE=MAPL_Initialize)
set_target_properties(${EXE} PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_link_libraries(${EXE} MAPL_Base_tests) # ${ESMF_LIBRARIES})


set(MPI_NPES 8)

if (CMAKE_SYSTEM_NAME STREQUAL Darwin)
  list(APPEND MPIEXEC_PREFLAGS --oversubscribe)
endif()

add_test(MAPL_Base_tests
   ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPI_NPES} ${MPIEXEC_PREFLAGS} ./${EXE} ${MPIEXEC_POSTFLAGS}
  )
add_dependencies(tests ${EXE})

