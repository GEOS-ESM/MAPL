set(MODULE_DIRECTORY "${esma_include}/MAPL.extdata.tests")

# For known sites, set LOCAL_REGRESSION_DATA_DIR automatically if not already set
if (${GEOS_SITE} MATCHES "NCCS")
  if (NOT DEFINED ENV{LOCAL_REGRESSION_DATA_DIR})
    set(ENV{LOCAL_REGRESSION_DATA_DIR} "/discover/nobackup/bmauer/test-data")
  endif ()
elseif (${GEOS_SITE} MATCHES "GMAO.desktop")
  if (NOT DEFINED ENV{LOCAL_REGRESSION_DATA_DIR})
    set(ENV{LOCAL_REGRESSION_DATA_DIR} "/home/bmauer/test-data")
  endif ()
endif ()

set (test_srcs
  Test_ExtDataNodeBracket.pf
  )

if (DEFINED ENV{LOCAL_REGRESSION_DATA_DIR} AND IS_DIRECTORY "$ENV{LOCAL_REGRESSION_DATA_DIR}")
  list(APPEND test_srcs Test_NonClimDataSetFileSelector.pf)
  list(APPEND test_srcs Test_DataSetNode.pf)
endif ()

add_pfunit_ctest(MAPL.extdata.tests
                TEST_SOURCES ${test_srcs}
                LINK_LIBRARIES MAPL.extdata MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE mapl_pFUnit_Initialize_mod
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                MAX_PES 1
                )
set_target_properties(MAPL.extdata.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
set_tests_properties(MAPL.extdata.tests PROPERTIES LABELS "ESSENTIAL")

if (APPLE)
  set(LD_PATH "DYLD_LIBRARY_PATH")
else()
  set(LD_PATH "LD_LIBRARY_PATH")
endif ()

set(TEST_ENV "${LD_PATH}=${CMAKE_CURRENT_BINARY_DIR}/gridcomps:$ENV{${LD_PATH}}")
list(APPEND TEST_ENV "LOCAL_REGRESSION_DATA_DIR=$ENV{LOCAL_REGRESSION_DATA_DIR}")

if(${GEOS_SITE} STREQUAL "NCCS" AND ${MPI_STACK} STREQUAL "intelmpi")
  list(APPEND TEST_ENV "I_MPI_OFI_PROVIDER=psm3")
endif()

set_property(TEST MAPL.extdata.tests PROPERTY ENVIRONMENT "${TEST_ENV}")

add_dependencies(build-tests MAPL.extdata.tests)
