cmake_minimum_required (VERSION 3.13)
cmake_policy (SET CMP0053 NEW)
cmake_policy (SET CMP0054 NEW)

project (
  MAPL
  VERSION 2.4.0
  LANGUAGES Fortran CXX C)  # Note - CXX is required for ESMF

if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/ESMA_cmake)
  list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/ESMA_cmake")
  include (esma)
endif ()

# build as standalone project
if (NOT COMMAND esma)
  include (esma OPTIONAL)
endif ()

if (NOT COMMAND esma)

  if (SKIP_MEPO)
    message (FATAL_ERROR "ESMA not found")
  else ()
     set (MEPO_INIT_COMMAND mepo init)
     execute_process (
        COMMAND ${MEPO_INIT_COMMAND}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        )

     set (MEPO_CLONE_COMMAND mepo clone)
     execute_process (
        COMMAND ${MEPO_CLONE_COMMAND}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        )
    list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/ESMA_cmake")
    include (esma)
    option (SKIP_MEPO "Set to skip mepo steps" ON)
  endif ()

endif ()

option(BUILD_WITH_FLAP "Use FLAP for command line processing" ON)
if (BUILD_WITH_FLAP)
  find_package(FLAP REQUIRED)
endif ()

ecbuild_declare_project()

if (NOT Baselibs_FOUND)
  find_package(NetCDF REQUIRED Fortran)
  add_definitions(-DHAS_NETCDF4)
  add_definitions(-DHAS_NETCDF3)
  add_definitions(-DNETCDF_NEED_NF_MPIIO)
  add_definitions(-DHAS_NETCDF3)

  find_package(HDF5 REQUIRED Fortran)
  if(HDF5_IS_PARALLEL)
     add_definitions(-DH5_HAVE_PARALLEL)
  endif()

  find_package(ESMF MODULE REQUIRED)

  find_package(YAFYAML REQUIRED)

  find_package(GFTL REQUIRED)
  find_package(GFTL_SHARED REQUIRED)
  find_package(YAFYAML REQUIRED)
  find_package(PFLOGGER REQUIRED)
endif ()

# Special case - MAPL_cfio is built twice with two different precisions.
add_subdirectory (MAPL_cfio MAPL_cfio_r4)
add_subdirectory (MAPL_cfio MAPL_cfio_r8)

add_subdirectory (pfio)
add_subdirectory (profiler)
add_subdirectory (generic)
add_subdirectory (shared)
add_subdirectory (include)
add_subdirectory (base)
add_subdirectory (MAPL)
add_subdirectory (gridcomps)

if (PFUNIT_FOUND)
  include (add_pfunit_ctest)
  add_subdirectory (MAPL_pFUnit EXCLUDE_FROM_ALL)
endif ()

# Support for automated code generation
list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
include(mapl_acg)
include(mapl_create_stub_component)
add_subdirectory (Apps)

add_subdirectory (Tests)

# @env will exist here if MAPL is built as itself but not as part of, say, GEOSgcm
esma_add_subdirectory (ESMA_env FOUND ESMA_env_FOUND)

# Install the Python directory
install (
   DIRECTORY Python
   DESTINATION lib
   USE_SOURCE_PERMISSIONS
   )

install (
   DIRECTORY cmake
   DESTINATION ${INSTALL_DATA_DIR}
   )

ecbuild_install_project (NAME MAPL)
