cmake_minimum_required (VERSION 3.24)

get_property(is_multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(NOT is_multi_config AND NOT (CMAKE_BUILD_TYPE OR DEFINED ENV{CMAKE_BUILD_TYPE}))
  message (STATUS "Setting build type to 'Release' as none was specified.")
  set (CMAKE_BUILD_TYPE Release CACHE STRING "Default Release-Choose the type of build.")
endif ()

project (
  MAPL
  VERSION 2.53.2
  LANGUAGES Fortran CXX C)  # Note - CXX is required for ESMF

# Set the possible values of build type for cmake-gui
set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
  "Debug" "Release" "Aggressive")

# mepo can now clone subrepos in three styles
set (ESMA_CMAKE_DIRS
  ESMA_cmake
  @ESMA_cmake
  ESMA_cmake@
  )

foreach (dir IN LISTS ESMA_CMAKE_DIRS)
  if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/${dir})
    list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/${dir}")
    set (ESMA_CMAKE_PATH "${CMAKE_CURRENT_LIST_DIR}/${dir}" CACHE PATH "Path to ESMA_cmake code")
    include (esma)
    set(MAPL_STANDALONE TRUE)
  endif ()
endforeach ()

# build as standalone project
if (NOT COMMAND esma)
  include (esma OPTIONAL)
endif ()

if (NOT COMMAND esma)

  if (SKIP_MEPO)
    message (FATAL_ERROR "ESMA not found")
  else ()
    set (MEPO_CLONE_COMMAND mepo clone)
    execute_process (
      COMMAND ${MEPO_CLONE_COMMAND}
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
      )
    foreach (dir IN LISTS ESMA_CMAKE_DIRS)
      if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/${dir})
        list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/${dir}")
        include (esma)
        set(MAPL_STANDALONE TRUE)
      endif ()
    endforeach ()
    option (SKIP_MEPO "Set to skip mepo steps" ON)
  endif ()

endif ()

# This tells cmake to assume MAPL's cmake directory is the first place to look
list (PREPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

option (BUILD_SHARED_MAPL "Build shared MAPL libraries" ON)
if (BUILD_SHARED_MAPL)
  set (MAPL_LIBRARY_TYPE SHARED)
else ()
  set (MAPL_LIBRARY_TYPE STATIC)
endif()
message (STATUS "Building MAPL as ${MAPL_LIBRARY_TYPE} libraries")

# Some users of MAPL build GFE libraries inline with their application
# using an add_subdirectory() call rather than as a pre-build library.
# This would then populate the target already leading to find_package()
# errors.
if(NOT TARGET GFTL::gftl)
  find_package(GFTL 1.10.0 REQUIRED)
else()
  if (GFTL_VERSION VERSION_LESS 1.10.0)
    message(FATAL_ERROR "gFTL must be at least 1.10.0")
  endif ()
endif()
message (STATUS "Found gFTL: ${GFTL_DIR} (found version ${GFTL_VERSION})")

if(NOT TARGET GFTL_SHARED::gftl-shared)
  # MAPL currently requires at least gFTL-shared 1.6.1
  find_package(GFTL_SHARED 1.6.1 REQUIRED)
else ()
  if (GFTL_SHARED_VERSION VERSION_LESS 1.6.1)
    message(FATAL_ERROR "gFTL-shared must be at least 1.6.1")
  endif ()
endif()
message (STATUS "Found gFTL-shared: ${GFTL_DIR} (found version ${GFTL_SHARED_VERSION})")

option(BUILD_WITH_FARGPARSE "Use fArgParse for command line processing" ON)
if(BUILD_WITH_FARGPARSE)
  if(NOT TARGET FARGPARSE::fargparse)
    find_package(FARGPARSE 1.5.0 REQUIRED)
  else()
    if (FARGPARSE_VERSION VERSION_LESS 1.5.0)
      message(FATAL_ERROR "fArgParse must be at least 1.5.0")
    endif ()
  endif()
  message (STATUS "Building with fArgParse")
  message (STATUS "Found fArgParse: ${FARGPARSE_DIR} (found version ${FARGPARSE_VERSION})")
endif()

option(USE_EXTDATA2G "Use ExtData2G" ON)
if(USE_EXTDATA2G)
  set (EXTDATA2G_TARGET "MAPL.ExtData2G" CACHE STRING "ExtData2G Target")
  message (STATUS "Building with ExtData2G")
else()
  set (EXTDATA2G_TARGET "" CACHE STRING "ExtData2G Target")
endif()

option(BUILD_WITH_PFLOGGER "Build MAPL with pFlogger library support" ON)
if (BUILD_WITH_PFLOGGER)
  if(NOT TARGET PFLOGGER::pflogger)
    find_package(PFLOGGER 1.9.5 REQUIRED)
  else()
    if (PFLOGGER_VERSION VERSION_LESS 1.9.5)
      message(FATAL_ERROR "pFlogger must be at least 1.9.5")
    endif ()
  endif()
  message (STATUS "Building with pFlogger")
  message (STATUS "Found pFlogger: ${PFLOGGER_DIR} (found version ${PFLOGGER_VERSION})")
endif()

option(BUILD_WITH_FLAP "Use FLAP for command line processing" OFF)
if (BUILD_WITH_FLAP)
  find_package(FLAP REQUIRED)
endif ()

ecbuild_declare_project()

if (NOT Baselibs_FOUND)
  set(MPI_DETERMINE_LIBRARY_VERSION TRUE)
  find_package(MPI)

  find_package(NetCDF REQUIRED C Fortran)
  add_definitions(-DHAS_NETCDF4)
  add_definitions(-DHAS_NETCDF3)
  add_definitions(-DNETCDF_NEED_NF_MPIIO)
  add_definitions(-DHAS_NETCDF3)

  find_package(HDF5 REQUIRED)
  if(HDF5_IS_PARALLEL)
     add_definitions(-DH5_HAVE_PARALLEL)
  endif()

  if (NOT TARGET ESMF::ESMF)
    find_package(ESMF 8.6.1 MODULE REQUIRED)
    target_link_libraries(ESMF::ESMF INTERFACE MPI::MPI_Fortran)
  endif ()
else ()
  # This is an ESMF version test when using Baselibs which doesn't use the
  # same find_package internally in ESMA_cmake as used above (with a version
  # number) so this lets us at least trap use of old Baselibs here.
  if (ESMF_VERSION VERSION_LESS 8.6.1)
    message(FATAL_ERROR "ESMF must be at least 8.6.1")
  endif ()
endif ()

# We wish to add extra flags when compiling as Debug. We should only
# do this if we are using esma_cmake since the flags are defined
# there. Note that some flags like STANDARD_F18 might be available on
# all compilers, but others might only be needed for, say, Intel, since,
# for example, GNU does not allow if(integer) no matter what
if (COMMAND esma)
  set (STRICT_DEBUG_FLAGS
    ${STANDARD_F18}
    ${ERROR_IF_INTEGER}
    ${ERROR_LOGICAL_SET_TO_INTEGER}
    ${DISABLE_LONG_LINE_LENGTH_WARNING}
    )

  # Are we compiling Fortran?
  set (is_fortran "$<COMPILE_LANGUAGE:Fortran>")
  # and is our build type Debug?
  set (is_build_type_debug "$<STREQUAL:${CMAKE_BUILD_TYPE},Debug>")
  # set the logical AND
  set (meets_requirements "$<AND:${is_fortran},${is_build_type_debug}>")

  foreach (flag ${STRICT_DEBUG_FLAGS})
    # We use SHELL to avoid de-duplication and preserve spaces:
    #   https://cmake.org/cmake/help/latest/command/target_compile_options.html?highlight=target_compile_options#option-de-duplication
    add_compile_options("SHELL:$<${meets_requirements}:${flag}>")
  endforeach ()
endif ()

if (BUILD_WITH_PFLOGGER)
  add_definitions(-DBUILD_WITH_PFLOGGER)
else ()
  add_subdirectory (pflogger_stub)
endif ()

add_definitions(-Dsys${CMAKE_SYSTEM_NAME})

# Support for automated code generation
include(mapl_acg)
include(mapl_create_stub_component)
add_subdirectory (Apps)

# Special case - MAPL_cfio is built twice with two different precisions.
add_subdirectory (MAPL_cfio MAPL_cfio_r4)
add_subdirectory (MAPL_cfio MAPL_cfio_r8)

# This tests for various capabilities of the compiler
# We mainly use it for MPICH issues
include(CheckCompilerCapabilities)

# 1. The first workaround is in pfio for https://github.com/pmodels/mpich/issues/6691
# 2. Below is to workaround https://github.com/pmodels/mpich/issues/6693
if(SUPPORT_FOR_MPI_IERROR_KEYWORD)
  add_compile_definitions(SUPPORT_FOR_MPI_IERROR_KEYWORD)
endif()
if (APPLE)
  add_compile_definitions("-D__DARWIN")
endif()

add_subdirectory (udunits2f)
add_subdirectory (pfio)
add_subdirectory (profiler)
add_subdirectory (generic)
add_subdirectory (field_utils)
add_subdirectory (oomph) # temporary - will rename to generic when done
add_subdirectory (shared)
add_subdirectory (include)
add_subdirectory (base)
add_subdirectory (MAPL)
add_subdirectory (gridcomps)
add_subdirectory (griddedio)
if (BUILD_WITH_FARGPARSE)
   add_subdirectory (docs)
   add_subdirectory (benchmarks)
endif()

if (PFUNIT_FOUND)
  include (add_pfunit_ctest)
  add_subdirectory (pfunit EXCLUDE_FROM_ALL)
endif ()

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
   FILES_MATCHING PATTERN "mapl_*.cmake"
   )

# https://www.scivision.dev/cmake-auto-gitignore-build-dir/
# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()

# Piggyback that file into install
install(
   FILES ${PROJECT_BINARY_DIR}/.gitignore
   DESTINATION ${CMAKE_INSTALL_PREFIX}
   )

ecbuild_install_project (NAME MAPL)

# This must be after ecbuild_install_project
if (MAPL_STANDALONE)
  # We only care about CPack if MAPL is a standalone
  include (esma_cpack OPTIONAL RESULT_VARIABLE esma_cpack_FOUND)
  if (esma_cpack_FOUND)
    message(STATUS "esma_cpack_FOUND: ${esma_cpack_FOUND}")

    # This installs a tarball of the source code
    # in the installation directory.
    # MUST BE THE LAST CODE IN THIS FILE
    option(INSTALL_SOURCE_TARFILE "Create and install source tarfile" OFF)
    if(INSTALL_SOURCE_TARFILE)
      install(CODE "set(CMAKE_PROJECT_NAME \"${CMAKE_PROJECT_NAME}\")")
      install(SCRIPT "${ESMA_CMAKE_PATH}/esma_postinstall.cmake")
    endif()
  endif ()
endif ()
