cmake_minimum_required (VERSION 3.13)
cmake_policy (SET CMP0053 NEW)
cmake_policy (SET CMP0054 NEW)

project (
  MAPL
  VERSION 1.1.8
  LANGUAGES Fortran CXX C)  # Note - CXX is required for ESMF

if (NOT COMMAND esma) # build as standalone project
  # Invoke checkout_ externals, but only the first time we
  # configure.  
  if (NOT SKIP_MANAGE_EXTERNALS)
    execute_process (
      COMMAND "checkout_externals"
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
      )
  endif ()
  set(SKIP_MANAGE_EXTERNALS ON CACHE BOOL "Set to skip manage externals step")

  list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/@cmake")
  include (esma)
endif()

ecbuild_declare_project()


# Special case - MAPL_cfio is built twice with two different precisions.
add_subdirectory (MAPL_cfio MAPL_cfio_r4)
add_subdirectory (MAPL_cfio MAPL_cfio_r8)

add_subdirectory (GMAO_pFIO)
add_subdirectory (MAPL_Base)

# Git transition defect:
# Uncomment the line below once the dev branch of MAPL has been brought in.
#add_subdirectory (Tests)

ecbuild_install_project (NAME MAPL)
