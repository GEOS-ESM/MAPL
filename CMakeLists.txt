cmake_minimum_required (VERSION 3.13)
cmake_policy (SET CMP0053 NEW)
cmake_policy (SET CMP0054 NEW)

project (
  MAPL
  VERSION 2.0
  LANGUAGES Fortran CXX C)  # Note - CXX is required for ESMF

option(USE_MEPO "Set to use mepo to get external dependencies" ON)

if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/@cmake)
  list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/@cmake")
  include (esma)
else ()
  if (NOT COMMAND esma) # build as standalone project

    if (USE_MEPO)
      if (NOT SKIP_MEPO)
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
      endif()
      option (SKIP_MEPO "Set to skip mepo steps" ON)
    else()
      # Invoke checkout_externals, but only the first time we
      # configure.
      if (NOT SKIP_MANAGE_EXTERNALS)
        execute_process (
          COMMAND "checkout_externals"
          WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
          )
      endif ()
      option (SKIP_MANAGE_EXTERNALS "Set to skip manage externals step" ON)
    endif()

    list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/@cmake")
    include (esma)
  endif()
endif()

ecbuild_declare_project()


# Special case - MAPL_cfio is built twice with two different precisions.
add_subdirectory (MAPL_cfio MAPL_cfio_r4)
add_subdirectory (MAPL_cfio MAPL_cfio_r8)

add_subdirectory (GMAO_pFIO)
add_subdirectory (MAPL_Profiler)
add_subdirectory (MAPL_Base)
add_subdirectory (MAPL)

if (PFUNIT_FOUND)
  add_subdirectory (MAPL_pFUnit EXCLUDE_FROM_ALL)
endif ()


# Git transition defect:
# Uncomment the line below once the dev branch of MAPL has been brought in.
add_subdirectory (Tests)

ecbuild_install_project (NAME MAPL)
