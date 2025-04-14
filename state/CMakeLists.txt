esma_set_this (OVERRIDE MAPL.state)

set(srcs
  API.F90
  StateGet.F90
  StateGetPointerToData.F90
)

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

if (BUILD_WITH_PFLOGGER)
  find_package (PFLOGGER REQUIRED)
endif ()

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared ESMF::ESMF
  TYPE SHARED
)

esma_add_fortran_submodules(
  TARGET ${this}
  SUBDIRECTORY StateGet
  SOURCES get_bundle_from_state.F90
)

# if (PFUNIT_FOUND)
#   add_subdirectory(tests EXCLUDE_FROM_ALL)
# endif ()
