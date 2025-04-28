esma_set_this (OVERRIDE MAPL.state)

set(srcs
  API.F90
  StateGet.F90
  StateSet.F90
  StateGetPointer.F90
)

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

if (BUILD_WITH_PFLOGGER)
  find_package (PFLOGGER REQUIRED)
endif ()

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.field MAPL.shared MAPL.esmf_utils ESMF::ESMF
  TYPE SHARED
)

# if (PFUNIT_FOUND)
#   add_subdirectory(tests EXCLUDE_FROM_ALL)
# endif ()
