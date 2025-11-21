esma_set_this (OVERRIDE MAPL.vm)

set(srcs
  API.F90
  vm.F90
)

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared ESMF::ESMF
  TYPE SHARED
  )

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
 
#if (PFUNIT_FOUND)
#  add_subdirectory(tests EXCLUDE_FROM_ALL)
#endif ()
