esma_set_this (OVERRIDE MAPL.field)

set(srcs
  API.F90
  VerticalStaggerLoc.F90
  FieldCreate.F90
  FieldReset.F90
  FieldGet.F90
  FieldInfo.F90
)

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared MAPL.esmf_utils ESMF::ESMF 
  TYPE SHARED
  )
  
if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
