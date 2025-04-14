esma_set_this (OVERRIDE MAPL.field_bundle)

set(srcs
  FieldBundleType_Flag.F90
  FieldBundleGet.F90
  FieldBundleInfo.F90
  FieldBundleDelta.F90
  FieldBundleCreate.F90
)

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

if (BUILD_WITH_PFLOGGER)
  find_package (PFLOGGER REQUIRED)
endif ()

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.field MAPL.shared ESMF::ESMF
  TYPE SHARED
  )
  
if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
