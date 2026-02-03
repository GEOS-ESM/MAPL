esma_set_this (OVERRIDE MAPL.field)

set(srcs
  API.F90
  FieldUtils.F90
  FieldBLAS.F90
  FieldPointerUtilities.F90
  FieldDelta.F90
  FieldUtilities.F90
  FieldUnaryFunctions.F90
  FieldBinaryOperations.F90
  FieldUnits.F90
  FieldCondensedArray.F90
  FieldCondensedArray_private.F90
  FieldDelta.F90
  FieldCreate.F90
  FieldGet.F90
  FieldSet.F90
  FieldInfo.F90
  StateItemAllocation.F90
  RestartModes.F90
)

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

if (BUILD_WITH_PFLOGGER)
  find_package (PFLOGGER REQUIRED)
endif ()

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.vertical_grid MAPL.shared MAPL.esmf_utils PFLOGGER::pflogger ESMF::ESMF udunits2f
  TYPE SHARED
  )
  
if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
