esma_set_this (OVERRIDE MAPL.esmf_utils)

set(srcs
  InfoUtilities.F90
  FieldDimensionInfo.F90
  UngriddedDim.F90
  UngriddedDims.F90
  UngriddedDimVector.F90
  LU_Bound.F90
  )

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared
  TYPE SHARED
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

if (PFUNIT_FOUND)
  add_subdirectory(tests)
endif ()
