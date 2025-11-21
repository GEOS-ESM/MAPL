esma_set_this (OVERRIDE MAPL.esmf_utils)

set(srcs
  ESMF_Utilities.F90
  InfoUtilities.F90
  UngriddedDim.F90
  UngriddedDims.F90
  UngriddedDimVector.F90
  LU_Bound.F90
  ESMF_Time_Utilities.F90
  )

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared ESMF::ESMF
  TYPE SHARED
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

if (PFUNIT_FOUND)
  add_subdirectory(tests)
endif ()
