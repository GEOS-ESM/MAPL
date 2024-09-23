esma_set_this (OVERRIDE MAPL.esmf_utils)

set(srcs
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
  #  DEPENDENCIES MAPL.shared MAPL.base

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

