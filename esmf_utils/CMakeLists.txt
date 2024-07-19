esma_set_this (OVERRIDE MAPL.esmf_utils)

set(srcs
  OutputInfo.F90
  UngriddedDim.F90
  UngriddedDims.F90
  UngriddedDimVector.F90
  LU_Bound.F90
  )

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared MAPL.base
  TYPE SHARED
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

