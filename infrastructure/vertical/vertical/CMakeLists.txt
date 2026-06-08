esma_set_this (OVERRIDE MAPL.vertical)

set (srcs
  Eta2Eta.F90
  VerticalCoordinate.F90
  VerticalRegridConserveInterface.F90
  VerticalRegridUtilities.F90
  # From generic3g/vertical/ (no generic3g dependencies)
  CSR_SparseMatrix.F90
  VerticalConservativeMap.F90
  VerticalLinearMap.F90
  VerticalRegridMethod.F90
)

if (BUILD_WITH_PFLOGGER)
  find_package (PFLOGGER REQUIRED)
endif ()

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.utils MAPL.utils MAPL.pfio udunits2f PFLOGGER::pflogger
  TYPE SHARED
)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)
