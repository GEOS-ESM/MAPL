esma_set_this (OVERRIDE MAPL.vertical)

set (srcs
  Eta2Eta.F90
  VerticalCoordinate.F90
  VerticalRegridConserveInterface.F90
  VerticalRegridUtilities.F90
)

esma_add_library(${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.shared MAPL.base MAPL.pfio PFLOGGER::pflogger
    TYPE SHARED
    )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)
