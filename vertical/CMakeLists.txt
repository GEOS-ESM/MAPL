esma_set_this (OVERRIDE ${MAPL_TARGET_PREFIX}.vertical)

set (srcs
     Eta2Eta.F90
     VerticalCoordinate.F90
	  VerticalRegridConserveInterface.F90
	  VerticalRegridUtilities.F90
     )

esma_add_library(${this}
    SRCS ${srcs}
    DEPENDENCIES ${MAPL_TARGET_PREFIX}.shared ${MAPL_TARGET_PREFIX}.base ${MAPL_TARGET_PREFIX}.pfio PFLOGGER::pflogger
    TYPE ${MAPL_LIBRARY_TYPE}
    )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)
